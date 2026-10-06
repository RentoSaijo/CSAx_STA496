# Outcome Preparation -----------------------------------------------------

# Define continuation controls.
primary_predictors <- base::c('CSAx', 'listedSize', 'ageC', 'ageSquared', 'gamesC', 'toiC', 'pointsC', 'satC', 'seasonFactor')
prior_role_predictors <- base::c('CSAx', 'listedSize', 'ageC', 'ageSquared', 'priorGamesC', 'priorToiC', 'pointsC', 'satC', 'seasonFactor')

# Assemble regular-season opportunity and career histories.
prepare_outcomes <- function(inputs) {
  regular <- purrr::imap_dfr(inputs$rosters, function(outcomes, season) {
    outcomes$regularRoster |>
      dplyr::full_join(outcomes$regularTime, by = 'playerId') |>
      dplyr::mutate(seasonId = base::as.integer(season))
  })
  assert_unique(regular, base::c('playerId', 'seasonId'), 'Regular roster outcomes')
  data <- inputs$features |>
    dplyr::filter(eventScope == 'All situations') |>
    dplyr::select(-eventScope)
  careers <- data |>
    dplyr::select(playerId, seasonId) |>
    dplyr::left_join(inputs$histories |> dplyr::rename(historySeasonId = seasonId), by = 'playerId', relationship = 'many-to-many') |>
    dplyr::filter(historySeasonId < seasonId) |>
    dplyr::group_by(playerId, seasonId) |>
    dplyr::summarise(priorNhlSeasons = dplyr::n_distinct(historySeasonId), .groups = 'drop')
  data |>
    dplyr::left_join(regular |> dplyr::select(playerId, seasonId, gamesDressed, finalTeamId), by = base::c('playerId', 'seasonId')) |>
    dplyr::left_join(careers, by = base::c('playerId', 'seasonId')) |>
    dplyr::mutate(nextSeasonId = next_season_id(seasonId), priorRoleSeasonId = previous_season_id(seasonId)) |>
    dplyr::left_join(regular |> dplyr::transmute(playerId, nextSeasonId = seasonId, nextGamesDressed = gamesDressed, nextTimeOnIce = regularTimeOnIce, nextToiPerGame = regularToiPerGame), by = base::c('playerId', 'nextSeasonId')) |>
    dplyr::left_join(regular |> dplyr::transmute(playerId, priorRoleSeasonId = seasonId, priorGamesDressed = dplyr::coalesce(gamesDressed, regularGamesPlayed), priorToiPerGame = regularToiPerGame), by = base::c('playerId', 'priorRoleSeasonId')) |>
    dplyr::mutate(returnedFlag = base::as.integer(!base::is.na(nextGamesDressed)), continued300Flag = base::as.integer(dplyr::coalesce(nextTimeOnIce, 0) >= xs_minutes * 60), returned = base::factor(returnedFlag, levels = base::c(0, 1)), continued300 = base::factor(continued300Flag, levels = base::c(0, 1)), nextGamesDressed = dplyr::coalesce(nextGamesDressed, 0), priorAppearance = base::as.integer(!base::is.na(priorGamesDressed)), priorGamesDressed = dplyr::coalesce(priorGamesDressed, 0), priorToiPerGame = dplyr::coalesce(priorToiPerGame, 0), priorNhlSeasons = dplyr::coalesce(priorNhlSeasons, 0L), careerStage = base::factor(dplyr::case_when(priorNhlSeasons <= 2L ~ '0-2 prior seasons', priorNhlSeasons <= 6L ~ '3-6 prior seasons', TRUE ~ '7+ prior seasons'), levels = base::c('0-2 prior seasons', '3-6 prior seasons', '7+ prior seasons')), seasonFactor = base::factor(seasonId))
}

# Standardize application controls within positional sample.
scale_controls <- function(data) {
  data |>
    dplyr::mutate(ageC = age - base::mean(age), ageSquared = ageC^2, gamesC = standardize_complete(gamesDressed), toiC = standardize_complete(timeOnIcePerGame), priorGamesC = standardize_complete(priorGamesDressed), priorToiC = standardize_complete(priorToiPerGame), pointsC = standardize_complete(pointsPer605v5), satC = standardize_complete(satRelative5v5))
}

# Join scored native players to application records.
application_panel <- function(predictions, outcomes) {
  predictions |>
    dplyr::filter(!isCenterComparison, model %in% base::c('Forwards', 'Defensemen')) |>
    dplyr::select(model, referencePopulation, rowId, playerId, seasonId, listedSize, CSAx) |>
    dplyr::left_join(outcomes, by = base::c('playerId', 'seasonId')) |>
    dplyr::group_by(model, referencePopulation) |>
    dplyr::group_modify(function(data, key) scale_controls(data)) |>
    dplyr::ungroup()
}

# Estimate continuation coefficient and standardized probabilities.
primary_statistics <- function(panel) {
  panel |>
    dplyr::group_by(model, referencePopulation) |>
    dplyr::group_modify(function(data, key) {
      fit <- fit_analysis_workflow(data, 'continued300', primary_predictors, model_type = 'logistic')
      grid <- base::c(-1, 0, 1)
      probabilities <- base::vapply(grid, function(value) {
        new_data <- data
        new_data$CSAx <- value
        base::mean(stats::predict(fit, new_data = new_data, type = 'prob')$'.pred_1')
      }, base::numeric(1L))
      tibble::tibble(logOdds = stats::coef(workflows::extract_fit_engine(fit))[['CSAx']], probabilityMinusOne = probabilities[1L], probabilityZero = probabilities[2L], probabilityPlusOne = probabilities[3L], probabilityDifference = probabilities[3L] - probabilities[1L])
    }) |>
    dplyr::ungroup()
}

# Bootstrap Inference -----------------------------------------------------

# Refit both applications and center comparison from shared player draw.
bootstrap_iteration <- function(index, features, outcomes) {
  base::set.seed(xs_bootstrap_seed + index)
  players <- base::sort(base::unique(features$playerId))
  draw <- tibble::tibble(playerId = base::sample(players, base::length(players), replace = TRUE), copyId = base::seq_along(players))
  resampled <- draw |>
    dplyr::inner_join(features, by = 'playerId', relationship = 'many-to-many')
  fitted <- fit_positional_system(resampled, seed = xs_bootstrap_seed + index, details = FALSE)
  panel <- application_panel(fitted$predictions, outcomes)
  primary <- primary_statistics(panel) |> dplyr::mutate(replicate = index, .before = 1L)
  centers <- center_statistics(compare_centers(fitted$predictions)) |> dplyr::mutate(replicate = index, .before = 1L)
  base::list(primary = primary, centers = centers, drawHash = digest::digest(draw, algo = 'sha256'))
}

# Run resumable shared full-pipeline bootstrap with fixed specifications.
run_positional_bootstrap <- function(features, outcomes, workers = 12L) {
  features <- features |> dplyr::filter(eventScope == 'All situations')
  signature <- digest::digest(base::list(features = features, outcomes = outcomes, modelCode = readr::read_file('R/models.R'), applicationCode = base::lapply(base::list(bootstrap_iteration, application_panel, scale_controls, primary_statistics, fit_analysis_workflow), base::body), settings = base::c(xs_seed, xs_bootstrap_seed, xs_bootstrap_reps, xs_outer_folds, xs_inner_folds, xs_penalty_grid)), algo = 'sha256')
  cache_path <- 'data/cache/positional_bootstrap.rds'
  cache <- if (base::file.exists(cache_path)) base::readRDS(cache_path) else NULL
  if (base::is.null(cache) || !base::identical(cache$signature, signature)) cache <- base::list(signature = signature, samples = base::vector('list', xs_bootstrap_reps))
  pending <- base::which(base::vapply(cache$samples, base::is.null, base::logical(1L)))
  batches <- base::split(pending, base::ceiling(base::seq_along(pending) / workers))
  for (batch in batches) {
    samples <- parallel::mclapply(batch, function(index) bootstrap_iteration(index, features, outcomes), mc.cores = base::min(workers, base::length(batch)), mc.set.seed = FALSE)
    failed <- base::vapply(samples, base::inherits, base::logical(1L), 'try-error')
    if (base::any(failed)) base::stop(base::paste('Bootstrap fitting failed:', base::paste(samples[failed], collapse = '\n')), call. = FALSE)
    cache$samples[batch] <- samples
    base::saveRDS(cache, cache_path, compress = 'xz')
    base::message('Completed ', base::sum(!base::vapply(cache$samples, base::is.null, base::logical(1L))), ' of ', xs_bootstrap_reps, ' shared bootstrap samples.')
  }
  base::list(signature = signature, primary = purrr::map_dfr(cache$samples, 'primary'), centers = purrr::map_dfr(cache$samples, 'centers'), drawHashes = purrr::map_chr(cache$samples, 'drawHash'), replicates = xs_bootstrap_reps, seed = xs_bootstrap_seed)
}

# Supporting Applications -------------------------------------------------

# Estimate positional roster outcomes and prespecified role comparisons.
analyze_roster <- function(panel) {
  parts <- base::split(panel, panel$model)
  results <- purrr::imap(parts, function(data, population) {
    estimate <- function(outcome, predictors = primary_predictors, label = outcome, subset = data, logistic = FALSE, term = 'CSAx') {
      fit <- fit_analysis_workflow(subset, outcome, predictors, model_type = if (logistic) 'logistic' else 'linear')
      if (term == 'CSAx') base::return(summarize_analysis_result(fit, subset, 'Supporting', label, scale = if (logistic) 'odds ratio' else 'linear'))
      clustered_term(fit, subset, term = term) |>
        dplyr::mutate(outcome = label, label = 'Supporting', sampleSize = base::nrow(subset), players = dplyr::n_distinct(subset$playerId), scale = 'odds ratio', effect = base::exp(estimate), effectLow = base::exp(confLow), effectHigh = base::exp(confHigh))
    }
    conditioning <- base::list('CSAx only' = 'CSAx', 'Listed size' = base::c('CSAx', 'listedSize'), 'Age' = base::c('CSAx', 'listedSize', 'ageC', 'ageSquared'), 'Games dressed' = base::c('CSAx', 'listedSize', 'ageC', 'ageSquared', 'gamesC'), 'Ice time' = base::c('CSAx', 'listedSize', 'ageC', 'ageSquared', 'gamesC', 'toiC'), 'Full controls' = primary_predictors)
    conditioning_results <- purrr::imap_dfr(conditioning, function(predictors, label) estimate('continued300', predictors, label, logistic = TRUE))
    returners <- data |> dplyr::filter(returnedFlag == 1L, base::is.finite(nextToiPerGame)) |> dplyr::mutate(nextToiMinutes = nextToiPerGame / 60)
    prior_sample <- data |> dplyr::filter(priorAppearance == 1L)
    interactions <- data |> dplyr::mutate(CSAxListedSize = CSAx * listedSize, CSAxToi = CSAx * toiC)
    estimates <- dplyr::bind_rows(
      estimate('continued300', label = 'Next-season continuation', logistic = TRUE),
      estimate('returned', label = 'Any next-season appearance', logistic = TRUE),
      estimate('nextGamesDressed', label = 'Next-season games dressed'),
      estimate('nextToiMinutes', label = 'Next-season TOI per game among returners', subset = returners),
      estimate('continued300', prior_role_predictors, 'Prior-season role', logistic = TRUE),
      estimate('continued300', primary_predictors, 'Current role among prior participants', subset = prior_sample, logistic = TRUE),
      estimate('continued300', prior_role_predictors, 'Prior role among prior participants', subset = prior_sample, logistic = TRUE),
      estimate('continued300', base::c(primary_predictors, 'CSAxListedSize'), 'CSAx by listed-size interaction', subset = interactions, logistic = TRUE, term = 'CSAxListedSize'),
      estimate('continued300', base::c(primary_predictors, 'CSAxToi'), 'CSAx by ice-time interaction', subset = interactions, logistic = TRUE, term = 'CSAxToi')
    )
    career_data <- data |> dplyr::mutate(careerStageMid = base::as.integer(careerStage == '3-6 prior seasons'), careerStageVeteran = base::as.integer(careerStage == '7+ prior seasons'), CSAxCareerStageMid = CSAx * careerStageMid, CSAxCareerStageVeteran = CSAx * careerStageVeteran)
    career_fit <- fit_analysis_workflow(career_data, 'continued300', base::c(primary_predictors, 'careerStageMid', 'careerStageVeteran', 'CSAxCareerStageMid', 'CSAxCareerStageVeteran'), model_type = 'logistic')
    career_slopes <- dplyr::bind_rows(clustered_linear_combination(career_fit, career_data, base::c(CSAx = 1), '0-2 prior seasons'), clustered_linear_combination(career_fit, career_data, base::c(CSAx = 1, CSAxCareerStageMid = 1), '3-6 prior seasons'), clustered_linear_combination(career_fit, career_data, base::c(CSAx = 1, CSAxCareerStageVeteran = 1), '7+ prior seasons')) |>
      dplyr::mutate(outcome = base::paste('Continuation:', label), scale = 'odds ratio', sampleSize = base::nrow(career_data), players = dplyr::n_distinct(career_data$playerId), effect = base::exp(estimate), effectLow = base::exp(confLow), effectHigh = base::exp(confHigh), term = 'CSAx')
    career_wald <- clustered_joint_wald(career_fit, career_data, base::c('CSAxCareerStageMid', 'CSAxCareerStageVeteran'))
    career_counts <- data |> dplyr::group_by(careerStage) |> dplyr::summarise(n = dplyr::n(), continued = base::sum(continued300Flag), meanCSAx = base::mean(CSAx), .groups = 'drop')
    role_summary <- data |> dplyr::mutate(toiQuartile = dplyr::ntile(timeOnIcePerGame, 4L)) |> dplyr::group_by(toiQuartile) |> dplyr::summarise(n = dplyr::n(), continuations = base::sum(continued300Flag), continuationRate = base::mean(continued300Flag), .groups = 'drop')
    base::list(estimates = dplyr::bind_rows(estimates, career_slopes), conditioning = conditioning_results, careerWald = career_wald, careerCounts = career_counts, roleSummary = role_summary)
  })
  purrr::map(stats::setNames(base::c('estimates', 'conditioning', 'careerWald', 'careerCounts', 'roleSummary'), base::c('estimates', 'conditioning', 'careerWald', 'careerCounts', 'roleSummary')), function(component) purrr::imap_dfr(results, function(part, population) part[[component]] |> dplyr::mutate(model = population, referencePopulation = population, intervalMethod = 'Player-clustered HC1; conditional on estimated scores', .before = 1L)))
}

# Preserve external veteran contract eligibility and timing controls.
prepare_contracts <- function(inputs, panel) {
  contracts <- inputs$contracts |>
    dplyr::left_join(inputs$players |> dplyr::select(playerId, birthDate), by = 'playerId') |>
    dplyr::arrange(playerId, startSeasonId) |>
    dplyr::group_by(playerId) |>
    dplyr::mutate(previousAav = dplyr::lag(aav), previousTerm = dplyr::lag(term)) |>
    dplyr::ungroup() |>
    dplyr::filter(positionCode %in% base::c('C', 'L', 'R', 'D'), ageAtSigning >= 27, startSeasonId %in% xs_contract_seasons) |>
    dplyr::add_count(playerId, startSeasonId, name = 'contractKeyCount') |>
    dplyr::filter(contractKeyCount == 1L, !base::is.na(previousAav), previousAav > 0, !base::is.na(previousTerm)) |>
    dplyr::mutate(priorSeasonId = previous_season_id(startSeasonId), contractRow = dplyr::row_number())
  transactions <- audit_contract_transactions(contracts, build_transaction_clauses(inputs$transactions))
  caps <- tibble::tibble(startSeasonId = xs_contract_seasons, capMillions = base::c(82.5, 83.5, 88, 95.5))
  dates <- inputs$seasons |> dplyr::transmute(priorSeasonId = seasonId, priorRegularEndDate = base::as.Date(regularSeasonEndDate))
  contracts <- contracts |>
    dplyr::left_join(transactions, by = 'contractRow') |>
    dplyr::left_join(caps, by = 'startSeasonId') |>
    dplyr::left_join(dates, by = 'priorSeasonId') |>
    dplyr::mutate(timingLeak = !base::is.na(matchDate) & matchDate <= priorRegularEndDate) |>
    dplyr::inner_join(panel |> dplyr::select(model, referencePopulation, playerId, priorSeasonId = seasonId, finalPriorTeamId = finalTeamId, CSAx, listedSize, gamesPlayed, timeOnIcePerGame, pointsPer605v5, satRelative5v5), by = base::c('playerId', 'priorSeasonId')) |>
    dplyr::filter(signedWithTeamId != finalPriorTeamId, !timingLeak) |>
    dplyr::group_by(model, referencePopulation) |>
    dplyr::mutate(logCapAav = base::log(aav / (capMillions * 1e6)), signingAgeC = ageAtSigning - base::mean(ageAtSigning), signingAgeSquared = signingAgeC^2, contractGamesC = standardize_complete(gamesPlayed), contractToiC = standardize_complete(timeOnIcePerGame), contractPointsC = standardize_complete(pointsPer605v5), contractSatC = standardize_complete(satRelative5v5), previousLogAavC = standardize_complete(base::log(previousAav)), previousTermC = standardize_complete(previousTerm), contractSeasonFactor = base::factor(startSeasonId)) |>
    dplyr::ungroup()
  assert_unique(contracts, base::c('playerId', 'startSeasonId'), 'External veteran contracts')
  contracts
}

# Assemble playoff dressing and ice-time opportunities.
prepare_playoffs <- function(inputs, panel) {
  teams <- purrr::map_dfr(xs_behavior_seasons, function(season) inputs$rosters[[base::as.character(season)]]$playoffTeam |> dplyr::mutate(seasonId = season))
  players <- purrr::map_dfr(xs_behavior_seasons, function(season) {
    outcomes <- inputs$rosters[[base::as.character(season)]]
    outcomes$playoffPlayer |> dplyr::left_join(outcomes$playoffTime, by = 'playerId') |> dplyr::mutate(seasonId = season)
  })
  panel |>
    dplyr::inner_join(teams, by = base::c('seasonId', 'finalTeamId' = 'teamId')) |>
    dplyr::left_join(players, by = base::c('playerId', 'seasonId', 'finalTeamId' = 'teamId')) |>
    dplyr::mutate(playoffGamesDressed = dplyr::coalesce(playoffGamesDressed, 0), playoffTimeOnIce = dplyr::coalesce(playoffTimeOnIce, 0), playoffDressedShare = playoffGamesDressed / teamPlayoffGames, playoffToiPerTeamGame = playoffTimeOnIce / 60 / teamPlayoffGames)
}

# Estimate contract and playoff associations by position.
analyze_contracts_playoffs <- function(contracts, playoffs) {
  contract_predictors <- base::c('CSAx', 'listedSize', 'signingAgeC', 'signingAgeSquared', 'contractGamesC', 'contractToiC', 'contractPointsC', 'contractSatC', 'previousLogAavC', 'previousTermC', 'contractSeasonFactor')
  contract_results <- contracts |> dplyr::group_by(model, referencePopulation) |> dplyr::group_modify(function(data, key) {
    term_fit <- fit_analysis_workflow(data, 'term', contract_predictors)
    salary_fit <- fit_analysis_workflow(data, 'logCapAav', contract_predictors)
    dplyr::bind_rows(summarize_analysis_result(term_fit, data, 'Supporting', 'External-contract duration'), summarize_analysis_result(salary_fit, data, 'Supporting', 'Cap-adjusted contract AAV', scale = 'percent'))
  }) |> dplyr::ungroup()
  playoff_results <- playoffs |> dplyr::group_by(model, referencePopulation) |> dplyr::group_modify(function(data, key) {
    dress_fit <- fit_analysis_workflow(data, 'playoffDressedShare', primary_predictors)
    time_fit <- fit_analysis_workflow(data, 'playoffToiPerTeamGame', primary_predictors)
    dplyr::bind_rows(summarize_analysis_result(dress_fit, data, 'Exploratory', 'Playoff dressing share'), summarize_analysis_result(time_fit, data, 'Exploratory', 'Playoff minutes per team game'))
  }) |> dplyr::ungroup()
  dplyr::bind_rows(contract_results, playoff_results) |> dplyr::mutate(intervalMethod = 'Player-clustered HC1; conditional on estimated scores')
}

# Keep forward and defenseman team exposures separate.
analyze_teams <- function(inputs, panel) {
  exposures <- purrr::map_dfr(xs_behavior_seasons, function(season) inputs$rosters[[base::as.character(season)]]$regularPlayerTeam |> dplyr::mutate(seasonId = season))
  positions <- if (!base::is.null(inputs$teamPlayerPositions)) inputs$teamPlayerPositions else inputs$players |> dplyr::select(playerId, positionCode)
  exposure_positions <- exposures |>
    dplyr::left_join(positions, by = 'playerId')
  if (base::anyNA(exposure_positions$positionCode)) {
    registry <- if (base::file.exists('data/cache/players.rds')) base::readRDS('data/cache/players.rds') else inputs$teamPlayerPositions
    exposure_positions <- exposures |> dplyr::left_join(registry |> dplyr::select(playerId, positionCode), by = 'playerId')
  }
  if (base::anyNA(exposure_positions$positionCode)) base::stop('Team exposure lacks positional attribution.', call. = FALSE)
  exposure_positions <- exposure_positions |>
    dplyr::filter(positionCode %in% base::c('C', 'L', 'R', 'D')) |>
    dplyr::mutate(model = dplyr::if_else(positionCode == 'D', 'Defensemen', 'Forwards'))
  expected <- purrr::imap_dfr(inputs$expectedGoals, function(data, season) data$team |> dplyr::mutate(seasonId = base::as.integer(season)))
  teams <- exposure_positions |>
    dplyr::left_join(panel |> dplyr::select(model, playerId, seasonId, CSAx, listedSize, age), by = base::c('model', 'playerId', 'seasonId')) |>
    dplyr::group_by(model, seasonId, teamId) |>
    dplyr::summarise(eligiblePlayerGames = base::sum(teamGamesDressed[!base::is.na(CSAx)]), allPlayerGames = base::sum(teamGamesDressed), coverage = eligiblePlayerGames / allPlayerGames, teamCSAx = stats::weighted.mean(CSAx, teamGamesDressed, na.rm = TRUE), teamListedSize = stats::weighted.mean(listedSize, teamGamesDressed, na.rm = TRUE), teamAge = stats::weighted.mean(age, teamGamesDressed, na.rm = TRUE), .groups = 'drop') |>
    dplyr::left_join(expected, by = base::c('teamId', 'seasonId')) |>
    dplyr::left_join(inputs$teams |> dplyr::select(teamId, teamFullName, teamTriCode), by = 'teamId') |>
    dplyr::group_by(model, seasonId) |>
    dplyr::mutate(teamCSAxZ = standardize_vector(teamCSAx), teamGAxZ = standardize_vector(GAx), referencePopulation = model) |>
    dplyr::ungroup()
  quality <- inputs$expectedGoals[[base::as.character(base::max(xs_behavior_seasons))]]$teamFiveOnFive |>
    dplyr::select(teamId, gameTypeId, attempts, xGoalsPerAttempt) |>
    tidyr::pivot_wider(names_from = gameTypeId, values_from = base::c('attempts', 'xGoalsPerAttempt'), names_glue = '{.value}_{gameTypeId}') |>
    dplyr::filter(!base::is.na(attempts_3))
  playoff_quality <- teams |> dplyr::filter(seasonId == base::max(xs_behavior_seasons)) |> dplyr::inner_join(quality, by = 'teamId') |> dplyr::mutate(absoluteQualityChange = base::abs(xGoalsPerAttempt_3 - xGoalsPerAttempt_2))
  summaries <- teams |> dplyr::group_by(model, referencePopulation) |> dplyr::summarise(teamSeasons = dplyr::n(), correlationGAx = stats::cor(teamCSAxZ, teamGAxZ), medianCoverage = stats::median(coverage), minimumCoverage = base::min(coverage), .groups = 'drop') |>
    dplyr::left_join(playoff_quality |> dplyr::group_by(model) |> dplyr::summarise(playoffTeams = dplyr::n(), correlationQualityChange = stats::cor(teamCSAx, absoluteQualityChange), .groups = 'drop'), by = 'model')
  assert_unique(teams, base::c('model', 'teamId', 'seasonId'), 'Positional team summaries')
  base::list(seasons = teams, playoffQuality = playoff_quality, summary = summaries, exposurePositions = exposure_positions |> dplyr::distinct(playerId, positionCode))
}

# Recompute associations with frozen forward scouting ratings.
analyze_scouting <- function(codes, panel) {
  means <- panel |> dplyr::filter(model == 'Forwards', playerId %in% codes$playerId) |> dplyr::group_by(playerId) |> dplyr::summarise(meanCSAx = base::mean(CSAx), observedSeasons = dplyr::n(), .groups = 'drop')
  data <- codes |> dplyr::select(-dplyr::any_of(base::c('meanCSAx', 'eligibleSeasons', 'firstSeason'))) |> dplyr::inner_join(means, by = 'playerId')
  if (base::nrow(data) != 40L) base::stop('Frozen scouting cohort is incomplete.', call. = FALSE)
  indicators <- base::c('overallPhysicality', 'playsBiggerExplicit', 'activePhysicalEngagement', 'interiorPlay')
  estimates <- purrr::map_dfr(indicators, function(indicator) {
    fit <- fit_analysis_workflow(data, 'meanCSAx', indicator)
    clustered_term(fit, data, term = indicator) |>
      dplyr::mutate(indicator = indicator, positiveReports = base::sum(data[[indicator]] == 1L), model = 'Forwards', referencePopulation = 'Forwards', .before = 1L)
  })
  estimates$holmPValue <- NA_real_
  estimates$holmPValue[-1L] <- stats::p.adjust(estimates$pValue[-1L], method = 'holm')
  base::list(scores = data |> dplyr::select(studyId, playerId, meanCSAx, observedSeasons), estimates = estimates, spearman = stats::cor(data$meanCSAx, data$overallPhysicality, method = 'spearman'), n = 40L, raters = 1L)
}

# Application Bundle ------------------------------------------------------

# Assemble compact positional application estimates and descriptive summaries.
analyze_positional_applications <- function(models, inputs) {
  outcomes <- prepare_outcomes(inputs)
  panel <- application_panel(models$primary$predictions, outcomes)
  roster <- analyze_roster(panel)
  contracts <- prepare_contracts(inputs, panel)
  playoffs <- prepare_playoffs(inputs, panel)
  additional_estimates <- analyze_contracts_playoffs(contracts, playoffs)
  teams <- analyze_teams(inputs, panel)
  scouting <- analyze_scouting(inputs$scoutingCodes, panel)
  sensitivities <- purrr::imap_dfr(models$sensitivities, function(fit, specification) {
    application_panel(fit$predictions, outcomes) |> dplyr::group_by(model, referencePopulation) |> dplyr::group_modify(function(data, key) {
      model <- fit_analysis_workflow(data, 'continued300', primary_predictors, model_type = 'logistic')
      summarize_analysis_result(model, data, 'Sensitivity', specification, scale = 'odds ratio')
    }) |> dplyr::ungroup() |> dplyr::mutate(specification = specification, intervalMethod = 'Player-clustered HC1; conditional on estimated scores')
  })
  sensitivity_agreement <- purrr::imap_dfr(models$sensitivities, function(fit, specification) {
    fit$predictions |> dplyr::filter(!isCenterComparison) |> dplyr::select(model, playerId, seasonId, CSAx) |>
      dplyr::inner_join(models$primary$predictions |> dplyr::filter(!isCenterComparison) |> dplyr::select(model, playerId, seasonId, primaryCSAx = CSAx), by = base::c('model', 'playerId', 'seasonId')) |>
      dplyr::group_by(model, seasonId) |> dplyr::summarise(n = dplyr::n(), correlation = stats::cor(CSAx, primaryCSAx), spearman = stats::cor(CSAx, primaryCSAx, method = 'spearman'), .groups = 'drop') |>
      dplyr::left_join(fit$performance |> dplyr::select(model, seasonId, predictiveRSquared, residualSizeCorrelation), by = base::c('model', 'seasonId')) |>
      dplyr::mutate(specification = specification, .before = 1L)
  })
  center_components <- purrr::map_dfr(base::c('Combined', 'Direct only', 'Indirect only'), function(specification) {
    fitted <- if (specification == 'Combined') models$primary else models$sensitivities[[specification]]
    center_statistics(compare_centers(fitted$predictions)) |> dplyr::mutate(specification = specification, .before = 1L)
  })
  native <- models$primary$predictions |> dplyr::filter(!isCenterComparison)
  stability <- native |> dplyr::select(model, playerId, seasonId, currentCSAx = CSAx) |> dplyr::mutate(nextSeasonId = next_season_id(seasonId)) |>
    dplyr::inner_join(native |> dplyr::select(model, playerId, nextSeasonId = seasonId, nextCSAx = CSAx), by = base::c('model', 'playerId', 'nextSeasonId')) |>
    dplyr::group_by(model, seasonId, nextSeasonId) |> dplyr::summarise(n = dplyr::n(), correlation = stats::cor(currentCSAx, nextCSAx), spearman = stats::cor(currentCSAx, nextCSAx, method = 'spearman'), .groups = 'drop')
  sparse <- inputs$features |> dplyr::filter(eventScope == 'All situations') |> dplyr::mutate(positionGroup = dplyr::if_else(positionCode == 'D', 'Defensemen', dplyr::if_else(positionCode == 'C', 'Centers', 'Wings'))) |>
    dplyr::group_by(positionGroup, seasonId) |> dplyr::summarise(n = dplyr::n(), medianDefensiveTakeaways = stats::median(defensiveTakeaways), lowerQuartileDefensiveTakeaways = stats::quantile(defensiveTakeaways, 0.25), upperQuartileDefensiveTakeaways = stats::quantile(defensiveTakeaways, 0.75), medianPerimeterTakeaways = stats::median(defensivePerimeterTakeaways), missingPerimeterShare = base::sum(base::is.na(defensivePerimeterTakeawayShare)), zeroFights = base::sum(fights == 0), medianContactTaken = stats::median(contactPenaltiesTaken), medianContactDrawn = stats::median(contactPenaltiesDrawn), .groups = 'drop')
  base::list(panel = panel, primaryPoints = primary_statistics(panel), roster = roster, estimates = dplyr::bind_rows(roster$estimates, additional_estimates), contracts = contracts, playoffs = playoffs, teams = teams, scouting = scouting, sensitivityEstimates = sensitivities, sensitivityAgreement = sensitivity_agreement, centerComponents = center_components, stability = stability, sparseEvents = sparse)
}

# Attach percentile intervals from shared full-pipeline samples.
summarize_positional_bootstrap <- function(bootstrap, primary_points, centers) {
  primary <- bootstrap$primary |> tidyr::pivot_longer(cols = base::c('logOdds', 'probabilityMinusOne', 'probabilityZero', 'probabilityPlusOne', 'probabilityDifference'), names_to = 'statistic', values_to = 'value') |>
    dplyr::group_by(model, referencePopulation, statistic) |> dplyr::summarise(confLow = stats::quantile(value, 0.025), confHigh = stats::quantile(value, 0.975), bootstrapSd = stats::sd(value), replicates = dplyr::n(), .groups = 'drop') |>
    dplyr::left_join(primary_points |> tidyr::pivot_longer(cols = base::c('logOdds', 'probabilityMinusOne', 'probabilityZero', 'probabilityPlusOne', 'probabilityDifference'), names_to = 'statistic', values_to = 'estimate'), by = base::c('model', 'referencePopulation', 'statistic')) |>
    dplyr::mutate(effect = dplyr::if_else(statistic == 'logOdds', base::exp(estimate), estimate), effectLow = dplyr::if_else(statistic == 'logOdds', base::exp(confLow), confLow), effectHigh = dplyr::if_else(statistic == 'logOdds', base::exp(confHigh), confHigh), intervalMethod = '499 full-pipeline player bootstrap; percentile interval')
  center_intervals <- bootstrap$centers |> tidyr::pivot_longer(cols = base::c('spearman', 'meanPercentileDifference', 'medianPercentileDifference', 'meanAbsolutePercentileDifference'), names_to = 'statistic', values_to = 'value') |>
    dplyr::group_by(seasonId, sample, statistic) |> dplyr::summarise(confLow = stats::quantile(value, 0.025), confHigh = stats::quantile(value, 0.975), replicates = dplyr::n(), .groups = 'drop') |>
    dplyr::left_join(center_statistics(centers) |> tidyr::pivot_longer(cols = base::c('spearman', 'meanPercentileDifference', 'medianPercentileDifference', 'meanAbsolutePercentileDifference'), names_to = 'statistic', values_to = 'estimate'), by = base::c('seasonId', 'sample', 'statistic')) |>
    dplyr::mutate(model = 'Wings versus Defensemen', referencePopulation = 'Wings and Defensemen separately', intervalMethod = '499 shared full-pipeline player bootstrap; percentile interval')
  if (base::any(primary$replicates != xs_bootstrap_reps) || base::any(center_intervals$replicates != xs_bootstrap_reps)) base::stop('Bootstrap inference is incomplete.', call. = FALSE)
  base::list(primary = primary, centers = center_intervals)
}

# Pilot Summaries ---------------------------------------------------------

# Pair centers while retaining positional opportunity cautions.
compare_a3z_centers <- function(fit) {
  compared <- compare_centers(fit$predictions)
  flags <- fit$predictions |>
    dplyr::filter(isCenterComparison) |>
    dplyr::select(playerId, seasonId, model, sparseDenominators, trackedMinutes, sourceTrackedMinutes, trackedGames, trackedGameShare) |>
    tidyr::pivot_wider(names_from = model, values_from = sparseDenominators)
  compared |>
    dplyr::left_join(flags |> dplyr::rename(sparseDenominators_Wings = Wings, sparseDenominators_Defensemen = Defensemen), by = base::c('playerId', 'seasonId')) |>
    dplyr::mutate(withinTrainingRanges = supported, supported = supported & !base::nzchar(sparseDenominators_Wings) & !base::nzchar(sparseDenominators_Defensemen))
}

# Evaluate positional physicality, scouting, and continuation from primary fits.
analyze_a3z_models <- function(fits, inputs, benchmark_inputs) {
  primary <- fits[[a3z_specification]]
  if (base::is.null(primary) || !base::identical(primary$version, a3z_version) || !base::identical(primary$inputSha256, digest::digest(inputs$features, algo = 'sha256'))) base::stop('Refit current physicality specification before analyzing results.', call. = FALSE)
  predictions <- primary$predictions |> dplyr::mutate(specification = a3z_specification, eventScope = a3z_scope, .before = 1L)
  performance <- primary$performance |>
    dplyr::mutate(specification = a3z_specification, eventScope = a3z_scope, scoreCaution = dplyr::case_when(residualSd < 0.02 ~ 'Near-degenerate residual scale', predictiveRSquared <= 0 ~ 'No improvement over mean-size prediction', base::abs(residualSizeCorrelation) > 0.10 ~ 'Remaining size gradient', TRUE ~ ''), .before = 1L)
  outcomes <- prepare_outcomes(benchmark_inputs)
  panel <- application_panel(predictions, outcomes)
  continuation_parts <- purrr::imap(base::split(panel, panel$model), function(data, population) {
    fit <- fit_analysis_workflow(data, 'continued300', primary_predictors, model_type = 'logistic')
    probabilities <- estimate_average_probabilities(fit, data, base::c(-1, 0, 1))
    label <- function(result) result |>
      dplyr::mutate(specification = a3z_specification, model = population, referencePopulation = population, eventScope = a3z_scope, outcomeScope = 'Next regular season; all NHL situations', roleTiming = 'Current season', cohort = 'All eligible scored players', intervalMethod = 'Player-clustered HC1; conditional on estimated scores and tracked sample', .before = 1L)
    base::list(estimates = label(summarize_analysis_result(fit, data, 'Primary application', 'Next-season continuation', scale = 'odds ratio')), probabilities = label(probabilities$curve), contrast = label(probabilities$contrast))
  })
  scouting <- analyze_physicality_scouting(predictions, read_physicality_scouting())
  native <- predictions |> dplyr::filter(!isCenterComparison)
  stability <- native |> dplyr::select(specification, model, playerId, seasonId, currentCSAx = CSAx) |> dplyr::mutate(nextSeasonId = next_season_id(seasonId)) |>
    dplyr::inner_join(native |> dplyr::select(specification, model, playerId, nextSeasonId = seasonId, nextCSAx = CSAx), by = base::c('specification', 'model', 'playerId', 'nextSeasonId')) |>
    dplyr::group_by(specification, model, seasonId, nextSeasonId) |>
    dplyr::summarise(n = dplyr::n(), correlation = stats::cor(currentCSAx, nextCSAx), spearman = stats::cor(currentCSAx, nextCSAx, method = 'spearman'), .groups = 'drop')
  base::list(predictions = predictions, performance = performance, continuation = purrr::map_dfr(continuation_parts, 'estimates'), continuationProbabilities = purrr::map_dfr(continuation_parts, 'probabilities'), continuationContrasts = purrr::map_dfr(continuation_parts, 'contrast'), scouting = scouting, stability = stability)
}

# Focused Role Applications -----------------------------------------------

# Compare next-season ice time among players who return to the NHL.
analyze_returner_ice_time <- function(panel) {
  returners <- panel |>
    dplyr::filter(returnedFlag == 1L, base::is.finite(nextToiPerGame)) |>
    dplyr::mutate(nextToiMinutes = nextToiPerGame / 60)
  estimates <- purrr::imap_dfr(base::split(returners, returners$model), function(data, population) {
    fitted <- fit_analysis_workflow(data, 'nextToiMinutes', primary_predictors)
    summarize_analysis_result(fitted, data, 'Playing time', 'Next-season ice time per game among returners', scale = 'minutes per game') |>
      dplyr::mutate(specification = a3z_specification, model = population, referencePopulation = population, eventScope = a3z_scope, outcomeScope = 'Next regular season; all NHL situations; minutes per game among returners', roleTiming = 'Current season', cohort = 'At least one next-season NHL appearance and finite ice time per game', statistic = 'Adjusted difference per CSAx SD', intervalMethod = 'Player-clustered HC1; conditional on estimated scores and returner sample', .before = 1L)
  })
  base::list(estimates = estimates)
}

# Compare current and preceding roles on identical returning-player observations.
analyze_role_timing <- function(panel) {
  cohort <- panel |> dplyr::filter(priorAppearance == 1L, priorGamesDressed > 0, base::is.finite(priorToiPerGame), priorToiPerGame > 0)
  parts <- purrr::imap(base::split(cohort, cohort$model), function(data, population) {
    purrr::imap(base::list('Current season' = primary_predictors, 'Previous season' = prior_role_predictors), function(predictors, timing) {
      fitted <- fit_analysis_workflow(data, 'continued300', predictors, model_type = 'logistic')
      probabilities <- estimate_average_probabilities(fitted, data, base::c(-1, 0, 1))
      label <- function(table) table |> dplyr::mutate(specification = a3z_specification, model = population, referencePopulation = population, eventScope = a3z_scope, outcomeScope = 'Next regular season; all NHL situations', roleTiming = timing, cohort = 'Observed NHL participation in preceding season; matched observations', intervalMethod = 'Player-clustered HC1; conditional on estimated scores and matched sample', .before = 1L)
      base::list(estimates = label(summarize_analysis_result(fitted, data, 'Matched role timing', 'Next-season continuation', scale = 'odds ratio')), probabilities = label(probabilities$curve), contrasts = label(probabilities$contrast))
    })
  }) |> purrr::flatten()
  base::list(estimates = purrr::map_dfr(parts, 'estimates'), probabilities = purrr::map_dfr(parts, 'probabilities'), contrasts = purrr::map_dfr(parts, 'contrasts'), cohort = cohort |> dplyr::select(model, playerId, seasonId, priorRoleSeasonId, nextSeasonId, gamesDressed, timeOnIcePerGame, priorGamesDressed, priorToiPerGame, continued300Flag))
}

# Describe official special-teams shares and adjusted percentage-point associations.
analyze_deployment <- function(panel, special_teams) {
  data <- panel |> dplyr::left_join(special_teams, by = base::c('playerId', 'seasonId')) |>
    dplyr::mutate(powerPlayShare = dplyr::if_else(totalSeconds > 0, 100 * powerPlaySeconds / totalSeconds, NA_real_), penaltyKillShare = dplyr::if_else(totalSeconds > 0, 100 * penaltyKillSeconds / totalSeconds, NA_real_))
  parts <- purrr::imap(base::split(data, data$model), function(rows, population) {
    purrr::map_dfr(base::c('powerPlayShare', 'penaltyKillShare'), function(outcome) {
      sample <- rows |> dplyr::filter(base::is.finite(.data[[outcome]]))
      fit <- fit_analysis_workflow(sample, outcome, primary_predictors)
      summarize_analysis_result(fit, sample, 'Deployment', outcome) |>
        dplyr::mutate(model = population, referencePopulation = population, meanShare = base::mean(sample[[outcome]]), zeroRecords = base::sum(sample[[outcome]] == 0), missingRecords = base::nrow(rows) - base::nrow(sample), pearson = stats::cor(sample$CSAx, sample[[outcome]]), spearman = stats::cor(sample$CSAx, sample[[outcome]], method = 'spearman'), statistic = 'Percentage-point association per CSAx SD', scale = 'percentage points')
    })
  })
  estimates <- dplyr::bind_rows(parts) |> dplyr::mutate(specification = a3z_specification, eventScope = a3z_scope, outcomeScope = 'Current regular season; official special-teams share of total ice time', roleTiming = 'Current season', intervalMethod = 'Player-clustered HC1; conditional on estimated scores and observed deployment')
  base::list(estimates = estimates, panel = data |> dplyr::select(model, playerId, seasonId, CSAx, totalSeconds, powerPlaySeconds, penaltyKillSeconds, powerPlayShare, penaltyKillShare))
}
