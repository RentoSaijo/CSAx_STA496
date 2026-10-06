# Research Settings --------------------------------------------------------

# Define analysis window and estimation settings.
xs_seed             <- 20260909L
xs_bootstrap_seed   <- 20260910L
xs_bootstrap_reps   <- 499L
xs_minutes          <- 300
xs_outer_folds      <- 5L
xs_inner_folds      <- 5L
confidence_level    <- 0.95
xs_package_sha      <- '58821f001469d5024766511e511051377c0f859f'
xs_behavior_seasons <- base::c(20212022L, 20222023L, 20232024L, 20242025L)
xs_roster_seasons   <- base::c(20202021L, xs_behavior_seasons, 20252026L)
xs_contract_seasons <- base::c(20222023L, 20232024L, 20242025L, 20252026L)
xs_penalty_grid     <- 10^base::seq(-4, 2, length.out = 20L)
xs_direct_features  <- base::c('hitsPer60', 'hitsReceivedPer60', 'blockedShotsPer60', 'fightsPer60', 'contactPenaltiesTakenPer60', 'contactPenaltiesDrawnPer60')
xs_forward_features <- base::c('netFrontAttemptShare', 'deflectionShare', 'medianShotDistance', 'backhandShare')
xs_defense_features <- base::c('defensiveTakeawaysPer60', 'defensivePerimeterTakeawayShare')
xs_features         <- base::unique(base::c(xs_direct_features, xs_forward_features, xs_defense_features, 'reboundAttemptShare'))
xs_contact_penalties <- base::c('boarding', 'charging', 'checking-from-behind', 'clipping', 'elbowing', 'illegal-check-to-head', 'illegal-check-to-the-head', 'checking-to-the-head', 'kneeing', 'roughing', 'roughing-removing-opponents-helmet', 'slew-footing', 'cross-checking', 'high-sticking', 'high-sticking-double-minor', 'holding', 'holding-the-stick', 'hooking', 'tripping')
xs_feature_version  <- '20260909-positional-v2'
xs_model_version    <- '20260909-nested-v1'

# Validation Helpers -------------------------------------------------------

# Validate required columns.
assert_columns <- function(data, columns, label) {
  missing_columns <- base::setdiff(columns, base::names(data))
  if (base::length(missing_columns) > 0L) base::stop(base::paste0(label, ' is missing: ', base::paste(missing_columns, collapse = ', ')), call. = FALSE)
  base::invisible(data)
}

# Validate unique keys.
assert_unique <- function(data, columns, label) {
  duplicate_count <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(columns)), name = 'keyCount') |>
    dplyr::filter(keyCount > 1L) |>
    base::nrow()
  if (duplicate_count > 0L) base::stop(base::paste0(label, ' contains ', duplicate_count, ' duplicated key(s).'), call. = FALSE)
  base::invisible(data)
}

# Validate finite values.
assert_finite <- function(data, columns, label) {
  invalid_count <- base::sum(!base::vapply(data[columns], function(x) base::all(base::is.finite(x)), base::logical(1L)))
  if (invalid_count > 0L) base::stop(base::paste0(label, ' contains non-finite modeled columns.'), call. = FALSE)
  base::invisible(data)
}

# Numeric Helpers ----------------------------------------------------------

# Standardize numeric vector.
standardize_vector <- function(x) {
  base::as.numeric(base::scale(x))
}

# Standardize complete numeric vector.
standardize_complete <- function(x) {
  x[!base::is.finite(x)] <- NA_real_
  x[base::is.na(x)]      <- stats::median(x, na.rm = TRUE)
  standardize_vector(x)
}

# Calculate safe rate.
rate_per_60 <- function(events, seconds) {
  dplyr::if_else(seconds > 0, events / seconds * 3600, NA_real_)
}

# Calculate age on October first.
calculate_season_age <- function(birth_date, season_id) {
  reference_date <- base::as.Date(base::paste0(season_id %/% 10000L, '-10-01'))
  birth_date     <- base::as.Date(birth_date)
  base::as.integer(base::format(reference_date, '%Y')) - base::as.integer(base::format(birth_date, '%Y')) - (base::format(reference_date, '%m%d') < base::format(birth_date, '%m%d'))
}

# Calculate following season identifier.
next_season_id <- function(season_id) {
  start_year <- season_id %/% 10000L + 1L
  base::as.integer(base::paste0(start_year, start_year + 1L))
}

# Calculate preceding season identifier.
previous_season_id <- function(season_id) {
  start_year <- season_id %/% 10000L - 1L
  base::as.integer(base::paste0(start_year, start_year + 1L))
}


# Data Collection Helpers --------------------------------------------------

# Load skater report.
load_skater_report <- function(season_id, category, game_type = 2L) {
  report <- base::suppressMessages(nhlscraper::skater_season_report(season = season_id, game_type = game_type, category = category))
  if (!base::is.data.frame(report) || base::nrow(report) == 0L) base::stop(base::paste0('Empty ', category, ' report for ', season_id, '.'), call. = FALSE)
  report
}

# Load player career history.
load_player_history <- function(player_id, attempts = 3L) {
  history <- NULL
  for (attempt in base::seq_len(attempts)) {
    candidate <- base::try(base::suppressMessages(nhlscraper::player_seasons(player = player_id)), silent = TRUE)
    if (base::is.data.frame(candidate) && base::all(base::c('seasonId', 'gameTypeIds') %in% base::names(candidate))) {
      history <- candidate |>
        dplyr::mutate(hasRegularSeason = purrr::map_lgl(gameTypeIds, function(game_types) 2L %in% game_types)) |>
        dplyr::filter(hasRegularSeason) |>
        dplyr::select(seasonId)
      break
    }
  }
  if (base::is.null(history) || base::nrow(history) == 0L) {
    for (attempt in base::seq_len(attempts)) {
      summary <- base::try(base::suppressMessages(nhlscraper::player_summary(player = player_id)), silent = TRUE)
      if (base::is.list(summary) && base::is.data.frame(summary$seasonTotals) && base::all(base::c('season', 'gameTypeId', 'gamesPlayed', 'leagueAbbrev') %in% base::names(summary$seasonTotals))) {
        history <- summary$seasonTotals |>
          dplyr::filter(leagueAbbrev == 'NHL', gameTypeId == 2L, gamesPlayed > 0) |>
          dplyr::transmute(seasonId = base::as.integer(season)) |>
          dplyr::distinct()
        break
      }
    }
  }
  if (base::is.null(history) || base::nrow(history) == 0L) base::stop(base::paste0('Player history failed for ', player_id, '.'), call. = FALSE)
  history |>
    dplyr::mutate(playerId = player_id) |>
    dplyr::select(playerId, seasonId)
}


# Contract Helpers ---------------------------------------------------------

# Normalize transaction text.
normalize_transaction_text <- function(x) {
  x |>
    stringi::stri_trans_general('Latin-ASCII') |>
    stringr::str_to_lower() |>
    stringr::str_replace_all('[^a-z0-9]+', ' ') |>
    stringr::str_squish()
}

# Build transaction clauses.
build_transaction_clauses <- function(transactions) {
  clauses <- purrr::map_dfr(base::seq_len(base::nrow(transactions)), function(index) {
    pieces <- stringr::str_split(transactions$description[index], '(?<=\\.)\\s+')[[1L]]
    tibble::tibble(date = transactions$date[index], teamTriCode = transactions$teamTriCode[index], description = transactions$description[index], clause = pieces)
  }) |>
    dplyr::mutate(
      date = base::as.Date(base::substr(date, 1L, 10L)),
      team = dplyr::recode(teamTriCode, NJ = 'NJD', TB = 'TBL', SJ = 'SJS', LA = 'LAK', WAS = 'WSH', MON = 'MTL', PHX = 'ARI', .default = teamTriCode),
      clauseNorm = normalize_transaction_text(clause),
      signingClause = stringr::str_detect(clauseNorm, '(^| )(signed|re signed|signs|re signs|agreed|agrees)( |$)|contract extension'),
      tryoutClause = stringr::str_detect(clauseNorm, 'professional tryout|pto contract|tryout contract')
    ) |>
    dplyr::filter(signingClause, !tryoutClause)
  clauses
}

# Calculate age on date.
calculate_date_age <- function(birth_date, reference_date) {
  birth_date     <- base::as.Date(birth_date)
  reference_date <- base::as.Date(reference_date)
  base::as.integer(base::format(reference_date, '%Y')) - base::as.integer(base::format(birth_date, '%Y')) - (base::format(reference_date, '%m%d') < base::format(birth_date, '%m%d'))
}

# Match contract announcements.
audit_contract_transactions <- function(contracts, clauses) {
  number_words <- base::c(one = 1L, two = 2L, three = 3L, four = 4L, five = 5L, six = 6L, seven = 7L, eight = 8L)
  purrr::map_dfr(base::seq_len(base::nrow(contracts)), function(index) {
    player_name  <- normalize_transaction_text(contracts$playerFullName[index])
    start_year   <- contracts$startSeasonId[index] %/% 10000L
    name_pattern <- base::paste0('(^| )', stringr::str_replace_all(player_name, ' ', ' +'), '( |$)')
    candidates <- clauses |>
      dplyr::filter(team == contracts$signedWithTeamTriCode[index], date >= base::as.Date(base::paste0(start_year - 2L, '-01-01')), date <= base::as.Date(base::paste0(start_year, '-12-31')), stringr::str_detect(clauseNorm, name_pattern))
    if (base::nrow(candidates) == 0L) base::return(tibble::tibble(contractRow = index, transactionCandidates = 0L, matchDate = base::as.Date(NA), matchedTerm = FALSE, matchedAge = FALSE, matchClause = NA_character_))
    term_word     <- base::names(number_words)[number_words == contracts$term[index]]
    term_pattern  <- base::paste0('(^| )(', contracts$term[index], '|', term_word, ') year( |$)')
    term_match    <- stringr::str_detect(candidates$clauseNorm, term_pattern)
    candidate_age <- calculate_date_age(contracts$birthDate[index], candidates$date)
    age_match     <- candidate_age == contracts$ageAtSigning[index]
    score         <- term_match * 2 + dplyr::coalesce(age_match, FALSE) * 4
    best          <- base::which(score == base::max(score, na.rm = TRUE))
    best          <- best[base::which.max(candidates$date[best])]
    tibble::tibble(contractRow = index, transactionCandidates = base::nrow(candidates), matchDate = candidates$date[best], matchedTerm = term_match[best], matchedAge = age_match[best], matchClause = candidates$clause[best])
  })
}


# Fit analysis workflow.
fit_analysis_workflow <- function(data, outcome, predictors, model_type = 'linear') {
  formula <- stats::reformulate(predictors, response = outcome)
  recipe  <- recipes::recipe(formula, data = data) |>
    recipes::step_dummy(recipes::all_nominal_predictors())
  model <- if (model_type == 'logistic') {
    parsnip::logistic_reg() |>
      parsnip::set_engine('glm')
  } else {
    parsnip::linear_reg() |>
      parsnip::set_engine('lm')
  }
  workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model) |>
    parsnip::fit(data = data)
}

# Inference Helpers --------------------------------------------------------

# Calculate normal-theory confidence multiplier.
confidence_multiplier <- function(level = confidence_level) {
  if (!base::is.numeric(level) || base::length(level) != 1L || !base::is.finite(level) || level <= 0 || level >= 1) base::stop('Confidence level must be a finite scalar between zero and one.', call. = FALSE)
  stats::qnorm((1 + level) / 2)
}

# Extract clustered coefficient inference.
clustered_term <- function(model, data, term = 'CSAx', level = confidence_level, cluster_data = NULL) {
  if (base::is.null(cluster_data)) cluster_data <- data$playerId
  engine         <- workflows::extract_fit_engine(model)
  variance       <- sandwich::vcovCL(engine, cluster = cluster_data, type = 'HC1')
  estimate       <- stats::coef(engine)[term]
  standard_error <- base::sqrt(base::diag(variance))[term]
  multiplier     <- confidence_multiplier(level)
  tibble::tibble(
    term = term,
    estimate = base::unname(estimate),
    stdError = base::unname(standard_error),
    statistic = base::unname(estimate / standard_error),
    pValue = base::unname(2 * stats::pnorm(-base::abs(estimate / standard_error))),
    confLow = base::unname(estimate - multiplier * standard_error),
    confHigh = base::unname(estimate + multiplier * standard_error)
  )
}

# Estimate clustered linear combination.
clustered_linear_combination <- function(model, data, weights, label, level = confidence_level) {
  engine         <- workflows::extract_fit_engine(model)
  estimates      <- stats::coef(engine)
  variance       <- sandwich::vcovCL(engine, cluster = data$playerId, type = 'HC1')
  missing_terms  <- base::setdiff(base::names(weights), base::names(estimates))
  if (base::length(missing_terms) > 0L) base::stop(base::paste0('Linear combination terms are unavailable: ', base::paste(missing_terms, collapse = ', '), '.'), call. = FALSE)
  contrast       <- stats::setNames(base::numeric(base::length(estimates)), base::names(estimates))
  contrast[base::names(weights)] <- weights
  estimate       <- base::sum(contrast * estimates)
  standard_error <- base::sqrt(base::drop(base::t(contrast) %*% variance %*% contrast))
  multiplier     <- confidence_multiplier(level)
  tibble::tibble(
    label = label,
    estimate = estimate,
    stdError = standard_error,
    statistic = estimate / standard_error,
    pValue = 2 * stats::pnorm(-base::abs(estimate / standard_error)),
    confLow = estimate - multiplier * standard_error,
    confHigh = estimate + multiplier * standard_error
  )
}

# Test clustered coefficient restrictions.
clustered_joint_wald <- function(model, data, terms) {
  engine        <- workflows::extract_fit_engine(model)
  estimates     <- stats::coef(engine)
  variance      <- sandwich::vcovCL(engine, cluster = data$playerId, type = 'HC1')
  missing_terms <- base::setdiff(terms, base::names(estimates))
  if (base::length(missing_terms) > 0L) base::stop(base::paste0('Wald-test terms are unavailable: ', base::paste(missing_terms, collapse = ', '), '.'), call. = FALSE)
  restriction          <- base::matrix(0, nrow = base::length(terms), ncol = base::length(estimates), dimnames = base::list(terms, base::names(estimates)))
  restriction[base::cbind(base::seq_along(terms), base::match(terms, base::names(estimates)))] <- 1
  restricted_estimates <- restriction %*% estimates
  restricted_variance  <- restriction %*% variance %*% base::t(restriction)
  statistic            <- base::drop(base::t(restricted_estimates) %*% base::solve(restricted_variance) %*% restricted_estimates)
  tibble::tibble(statistic = statistic, degreesFreedom = base::length(terms), pValue = stats::pchisq(statistic, df = base::length(terms), lower.tail = FALSE))
}

# Summarize analysis result.
summarize_analysis_result <- function(model, data, label, outcome, scale = 'linear', level = confidence_level, cluster_data = NULL) {
  result <- clustered_term(model, data, level = level, cluster_data = cluster_data) |>
    dplyr::mutate(label = label, outcome = outcome, sampleSize = base::nrow(data), players = dplyr::n_distinct(data$playerId), scale = scale, .before = 1L)
  if (scale == 'odds ratio') {
    result <- result |>
      dplyr::mutate(effect = base::exp(estimate), effectLow = base::exp(confLow), effectHigh = base::exp(confHigh))
  } else if (scale == 'percent') {
    result <- result |>
      dplyr::mutate(effect = 100 * (base::exp(estimate) - 1), effectLow = 100 * (base::exp(confLow) - 1), effectHigh = 100 * (base::exp(confHigh) - 1))
  } else {
    result <- result |>
      dplyr::mutate(effect = estimate, effectLow = confLow, effectHigh = confHigh)
  }
  result
}

# Estimate average logistic probabilities.
estimate_average_probabilities <- function(model, data, csax_values, contrast_values = base::c(-1, 1), level = confidence_level) {
  engine     <- workflows::extract_fit_engine(model)
  blueprint  <- workflows::extract_mold(model)$blueprint
  variance   <- sandwich::vcovCL(engine, cluster = data$playerId, type = 'HC1')
  terms      <- stats::delete.response(stats::terms(engine))
  estimates  <- stats::coef(engine)
  multiplier <- confidence_multiplier(level)
  # Summarize fixed CSAx value.
  build_summary <- function(csax_value) {
    prediction_data <- data |>
      dplyr::mutate(CSAx = csax_value)
    predictors     <- hardhat::forge(prediction_data, blueprint = blueprint)$predictors
    design         <- stats::model.matrix(terms, data = predictors)
    design         <- design[, base::names(estimates), drop = FALSE]
    probability    <- stats::plogis(base::as.vector(design %*% estimates))
    gradient       <- base::colMeans(design * probability * (1 - probability))
    standard_error <- base::sqrt(base::drop(base::t(gradient) %*% variance %*% gradient))
    base::list(
      summary = tibble::tibble(
        CSAx = csax_value,
        probability = base::mean(probability),
        stdError = standard_error,
        confLow = base::max(0, base::mean(probability) - multiplier * standard_error),
        confHigh = base::min(1, base::mean(probability) + multiplier * standard_error)
      ),
      gradient = gradient
    )
  }
  probability_parts <- purrr::map(csax_values, build_summary)
  contrast_parts    <- purrr::map(contrast_values, build_summary)
  probability_curve <- dplyr::bind_rows(purrr::map(probability_parts, 'summary'))
  contrast_estimate <- contrast_parts[[2L]]$summary$probability - contrast_parts[[1L]]$summary$probability
  contrast_gradient <- contrast_parts[[2L]]$gradient - contrast_parts[[1L]]$gradient
  contrast_error    <- base::sqrt(base::drop(base::t(contrast_gradient) %*% variance %*% contrast_gradient))
  contrast <- tibble::tibble(
    CSAxLow = contrast_values[[1L]],
    CSAxHigh = contrast_values[[2L]],
    estimate = contrast_estimate,
    stdError = contrast_error,
    confLow = contrast_estimate - multiplier * contrast_error,
    confHigh = contrast_estimate + multiplier * contrast_error
  )
  base::list(curve = probability_curve, contrast = contrast)
}


# Read hash-locked scouting ratings.
read_scouting_validation <- function(ratings_path = 'validation_private/external_validation_ratings.csv', lock_path = 'validation_private/external_validation_hashes.csv', key_path = 'validation_private/external_validation_key.csv') {
  expected_columns <- base::c('studyId', 'reportText', 'playsBiggerExplicit', 'activePhysicalEngagement', 'interiorPlay', 'overallPhysicality', 'notes')
  binary_columns   <- base::c('playsBiggerExplicit', 'activePhysicalEngagement', 'interiorPlay')
  if (!base::file.exists(ratings_path) || !base::file.exists(lock_path) || !base::file.exists(key_path)) base::stop('Completed scouting ratings, their lock file, and the private study key are required.', call. = FALSE)
  manifest <- readr::read_csv(lock_path, show_col_types = FALSE, col_types = readr::cols(artifact = readr::col_character(), file = readr::col_character(), sha256 = readr::col_character(), lockedAt = readr::col_date()))
  expected_files <- base::c('external_validation_packet.csv', 'external_validation_ratings.numbers', 'external_validation_ratings.csv', 'external_validation_key.csv')
  if (!base::identical(base::sort(manifest$file), base::sort(expected_files))) base::stop('External-validation lock manifest is incomplete or contains unexpected files.', call. = FALSE)
  assert_unique(manifest, 'file', 'External-validation lock manifest')
  manifest_paths <- base::file.path(base::dirname(lock_path), manifest$file)
  if (base::any(!base::file.exists(manifest_paths))) base::stop('A locked external-validation artifact is missing.', call. = FALSE)
  observed_hashes <- base::vapply(manifest_paths, function(path) digest::digest(file = path, algo = 'sha256'), base::character(1L))
  if (!base::identical(base::unname(observed_hashes), manifest$sha256)) base::stop('A locked external-validation artifact has changed.', call. = FALSE)
  lock <- manifest |>
    dplyr::filter(file == base::basename(ratings_path)) |>
    dplyr::select(file, sha256, lockedAt)
  if (base::nrow(lock) != 1L) base::stop('Completed scouting-ratings lock metadata is invalid.', call. = FALSE)
  ratings <- readr::read_csv(ratings_path, show_col_types = FALSE, col_types = readr::cols(studyId = readr::col_character(), reportText = readr::col_character(), playsBiggerExplicit = readr::col_integer(), activePhysicalEngagement = readr::col_integer(), interiorPlay = readr::col_integer(), overallPhysicality = readr::col_integer(), notes = readr::col_character()))
  if (!base::identical(base::names(ratings), expected_columns)) base::stop('Completed scouting-ratings columns changed.', call. = FALSE)
  assert_unique(ratings, 'studyId', 'Completed scouting ratings')
  if (base::nrow(ratings) != 40L || base::anyNA(ratings$studyId) || base::anyNA(ratings$reportText)) base::stop('Completed scouting ratings must contain 40 identified passages.', call. = FALSE)
  if (base::any(!purrr::map_lgl(ratings[binary_columns], function(values) base::all(values %in% base::c(0L, 1L))))) base::stop('A binary scouting rating uses an invalid or missing code.', call. = FALSE)
  if (base::anyNA(ratings$overallPhysicality) || base::any(!ratings$overallPhysicality %in% base::c(-1L, 0L, 1L))) base::stop('A scouting passage lacks a permitted overall rating.', call. = FALSE)
  planned_columns <- base::c('overallPhysicality', 'playsBiggerExplicit', 'activePhysicalEngagement', 'interiorPlay')
  missing_variation <- purrr::keep(planned_columns, function(column) dplyr::n_distinct(ratings[[column]]) < 2L)
  if (base::length(missing_variation) > 0L) base::stop(base::paste0('Fresh scouting coding requires consultation because planned comparisons lack variation: ', base::paste(missing_variation, collapse = ', '), '.'), call. = FALSE)
  key <- readr::read_csv(key_path, show_col_types = FALSE)
  assert_columns(key, base::c('studyId', 'playerId', 'redactedTextSha256'), 'Private scouting study key')
  assert_unique(key, 'studyId', 'Private scouting study key')
  if (!base::identical(base::sort(ratings$studyId), base::sort(key$studyId))) base::stop('Completed scouting ratings do not match the locked study key.', call. = FALSE)
  text_checks <- ratings |>
    dplyr::transmute(studyId, redactedTextSha256 = base::vapply(reportText, digest::digest, base::character(1L), algo = 'sha256', serialize = FALSE)) |>
    dplyr::inner_join(key |> dplyr::select(studyId, expectedRedactedTextSha256 = redactedTextSha256), by = 'studyId')
  if (base::nrow(text_checks) != 40L || base::any(text_checks$redactedTextSha256 != text_checks$expectedRedactedTextSha256)) base::stop('A blinded scouting passage changed during coding.', call. = FALSE)
  joined <- ratings |>
    dplyr::select(-reportText) |>
    dplyr::inner_join(key, by = 'studyId') |>
    dplyr::arrange(studyId)
  if (base::nrow(joined) != base::nrow(ratings) || base::anyNA(joined$playerId)) base::stop('Scouting ratings did not join completely to the study key.', call. = FALSE)
  base::list(data = joined, lock = lock)
}
