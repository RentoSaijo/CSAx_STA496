# Application Sources -----------------------------------------------------

# Define outcome scope and cache identity independently of score construction.
outcome_version <- '20260926-postseason-v2'
outcome_counts <- base::c('hits', 'hitsReceived', 'blockedShots', 'fights', 'contactPenaltiesTaken', 'contactPenaltiesDrawn')
outcome_scope <- 'NHL five on five; disjoint regular-season baseline and playoffs'

# Verify every pinned model file before expected-goal scoring.
verify_outcome_xg <- function(expected_source) {
  bundle <- utils::getFromNamespace('.xg_load_bundle', 'nhlscraper')()
  index <- bundle$model_index |> dplyr::filter(targetSeason %in% xs_behavior_seasons) |> dplyr::arrange(targetSeason, partition)
  expected <- expected_source$modelIndex |> dplyr::arrange(targetSeason, partition)
  if (!base::identical(base::as.data.frame(index), base::as.data.frame(expected))) base::stop('Expected-goal model index differs from pinned source.', call. = FALSE)
  resolve <- utils::getFromNamespace('.xg_resolve_booster_path', 'nhlscraper')
  index$fileSha256 <- purrr::map2_chr(index$vintage, index$partition, function(vintage, partition) digest::digest(file = resolve(base::paste(vintage, partition, sep = '_'), bundle), algo = 'sha256'))
  if (!base::all(index$fileSha256 == index$boosterSha256)) base::stop('Expected-goal model checksum mismatch.', call. = FALSE)
  base::list(name = expected_source$name, url = expected_source$url, bundleBuiltAt = bundle$built_at, models = index)
}

# Download complete season records before choosing any outcome window.
load_outcome_source <- function(season, refresh = FALSE) {
  path <- base::file.path('data/cache', base::paste0('application_raw_', season, '.rds'))
  if (!refresh && base::file.exists(path)) base::return(base::readRDS(path))
  base::message('Reading complete NHL application records for ', season, '.')
  result <- base::list(plays = nhlscraper::gc_play_by_plays(season), shifts = nhlscraper::shift_charts(season), rosters = nhlscraper::game_rosters(season), specialTeams = load_skater_report(season, 'timeonice'), collectedAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE))
  if (base::any(purrr::map_int(result[1:4], base::nrow) == 0L)) base::stop('NHL application source is empty.', call. = FALSE)
  base::saveRDS(result, path, compress = FALSE)
  result
}

# Merge overlapping shifts using identical rules to matched score exposure.
union_outcome_shifts <- function(shifts) {
  shifts |>
    dplyr::filter(!base::is.na(playerId), !base::is.na(teamId), startSecondsElapsedInPeriod >= 0, endSecondsElapsedInPeriod > startSecondsElapsedInPeriod) |>
    dplyr::arrange(gameId, teamId, playerId, periodNumber, startSecondsElapsedInPeriod, endSecondsElapsedInPeriod) |>
    dplyr::group_by(gameId, teamId, playerId, periodNumber) |>
    dplyr::mutate(shiftBlock = base::cumsum(startSecondsElapsedInPeriod > dplyr::lag(base::cummax(endSecondsElapsedInPeriod), default = -1L))) |>
    dplyr::group_by(gameId, teamId, playerId, periodNumber, shiftBlock) |>
    dplyr::summarise(startSecondsElapsedInPeriod = base::min(startSecondsElapsedInPeriod), endSecondsElapsedInPeriod = base::max(endSecondsElapsedInPeriod), .groups = 'drop')
}

# Attribute individual counts to verified roster actors.
outcome_event_counts <- function(plays, rosters) {
  definitions <- tibble::tribble(~measure, ~actor, ~event, ~sameTeam,
    'hits', 'hittingPlayerId', 'hit', TRUE,
    'hitsReceived', 'hitteePlayerId', 'hit', FALSE,
    'blockedShots', 'blockingPlayerId', 'blocked-shot', FALSE,
    'fights', 'committedByPlayerId', 'penalty', TRUE,
    'contactPenaltiesTaken', 'committedByPlayerId', 'penalty', TRUE,
    'contactPenaltiesDrawn', 'drawnByPlayerId', 'penalty', FALSE)
  parts <- purrr::map(base::seq_len(base::nrow(definitions)), function(index) {
    definition <- definitions[index, ]
    rows <- plays |> dplyr::filter(eventTypeDescKey == definition$event)
    if (definition$measure == 'fights') rows <- rows |> dplyr::filter(penaltyTypeDescKey == 'fighting')
    if (definition$measure %in% base::c('contactPenaltiesTaken', 'contactPenaltiesDrawn')) rows <- rows |> dplyr::filter(penaltyTypeDescKey %in% xs_contact_penalties)
    if (definition$measure == 'blockedShots') rows <- rows |> dplyr::filter(base::is.na(reason) | reason != 'teammate-blocked')
    rows <- rows |>
      dplyr::transmute(gameId, eventId, eventOwnerTeamId, playerId = .data[[definition$actor]]) |>
      dplyr::left_join(rosters |> dplyr::select(gameId, playerId, rosterTeamId = teamId, rosterPosition = positionCode), by = base::c('gameId', 'playerId')) |>
      dplyr::mutate(correctTeam = (rosterTeamId == eventOwnerTeamId) == definition$sameTeam, valid = !base::is.na(rosterTeamId) & correctTeam & rosterPosition != 'G')
    base::list(counts = rows |> dplyr::filter(valid) |> dplyr::distinct(gameId, eventId, playerId) |> dplyr::count(gameId, playerId, name = definition$measure), audit = rows |> dplyr::group_by(gameId) |> dplyr::summarise(measure = definition$measure, recorded = dplyr::n(), absentActor = base::sum(base::is.na(playerId)), missingRoster = base::sum(!base::is.na(playerId) & base::is.na(rosterTeamId)), goalieActor = base::sum(rosterPosition %in% 'G'), wrongTeam = base::sum(!base::is.na(rosterTeamId) & !correctTeam, na.rm = TRUE), .groups = 'drop'))
  })
  base::list(counts = purrr::map(parts, 'counts') |> purrr::reduce(dplyr::full_join, by = base::c('gameId', 'playerId')), audit = purrr::map_dfr(parts, 'audit'))
}

# Aggregate full scored game records to player and team five-on-five summaries.
prepare_outcome_season <- function(season, schedule, refresh = FALSE) {
  path <- base::file.path('data/cache', base::paste0('application_season_', season, '.rds'))
  if (!refresh && base::file.exists(path)) {
    cached <- base::readRDS(path)
    if (base::identical(cached$version, outcome_version)) base::return(cached)
  }
  raw <- load_outcome_source(season, refresh)
  games <- schedule |> dplyr::filter(seasonId == season, gameTypeId %in% base::c(2L, 3L), gameStateId == 7L)
  if (!base::all(games$gameId %in% raw$plays$gameId) || !base::all(games$gameId %in% raw$shifts$gameId) || !base::all(games$gameId %in% raw$rosters$gameId)) base::stop('Completed games lack outcome sources.', call. = FALSE)
  assert_unique(raw$plays, base::c('gameId', 'eventId'), 'Complete NHL events')
  base::message('Scoring complete games and deriving five-on-five exposure for ', season, '.')
  scored_path <- base::file.path('data/cache', base::paste0('application_scored_', season, '.rds'))
  scored <- if (!refresh && base::file.exists(scored_path)) base::readRDS(scored_path) else NULL
  if (base::is.null(scored) || !base::identical(scored[base::names(raw$plays)], raw$plays)) {
    scored <- nhlscraper::calculate_expected_goals(raw$plays)
    base::saveRDS(scored, scored_path, compress = FALSE)
  }
  assert_columns(scored, 'xG', 'Scored NHL events')
  shifts <- union_outcome_shifts(raw$shifts)
  exposure <- utils::getFromNamespace('.calculate_shift_times_by_situation_data', 'nhlscraper')(raw$plays, shifts) |>
    dplyr::group_by(gameId, teamId, playerId) |>
    dplyr::summarise(exposureSeconds = base::sum(.data[['1551TimeOnIce']]), .groups = 'drop')
  intervals <- utils::getFromNamespace('.shift_situation_intervals', 'nhlscraper')(raw$plays, shifts)
  clock <- intervals |> dplyr::filter(situationCode == 1551L) |> dplyr::group_by(gameId) |>
    dplyr::summarise(exposureSeconds = base::sum(endSecondsElapsedInPeriod - startSecondsElapsedInPeriod), .groups = 'drop')
  rosters <- raw$rosters |> dplyr::semi_join(games, by = 'gameId') |> dplyr::filter(positionCode != 'G') |> dplyr::select(gameId, playerId, teamId, positionCode)
  assert_unique(rosters, base::c('gameId', 'playerId'), 'Outcome skater rosters')
  plays <- scored |> dplyr::semi_join(games, by = 'gameId') |> dplyr::filter(periodType != 'SO', base::as.character(situationCode) == '1551')
  attributed <- outcome_event_counts(plays, raw$rosters)
  player_games <- rosters |> dplyr::left_join(exposure, by = base::c('gameId', 'teamId', 'playerId')) |>
    dplyr::left_join(attributed$counts, by = base::c('gameId', 'playerId')) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(outcome_counts), ~ dplyr::coalesce(.x, 0L)))
  shot_events <- plays |> dplyr::filter(eventTypeDescKey %in% base::c('goal', 'shot-on-goal', 'missed-shot')) |>
    dplyr::left_join(raw$rosters |> dplyr::select(gameId, shootingPlayerId = playerId, shooterPosition = positionCode), by = base::c('gameId', 'shootingPlayerId'))
  goalie_attempts <- shot_events |> dplyr::filter(shooterPosition %in% 'G') |> dplyr::count(gameId, teamId = eventOwnerTeamId, name = 'excludedGoalieAttempts')
  shots <- shot_events |> dplyr::filter(!shooterPosition %in% 'G') |>
    dplyr::group_by(gameId, teamId = eventOwnerTeamId) |>
    dplyr::summarise(unblockedAttempts = dplyr::n(), scoredAttempts = base::sum(base::is.finite(xG)), xGoals = if (base::all(base::is.finite(xG))) base::sum(xG) else NA_real_, .groups = 'drop')
  if (base::any(shots$unblockedAttempts != shots$scoredAttempts)) base::stop('Five-on-five attempts lack expected-goal predictions.', call. = FALSE)
  team_games <- dplyr::bind_rows(games |> dplyr::transmute(gameId, seasonId, gameTypeId, gameDate, teamId = homeTeamId, opponentId = visitingTeamId), games |> dplyr::transmute(gameId, seasonId, gameTypeId, gameDate, teamId = visitingTeamId, opponentId = homeTeamId)) |>
    dplyr::left_join(clock, by = 'gameId') |> dplyr::left_join(shots, by = base::c('gameId', 'teamId')) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(base::c('unblockedAttempts', 'scoredAttempts', 'xGoals')), ~ dplyr::coalesce(.x, 0))) |>
    dplyr::left_join(shots |> dplyr::select(gameId, opponentId = teamId, xGoalsAgainst = xGoals), by = base::c('gameId', 'opponentId')) |>
    dplyr::mutate(xGoalsAgainst = dplyr::coalesce(xGoalsAgainst, 0)) |>
    dplyr::left_join(goalie_attempts, by = base::c('gameId', 'teamId')) |>
    dplyr::mutate(excludedGoalieAttempts = dplyr::coalesce(excludedGoalieAttempts, 0L))
  assert_unique(team_games, base::c('gameId', 'teamId'), 'Team outcome games')
  assert_finite(team_games, base::c('exposureSeconds', 'xGoals', 'xGoalsAgainst'), 'Team outcome values')
  if (base::any(team_games$exposureSeconds <= 0)) base::stop('Outcome game lacks positive five-on-five exposure.', call. = FALSE)
  shift_clock <- player_games |> dplyr::group_by(gameId, teamId) |>
    dplyr::summarise(skaterSeconds = base::sum(exposureSeconds, na.rm = TRUE), missingPlayers = base::sum(base::is.na(exposureSeconds)), .groups = 'drop') |>
    dplyr::left_join(clock, by = 'gameId') |> dplyr::mutate(skaterClockRatio = skaterSeconds / (5 * exposureSeconds))
  special <- raw$specialTeams |> dplyr::transmute(playerId, seasonId, totalSeconds = timeOnIce, powerPlaySeconds = ppTimeOnIce, penaltyKillSeconds = shTimeOnIce, gamesPlayed)
  assert_unique(special, base::c('seasonId', 'playerId'), 'Special-teams ice time')
  invalid_time <- special |> dplyr::filter(totalSeconds < 0 | powerPlaySeconds < 0 | penaltyKillSeconds < 0 | powerPlaySeconds + penaltyKillSeconds > totalSeconds)
  if (base::nrow(invalid_time)) base::stop('Official special-teams exposure is invalid.', call. = FALSE)
  result <- base::list(version = outcome_version, playerGames = player_games, teamGames = team_games, specialTeams = special, attribution = attributed$audit, shiftClock = shift_clock,
    provenance = base::list(seasonId = season, collectedAt = raw$collectedAt, sourceHash = purrr::map_chr(raw[1:4], digest::digest, algo = 'sha256'), completeGames = base::nrow(games), completeEventsScored = base::nrow(scored), fiveOnFiveAttempts = base::sum(shots$unblockedAttempts), excludedGoalieAttempts = base::sum(goalie_attempts$excludedGoalieAttempts), missingXG = base::sum(shots$unblockedAttempts - shots$scoredAttempts), overlappingShiftSeconds = base::sum(raw$shifts$endSecondsElapsedInPeriod - raw$shifts$startSecondsElapsedInPeriod, na.rm = TRUE) - base::sum(shifts$endSecondsElapsedInPeriod - shifts$startSecondsElapsedInPeriod)))
  base::saveRDS(result, path, compress = 'xz')
  result
}

# Distinguish confirmed zero ice time from unresolved missing shift records.
reconcile_zero_exposure <- function(player_games, schedule, refresh = FALSE) {
  missing <- player_games |> dplyr::filter(!base::is.finite(exposureSeconds)) |>
    dplyr::left_join(schedule |> dplyr::select(gameId, homeTeamId), by = 'gameId')
  cache_path <- 'data/cache/zero_exposure_boxscores.rds'
  cached <- if (!refresh && base::file.exists(cache_path)) base::readRDS(cache_path) else base::list()
  records <- purrr::map(base::seq_len(base::nrow(missing)), function(index) {
    row <- missing[index, ]
    found <- base::which(purrr::map_lgl(cached, function(record) record$gameId == row$gameId && record$playerId == row$playerId))
    if (base::length(found)) base::return(cached[[found[1L]]])
    boxscore <- nhlscraper::boxscore(row$gameId, team = if (row$teamId == row$homeTeamId) 'home' else 'away', position = if (row$positionCode == 'D') 'defense' else 'forwards')
    base::list(gameId = row$gameId, playerId = row$playerId, teamId = row$teamId, boxscore = boxscore, collectedAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE))
  })
  base::saveRDS(records, cache_path, compress = 'xz')
  audit <- purrr::map_dfr(records, function(record) {
    matched <- record$boxscore |> dplyr::filter(playerId == record$playerId)
    recorded_time <- if (base::nrow(matched) == 1L) matched$toi else NA_character_
    tibble::tibble(gameId = record$gameId, playerId = record$playerId, teamId = record$teamId, boxscoreTime = recorded_time, confirmedZero = !base::is.na(recorded_time) && recorded_time == '00:00', sourceUrl = base::paste0('https://api-web.nhle.com/v1/gamecenter/', record$gameId, '/boxscore'), collectedAt = record$collectedAt, sourceSha256 = digest::digest(record$boxscore, algo = 'sha256'))
  })
  result <- player_games |> dplyr::left_join(audit |> dplyr::select(gameId, playerId, confirmedZero), by = base::c('gameId', 'playerId')) |>
    dplyr::mutate(exposureSeconds = dplyr::if_else(confirmedZero %in% TRUE, 0, exposureSeconds)) |> dplyr::select(-confirmedZero)
  if (base::any(base::rowSums(result[result$exposureSeconds %in% 0, outcome_counts, drop = FALSE]) > 0)) base::stop('Attributed events occur without measured five-on-five exposure.', call. = FALSE)
  base::list(playerGames = result, audit = audit)
}

# Freeze outcome windows, source provenance, and exposure without refitting scores.
prepare_application_inputs <- function(score_inputs, benchmark_inputs, refresh = FALSE) {
  if (!base::identical(utils::packageDescription('nhlscraper')[['RemoteSha']], xs_package_sha)) base::stop('NHL package revision differs from pinned source.', call. = FALSE)
  xg <- verify_outcome_xg(benchmark_inputs$provenance$inherited$expectedGoalSource)
  schedule_path <- 'data/cache/application_schedule.rds'
  if (refresh || !base::file.exists(schedule_path)) base::saveRDS(nhlscraper::games(), schedule_path)
  schedule <- base::readRDS(schedule_path)
  parts <- purrr::map(xs_behavior_seasons, function(season) prepare_outcome_season(season, schedule, refresh))
  excluded_games <- score_inputs$sourceRows |> dplyr::filter(rowStatus == 'Retained') |> dplyr::distinct(gameId)
  team_games <- purrr::map_dfr(parts, 'teamGames') |>
    dplyr::mutate(gameDate = base::as.Date(gameDate), scoreConstructionGame = gameId %in% excluded_games$gameId) |>
    dplyr::arrange(seasonId, teamId, gameTypeId, gameDate, gameId) |>
    dplyr::group_by(seasonId, teamId, gameTypeId) |>
    dplyr::mutate(playoffGameNumber = dplyr::if_else(gameTypeId == 3L, dplyr::row_number(), NA_integer_)) |> dplyr::ungroup() |>
    dplyr::mutate(baseline = gameTypeId == 2L & !scoreConstructionGame, firstFour = gameTypeId == 3L & playoffGameNumber <= 4L)
  if (base::any(team_games$baseline & team_games$scoreConstructionGame)) base::stop('Outcome baseline overlaps score construction.', call. = FALSE)
  playoff_windows <- team_games |> dplyr::filter(gameTypeId == 3L) |> dplyr::group_by(seasonId, teamId) |> dplyr::summarise(firstFourGames = base::sum(firstFour), .groups = 'drop')
  if (base::any(playoff_windows$firstFourGames != 4L) || base::nrow(playoff_windows) != 64L) base::stop('Playoff windows do not cover four games for every qualifier.', call. = FALSE)
  reconciled <- reconcile_zero_exposure(purrr::map_dfr(parts, 'playerGames'), schedule, refresh)
  player_games <- reconciled$playerGames |>
    dplyr::left_join(team_games |> dplyr::select(gameId, teamId, seasonId, gameTypeId, baseline, firstFour, playoffGameNumber, scoreConstructionGame), by = base::c('gameId', 'teamId'))
  matched_exposure <- score_inputs$sourceRows |> dplyr::filter(rowStatus == 'Retained') |>
    dplyr::select(gameId, playerId, scoreSeconds = nhlSeconds) |>
    dplyr::left_join(player_games |> dplyr::select(gameId, playerId, exposureSeconds), by = base::c('gameId', 'playerId')) |>
    dplyr::mutate(differenceSeconds = exposureSeconds - scoreSeconds)
  if (base::any(!base::is.finite(matched_exposure$differenceSeconds))) base::stop('Matched score observations lack outcome exposure.', call. = FALSE)
  base::list(version = outcome_version, playerGames = player_games, teamGames = team_games, specialTeams = purrr::map_dfr(parts, 'specialTeams'),
    provenance = base::list(packageSha = xs_package_sha, expectedGoals = xg, seasons = purrr::map(parts, 'provenance'), scoreConstructionGameIds = excluded_games$gameId, scheduleSha256 = digest::digest(schedule, algo = 'sha256'), collectedAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE)),
    zeroExposureRecords = reconciled$audit, attribution = purrr::map_dfr(parts, 'attribution'), shiftClock = purrr::map_dfr(parts, 'shiftClock'), scoreExposureComparison = matched_exposure)
}

# Player Engagement -------------------------------------------------------

# Pair positive exposure with playoff-team regular-season games only.
prepare_contact_panel <- function(application_inputs, panel, window = 'First four') {
  games <- application_inputs$playerGames |>
    dplyr::filter(baseline | (gameTypeId == 3L & (window == 'Full postseason' | firstFour))) |>
    dplyr::mutate(period = dplyr::if_else(baseline, 'Baseline', 'Postseason'))
  playoff_teams <- games |> dplyr::filter(period == 'Postseason', exposureSeconds > 0) |> dplyr::distinct(playerId, seasonId, teamId)
  assert_unique(playoff_teams, base::c('playerId', 'seasonId'), 'Player playoff teams')
  periods <- games |> dplyr::inner_join(playoff_teams, by = base::c('playerId', 'seasonId', 'teamId')) |>
    dplyr::group_by(playerId, seasonId, teamId, period) |>
    dplyr::summarise(games = dplyr::n_distinct(gameId), missingExposureGames = base::sum(!base::is.finite(exposureSeconds)), exposureSeconds = base::sum(exposureSeconds), dplyr::across(dplyr::all_of(outcome_counts), base::sum), .groups = 'drop') |>
    dplyr::inner_join(panel |> dplyr::select(model, referencePopulation, playerId, seasonId, dplyr::all_of(primary_predictors)), by = base::c('playerId', 'seasonId'))
  eligibility <- periods |> dplyr::group_by(model, playerId, seasonId) |>
    dplyr::summarise(paired = dplyr::n() == 2L & base::all(base::is.finite(exposureSeconds) & exposureSeconds > 0), .groups = 'drop')
  data <- periods |> dplyr::inner_join(eligibility |> dplyr::filter(paired), by = base::c('model', 'playerId', 'seasonId')) |>
    dplyr::mutate(window = window, postseason = base::as.integer(period == 'Postseason'), playerSeason = base::factor(base::paste(playerId, seasonId, sep = '_')), logExposure = base::log(exposureSeconds / 3600))
  selection <- panel |> dplyr::left_join(eligibility, by = base::c('model', 'playerId', 'seasonId')) |>
    dplyr::group_by(model) |> dplyr::summarise(scoredPlayerSeasons = dplyr::n(), playoffParticipants = base::sum(!base::is.na(paired)), pairedPlayerSeasons = base::sum(paired %in% TRUE), .groups = 'drop') |> dplyr::mutate(window = window)
  if (base::anyDuplicated(data[base::c('model', 'playerId', 'seasonId', 'period')])) base::stop('Player-period engagement rows are duplicated.', call. = FALSE)
  base::list(panel = data, selection = selection)
}

# Estimate conditional count changes with player-season effects and offsets.
fit_contact_counts <- function(data, measure) {
  totals <- data |> dplyr::group_by(playerSeason) |> dplyr::summarise(events = base::sum(.data[[measure]]), .groups = 'drop')
  informative <- data |> dplyr::semi_join(totals |> dplyr::filter(events > 0), by = 'playerSeason') |> base::droplevels()
  summary <- data |> dplyr::group_by(period) |>
    dplyr::summarise(count = base::sum(.data[[measure]]), exposureSeconds = base::sum(exposureSeconds), games = base::sum(games), playerSeasons = dplyr::n(), players = dplyr::n_distinct(playerId), ratePer60 = rate_per_60(count, exposureSeconds), .groups = 'drop')
  controls <- base::setdiff(primary_predictors, 'CSAx')
  matrix <- stats::model.matrix(stats::reformulate(controls), informative)[, -1L, drop = FALSE]
  interactions <- base::paste0('post_', base::make.names(base::colnames(matrix)))
  informative[interactions] <- base::as.data.frame(matrix * informative$postseason)
  informative$post_CSAx <- informative$postseason * informative$CSAx
  informative$count <- informative[[measure]]
  terms <- base::c('postseason', 'post_CSAx', interactions)
  formula <- stats::reformulate(base::c('playerSeason', terms, 'offset(logExposure)'), response = 'count')
  if (!base::requireNamespace('poissonreg', quietly = TRUE)) base::stop('Restore pinned poissonreg before fitting counts.', call. = FALSE)
  fit <- workflows::workflow() |>
    workflows::add_variables(outcomes = dplyr::all_of('count'), predictors = dplyr::all_of(base::c('playerSeason', terms, 'logExposure'))) |>
    workflows::add_model(parsnip::poisson_reg() |> parsnip::set_engine('glm'), formula = formula) |>
    parsnip::fit(data = informative)
  engine <- workflows::extract_fit_engine(fit)
  if (!base::isTRUE(engine$converged) || base::any(!base::is.finite(stats::coef(engine)[terms]))) base::stop('Engagement contrast is unidentified or unconverged.', call. = FALSE)
  variance <- sandwich::vcovCL(engine, cluster = informative$playerId, type = 'HC1')[terms, terms, drop = FALSE]
  estimates <- stats::coef(engine)[terms]
  contrast <- function(weights) {
    value <- base::sum(weights * estimates)
    se <- base::sqrt(base::drop(base::t(weights) %*% variance %*% weights))
    tibble::tibble(estimate = value, stdError = se, confLow = value - confidence_multiplier() * se, confHigh = value + confidence_multiplier() * se, pValue = 2 * stats::pnorm(-base::abs(value / se)), effect = base::exp(estimate), effectLow = base::exp(confLow), effectHigh = base::exp(confHigh))
  }
  weights <- stats::setNames(base::numeric(base::length(terms)), terms)
  weights['post_CSAx'] <- 1
  coefficient <- contrast(weights) |> dplyr::mutate(statistic = 'Postseason-to-baseline rate ratio multiplier per CSAx SD', scale = 'rate ratio', sampleSize = base::nrow(informative) / 2L, players = dplyr::n_distinct(informative$playerId), pairedPlayerSeasons = base::nrow(data) / 2L, zeroTotalStrata = base::sum(totals$events == 0), status = 'Estimated')
  baseline <- informative$postseason == 0
  weights['postseason'] <- 1
  weights[interactions] <- base::colMeans(matrix[baseline, , drop = FALSE])
  curve <- purrr::map_dfr(base::c(-1, 0, 1), function(value) {
    weights['post_CSAx'] <- value
    contrast(weights) |> dplyr::mutate(CSAx = value, statistic = 'Adjusted postseason-to-baseline rate ratio', scale = 'rate ratio', sampleSize = base::nrow(informative) / 2L, rateChangePercent = 100 * (effect - 1))
  })
  base::list(estimates = coefficient, rateChanges = curve, descriptive = summary)
}

# Analyze primary contacts and descriptive sparse events across fixed windows.
analyze_postseason_engagement <- function(application_inputs, panel) {
  windows <- base::c('First four', 'Full postseason')
  samples <- stats::setNames(purrr::map(windows, function(window) prepare_contact_panel(application_inputs, panel, window)), windows)
  parts <- purrr::imap(samples, function(sample, window) {
    by_position <- purrr::imap(base::split(sample$panel, sample$panel$model), function(data, population) {
      measures <- if (window == 'First four') base::c('hits', 'hitsReceived', 'blockedShots') else 'hits'
      estimates <- purrr::map(measures, function(measure) {
        base::message('Estimating ', population, ' ', measure, ' in ', window, ' window.')
        fitted <- fit_contact_counts(data, measure)
        purrr::map(fitted, function(table) table |> dplyr::mutate(model = population, referencePopulation = population, outcome = measure, window = window, .before = 1L))
      })
      descriptive <- data |> dplyr::group_by(period) |>
        dplyr::summarise(exposureSeconds = base::sum(exposureSeconds), games = base::sum(games), playerSeasons = dplyr::n(), players = dplyr::n_distinct(playerId), dplyr::across(dplyr::all_of(outcome_counts), base::sum), .groups = 'drop') |>
        tidyr::pivot_longer(dplyr::all_of(outcome_counts), names_to = 'outcome', values_to = 'count') |>
        dplyr::mutate(model = population, referencePopulation = population, window = window, ratePer60 = rate_per_60(count, exposureSeconds))
      base::list(estimates = purrr::map_dfr(estimates, 'estimates'), rateChanges = purrr::map_dfr(estimates, 'rateChanges'), descriptive = descriptive)
    })
    purrr::map(stats::setNames(base::c('estimates', 'rateChanges', 'descriptive'), base::c('estimates', 'rateChanges', 'descriptive')), function(component) purrr::map_dfr(by_position, component))
  })
  result <- purrr::map(stats::setNames(base::c('estimates', 'rateChanges', 'descriptive'), base::c('estimates', 'rateChanges', 'descriptive')), function(component) purrr::map_dfr(parts, component) |>
    dplyr::mutate(specification = a3z_specification, eventScope = a3z_scope, outcomeScope = outcome_scope, roleTiming = 'Current regular season', intervalMethod = 'Player-clustered HC1; conditional on estimated scores and positive-exposure pairs'))
  result$selection <- purrr::map_dfr(samples, 'selection')
  result$panel <- purrr::map_dfr(samples, 'panel') |> dplyr::select(model, playerId, seasonId, teamId, window, period, games, exposureSeconds, dplyr::all_of(outcome_counts), CSAx)
  result
}

# Team Chance Creation ----------------------------------------------------

# Construct positional team scores across all teams before playoff selection.
prepare_team_scores <- function(application_inputs, predictions, teams) {
  player_time <- application_inputs$playerGames |> dplyr::filter(gameTypeId == 2L) |>
    dplyr::group_by(seasonId, teamId, playerId) |>
    dplyr::summarise(exposureSeconds = base::sum(exposureSeconds), rosterDefenseman = base::mean(positionCode == 'D') >= 0.5, .groups = 'drop') |>
    dplyr::left_join(predictions |> dplyr::select(seasonId, playerId, scoreModel = model, CSAx), by = base::c('seasonId', 'playerId')) |>
    dplyr::mutate(model = dplyr::coalesce(scoreModel, dplyr::if_else(rosterDefenseman, 'Defensemen', 'Forwards')))
  scores <- player_time |> dplyr::group_by(seasonId, teamId, model) |>
    dplyr::summarise(playerSeconds = base::sum(exposureSeconds, na.rm = TRUE), missingExposurePlayers = base::sum(!base::is.finite(exposureSeconds)), scoredSeconds = base::sum(exposureSeconds[base::is.finite(CSAx)], na.rm = TRUE), scoredPlayers = base::sum(base::is.finite(CSAx)), players = dplyr::n(), teamCSAx = stats::weighted.mean(CSAx[base::is.finite(CSAx) & base::is.finite(exposureSeconds)], exposureSeconds[base::is.finite(CSAx) & base::is.finite(exposureSeconds)]), .groups = 'drop') |>
    dplyr::mutate(scoredCoverage = scoredSeconds / playerSeconds) |>
    dplyr::group_by(seasonId, model) |>
    dplyr::mutate(referenceTeams = dplyr::n(), teamReferenceMean = base::mean(teamCSAx), teamReferenceSd = stats::sd(teamCSAx), teamScore = (teamCSAx - teamReferenceMean) / teamReferenceSd) |>
    dplyr::ungroup() |> dplyr::left_join(teams |> dplyr::select(teamId, teamTriCode, dplyr::any_of('franchiseId')), by = 'teamId')
  if (base::any(scores$referenceTeams != 32L) || base::any(!base::is.finite(scores$teamScore))) base::stop('Team-score reference does not cover all 32 teams.', call. = FALSE)
  scores
}

# Match opponent strength using regular-season defense excluding focal head-to-head games.
add_opponent_strength <- function(team_games) {
  regular <- team_games |> dplyr::filter(gameTypeId == 2L)
  totals <- regular |> dplyr::group_by(seasonId, teamId) |> dplyr::summarise(regularXGA = base::sum(xGoalsAgainst), regularSeconds = base::sum(exposureSeconds), .groups = 'drop')
  head_to_head <- regular |> dplyr::group_by(seasonId, teamId, opponentId) |> dplyr::summarise(headToHeadXGA = base::sum(xGoalsAgainst), headToHeadSeconds = base::sum(exposureSeconds), .groups = 'drop')
  strength <- head_to_head |> dplyr::left_join(totals, by = base::c('seasonId', 'teamId')) |>
    dplyr::transmute(seasonId, focalTeamId = opponentId, opponentId = teamId, opponentXGA60 = rate_per_60(regularXGA - headToHeadXGA, regularSeconds - headToHeadSeconds)) |> dplyr::rename(teamId = focalTeamId)
  result <- team_games |> dplyr::left_join(strength, by = base::c('seasonId', 'teamId', 'opponentId'))
  assert_finite(result, 'opponentXGA60', 'Regular-season opponent strength')
  result
}

# Summarize signed postseason changes with counts and unequal exposure retained.
prepare_team_changes <- function(team_games, scores, window) {
  qualifier <- team_games |> dplyr::filter(gameTypeId == 3L) |> dplyr::distinct(seasonId, teamId)
  periods <- team_games |> dplyr::semi_join(qualifier, by = base::c('seasonId', 'teamId')) |>
    dplyr::filter(baseline | (gameTypeId == 3L & (window == 'Full postseason' | firstFour))) |>
    dplyr::mutate(period = dplyr::if_else(baseline, 'Baseline', 'Postseason')) |>
    dplyr::group_by(seasonId, teamId, period) |>
    dplyr::summarise(opponentXGA60 = stats::weighted.mean(opponentXGA60, exposureSeconds), games = dplyr::n(), exposureSeconds = base::sum(exposureSeconds), attempts = base::sum(unblockedAttempts), xGoals = base::sum(xGoals), .groups = 'drop') |>
    dplyr::mutate(xGF60 = rate_per_60(xGoals, exposureSeconds), attempts60 = rate_per_60(attempts, exposureSeconds), quality100 = 100 * xGoals / attempts)
  score_values <- scores |> dplyr::select(seasonId, teamId, model, teamScore, scoredCoverage, missingExposurePlayers) |>
    tidyr::pivot_wider(names_from = model, values_from = base::c('teamScore', 'scoredCoverage', 'missingExposurePlayers'))
  changes <- periods |> tidyr::pivot_wider(names_from = period, values_from = base::c('games', 'exposureSeconds', 'attempts', 'xGoals', 'opponentXGA60', 'xGF60', 'attempts60', 'quality100')) |>
    dplyr::left_join(score_values, by = base::c('seasonId', 'teamId')) |>
    dplyr::mutate(window = window, deltaXGF60 = xGF60_Postseason - xGF60_Baseline, deltaAttempts60 = attempts60_Postseason - attempts60_Baseline, deltaQuality100 = quality100_Postseason - quality100_Baseline, absoluteQualityChange = base::abs(deltaQuality100), deltaOpponentXGA60 = opponentXGA60_Postseason - opponentXGA60_Baseline,
      timeWeight = 1 / (1 / exposureSeconds_Baseline + 1 / exposureSeconds_Postseason), attemptWeight = 1 / (1 / attempts_Baseline + 1 / attempts_Postseason), seasonFactor = base::factor(seasonId),
      included = scoredCoverage_Forwards >= 0.80 & scoredCoverage_Defensemen >= 0.80 & missingExposurePlayers_Forwards == 0L & missingExposurePlayers_Defensemen == 0L & exposureSeconds_Baseline > 0 & exposureSeconds_Postseason > 0,
      exclusion = dplyr::case_when(missingExposurePlayers_Forwards > 0L | missingExposurePlayers_Defensemen > 0L ~ 'Missing regular-season player exposure', scoredCoverage_Forwards < 0.80 & scoredCoverage_Defensemen < 0.80 ~ 'Both positions below 80% scored ice time', scoredCoverage_Forwards < 0.80 ~ 'Forwards below 80% scored ice time', scoredCoverage_Defensemen < 0.80 ~ 'Defensemen below 80% scored ice time', !included ~ 'Missing period exposure', TRUE ~ ''))
  base::list(periods = periods |> dplyr::mutate(window = window), changes = changes)
}

# Estimate weighted team changes with franchise-clustered Student intervals.
fit_team_change <- function(data, outcome, weight) {
  data <- data |> dplyr::filter(included) |> base::droplevels()
  predictors <- base::c('teamScore_Forwards', 'teamScore_Defensemen', 'seasonFactor', 'deltaOpponentXGA60')
  assert_finite(data, base::c(outcome, weight, 'teamScore_Forwards', 'teamScore_Defensemen', 'deltaOpponentXGA60', 'franchiseId'), 'Joint team model')
  data$exposureWeight <- hardhat::importance_weights(data[[weight]] / base::mean(data[[weight]]))
  formula <- stats::reformulate(predictors, response = outcome)
  fit <- workflows::workflow() |>
    workflows::add_formula(formula) |>
    workflows::add_case_weights(exposureWeight) |>
    workflows::add_model(parsnip::linear_reg() |> parsnip::set_engine('lm')) |>
    parsnip::fit(data = data)
  engine <- workflows::extract_fit_engine(fit)
  clusters <- dplyr::n_distinct(data$franchiseId)
  variance <- sandwich::vcovCL(engine, cluster = data$franchiseId, type = 'HC1')
  terms <- base::c('teamScore_Forwards', 'teamScore_Defensemen')
  if (engine$rank != base::length(stats::coef(engine)) || clusters <= 1L) base::stop('Team change contrast is unidentified.', call. = FALSE)
  estimate <- stats::coef(engine)[terms]
  se <- base::sqrt(base::diag(variance))[terms]
  multiplier <- stats::qt((1 + confidence_level) / 2, df = clusters - 1L)
  tibble::tibble(model = base::c('Forwards', 'Defensemen'), referencePopulation = 'All 32 teams within season; positional scores separate', outcome = outcome, statistic = 'Signed outcome change per positional team-score SD', scale = 'outcome units', effect = base::unname(estimate), stdError = base::unname(se), effectLow = effect - multiplier * stdError, effectHigh = effect + multiplier * stdError, pValue = 2 * stats::pt(-base::abs(effect / stdError), df = clusters - 1L), sampleSize = base::nrow(data), franchises = clusters, degreesFreedom = clusters - 1L, weight = weight, status = 'Estimated')
}

# Relate positional team scores to primary and decomposed chance-creation changes.
analyze_team_chances <- function(application_inputs, predictions, teams) {
  scores <- prepare_team_scores(application_inputs, predictions, teams)
  team_games <- add_opponent_strength(application_inputs$teamGames)
  parts <- purrr::map(base::c('First four', 'Full postseason'), function(window) {
    prepared <- prepare_team_changes(team_games, scores, window)
    prepared$changes <- prepared$changes |> dplyr::left_join(scores |> dplyr::distinct(teamId, teamTriCode, franchiseId), by = 'teamId')
    outcomes <- if (window == 'First four') base::c('deltaXGF60', 'deltaAttempts60', 'deltaQuality100') else 'deltaXGF60'
    estimates <- purrr::map_dfr(outcomes, function(outcome) fit_team_change(prepared$changes, outcome, if (outcome == 'deltaQuality100') 'attemptWeight' else 'timeWeight')) |> dplyr::mutate(window = window)
    base::list(changes = prepared$changes, periods = prepared$periods, estimates = estimates)
  })
  result <- base::list(scores = scores, changes = purrr::map_dfr(parts, 'changes'), periods = purrr::map_dfr(parts, 'periods'), estimates = purrr::map_dfr(parts, 'estimates'))
  result$estimates <- result$estimates |> dplyr::mutate(specification = a3z_specification, eventScope = a3z_scope, outcomeScope = outcome_scope, roleTiming = 'Regular-season five-on-five team weights', intervalMethod = 'Franchise-clustered HC1 with t(G-1); conditional on estimated scores and coverage-qualified teams')
  result
}

# Combined Applications ---------------------------------------------------

# Reuse completed application estimates when scores, inputs, and methods match.
analyze_focused_applications <- function(predictions, application_inputs, benchmark_inputs) {
  if (!base::identical(application_inputs$version, outcome_version)) base::stop('Prepare current application inputs before fitting outcomes.', call. = FALSE)
  assert_unique(predictions, base::c('playerId', 'seasonId'), 'Native positional scores')
  panel <- application_panel(predictions, prepare_outcomes(benchmark_inputs))
  signature <- digest::digest(base::list(predictions = predictions, panel = panel, inputs = application_inputs, code = base::c(readr::read_file('R/postseason.R'), readr::read_file('R/applications.R'), readr::read_file('R/functions.R'))), algo = 'sha256')
  path <- 'data/cache/focused_applications.rds'
  cached <- if (base::file.exists(path)) base::readRDS(path) else NULL
  if (base::identical(cached$signature, signature)) base::return(cached)
  result <- base::list(signature = signature, roleTiming = analyze_role_timing(panel), playingTime = analyze_returner_ice_time(panel), deployment = analyze_deployment(panel, application_inputs$specialTeams), engagement = analyze_postseason_engagement(application_inputs, panel))
  base::saveRDS(result, path, compress = 'xz')
  result
}
