# Event Features -----------------------------------------------------------

# Calculate share with explicit opportunity denominator.
event_share <- function(events, opportunities) {
  dplyr::if_else(opportunities > 0, events / opportunities, NA_real_)
}

# Count attributed player events.
count_events <- function(data, player_column, count_name) {
  data |>
    dplyr::filter(!base::is.na(.data[[player_column]])) |>
    dplyr::distinct(gameId, eventId, .data[[player_column]], .keep_all = TRUE) |>
    dplyr::count(playerId = .data[[player_column]], name = count_name)
}

# Orient coordinates toward event owner's attacking end.
prepare_coordinates <- function(plays) {
  plays |>
    dplyr::mutate(
      recordedXCoordNorm = xCoordNorm,
      coordinateValid = base::is.finite(xCoord) & base::is.finite(yCoord) & base::abs(xCoord) <= 100 & base::abs(yCoord) <= 43,
      directionKnown = homeTeamDefendingSide %in% base::c('left', 'right') & !base::is.na(isHome),
      attackDirection = dplyr::if_else(isHome == (homeTeamDefendingSide == 'left'), 1, -1),
      xCoordNorm = dplyr::if_else(coordinateValid & directionKnown, attackDirection * xCoord, NA_real_),
      yCoordNorm = dplyr::if_else(coordinateValid & directionKnown, attackDirection * yCoord, NA_real_)
    )
}

# Summarize recorded actor attribution against game rosters.
summarize_event_attribution <- function(plays, team_keys, season_id) {
  definitions <- tibble::tribble(~actor, ~event, ~sameTeam, 'hittingPlayerId', 'hit', TRUE, 'hitteePlayerId', 'hit', FALSE, 'blockingPlayerId', 'blocked-shot', FALSE, 'committedByPlayerId', 'penalty', TRUE, 'drawnByPlayerId', 'penalty', FALSE)
  purrr::map_dfr(base::seq_len(base::nrow(definitions)), function(index) {
    actor <- definitions$actor[index]
    event_rows <- plays |> dplyr::filter(eventTypeDescKey == definitions$event[index])
    if (definitions$event[index] == 'penalty') event_rows <- event_rows |> dplyr::filter(penaltyTypeDescKey %in% xs_contact_penalties)
    if (definitions$event[index] == 'blocked-shot') event_rows <- event_rows |> dplyr::filter(base::is.na(reason) | reason != 'teammate-blocked')
    event_rows <- event_rows |> dplyr::filter(!base::is.na(.data[[actor]])) |> dplyr::mutate(actorId = .data[[actor]]) |> dplyr::left_join(team_keys, by = base::c('gameId', 'actorId' = 'playerId'))
    tibble::tibble(seasonId = season_id, actor = actor, events = base::nrow(event_rows), missingRoster = base::sum(base::is.na(event_rows$rosterTeamId)), wrongTeam = base::sum((event_rows$rosterTeamId == event_rows$eventOwnerTeamId) != definitions$sameTeam[index], na.rm = TRUE))
  })
}

# Aggregate direct and positional indirect event measures.
aggregate_behavior <- function(plays, exposure, season_id, scope) {
  event_counts <- base::list(
    count_events(plays |> dplyr::filter(eventTypeDescKey == 'hit'), 'hittingPlayerId', 'hits'),
    count_events(plays |> dplyr::filter(eventTypeDescKey == 'hit'), 'hitteePlayerId', 'hitsReceived'),
    count_events(plays |> dplyr::filter(eventTypeDescKey == 'blocked-shot', base::is.na(reason) | reason != 'teammate-blocked'), 'blockingPlayerId', 'blockedShots'),
    count_events(plays |> dplyr::filter(eventTypeDescKey == 'penalty', penaltyTypeDescKey == 'fighting'), 'committedByPlayerId', 'fights'),
    count_events(plays |> dplyr::filter(eventTypeDescKey == 'penalty', penaltyTypeDescKey %in% xs_contact_penalties), 'committedByPlayerId', 'contactPenaltiesTaken'),
    count_events(plays |> dplyr::filter(eventTypeDescKey == 'penalty', penaltyTypeDescKey %in% xs_contact_penalties), 'drawnByPlayerId', 'contactPenaltiesDrawn')
  ) |>
    purrr::reduce(dplyr::full_join, by = 'playerId')
  unblocked <- plays |>
    dplyr::filter(eventTypeDescKey %in% base::c('goal', 'shot-on-goal', 'missed-shot'), !base::is.na(shootingPlayerId)) |>
    dplyr::group_by(playerId = shootingPlayerId) |>
    dplyr::summarise(
      unblockedAttempts = dplyr::n(),
      reboundAttempts = base::sum(isRebound %in% TRUE),
      locatedAttempts = base::sum(base::is.finite(xCoordNorm) & base::is.finite(yCoordNorm)),
      netFrontAttempts = base::sum(xCoordNorm >= 82 & xCoordNorm <= 89 & base::abs(yCoordNorm) <= 8, na.rm = TRUE),
      medianShotDistance = if (base::any(base::is.finite(xCoordNorm))) stats::median(base::sqrt((89 - xCoordNorm)^2 + yCoordNorm^2), na.rm = TRUE) else NA_real_,
      .groups = 'drop'
    )
  shot_types <- plays |>
    dplyr::filter(eventTypeDescKey %in% base::c('goal', 'shot-on-goal'), !base::is.na(shootingPlayerId), !base::is.na(shotType), base::nzchar(shotType)) |>
    dplyr::group_by(playerId = shootingPlayerId) |>
    dplyr::summarise(typedShotsOnNet = dplyr::n(), backhandShots = base::sum(shotType == 'backhand'), deflectionShots = base::sum(shotType %in% base::c('deflected', 'tip-in')), .groups = 'drop')
  takeaways <- plays |>
    dplyr::filter(eventTypeDescKey == 'takeaway', !base::is.na(playerId)) |>
    dplyr::group_by(playerId) |>
    dplyr::summarise(takeaways = dplyr::n(), locatedTakeaways = base::sum(base::is.finite(xCoordNorm)), defensiveTakeaways = base::sum(xCoordNorm < -25, na.rm = TRUE), defensivePerimeterTakeaways = base::sum(xCoordNorm < -25 & (base::abs(yCoordNorm) >= 32.5 | xCoordNorm <= -89), na.rm = TRUE), .groups = 'drop')
  count_columns <- base::c('hits', 'hitsReceived', 'blockedShots', 'fights', 'contactPenaltiesTaken', 'contactPenaltiesDrawn', 'unblockedAttempts', 'reboundAttempts', 'locatedAttempts', 'netFrontAttempts', 'typedShotsOnNet', 'backhandShots', 'deflectionShots', 'takeaways', 'locatedTakeaways', 'defensiveTakeaways', 'defensivePerimeterTakeaways')
  exposure |>
    dplyr::left_join(event_counts, by = 'playerId') |>
    dplyr::left_join(unblocked, by = 'playerId') |>
    dplyr::left_join(shot_types, by = 'playerId') |>
    dplyr::left_join(takeaways, by = 'playerId') |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_columns), ~ dplyr::coalesce(.x, 0L)),
      hitsPer60 = rate_per_60(hits, exposureSeconds),
      hitsReceivedPer60 = rate_per_60(hitsReceived, exposureSeconds),
      blockedShotsPer60 = rate_per_60(blockedShots, exposureSeconds),
      fightsPer60 = rate_per_60(fights, exposureSeconds),
      contactPenaltiesTakenPer60 = rate_per_60(contactPenaltiesTaken, exposureSeconds),
      contactPenaltiesDrawnPer60 = rate_per_60(contactPenaltiesDrawn, exposureSeconds),
      netFrontAttemptShare = event_share(netFrontAttempts, locatedAttempts),
      deflectionShare = event_share(deflectionShots, typedShotsOnNet),
      backhandShare = event_share(backhandShots, typedShotsOnNet),
      reboundAttemptShare = event_share(reboundAttempts, unblockedAttempts),
      defensiveTakeawaysPer60 = rate_per_60(defensiveTakeaways, exposureSeconds),
      defensivePerimeterTakeawayShare = event_share(defensivePerimeterTakeaways, defensiveTakeaways),
      seasonId = season_id,
      eventScope = scope
    )
}

# Prepare season with matched event and ice-time scopes.
prepare_behavior_season <- function(season_id) {
  base_features <- base::readRDS(base::file.path('data/cache', base::paste0('features_', season_id, '.rds')))
  plays_raw <- base::suppressMessages(nhlscraper::gc_play_by_plays(season = season_id))
  rosters <- base::suppressMessages(nhlscraper::game_rosters(season = season_id))
  shifts <- base::suppressMessages(nhlscraper::shift_charts(season = season_id))
  if (base::nrow(plays_raw) == 0L || base::nrow(rosters) == 0L || base::nrow(shifts) == 0L) base::stop('Season source is empty.', call. = FALSE)
  plays <- plays_raw |>
    dplyr::filter(gameTypeId == 2L, periodType != 'SO') |>
    prepare_coordinates()
  regular_rosters <- rosters |>
    dplyr::filter((gameId %/% 10000L) %% 100L == 2L)
  team_keys <- regular_rosters |>
    dplyr::distinct(gameId, playerId, rosterTeamId = teamId)
  assert_unique(team_keys, base::c('gameId', 'playerId'), 'Game player teams')
  event_attribution <- summarize_event_attribution(plays, team_keys, season_id)
  if (base::any(event_attribution$missingRoster > 0L | event_attribution$wrongTeam > 0L)) base::stop('Direct-event actor attribution does not match game rosters.', call. = FALSE)
  ownership <- plays |>
    dplyr::filter(eventTypeDescKey == 'takeaway') |>
    dplyr::left_join(team_keys, by = base::c('gameId', 'playerId'))
  if (base::anyNA(ownership$rosterTeamId) || base::any(ownership$rosterTeamId != ownership$eventOwnerTeamId)) base::stop('Takeaway attribution does not match game rosters.', call. = FALSE)
  games <- plays |>
    dplyr::filter(!base::is.na(eventOwnerTeamId), !base::is.na(isHome)) |>
    dplyr::distinct(gameId, teamId = eventOwnerTeamId, isHome)
  assert_unique(games, base::c('gameId', 'teamId'), 'Game team sides')
  away_players <- team_keys |>
    dplyr::inner_join(games |> dplyr::filter(!isHome), by = base::c('gameId', 'rosterTeamId' = 'teamId'))
  regular_shifts <- shifts |>
    dplyr::semi_join(plays |> dplyr::distinct(gameId), by = 'gameId')
  away_exposure <- regular_shifts |>
    dplyr::inner_join(games |> dplyr::filter(!isHome), by = base::c('gameId', 'teamId')) |>
    dplyr::filter(duration > 0) |>
    dplyr::group_by(playerId) |>
    dplyr::summarise(exposureSeconds = base::sum(duration), .groups = 'drop')
  situation_function <- utils::getFromNamespace('.calculate_shift_times_by_situation_data', 'nhlscraper')
  situation_time <- situation_function(play_by_play = plays_raw |> dplyr::filter(gameTypeId == 2L), shift_chart = regular_shifts)
  assert_columns(situation_time, '1551TimeOnIce', 'Five-on-five shift exposure')
  five_exposure <- situation_time |>
    dplyr::group_by(playerId) |>
    dplyr::summarise(exposureSeconds = base::sum(.data[['1551TimeOnIce']]), .groups = 'drop')
  full_features <- aggregate_behavior(plays, base_features |> dplyr::transmute(playerId, exposureSeconds = timeOnIce), season_id, 'All situations')
  five_features <- aggregate_behavior(plays |> dplyr::filter(base::as.character(situationCode) == '1551'), five_exposure, season_id, 'Five on five')
  away_plays <- plays
  away_keys <- base::paste(away_players$gameId, away_players$playerId)
  for (column in base::c('hittingPlayerId', 'hitteePlayerId', 'blockingPlayerId', 'committedByPlayerId', 'drawnByPlayerId', 'shootingPlayerId', 'playerId')) {
    away_plays[[column]][!base::paste(away_plays$gameId, away_plays[[column]]) %in% away_keys] <- NA_integer_
  }
  away_features <- aggregate_behavior(away_plays, away_exposure, season_id, 'Away games')
  metadata <- base_features |>
    dplyr::select(playerId, seasonId, playerFullName, positionCode, height, weight, birthDate, gamesPlayed, timeOnIce, timeOnIcePerGame, pointsPer605v5, satRelative5v5)
  features <- dplyr::bind_rows(full_features, away_features, five_features) |>
    dplyr::inner_join(metadata, by = base::c('playerId', 'seasonId'))
  assert_unique(features, base::c('playerId', 'seasonId', 'eventScope'), 'Positional event features')
  eligible <- metadata |>
    dplyr::filter(positionCode %in% base::c('C', 'L', 'R', 'D'), timeOnIce >= xs_minutes * 60, !base::is.na(height), !base::is.na(weight), !base::is.na(birthDate))
  coverage <- features |>
    dplyr::semi_join(eligible, by = base::c('playerId', 'seasonId')) |>
    dplyr::count(eventScope)
  if (base::nrow(coverage) != 3L || base::any(coverage$n != base::nrow(eligible))) base::stop('Event scopes do not cover eligible skaters.', call. = FALSE)
  if (base::any(features$exposureSeconds[features$playerId %in% eligible$playerId] <= 0)) base::stop('Eligible skater lacks positive exposure.', call. = FALSE)
  outcomes <- base::readRDS(base::file.path('data/cache', base::paste0('outcomes_', season_id, '.rds')))
  outcomes$regularPlayerTeam <- regular_rosters |>
    dplyr::filter(positionCode %in% base::c('C', 'L', 'R', 'D')) |>
    dplyr::group_by(playerId, teamId) |>
    dplyr::summarise(teamGamesDressed = dplyr::n_distinct(gameId), .groups = 'drop')
  base::saveRDS(outcomes, base::file.path('data/cache', base::paste0('outcomes_', season_id, '.rds')), compress = 'xz')
  location_quality <- plays |>
    dplyr::filter(eventTypeDescKey %in% base::c('takeaway', 'goal', 'shot-on-goal', 'missed-shot')) |>
    dplyr::group_by(eventTypeDescKey) |>
    dplyr::summarise(events = dplyr::n(), missingLocation = base::sum(!base::is.finite(xCoordNorm)), zoneDisagreement = base::sum(zoneCode != dplyr::case_when(xCoordNorm < -25 ~ 'D', xCoordNorm > 25 ~ 'O', TRUE ~ 'N'), na.rm = TRUE), .groups = 'drop') |>
    dplyr::mutate(seasonId = season_id, .before = 1L)
  penalty_inventory <- plays |>
    dplyr::filter(eventTypeDescKey == 'penalty') |>
    dplyr::count(penaltyTypeDescKey, name = 'events') |>
    dplyr::mutate(seasonId = season_id, includedContact = penaltyTypeDescKey %in% xs_contact_penalties, .before = 1L)
  penalty_attribution <- plays |> dplyr::filter(eventTypeDescKey == 'penalty', penaltyTypeDescKey %in% xs_contact_penalties) |> dplyr::summarise(seasonId = season_id, infractions = dplyr::n(), missingTaken = base::sum(base::is.na(committedByPlayerId)), missingDrawn = base::sum(base::is.na(drawnByPlayerId)))
  base::list(version = xs_feature_version, features = features, locationQuality = location_quality, penalties = penalty_inventory, eventAttribution = event_attribution, penaltyAttribution = penalty_attribution, teammateBlocksExcluded = base::sum(plays$eventTypeDescKey == 'blocked-shot' & plays$reason == 'teammate-blocked', na.rm = TRUE), playByPlaySha256 = digest::digest(plays_raw, algo = 'sha256'), rosterSha256 = digest::digest(rosters, algo = 'sha256'), shiftSha256 = digest::digest(shifts, algo = 'sha256'), sourceRows = base::nrow(plays_raw), collectedAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE))
}
