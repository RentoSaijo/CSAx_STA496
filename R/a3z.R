# A3Z Settings ------------------------------------------------------------

# Specify positional physicality models on matched five-on-five observations.
a3z_scope <- 'A3Z tracked games; five on five'
a3z_minutes <- 150
a3z_player_time_tolerance <- 120
a3z_game_time_tolerance <- 60
a3z_version <- '20260926-physicality-v2'
a3z_specification <- 'A3Z physicality'
a3z_forward_features <- base::c('dumpInRecoveriesPer60', 'forecheckPressuresPer60')
a3z_defense_features <- base::c('defensiveRetrievalsPer60', 'botchedRetrievalsPer60', 'entryDenialShare')
a3z_definition <- 'Playing tougher for one’s size means making one’s presence felt beyond what body size would suggest, through both direct physical contact and indirect signs of physicality in battles for the puck and space. We quantify this with CSAx by predicting listed height-and-weight size from shared direct and position-specific indirect measures, calibrating that prediction against the player’s listed frame, and standardizing the resulting residual within each season and reference population.'
a3z_source_columns <- base::c(sourceMinutes = '5v5 TOI', dumpInRecoveries = 'Recoveries', forecheckPressures = 'Forecheck Pressures', forecheckAssists = 'Assists off Forecheck', cycleAssists = 'Assists off Cycle', retrievalExits = 'Retrievals Leading to Exits', botchedRetrievals = 'Botched Retrievals', successfulExits = 'Zone Exits', possessionExits = 'Exits w/ Possession', entryTargets = 'Targets', entryDenials = 'Denials', defensiveRetrievals = 'DZ Retrievals', defensiveTouches = 'DZ Puck Touches', failedExits = 'Failed Exit', missedPasses = 'Missed Passes', clearedExits = 'Clears', carriedExits = 'Carried Exits', passedExits = 'Passed Exits', rushedExits = 'Rushed Exits', secondTouchExits = 'Second Touch Exits', forecheckCycleShots = 'Shots off Forecheck or Cycle', behindNetAssists = 'Behind Net', highDangerAssists = 'Home Plate', primaryShotAssists = 'Primary Shot Assists', shotAssists = 'Passes', sourceShots = 'Shots', sourceRebounds = 'Rebounds', sourceDeflections = 'Deflections')

# Define shared direct and position-specific indirect predictors.
a3z_feature_sets <- function() {
  forward <- base::c(xs_direct_features, xs_forward_features, a3z_forward_features)
  base::list(Forwards = forward, Defensemen = base::c(xs_direct_features, a3z_defense_features))
}

# Derive clean-retrieval rate from retained counts and matched exposure.
update_a3z_features <- function(inputs) {
  counts <- inputs$features$defensiveRetrievals
  if (base::any(!base::is.finite(counts) | counts < 0 | counts != base::floor(counts))) base::stop('Clean defensive retrieval counts are invalid.', call. = FALSE)
  inputs$features <- inputs$features |>
    dplyr::mutate(defensiveRetrievalsPer60 = rate_per_60(defensiveRetrievals, exposureSeconds))
  inputs
}

# Source Identity ---------------------------------------------------------

# Normalize spelling while retaining original source labels.
a3z_name_key <- function(values) stringr::str_replace_all(stringr::str_to_lower(stringi::stri_trans_general(values, 'Latin-ASCII')), '[^a-z0-9]', '')

# Normalize source team abbreviations to NHL abbreviations.
a3z_team_key <- function(values) {
  values <- stringr::str_remove_all(stringr::str_to_upper(values), '[^A-Z]')
  dplyr::recode(values, LA = 'LAK', NJ = 'NJD', SJ = 'SJS', TB = 'TBL')
}

# Match player names and sweater numbers within game rosters.
a3z_player_match <- function(source, roster) {
  base::vapply(base::seq_len(base::nrow(source)), function(index) {
    candidate <- roster[roster$teamKey == source$teamKey[index], ]
    name <- source$nameKey[index]
    exact <- candidate$nameKey == name
    named <- base::which(exact)
    if (base::length(named) == 1L) base::return(candidate$playerId[named])
    surname <- base::vapply(candidate$lastKey, function(last) !base::is.na(last) && base::nzchar(last) && base::grepl(base::paste0(last, '[0-9]*$'), name), base::logical(1L))
    number <- !base::is.na(source$sourceSweater[index]) & candidate$sweaterNumber == source$sourceSweater[index]
    strong <- base::which((exact | surname) & number)
    if (base::length(strong) == 1L) base::return(candidate$playerId[strong])
    compatible <- base::which(surname & base::substr(candidate$nameKey, 1L, 3L) == base::substr(name, 1L, 3L))
    if (base::length(compatible) == 1L) base::return(candidate$playerId[compatible])
    NA_integer_
  }, base::integer(1L))
}

# Read raw workbook export with preserved season and game labels.
read_a3z_source <- function(path) {
  raw <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  assert_columns(raw, base::c('Year', 'Game', 'Player', 'Team', 'Pos.', '#', a3z_source_columns), 'A3Z extract')
  labels <- base::paste0(xs_behavior_seasons %/% 10000L, '-', base::substr(base::as.character(xs_behavior_seasons %% 10000L), 3L, 4L))
  raw <- raw |>
    dplyr::mutate(sourceRow = dplyr::row_number()) |>
    dplyr::filter(Year %in% labels, !base::is.na(Player), stringr::str_to_upper(.data[['Pos.']]) != 'G')
  result <- raw |>
    dplyr::transmute(sourceRow, sourceSeason = Year, seasonId = xs_behavior_seasons[base::match(Year, labels)], sourceGame = Game, sourcePlayer = Player, sourceTeam = Team, sourcePosition = .data[['Pos.']], sourceSweater = base::as.integer(.data[['#']]), teamKey = a3z_team_key(Team), nameKey = a3z_name_key(Player))
  for (column in base::names(a3z_source_columns)) result[[column]] <- base::as.numeric(raw[[a3z_source_columns[[column]]]])
  result
}

# Reconcile schedule candidates using dates, teams, and roster identities.
match_a3z_games <- function(source, games, rosters) {
  groups <- base::split(source, base::paste(source$seasonId, source$sourceGame))
  mapped <- purrr::map(groups, function(rows) {
    parsed <- stringr::str_match(rows$sourceGame[1L], stringr::regex('^(\\d+)/+(\\d+)/(\\d+)\\s+([A-Za-z.]+)\\s+vs\\.?\\s*([A-Za-z.]+)\\s*$', ignore_case = TRUE))
    candidates <- games |> dplyr::filter(seasonId == rows$seasonId[1L])
    if (!base::is.na(parsed[1L, 1L])) {
      month <- base::as.integer(parsed[1L, 2L])
      year <- rows$seasonId[1L] %/% 10000L + base::as.integer(month < 9L)
      source_date <- base::as.Date(base::sprintf('%04d-%02d-%02d', year, month, base::as.integer(parsed[1L, 3L])))
      pair <- base::paste(base::sort(a3z_team_key(parsed[1L, 5:6])), collapse = '|')
      candidates <- candidates |>
        dplyr::filter(teamPair == pair) |>
        dplyr::mutate(dateDifference = base::as.integer(gameDate - source_date)) |>
        dplyr::filter(base::abs(dateDifference) <= 3L)
    } else {
      source_date <- base::as.Date(NA)
      candidates <- candidates[0L, ] |> dplyr::mutate(dateDifference = base::integer())
    }
    matches <- purrr::map(candidates$gameId, function(game) a3z_player_match(rows, rosters[rosters$gameId == game, ]))
    candidates$rosterShare <- purrr::map_dbl(matches, function(ids) base::mean(!base::is.na(ids)))
    eligible <- base::which(candidates$rosterShare >= 0.90)
    exact <- eligible[candidates$dateDifference[eligible] == 0L]
    selected <- if (base::length(exact) == 1L) exact else if (base::length(eligible) == 1L) eligible else base::integer()
    found <- base::length(selected) == 1L
    rows$gameId <- if (found) candidates$gameId[selected] else NA_integer_
    rows$playerId <- if (found) matches[[selected]] else NA_integer_
    audit <- tibble::tibble(seasonId = rows$seasonId[1L], sourceGame = rows$sourceGame[1L], sourceDateInSeason = source_date, gameId = rows$gameId[1L], dateDifference = if (found) candidates$dateDifference[selected] else NA_integer_, rosterShare = if (found) candidates$rosterShare[selected] else if (base::nrow(candidates)) base::max(candidates$rosterShare) else NA_real_, candidateGames = base::paste(candidates$gameId, collapse = '; '), matchStatus = if (!found) 'Unresolved' else if (candidates$dateDifference[selected] == 0L) 'Date and roster' else 'Nearby date and roster', sourceRows = base::nrow(rows))
    base::list(rows = rows, games = audit)
  })
  base::list(rows = purrr::map_dfr(mapped, 'rows'), games = purrr::map_dfr(mapped, 'games'))
}

# Download compact schedules and complete game roster identities.
load_a3z_identity <- function(inputs, refresh = FALSE) {
  path <- 'data/cache/a3z_identity.rds'
  if (!refresh && base::file.exists(path)) {
    cached <- base::readRDS(path)
    if (base::any(cached$rosters$rosterPosition == 'G')) base::return(cached)
  }
  teams <- inputs$teams |> dplyr::transmute(teamId, teamKey = a3z_team_key(teamTriCode))
  games <- nhlscraper::games() |>
    dplyr::filter(seasonId %in% xs_behavior_seasons, gameTypeId == 2L) |>
    dplyr::mutate(gameDate = base::as.Date(gameDate)) |>
    dplyr::left_join(teams |> dplyr::rename(homeTeamId = teamId, homeKey = teamKey), by = 'homeTeamId') |>
    dplyr::left_join(teams |> dplyr::rename(visitingTeamId = teamId, awayKey = teamKey), by = 'visitingTeamId') |>
    dplyr::mutate(teamPair = purrr::map2_chr(homeKey, awayKey, function(home, away) base::paste(base::sort(base::c(home, away)), collapse = '|')))
  rosters <- purrr::map_dfr(xs_behavior_seasons, function(season) {
    base::message('Reading NHL game rosters for ', season, '.')
    nhlscraper::game_rosters(season) |> dplyr::semi_join(games, by = 'gameId')
  }) |>
    dplyr::left_join(teams, by = 'teamId') |>
    dplyr::transmute(gameId, teamId, teamKey, playerId, sweaterNumber, rosterPosition = positionCode, rosterName = base::paste(playerFirstName, playerLastName), nameKey = a3z_name_key(rosterName), lastKey = a3z_name_key(playerLastName))
  assert_unique(rosters, base::c('gameId', 'playerId'), 'NHL skater rosters')
  result <- base::list(games = games, rosters = rosters, collectedAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE))
  base::saveRDS(result, path, compress = 'xz')
  result
}

# Matched NHL Exposure ----------------------------------------------------

# Preserve source-matched events and shift-derived five-on-five minutes.
load_a3z_events <- function(season, game_ids, refresh = FALSE) {
  path <- base::file.path('data/cache', base::paste0('a3z_nhl_', season, '.rds'))
  if (!refresh && base::file.exists(path)) {
    cached <- base::readRDS(path)
    if (base::identical(cached$exposureVersion, 'Union of shift intervals') && base::all(game_ids %in% cached$gameIds)) base::return(cached)
  }
  base::message('Reading matched NHL events and shifts for ', season, '.')
  plays <- nhlscraper::gc_play_by_plays(season) |> dplyr::filter(gameId %in% game_ids, gameTypeId == 2L)
  shifts <- nhlscraper::shift_charts(season) |> dplyr::filter(gameId %in% game_ids)
  if (!base::all(game_ids %in% plays$gameId) || !base::all(game_ids %in% shifts$gameId)) base::stop('Matched games lack NHL events or shifts.', call. = FALSE)
  source_hash <- base::list(playByPlay = digest::digest(plays, algo = 'sha256'), shifts = digest::digest(shifts, algo = 'sha256'))
  shift_keys <- base::c('gameId', 'teamId', 'playerId', 'periodNumber', 'startSecondsElapsedInPeriod', 'endSecondsElapsedInPeriod')
  duplicate_shifts <- base::sum(base::duplicated(shifts[shift_keys]))
  raw_seconds <- base::sum(shifts$endSecondsElapsedInPeriod - shifts$startSecondsElapsedInPeriod, na.rm = TRUE)
  shifts <- shifts |>
    dplyr::filter(!base::is.na(playerId), !base::is.na(teamId), startSecondsElapsedInPeriod >= 0, endSecondsElapsedInPeriod > startSecondsElapsedInPeriod) |>
    dplyr::arrange(gameId, teamId, playerId, periodNumber, startSecondsElapsedInPeriod, endSecondsElapsedInPeriod) |>
    dplyr::group_by(gameId, teamId, playerId, periodNumber) |>
    dplyr::mutate(shiftBlock = base::cumsum(startSecondsElapsedInPeriod > dplyr::lag(base::cummax(endSecondsElapsedInPeriod), default = -1L))) |>
    dplyr::group_by(gameId, teamId, playerId, periodNumber, shiftBlock) |>
    dplyr::summarise(startSecondsElapsedInPeriod = base::min(startSecondsElapsedInPeriod), endSecondsElapsedInPeriod = base::max(endSecondsElapsedInPeriod), .groups = 'drop')
  overlap_seconds <- raw_seconds - base::sum(shifts$endSecondsElapsedInPeriod - shifts$startSecondsElapsedInPeriod)
  calculate_time <- utils::getFromNamespace('.calculate_shift_times_by_situation_data', 'nhlscraper')
  exposure <- calculate_time(play_by_play = plays, shift_chart = shifts) |>
    dplyr::group_by(gameId, playerId) |>
    dplyr::summarise(nhlSeconds = base::sum(.data[['1551TimeOnIce']]), .groups = 'drop')
  assert_unique(exposure, base::c('gameId', 'playerId'), 'NHL player-game exposure')
  columns <- base::c('gameId', 'eventId', 'eventTypeDescKey', 'periodType', 'situationCode', 'hittingPlayerId', 'hitteePlayerId', 'blockingPlayerId', 'shootingPlayerId', 'committedByPlayerId', 'drawnByPlayerId', 'playerId', 'eventOwnerTeamId', 'isHome', 'homeTeamDefendingSide', 'xCoord', 'yCoord', 'xCoordNorm', 'reason', 'penaltyTypeDescKey', 'shotType', 'isRebound')
  result <- base::list(gameIds = game_ids, exposure = exposure, exposureVersion = 'Union of shift intervals', duplicateShiftRows = duplicate_shifts, overlappingShiftSeconds = overlap_seconds, plays = plays |> dplyr::select(dplyr::all_of(columns)) |> dplyr::filter(periodType != 'SO') |> prepare_coordinates(), sourceHash = source_hash, collectedAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE))
  base::saveRDS(result, path, compress = 'xz')
  result
}

# Positional Inputs -------------------------------------------------------

# Retain one valid observation per source-matched player-game.
validate_a3z_rows <- function(mapping, exposure, rosters) {
  rows <- mapping$rows |>
    dplyr::left_join(exposure, by = base::c('gameId', 'playerId')) |>
    dplyr::left_join(rosters |> dplyr::select(gameId, playerId, teamId, rosterName, rosterPosition, sweaterNumber), by = base::c('gameId', 'playerId')) |>
    dplyr::mutate(deltaSeconds = sourceMinutes * 60 - nhlSeconds, rowStatus = dplyr::case_when(base::is.na(gameId) ~ 'Unresolved game', base::is.na(playerId) ~ 'Unresolved player', !base::is.finite(sourceMinutes) | sourceMinutes <= 0 ~ 'Invalid source exposure', !base::is.finite(nhlSeconds) | nhlSeconds <= 0 ~ 'Invalid NHL exposure', TRUE ~ 'Retained'))
  required <- base::c(base::names(a3z_source_columns)[2:12], 'defensiveRetrievals')
  valid_counts <- base::apply(rows[required], 1L, function(values) base::all(base::is.finite(values) & values >= 0 & values == base::floor(values)))
  valid_shares <- rows$possessionExits <= rows$successfulExits & rows$entryDenials <= rows$entryTargets & rows$successfulExits == rows$possessionExits + rows$clearedExits
  rows$rowStatus[rows$rowStatus == 'Retained' & !(valid_counts & valid_shares) %in% TRUE] <- 'Invalid event counts'
  repeated <- rows |>
    dplyr::filter(rowStatus == 'Retained') |>
    dplyr::add_count(gameId, playerId) |>
    dplyr::filter(n > 1L) |>
    dplyr::group_split(gameId, playerId)
  for (group in repeated) {
    identical <- base::nrow(dplyr::distinct(group, dplyr::across(dplyr::all_of(base::names(a3z_source_columns))))) == 1L
    discard <- if (identical) base::sort(group$sourceRow)[-1L] else group$sourceRow
    rows$rowStatus[rows$sourceRow %in% discard] <- if (identical) 'Identical duplicate' else 'Conflicting duplicate'
  }
  game_time <- rows |>
    dplyr::filter(rowStatus == 'Retained') |>
    dplyr::group_by(gameId) |>
    dplyr::summarise(gameMedianAbsoluteSeconds = stats::median(base::abs(deltaSeconds)), .groups = 'drop')
  rows <- rows |>
    dplyr::left_join(game_time, by = 'gameId') |>
    dplyr::mutate(rowStatus = dplyr::case_when(rowStatus != 'Retained' ~ rowStatus, gameMedianAbsoluteSeconds > a3z_game_time_tolerance ~ 'Game exposure disagreement', base::abs(deltaSeconds) > a3z_player_time_tolerance ~ 'Player exposure disagreement', TRUE ~ 'Retained'), sourcePositionDisagreement = (sourcePosition == 'D') != (rosterPosition == 'D'), sourceSweaterDisagreement = sourceSweater != sweaterNumber)
  assert_unique(rows |> dplyr::filter(rowStatus == 'Retained'), base::c('gameId', 'playerId'), 'Retained A3Z player-games')
  rows
}

# Aggregate NHL events only when their attributed actor has matched tracking.
aggregate_a3z_season <- function(season, rows, nhl, metadata) {
  tracked <- rows |> dplyr::filter(seasonId == season, rowStatus == 'Retained')
  keys <- base::paste(tracked$gameId, tracked$playerId)
  plays <- nhl$plays |> dplyr::filter(gameId %in% tracked$gameId, base::as.character(situationCode) == '1551')
  actors <- base::c('hittingPlayerId', 'hitteePlayerId', 'blockingPlayerId', 'shootingPlayerId', 'committedByPlayerId', 'drawnByPlayerId', 'playerId')
  for (actor in actors) plays[[actor]][!base::paste(plays$gameId, plays[[actor]]) %in% keys] <- NA_integer_
  exposure <- tracked |> dplyr::group_by(playerId) |> dplyr::summarise(exposureSeconds = base::sum(nhlSeconds), .groups = 'drop')
  behavior <- aggregate_behavior(plays, exposure, season, a3z_scope)
  microstats <- tracked |>
    dplyr::group_by(playerId) |>
    dplyr::summarise(trackedGames = dplyr::n_distinct(gameId), sourceTrackedMinutes = base::sum(sourceMinutes), dplyr::across(dplyr::all_of(base::setdiff(base::names(a3z_source_columns), 'sourceMinutes')), ~ base::sum(.x)), .groups = 'drop')
  behavior |>
    dplyr::inner_join(metadata |> dplyr::filter(seasonId == season), by = base::c('playerId', 'seasonId')) |>
    dplyr::left_join(microstats, by = 'playerId') |>
    dplyr::mutate(trackedMinutes = exposureSeconds / 60, trackedGameShare = trackedGames / gamesPlayed, dumpInRecoveriesPer60 = rate_per_60(dumpInRecoveries, exposureSeconds), forecheckPressuresPer60 = rate_per_60(forecheckPressures, exposureSeconds), forecheckCycleAssists = forecheckAssists + cycleAssists, forecheckCycleAssistsPer60 = rate_per_60(forecheckCycleAssists, exposureSeconds), retrievalExitsPer60 = rate_per_60(retrievalExits, exposureSeconds), botchedRetrievalsPer60 = rate_per_60(botchedRetrievals, exposureSeconds), possessionExitShare = event_share(possessionExits, successfulExits), entryDenialShare = event_share(entryDenials, entryTargets))
}

# Prepare frozen source mapping, matched features, and coverage summaries.
prepare_a3z_inputs <- function(inputs, refresh = FALSE) {
  source <- read_a3z_source('data/cache/a3z_raw.csv')
  identity <- load_a3z_identity(inputs, refresh = refresh)
  mapping <- match_a3z_games(source, identity$games, identity$rosters)
  parts <- purrr::map(xs_behavior_seasons, function(season) load_a3z_events(season, base::unique(stats::na.omit(mapping$rows$gameId[mapping$rows$seasonId == season])), refresh = refresh))
  rows <- validate_a3z_rows(mapping, purrr::map_dfr(parts, 'exposure'), identity$rosters)
  metadata <- inputs$features |>
    dplyr::filter(eventScope == 'All situations') |>
    dplyr::select(playerId, seasonId, playerFullName, positionCode, height, weight, birthDate, gamesPlayed, timeOnIce, timeOnIcePerGame, pointsPer605v5, satRelative5v5, age)
  all_features <- purrr::map_dfr(base::seq_along(xs_behavior_seasons), function(index) aggregate_a3z_season(xs_behavior_seasons[index], rows, parts[[index]], metadata))
  features <- all_features |> dplyr::filter(timeOnIce >= xs_minutes * 60, trackedMinutes >= a3z_minutes) |> dplyr::arrange(seasonId, playerId)
  assert_unique(features, base::c('playerId', 'seasonId'), 'Eligible A3Z features')
  coverage <- metadata |>
    dplyr::left_join(all_features |> dplyr::select(playerId, seasonId, trackedGames, trackedMinutes, sourceTrackedMinutes, trackedGameShare), by = base::c('playerId', 'seasonId')) |>
    dplyr::mutate(positionGroup = dplyr::if_else(positionCode == 'D', 'Defensemen', 'Forwards'), eligible = !base::is.na(trackedMinutes) & trackedMinutes >= a3z_minutes)
  attribution <- purrr::map_dfr(base::seq_along(parts), function(index) summarize_event_attribution(parts[[index]]$plays, identity$rosters |> dplyr::transmute(gameId, playerId, rosterTeamId = teamId), xs_behavior_seasons[index]))
  if (base::any(attribution$missingRoster > 0L | attribution$wrongTeam > 0L)) base::stop('NHL event attribution disagrees with matched rosters.', call. = FALSE)
  games <- mapping$games |> dplyr::left_join(rows |> dplyr::filter(rowStatus == 'Retained') |> dplyr::count(sourceGame, seasonId, name = 'retainedRows'), by = base::c('sourceGame', 'seasonId'))
  provenance <- base::list(
    version = a3z_version, workbookUrl = 'https://public.tableau.com/app/profile/corey.sznajder/viz/transitionstats/Sheet1', sourceLinksUrl = 'https://www.allthreezones.com/links.html', glossaryUrl = 'https://www.allthreezones.com/player-cardsfaq.html', methodologyUrl = 'https://allthreezones.substack.com/p/catch-and-retrieve',
    workbookSha256 = digest::digest(file = 'data/cache/a3z_transition.twbx', algo = 'sha256'), exportSha256 = digest::digest(file = 'data/cache/a3z_raw.csv', algo = 'sha256'), collectedAt = base::as.character(base::as.Date(base::file.info('data/cache/a3z_transition.twbx')$mtime, tz = 'UTC')), preparedAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE), packageSha = xs_package_sha,
    sourceInventory = stats::setNames(purrr::map(parts, function(part) part[base::c('sourceHash', 'collectedAt', 'exposureVersion', 'duplicateShiftRows', 'overlappingShiftSeconds')]), xs_behavior_seasons), playerTimeToleranceSeconds = a3z_player_time_tolerance, gameMedianTimeToleranceSeconds = a3z_game_time_tolerance, minimumTrackedMinutes = a3z_minutes
  )
  update_a3z_features(base::list(features = features, coverage = coverage, sourceRows = rows, games = games, schedules = identity$games, eventAttribution = attribution, provenance = provenance))
}
