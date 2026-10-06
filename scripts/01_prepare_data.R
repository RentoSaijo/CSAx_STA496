# Setup --------------------------------------------------------------------

# Load research functions.
base::source('R/functions.R')
base::source('R/data.R')
base::dir.create('data/cache', recursive = TRUE, showWarnings = FALSE)

# Lock completed human scouting ratings independently of model preparation.
if ('--lock-scouting' %in% base::commandArgs(trailingOnly = TRUE)) {
  base::source('R/scouting.R')
  lock_scouting_expansion()
  base::quit(save = 'no', status = 0L)
}

# Verify pinned NHL data package before preparing observations.
package_sha <- utils::packageDescription('nhlscraper')[['RemoteSha']]
if (!base::identical(package_sha, xs_package_sha)) base::stop('Installed nhlscraper revision does not match research revision.', call. = FALSE)

# Prepare matched A3Z inputs or restore frozen pilot observations.
if (!'--benchmark' %in% base::commandArgs(trailingOnly = TRUE)) {
  base::source('R/a3z.R')
  base::source('R/postseason.R')
  analysis <- base::readRDS('data/analysis_data.rds')
  refresh <- '--refresh-events' %in% base::commandArgs(trailingOnly = TRUE)
  prepared <- if (!refresh && !base::is.null(analysis$a3z$inputs)) analysis$a3z$inputs else prepare_a3z_inputs(analysis$inputs, refresh = refresh)
  prepared <- update_a3z_features(prepared)
  base::saveRDS(prepared, 'data/cache/a3z_inputs.rds', compress = 'xz')
  applications <- if (!refresh && !base::is.null(analysis$a3z$applicationInputs) && base::identical(analysis$a3z$applicationInputs$version, outcome_version)) analysis$a3z$applicationInputs else prepare_application_inputs(prepared, analysis$inputs, refresh)
  base::saveRDS(applications, 'data/cache/application_inputs.rds', compress = 'xz')
  base::message('Prepared ', base::nrow(prepared$features), ' matched A3Z skater-seasons.')
  base::quit(save = 'no', status = 0L)
}

# Restore bundled scientific snapshot when local caches are unavailable.
refresh_events <- '--refresh-events' %in% base::commandArgs(trailingOnly = TRUE)
snapshot <- if (base::file.exists('data/analysis_data.rds')) base::readRDS('data/analysis_data.rds')$inputs else NULL
if (!refresh_events && !base::is.null(snapshot)) {
  assert_unique(snapshot$features, base::c('playerId', 'seasonId', 'eventScope'), 'Bundled positional inputs')
  base::saveRDS(snapshot, 'data/cache/positional_inputs.rds', compress = 'xz')
  base::message('Restored frozen positional inputs from analysis object.')
  base::quit(save = 'no', status = 0L)
}
if (refresh_events && !base::is.null(snapshot)) {
  for (season_id in xs_behavior_seasons) {
    base::saveRDS(snapshot$features |> dplyr::filter(seasonId == season_id, eventScope == 'All situations'), base::file.path('data/cache', base::paste0('features_', season_id, '.rds')))
    base::saveRDS(snapshot$expectedGoals[[base::as.character(season_id)]], base::file.path('data/cache', base::paste0('expected_goals_', season_id, '.rds')))
  }
  for (season_id in xs_roster_seasons) base::saveRDS(snapshot$rosters[[base::as.character(season_id)]], base::file.path('data/cache', base::paste0('outcomes_', season_id, '.rds')))
  for (component in base::c('players', 'teams', 'seasons', 'contracts', 'transactions')) base::saveRDS(snapshot[[component]], base::file.path('data/cache', base::paste0(component, '.rds')))
  base::saveRDS(dplyr::full_join(snapshot$players, snapshot$teamPlayerPositions, by = base::c('playerId', 'positionCode')), 'data/cache/players.rds')
  base::saveRDS(snapshot$histories, 'data/cache/player_seasons.rds')
  base::saveRDS(snapshot$provenance$inherited, 'data/cache/provenance.rds')
}

# Event Features -----------------------------------------------------------

# Prepare reusable aggregates for three event scopes.
season_parts <- purrr::map(xs_behavior_seasons, function(season_id) {
  cache_path <- base::file.path('data/cache', base::paste0('positional_features_', season_id, '.rds'))
  cached <- if (base::file.exists(cache_path)) base::readRDS(cache_path) else NULL
  if (!refresh_events && !base::is.null(cached) && base::identical(cached$version, xs_feature_version)) base::return(cached)
  base::message('Preparing positional events for ', season_id, '.')
  prepared <- prepare_behavior_season(season_id)
  base::saveRDS(prepared, cache_path, compress = 'xz')
  prepared
})
features <- purrr::map_dfr(season_parts, 'features') |>
  dplyr::filter(positionCode %in% base::c('C', 'L', 'R', 'D'), timeOnIce >= xs_minutes * 60, !base::is.na(height), !base::is.na(weight), !base::is.na(birthDate)) |>
  dplyr::mutate(age = calculate_season_age(birthDate, seasonId)) |>
  dplyr::arrange(eventScope, seasonId, playerId)
assert_unique(features, base::c('playerId', 'seasonId', 'eventScope'), 'Eligible positional features')

# Career Histories ---------------------------------------------------------

# Complete histories for eligible forwards and defensemen.
history_path <- 'data/cache/player_seasons.rds'
histories <- base::readRDS(history_path)
missing_players <- base::setdiff(base::sort(base::unique(features$playerId)), histories$playerId)
if (base::length(missing_players) > 0L) {
  for (index in base::seq_along(missing_players)) {
    histories <- dplyr::bind_rows(histories, load_player_history(missing_players[index])) |>
      dplyr::distinct(playerId, seasonId)
    if (index %% 25L == 0L || index == base::length(missing_players)) {
      base::saveRDS(histories, history_path, compress = 'xz')
      base::message('Completed ', index, ' of ', base::length(missing_players), ' additional player histories.')
    }
  }
}
missing_histories <- features |>
  dplyr::distinct(playerId, seasonId) |>
  dplyr::anti_join(histories, by = base::c('playerId', 'seasonId'))
if (base::nrow(missing_histories) > 0L) base::stop('Career histories do not cover eligible seasons.', call. = FALSE)

# Scientific Inputs --------------------------------------------------------

# Assemble compact inputs without scouting prose.
scouting_codes <- readr::read_csv('validation/external_validation_data.csv', show_col_types = FALSE) |>
  dplyr::select(-dplyr::any_of('meanCSAx'))
if (base::file.exists('validation_private/external_validation_hashes.csv')) {
  private_codes <- read_scouting_validation()$data
  code_columns <- base::c('studyId', 'playerId', 'overallPhysicality', 'playsBiggerExplicit', 'activePhysicalEngagement', 'interiorPlay')
  public_check <- scouting_codes |> dplyr::select(dplyr::all_of(code_columns)) |> dplyr::arrange(studyId)
  private_check <- private_codes |> dplyr::select(dplyr::all_of(code_columns)) |> dplyr::arrange(studyId)
  if (!base::isTRUE(base::all.equal(public_check, private_check, check.attributes = FALSE))) base::stop('Public scouting codes differ from locked ratings.', call. = FALSE)
}
inputs <- base::list(
  features = features,
  histories = histories,
  rosters = stats::setNames(purrr::map(xs_roster_seasons, function(season_id) base::readRDS(base::file.path('data/cache', base::paste0('outcomes_', season_id, '.rds')))), xs_roster_seasons),
  expectedGoals = stats::setNames(purrr::map(xs_behavior_seasons, function(season_id) base::readRDS(base::file.path('data/cache', base::paste0('expected_goals_', season_id, '.rds')))), xs_behavior_seasons),
  contracts = base::readRDS('data/cache/contracts.rds'),
  transactions = base::readRDS('data/cache/transactions.rds'),
  players = base::readRDS('data/cache/players.rds') |> dplyr::filter(playerId %in% features$playerId),
  teamPlayerPositions = base::readRDS('data/cache/players.rds') |> dplyr::select(playerId, positionCode),
  teams = base::readRDS('data/cache/teams.rds'),
  seasons = base::readRDS('data/cache/seasons.rds'),
  scoutingCodes = scouting_codes,
  provenance = base::list(featureVersion = xs_feature_version, packageSha = xs_package_sha, inherited = base::readRDS('data/cache/provenance.rds'), sourceInventory = stats::setNames(purrr::map(season_parts, function(part) part[base::c('playByPlaySha256', 'rosterSha256', 'shiftSha256', 'sourceRows', 'collectedAt')]), xs_behavior_seasons), locationQuality = purrr::map_dfr(season_parts, 'locationQuality'), penaltyInventory = purrr::map_dfr(season_parts, 'penalties'), eventAttribution = purrr::map_dfr(season_parts, 'eventAttribution'), penaltyAttribution = purrr::map_dfr(season_parts, 'penaltyAttribution'), teammateBlocksExcluded = tibble::tibble(seasonId = xs_behavior_seasons, events = purrr::map_int(season_parts, 'teammateBlocksExcluded')))
)
base::saveRDS(inputs, 'data/cache/positional_inputs.rds', compress = 'xz')
base::message('Prepared ', base::nrow(features) / 3L, ' skater-seasons across three event scopes.')
