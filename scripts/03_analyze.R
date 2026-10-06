# Setup -------------------------------------------------------------------

# Load fitted models and application helpers.
base::source('R/functions.R')
base::source('R/models.R')
base::source('R/applications.R')
base::source('R/scouting.R')
base::Sys.setenv(OMP_NUM_THREADS = '1', OPENBLAS_NUM_THREADS = '1', VECLIB_MAXIMUM_THREADS = '1')

# Estimate pilot relationships while preserving full-season benchmark.
if (!'--benchmark' %in% base::commandArgs(trailingOnly = TRUE)) {
  base::source('R/a3z.R')
  base::source('R/postseason.R')
  analysis <- base::readRDS('data/analysis_data.rds')
  inputs <- base::readRDS('data/cache/a3z_inputs.rds')
  fits <- base::readRDS('data/cache/a3z_models.rds')
  pilot <- analyze_a3z_models(fits, inputs, analysis$inputs)
  application_inputs <- base::readRDS('data/cache/application_inputs.rds')
  pilot$applications <- analyze_focused_applications(pilot$predictions, application_inputs, analysis$inputs)
  pilot$applicationInputs <- application_inputs
  pilot$inputs <- inputs
  pilot$coefficients <- purrr::imap_dfr(fits, function(fit, label) fit$coefficients |> dplyr::mutate(specification = label))
  pilot$tuning <- purrr::imap_dfr(fits, function(fit, label) fit$tuning |> dplyr::mutate(specification = label))
  pilot$sizeReference <- fits[[a3z_specification]]$sizeReference
  pilot$benchmark <- base::list(specification = 'Full-season play-by-play benchmark', eventScope = 'All situations with labeled sensitivities', components = base::c('inputs', 'primary', 'centers', 'sensitivities', 'applications', 'bootstrap', 'inference'), builtAt = analysis$builtAt)
  pilot$settings <- base::list(version = a3z_version, definition = a3z_definition, features = purrr::map(fits, 'features'), eligibilityMinutes = xs_minutes, trackedMinutes = a3z_minutes, rankingMinutes = 500, sparseDenominatorThreshold = 20L, outerFolds = xs_outer_folds, innerFolds = xs_inner_folds, penaltyGrid = xs_penalty_grid, seed = xs_seed)
  pilot$builtAt <- base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE)
  if (base::is.null(analysis$a3zBenchmark) && !base::identical(analysis$a3z$settings$version, a3z_version)) {
    analysis$a3zBenchmark <- analysis$a3z[base::setdiff(base::names(analysis$a3z), 'inputs')]
  }
  if (base::is.null(analysis$a3zCenterBenchmark) && !base::is.null(analysis$a3z$centers)) {
    historical <- analysis$a3z
    analysis$a3zCenterBenchmark <- base::list(predictions = historical$predictions |> dplyr::filter(model == 'Wings' | isCenterComparison), centers = historical$centers, agreement = historical$centerAgreement, contributions = historical$centerContributions, coefficients = historical$coefficients, sizeReference = historical$sizeReference, settings = historical$settings, builtAt = historical$builtAt, status = 'Historical cross-position comparison')
  }
  if (base::is.null(analysis$a3zTeamBenchmark) && !base::is.null(analysis$a3z$applications$teams)) {
    analysis$a3zTeamBenchmark <- base::list(results = analysis$a3z$applications$teams, provenance = analysis$a3z$applicationInputs$provenance, settings = analysis$a3z$settings, builtAt = analysis$a3z$builtAt, status = 'Historical team chance-creation analysis')
  }
  analysis$a3z <- pilot
  analysis$definition <- a3z_definition
  base::saveRDS(analysis, 'data/analysis_data.rds', compress = 'xz')
  base::message('Saved positional physicality results; scouting status: ', pilot$scouting$status, '.')
  base::quit(save = 'no', status = 0L)
}

# Read full-season benchmark models and inputs.
models <- base::readRDS('data/cache/positional_models.rds')
inputs <- base::readRDS('data/cache/positional_inputs.rds')

# Positional Applications -------------------------------------------------

# Estimate roster, contract, playoff, scouting, and team relationships.
applications <- analyze_positional_applications(models, inputs)
inputs$teamPlayerPositions <- applications$teams$exposurePositions
assert_unique(applications$panel, base::c('model', 'playerId', 'seasonId'), 'Positional application panel')
assert_unique(applications$playoffs, base::c('model', 'playerId', 'seasonId'), 'Playoff application panel')
if (base::any(applications$playoffs$playoffDressedShare < 0 | applications$playoffs$playoffDressedShare > 1)) base::stop('Playoff dressing denominator is invalid.', call. = FALSE)

# Shared Bootstrap --------------------------------------------------------

# Propagate player sampling through complete positional pipeline.
bootstrap <- run_positional_bootstrap(inputs$features, prepare_outcomes(inputs))
inference <- summarize_positional_bootstrap(bootstrap, applications$primaryPoints, models$centers)

# Analysis Object ---------------------------------------------------------

# Preserve scientific inputs and compact results in single analysis object.
analysis <- base::list(definition = 'Playing big for one\'s size means showing a pattern of direct physical engagement and position-specific indirect behaviors associated with contested space that is more characteristic of a larger player than expected for one\'s listed height and weight.', settings = base::list(behaviorSeasons = xs_behavior_seasons, nextOutcomeSeasons = next_season_id(xs_behavior_seasons), eligibilityMinutes = xs_minutes, rankingMinutes = 500, directFeatures = xs_direct_features, forwardFeatures = xs_forward_features, defenseFeatures = xs_defense_features, contactPenalties = xs_contact_penalties, penaltyGrid = xs_penalty_grid, outerFolds = xs_outer_folds, innerFolds = xs_inner_folds, seed = xs_seed, bootstrapSeed = xs_bootstrap_seed, bootstrapReplicates = xs_bootstrap_reps, modelVersion = xs_model_version), inputs = inputs, primary = models$primary, centers = models$centers, sensitivities = models$sensitivities, applications = applications, bootstrap = bootstrap, inference = inference, builtAt = base::format(base::Sys.time(), tz = 'UTC', usetz = TRUE))
previous <- if (base::file.exists('data/analysis_data.rds')) base::readRDS('data/analysis_data.rds') else NULL
for (component in base::c('a3z', 'a3zBenchmark', 'a3zCenterBenchmark', 'a3zTeamBenchmark')) if (!base::is.null(previous[[component]])) analysis[[component]] <- previous[[component]]
if (!base::is.null(analysis$a3z)) analysis$definition <- analysis$a3z$settings$definition
base::saveRDS(analysis, 'data/analysis_data.rds', compress = 'xz')
base::message('Saved positional analysis with ', bootstrap$replicates, ' shared bootstrap samples.')
