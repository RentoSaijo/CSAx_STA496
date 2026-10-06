# Setup -------------------------------------------------------------------

# Load positional model functions and frozen scientific inputs.
base::source('R/functions.R')
base::source('R/models.R')
base::Sys.setenv(OMP_NUM_THREADS = '1', OPENBLAS_NUM_THREADS = '1', VECLIB_MAXIMUM_THREADS = '1')

# Fit shared direct and positional indirect physicality models.
if (!'--benchmark' %in% base::commandArgs(trailingOnly = TRUE)) {
  base::source('R/a3z.R')
  inputs <- base::readRDS('data/cache/a3z_inputs.rds')
  fits <- build_a3z_models(inputs)
  base::saveRDS(fits, 'data/cache/a3z_models.rds', compress = 'xz')
  base::message('A3Z positional models are complete.')
  base::quit(save = 'no', status = 0L)
}

# Read full-season benchmark inputs.
input_path <- 'data/cache/positional_inputs.rds'
inputs <- if (base::file.exists(input_path)) base::readRDS(input_path) else base::readRDS('data/analysis_data.rds')$inputs
if (base::is.null(inputs$features)) base::stop('Prepared positional inputs are unavailable.', call. = FALSE)

# Primary Models ----------------------------------------------------------

# Fit native positional models and symmetric center comparison.
base::message('Fitting combined positional models.')
primary <- fit_positional_system(inputs$features, parallel_seasons = TRUE)
centers <- compare_centers(primary$predictions)
assert_unique(primary$predictions, base::c('model', 'playerId', 'seasonId'), 'Excluded-sample predictions')
assert_unique(centers, base::c('playerId', 'seasonId'), 'Paired centers')
assert_finite(primary$predictions, base::c('listedSize', 'xS', 'CSAx', 'directContribution', 'indirectContribution', 'frameAdjustment'), 'Positional predictions')
if (base::max(base::abs(base::with(primary$predictions, directContribution + indirectContribution + frameAdjustment - CSAx))) > 1e-8) base::stop('Feature contributions do not sum to CSAx.', call. = FALSE)
base::saveRDS(base::list(inputs = inputs, primary = primary, centers = centers), 'data/cache/positional_models.rds', compress = 'xz')

# Focused Sensitivities ----------------------------------------------------

# Reconstruct prespecified alternatives without further bootstrap campaigns.
settings <- tibble::tribble(~label, ~specification, ~scope, ~minimumMinutes, 'Direct only', 'Direct only', 'All situations', 300, 'Indirect only', 'Indirect only', 'All situations', 300, 'Rebound addition', 'Rebound addition', 'All situations', 300, '500 minutes', 'Combined', 'All situations', 500, 'Away games', 'Combined', 'Away games', 300, 'Five on five', 'Combined', 'Five on five', 300)
sensitivities <- purrr::map(base::seq_len(base::nrow(settings)), function(index) {
  base::message('Fitting sensitivity: ', settings$label[index], '.')
  fit_positional_system(inputs$features, specification = settings$specification[index], scope = settings$scope[index], minimum_minutes = settings$minimumMinutes[index], parallel_seasons = TRUE)
})
base::names(sensitivities) <- settings$label
base::saveRDS(base::list(inputs = inputs, primary = primary, centers = centers, sensitivities = sensitivities), 'data/cache/positional_models.rds', compress = 'xz')
base::message('Positional models and focused sensitivities are complete.')
