# Reference Populations ----------------------------------------------------

# Group repeated player copies within folds.
player_folds <- function(player_ids, seed, folds = xs_outer_folds) {
  base::set.seed(seed)
  unique_ids <- base::sort(base::unique(player_ids))
  assignments <- base::sample(base::rep(base::seq_len(folds), length.out = base::length(unique_ids)))
  assignments[base::match(player_ids, unique_ids)]
}

# Learn listed-size reference from native population.
size_reference <- function(data) {
  height_mean <- base::mean(data$height)
  height_sd   <- stats::sd(data$height)
  weight_mean <- base::mean(data$weight)
  weight_sd   <- stats::sd(data$weight)
  combined    <- ((data$height - height_mean) / height_sd + (data$weight - weight_mean) / weight_sd) / base::sqrt(2)
  base::list(heightMean = height_mean, heightSd = height_sd, weightMean = weight_mean, weightSd = weight_sd, combinedMean = base::mean(combined), combinedSd = stats::sd(combined))
}

# Apply native listed-size reference to new observations.
apply_size_reference <- function(data, reference) {
  combined <- ((data$height - reference$heightMean) / reference$heightSd + (data$weight - reference$weightMean) / reference$weightSd) / base::sqrt(2)
  (combined - reference$combinedMean) / reference$combinedSd
}

# Calculate midrank percentile against native scores.
reference_percentile <- function(values, reference) {
  base::vapply(values, function(value) 100 * (base::sum(reference < value) + 0.5 * base::sum(reference == value)) / base::length(reference), base::numeric(1L))
}

# Ridge Estimation --------------------------------------------------------

# Fit training preprocessing and complete ridge path through tidymodels.
fit_ridge_path <- function(training, features) {
  model_data <- training |> dplyr::select(listedSize, dplyr::all_of(features))
  recipe <- recipes::recipe(listedSize ~ ., data = model_data) |>
    recipes::step_impute_median(recipes::all_numeric_predictors()) |>
    recipes::step_zv(recipes::all_predictors()) |>
    recipes::step_YeoJohnson(recipes::all_numeric_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    recipes::prep(training = model_data)
  predictors <- recipes::bake(recipe, new_data = NULL, recipes::all_predictors())
  model <- parsnip::linear_reg(penalty = xs_penalty_grid[1L], mixture = 0) |>
    parsnip::set_engine('glmnet', standardize = FALSE, path_values = xs_penalty_grid) |>
    parsnip::fit_xy(x = predictors, y = training$listedSize)
  base::list(recipe = recipe, model = model)
}

# Predict complete tuning path for excluded observations.
predict_ridge_path <- function(fit, assessment, penalties = xs_penalty_grid) {
  predictors <- recipes::bake(fit$recipe, new_data = assessment, recipes::all_predictors())
  predictions <- parsnip::multi_predict(fit$model, new_data = predictors, penalty = penalties)$'.pred'
  values <- base::lapply(predictions, function(prediction) prediction$'.pred'[base::match(penalties, prediction$penalty)])
  base::matrix(base::unlist(values, use.names = FALSE), nrow = base::nrow(assessment), ncol = base::length(penalties), byrow = TRUE)
}

# Decompose ridge prediction into standardized feature contributions.
ridge_contributions <- function(fit, assessment, penalty) {
  predictors <- recipes::bake(fit$recipe, new_data = assessment, recipes::all_predictors())
  coefficients <- base::as.matrix(stats::coef(fit$model$fit, s = penalty))[, 1L]
  contributions <- base::sweep(base::as.matrix(predictors), 2L, coefficients[base::names(predictors)], '*')
  direct_columns <- base::intersect(xs_direct_features, base::colnames(contributions))
  indirect_columns <- base::setdiff(base::colnames(contributions), direct_columns)
  tibble::tibble(directRaw = base::rowSums(contributions[, direct_columns, drop = FALSE]), indirectRaw = base::rowSums(contributions[, indirect_columns, drop = FALSE]), intercept = coefficients['(Intercept)'])
}

# Fit nested season model with excluded-sample frame calibration.
fit_reference_model <- function(reference, centers, features, model_id, seed, details = TRUE) {
  native_size <- size_reference(reference)
  reference$listedSize <- apply_size_reference(reference, native_size)
  centers$listedSize   <- apply_size_reference(centers, native_size)
  native_predictions <- base::vector('list', xs_outer_folds)
  center_predictions <- base::vector('list', xs_outer_folds)
  coefficient_parts  <- base::vector('list', xs_outer_folds)
  tuning_parts       <- base::vector('list', xs_outer_folds)
  for (fold in base::seq_len(xs_outer_folds)) {
    training   <- reference[reference$outerFold != fold, ]
    assessment <- reference[reference$outerFold == fold, ]
    center_assessment <- centers[centers$outerFold == fold, ]
    if (base::length(base::intersect(training$playerId, assessment$playerId)) > 0L || base::length(base::intersect(training$playerId, centers$playerId)) > 0L) base::stop('Player leakage in reference folds.', call. = FALSE)
    inner_fold <- player_folds(training$playerId, seed + fold, xs_inner_folds)
    inner_predictions <- base::matrix(NA_real_, base::nrow(training), base::length(xs_penalty_grid))
    for (inner in base::seq_len(xs_inner_folds)) {
      inner_fit <- fit_ridge_path(training[inner_fold != inner, ], features)
      inner_predictions[inner_fold == inner, ] <- predict_ridge_path(inner_fit, training[inner_fold == inner, ])
    }
    errors  <- base::colMeans((inner_predictions - training$listedSize)^2)
    best    <- base::which.min(errors)
    penalty <- xs_penalty_grid[best]
    calibration <- parsnip::linear_reg() |>
      parsnip::set_engine('lm') |>
      parsnip::fit_xy(x = training |> dplyr::select(listedSize), y = inner_predictions[, best])
    outer_fit <- fit_ridge_path(training, features)
    score_fold <- function(data, center = FALSE) {
      if (base::nrow(data) == 0L) base::return(tibble::tibble())
      prediction <- predict_ridge_path(outer_fit, data, penalties = penalty)[, 1L]
      expected   <- stats::predict(calibration, new_data = data |> dplyr::select(listedSize))$'.pred'
      ranges <- base::c('height', 'weight', features)
      unsupported <- base::vapply(base::seq_len(base::nrow(data)), function(index) {
        outside <- base::vapply(ranges, function(column) base::is.finite(data[[column]][index]) && (data[[column]][index] < base::min(training[[column]], na.rm = TRUE) || data[[column]][index] > base::max(training[[column]], na.rm = TRUE)), base::logical(1L))
        base::paste(ranges[outside], collapse = '; ')
      }, base::character(1L))
      result <- data |> dplyr::select(rowId, playerId, seasonId, playerFullName, positionCode, timeOnIce, listedSize, outerFold, dplyr::any_of(base::c('trackedMinutes', 'sourceTrackedMinutes', 'trackedGames', 'trackedGameShare', 'sparseDenominators')))
      result$xS <- prediction
      result$expectedXSGivenS <- expected
      result$rawResidual <- prediction - expected
      result$meanSizePrediction <- base::mean(training$listedSize)
      result$outsideTrainingRange <- base::nzchar(unsupported)
      result$outsideFeatures <- unsupported
      result$imputedFeatures <- base::apply(!base::is.finite(base::as.matrix(data[features])), 1L, base::sum)
      result$isCenterComparison <- center
      if (details) result <- dplyr::bind_cols(result, ridge_contributions(outer_fit, data, penalty))
      result
    }
    native_predictions[[fold]] <- score_fold(assessment)
    center_predictions[[fold]] <- score_fold(center_assessment, center = TRUE)
    tuning_parts[[fold]] <- tibble::tibble(outerFold = fold, penalty = penalty, innerRmse = base::sqrt(errors[best]), trainingRows = base::nrow(training), assessmentRows = base::nrow(assessment), calibrationIntercept = stats::coef(calibration$fit)[1L], calibrationSlope = stats::coef(calibration$fit)[2L])
    if (details) {
      coefficients <- base::as.matrix(stats::coef(outer_fit$model$fit, s = penalty))[, 1L]
      coefficient_parts[[fold]] <- tibble::tibble(outerFold = fold, feature = features, coefficient = dplyr::coalesce(base::unname(coefficients[features]), 0), component = dplyr::if_else(features %in% xs_direct_features, 'Direct', 'Indirect'))
    }
  }
  native <- dplyr::bind_rows(native_predictions)
  scored_centers <- dplyr::bind_rows(center_predictions)
  residual_mean <- base::mean(native$rawResidual)
  residual_sd   <- stats::sd(native$rawResidual)
  if (!base::is.finite(residual_sd) || residual_sd <= 0) base::stop('Native residual scale is undefined.', call. = FALSE)
  predictions <- dplyr::bind_rows(native, scored_centers) |>
    dplyr::mutate(CSAx = (rawResidual - residual_mean) / residual_sd, referencePercentile = reference_percentile(rawResidual, native$rawResidual), model = model_id, referencePopulation = model_id)
  if (details) predictions <- predictions |> dplyr::mutate(directContribution = directRaw / residual_sd, indirectContribution = indirectRaw / residual_sd, frameAdjustment = (intercept - expectedXSGivenS - residual_mean) / residual_sd)
  native <- predictions |> dplyr::filter(!isCenterComparison)
  performance <- tibble::tibble(model = model_id, seasonId = reference$seasonId[1L], n = base::nrow(native), players = dplyr::n_distinct(native$playerId), rmse = base::sqrt(base::mean((native$xS - native$listedSize)^2)), baselineRmse = base::sqrt(base::mean((native$meanSizePrediction - native$listedSize)^2)), predictiveRSquared = 1 - base::sum((native$xS - native$listedSize)^2) / base::sum((native$meanSizePrediction - native$listedSize)^2), squaredCorrelation = stats::cor(native$xS, native$listedSize)^2, residualSizeCorrelation = stats::cor(native$CSAx, native$listedSize), residualMean = residual_mean, residualSd = residual_sd)
  base::list(predictions = predictions, performance = performance, coefficients = dplyr::bind_rows(coefficient_parts) |> dplyr::mutate(model = model_id, seasonId = reference$seasonId[1L]), tuning = dplyr::bind_rows(tuning_parts) |> dplyr::mutate(model = model_id, seasonId = reference$seasonId[1L]), sizeReference = dplyr::bind_cols(tibble::tibble(model = model_id, seasonId = reference$seasonId[1L]), tibble::as_tibble(native_size)))
}

# Positional Systems ------------------------------------------------------

# Fit native applications and symmetric center references.
fit_positional_system <- function(features, seed = xs_seed, specification = 'Combined', scope = 'All situations', minimum_minutes = xs_minutes, details = TRUE, parallel_seasons = FALSE, feature_sets = NULL, score_centers = TRUE) {
  data <- features |>
    dplyr::filter(eventScope == scope, timeOnIce >= minimum_minutes * 60) |>
    dplyr::mutate(rowId = dplyr::row_number())
  model_features <- base::list(Forwards = base::c(xs_direct_features, xs_forward_features), Wings = base::c(xs_direct_features, xs_forward_features), Defensemen = base::c(xs_direct_features, xs_defense_features))
  if (specification == 'Direct only') model_features <- purrr::map(model_features, function(columns) xs_direct_features)
  if (specification == 'Indirect only') model_features <- purrr::map(model_features, function(columns) base::setdiff(columns, xs_direct_features))
  if (specification == 'Rebound addition') {
    model_features$Forwards <- base::c(model_features$Forwards, 'reboundAttemptShare')
    model_features$Wings <- base::c(model_features$Wings, 'reboundAttemptShare')
    model_features$Defensemen <- NULL
  }
  if (!base::is.null(feature_sets)) model_features <- feature_sets
  jobs <- tidyr::expand_grid(seasonId = xs_behavior_seasons, model = base::names(model_features))
  fit_job <- function(index) {
    season <- jobs$seasonId[index]
    model  <- jobs$model[index]
    season_data <- data |> dplyr::filter(seasonId == season)
    season_data$outerFold <- player_folds(season_data$playerId, seed + season)
    positions <- switch(model, Forwards = base::c('C', 'L', 'R'), Wings = base::c('L', 'R'), Defensemen = 'D')
    reference <- season_data |> dplyr::filter(positionCode %in% positions)
    centers <- season_data |> dplyr::filter(positionCode == 'C', model != 'Forwards', score_centers)
    fit_reference_model(reference, centers, model_features[[model]], model, seed + season, details = details)
  }
  fits <- if (parallel_seasons) parallel::mclapply(base::seq_len(base::nrow(jobs)), fit_job, mc.cores = base::min(12L, base::nrow(jobs)), mc.set.seed = FALSE) else base::lapply(base::seq_len(base::nrow(jobs)), fit_job)
  if (base::any(base::vapply(fits, base::inherits, base::logical(1L), 'try-error'))) base::stop('Positional model worker failed.', call. = FALSE)
  result <- purrr::map(stats::setNames(base::c('predictions', 'performance', 'coefficients', 'tuning', 'sizeReference'), base::c('predictions', 'performance', 'coefficients', 'tuning', 'sizeReference')), function(component) purrr::map_dfr(fits, component))
  result$specification <- specification
  result$scope <- scope
  result$minimumMinutes <- minimum_minutes
  result
}

# Pair center scores without altering native reference scales.
compare_centers <- function(predictions) {
  predictions |>
    dplyr::filter(isCenterComparison) |>
    dplyr::select(rowId, playerId, seasonId, playerFullName, timeOnIce, outerFold, model, CSAx, referencePercentile, outsideTrainingRange, outsideFeatures, imputedFeatures, dplyr::any_of(base::c('directContribution', 'indirectContribution', 'frameAdjustment'))) |>
    tidyr::pivot_wider(names_from = model, values_from = base::c('CSAx', 'referencePercentile', 'outsideTrainingRange', 'outsideFeatures', 'imputedFeatures', base::intersect(base::c('directContribution', 'indirectContribution', 'frameAdjustment'), base::names(predictions)))) |>
    dplyr::mutate(percentileDifference = referencePercentile_Wings - referencePercentile_Defensemen, supported = !outsideTrainingRange_Wings & !outsideTrainingRange_Defensemen & imputedFeatures_Wings == 0 & imputedFeatures_Defensemen == 0)
}

# Summarize paired center standing within each season.
center_statistics <- function(comparisons) {
  dplyr::bind_rows(comparisons |> dplyr::mutate(sample = 'All centers'), comparisons |> dplyr::filter(supported) |> dplyr::mutate(sample = 'Within observed ranges')) |>
    dplyr::group_by(seasonId, sample) |>
    dplyr::summarise(n = dplyr::n(), players = dplyr::n_distinct(playerId), spearman = stats::cor(CSAx_Wings, CSAx_Defensemen, method = 'spearman'), meanPercentileDifference = base::mean(percentileDifference), medianPercentileDifference = stats::median(percentileDifference), meanAbsolutePercentileDifference = base::mean(base::abs(percentileDifference)), .groups = 'drop')
}

# Positional Models -------------------------------------------------------

# Identify low-information shares without adding opportunity counts as predictors.
a3z_sparse_flags <- function(predictions, features, feature_sets) {
  denominators <- base::c(netFrontAttemptShare = 'locatedAttempts', deflectionShare = 'typedShotsOnNet', backhandShare = 'typedShotsOnNet', defensivePerimeterTakeawayShare = 'defensiveTakeaways', possessionExitShare = 'successfulExits', entryDenialShare = 'entryTargets')
  data <- predictions |> dplyr::left_join(features |> dplyr::select(playerId, seasonId, dplyr::all_of(base::unique(denominators))), by = base::c('playerId', 'seasonId'))
  data$sparseDenominators <- base::vapply(base::seq_len(base::nrow(data)), function(index) {
    relevant <- denominators[base::intersect(base::names(denominators), feature_sets[[data$model[index]]])]
    sparse <- base::vapply(relevant, function(column) !base::is.finite(data[[column]][index]) || data[[column]][index] < 20, base::logical(1L))
    base::paste(base::unique(base::unname(relevant[sparse])), collapse = '; ')
  }, base::character(1L))
  data |> dplyr::select(-dplyr::all_of(base::unique(denominators)))
}

# Fit primary forward and defenseman specifications.
build_a3z_models <- function(inputs) {
  feature_sets <- a3z_feature_sets()
  base::message('Fitting ', a3z_specification, '.')
  result <- fit_positional_system(inputs$features, specification = a3z_specification, scope = a3z_scope, feature_sets = feature_sets, parallel_seasons = TRUE, score_centers = FALSE)
  result$predictions <- a3z_sparse_flags(result$predictions, inputs$features, feature_sets)
  result$features <- feature_sets
  result$version <- a3z_version
  result$inputSha256 <- digest::digest(inputs$features, algo = 'sha256')
  assert_unique(result$predictions, base::c('model', 'playerId', 'seasonId'), a3z_specification)
  assert_finite(result$predictions, base::c('listedSize', 'xS', 'CSAx'), a3z_specification)
  if (base::max(base::abs(base::with(result$predictions, CSAx - directContribution - indirectContribution - frameAdjustment))) > 1e-8) base::stop('A3Z contributions do not sum to CSAx.', call. = FALSE)
  stats::setNames(base::list(result), a3z_specification)
}
