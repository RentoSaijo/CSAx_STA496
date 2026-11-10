# Scouting Ratings --------------------------------------------------------

# Read the reviewed scouting passages and codes from one source.
read_physicality_scouting <- function() {
  columns <- base::c('studyId', 'playerId', 'player', 'position', 'sourceId', 'reportYear', 'publicationDate', 'reportDate', 'sourcePage', 'sourceLocator', 'publicUrl', 'sourceTextSha256', 'reportText', 'activePhysicalPlayProfile', 'notes')
  data <- readr::read_csv('validation/scouting_annotations.csv', show_col_types = FALSE, col_types = readr::cols(studyId = readr::col_character(), playerId = readr::col_integer(), publicationDate = readr::col_date(), reportDate = readr::col_date(), reportText = readr::col_character(), activePhysicalPlayProfile = readr::col_integer(), .default = readr::col_guess()))
  if (!base::identical(base::names(data), columns) || base::nrow(data) != 82L) base::stop('Scouting annotations must contain the 82 retained records and required fields.', call. = FALSE)
  assert_unique(data, 'studyId', 'Scouting annotations')
  assert_unique(data, 'playerId', 'Scouting annotations')
  required <- base::c('studyId', 'playerId', 'player', 'position', 'sourceId', 'reportYear', 'reportDate', 'sourceLocator', 'publicUrl', 'sourceTextSha256', 'reportText', 'activePhysicalPlayProfile')
  if (base::anyNA(data[required]) || base::any(!base::nzchar(base::trimws(data$reportText))) || base::any(!base::nzchar(base::trimws(data$sourceLocator))) || base::any(!base::grepl('^[0-9a-f]{64}$', data$sourceTextSha256)) || !base::all(data$activePhysicalPlayProfile %in% base::c(0L, 1L))) base::stop('Scouting annotations contain missing source details, passages, or binary codes.', call. = FALSE)
  if (base::sum(data$position == 'Forwards') != 59L || base::sum(data$position == 'Defensemen') != 23L) base::stop('Scouting annotations do not match the retained positional cohort.', call. = FALSE)
  sources <- readr::read_csv('validation/external_validation_source_catalog.csv', show_col_types = FALSE, col_types = readr::cols(publicationDate = readr::col_date(), reportDate = readr::col_date(), .default = readr::col_guess()))
  source_match <- dplyr::left_join(data |> dplyr::select(sourceId, reportYear, publicationDate, reportDate, publicUrl), sources |> dplyr::select(sourceId, sourceYear = reportYear, sourcePublicationDate = publicationDate, sourceReportDate = reportDate, sourceUrl = publicUrl), by = 'sourceId')
  if (base::anyNA(source_match$sourceYear) || base::any(source_match$reportYear != source_match$sourceYear) || base::any(source_match$reportDate != source_match$sourceReportDate) || base::any(source_match$publicUrl != source_match$sourceUrl) || !base::identical(source_match$publicationDate, source_match$sourcePublicationDate)) base::stop('Scouting report source details do not match the source catalog.', call. = FALSE)
  base::list(data = data, status = 'Complete', raters = 1L, indicators = 'activePhysicalPlayProfile')
}

# Compare scored physicality with reviewed scouting descriptions.
analyze_physicality_scouting <- function(predictions, ratings) {
  observations <- predictions |>
    dplyr::filter(!isCenterComparison, model %in% base::c('Forwards', 'Defensemen')) |>
    dplyr::inner_join(ratings$data, by = 'playerId') |>
    dplyr::filter(dplyr::coalesce(publicationDate, reportDate) < base::as.Date(base::paste0(seasonId %/% 10000L, '-09-01')))
  if (base::any(observations$model != observations$position) || dplyr::n_distinct(observations$playerId) != 82L) base::stop('Scouting annotations do not align with the scored cohort.', call. = FALSE)
  scores <- observations |>
    dplyr::group_by(model, referencePopulation, studyId, playerId, activePhysicalPlayProfile) |>
    dplyr::summarise(meanCSAx = base::mean(CSAx), observedSeasons = dplyr::n(), .groups = 'drop') |>
    dplyr::mutate(activePhysicalEngagement = activePhysicalPlayProfile)
  estimates <- purrr::map_dfr(base::c('Forwards', 'Defensemen'), function(population) {
    data <- scores |> dplyr::filter(model == population)
    purrr::map_dfr(ratings$indicators, function(indicator) {
      n <- base::nrow(data)
      positive <- base::sum(data[[indicator]] == 1L)
      available <- n > 2L && positive > 1L && n - positive > 1L
      result <- if (available) {
        fit <- fit_analysis_workflow(data, 'meanCSAx', indicator)
        clustered_term(fit, data, term = indicator)
      } else tibble::tibble(term = indicator, estimate = NA_real_, stdError = NA_real_, confLow = NA_real_, confHigh = NA_real_, pValue = NA_real_)
      contrast_status <- if (available) 'Available' else if (n == 0L) 'No eligible coded players' else if (positive == 0L || positive == n) 'No code variation' else 'Fewer than two players in at least one code group'
      result |>
        dplyr::mutate(model = population, referencePopulation = population, indicator = indicator, n = n, positiveReports = positive, absentReports = n - positive, spearman = if (n > 1L && positive > 0L && positive < n && stats::sd(data$meanCSAx) > 0) stats::cor(data$meanCSAx, data[[indicator]], method = 'spearman') else NA_real_, contrastStatus = contrast_status, cohortStatus = ratings$status, intervalMethod = 'Player-clustered HC1; conditional on estimated scores and observed scouting cohort', .before = 1L)
    })
  })
  ratings$data <- NULL
  base::c(ratings, base::list(scores = scores, estimates = estimates))
}
