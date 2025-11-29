
##### CORE FUNCTION 4 (GENERATES FAKE DATA MATCHING ORIGINAL DATASET) AND ADDITIONAL FUNCTIONS #####
#' Compare Data Distributions
#'
#' Compares distributions of real and synthetic data using a Kolmogorov-Smirnov test.
#'
#' @param real_data Original dataset.
#' @param synthetic_data Synthetic dataset.
#' @return Tibble with means by variable for real and synthetic data and the corresponding KS p-value.
#' @examples
#' structure <- describe_structure(mtcars)
#' synth <- generate_synthetic_sample(n = 2, structure = structure, prompt = "Simulate realistic car performance data with similar distributions.")
#' compare_similarity(mtcars, synth)
#' @export
compare_similarity <- function(real_data, synthetic_data) {
  common <- intersect(names(real_data), names(synthetic_data))
  numeric_cols <- common[sapply(real_data[common], is.numeric)]
  comp <- lapply(numeric_cols, function(col) {
    ks <- suppressWarnings(ks.test(real_data[[col]], synthetic_data[[col]])$p.value)
    tibble::tibble(
      variable = col,
      mean_real = mean(real_data[[col]], na.rm = TRUE),
      mean_synth = mean(synthetic_data[[col]], na.rm = TRUE),
      ks_p_value = ks
    )
  })
  dplyr::bind_rows(comp)
}

