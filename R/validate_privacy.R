
##### CORE FUNCTION 4 (GENERATES FAKE DATA MATCHING ORIGINAL DATASET) AND ADDITIONAL FUNCTIONS #####
#' Check Similarity Between Real and Synthetic Data
#'
#' For confidential data: evaluates the degree to which synthetic data matches exact values or is very close to the original data.
#' This can help identify columns where synthetic data may be too similar to real respondents, which could risk disclosure.
#'
#' @param real_data Original dataset.
#' @param synthetic_data Synthetic dataset.
#' @param threshold Proportion of overlap that triggers a flag.
#' @param tolerance Defines how close a synthetic value can be to a real value to count as overlapping.
#' @return A list summarizing each variable, including:
#'   \item{overlap_prop} proportion of synthetic values that are exact (or near) copies of real values.
#'   \item{flagged: True} column has too many copies (above the threshold) and might leak real data.
#'   \item{flagged: False} synthetic values are sufficiently different (based on threshold).
#' @examples
#' structure <- describe_structure(mtcars)
#' synth <- generate_synthetic_sample(n = 2, structure = structure, prompt = "Simulate realistic car performance data with similar distributions.")
#' validate_privacy(mtcars, synth)
#' @export
validate_privacy <- function(real_data, synthetic_data, threshold = 0.4, tolerance = 0.05) {
  common_cols <- intersect(names(real_data), names(synthetic_data))
  if (length(common_cols) == 0) stop("No common columns to compare.")

  safe <- lapply(common_cols, function(col) {
    real_col <- real_data[[col]]
    synth_col <- synthetic_data[[col]]
    if (is.factor(real_col) || is.character(real_col)) {
      overlap <- mean(synth_col %in% real_col, na.rm = TRUE)
    } else if (is.numeric(real_col)) {
      overlap <- mean(abs(synth_col - real_col) / (abs(real_col) + 1e-7) <= tolerance, na.rm = TRUE)
    } else {
      overlap <- mean(synth_col %in% real_col, na.rm = TRUE)
    }
    tibble::tibble(
      column = col,
      overlap_prop = overlap,
      flagged = overlap > threshold)
  })
  dplyr::bind_rows(safe)
}
