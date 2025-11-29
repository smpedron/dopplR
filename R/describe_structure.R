
##### CORE FUNCTION 4 (GENERATES FAKE DATA MATCHING ORIGINAL DATASET) AND ADDITIONAL FUNCTIONS #####


#' Describe data structure
#'
#' Summarizes the structure of a data frame, recording variable name, data types, and n sample values.
#'
#' @param data One or more data frames of real respondent profiles.
#' @param n Number of examples to show from each column.
#' @return A summary of each variable.
#' @examples
#' structure <- describe_structure(mtcars, n = 10)
#' @export
describe_structure <- function(data, n_examples = 100) {
  structure <- lapply(names(data), function(col) {
    vals <- head(unique(data[[col]]), n_examples)
    list(name = col, type = class(data[[col]])[1], examples = paste(vals, collapse = ", "))
  })
  structure
}
