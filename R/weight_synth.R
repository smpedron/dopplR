
##### CORE FUNCTION 3 (CREATE SYNTHETIC RESPONDENTS) AND ADDITIONAL FUNCTIONS #####
#' Weight Synthetic Respondents to Match Target Marginals
#'
#' Adjusts the weights of synthetic respondents so that the marginal distributions of specified variables align with a target population.
#' Weights are normalized.
#'
#' @param synth_data A data frame of synthetic respondents to be re-weighted.
#' @param target_margins A list or data frame specifying the desired marginal distributions for each variable.
#' @param vars A character vector of variable names in `synth_data` to match to the target marginals.
#' @param replacement Logical: if `TRUE`, allows sampling with replacement when adjusting weights. Default is `FALSE`.
#' @return A data frame with the same rows as `synth_data`, with an additional column of weights reflecting the target margins.
#' @examples
#' synth_data <- data.frame(
#'   age = c(25, 35, 45, 30, 22, 41, 33),
#'   gender = c("M", "F", "F", "M", "F", "F", "M"))
#'
#' target_margins <- list(
#'   age = c("20-29" = 0.25, "30-39" = 0.50, "40-49" = 0.25),
#'   gender = c("M" = 0.5, "F" = 0.5))
#'
#' weighted_data <- weight_synth(synth_data, target_margins, vars = c("age", "gender"))
#' @export
weight_synth <- function(synth_data, target_margins, vars, replacement = FALSE) {
  stopifnot(all(vars %in% names(synth_data)))

  weights <- rep(1, nrow(synth_data))

  for (v in vars) {
    current <- prop.table(table(synth_data[[v]]))
    target <- target_margins[[v]]
    if (is.null(target)) next
    ratio <- target[names(current)] / current
    ratio[is.na(ratio)] <- 1
    weights <- weights * ratio[as.character(synth_data[[v]])]
  }

  # normalize weights
  weights <- weights / mean(weights)
  synth_data$weight <- weights

  if (replacement) {
    synth_data <- synth_data[sample(
      seq_len(nrow(synth_data)),
      size = nrow(synth_data),
      replace = TRUE,
      prob = synth_data$weight
    ), ]
  }
  return(synth_data)
}

#
