
##### CORE FUNCTION 4 (GENERATES FAKE DATA MATCHING ORIGINAL DATASET) AND ADDITIONAL FUNCTIONS #####
#' Iterate through Variation for Synthetic Data Tuning
#'
#' Generates multiple synthetic datasets using different variations or scenarios, allowing you to tune and compare outputs.
#'
#' @param structure The output from the `describe_structure` function telling the generator what columns and datatypes to use.
#' @param variations String vector describing a different variation.
#' @param n Number of rows to generate per dataset.
#' @param model The model to use for the chat (defaults to "gpt-4.1")
#' @param base_url The base URL to the endpoint. Default is "https://litellmproxy.osu-ai.org".
#' @return A list of data.frames by variation.
#' @examples
#' structure <- describe_structure(mtcars)
#' synth <- generate_synthetic_sample(n = 2, structure = structure, prompt = "Simulate realistic car performance data with similar distributions.")
#' variations <- c("more variance", "include some missing values")
#' tuned <- synthetic_tune(structure, variations)
#'
#' synth1 <- tuned[[1]]
#' real_var <- sapply(mtcars, var)
#' synthetic_var <- sapply(synth1, var)
#' tibble(variable = names(real_var), real = real_var, synthetic = synthetic_var, ratio = synthetic / real)
#' @export
synthetic_tune <- function(structure, variations, n = 5, model = "GPT-4.1", base_url = "https://litellmproxy.osu-ai.org/") {
  out <- lapply(variations, function(v) {
    data_x <- generate_synthetic_sample(
      n = n,
      structure = structure,
      prompt = paste("Generate data with variation:", v),
      model = model,
      base_url = base_url)
    attr(data_x, "variation") <- v
    data_x
  })
  names(out) <- variations
  out
}


