##### CORE FUNCTION 4 (GENERATES FAKE DATA MATCHING ORIGINAL DATASET) AND ADDITIONAL FUNCTIONS #####

#' Generate a synthetic dataset
#'
#' Creates a dataset with synthetic data that matches the variables, distributions, ranges, and data types of the input structure.
#'
#' @param n Number of rows of synthetic data to generate.
#' @param structure The output from `describe_structure` function telling the generator what columns and datatypes to use.
#' @param prompt Instructions to give the LLM
#' @param model The model to use for the chat (defaults to "gpt-4.1")
#' @param base_url The base URL to the endpoint. Default is "https://litellmproxy.osu-ai.org".
#' @return A data frame containing synthetic values, with the same variables (columns) as the original data.
#' @examples
#' structure <- describe_structure(mtcars)
#' synth <- gen_synthetic_sample(n = 2, structure = structure, prompt = "Simulate realistic car performance data with similar distributions.")
#' @export
gen_synthetic_dataset <- function(n = 10, structure = NULL, prompt = NULL, model = "GPT-4.1",
                                  base_url = "https://litellmproxy.osu-ai.org/") {

  system_msg <- paste("You are an expert data generator that produces realistic but fully synthetic tabular datasets.")

  structure_text <- if (!is.null(structure)) {
    paste(capture.output(str(structure, max.level = 2)), collapse = "\n")
  } else {""
  }

  user_msg <- paste0(
    "Generate ", n, " rows of synthetic data as an R data.frame. ",
    if (structure_text != "") paste0("\nFollow this structure:\n", structure_text),
    if (!is.null(prompt)) paste0("\nAdditional context:\n", prompt),
    "\nDO NOT RETURN ANY TEXT OR EXPLANATION. RETURN ONLY R CODE.")

  chat_obj <- ellmer::chat_openai(
    system_prompt = system_msg,
    model = model,
    base_url = base_url)

  chat_obj$chat(user_msg)
  last_turn <- chat_obj$last_turn("assistant")
  reply_lines <- capture.output(print(last_turn))
  reply_text <- paste(reply_lines, collapse = "\n")

  clean_code <- reply_text %>%
    str_replace_all("```[a-zA-Z]*", "") %>%
    str_replace_all("```", "") %>%
    str_replace_all("<.*?>", "") %>%
    str_trim()

  output <- tryCatch(eval(parse(text = clean_code)), error = function(e) {
    stop("LLM did not return valid output: ", e$message)
  })

  return(output)
}
