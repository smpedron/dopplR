##### CORE FUNCTION 3 (CREATE SYNTHETIC RESPONDENTS) AND ADDITIONAL FUNCTIONS #####

#' Generate a Synthetic Panel of Respondents
#'
#' Creates new fictional respondents based on input profiles and simulates their responses to a survey question.
#' The synthetic respondents match the distributions and ranges of the provided profiles.
#'
#' @param ... One or more data frames of real respondent profiles to base the synthetic panel on.
#' @param variations String vector describing a different variation.
#' @param n Number of rows to generate. Default is 50.
#' @param question The prompt or question the synthetic respondents will answer.
#' @param model The model to use for the chat (defaults to "gpt-4.1")
#' @param base_url The base URL to the endpoint. Default is "https://litellmproxy.osu-ai.org".
#' @return A synthetic panel of n respondents with simulated responses to a survey question.
#' @examples
#' test1 <- data.frame(
#'   age = c(23, 45, 38, 52, 33, 33),
#'   gender = c("F", "M", "M", "F", "M", "M"),
#'   income = c(44000, 72000, 1000, 84000, 35000, 111000))
#'
#' test2 <- data.frame(
#'   educ = c("BS", "MD", "HS", "PhD", "BA"),
#'   party = c("R", "R", "D", "I", "D"))
#'
#' synth_panel <- gen_synthetic_panel(
#'   test1, test2, n = 10,
#'   question = "How do you feel about calling over texting?")
#' @export
gen_synthetic_panel <- function(..., n = 50, question = "Simulate a survey question about a generic preference, rating, or behavior.",
                                model = "GPT-4.1", base_url = "https://litellmproxy.osu-ai.org/") {

  datasets <- list(...)
  if (length(datasets) == 0) stop("Provide at least one dataset.")
  structure <- lapply(datasets, function(df) {
    tibble::tibble(
      name = names(df),
      type = sapply(df, function(x) class(x)[1])
    )
  }) |> dplyr::bind_rows() |> dplyr::distinct()

  structure_text <- paste(
    apply(structure, 1, function(x) paste0(x[1], " (", x[2], ")")),
    collapse = "\n")

  system_msg <- "You are a survey statistician generating fully synthetic, fictional responses based on sample respondent profiles. All output should be realistic but anonymized and safe for research purposes."

  user_msg <- paste0(
    "Generate ", n, " synthetic survey responses based only on the provided profiles as an R data.frame.\n",
    "All responses must be fictional, anonymized, and safe. Each row represents one individual. Follow this variable structure:\n",
    structure_text,
    "\n\nThen, for each individual, generate a realistic response to this question:\n",
    question,
    "\n\nDO NOT RETURN ANY TEXT OR EXPLANATION. RETURN ONLY R CODE. Do NOT use NA or any invalid R variable names as column names. If a value is missing for a respondent, include NA inside the vector."
  )

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
  clean_code <- clean_code %>%
    stringr::str_replace_all("([a-zA-Z0-9_]+)\\[.*>.*\\]\\s+([0-9]+)", "\\1[\\2] <- \\2") # throwing in extra cleaning

  output <- tryCatch(eval(parse(text = clean_code)), error = function(e) {
    stop("LLM did not return valid output: ", e$message)
  })
  return(output)
}
