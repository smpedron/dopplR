##### CORE FUNCTION 1 (CREATE SYNTHETIC RESPONSES FOR PROFILE DESCRIBED) ######

#' Generate a Simulated Interview Response Based on Profile Inputted
#'
#' This function loads interview segments from a text file, embeds each segment,
#' finds the most relevant segments to a given question, and then uses the OPENAI chat model to produce
#' a realistic response as if from a specific respondent.
#'
#' @param txt_file Text file containing interview segments. Each interview should be separated by a line containing only "---".
#' @param respondent_profile A brief description of the respondent's profile to guide the simulated response.
#' @param question The prompt or question to simulate responses for.
#' @param model The model to use for the chat (defaults to "gpt-4o").
#' @param embedding_model The embedding model to use (defaults to "text-embedding-3-small").
#' @param base_url The base URL to the endpoint. Default is "https://api.openai.com/v1".
#' @param k_similar Number of most similar respondents to use as reference when generating. Default is 10.
#' @param api_key API key for the embedding and chat service. Default reads from `Sys.getenv("OPENAI_API_KEY")`.
#'
#' @return A simulated response from the respondent based on the most relevant interview segments.
#'
#' @examples
#' \dontrun{
#' response <- gen_response(
#' txt_file = "cces_observations.txt",
#' respondent_profile = "30-year-old Asian-American female.",
#' k_similar = 3,
#' question = "What do you know about the Luka Doncic trade to the Lakers?")
#' }
#'
#' @export
gen_response <- function(txt_file, respondent_profile, question, k_similar = 10,
                         model = "gpt-4o", embedding_model = "text-embedding-3-small", base_url = "https://api.openai.com/v1",
                         api_key = Sys.getenv("OPENAI_API_KEY")
) {
  get_embeddings <- function(txt) {
    tryCatch({
      res <- httr::POST(
        url = paste0(base_url, "/embeddings"),
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"),
        body = jsonlite::toJSON(list(model = embedding_model, input = txt), auto_unbox = TRUE))
      if (res$status_code != 200) {
        message("HTTP ", res$status_code, " returned for embedding request")
        message("Response body: ", httr::content(res, "text", encoding = "UTF-8"))
        stop("Embedding request failed")
      }
      parsed <- tryCatch(
        jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE),
        error = function(e) NULL)
      if (!is.null(parsed) &&
          "data" %in% names(parsed) &&
          length(parsed$data) >= 1 &&
          is.list(parsed$data[[1]]) &&
          "embedding" %in% names(parsed$data[[1]])) {
        return(unlist(parsed$data[[1]][["embedding"]]))
      } else {
        stop("No embedding returned in API response")
      }
    }, error = function(e) {
      message("Embedding failed, using fallback vector: ", e$message)
      fallback_len <- ifelse(embedding_model == "text-embedding-3-small", 1536, 768)
      rep(1, fallback_len)
    })
  }

  ## embed interviews one by one
  con <- file(text_file, open = "r")
  interviews <- list()
  embeddings_list <- list()
  current_segment <- ""
  while(length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if (grepl("^---$", line)) {  # separator for interview segments
      if (nchar(trimws(current_segment)) > 50) {
        trimmed <- trimws(current_segment)
        interviews <- append(interviews, list(trimmed))
        embeddings_list <- append(embeddings_list, list(get_embeddings(trimmed)))
      }
      current_segment <- ""
    } else {
      current_segment <- paste(current_segment, line, sep = "\n")
    }
  }
  if (nchar(trimws(current_segment)) > 50) {
    trimmed <- trimws(current_segment)
    interviews <- append(interviews, list(trimmed))
    embeddings_list <- append(embeddings_list, list(get_embeddings(trimmed)))
  }
  close(con)
  message("Loaded and embedded ", length(interviews), " interview segments.")

  ## question embed
  q_emb <- get_embeddings(question)

  ## cosine similarity
  cosine_sim <- function(a, b) sum(a * b) / ((sqrt(sum(a^2)) * sqrt(sum(b^2))) + 1e-12)
  sims <- sapply(embeddings_list, function(x) cosine_sim(x, q_emb))
  top_idx <- head(order(sims, decreasing = TRUE), k_similar)
  context_text <- paste(interviews[top_idx], collapse = "\n\n---\n\n")

  ## generate response
  system_msg <- paste0(
    "You are simulating an interview respondent. ",
    "Use the tone, content, and reasoning from the similar responses provided. ",
    "Produce a realistic short answer as if this respondent answered the question.")

  user_msg <- paste0(
    "Respondent profile: ", respondent_profile, "\n\n",
    "Here are ", k_similar, " interview responses most related to the question:\n",
    context_text, "\n\n",
    "Question: ", question, "\n\n",
    "Generate how this respondent might realistically answer.")

  chat_obj <- ellmer::chat_openai(system_prompt = system_msg, model = model, base_url = base_url)
  chat_obj$chat(user_msg)
  assistant <- chat_obj$last_turn("assistant")
  reply_lines <- capture.output(print(assistant))
  assistant_text <- paste(reply_lines, collapse = "\n")
  assistant_text <- str_replace_all(assistant_text, "```.*?```", "")
  str_trim(assistant_text)
}
