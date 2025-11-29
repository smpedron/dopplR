##### CORE FUNCTION 2 (CREATE SYNTHETIC RESPONSES FOR ALL PROFILES IN DATASET) #####

#' Generate Synthetic Survey Responses from Existing Data
#'
#' This function generates realistic synthetic responses for survey data by leveraging text embeddings (Titan embeddings)
#' or a TF-IDF (Term Frequency – Inverse Document Frequency) fallback if embeddings fail.
#' It selects a set of respondents most similar to each target respondent and prompts a language model to produce a realistic answer to a survey question.
#' No new profiles are created. The function generates answers based on the characteristics of the input data.
#'
#' @param respondents A dataframe containing the survey respondents.
#' @param question The prompt or question to simulate responses for.
#' @param model The model to use for the chat (defaults to "gpt-4.1")
#' @param embedding_model The embedding model to use for generating vector representations (defaults to "titan-embed-text-v2:0")
#' @param base_url The base URL to the endpoint. Default is "https://litellmproxy.osu-ai.org".
#' @param k_similar Number of most similar respondents to use as reference when generating. Default is 10.
#' @param api_key API key for the embedding and chat service. Default reads from `Sys.getenv("OPENAI_API_KEY")`.
#' @return A data frame containing the input profiles along with a new column 'response' containing simulated responses to the question.
#'
#' @details
#' The function does the following:
#' 1. Builds a text corpus from character columns in `respondents`.
#' 2. Computes numeric representations of text. It attempts embeddings first, then falls back to TF-IDF vectors if embeddings fail.
#' 3. Standardizes numeric columns and combines with text embeddings.
#' 4. Computes cosine similarity between each respondent and all others.
#' 5. Selects the top `k_similar` most similar respondents for each target respondent. The target respondent’s own profile is always counted in the set of similar profiles, so the model sees their data plus the `k_similar`- 1 most similar other profiles.
#' 6. Prompts the LLM to generate a realistic, short answer based on the similar profiles.
#' 7. Cleans and stores the generated response in the `response` column.
#'
#'
#' @examples
#' respondents <- data.frame(
#'   age = c(23, 45, 38, 52, 33, 33, 51),
#'   gender = c("F", "M", "M", "F", "M", "M", "M"),
#'   department = c("HR", "IT", "IT", "Marketing", "Advertising", "Finance", "HR"),
#'   years_in_company = c(3, 4, 1, 5, 2, 8, 5),
#'   remote = c("No", "Yes", "Yes", "Yes", "No", "No", "Yes"),
#'   satisfaction =c("High", "Medium", "High", "Low", "Medium", "High", "Low"),
#'   stringsAsFactors = FALSE)
#'
#' synthetic_responses <- gen_responses_all(
#'   respondents,
#'   question = "Do you feel supported at your job?",
#'   k_similar = 3)
#'
#' @export
gen_responses_all <- function(respondents, question, model = "GPT-4.1", embedding_model = "titan-embed-text-v2:0",
                              base_url = "https://litellmproxy.osu-ai.org", k_similar = 10, api_key = Sys.getenv("OPENAI_API_KEY")
) {
  n_rows <- nrow(respondents)
  if (n_rows == 0) return(respondents)

  ## building text corpus
  text_cols <- names(respondents)[sapply(respondents, is.character)]
  text_corpus <- if (length(text_cols) == 0) {
    rep("", n_rows)
  } else {
    apply(respondents[text_cols], 1, function(r) {
      r <- ifelse(is.na(r), "", r)
      paste(r[r != ""], collapse = " | ")
    })
  }

  ## embeddings
  embeddings_matrix <- NULL
  if (nzchar(api_key)) {
    message("Attempting embeddings via API")
    res <- httr::POST(
      url = paste0(base_url, "/v1/embeddings"),
      httr::add_headers(
        `x-api-key` = api_key,
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(list(model = embedding_model, input = as.list(text_corpus)), auto_unbox = TRUE)
    )
    parsed <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
    if (!is.null(parsed$data)) {
      embeddings_list <- lapply(parsed$data$embedding, function(x) unlist(x))
      d <- length(embeddings_list[[1]])
      embeddings_matrix <- matrix(0, nrow = n_rows, ncol = d)
      for (i in seq_len(n_rows)) embeddings_matrix[i, ] <- embeddings_list[[i]]
    } else {
      warning("Embeddings API returned no valid data, switching to TF-IDF fallback.")
    }
  }

  ## if embedding fails, fallback to TF-IDF to reflect word importance
  if (is.null(embeddings_matrix)) {
    message("Using TF-IDF fallback embeddings.")
    it <- itoken(text_corpus, progressbar = FALSE)
    vocab <- create_vocabulary(it, stopwords = stopwords::stopwords("en"))
    pruned <- prune_vocabulary(vocab, term_count_min = 1, doc_proportion_max = 0.95)
    vectorizer <- vocab_vectorizer(pruned)
    dtm <- create_dtm(it, vectorizer)
    tfidf <- TfIdf$new()
    dtm_tfidf <- tfidf$fit_transform(dtm)
    embeddings_matrix <- as.matrix(dtm_tfidf)
  }

  ## numeric columns
  numeric_cols <- respondents %>% select(where(is.numeric))
  numeric_matrix <- if (ncol(numeric_cols) == 0) matrix(0, nrow = n_rows, ncol = 1) else as.matrix(scale(numeric_cols))
  numeric_matrix[is.na(numeric_matrix)] <- 0
  combined_matrix <- cbind(numeric_matrix, embeddings_matrix)

  ## Cosine similarity
  cosine_sim <- function(a, b) sum(a * b) / ((sqrt(sum(a^2)) * sqrt(sum(b^2))) + 1e-12)

  ## collapse rows
  collapse_row <- function(row, col_names) {
    vals <- lapply(seq_along(row), function(i) {
      x <- row[[i]]
      if (is.null(x) || length(x) == 0 || all(is.na(x))) return("NA")
      if (is.list(x)) return("NA")
      if (is.factor(x)) x <- as.character(x)
      as.character(x)
    })
    paste(paste0(col_names, "=", unlist(vals)), collapse = " | ")
  }

  ## generate synthetic responses
  responses <- character(n_rows)
  system_msg <- "You are a survey statistician generating realistic synthetic responses from survey profiles. Return only R code that creates a one-row data.frame with column `response`."

  for (i in seq_len(n_rows)) {
    target_vec <- combined_matrix[i, ]
    sims <- apply(combined_matrix, 1, function(x) cosine_sim(x, target_vec))
    top_idx <- head(order(sims, decreasing = TRUE), k_similar)
    similar_rows <- respondents[top_idx, , drop = FALSE]

    rows_text <- apply(similar_rows, 1, function(r) collapse_row(as.list(r), names(similar_rows)))

    user_msg <- paste0(
      "Here are ", length(top_idx), " profiles similar to the target respondent:\n",
      paste0(rows_text, collapse = "\n"),
      "\n\nQuestion: ", question,
      "\nBased on these similar profiles, produce a single realistic short answer. Return **ONLY** valid R code that when evaluated produces a one-row data.frame with column `response`."
    )

    ## llm call
    chat_obj <- ellmer::chat_openai(system_prompt = system_msg, model = model, base_url = base_url)
    chat_obj$chat(user_msg)

    ## flatten llm output
    flatten_assistant_output <- function(obj) {
      if (is.null(obj)) return("")
      if (is.character(obj)) return(paste(obj, collapse = " "))
      if (is.list(obj)) return(paste(unlist(lapply(obj, flatten_to_char)), collapse = " "))
      if (inherits(obj, "ellmer_Turn")) return(paste(sapply(obj$messages, function(m) m$content), collapse = " "))
      return(as.character(deparse(obj)))
    }

    ## grab output and clean
    assistant <- chat_obj$last_turn("assistant")
    reply_lines <- capture.output(print(assistant))
    assistant_text <- paste(reply_lines, collapse = "\n")

    assistant_text <- assistant_text %>%
      stringr::str_replace_all("```\\w*", "") %>%
      stringr::str_replace_all("```", "") %>%
      stringr::str_replace_all("<.*?>", "") %>%
      str_replace_all("\\[|\\]|,", "") %>%
      stringr::str_replace_all("([a-zA-Z0-9_]+)\\[.*>.*\\]\\s+([0-9]+)", "\\1[\\2] <- \\2") %>%
      stringr::str_trim()

    ## attach output to original data
    responses[[i]] <- paste(assistant_text)
  }
  output <- respondents
  output$response <- responses

  ## cleanup output
  output <- output %>%
    mutate(response = response %>%
             str_replace_all("^data\\.frame\\(response\\s*=\\s*", "") %>%
             str_replace_all("\"\\)$", "") %>%
             str_replace_all("^\"|\"$", "") %>%
             str_trim())

  return(output)
}
