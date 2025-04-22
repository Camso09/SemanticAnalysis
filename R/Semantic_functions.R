

#' Read_text: Text_to_list_words
#'
#' @param x Path to the text to be analysed.
#'
#' @returns List of words from the text
#' @export
#'
#' @examples Read_text(~/exampleNegative.txt)
Read_text <- function(x) {
  temp <- read.delim(x, header = F, sep = "\n", stringsAsFactors = FALSE)
  temp1 <- paste(temp$V1, collapse = " ")
  sentences <- unlist(strsplit(temp1, "(?<=[.!?])\\s+", perl = TRUE))
  analysed_text <- data.frame(sentence = sentences, stringsAsFactors = FALSE)
  analysed_text <- tolower(analysed_text)
  cleaned_text <- gsub("[[:punct:]]", "", analysed_text)
  word_list <- unlist(strsplit(cleaned_text, "\\s+"))
  return(word_list)
}


#' sentiment_search: Word_search_and_match
#'
#' @param doc_words Words list from the text
#' @param sentiment_patterns vectors of positive or negative pattern words (vector) to look for in the the text we are analyzing. Vectors enable an unknown prefix or suffix.
#'
#' @returns list of the number of matched pattern in the text.
#' @export
#'
#' @examples sentiment_search(word_doc, Negative)
sentiment_search <- function(doc_words, sentiment_patterns) {
  match_count <- 0
  matched_words <- character()
  for (word in doc_words) {
    for (pattern in sentiment_patterns) {
      if (endsWith(pattern, "*")) {
        root <- sub("\\*$", "", pattern)
        if (startsWith(word, root)) {
          match_count <- match_count + 1
          matched_words <- c(matched_words, word)
          break
        }
      } else if (startsWith(pattern, "*")) {
        root <- sub("^\\*", "", pattern)
        if (endsWith(word, root)) {
          match_count <- match_count + 1
          matched_words <- c(matched_words, word)
          break
        }
      } else {
        if (word == pattern) {
          match_count <- match_count + 1
          matched_words <- c(matched_words, word)
          break
        }
      }
    }
  }
  return(list(count = match_count, words = matched_words))
}


#' analyze_sentiment
#'
#' @param x Path to the text to be analysed.
#' @param positive vectors of positive pattern words (vector) to look for in the the text we are analyzing. Vectors enable an unknown prefix or suffix.
#' @param negative vectors of negative pattern words (vector) to look for in the the text we are analyzing. Vectors enable an unknown prefix or suffix.
#'
#' @returns synthesized results as list with the predictive tendency of the sentiment from the reader perspective
#' @export
#'
#' @examples analyze_sentiment("~/Examen_1/Example_negative.txt", Positive, Negative)
analyze_sentiment <- function(x, positive, negative) {
  doc_words <- Read_text(x)
  pos_result <- sentiment_search(doc_words, positive)
  neg_result <- sentiment_search(doc_words, negative)
  sentiment_ratio <- if (neg_result$count == 0) NA else pos_result$count / neg_result$count
  result <- list(
    positive_count = pos_result$count,
    negative_count = neg_result$count,
    sentiment_ratio = sentiment_ratio,
    positive_words = unique(pos_result$words),
    negative_words = unique(neg_result$words)
  )
  return(result)
}


# How to be used.

word_doc <- Read_text("~/Documents/Courses/Intermediate_R_Data_Science_and_Visualization_Techniques_beyong_base_R/Examen_1/Example_negative.txt")

Negative <- c("overwhelm*", "nause*", "frustrat*", "trap*")
Positive <- c("cooperate", "reason*")

sentiment_search(word_doc, Negative)
test1<- sentiment_search(word_doc, Negative)
test2 <- analyze_sentiment("~/Documents/Courses/Intermediate_R_Data_Science_and_Visualization_Techniques_beyong_base_R/Examen_1/Example_negative.txt", Positive, Negative)

