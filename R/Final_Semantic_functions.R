#' Title List of words to be analyzed
#'
#' @param x Path to the file
#'
#' @returns list of words from the text to be analysed.
#' @export
#'
#' @examples
#' Read_text("~/Documents/Courses/Intermediate_R_Data_Science_and_Visualization_Techniques_beyong_base_R/Examen_1/Example_negative.txt")
Read_text <- function(x) {
  temp <- read.delim(x, header = F, sep = "\n", stringsAsFactors = FALSE)
  temp_1 <- paste(temp$V1, collapse = " ")

  sentences <- unlist(strsplit(temp_1, "(?<=[.,!?])\\s+", perl = TRUE))

  analysed_text <- data.frame(sentence = sentences, stringsAsFactors = FALSE)
  analysed_text$sentence <- tolower(analysed_text$sentence)

  cleaned_text <- gsub("[[:punct:]]", "", paste(analysed_text$sentence, collapse = " ")) # paste(text_vector, collapse = " ")

  word_list <- unlist(strsplit(cleaned_text, "\\s+"))
  word_list <- word_list[word_list != ""]
  return(word_list)
}


#' Title Sentiments matching word in the text
#'
#' @param doc_words document word list
#' @param sentiment_patterns vectors of positive or negative pattern words (vector) to look for in the the text we are analyzing. Vectors enable an unknown prefix or suffix.
#'
#' @returns list of the number of matched pattern in the text an the exact words.
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

#' Title
#'
#' @param x Path to the file
#' @param positive vectors of positive pattern words (vector) to look for in the the text we are analyzing. These Vectors enable an unknown prefix or suffix by use of (*) after the root word.
#' @param negative vectors of negative pattern words (vector) to look for in the the text we are analyzing. These Vectors enable an unknown prefix or suffix by use of (*) after the root word.
#'
#' @returns synthetised results as list with the tendency of the sentiment from the reader perspective
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
