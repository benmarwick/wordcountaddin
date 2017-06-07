#' wordcountaddin
#'
#' This packages is an addin for RStudio that will count the words and characters in a plain text document. It is designed for use with R markdown documents and will exclude YAML header content, code chunks and inline code from the counts. It also computes readability statistics so you can get an idea of how easy or difficult your text is to read.
#'
#' @name wordcountaddin
#' @docType package
#' @import purrr stringi koRpus
NULL

#-------------------------------------------------------------------
# fns for working with selected text in an active Rmd

#' Get text stats for selected text (excluding code chunks and inline code)
#'
#' Call this addin to get a word count and some other stats about the text
#'
#' @export
text_stats <- function(filename = "") {

  text_to_count <-

  if(nchar(filename) > 0){
    # if a filename is supplied, check that it is a md or rmd file
    if(!grepl(".md$|.rmd$|.Rmd$|.RMD$", filename)){
           stop("The file supplied is not a .md or .Rmd file. This function only works with markdown or R markdown files.")
    } else {
      # if we have an md or Rmd file, read it in as a character vector
      paste(scan(filename, 'character', quiet = TRUE), collapse = " ")
    }

    } else  {

    # if we don'thave a filename, then work with current Rmd in RStudio
    context <- rstudioapi::getActiveDocumentContext()

    # get selection text and full text of Rmd
    selection_text <- unname(unlist(context$selection)["text"])
    entire_document_text <- paste(scan(context$path, 'character', quiet = TRUE), collapse = " ")

    # if the selection has no characters (ie. there is no selection), then count the words in the full text of the Rmd
      if(nchar(selection_text) > 0){
        selection_text
      } else  {
        entire_document_text
      }
  }





  text_stats_fn(text_to_count)
}

#' Get readability stats for selected text (excluding code chunks)
#'
#' Call this addin to get readbility stats about the text
#'
#' @export
readability <- function(filename = "") {

  text_to_count <-

    if(nchar(filename) > 0){
      # if a filename is supplied, check that it is a md or rmd file
      if(!grepl(".md$|.rmd$|.Rmd$|.RMD$", filename)){
        stop("The file supplied is not a .md or .Rmd file. This function only works with markdown or R markdown files.")
      } else {
        # if we have an md or Rmd file, read it in as a character vector
        paste(scan(filename, 'character', quiet = TRUE), collapse = " ")
      }

    } else  {

      # if we don'thave a filename, then work with current Rmd in RStudio
      context <- rstudioapi::getActiveDocumentContext()

      # get selection text and full text of Rmd
      selection_text <- unname(unlist(context$selection)["text"])
      entire_document_text <- paste(scan(context$path, 'character', quiet = TRUE), collapse = " ")

      # if the selection has no characters (ie. there is no selection), then count the words in the full text of the Rmd
      if(nchar(selection_text) > 0){
        selection_text
      } else  {
        entire_document_text
      }
    }



  readability_fn(text_to_count)
}

#---------------------------------------------------------------
# directly work on a character string in the console


#' Get text stats for selected text (excluding code chunks and inline code)
#'
#' Use this function with a character string as input
#'
#' @export
text_stats_chr <- function(text) {

  text <- paste(text, collapse="\n")

  text_stats_fn(text)

}


#' Get readability stats for selected text (excluding code chunks)
#'
#' Use this function with a character string as input
#'
#' @param text a character string of text, length of one
#'
#' @export
readability_chr <- function(text) {

  text <- paste(text, collapse = "\n")

  readability_fn(text)

}
#-----------------------------------------------------------
# helper fns

prep_text <- function(text){

  # remove all line breaks, http://stackoverflow.com/a/21781150/1036500
  text <- gsub("[\r\n]", " ", text)

  # don't include front yaml
  text <- gsub("---.*--- ", "", text)

  # don't include text in code chunks: https://regex101.com/#python
  text <- gsub("```\\{.+?\\}.+?```", "", text)

  # don't include text in in-line R code
  text <- gsub("`r.+?`", "", text)

  # don't include HTML comments
  text <- gsub("<!--.+?-->", "", text)

  # don't include LaTeX comments
  # how to do this? %%

  # don't include inline markdown URLs
  text <- gsub("\\(http.+?\\)", "", text)

  # don't include images with captions
  text <- gsub("!\\[.+?\\)", "", text)

  # don't include # for headings
  text <- gsub("#*", "", text)

  # don't include html tags
  text <- gsub("<.+?>|</.+?>", "", text)

  # don't include LaTeX \eggs{ham}
  # how to do? problem with capturing \x


  if(nchar(text) == 0){
    stop("You have not selected any text. Please select some text with the mouse and try again")
  } else {

  return(text)

  }

}

prep_text_korpus <- function(text){
  requireNamespace("purrr")
  tokenize_safe <- safely(tokenize)
  k1 <- tokenize_safe(text, lang = 'en', format = 'obj')
  k1 <- k1$result
  return(k1)
}


# These functions do the actual work
text_stats_fn_ <- function(text){
  # suppress warnings
  oldw <- getOption("warn")
  options(warn = -1)

  text <- prep_text(text)


  requireNamespace("stringi")
  requireNamespace("koRpus")

  # stringi methods
  n_char_tot <- sum(stri_stats_latex(text)[c(1,3)])
  n_words_stri <- unname(stri_stats_latex(text)[4])

  #korpus methods
  k1 <- prep_text_korpus(text)
  korpus_stats <- describe(k1)
  k_nchr <- korpus_stats$all.chars
  k_wc <- korpus_stats$words
  k_sent <- korpus_stats$sentences
  k_wps <- k_wc / k_sent

  # reading time
  # https://en.wikipedia.org/wiki/Words_per_minute#Reading_and_comprehension
  # assume 200 words per min
  wpm <-  200
  reading_time_korp <- paste0(round(k_wc / wpm, 1), " minutes")
  reading_time_stri <- paste0(round(n_words_stri / wpm, 1), " minutes")

  return(list(
  # make the names more useful
  n_char_tot_stri = n_char_tot,
  n_char_tot_korp = k_nchr,
  n_words_korp = k_wc,
  n_words_stri = n_words_stri,
  n_sentences_korp = k_sent,
  words_per_sentence_korp = k_wps,
  reading_time_korp = reading_time_korp,
  reading_time_stri = reading_time_stri
  ))

  # resume warnings
  options(warn = oldw)

}


text_stats_fn <- function(text){

  l <- text_stats_fn_(text)

  results_df <- data.frame(Method = c("Word count", "Character count", "Sentence count", "Reading time"),
                           koRpus  = c(l$n_words_korp, l$n_char_tot_korp, l$n_sentences_korp, l$reading_time_korp),
                           stringi = c(l$n_words_stri, l$n_char_tot_stri, "Not available", l$reading_time_stri)
                           )

  results_df_tab <- knitr::kable(results_df)
  return(results_df_tab)

}


readability_fn_ <- function(text){

  text <- prep_text(text)

  oldw <- getOption("warn")
  options(warn = -1)

 requireNamespace("koRpus")

  # korpus methods
  k1 <- prep_text_korpus(text)
  k_readability <- koRpus::readability(k1)


  return(k_readability)

  # resume warnings
  options(warn = oldw)
}


readability_fn <- function(text){
  # a more condensed overview of the results
  k_readability <- readability_fn_(text)
  readability_summary_table <- knitr::kable(summary(k_readability))
  return(readability_summary_table)

}
