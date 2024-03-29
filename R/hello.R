#' wordcountaddin
#'
#' This packages is an addin for RStudio that will count the words and characters in a plain text document. It is designed for use with R markdown documents and will exclude YAML header content, code chunks and inline code from the counts. It also computes readability statistics so you can get an idea of how easy or difficult your text is to read.
#'
#' @name wordcountaddin
#' @docType package
#' @import purrr stringi koRpus
NULL

# global things

 md_file_ext_regex <- paste(
    "\\.markdown$",
    "\\.mdown$",
    "\\.mkdn$",
    "\\.md$",
    "\\.mkd$",
    "\\.mdwn$",
    "\\.mdtxt$",
    "\\.mdtext$",
    "\\.rmd$",
    "\\.Rmd$",
    "\\.RMD$",
    "\\.Rmarkdown$",
    "\\.qmd$",
  sep = "|")


#-------------------------------------------------------------------
# fns for working with selected text in an active Rmd

#' Get text stats for selected text (excluding code chunks and inline code)
#'
#' Call this addin to get a word count and some other stats about the text
#' @param filename Path to the file on which to compute text stats.
#' Default is the current file (when working in RStudio) or the file being
#' knit (when compiling with \code{knitr}).
#'
#' @export
#' @examples
#' md <- system.file(package = "wordcountaddin", "NEWS.md")
#' text_stats(md)
#' word_count(md)
#' \dontrun{
#' readability(md)
#' }
text_stats <- function(filename = this_filename()) {

  text_to_count_output <- text_to_count(filename)

  text_stats_fn(text_to_count_output)
}


#' @rdname text_stats
#' @description Get a word count as a single integer
#' @export
word_count <- function(filename = this_filename()){

  text_to_count_output <- text_to_count(filename)

  word_count_output <- text_stats_fn_(text_to_count_output)

  word_count_output$n_words_korp
}






#' @rdname text_stats
#' @description Get readability stats for selected text (excluding code chunks)
#' @param quiet Logical. Should task be performed quietly?
#'
#' @details Call this addin to get readbility stats about the text
#'
#' @export
readability <- function(filename = this_filename(), quiet = TRUE) {


  text_to_count_output <- text_to_count(filename)

  readability_fn(text_to_count_output, quiet = TRUE)
}

#---------------------------------------------------------------
# directly work on a character string in the console


#' @rdname text_stats
#' @description Get text stats for selected text (excluding code chunks and inline code)
#'
#' @details Use this function with a character string as input
#'
#' @export
text_stats_chr <- function(text) {

  text <- paste(text, collapse="\n")

  text_stats_fn(text)

}


#' @rdname text_stats
#' @description Get readability stats for selected text (excluding code chunks)
#'
#' @details Use this function with a character string as input
#'
#' @param text a character string of text, length of one
#'
#' @export
readability_chr <- function(text, quiet = TRUE) {

  text <- paste(text, collapse = "\n")

  readability_fn(text, quiet = TRUE)

}
#-----------------------------------------------------------
# helper fns, not exported

text_to_count <- function(filename){
  # selected text takes precedence over the filename argument:
  # if text is selected, it is used. Otherwise, the text in filename is used
  if (rstudioapi::isAvailable()) {
    context <- rstudioapi::getActiveDocumentContext()
    selection_text <- unname(unlist(context$selection)["text"])
    text_is_selected <- nchar(selection_text) > 0
  } else {
    # if not running in RStudio, assume no text is selected
    text_is_selected <- FALSE
  }

  if (text_is_selected) {
    text <- selection_text
  } else {
    # if no text is selected, read text from "filename" as character vector
    is_extension_invalid <- !grepl(md_file_ext_regex, filename)
    if (is_extension_invalid) {
      stop(paste("The supplied file has an extension which is not associated with markdown.",
                 "This function only works with markdown or R markdown files.", sep = "\n  "))
    }
    text <- paste(scan(filename, 'character', quiet = TRUE), collapse = " ")
  }
  text
}

prep_text <- function(text){

  # remove all line breaks, http://stackoverflow.com/a/21781150/1036500
  text <- gsub("[\r\n]", " ", text)

  # don't include yaml front matter
  three_dashes <- unlist(gregexpr('---', text))
  if (three_dashes[1]==1L) {
    yaml_end <- three_dashes[2] + 2L
    text <- substr(text, yaml_end + 1L, nchar(text))
  } else {
    text
  }

  # don't include text in code chunks: https://regex101.com/#python
  text <- gsub("```\\{.+?\\}.+?```", "", text)

  # don't include text in in-line R code
  text <- gsub("`r.+?`", "", text)

  # don't include HTML comments
  text <- gsub("<!--.+?-->", "", text)

  # don't include LaTeX comments
  # how to do this? %%

  # don't include images with captions
  text <- gsub("!\\[.+?\\]\\(.+?\\)\\{.+?\\}", "", text)
  text <- gsub("!\\[.+?\\]\\(.+?\\)", "", text)

  # don't include inline markdown URLs
  text <- gsub("\\(http.+?\\)", "", text)

  # don't include # for headings
  text <- gsub("#*", "", text)

  # don't include opening html tags
  # (source: https://www.w3schools.com/TAGS/default.ASP)

  tags <- paste0("!DOCTYPE|a|abbr|acronym|address|applet|area|article|aside|",
                 "audio|b|base|basefont|bdi|bdo|big|blockquote|body|br|button|",
                 "canvas|caption|center|cite|code|col|colgroup|data|datalist|",
                 "dd|del|details|dfn|dialog|dir|div|dl|dt|em|embed|fieldset|",
                 "figcaption|figure|font|footer|form|frame|frameset|h1 to h6|",
                 "head|header|hr|html|i|iframe|img|input|ins|kbd|label|legend|",
                 "li|link|main|map|mark|meta|meter|nav|noframes|noscript|",
                 "object|ol|optgroup|option|output|p|param|picture|pre|",
                 "progress|q|rp|rt|ruby|s|samp|script|section|select|small|",
                 "source|span|strike|strong|style|sub|summary|sup|svg|table|",
                 "tbody|td|template|textarea|tfoot|th|thead|time|title|tr|",
                 "track|tt|u|ul|var|video|wbr")

  text <- gsub(paste0("<\\s*(",tags,")[^>]*>"),"", text)

  # don't include closing html tags
  text <- gsub("</.+?>", "", text)

  # don't include greater/less than signs because they trip up koRpus
  text <- gsub("<|>", "", text)

  # don't include percent signs because they trip up stringi
  text <- gsub("%", "", text)

  # don't include figures and tables inserted using plain LaTeX code
  text <- gsub("\\\\begin\\{figure\\}(.*?)\\\\end\\{figure\\}", "", text)
  text <- gsub("\\\\begin\\{table\\}(.*?)\\\\end\\{table\\}", "", text)

  # don't count abbreviations as multiple words, but leave
  # the period at the end in case it's the end of a sentence
  text <- gsub("\\.(?=[a-z]+)", "", text, perl = TRUE)

  # don't include LaTeX \eggs{ham}
  # how to do? problem with capturing \x

  # remove lines starting with :::
  text <- gsub(":::.*", "", text)
 


  if(nchar(text) == 0){
    stop("You have not selected any text. Please select some text with the mouse and try again")
  }

  return(text)

}

prep_text_korpus <- function(text){
  lengths <- unlist(strsplit(text, " "))
  no_long_one <- paste0(ifelse(nchar(lengths) > 30, substr(lengths, 1, 10), lengths), collapse = " ")
  tokenize_safe <- purrr::safely(koRpus::tokenize)
  k1 <- tokenize_safe(no_long_one, lang = 'en', format = 'obj')
  k1 <- k1$result
  return(k1)
}


# These functions do the actual work

#' @rdname text_stats
#' @export
text_stats_fn_ <- function(text){
  # suppress warnings
  oldw <- getOption("warn")
  options(warn = -1)

  text <- prep_text(text)

  require("koRpus.lang.en", quietly = TRUE)

  # stringi methods
  n_char_tot <- sum(stri_stats_latex(text)[c(1,3)])
  n_words_stri <- unname(stri_stats_latex(text)[4])

  #korpus methods
  k1 <- prep_text_korpus(text)
  korpus_stats <- sylly::describe(k1)
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


readability_fn_ <- function(text, quiet = TRUE){

  text <- prep_text(text)

  oldw <- getOption("warn")
  options(warn = -1)

  require("koRpus.lang.en", quietly = TRUE)

  # korpus methods
  k1 <- prep_text_korpus(text)
  k_readability <- koRpus::readability(k1, quiet = TRUE)

  return(k_readability)

  # resume warnings
  options(warn = oldw)
}


readability_fn <- function(text, quiet = TRUE){
  # a more condensed overview of the results
  k_readability <- readability_fn_(text, quiet = TRUE)
  readability_summary_table <- knitr::kable(summary(k_readability))
  return(readability_summary_table)

}
