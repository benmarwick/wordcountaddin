#' wordcountaddin
#'
#' This packages is an addin for RStudio that will count the words and characters in a plain text document. It is designed for use with R markdown documents and will exclude YAML header content, code chunks and inline code from the counts. It also computes readability statistics so you can get an idea of how easy or difficult your text is to read.
#'
#' @name wordcountaddin
#' @docType package
#' @import  qdap purrr stringi koRpus
NULL


#' Get text stats for selected text (excluding code chunks and inline code)
#'
#' Call this addin to get a word count and some other stats about the text
#'
#' @export
text_stats <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  selection <- context$selection
  text <- unlist(selection)["text"]
  text_stats_fn(text)
}

#' Get readability stats for selected text (excluding code chunks)
#'
#' Call this addin to get readbility stats about the text
#'
#' @export
readability <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  selection <- context$selection
  text <- unlist(selection)["text"]
  readability_fn(text)
}

prep_text <- function(text){

  # remove all line breaks, http://stackoverflow.com/a/21781150/1036500
  text <- gsub("[\r\n]", " ", text)

  # don't inlcude front yml
  text <- gsub("---.*--- ", "", text)

  # don't include text in code chunks: https://regex101.com/#python
  text <- gsub("```\\{.+?\\}.+?```", "", text)

  # don't include text in in-line R code
  text <- gsub("`r.+?`", "", text)

  if(nchar(text) == 0){
    stop("You have not selected any text. Please select some text with the mouse and try again")
  } else {

  return(text)

  }

}


prep_text_qdap <- function(text){

  requireNamespace("qdap")
  requireNamespace("purrr")
  # prepare for qdap functions
  df <-  data.frame(text.var = text)
  # check_text(df)

  sentSplit_safe <- safely(sentSplit)
  df_ss <- sentSplit_safe(df, text.var = "text.var")

  word_stats_safe <- safely(word_stats)
  df_ws <- word_stats_safe(df_ss$result, digit.remove = TRUE, apostrophe.remove	= TRUE)

  return(list(df_ss = df_ss, df_ws = df_ws))
}

prep_text_korpus <- function(text){
  requireNamespace("purrr")
  tokenize_safe <- safely(tokenize)
  k1 <- tokenize_safe(text, lang = 'en', format = 'obj')
  k1 <- k1$result
  return(k1)
}


# These functions do the actual work
text_stats_fn <- function(text){

  text <- prep_text(text)

  # qdap gives a lot of warnings!
  oldw <- getOption("warn")
  options(warn = -1)

  requireNamespace("qdap")
  requireNamespace("stringi")
  requireNamespace("koRpus")

  # qdap methods
  qdap_output <-  prep_text_qdap(text)
  df_ws <- qdap_output$df_ws$result
  # df_ws$ts # word count, char count, syllable count per sentence
  # df_ws$gts # totals
  # basic stats on the text:
  n_words_qdap <- df_ws$gts$n.words # -1
  # n_sent <- df_ws$gts$n.sent
  n_sent_qdap <- nrow(qdap_output$df_ss$result)
  # wps <- df_ws$gts$wps
  wps_qdap <- df_ws$gts$n.words / n_sent_qdap

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
  reading_time <- paste0(round(k_wc / 200, 0), " minutes") # assume 200 words per min


  return(list(n_char_tot_stri = n_char_tot,
              n_char_tot_korp = k_nchr,
              n_words_stri = n_words_stri,
              n_words_qdap = n_words_qdap,
              n_words_korp = k_wc,
              n_sentences_qdap = n_sent_qdap,
              n_sentences_korp = k_sent,
              words_per_sentence_qdap = wps_qdap,
              words_per_sentence_korp = k_wps,
              reading_time = reading_time))

  # resume warnings
  options(warn = oldw)

}


readability_fn <- function(text){

  text <- prep_text(text)

  oldw <- getOption("warn")
  options(warn = -1)

  # requireNamespace(qdap)
  # requireNamespace(purrr)
  # requireNamespace(stringi)
  requireNamespace("koRpus")

  # readability stats

  # # qdap methods
  # df_ss <-  prep_text_qdap(text)
  #
  # # https://en.wikipedia.org/wiki/Automated_readability_index
  # ari_fn <- safely(automated_readability_index)
  # ari <- ari_fn(df_ss)$result$Readability$Automated_Readability_Index
  #
  # # https://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_tests
  # fk_grd_fn <- safely(flesch_kincaid)
  # fk_grd <- fk_grd_fn(df_ss)$result$Readability$FK_grd.lvl
  # fk_ease_fn <- safely(flesch_kincaid)
  # fk_ease <- fk_ease_fn(df_ss)$result$Readability$FK_read.ease
  #
  # # https://en.wikipedia.org/wiki/Coleman%E2%80%93Liau_index
  # cl_fn <- safely(coleman_liau)
  # coleman_liau <- cl_fn(df_ss)$result$Readability$Coleman_Liau
  #
  # # https://en.wikipedia.org/wiki/Linsear_Write
  # lw_fn <- safely(linsear_write)
  # Linsear_Write <- lw_fn(df_ss)$result$Readability$Linsear_Write
  #
  # # https://en.wikipedia.org/wiki/SMOG
  # smog_fn <- safely(SMOG)
  # sm <- smog_fn(df_ss)$result

  # korpus methods
  k1 <- prep_text_korpus(text)
  k_readability <- koRpus::readability(k1)


  return(list(k_readability = k_readability
              # automated_readability_index = ari,
              # Flesch_Kincaid_reading_grade_level = fk_grd,
              # Flesch_Kincaid_reading_ease = fk_ease,
              # Coleman_Liau = coleman_liau,
              # Linsear_Write = Linsear_Write,
              # SMOG = sm
              ))

  # resume warnings
  options(warn = oldw)
}

