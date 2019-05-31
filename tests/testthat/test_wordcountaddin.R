library(wordcountaddin)

context("Word count")

test_that("Word count is correct for short simple sentence", {
  # short sentence
  eleven_words <- "here are exactly eleven words of fairly boring and unpunctuated text"

  short_stats <-  text_stats_fn_(eleven_words)
  # qdap cannot manage without final punct.
  n_words_stri_11 <-short_stats$n_words_stri
  n_words_korp_11 <- short_stats$n_words_korp

  n_char_tot_stri_11 <-  short_stats$n_char_tot_stri
  n_char_tot_korp_11 <- short_stats$n_char_tot_korp

  expect_equal(n_words_stri_11, 11)
  expect_equal(n_words_korp_11, 11)
  expect_equal(n_char_tot_stri_11, 68)
  expect_equal(n_char_tot_korp_11, 69)
})

test_that("Word count is correct for moderately complex sentences", {
  # Moderate: Harvard sentences, https://en.wikipedia.org/wiki/Harvard_sentences
  moderately_complex <- "The birch canoe slid on the smooth planks. Glue the sheet to the dark blue background. It's easy to tell the depth of a well. These days a chicken leg is a rare dish. Rice is often served in round bowls. The juice of lemons makes fine punch. The box was thrown beside the parked truck. The hogs were fed chopped corn and garbage. Four hours of steady work faced us. Large size in stockings is hard to sell."

  moderately_complex_stats <- text_stats_fn_(moderately_complex)

  n_char_tot_stri_mc <-  moderately_complex_stats$n_char_tot_stri
  n_char_tot_korp_mc <- moderately_complex_stats$n_char_tot_korp

  n_words_stri_mc <- moderately_complex_stats$n_words_stri
  n_words_korp_mc <- moderately_complex_stats$n_words_korp

  n_sentences_korp_mc <- moderately_complex_stats$n_sentences_korp

  expect_equal(n_char_tot_stri_mc, 406)
  expect_equal(n_char_tot_korp_mc, 407)
  expect_equal(n_words_stri_mc, 80)  # MS Word says 79
  expect_equal(n_words_korp_mc, 80)
  expect_equal(n_sentences_korp_mc, 10)
})



test_that("Word count is correct for complex sentences in filler text", {
  # Filler text with various punctuation
  filler <- "Lorem ipsum dolor sit amet, ea debet error sensibus vix, at esse decore vivendo vim, rebum aliquip an cum? His ea agam novum dissentiet! At mel audire liberavisse, mundi audiam quaeque sea ne. In eam error habemus delectus, audiam ocurreret ne sit, sit ei salutandi liberavisse! Ut vix case corpora.

Posse malorum ponderum in qui, et eum dicam disputando, an vix quaestio scripserit. Falli veniam tamquam id mei. Modo sumo appetere cu mea, mutat possim rationibus ius id. Sed nominati antiopam cu, cu prima mandamus vim. Eos cu exerci consul!

Nam case atomorum suavitate cu? No quo inermis necessitatibus, eos ne essent scripta vivendum, ea euismod quaestio qui? Per minim tation accusamus eu, audire dolores nam an. Vel vocent inimicus ut, eu porro libris argumentum quo.

Vim no solet tempor, aperiam habemus assueverit ea usu: sea ut quodsi gloriatur! Eum te laudem aliquid inciderint, mollis prodesset mea ad. Dico definiebas efficiendi id usu. No bonorum suavitate adolescens per, ius oratio pericula ut, at mel porro vocibus scriptorem. Sea incorrupte definitiones necessitatibus in, cu ancillae conclusionemque duo. Ex vix dolore propriae principes, ius in augue ludus?

Solet copiosae ea sed, at assum  - dolore delenit has, ex aperiam honestatis mei. No legere nemore nonumes mel. Eu ullum accusata nam, an sea wisi rebum. Ei homero equidem sea! Sed erat augue eripuit et, ea vim altera eirmod labores, ad noster veritus nec.

Ut porro sententiae vis, debet affert eligendi id eam! In, nominati, pertinacia has, sea admodum dissentiunt eu! Volumus appellantur ex eos. Ei duo movet scripta aliquid, ea blandit explicari consectetuer eos.

Ne cibo ornatus vituperata pri. Soleat populo fierent ne sed, vel congue consequat temporibus in. Pro eu nostro inermis sadipscing, ne pri possim lobortis! Sea sonet nihil accusata no. Mei virtute noluisse pericula ex, aliquid mandamus inimicus quo ex.

Esse patrioque at qui, cum sanctus; consequuntur conclusionemque cu? Ut summo oportere  appellantur mel, ex per tale semper appellantur. Usu ea alia insolens sadipscing, eu aeterno persius vix. Agam prodesset interpretaris at ius, ne est malis signiferumque, illum soluta albucius mei an. Ex error tollit recusabo est, ut prompta consectetuer per. Dicam numquam eum id, brute mollis nam cu!

Ei vis discere interesset! Mutat 'option' qualisque ius te, sea deserunt lobortis voluptatum at. Qui et impedit accumsan atomorum, nam dicat possit ornatus an? Eu mei aperiri discere, sea veri homero ad, stet dolore putant mei in. Eu pri debet populo luptatum, eos te nominati concludaturque.

Tota veritus similique ne per, eam fastidii voluptatum eu. Sea tale mandamus suscipiantur ex. Ullum ullamcorper consequuntur et cum, aeque fuisset ut sea! Mea graecis pertinax explicari ne, pri tale hinc no? Eu vidisse nominati eum, et eam hendrerit voluptatum assueverit, qui ne munere recusabo democritum."

  filler_stats <- text_stats_fn_(filler)

  n_char_tot_stri_f <-  filler_stats$n_char_tot_stri
  n_char_tot_korp_f <- filler_stats$n_char_tot_korp

  n_words_stri_f <- filler_stats$n_words_stri
  n_words_korp_f <- filler_stats$n_words_korp

  n_sentences_korp_f <- filler_stats$n_sentences_korp

  expect_equal(n_char_tot_stri_f, 2896)
  expect_equal(n_char_tot_korp_f, 2897)
  expect_equal(n_words_stri_f, 450)
  expect_equal(n_words_korp_f, 450) # MS Word says 442
  expect_equal(n_sentences_korp_f, 52)
})



test_that("Word count is correct for rmd text", {
  # text with code chunks, etc.
  rmd_text <- "

---
title: 'Untitled'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<!-- this is an HTML comment -->

## Heading

This is an [R markdown](http://rmarkdown.rstudio.com/) document.

```{r cars}
summary(cars)
# Lines line this have caused problems -----------------------------------------
```

`r 2+2`

`r nrow(cars)`

##  Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

![this is the caption](/path/to/image.png)

"

  rmd_stats <- text_stats_fn_(rmd_text)

  n_char_tot_stri_r <-  rmd_stats$n_char_tot_stri
  n_char_tot_korp_r <- rmd_stats$n_char_tot_korp

  n_words_stri_r <- rmd_stats$n_words_stri
  n_words_korp_r <- rmd_stats$n_words_korp

  n_sentences_korp_r <- rmd_stats$n_sentences_korp

  expect_equal(n_char_tot_stri_r, 159)
  expect_equal(n_char_tot_korp_r, 159)
  expect_equal(n_words_stri_r, 20)
  expect_equal(n_words_korp_r, 20)
  expect_equal(n_sentences_korp_r, 4)
})

test_that("we can ignore <br> and </br>", {
  #  test for <br>
  string_with_br <- "Hi, I have <br> in the </br> string"

  string_with_br_stats <- text_stats_fn_(string_with_br)

  n_char_tot_stri_r <-  string_with_br_stats$n_char_tot_stri
  n_char_tot_korp_r <- string_with_br_stats$n_char_tot_korp

  n_words_stri_r <- string_with_br_stats$n_words_stri
  n_words_korp_r <- string_with_br_stats$n_words_korp

  n_sentences_korp_r <- string_with_br_stats$n_sentences_korp

  expect_equal(n_char_tot_stri_r, 26)
  expect_equal(n_char_tot_korp_r, 27)
  expect_equal(n_words_stri_r, 6)
  expect_equal(n_words_korp_r, 6)
  expect_equal(n_sentences_korp_r, 0)
})


test_that("Word count is correct for rmd file", {
  # test that we can word count on a file
  the_rmd_file_stats <- text_stats(filename = test_path("test_wordcountaddin.Rmd"))

  expect_equal(the_rmd_file_stats[3],
               "|Word count      |117         |116           |")
  expect_equal(the_rmd_file_stats[4],
               "|Character count |687         |687           |")
  expect_equal(the_rmd_file_stats[5],
               "|Sentence count  |12          |Not available |")
  expect_equal(the_rmd_file_stats[6],
               "|Reading time    |0.6 minutes |0.6 minutes   |")
})


test_that("Word count is correct for cmd line", {
  # command line fns
  text_on_the_command_line <- "here is some text"
  text_stats_chr_out <- text_stats_chr(text_on_the_command_line)

  expect_equal(text_stats_chr_out[3],
               "|Word count      |4         |4             |")
  expect_equal(text_stats_chr_out[4],
               "|Character count |18        |17            |")
  expect_equal(text_stats_chr_out[5],
               "|Sentence count  |0         |Not available |")
  expect_equal(text_stats_chr_out[6],
               "|Reading time    |0 minutes |0 minutes     |")
})


test_that("readability is correct for cmd line", {
  text_on_the_command_line <- "here is some text"
  expect_output(
    expect_warning(
      readability_chr_out <- readability_chr(text_on_the_command_line)
    )
  )
  expect_length(readability_chr_out, 26)
})

test_that("Word count is correct for text with % sign", {
  # test for escaping the percent sign in plain text
  text_with_percent_sign <- "Here is some % text with percent % signs in it."

  text_stats_percent_chr_out <- text_stats_chr(text_with_percent_sign)
  expect_equal(text_stats_percent_chr_out[3],
               "|Word count      |9         |9             |")
})


test_that("Word count is correct for text with figures included using LaTeX code", {
  # test for escaping the percent sign in plain text
  text_with_figures <- "One \\begin{figure} \\caption{text} \\label{text} \\includegraphics[width=\\textwidth]{figure.png} \\end{figure} Two \\begin{figure} \\caption{text} \\label{text} \\includegraphics[width=\\textwidth]{figure.png} \\end{figure} Three"

  text_stats_percent_chr_out <- text_stats_chr(text_with_figures)
  expect_equal(text_stats_percent_chr_out[3],
               "|Word count      |3         |3             |")
})


test_that("Word count is a single integer for a Rmd file when using word_count", {
  # test that we can word count on a file
  the_rmd_word_count <- word_count(filename = test_path("test_wordcountaddin.Rmd"))

  expect_equal(the_rmd_word_count,
               117L)
})

test_that("We can handle very long strings, like citation keys", {

  expect_output(
    expect_warning(
  # test that we can word count on a file
 long_string_read <- readability_chr("it's a long string right at the end here because a tiny
                                     fraction of the refreneces have crazy long keys. Why do they do
                                     that? It's autogenerated. Why does this give so many warnings when
                                     testing. It's a puzzle. [@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa].",
                                     quiet = TRUE)))

 expect_equal( attr(long_string_read, 'format'), "markdown")

})

test_that("don't count abbreviations as multiple words", {


  # test that we can word count on a file
  words_with_abbv <- "zero .o.n.e .t.wo."
  abbrev_count <- text_stats_chr(words_with_abbv)

 expect_equal( abbrev_count[3], "|Word count      |3         |3             |")

})




