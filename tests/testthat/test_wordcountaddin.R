library(wordcountaddin)
context("Word count")

# short sentence
eleven_words <- "here are exactly eleven words of fairly boring and unpunctuated text"

short_stats <-  wordcountaddin:::text_stats_fn(eleven_words)
# qdap cannot manage without final punct.
n_words_stri_11 <-short_stats$n_words_stri
n_words_korp_11 <- short_stats$n_words_korp

n_char_tot_stri_11 <-  short_stats$n_char_tot_stri
n_char_tot_korp_11 <- short_stats$n_char_tot_korp

test_that("Word count is correct for short simple sentence", {
  expect_equal(n_words_stri_11, 11)
  expect_equal(n_words_korp_11, 11)
  expect_equal(n_char_tot_stri_11, 68)
  expect_equal(n_char_tot_korp_11, 69)
})

# Moderate: Harvard sentences, https://en.wikipedia.org/wiki/Harvard_sentences
moderately_complex <- "The birch canoe slid on the smooth planks. Glue the sheet to the dark blue background. It's easy to tell the depth of a well. These days a chicken leg is a rare dish. Rice is often served in round bowls. The juice of lemons makes fine punch. The box was thrown beside the parked truck. The hogs were fed chopped corn and garbage. Four hours of steady work faced us. Large size in stockings is hard to sell."

moderately_complex_stats <- wordcountaddin:::text_stats_fn(moderately_complex)

n_char_tot_stri_mc <-  moderately_complex_stats$n_char_tot_stri
n_char_tot_korp_mc <- moderately_complex_stats$n_char_tot_korp

n_words_qdap_mc <- moderately_complex_stats$n_words_qdap
n_words_stri_mc <- moderately_complex_stats$n_words_stri
n_words_korp_mc <- moderately_complex_stats$n_words_korp

n_sentences_qdap_mc <- moderately_complex_stats$n_sentences_qdap
n_sentences_korp_mc <- moderately_complex_stats$n_sentences_korp

test_that("Word count is correct for moderately complex sentences", {
  expect_equal(n_char_tot_stri_mc, 406)
  expect_equal(n_char_tot_korp_mc, 407)
  expect_equal(n_words_qdap_mc, 80)
  expect_equal(n_words_stri_mc, 80)
  expect_equal(n_words_korp_mc, 80)
  expect_equal(n_sentences_qdap_mc, 10)
  expect_equal(n_sentences_korp_mc, 10)
})

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

filler_stats <- wordcountaddin:::text_stats_fn(filler)

n_char_tot_stri_f <-  filler_stats$n_char_tot_stri
n_char_tot_korp_f <- filler_stats$n_char_tot_korp

n_words_qdap_f <- filler_stats$n_words_qdap
n_words_stri_f <- filler_stats$n_words_stri
n_words_korp_f <- filler_stats$n_words_korp

n_sentences_qdap_f <- filler_stats$n_sentences_qdap
n_sentences_korp_f <- filler_stats$n_sentences_korp

test_that("Word count is correct for complex sentences in filler text", {
  expect_equal(n_char_tot_stri_f, 2896)
  expect_equal(n_char_tot_korp_f, 2897)
  expect_equal(n_words_qdap_f, 451)
  expect_equal(n_words_stri_f, 450)
  expect_equal(n_words_korp_f, 450)
  expect_equal(n_sentences_qdap_f, 50)
  expect_equal(n_sentences_korp_f, 52)
})

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

rmd_stats <- wordcountaddin:::text_stats_fn(rmd_text)

n_char_tot_stri_r <-  rmd_stats$n_char_tot_stri
n_char_tot_korp_r <- rmd_stats$n_char_tot_korp

n_words_qdap_r <- rmd_stats$n_words_qdap
n_words_stri_r <- rmd_stats$n_words_stri
n_words_korp_r <- rmd_stats$n_words_korp

n_sentences_qdap_r <- rmd_stats$n_sentences_qdap
n_sentences_korp_r <- rmd_stats$n_sentences_korp

test_that("Word count is correct for rmd text", {
  expect_equal(n_char_tot_stri_r, 115)
  expect_equal(n_char_tot_korp_r, 116)
  expect_equal(n_words_qdap_r, 5)
  expect_equal(n_words_stri_r, 15)
  expect_equal(n_words_korp_r, 15)
  expect_equal(n_sentences_qdap_r, 1)
  expect_equal(n_sentences_korp_r, 2)
})
