

# packages ----------------------------------------------------------------


library(tidyverse)
library(collostructions) # available at sflach.ch


# helper functions --------------------------------------------------------

# search for entire words
grepw <- function(pattern, x, perl = F, ...) {
  grep(paste("^", pattern, "$", sep="", collapse=""), x, perl = perl, ...)
}


# read data ---------------------------------------------------------------

d <- read_csv("ENCOW_x_is_the_new_y_without_false_hits.csv")



# exclude false hits ------------------------------------------------------

d <- filter(d, keep == "y")


# add wordcount for x and y lemmas ----------------------------------------

d$wordcount_x <- sapply(1:nrow(d), function(i) wordcount(trimws(d$Lemma_x[i])))
d$wordcount_y <- sapply(1:nrow(d), function(i) wordcount(trimws(d$Lemma_y[i])))



# get heads of compounds and phrases --------------------------------------

# find instances in which there are words
# written entirely in uppercase (= our way of
# marking heads in the data, unless in the case of
# right-hand heads)

# empty columns for heads
d$head_x <- character(nrow(d)); d$head_y <- character(nrow(d))

# replace NAs by empty string
d <- replace_na(d, list(pos_x = "", pos_y = ""))

# get heads
for(i in 1:nrow(d)) {
  
  if(d$wordcount_x[i]>1) {
    if(d$pos_x[i]!="NE" & grepl("[A-Z]{2,}", d$Lemma_x[i])) {
      d$head_x[i] <- tolower(unlist(strsplit(d$Lemma_x[i], " "))[grepl("[A-Z]{2,}", unlist(strsplit(d$Lemma_x[i], " ")))][1])
    } else{
      temp <- unlist(strsplit(d$Lemma_x[i], " "))
      d$head_x[i] <- tolower(temp[length(temp)])
    }
  } else {
    d$head_x[i] <- tolower(d$Lemma_x[i])
  }
  
  
  if(d$wordcount_y[i]>1) {
    if(d$pos_y[i]!="NE" & grepl("[A-Z]{2,}", d$Lemma_y[i])) {
      d$head_y[i] <- tolower(unlist(strsplit(d$Lemma_y[i], " "))[grepl("[A-Z]{2,}", unlist(strsplit(d$Lemma_y[i], " ")))][1])
    } else{
      temp <- unlist(strsplit(d$Lemma_y[i], " "))
      d$head_y[i] <- tolower(temp[length(temp)])
    }
  } else {
    d$head_y[i] <- tolower(d$Lemma_y[i])
  }
  
  
}


# covarying collexeme analysis --------------------------------------------

# d %>% select(head_x, head_y) %>% as.data.frame %>% collex.covar %>% pretty_df() %>% write_excel_csv("collex_covar_x_is_the_new_y_ENCOW.csv")
d_covar <- d %>% select(head_x, head_y) %>% as.data.frame %>% collex.covar 
d_covar %>% head(10)

