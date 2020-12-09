library(tidyverse)
library(collostructions)
library(readxl)
library(data.table)

# read data ---------------------------------------------------------------
d <- read_xlsx("mother_of_all_ENCOW.xlsx")


# read DECOW frequency list -----------------------------------------------
# (available from https://www.webcorpora.org/opendata/frequencies/english/encow16a/)
encow <- fread("/Volumes/My Passport/ENCOW word lists/encow16ax.lp.tsv",
               header = F)

# only nouns
encow <- encow[V2 %in% c("NN", "NE")]
colnames(encow) <- c("Lemma", "POS", "Freq")

# if something is attested both as NN and
# NE, sum them up
encow <- encow[, sum(Freq), by = Lemma]
setnames(encow, old = "V1", new = "Freq")

# omit false hits ---------------------------------------------------------
d <- filter(d, keep == "y")

# get frequencies
d_tbl <- d %>% select(lemma) %>% table %>% sort(decreasing = T) %>% as.data.frame(stringsAsFactors = F)
colnames(d_tbl) <- c("Lemma", "Freq_in_cxn")

# join tables
d_tbl <- left_join(d_tbl, encow, by = "Lemma")

# omit one case where corpus frequency is
# smaller than cxn frequency
d_tbl <- subset(d_tbl, d_tbl[,2] <= d_tbl[,3])


# perform collexeme analysis
collex(d_tbl, corpsize = sum(encow$Freq)) %>% write_excel_csv("simple_collexeme_analysis.csv")
