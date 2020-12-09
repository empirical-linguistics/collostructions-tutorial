library(tidyverse)
library(collostructions)

d <- read_delim("thanks.txt", delim = "\t", quote = "")
d <- as.data.frame(d)

head(d, 50)
