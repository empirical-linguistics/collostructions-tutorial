# install CRAN packages (if not yet installed)
sapply(c("bookdown", "kableExtra", "tufte", "knitr"), function(x) 
  if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

# compile HTML book
bookdown::render_book("index.Rmd", "bookdown::pdf_book")