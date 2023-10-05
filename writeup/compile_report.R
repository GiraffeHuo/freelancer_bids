#!/usr/bin/env Rscript

source('renv/activate.R') # activate renv, so that all the libraries below are loaded from the local library
invisible(capture.output(renv::restore())) # install the packages the way they should be installed (this step is skipped if it is excessive)

library(here)

# report options
knitr::opts_chunk$set(eval=T, echo=F, warning=F, message=F, cache=F)

# compile the report
rmarkdown::render(here("writeup", "report.Rmd"),
                  output_dir = here("writeup"),
                  knit_root_dir = here())
