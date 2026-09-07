library(testthat)
library(survival)
library(dplyr)
library(readr)

# Carrega módulos em R/
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f)
}

test_check("CustomerChurnSurvival")