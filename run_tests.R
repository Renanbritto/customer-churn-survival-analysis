# Script de Execução da Suíte de Testes Unitários
if (!"testthat" %in% installed.packages()[, "Package"]) {
  install.packages("testthat", repos = "https://cloud.r-project.org")
}

library(testthat)

message("Carregando módulos da pasta R/...")
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f)
}

message("Executando suíte testthat...")
test_dir("tests/testthat")