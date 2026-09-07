# Script de Execução da Suíte de Testes Unitários
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

installed <- installed.packages(lib.loc = .libPaths())[, "Package"]
if (!"testthat" %in% installed) {
  install.packages("testthat", lib = user_lib, repos = "https://cloud.r-project.org")
}

library(testthat)

message("Carregando módulos da pasta R/...")
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f)
}

message("Executando suíte testthat...")
test_dir("tests/testthat")