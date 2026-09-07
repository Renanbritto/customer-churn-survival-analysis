# Script de Instalação Automática com Biblioteca Pessoal (Sem Admin)
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

required_packages <- c(
  "shiny", "bslib", "survival", "survminer", 
  "dplyr", "ggplot2", "plotly", "DT", "readr", "testthat"
)

installed <- installed.packages(lib.loc = .libPaths())[, "Package"]
missing_packages <- required_packages[!(required_packages %in% installed)]

if (length(missing_packages) > 0) {
  message(paste("Instalando pacotes na pasta do usuário:", user_lib))
  message(paste("Pacotes:", paste(missing_packages, collapse = ", ")))
  install.packages(missing_packages, lib = user_lib, repos = "https://cloud.r-project.org")
} else {
  message("Todos os pacotes já estão instalados com sucesso!")
}