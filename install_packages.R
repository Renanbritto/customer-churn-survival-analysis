# Script de Instalação Automática das Dependências
required_packages <- c(
  "shiny", "bslib", "survival", "survminer", 
  "dplyr", "ggplot2", "plotly", "DT", "readr", "testthat"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  message(paste("Instalando pacotes faltantes:", paste(missing_packages, collapse = ", ")))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("Todos os pacotes já estão instalados com sucesso!")
}