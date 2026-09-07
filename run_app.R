# Script de Inicialização do Dashboard R Shiny
required_packages <- c("shiny", "bslib", "survival", "survminer", "dplyr", "ggplot2", "plotly", "DT", "readr")
missing <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing) > 0) {
  message(paste("Instalando dependências necessárias:", paste(missing, collapse = ", ")))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

message("Iniciando o Dashboard Customer Churn Survival Analysis no navegador...")
shiny::runApp("app", launch.browser = TRUE)