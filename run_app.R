# Script de Inicialização do Dashboard R Shiny
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

required_packages <- c("shiny", "bslib", "survival", "survminer", "dplyr", "ggplot2", "plotly", "DT", "readr")
installed <- installed.packages(lib.loc = .libPaths())[, "Package"]
missing <- required_packages[!(required_packages %in% installed)]

if (length(missing) > 0) {
  message(paste("Instalando dependências na biblioteca do usuário:", user_lib))
  message(paste("Pacotes:", paste(missing, collapse = ", ")))
  install.packages(missing, lib = user_lib, repos = "https://cloud.r-project.org")
}

message("Iniciando o Dashboard Customer Churn Survival Analysis no navegador...")
shiny::runApp("app", launch.browser = TRUE)