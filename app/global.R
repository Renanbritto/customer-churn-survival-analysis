# ==============================================================================
# R Shiny Application - Global Setup & Model Preloading
# Customer Churn Survival Analysis & LTV Modeling
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(survival)
  library(survminer)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(readr)
})

# Carrega módulos de R/
r_scripts <- list.files("../R", pattern = "\\.R$", full.names = TRUE)
if (length(r_scripts) == 0) {
  # Caso executado na raiz do repositório
  r_scripts <- list.files("R", pattern = "\\.R$", full.names = TRUE)
}
for (s in r_scripts) {
  source(s)
}

# Caminho para os dados
data_path <- if (file.exists("data/raw/saas_customer_churn_survival.csv")) {
  "data/raw/saas_customer_churn_survival.csv"
} else if (file.exists("../data/raw/saas_customer_churn_survival.csv")) {
  "../data/raw/saas_customer_churn_survival.csv"
} else {
  "data/raw/saas_customer_churn_survival.csv"
}

# Carrega e prepara os dados
df_raw <- load_survival_data(data_path)
df_clean <- prepare_survival_data(df_raw)
censoring_stats <- get_censoring_summary(df_clean)

# Ajustes pré-carregados
km_global_fit <- fit_global_km(df_clean)
km_contract_fit <- fit_stratified_km(df_clean, "contract_type")
km_payment_fit <- fit_stratified_km(df_clean, "payment_method")

logrank_contract <- run_log_rank_test(df_clean, "contract_type")

cox_model_fit <- fit_multivariable_cox(df_clean)
cox_summary_df <- extract_cox_summary(cox_model_fit)
schoenfeld_diag <- test_schoenfeld_residuals(cox_model_fit)