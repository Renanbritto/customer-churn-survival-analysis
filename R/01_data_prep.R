# ==============================================================================
# Módulo de Preparação de Dados e Diagnóstico de Censura em Análise de Sobrevivência
# Pacote: CustomerChurnSurvival
# ==============================================================================

library(dplyr)
library(readr)
library(survival)

#' Carrega o conjunto de dados bruto de churn de clientes
#'
#' @param filepath Caminho relativo ou absoluto para o arquivo CSV.
#' @return Um data.frame / tibble com os dados brutos.
load_survival_data <- function(filepath = "data/raw/saas_customer_churn_survival.csv") {
  if (!file.exists(filepath)) {
    stop(paste("Arquivo não encontrado no caminho:", filepath))
  }
  df <- readr::read_csv(filepath, show_col_types = FALSE)
  return(df)
}

#' Prepara e tipifica variáveis para modelagem de sobrevivência
#'
#' Converte variáveis categóricas em fatores com níveis de referência bem definidos,
#' valida a não-negatividade do tempo (tenure) e a binaridade do evento (churn).
#'
#' @param df Data frame retornado por load_survival_data.
#' @return Data frame tratado com fatores ordenados e prontos para Surv().
prepare_survival_data <- function(df) {
  required_cols <- c("customer_id", "tenure_months", "churn", "contract_type",
                     "monthly_charges", "support_tickets", "payment_method")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Colunas obrigatórias ausentes:", paste(missing_cols, collapse = ", ")))
  }

  # Validações de integridade
  if (any(df$tenure_months <= 0)) {
    stop("O tempo de permanência (tenure_months) deve ser estritamente positivo (> 0).")
  }
  if (!all(df$churn %in% c(0, 1))) {
    stop("O indicador de evento (churn) deve conter apenas valores 0 (censurado) ou 1 (evento).")
  }

  df_clean <- df %>%
    dplyr::mutate(
      contract_type = factor(contract_type, levels = c("Month-to-month", "One year", "Two year")),
      payment_method = factor(payment_method, levels = c("Electronic check", "Credit card (automatic)", "Bank transfer (automatic)")),
      paperless_billing = factor(paperless_billing, levels = c("No", "Yes")),
      partner = factor(partner, levels = c("No", "Yes")),
      dependents = factor(dependents, levels = c("No", "Yes")),
      internet_service = factor(internet_service, levels = c("DSL", "Fiber optic", "No")),
      # Criação do objeto de sobrevivência canônico Surv(tempo, evento)
      surv_object = survival::Surv(time = tenure_months, event = churn)
    )

  return(df_clean)
}

#' Gera sumário executivo sobre a taxa de censura à direita e distribuição do tempo
#'
#' @param df Data frame preparado contendo tenure_months e churn.
#' @return Lista contendo estatísticas descritivas e indicadores de censura.
get_censoring_summary <- function(df) {
  total_customers <- nrow(df)
  events_count <- sum(df$churn == 1)
  censored_count <- sum(df$churn == 0)
  censoring_rate <- (censored_count / total_customers) * 100
  event_rate <- (events_count / total_customers) * 100

  median_tenure <- stats::median(df$tenure_months)
  iqr_tenure <- stats::IQR(df$tenure_months)
  mean_tenure <- mean(df$tenure_months)

  # Breakdown por tipo de contrato
  contract_breakdown <- df %>%
    dplyr::group_by(contract_type) %>%
    dplyr::summarise(
      n = dplyr::n(),
      events = sum(churn == 1),
      censored = sum(churn == 0),
      event_rate_pct = round((sum(churn == 1) / dplyr::n()) * 100, 2),
      median_tenure = stats::median(tenure_months),
      .groups = "drop"
    )

  summary_res <- list(
    total_customers = total_customers,
    events_count = events_count,
    censored_count = censored_count,
    censoring_rate_pct = round(censoring_rate, 2),
    event_rate_pct = round(event_rate, 2),
    median_tenure_months = median_tenure,
    iqr_tenure_months = iqr_tenure,
    mean_tenure_months = round(mean_tenure, 2),
    contract_breakdown = contract_breakdown
  )

  return(summary_res)
}