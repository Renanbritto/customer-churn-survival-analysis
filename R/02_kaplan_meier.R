# ==============================================================================
# Módulo de Estimação de Kaplan-Meier e Testes de Hipótese Log-Rank
# Pacote: CustomerChurnSurvival
# ==============================================================================

library(survival)
library(dplyr)
library(ggplot2)

#' Ajusta o Estimador Global de Kaplan-Meier
#'
#' Calcula a curva S(t) = P(T > t) para toda a base de clientes.
#'
#' @param df Data frame contendo tenure_months e churn.
#' @return Objeto survfit com o modelo global.
fit_global_km <- function(df) {
  km_fit <- survival::survfit(survival::Surv(tenure_months, churn) ~ 1, data = df)
  return(km_fit)
}

#' Ajusta o Estimador de Kaplan-Meier Estratificado por uma Covariável
#'
#' Permite comparar curvas de sobrevida entre grupos (ex: Contrato Mensal vs Anual).
#'
#' @param df Data frame contendo tenure_months, churn e a coluna de estratificação.
#' @param strata_col Nome da coluna categórica de estratificação (ex: "contract_type").
#' @return Objeto survfit estratificado.
fit_stratified_km <- function(df, strata_col = "contract_type") {
  if (!strata_col %in% names(df)) {
    stop(paste("Coluna de estratificação não encontrada:", strata_col))
  }
  formula_str <- paste("survival::Surv(tenure_months, churn) ~", strata_col)
  km_fit <- survival::survfit(stats::as.formula(formula_str), data = df)
  return(km_fit)
}

#' Extrai Tabela Tidy do Ajuste de Kaplan-Meier
#'
#' @param km_fit Objeto survfit retornado por fit_global_km ou fit_stratified_km.
#' @return Data frame estruturado com tempo, indivíduos em risco, eventos, censuras e S(t).
extract_km_table <- function(km_fit) {
  sum_fit <- summary(km_fit)
  
  strata_vals <- if (!is.null(sum_fit$strata)) as.character(sum_fit$strata) else "Global"
  
  df_res <- data.frame(
    time = sum_fit$time,
    n_risk = sum_fit$n.risk,
    n_event = sum_fit$n.event,
    n_censor = sum_fit$n.censor,
    survival_prob = round(sum_fit$surv, 4),
    std_err = round(sum_fit$std.err, 4),
    lower_95 = round(sum_fit$lower, 4),
    upper_95 = round(sum_fit$upper, 4),
    strata = strata_vals,
    stringsAsFactors = FALSE
  )
  return(df_res)
}

#' Calcula Probabilidades de Sobrevivência em Milestones Fixos (ex: 6, 12, 24 meses)
#'
#' @param km_fit Objeto survfit.
#' @param times Vetor numérico com os tempos de interesse (em meses).
#' @return Data frame com as probabilidades pontuais e intervalos de 95%.
get_survival_milestones <- function(km_fit, times = c(6, 12, 24, 36, 48, 60)) {
  sum_milestones <- summary(km_fit, times = times, extend = TRUE)
  
  strata_vals <- if (!is.null(sum_milestones$strata)) as.character(sum_milestones$strata) else "Global"
  
  df_milestones <- data.frame(
    milestone_months = sum_milestones$time,
    retention_prob = round(sum_milestones$surv, 4),
    churn_prob_cum = round(1 - sum_milestones$surv, 4),
    lower_95 = round(sum_milestones$lower, 4),
    upper_95 = round(sum_milestones$upper, 4),
    strata = strata_vals,
    stringsAsFactors = FALSE
  )
  return(df_milestones)
}

#' Executa o Teste de Log-Rank para Comparação de Curvas de Sobrevivência
#'
#' Testa a hipótese nula H0 de que as funções de sobrevivência de todos os estratos
#' são estatisticamente idênticas.
#'
#' @param df Data frame de clientes.
#' @param strata_col Nome da variável de comparação.
#' @return Lista com estatística Qui-quadrado, graus de liberdade, p-valor e interpretação.
run_log_rank_test <- function(df, strata_col = "contract_type") {
  formula_str <- paste("survival::Surv(tenure_months, churn) ~", strata_col)
  diff_res <- survival::survdiff(stats::as.formula(formula_str), data = df)
  
  chisq_stat <- diff_res$chisq
  df_degrees <- length(diff_res$n) - 1
  p_value <- stats::pchisq(chisq_stat, df = df_degrees, lower.tail = FALSE)
  
  interpretation <- if (p_value < 0.001) {
    "Diferença altamente significante entre os grupos (p < 0.001). As curvas de retenção divergem substancialmente."
  } else if (p_value < 0.05) {
    "Diferença estatisticamente significante entre os grupos (p < 0.05)."
  } else {
    "Não há evidência estatística de diferença entre as curvas de sobrevivência (p >= 0.05)."
  }
  
  return(list(
    strata = strata_col,
    chisq = round(chisq_stat, 2),
    df = df_degrees,
    p_value = p_value,
    is_significant = (p_value < 0.05),
    interpretation = interpretation,
    n_groups = diff_res$n,
    obs_events = diff_res$obs,
    exp_events = round(diff_res$exp, 2)
  ))
}