# ==============================================================================
# Módulo de Predição Individual de Sobrevivência e Cálculo de LTV Residual
# Pacote: CustomerChurnSurvival
# ==============================================================================

library(survival)
library(dplyr)

#' Prediz a Curva de Sobrevivência para um Perfil Específico de Cliente
#'
#' Calcula S(t | X_novo) para um horizonte temporal contínuo.
#'
#' @param cox_fit Objeto coxph ajustado.
#' @param new_customer Data frame de 1 linha com as características do cliente simulado.
#' @param max_months Horizonte máximo em meses para projeção (default: 60).
#' @return Data frame contendo month, survival_prob, churn_prob_cum, lower_95, upper_95.
predict_individual_survival <- function(cox_fit, new_customer, max_months = 60) {
  surv_pred <- survival::survfit(cox_fit, newdata = new_customer)
  
  # Interpolação para cada mês de 1 até max_months
  eval_times <- seq(1, max_months, by = 1)
  summary_pred <- summary(surv_pred, times = eval_times, extend = TRUE)
  
  df_pred <- data.frame(
    month = summary_pred$time,
    survival_prob = round(summary_pred$surv, 4),
    churn_prob_cum = round(1 - summary_pred$surv, 4),
    lower_95 = round(summary_pred$lower, 4),
    upper_95 = round(summary_pred$upper, 4)
  )
  return(df_pred)
}

#' Calcula a Expectativa de Vida Residual (Restricted Mean Survival Time - RMST)
#'
#' RMST(tau) = Integral_0^tau S(t) dt
#'
#' @param surv_df Data frame retornado por predict_individual_survival.
#' @return Número com o tempo médio de vida esperado em meses.
calculate_expected_residual_lifetime <- function(surv_df) {
  # Regra dos trapézios para integração numérica
  times <- c(0, surv_df$month)
  probs <- c(1.0, surv_df$survival_prob)
  
  dt <- diff(times)
  avg_p <- (probs[-length(probs)] + probs[-1]) / 2
  rmst <- sum(avg_p * dt)
  
  return(round(rmst, 2))
}

#' Calcula o Valor Vitalício Residual Esperado (Residual LTV)
#'
#' LTV = Somatório [ Mensalidade * S(t) / (1 + r)^t ]
#'
#' @param surv_df Data frame com month e survival_prob.
#' @param monthly_charges Valor da mensalidade (MRR) em R$.
#' @param discount_rate Taxa mensal de desconto intertemporal (default: 0.008 = ~10% a.a.).
#' @return Lista contendo o LTV nominal, LTV descontado e média mensal esperada.
calculate_residual_ltv <- function(surv_df, monthly_charges, discount_rate = 0.008) {
  if (monthly_charges <= 0) {
    stop("A mensalidade deve ser positiva (> 0).")
  }
  
  # Fluxo de caixa esperado mês a mês: E[Cashflow_t] = monthly_charges * S(t)
  surv_df <- surv_df %>%
    dplyr::mutate(
      expected_cashflow = monthly_charges * survival_prob,
      discount_factor = 1 / ((1 + discount_rate) ^ month),
      discounted_cashflow = expected_cashflow * discount_factor
    )
  
  nominal_ltv <- sum(surv_df$expected_cashflow)
  discounted_ltv <- sum(surv_df$discounted_cashflow)
  expected_months <- calculate_expected_residual_lifetime(surv_df)
  
  return(list(
    monthly_charges = monthly_charges,
    discount_rate_monthly = discount_rate,
    expected_lifetime_months = expected_months,
    nominal_ltv = round(nominal_ltv, 2),
    discounted_ltv = round(discounted_ltv, 2),
    cashflow_schedule = surv_df
  ))
}