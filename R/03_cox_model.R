# ==============================================================================
# Módulo do Modelo de Riscos Proporcionais de Cox (Cox Proportional Hazards)
# Pacote: CustomerChurnSurvival
# ==============================================================================

library(survival)
library(dplyr)

#' Ajusta o Modelo Semiparamétrico de Riscos Proporcionais de Cox
#'
#' Modela o risco instantâneo: h(t | X) = h0(t) * exp(beta * X)
#'
#' @param df Data frame contendo a variável Surv() e covariáveis.
#' @param formula_str Fórmula do modelo (default inclui contrato, tickets, pagamento, cobrança e suporte).
#' @return Objeto coxph contendo o modelo ajustado.
fit_multivariable_cox <- function(df, formula_str = NULL) {
  if (is.null(formula_str)) {
    formula_str <- "survival::Surv(tenure_months, churn) ~ contract_type + monthly_charges + support_tickets + payment_method + paperless_billing + partner"
  }
  cox_fit <- survival::coxph(stats::as.formula(formula_str), data = df)
  return(cox_fit)
}

#' Extrai Tabela Consolidada de Hazard Ratios (HR) com Intervalos de Confiança de 95%
#'
#' @param cox_fit Objeto coxph retornado por fit_multivariable_cox.
#' @return Data frame estruturado com Coeficientes, Hazard Ratios, Erros Padrão, Z, P-valor e Classificação.
extract_cox_summary <- function(cox_fit) {
  sum_cox <- summary(cox_fit)
  
  coef_matrix <- sum_cox$coefficients
  conf_matrix <- sum_cox$conf.int
  
  variables <- rownames(coef_matrix)
  beta_coef <- coef_matrix[, "coef"]
  hr_val <- conf_matrix[, "exp(coef)"]
  se_coef <- coef_matrix[, "se(coef)"]
  z_stat <- coef_matrix[, "z"]
  p_val <- coef_matrix[, "Pr(>|z|)"]
  ci_lower <- conf_matrix[, "lower .95"]
  ci_upper <- conf_matrix[, "upper .95"]
  
  # Classificação de Risco de Negócio
  classifications <- sapply(seq_along(variables), function(i) {
    if (p_val[i] >= 0.05) {
      return("Efeito Neutro (p >= 0.05)")
    } else if (hr_val[i] > 1.0) {
      return("Fator de Risco (Acelera Churn)")
    } else {
      return("Fator de Proteção (Aumenta Retenção)")
    }
  })
  
  # Interpretação em Linguagem Natural
  interpretations <- sapply(seq_along(variables), function(i) {
    pct_change <- round(abs(hr_val[i] - 1.0) * 100, 1)
    if (p_val[i] >= 0.05) {
      return("Impacto estatisticamente não significante no risco de cancelamento.")
    } else if (hr_val[i] > 1.0) {
      return(paste0("Aumenta o risco de cancelamento em +", pct_change, "% (HR = ", round(hr_val[i], 2), ")."))
    } else {
      return(paste0("Reduz o risco de cancelamento em -", pct_change, "% (HR = ", round(hr_val[i], 2), ")."))
    }
  })
  
  df_summary <- data.frame(
    variable = variables,
    coef = round(beta_coef, 4),
    hazard_ratio = round(hr_val, 4),
    se_coef = round(se_coef, 4),
    z_stat = round(z_stat, 2),
    p_value = round(p_val, 5),
    ci_lower_95 = round(ci_lower, 4),
    ci_upper_95 = round(ci_upper, 4),
    classification = classifications,
    interpretation = interpretations,
    stringsAsFactors = FALSE
  )
  rownames(df_summary) <- NULL
  return(df_summary)
}

#' Testa a Suposição de Riscos Proporcionais via Resíduos de Schoenfeld
#'
#' @param cox_fit Objeto coxph.
#' @return Lista com matriz zph de testes por covariável e teste global.
test_schoenfeld_residuals <- function(cox_fit) {
  zph_res <- survival::cox.zph(cox_fit)
  
  table_res <- as.data.frame(zph_res$table)
  table_res$variable <- rownames(table_res)
  rownames(table_res) <- NULL
  
  global_p <- table_res$p[table_res$variable == "GLOBAL"]
  assumption_met <- if (length(global_p) > 0) (global_p >= 0.05) else TRUE
  
  return(list(
    zph_object = zph_res,
    diagnostic_table = table_res,
    global_p_value = global_p,
    assumption_met = assumption_met,
    conclusion = if (assumption_met) {
      "Premissa de Riscos Proporcionais satisfeita (p global >= 0.05)."
    } else {
      "Indício de violação de riscos proporcionais para uma ou mais covariáveis (p global < 0.05)."
    }
  ))
}