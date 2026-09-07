test_that("fit_global_km produz probabilidades de sobrevivência monotonicamente não-crescentes", {
  df_sample <- data.frame(
    tenure_months = c(2, 5, 8, 12, 15, 20, 24),
    churn = c(1, 1, 0, 1, 0, 1, 0)
  )

  km_fit <- fit_global_km(df_sample)
  km_table <- extract_km_table(km_fit)

  expect_true(nrow(km_table) > 0)
  # Verifica que as probabilidades nunca aumentam ao longo do tempo (S(t1) >= S(t2))
  diffs <- diff(km_table$survival_prob)
  expect_true(all(diffs <= 0.0001))
  # Probabilidades devem estar no intervalo [0, 1]
  expect_true(all(km_table$survival_prob >= 0 & km_table$survival_prob <= 1))
})

test_that("get_survival_milestones retorna estimativas nos horizontes temporais solicitados", {
  df_sample <- data.frame(
    tenure_months = sample(1:36, 100, replace = TRUE),
    churn = sample(c(0, 1), 100, replace = TRUE)
  )

  km_fit <- fit_global_km(df_sample)
  milestones <- get_survival_milestones(km_fit, times = c(6, 12, 24))

  expect_equal(milestones$milestone_months, c(6, 12, 24))
  expect_true(all(milestones$retention_prob + milestones$churn_prob_cum == 1.0))
})

test_that("run_log_rank_test identifica diferenças entre grupos heterogêneos", {
  # Grupo A: churn rápido; Grupo B: alta retenção
  df_test <- data.frame(
    tenure_months = c(1, 2, 2, 3, 3, 4, 20, 24, 30, 32, 36, 40),
    churn = c(1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0),
    contract_type = factor(c(rep("Month-to-month", 6), rep("Two year", 6)))
  )

  res_logrank <- run_log_rank_test(df_test, strata_col = "contract_type")

  expect_true(res_logrank$is_significant)
  expect_true(res_logrank$p_value < 0.05)
  expect_equal(res_logrank$df, 1)
})