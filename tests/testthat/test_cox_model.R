test_that("fit_multivariable_cox e extract_cox_summary produzem Hazard Ratios consistentes", {
  set.seed(123)
  n <- 200
  df_sim <- data.frame(
    tenure_months = sample(1:48, n, replace = TRUE),
    churn = sample(c(0, 1), n, replace = TRUE),
    contract_type = factor(sample(c("Month-to-month", "One year", "Two year"), n, replace = TRUE)),
    monthly_charges = rnorm(n, mean = 100, sd = 25),
    support_tickets = rpois(n, lambda = 1.5),
    payment_method = factor(sample(c("Electronic check", "Credit card (automatic)", "Bank transfer (automatic)"), n, replace = TRUE)),
    paperless_billing = factor(sample(c("No", "Yes"), n, replace = TRUE)),
    partner = factor(sample(c("No", "Yes"), n, replace = TRUE))
  )

  cox_fit <- fit_multivariable_cox(df_sim)
  expect_s3_class(cox_fit, "coxph")

  cox_sum <- extract_cox_summary(cox_fit)
  expect_true(nrow(cox_sum) > 0)
  expect_true(all(cox_sum$hazard_ratio > 0))
  expect_true(all(c("hazard_ratio", "ci_lower_95", "ci_upper_95", "classification") %in% names(cox_sum)))
})

test_that("calculate_residual_ltv calcula valores proporcionais à mensalidade", {
  mock_surv <- data.frame(
    month = 1:24,
    survival_prob = seq(0.95, 0.50, length.out = 24)
  )

  ltv_100 <- calculate_residual_ltv(mock_surv, monthly_charges = 100.0)
  ltv_200 <- calculate_residual_ltv(mock_surv, monthly_charges = 200.0)

  # Dobrar a mensalidade deve dobrar exatamente o LTV projetado
  expect_equal(ltv_200$nominal_ltv, 2 * ltv_100$nominal_ltv)
  expect_equal(ltv_200$discounted_ltv, 2 * ltv_100$discounted_ltv)
  expect_true(ltv_100$discounted_ltv < ltv_100$nominal_ltv)  # Desconto intertemporal
})

test_that("calculate_residual_ltv rejeita mensalidades não positivas", {
  mock_surv <- data.frame(month = 1:12, survival_prob = seq(0.9, 0.6, length.out = 12))
  expect_error(calculate_residual_ltv(mock_surv, monthly_charges = -50))
})