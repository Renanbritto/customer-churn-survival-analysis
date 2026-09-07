test_that("prepare_survival_data valida e formata colunas corretamente", {
  df_sample <- data.frame(
    customer_id = c("C1", "C2", "C3", "C4"),
    tenure_months = c(12, 24, 6, 36),
    churn = c(1, 0, 1, 0),
    contract_type = c("Month-to-month", "One year", "Two year", "Month-to-month"),
    monthly_charges = c(99.0, 149.0, 79.0, 199.0),
    support_tickets = c(2, 0, 4, 1),
    payment_method = c("Electronic check", "Credit card (automatic)", "Bank transfer (automatic)", "Electronic check"),
    paperless_billing = c("Yes", "No", "Yes", "No"),
    partner = c("No", "Yes", "No", "Yes"),
    dependents = c("No", "No", "Yes", "Yes"),
    internet_service = c("Fiber optic", "DSL", "No", "Fiber optic")
  )

  res <- prepare_survival_data(df_sample)

  expect_s3_class(res$contract_type, "factor")
  expect_equal(levels(res$contract_type)[1], "Month-to-month")
  expect_true("surv_object" %in% names(res))
  expect_s4_class(res$surv_object, "Surv")
})

test_that("prepare_survival_data rejeita dados inconsistentes", {
  df_invalid_tenure <- data.frame(
    customer_id = "C1", tenure_months = -5, churn = 1, contract_type = "Month-to-month",
    monthly_charges = 100, support_tickets = 1, payment_method = "Electronic check"
  )
  expect_error(prepare_survival_data(df_invalid_tenure))

  df_invalid_churn <- data.frame(
    customer_id = "C1", tenure_months = 10, churn = 3, contract_type = "Month-to-month",
    monthly_charges = 100, support_tickets = 1, payment_method = "Electronic check"
  )
  expect_error(prepare_survival_data(df_invalid_churn))
})

test_that("get_censoring_summary calcula percentuais complementares corretamente", {
  df_sample <- data.frame(
    customer_id = c("C1", "C2", "C3", "C4"),
    tenure_months = c(10, 20, 30, 40),
    churn = c(1, 0, 1, 0),
    contract_type = factor(c("Month-to-month", "One year", "Two year", "Month-to-month"))
  )

  sum_res <- get_censoring_summary(df_sample)

  expect_equal(sum_res$total_customers, 4)
  expect_equal(sum_res$events_count, 2)
  expect_equal(sum_res$censored_count, 2)
  expect_equal(sum_res$censoring_rate_pct + sum_res$event_rate_pct, 100.0)
})