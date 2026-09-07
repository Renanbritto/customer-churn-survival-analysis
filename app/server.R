# ==============================================================================
# R Shiny Application - Server Logic
# Customer Churn Survival Analysis & LTV Modeling
# ==============================================================================

server <- function(input, output, session) {
  
  # --- KPIs da Aba 1 (Kaplan-Meier) ---
  output$kpi_total_customers <- renderText({
    format(censoring_stats$total_customers, big.mark = ".")
  })
  
  output$kpi_churn_count <- renderText({
    paste0(format(censoring_stats$events_count, big.mark = "."), 
           " (", censoring_stats$event_rate_pct, "%)")
  })
  
  output$kpi_censored_count <- renderText({
    paste0(format(censoring_stats$censored_count, big.mark = "."), 
           " (", censoring_stats$censoring_rate_pct, "%)")
  })
  
  output$kpi_median_tenure <- renderText({
    paste(censoring_stats$median_tenure_months, "meses")
  })
  
  # --- Gráficos Plotly da Aba 1 ---
  output$plot_km_global <- renderPlotly({
    df_km <- extract_km_table(km_global_fit)
    
    p <- plot_ly(df_km, x = ~time) %>%
      add_ribbons(ymin = ~lower_95, ymax = ~upper_95, name = "IC 95%",
                  fillcolor = "rgba(56, 189, 248, 0.2)", line = list(color = "transparent")) %>%
      add_lines(y = ~survival_prob, name = "S(t) Global",
                line = list(color = "#38bdf8", width = 3, shape = "hv")) %>%
      layout(
        xaxis = list(title = "Tempo de Permanência (Meses)", gridcolor = "#1e293b"),
        yaxis = list(title = "Probabilidade de Sobrevivência S(t)", range = c(0, 1.05), gridcolor = "#1e293b"),
        paper_bgcolor = "#0b0f19",
        plot_bgcolor = "#0b0f19",
        font = list(color = "#f8fafc"),
        hovermode = "x unified"
      )
    p
  })
  
  output$plot_km_stratified <- renderPlotly({
    df_strat <- extract_km_table(km_contract_fit)
    
    p <- plot_ly(df_strat, x = ~time, y = ~survival_prob, color = ~strata,
                 colors = c("#ef4444", "#f59e0b", "#10b981")) %>%
      add_lines(line = list(width = 2.5, shape = "hv")) %>%
      layout(
        xaxis = list(title = "Tempo de Permanência (Meses)", gridcolor = "#1e293b"),
        yaxis = list(title = "Probabilidade de Sobrevivência S(t)", range = c(0, 1.05), gridcolor = "#1e293b"),
        paper_bgcolor = "#0b0f19",
        plot_bgcolor = "#0b0f19",
        font = list(color = "#f8fafc"),
        hovermode = "x unified"
      )
    p
  })
  
  output$table_milestones <- renderDataTable({
    df_ms <- get_survival_milestones(km_contract_fit, times = c(6, 12, 24, 36, 48))
    
    df_ms_clean <- df_ms %>%
      mutate(
        retention_prob = paste0(round(retention_prob * 100, 1), "%"),
        churn_prob_cum = paste0(round(churn_prob_cum * 100, 1), "%"),
        ci_95 = paste0("[", round(lower_95 * 100, 1), "% - ", round(upper_95 * 100, 1), "%]")
      ) %>%
      select(
        Contrato = strata,
        Meses = milestone_months,
        Retenção = retention_prob,
        `Churn Acumulado` = churn_prob_cum,
        `IC 95%` = ci_95
      )
    
    datatable(df_ms_clean, options = list(pageLength = 6, dom = 't'), rownames = FALSE)
  })
  
  output$logrank_diagnostic_ui <- renderUI({
    lr <- logrank_contract
    tagList(
      tags$p(tags$b("Estatística Chi-Quadrado: "), lr$chisq),
      tags$p(tags$b("Graus de Liberdade: "), lr$df),
      tags$p(tags$b("P-Valor: "), ifelse(lr$p_value < 0.001, "< 0.0001", round(lr$p_value, 4))),
      tags$div(
        class = "alert alert-success mt-2",
        lr$interpretation
      )
    )
  })
  
  # --- Aba 2: Modelo de Cox & Hazard Ratios ---
  output$plot_forest_hr <- renderPlotly({
    df_plot <- cox_summary_df %>%
      mutate(
        var_label = factor(variable, levels = rev(variable))
      )
    
    p <- plot_ly(df_plot) %>%
      add_segments(x = ~ci_lower_95, xend = ~ci_upper_95, y = ~var_label, yend = ~var_label,
                   color = ~classification, colors = c("#ef4444", "#10b981", "#94a3b8"),
                   line = list(width = 3), showlegend = FALSE) %>%
      add_markers(x = ~hazard_ratio, y = ~var_label, color = ~classification,
                  colors = c("#ef4444", "#10b981", "#94a3b8"),
                  marker = list(size = 10), name = "Hazard Ratio") %>%
      add_segments(x = 1, xend = 1, y = 0.5, yend = length(df_plot$variable) + 0.5,
                   line = list(dash = "dash", color = "#cbd5e1", width = 1.5), name = "Sem Efeito (HR = 1)") %>%
      layout(
        xaxis = list(title = "Hazard Ratio (Escala de Risco Multiplicativo)", gridcolor = "#1e293b"),
        yaxis = list(title = "", gridcolor = "#1e293b"),
        paper_bgcolor = "#0b0f19",
        plot_bgcolor = "#0b0f19",
        font = list(color = "#f8fafc")
      )
    p
  })
  
  output$table_cox_summary <- renderDataTable({
    df_clean_tbl <- cox_summary_df %>%
      select(
        Covariável = variable,
        `Hazard Ratio (HR)` = hazard_ratio,
        `IC 95% Inferior` = ci_lower_95,
        `IC 95% Superior` = ci_upper_95,
        `P-Valor` = p_value,
        Classificação = classification
      )
    
    datatable(df_clean_tbl, options = list(pageLength = 8, dom = 'tp'), rownames = FALSE)
  })
  
  output$table_schoenfeld <- renderDataTable({
    tbl <- schoenfeld_diag$diagnostic_table %>%
      rename(Covariável = variable, Chisq = chisq, DF = df, `P-Valor` = p)
    datatable(tbl, options = list(pageLength = 7, dom = 't'), rownames = FALSE)
  })
  
  output$schoenfeld_diagnostic_ui <- renderUI({
    tagList(
      tags$p(tags$b("Status Global: "), 
             if(schoenfeld_diag$assumption_met) "Validada (p >= 0.05)" else "Alerta de Violação"),
      tags$div(
        class = if(schoenfeld_diag$assumption_met) "alert alert-success" else "alert alert-warning",
        schoenfeld_diag$conclusion
      )
    )
  })
  
  # --- Aba 3: Simulador What-If Reativo ---
  sim_customer <- reactive({
    data.frame(
      contract_type = factor(input$sim_contract, levels = levels(df_clean$contract_type)),
      monthly_charges = as.numeric(input$sim_charges),
      support_tickets = as.numeric(input$sim_tickets),
      payment_method = factor(input$sim_payment, levels = levels(df_clean$payment_method)),
      paperless_billing = factor(input$sim_paperless, levels = levels(df_clean$paperless_billing)),
      partner = factor(input$sim_partner, levels = levels(df_clean$partner))
    )
  })
  
  sim_results <- reactive({
    cust <- sim_customer()
    surv_proj <- predict_individual_survival(cox_model_fit, cust, max_months = input$sim_horizon)
    ltv_calc <- calculate_residual_ltv(surv_proj, monthly_charges = input$sim_charges)
    list(survival = surv_proj, ltv = ltv_calc)
  })
  
  output$kpi_sim_lifetime <- renderText({
    paste(sim_results()$ltv$expected_lifetime_months, "meses")
  })
  
  output$kpi_sim_prob12 <- renderText({
    prob12 <- sim_results()$survival$survival_prob[sim_results()$survival$month == 12]
    if (length(prob12) == 0) prob12 <- sim_results()$survival$survival_prob[nrow(sim_results()$survival)]
    paste0(round(prob12 * 100, 1), "%")
  })
  
  output$kpi_sim_ltv <- renderText({
    paste0("R$ ", format(sim_results()$ltv$discounted_ltv, big.mark = ".", decimal.mark = ","))
  })
  
  output$plot_sim_curve <- renderPlotly({
    res <- sim_results()$survival
    
    p <- plot_ly(res, x = ~month) %>%
      add_ribbons(ymin = ~lower_95, ymax = ~upper_95, name = "IC 95%",
                  fillcolor = "rgba(16, 185, 129, 0.2)", line = list(color = "transparent")) %>%
      add_lines(y = ~survival_prob, name = "S(t | Perfil Simulado)",
                line = list(color = "#10b981", width = 3)) %>%
      layout(
        xaxis = list(title = "Meses Futuros Projetados", gridcolor = "#1e293b"),
        yaxis = list(title = "Probabilidade de Retenção S(t)", range = c(0, 1.05), gridcolor = "#1e293b"),
        paper_bgcolor = "#0b0f19",
        plot_bgcolor = "#0b0f19",
        font = list(color = "#f8fafc"),
        hovermode = "x unified"
      )
    p
  })
  
  output$plot_sim_cashflow <- renderPlotly({
    cf_data <- sim_results()$ltv$cashflow_schedule
    
    p <- plot_ly(cf_data, x = ~month) %>%
      add_bars(y = ~discounted_cashflow, name = "Fluxo de Caixa Descontado",
               marker = list(color = "#f59e0b")) %>%
      add_lines(y = ~expected_cashflow, name = "Fluxo Nominal Esperado",
                line = list(color = "#38bdf8", width = 2)) %>%
      layout(
        xaxis = list(title = "Mês", gridcolor = "#1e293b"),
        yaxis = list(title = "Receita Projetada (R$)", gridcolor = "#1e293b"),
        paper_bgcolor = "#0b0f19",
        plot_bgcolor = "#0b0f19",
        font = list(color = "#f8fafc"),
        barmode = "overlay"
      )
    p
  })
  
  output$retention_recommendation_ui <- renderUI({
    res <- sim_results()
    cust <- sim_customer()
    lifetime <- res$ltv$expected_lifetime_months
    
    if (cust$contract_type == "Month-to-month" && cust$support_tickets >= 3) {
      tagList(
        tags$div(
          class = "alert alert-danger",
          tags$h6("🚨 Perfil Crítico de Churn Iminente!"),
          tags$p("Cliente em contrato mensal com múltiplos chamados abertos no suporte técnico. ",
                 "O risco acumulado indica sobrevida média de apenas ", tags$b(paste(lifetime, "meses.")),
                 " Ação recomendada: contato prioritário pelo time de Customer Success (CS) e oferta de migração para plano anual com desconto tático.")
        )
      )
    } else if (cust$contract_type == "Two year") {
      tagList(
        tags$div(
          class = "alert alert-success",
          tags$h6("🌟 Cliente Campeão de Retenção (High LTV)"),
          tags$p("O contrato de dois anos atua como forte barreira protetiva contra cancelamento (Hazard Ratio < 0.40). ",
                 "Expectativa de LTV residual de ", tags$b(paste0("R$ ", format(res$ltv$discounted_ltv, big.mark = ".", decimal.mark = ","))),
                 ". Perfil ideal para estratégias de expansão de conta (upsell e cross-sell).")
        )
      )
    } else {
      tagList(
        tags$div(
          class = "alert alert-info",
          tags$h6("ℹ️ Perfil Moderado"),
          tags$p("Risco dentro da média esperada da safra. Recomenda-se incentivar o cadastro de débito automático ou cartão de crédito para reduzir a fricção de pagamento por boleto/cheque eletrônico.")
        )
      )
    }
  })
  
  # --- Aba 4: Tabela e Downloads ---
  output$table_raw_data <- renderDataTable({
    datatable(df_raw, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_raw_csv <- downloadHandler(
    filename = function() { "saas_customer_churn_survival.csv" },
    content = function(file) {
      readr::write_csv(df_raw, file)
    }
  )
  
  output$download_cox_csv <- downloadHandler(
    filename = function() { "cox_hazard_ratios_summary.csv" },
    content = function(file) {
      readr::write_csv(cox_summary_df, file)
    }
  )
}