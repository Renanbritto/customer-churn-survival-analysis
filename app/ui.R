# ==============================================================================
# R Shiny Application - User Interface (UI)
# Customer Churn Survival Analysis & LTV Modeling
# ==============================================================================

theme_executive <- bslib::bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary = "#38bdf8",
  secondary = "#818cf8",
  success = "#10b981",
  info = "#06b6d4",
  warning = "#f59e0b",
  danger = "#ef4444",
  bg = "#0b0f19",
  fg = "#f8fafc",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)

ui <- bslib::page_navbar(
  theme = theme_executive,
  title = "⏱️ Customer Churn Survival Analysis",
  fillable = TRUE,
  
  # Sidebar com Perfil de Simulação What-If
  sidebar = bslib::sidebar(
    title = "🎛️ Perfil do Cliente (Simulador)",
    width = 330,
    open = "open",
    
    shiny::helpText("Defina as características do cliente para estimar sua sobrevida e LTV residual."),
    
    shiny::selectInput(
      inputId = "sim_contract",
      label = "Tipo de Contrato:",
      choices = c("Month-to-month", "One year", "Two year"),
      selected = "Month-to-month"
    ),
    
    shiny::sliderInput(
      inputId = "sim_charges",
      label = "Mensalidade (R$ MRR):",
      min = 49.0,
      max = 249.0,
      value = 119.0,
      step = 5.0,
      pre = "R$ "
    ),
    
    shiny::sliderInput(
      inputId = "sim_tickets",
      label = "Chamados no Suporte Técnico:",
      min = 0,
      max = 10,
      value = 2,
      step = 1
    ),
    
    shiny::selectInput(
      inputId = "sim_payment",
      label = "Método de Pagamento:",
      choices = c("Electronic check", "Credit card (automatic)", "Bank transfer (automatic)"),
      selected = "Electronic check"
    ),
    
    shiny::selectInput(
      inputId = "sim_paperless",
      label = "Fatura Digital (Paperless):",
      choices = c("Yes", "No"),
      selected = "Yes"
    ),
    
    shiny::selectInput(
      inputId = "sim_partner",
      label = "Possui Parceiro/Cônjuge:",
      choices = c("Yes", "No"),
      selected = "No"
    ),
    
    shiny::hr(),
    shiny::sliderInput(
      inputId = "sim_horizon",
      label = "Horizonte de Projeção (Meses):",
      min = 12,
      max = 60,
      value = 36,
      step = 6
    ),
    
    shiny::helpText("Modelagem com Riscos Proporcionais de Cox e desconto intertemporal a 0.8% ao mês.")
  ),
  
  # Aba 1: Kaplan-Meier
  bslib::nav_panel(
    title = "📈 Kaplan-Meier & Retenção",
    bslib::layout_column_wrap(
      width = 1/4,
      bslib::value_box(
        title = "Total de Clientes Monitorados",
        value = textOutput("kpi_total_customers"),
        showcase = shiny::icon("users"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Cancelamentos Ocorridos (Churn)",
        value = textOutput("kpi_churn_count"),
        showcase = shiny::icon("user-xmark"),
        theme = "danger"
      ),
      bslib::value_box(
        title = "Clientes Ativos (Censurados)",
        value = textOutput("kpi_censored_count"),
        showcase = shiny::icon("shield-heart"),
        theme = "success"
      ),
      bslib::value_box(
        title = "Mediana de Sobrevivência Geral",
        value = textOutput("kpi_median_tenure"),
        showcase = shiny::icon("calendar-days"),
        theme = "info"
      )
    ),
    
    shiny::br(),
    
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        bslib::card_header("Curva Global de Sobrevivência S(t) de Kaplan-Meier"),
        plotly::plotlyOutput("plot_km_global", height = "380px")
      ),
      bslib::card(
        bslib::card_header("Curvas Estratificadas por Tipo de Contrato (Log-Rank Test)"),
        plotly::plotlyOutput("plot_km_stratified", height = "380px")
      )
    ),
    
    shiny::br(),
    
    bslib::card(
      bslib::card_header("Marcos Temporais de Retenção e Diagnóstico de Log-Rank"),
      shiny::fluidRow(
        shiny::column(
          width = 7,
          DT::dataTableOutput("table_milestones")
        ),
        shiny::column(
          width = 5,
          shiny::div(
            class = "p-3 rounded border border-secondary",
            shiny::h5("⚖️ Diagnóstico do Teste de Log-Rank"),
            shiny::uiOutput("logrank_diagnostic_ui")
          )
        )
      )
    )
  ),
  
  # Aba 2: Modelo de Cox & Hazard Ratios
  bslib::nav_panel(
    title = "⚖️ Modelo de Cox & Hazard Ratios",
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        bslib::card_header("Forest Plot: Hazard Ratios (HR) com Intervalo de Confiança de 95%"),
        plotly::plotlyOutput("plot_forest_hr", height = "420px"),
        bslib::card_footer("HR > 1 indica aceleração do cancelamento; HR < 1 indica proteção e retenção.")
      ),
      bslib::card(
        bslib::card_header("Tabela de Coeficientes e Significância Estatística"),
        DT::dataTableOutput("table_cox_summary"),
        bslib::card_footer("Estimativas obtidas por Mxima Verossimilhança Parcial (Partial Likelihood).")
      )
    ),
    
    shiny::br(),
    
    bslib::card(
      bslib::card_header("Diagnóstico da Premissa de Riscos Proporcionais (Resíduos de Schoenfeld)"),
      shiny::fluidRow(
        shiny::column(
          width = 8,
          DT::dataTableOutput("table_schoenfeld")
        ),
        shiny::column(
          width = 4,
          shiny::div(
            class = "p-3 rounded border border-secondary",
            shiny::h5("🔬 Interpretação Metodológica"),
            shiny::uiOutput("schoenfeld_diagnostic_ui")
          )
        )
      )
    )
  ),
  
  # Aba 3: Simulador What-If & LTV
  bslib::nav_panel(
    title = "🎛️ Simulador What-If & LTV",
    bslib::layout_column_wrap(
      width = 1/3,
      bslib::value_box(
        title = "Expectativa de Sobrevida Residual",
        value = textOutput("kpi_sim_lifetime"),
        showcase = shiny::icon("clock"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Retenção em 12 Meses P(T > 12)",
        value = textOutput("kpi_sim_prob12"),
        showcase = shiny::icon("percent"),
        theme = "success"
      ),
      bslib::value_box(
        title = "LTV Residual Descontado Projetado",
        value = textOutput("kpi_sim_ltv"),
        showcase = shiny::icon("money-bill-trend-up"),
        theme = "warning"
      )
    ),
    
    shiny::br(),
    
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        bslib::card_header("Curva Individual de Sobrevivência Projetada S(t | Perfil)"),
        plotly::plotlyOutput("plot_sim_curve", height = "380px")
      ),
      bslib::card(
        bslib::card_header("Cronograma de Fluxo de Caixa Esperado Descontado Mês a Mês"),
        plotly::plotlyOutput("plot_sim_cashflow", height = "380px")
      )
    ),
    
    shiny::br(),
    
    bslib::card(
      bslib::card_header("Recomendação Tática do Comitê de Retenção"),
      shiny::uiOutput("retention_recommendation_ui")
    )
  ),
  
  # Aba 4: Base de Dados & Exportação
  bslib::nav_panel(
    title = "📊 Base de Dados & Exportação",
    bslib::card(
      bslib::card_header("Histórico Censurado de Clientes (2.500 Observações)"),
      DT::dataTableOutput("table_raw_data"),
      bslib::card_footer(
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::downloadButton("download_raw_csv", "📥 Baixar Base de Dados (CSV)", class = "btn-outline-primary")
          ),
          shiny::column(
            width = 6,
            shiny::downloadButton("download_cox_csv", "📥 Baixar Tabela de Hazard Ratios (CSV)", class = "btn-outline-secondary")
          )
        )
      )
    )
  )
)