# ==============================================================================
# R Shiny Application - User Interface (UI) - Redesign Executivo Premium
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
  bg = "#070b14",
  fg = "#f1f5f9",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Outfit")
)

ui <- bslib::page_navbar(
  theme = theme_executive,
  title = shiny::div(
    style = "display: flex; align-items: center; gap: 10px; font-weight: 700; font-size: 1.15rem;",
    shiny::tags$span(style = "font-size: 1.4rem;", "⏱️"),
    shiny::tags$span(
      style = "background: linear-gradient(90deg, #38bdf8, #818cf8, #c084fc); -webkit-background-clip: text; -webkit-text-fill-color: transparent;",
      "Customer Churn Survival Analytics"
    )
  ),
  fillable = TRUE,
  
  # Injeção de CSS de Alta Fidelidade (Custom Design System)
  header = shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* Background Global & Scrollbar */
      body {
        background-color: #070b14 !important;
      }
      ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }
      ::-webkit-scrollbar-track {
        background: #0b1120;
      }
      ::-webkit-scrollbar-thumb {
        background: #1e293b;
        border-radius: 4px;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: #334155;
      }

      /* Navbar Moderna */
      .navbar {
        background: rgba(11, 17, 32, 0.85) !important;
        backdrop-filter: blur(16px);
        border-bottom: 1px solid rgba(255, 255, 255, 0.08) !important;
        padding: 12px 24px !important;
      }
      .nav-link {
        font-weight: 500 !important;
        font-size: 0.95rem !important;
        color: #94a3b8 !important;
        transition: all 0.2s ease-in-out !important;
        border-radius: 8px !important;
        margin: 0 4px !important;
        padding: 8px 16px !important;
      }
      .nav-link:hover {
        color: #f8fafc !important;
        background: rgba(255, 255, 255, 0.05) !important;
      }
      .nav-link.active {
        color: #38bdf8 !important;
        background: rgba(56, 189, 248, 0.12) !important;
        border-bottom: 2px solid #38bdf8 !important;
        font-weight: 600 !important;
      }

      /* Cards Executivos de KPIs (Substituindo o value_box padrão quebrado) */
      .kpi-card {
        background: linear-gradient(145deg, #111827 0%, #0b0f19 100%);
        border: 1px solid rgba(255, 255, 255, 0.08);
        border-radius: 14px;
        padding: 20px 22px;
        position: relative;
        overflow: hidden;
        box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.4);
        transition: transform 0.2s ease, border-color 0.2s ease;
      }
      .kpi-card:hover {
        transform: translateY(-2px);
        border-color: rgba(56, 189, 248, 0.3);
      }
      .kpi-card-glow-blue { border-top: 3px solid #38bdf8; }
      .kpi-card-glow-red { border-top: 3px solid #ef4444; }
      .kpi-card-glow-green { border-top: 3px solid #10b981; }
      .kpi-card-glow-amber { border-top: 3px solid #f59e0b; }

      .kpi-label {
        font-size: 0.78rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        color: #94a3b8;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .kpi-value {
        font-size: 1.85rem;
        font-weight: 800;
        color: #f8fafc;
        margin-top: 8px;
        margin-bottom: 4px;
        line-height: 1.2;
      }
      .kpi-subtext {
        font-size: 0.85rem;
        font-weight: 500;
        color: #64748b;
      }
      .kpi-icon-badge {
        width: 34px;
        height: 34px;
        border-radius: 8px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 1rem;
      }

      /* Cards de Conteúdo e Gráficos */
      .card {
        background: #0d1322 !important;
        border: 1px solid rgba(255, 255, 255, 0.08) !important;
        border-radius: 14px !important;
        box-shadow: 0 8px 30px rgba(0, 0, 0, 0.35) !important;
        overflow: hidden !important;
      }
      .card-header {
        background: rgba(17, 24, 39, 0.6) !important;
        border-bottom: 1px solid rgba(255, 255, 255, 0.06) !important;
        font-weight: 700 !important;
        font-size: 0.95rem !important;
        color: #e2e8f0 !important;
        padding: 14px 20px !important;
      }
      .card-footer {
        background: rgba(15, 23, 42, 0.4) !important;
        border-top: 1px solid rgba(255, 255, 255, 0.05) !important;
        font-size: 0.82rem !important;
        color: #94a3b8 !important;
        padding: 10px 18px !important;
      }

      /* Sidebar Customizada */
      .bslib-sidebar-layout > .sidebar {
        background: #090e1a !important;
        border-right: 1px solid rgba(255, 255, 255, 0.08) !important;
        padding: 20px 18px !important;
      }
      .sidebar-title {
        font-size: 1.05rem;
        font-weight: 700;
        color: #38bdf8;
        margin-bottom: 4px;
      }

      /* Tabelas DT Dark */
      table.dataTable {
        background-color: transparent !important;
        color: #cbd5e1 !important;
        font-size: 0.88rem !important;
      }
      table.dataTable thead th {
        background-color: #111827 !important;
        color: #94a3b8 !important;
        border-bottom: 1px solid #1e293b !important;
        font-weight: 600 !important;
        text-transform: uppercase !important;
        font-size: 0.76rem !important;
        letter-spacing: 0.05em !important;
      }
      table.dataTable tbody tr {
        background-color: transparent !important;
        border-bottom: 1px solid rgba(255, 255, 255, 0.04) !important;
      }
      table.dataTable tbody tr:hover {
        background-color: rgba(56, 189, 248, 0.05) !important;
      }

      /* Badges */
      .badge-custom {
        display: inline-block;
        padding: 4px 10px;
        border-radius: 6px;
        font-size: 0.78rem;
        font-weight: 600;
      }
      .badge-risk {
        background: rgba(239, 68, 68, 0.15);
        color: #f87171;
        border: 1px solid rgba(239, 68, 68, 0.3);
      }
      .badge-protect {
        background: rgba(16, 185, 129, 0.15);
        color: #34d399;
        border: 1px solid rgba(16, 185, 129, 0.3);
      }
      .badge-neutral {
        background: rgba(148, 163, 184, 0.15);
        color: #94a3b8;
        border: 1px solid rgba(148, 163, 184, 0.3);
      }
    "))
  ),
  
  # Sidebar com Perfil de Simulação What-If
  sidebar = bslib::sidebar(
    title = shiny::div(
      class = "sidebar-title",
      "🎛️ Perfil do Cliente"
    ),
    width = 320,
    open = "open",
    
    shiny::tags$p(
      style = "font-size: 0.85rem; color: #94a3b8; margin-bottom: 16px;",
      "Altere as variáveis para prever a curva de sobrevida individual e o LTV residual."
    ),
    
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
      label = "Chamados no Suporte:",
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
    
    shiny::hr(style = "border-color: rgba(255, 255, 255, 0.08);"),
    
    shiny::sliderInput(
      inputId = "sim_horizon",
      label = "Horizonte de Projeção (Meses):",
      min = 12,
      max = 60,
      value = 36,
      step = 6
    ),
    
    shiny::tags$div(
      style = "background: rgba(56, 189, 248, 0.08); border: 1px solid rgba(56, 189, 248, 0.2); border-radius: 8px; padding: 10px 12px; font-size: 0.8rem; color: #94a3b8;",
      "💡 ", shiny::tags$b("Metodologia:"), " Riscos Proporcionais de Cox com taxa de desconto intertemporal de 0.8% a.m."
    )
  ),
  
  # ============================================================================
  # ABA 1: KAPLAN-MEIER & RETENÇÃO
  # ============================================================================
  bslib::nav_panel(
    title = "📈 Kaplan-Meier & Retenção",
    
    # 4 Cards Executivos com Layout Elegante
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::div(
          class = "kpi-card kpi-card-glow-blue",
          shiny::div(
            class = "kpi-label",
            "Base Monitorada",
            shiny::div(class = "kpi-icon-badge", style = "background: rgba(56, 189, 248, 0.15); color: #38bdf8;", "👥")
          ),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_total_customers")),
          shiny::div(class = "kpi-subtext", "Total de clientes sob observação")
        )
      ),
      shiny::column(
        width = 3,
        shiny::div(
          class = "kpi-card kpi-card-glow-red",
          shiny::div(
            class = "kpi-label",
            "Eventos de Churn",
            shiny::div(class = "kpi-icon-badge", style = "background: rgba(239, 68, 68, 0.15); color: #ef4444;", "❌")
          ),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_churn_count")),
          shiny::div(class = "kpi-subtext", shiny::textOutput("kpi_churn_pct"))
        )
      ),
      shiny::column(
        width = 3,
        shiny::div(
          class = "kpi-card kpi-card-glow-green",
          shiny::div(
            class = "kpi-label",
            "Clientes Ativos (Censurados)",
            shiny::div(class = "kpi-icon-badge", style = "background: rgba(16, 185, 129, 0.15); color: #10b981;", "🛡️")
          ),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_censored_count")),
          shiny::div(class = "kpi-subtext", shiny::textOutput("kpi_censored_pct"))
        )
      ),
      shiny::column(
        width = 3,
        shiny::div(
          class = "kpi-card kpi-card-glow-amber",
          shiny::div(
            class = "kpi-label",
            "Mediana de Sobrevivência",
            shiny::div(class = "kpi-icon-badge", style = "background: rgba(245, 158, 11, 0.15); color: #f59e0b;", "📅")
          ),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_median_tenure")),
          shiny::div(class = "kpi-subtext", "Tempo em que 50% ainda retém")
        )
      )
    ),
    
    shiny::br(),
    
    # Linha com Gráficos Kaplan-Meier
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Curva Global de Sobrevivência S(t) de Kaplan-Meier"),
          plotly::plotlyOutput("plot_km_global", height = "370px"),
          bslib::card_footer("Faixa sombreada representa o Intervalo de Confiança assintótico de 95%.")
        )
      ),
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Curvas Estratificadas por Tipo de Contrato (Log-Rank Test)"),
          plotly::plotlyOutput("plot_km_stratified", height = "370px"),
          bslib::card_footer("Contratos anuais e bianuais atuam como forte barreira à evasão precoce.")
        )
      )
    ),
    
    shiny::br(),
    
    # Marcos Temporais e Diagnóstico
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
            style = "background: rgba(15, 23, 42, 0.6); border: 1px solid rgba(255, 255, 255, 0.08); border-radius: 10px; padding: 18px;",
            shiny::h5(style = "color: #38bdf8; font-weight: 700; margin-bottom: 14px;", "⚖️ Diagnóstico do Teste de Log-Rank"),
            shiny::uiOutput("logrank_diagnostic_ui")
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # ABA 2: MODELO DE COX & HAZARD RATIOS
  # ============================================================================
  bslib::nav_panel(
    title = "⚖️ Modelo de Cox & Hazard Ratios",
    
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Forest Plot: Hazard Ratios (HR) com Intervalos de Confiança (95%)"),
          plotly::plotlyOutput("plot_forest_hr", height = "430px"),
          bslib::card_footer("HR > 1 indica risco aumentado de churn; HR < 1 indica fator de proteção/retenção.")
        )
      ),
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Tabela de Coeficientes e Significância Estatística"),
          DT::dataTableOutput("table_cox_summary"),
          bslib::card_footer("Estimativas ajustadas por Mxima Verossimilhança Parcial de Cox.")
        )
      )
    ),
    
    shiny::br(),
    
    bslib::card(
      bslib::card_header("Diagnóstico de Riscos Proporcionais (Resíduos de Schoenfeld)"),
      shiny::fluidRow(
        shiny::column(
          width = 7,
          DT::dataTableOutput("table_schoenfeld")
        ),
        shiny::column(
          width = 5,
          shiny::div(
            style = "background: rgba(15, 23, 42, 0.6); border: 1px solid rgba(255, 255, 255, 0.08); border-radius: 10px; padding: 18px;",
            shiny::h5(style = "color: #818cf8; font-weight: 700; margin-bottom: 12px;", "🔬 Avaliação Metodológica"),
            shiny::uiOutput("schoenfeld_diagnostic_ui")
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # ABA 3: SIMULADOR WHAT-IF & LTV
  # ============================================================================
  bslib::nav_panel(
    title = "🎛️ Simulador What-If & LTV",
    
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::div(
          class = "kpi-card kpi-card-glow-blue",
          shiny::div(class = "kpi-label", "Sobrevida Média Esperada", shiny::div(class = "kpi-icon-badge", "⏳")),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_sim_lifetime")),
          shiny::div(class = "kpi-subtext", "Meses projetados de vida útil")
        )
      ),
      shiny::column(
        width = 4,
        shiny::div(
          class = "kpi-card kpi-card-glow-green",
          shiny::div(class = "kpi-label", "Retenção em 12 Meses", shiny::div(class = "kpi-icon-badge", "🎯")),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_sim_prob12")),
          shiny::div(class = "kpi-subtext", "Probabilidade acumulada P(T > 12)")
        )
      ),
      shiny::column(
        width = 4,
        shiny::div(
          class = "kpi-card kpi-card-glow-amber",
          shiny::div(class = "kpi-label", "LTV Residual Descontado", shiny::div(class = "kpi-icon-badge", "💰")),
          shiny::div(class = "kpi-value", shiny::textOutput("kpi_sim_ltv")),
          shiny::div(class = "kpi-subtext", "Valor presente líquido da receita futura")
        )
      )
    ),
    
    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Curva Individualizada de Sobrevivência S(t | Perfil Simulado)"),
          plotly::plotlyOutput("plot_sim_curve", height = "370px"),
          bslib::card_footer("Projeção condicional ao perfil selecionado nos controles laterais.")
        )
      ),
      shiny::column(
        width = 6,
        bslib::card(
          bslib::card_header("Cronograma de Fluxo de Caixa Esperado Mês a Mês (R$)"),
          plotly::plotlyOutput("plot_sim_cashflow", height = "370px"),
          bslib::card_footer("Barras = Fluxo Descontado a 0.8% a.m.; Linha = Receita Nominal Esperada.")
        )
      )
    ),
    
    shiny::br(),
    
    bslib::card(
      bslib::card_header("Plano de Ação Tático para Retenção & CS"),
      shiny::uiOutput("retention_recommendation_ui")
    )
  ),
  
  # ============================================================================
  # ABA 4: BASE DE DADOS & EXPORTAÇÃO
  # ============================================================================
  bslib::nav_panel(
    title = "📊 Base de Dados & Exportação",
    bslib::card(
      bslib::card_header("Base de Dados Completa (2.500 Clientes)"),
      DT::dataTableOutput("table_raw_data"),
      bslib::card_footer(
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::downloadButton("download_raw_csv", "📥 Exportar Base Completa (CSV)", class = "btn btn-outline-info")
          ),
          shiny::column(
            width = 6,
            shiny::downloadButton("download_cox_csv", "📥 Exportar Tabela de Hazard Ratios (CSV)", class = "btn btn-outline-secondary")
          )
        )
      )
    )
  )
)