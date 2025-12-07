library(shiny)
library(tidyverse)
library(lubridate)
library(sf)
library(geobr)
library(viridis)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(plotly)

source("ETL.R")  # suas bases já preparadas: base_cams, base_don, base_merge, mapa_cams, mapa_don

# ============================================
# UI
# ============================================
ui <- fluidPage(
  tags$style(HTML("
    #sidebar { width: 260px; height: 100vh; position: fixed; top: 0; left: 0; background-color: #001f3f; padding: 25px; color: white; overflow-y: auto; }
    #sidebar .control-label, #sidebar h4, #sidebar h3 { color: white; }
    #sidebar .nav-tabs > li > a { color: white !important; }
    #sidebar .nav-tabs > li.active > a { background-color: #003366 !important; color: white !important; }
    #main { margin-left: 280px; padding: 20px; }
  ")),
  
  div(
    id = "sidebar",
    h3("📊 PM2.5 — CAMS x DON (PA)"),
    hr(),
    h4("Visualizações"),
    tabsetPanel(
      id = "grafico",
      type = "tabs",
      tabPanel("Mapas", value = "mapa"),
      tabPanel("Boxplots", value = "box"),
      tabPanel("Dispersão", value = "scatter"),
      tabPanel("Bland–Altman", value = "bland"),
      tabPanel("Séries Temporais", value = "serie"),
      tabPanel("Resumo Estatístico", value = "resumo")
    )
  ),
  
  div(
    id = "main",
    uiOutput("main_ui")
  )
)

# ============================================
# SERVER
# ============================================
server <- function(input, output, session) {
  
  output$main_ui <- renderUI({
    g <- input$grafico
    tagList(
      # Filtro de ano para mapas, boxplots e séries
      if(g %in% c("mapa","box","serie")) {
        selectInput("ano_sel","Ano:", choices = c(2022,2023), selected=2023)
      },
      uiOutput(paste0("titulo_", g)),
      uiOutput(paste0("texto_", g)),
      
      # Escolhe o output certo para cada aba
      if(g %in% c("mapa","serie")) {
        plotOutput("plot_gg", height="650px")
      } else if(g == "box") {
        plotlyOutput("plot_box", height="650px")
      } else if(g == "scatter") {
        plotlyOutput("plot_scatter", height="650px")
      } else if(g == "bland") {
        plotlyOutput("plot_bland", height="650px")
      } else if(g == "resumo") {
        htmlOutput("tabela_ui")
      }
    )
  })
  
  # ============================================
  # Textos de cada aba
  # ============================================
  output$titulo_mapa <- renderUI({ h3("Distribuição Espacial das Concentrações de PM2.5") })
  output$texto_mapa <- renderUI({ HTML("O mapa mostra a distribuição média de PM2.5 estimada pelas fontes DON (Donkelar) e CAMS, bem como a diferença entre elas.") })
  
  output$titulo_box <- renderUI({ h3("Distribuição Mensal — Boxplots") })
  output$texto_box <- renderUI({ HTML("Boxplots mensais de PM2.5 (log1p) por fonte.") })
  
  output$titulo_scatter <- renderUI({ h3("Correlação CAMS x DON") })
  output$texto_scatter <- renderUI({ HTML("Relação entre CAMS e DON.") })
  
  output$titulo_bland <- renderUI({ h3("Concordância Bland–Altman") })
  output$texto_bland <- renderUI({ HTML("Diferenças médias e limites de concordância.") })
  
  output$titulo_serie <- renderUI({ h3("Séries Temporais das Diferenças Mensais") })
  output$texto_serie <- renderUI({ HTML("Diferença mensal DON−CAMS.") })
  
  output$titulo_resumo <- renderUI({ h3("Resumo Estatístico") })
  output$texto_resumo <- renderUI({ HTML("Média, mediana e desvio padrão por fonte e diferenças.") })
  
  # ============================================
  # Gráficos ggplot2: Mapas e Série Temporal
  # ============================================
  output$plot_gg <- renderPlot({
    g <- input$grafico
    ano_sel <- input$ano_sel
    
    if(g=="mapa"){
      m1 <- mapa_cams |> filter(ano == ano_sel)
      m2 <- mapa_don |> filter(Ano == ano_sel)
      
      diff <- tibble(
        pm25_diff = m2$Media_PM25 - m1$pm2.5_mean,
        geom = m1$geom
      ) |> st_sf()
      
      lims <- range(c(m1$pm2.5_mean, m2$Media_PM25), na.rm=TRUE)
      p1 <- ggplot(m1) + geom_sf(aes(fill = pm2.5_mean)) + scale_fill_viridis_c(option="plasma",limits=lims) + labs(title="CAMS") + theme_minimal()
      p2 <- ggplot(m2) + geom_sf(aes(fill = Media_PM25)) + scale_fill_viridis_c(option="plasma",limits=lims) + labs(title="DON") + theme_minimal()
      p3 <- ggplot(diff) + geom_sf(aes(fill = pm25_diff)) + scale_fill_viridis_c(option="plasma") + labs(title="Diferença DON−CAMS") + theme_minimal()
      
      p1 | p2 | p3
      
    } else if(g=="serie"){
      df_c <- base_cams |> filter(ano == ano_sel) |> group_by(month) |> summarise(PM25_CAMS = mean(pm2.5, na.rm=TRUE))
      df_d <- base_don |> filter(Ano == ano_sel) |> group_by(Mes) |> summarise(PM25_DON = mean(Media_PM25, na.rm=TRUE)) |> rename(month = Mes)
      df <- inner_join(df_c, df_d, by="month") |> mutate(Diff = PM25_DON - PM25_CAMS)
      df$Data <- as.Date(paste(ano_sel, df$month, "01", sep="-"))
      ggplot(df, aes(Data, Diff)) + geom_line(color="purple") + geom_point(color="darkred") + geom_hline(yintercept=0, linetype="dashed") + labs(title="Diferença mensal DON−CAMS", y="Diferença") + theme_minimal()
    }
  })
  
  # ============================================
  # Gráficos interativos com Plotly
  # ============================================
  output$plot_box <- renderPlotly({
    bc <- base_cams |> filter(ano==input$ano_sel) |> mutate(Fonte="CAMS", Valor_PM25=log1p(pm2.5)) |> select(month, ano, Fonte, Valor_PM25)
    bd <- base_don |> filter(Ano==input$ano_sel) |> mutate(Fonte="DON", Valor_PM25=log1p(Media_PM25)) |> rename(month=Mes, ano=Ano) |> select(month, ano, Fonte, Valor_PM25)
    dados <- bind_rows(bc, bd)
    
    p <- ggplot(dados, aes(factor(month), Valor_PM25, fill=Fonte)) +
      geom_boxplot(alpha=.7, position=position_dodge()) +
      scale_fill_manual(values=c("CAMS"="#8B008B", "DON"="#FF8C00")) +
      labs(title="Boxplots Mensais", x="Mês", y="log(PM2.5 + 1)") + theme_minimal()
    
    ggplotly(p)
  })
  
  output$plot_scatter <- renderPlotly({
    p <- ggplot(base_merge, aes(PM25_CAMS, PM25_DON)) +
      geom_point(alpha=.6, color="#8B008B") +
      geom_smooth(method="lm", color="orange") +
      labs(title="Correlação CAMS x DON", x="CAMS", y="DON") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_bland <- renderPlotly({
    df <- base_merge |> mutate(media=(PM25_CAMS+PM25_DON)/2, diff=PM25_DON-PM25_CAMS)
    mean_diff <- mean(df$diff, na.rm=TRUE)
    sd_diff <- sd(df$diff, na.rm=TRUE)
    p <- ggplot(df, aes(media,diff)) +
      geom_point(alpha=.6) +
      geom_hline(yintercept=mean_diff, color="red") +
      geom_hline(yintercept=mean_diff+1.96*sd_diff, linetype="dashed") +
      geom_hline(yintercept=mean_diff-1.96*sd_diff, linetype="dashed") +
      labs(title="Bland–Altman", x="Média", y="Diferença DON−CAMS") + theme_minimal()
    ggplotly(p)
  })
  
  # ============================================
  # Tabela resumo estilizada
  # ============================================
  output$tabela_ui <- renderText({
    tbl <- tibble(
      Fonte = c("CAMS", "DON", "Diferença"),
      Média = c(
        mean(mapa_cams$pm2.5_mean, na.rm = TRUE),
        mean(mapa_don$Media_PM25, na.rm = TRUE),
        mean(mapa_don$Media_PM25 - mapa_cams$pm2.5_mean, na.rm = TRUE)
      ),
      Mediana = c(
        median(mapa_cams$pm2.5_mean, na.rm = TRUE),
        median(mapa_don$Media_PM25, na.rm = TRUE),
        median(mapa_don$Media_PM25 - mapa_cams$pm2.5_mean, na.rm = TRUE)
      ),
      Desvio = c(
        sd(mapa_cams$pm2.5_mean, na.rm = TRUE),
        sd(mapa_don$Media_PM25, na.rm = TRUE),
        sd(mapa_don$Media_PM25 - mapa_cams$pm2.5_mean, na.rm = TRUE)
      )
    )
    
    HTML(
      tbl |> 
        kable("html", digits = 2, align = c("l", "r", "r", "r")) |>  
        kable_styling(
          full_width = FALSE, 
          bootstrap_options = c("striped", "hover", "condensed", "responsive")
        ) |>  
        row_spec(0, bold = TRUE, background = "#00539C", color = "white") |>  
        column_spec(1, bold = TRUE) |>  
        as.character()
    )
  })
  
}

# ============================================
# RUN APP
# ============================================
shinyApp(ui, server)
