# ---------------------------------------------------------
# Aplicação Shiny – Comparação PM2.5 (CAMS x DON)
# ---------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(sf)
library(geobr)
library(viridis)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(DescTools)

source('ETL.R')

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("📊 Comparação PM2.5 — CAMS x DON (PA)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ano_sel", "Selecione o ano:", choices = c(2022, 2023), selected = 2023),
      selectInput("grafico", "Selecione o gráfico:",
                  choices = c(
                    "Mapas (CAMS, DON, Diferença)" = "mapa",
                    "Boxplots Mensais" = "box",
                    "Dispersão (Correlação)" = "scatter",
                    "Bland–Altman" = "bland",
                    "Séries Temporais (Diferença DON−CAMS)" = "serie",
                    "Resumo Estatístico" = "resumo"
                  )),
      width = 3
    ),
    
    mainPanel(
      plotOutput("plot", height = "650px"),
      tableOutput("tabela")
    )
  )
)

# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------

server <- function(input, output, session) {
  
  # -------------------------------
  # MAPAS
  # -------------------------------
  output$plot <- renderPlot({
    
    if (input$grafico == "mapa") {
      
      ano_plot <- input$ano_sel
      
      m1 <- mapa_cams |> filter(ano == ano_plot)
      m2 <- mapa_don |> filter(Ano == ano_plot)
      
      diff <- tibble(
        pm25_diff = m2$Media_PM25 - m1$pm2.5_mean,
        geom = m1$geom
      ) |> st_sf()
      
      lims <- range(c(m1$pm2.5_mean, m2$Media_PM25), na.rm=TRUE)
      
      #browser()
      
      p1 <- ggplot(m1) +
        geom_sf(aes(fill = pm2.5_mean)) +
        scale_fill_viridis_c(option="plasma", limits=lims) +
        labs(title = paste("CAMS", ano_plot)) +
        theme_minimal()
      
      p2 <- ggplot(m2) +
        geom_sf(aes(fill = Media_PM25)) +
        scale_fill_viridis_c(option="plasma", limits=lims) +
        labs(title = paste("DON", ano_plot)) +
        theme_minimal()
      
      p3 <- ggplot(diff) +
        geom_sf(aes(fill = pm25_diff)) +
        scale_fill_viridis_c(option="plasma") +
        labs(title = paste("Diferença DON−CAMS", ano_plot)) +
        theme_minimal()
      
      return((p1 | p2 | p3))
      
    }
    
    # -------------------------------
    # BOXPLOTS
    # -------------------------------
    if (input$grafico == "box") {
      
      bc <- base_cams %>%
        mutate(Fonte="CAMS", Valor_PM25 = log1p(pm2.5)) %>%
        select(month, ano, Fonte, Valor_PM25)
      
      bd <- base_don %>%
        mutate(Fonte="DON", Valor_PM25 = log1p(Media_PM25)) %>%
        rename(month = Mes, ano = Ano) %>%
        select(month, ano, Fonte, Valor_PM25)
      
      dados <- bind_rows(bc, bd) |> filter(ano == input$ano_sel)
      
      p <- ggplot(dados, aes(x=factor(month), y=Valor_PM25, fill=Fonte)) +
        geom_boxplot(alpha=0.7, position=position_dodge()) +
        scale_fill_manual(values=c("CAMS"="#8B008B","DON"="#FF8C00")) +
        labs(title=paste("Boxplots Mensais —", input$ano_sel),
             x="Mês", y="log(PM2.5 + 1)") +
        theme_minimal()
      
      return(p)
    }
    
    # -------------------------------
    # DISPERSÃO
    # -------------------------------
    if (input$grafico == "scatter") {
      
      df <- base_merge |> filter(ano == input$ano_sel)
      
      p <- ggplot(df, aes(PM25_CAMS, PM25_DON)) +
        geom_point(alpha=0.6, color="#8B008B") +
        geom_smooth(method="lm", color="orange") +
        labs(title="Correlação CAMS x DON",
             x="CAMS", y="DON") +
        theme_minimal()
      
      return(p)
    }
    
    # -------------------------------
    # BLAND–ALTMAN
    # -------------------------------
    if (input$grafico == "bland") {
      
      df <- base_merge |> filter(ano == input$ano_sel)
      
      df <- df |> mutate(
        media = (PM25_CAMS + PM25_DON)/2,
        diff = PM25_DON - PM25_CAMS
      )
      
      mean_diff <- mean(df$diff)
      sd_diff   <- sd(df$diff)
      
      p <- ggplot(df, aes(media, diff)) +
        geom_point(alpha=.6) +
        geom_hline(yintercept=mean_diff, color="red") +
        geom_hline(yintercept=mean_diff + 1.96*sd_diff, linetype="dashed") +
        geom_hline(yintercept=mean_diff - 1.96*sd_diff, linetype="dashed") +
        labs(title="Bland–Altman",
             x="Média", y="Diferença DON−CAMS") +
        theme_minimal()
      
      return(p)
    }
    
    # -------------------------------
    # SÉRIE TEMPORAL
    # -------------------------------
    if (input$grafico == "serie") {
      
      ano <- input$ano_sel
      
      df_c <- base_cams |> filter(ano == ano) |>
        group_by(month) |>
        summarise(PM25_CAMS = mean(pm2.5))
      
      df_d <- base_don |> filter(Ano == ano) |>
        group_by(Mes) |>
        summarise(PM25_DON = mean(Media_PM25)) |>
        rename(month = Mes)
      
      df <- inner_join(df_c, df_d, by="month") |>
        mutate(Diff = PM25_DON - PM25_CAMS)
      
      df$Data <- as.Date(paste(ano, df$month, "01", sep="-"))
      
      p <- ggplot(df, aes(Data, Diff)) +
        geom_line(color="purple") +
        geom_point(color="darkred") +
        geom_hline(yintercept=0, linetype="dashed") +
        labs(title=paste("Diferença mensal DON−CAMS —", ano),
             y="Diferença") +
        theme_minimal()
      
      return(p)
    }
    
  })
  
  # -------------------------------
  # Tabela resumo
  # -------------------------------
  output$tabela <- renderTable({
    
    if (input$grafico != "resumo") return(NULL)
    
    tibble(
      Fonte = c("CAMS", "DON", "Diferença"),
      Média = c(
        mean(mapa_cams$pm2.5_mean, na.rm=TRUE),
        mean(mapa_don$Media_PM25, na.rm=TRUE),
        mean(mapa_don$Media_PM25 - mapa_cams$pm2.5_mean, na.rm=TRUE)
      ),
      Mediana = c(
        median(mapa_cams$pm2.5_mean, na.rm=TRUE),
        median(mapa_don$Media_PM25, na.rm=TRUE),
        median(mapa_don$Media_PM25 - mapa_cams$pm2.5_mean, na.rm=TRUE)
      ),
      Desvio = c(
        sd(mapa_cams$pm2.5_mean, na.rm=TRUE),
        sd(mapa_don$Media_PM25, na.rm=TRUE),
        sd(mapa_don$Media_PM25 - mapa_cams$pm2.5_mean, na.rm=TRUE)
      )
    )
    
  })
  
}

# ---------------------------------------------------------
# EXECUTAR APP
# ---------------------------------------------------------
shinyApp(ui, server)
