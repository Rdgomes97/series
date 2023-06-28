install.packages("shinythemes")
library(shinythemes)
library(shiny)
library(readr)
library(ggplot2)
library(forecast)

# Função para carregar os dados do link do GitHub
carregarDados <- function() {
  # URL do arquivo CSV no GitHub
  url_csv <- "https://raw.githubusercontent.com/Rdgomes97/series/main/temp_vf.csv"
  
  # Leitura dos dados do arquivo CSV
  dados <- read_csv(url_csv)
  
  return(dados)
}


# Definir a interface do usuário (ui)
ui <- fluidPage(
  navbarPage(
    title = "Dashboard ~ Análise exploratória de dados",
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    inverse = TRUE,
    header = tags$head(
      tags$style(HTML("
        .navbar-inverse .navbar-nav > li > a {
          color: #fff;
        }
      "))
    ),
    tabPanel(
      "Informações",
      p("Série temporal das temperaturas diárias de São Paulo-Mirante (sempre aferiada as 10:00 am), valores de 2021 até maio de 2023. Trabalho feito por : Rodrigo Gomes ~ 186777 e Vitoria Kauling ~ 245664."),
      style = "padding: 20px;"
    ),
    tabPanel(
      "Série Temporal",
      plotOutput("plot1", height = "400px"),
      plotOutput("tendencia", height = "400px"),
      plotOutput("sazonalidade", height = "400px"),
      plotOutput("aleatoriedade", height = "400px"),
      plotOutput("acf", height = "400px"),
      plotOutput("pacf", height = "400px")
    ),
    tabPanel(
      "Série Diferenciada",
      plotOutput("time_diff_plot", height = "400px"),
      plotOutput("time_diff_acf", height = "400px"),
      plotOutput("time_diff_pacf", height = "400px")
    ),
    tabPanel(
      "Estatísticas Gerais",
      plotOutput("histogram", height = "400px")
    )
  )
)

# Resto do código permanece igual

# Definir o servidor (server)
server <- function(input, output, session) {
  
  # Carregar os dados do link do GitHub
  dados <- carregarDados()
  
  output$plot1 <- renderPlot({
    if (nrow(dados) > 0) {
      # Converter as datas para o formato correto
      dados$data <- as.Date(dados$data, format = "%Y-%m-%d")
      
      # Criar o gráfico da série temporal
      ggplot(data = dados, aes(x = data, y = temp)) +
        geom_line(color = "blue") +
        labs(title = "Time Series Plot",
             x = "Data", y = "Valor") +
        theme_minimal()
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados não encontrados")
      text(0.5, 0.5, "Os dados não puderam ser carregados.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$tendencia <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Converter as datas para o formato correto
      dados$data <- as.Date(dados$data, format = "%Y-%m-%d")
      
      # Decomposição clássica da série temporal
      decomposicao <- stl(ts(dados$temp, frequency = 365), s.window = "periodic")
      
      # Plotar a componente de tendência
      plot(decomposicao$time.series[, "trend"], ylab = "Tendência", xlab = "Data",
           main = "Trend Component", type = "l", col = "blue")
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para decomposição")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$sazonalidade <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Converter as datas para o formato correto
      dados$data <- as.Date(dados$data, format = "%Y-%m-%d")
      
      # Decomposição clássica da série temporal
      decomposicao <- stl(ts(dados$temp, frequency = 365), s.window = "periodic")
      
      # Plotar a componente de sazonalidade
      plot(decomposicao$time.series[, "seasonal"], ylab = "Sazonalidade", xlab = "Data",
           main = "Seasonal Component", type = "l", col = "blue")
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para decomposição")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$aleatoriedade <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Converter as datas para o formato correto
      dados$data <- as.Date(dados$data, format = "%Y-%m-%d")
      
      # Decomposição clássica da série temporal
      decomposicao <- stl(ts(dados$temp, frequency = 365), s.window = "periodic")
      
      # Plotar a componente de aleatoriedade
      plot(decomposicao$time.series[, "remainder"], ylab = "Aleatoriedade", xlab = "Data",
           main = "Random Component", type = "l", col = "blue")
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para decomposição")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$acf <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Calcular a função de autocorrelação
      acf_result <- acf(dados$temp, main = "Autocorrelation Function (ACF)", col = "blue")
      plot(acf_result)
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para cálculo da ACF")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$pacf <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Calcular a função de autocorrelação parcial
      pacf_result <- pacf(dados$temp, main = "Partial Autocorrelation Function (PACF)", col = "blue")
      plot(pacf_result)
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para cálculo da PACF")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$time_diff_plot <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Calcular a série diferencial
      diff_series <- diff(dados$temp, differences = 1)
      
      # Plotar a série diferencial
      plot(diff_series, ylab = "Differential Series", xlab = "Data",
           main = "Differential Series Plot", type = "l", col = "blue")
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para cálculo da série diferencial")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$time_diff_acf <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Calcular a função de autocorrelação da série diferencial
      acf_result <- acf(diff(dados$temp, differences = 1), main = "Autocorrelation Function (ACF) - Differential Series", col = "blue")
      plot(acf_result)
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para cálculo da ACF da série diferencial")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  
  output$time_diff_pacf <- renderPlot({
    if (nrow(dados) > 1) {  # Verificar se há pelo menos 2 observações
      # Calcular a função de autocorrelação parcial da série diferencial
      pacf_result <- pacf(diff(dados$temp, differences = 1), main = "Partial Autocorrelation Function (PACF) - Differential Series", col = "blue")
      plot(pacf_result)
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados insuficientes para cálculo da PACF da série diferencial")
      text(0.5, 0.5, "A série temporal precisa ter pelo menos 2 observações.",
           adj = 0.5, cex = 1.2)
    }
  })
  output$histogram <- renderPlot({
    if (nrow(dados) > 0) {
      # Criar o histograma da variável temp
      ggplot(data = dados, aes(x = temp)) +
        geom_histogram(fill = "blue", color = "black", bins = 30) +
        labs(title = "Histograma da Temperatura",
             x = "Temperatura", y = "Frequência") +
        theme_minimal()
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n",
           main = "Dados não encontrados")
      text(0.5, 0.5, "Os dados não puderam ser carregados.",
           adj = 0.5, cex = 1.2)
    }
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
