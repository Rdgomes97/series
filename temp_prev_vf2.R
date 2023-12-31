#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("fpp3")
#install.packages("tsibble")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("readr")
#install.packages("rugarch")
library(lubridate)
library(tsibble)
library(tidyverse)
library(stringr)
library(fpp3)
library(tseries)
library(forecast)
library(readr)
library(rugarch)


temp <- read_csv("https://raw.githubusercontent.com/Rdgomes97/series/main/temp_vf.csv",
                 col_types = c(temp = 'd'))

temp <- temp %>% 
  select(data,temp)
# Gr�ficos
#Sequ�ncia:
temp %>%
  ggplot(aes(x = data, y = temp)) +
  geom_line(col = 'blueviolet') +
  labs(x = "Data", y = "Temperatura") +
  scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 25)) +
  theme_minimal()

#Boxplot por ano
ggplot(temp, aes(x = data, y = temp, fill = factor(year(data)))) +
  geom_boxplot() +
  labs(
    title = "Distribui��o anual de temperatura",
    x = "Data",
    y = "Temperatura (�C)",
    fill = "Ano"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


#ACF e PACF
op_2 <-  par(mfrow = c(1,2))
acf(temp$temp, lag.max = 50, main = "Autocorrela��o")
pacf(temp$temp, lag.max = 50, 
     main = "Autocorrela��o parcial", ylab = "ACF parcial")
par(op_2)


# Transformando em ts
ts_temp <- ts(temp$temp, 
              frequency=365, start=c(year(min(temp$data)), 
                                     month(min(temp$data))))

# Decomposicao para EDA
decomposicao <- decompose(ts_temp)
plot(decomposicao) # decomposicao classica

plot(decomposicao$seasonal, main = "Sazonalidade - St", 
     xlab="Data", ylab="Valor", col="mediumpurple1") # apenas a parte sazonal 

summary(ts_temp)

# Como � poss�vel observar a existencia de tendencia na decomposicao da serie

temp_diff <- diff(temp$temp)
temp$temp_diff <- c(temp$temp[1], temp_diff)
summary(temp_diff)

op_orig <- par(mfrow = c(1,1))
temp %>%
  ggplot(aes(x = data, y = temp_diff)) +
  geom_line(col = 'blueviolet') +
  labs(x = "Data", y = "Temperatura") +
  scale_y_continuous(breaks = seq(-10, 8, 2), limits = c(-10, 8)) +
  theme_minimal()
par(op_orig)

# ACF e PACF s�rie diferenciada
acf(temp$temp_diff, lag.max = 50, main = "Autocorrela��o s�rie diff")
pacf(temp$temp_diff, lag.max = 50, 
     main = "Autocorrela��o parcial s�rie diff", ylab = "ACF parcial")
par(op_2)



#_____________________ s�rie dif � estacion�ria ? _________________________# 

#usar adf.test() (teste da raiz unitaria) para testar se a s�rie � estacionaria ou n�o
#h0: A serie temporal nao � considerada estacionaria
adf.test(temp_diff) 
pp.test(temp_diff)
kpss.test(temp_diff) #H0: assume estacionaridade
# O resultado dos testes ADF e PP indica que o valor-p � menor do que o valor-p impresso, ou seja, 
# � poss�vel rejeitar H0 indicando que a s�rie � estacion�ria 
#O resultado so teste KPSS  indica que o valor-p � maior do que o valor-p impresso, ou seja, 
# n�o rejeita-se H0 indicando que a s�rie � estacion�ria


# Usamos o auto.arima() para conseguirmos obter mais alguma informa��o antes de 
# escolhermos os  modelos que consideramos adequados 
modelo_aut <- auto.arima(ts_temp)
summary(modelo_aut)



#_____________________________________________________________________________#
# ARIMA 212 
modelo_arima <- arima(temp_diff, order = c(2, 0, 2))
summary(modelo_arima)
# AIC 3436.42

# Obten��o dos res�duos do modelo ARIMA
residuos <- residuals(modelo_arima)

par(op_orig)
ts.plot(residuos, xlab = "Lag", ylab = "Res�duos")

acf(residuos, main = "Autocorrelation Function (ACF) of Residuals")
pacf(residuos, main = "Partial Autocorrelation Function (PACF) of Residuals")

# Step 5: Statistical tests
Box.test(residuos, lag = lag_value, type = test_type)  # Replace 'lag_value' and 'test_type' with appropriate values for the Ljung-Box test

# Step 6: Histogram and summary statistics
hist(residuos, main = "Histogram of Residuals")
summary(residuos)

# Ajuste do modelo SARIMA(2,0,2)(1,0,1,12)
modelo_sarima <- arima(temp_diff, order = c(2, 0, 2), seasonal = list(order = c(1, 1, 1), period = 12))
summary(modelo_sarima)
# AIC 3442.29


# Segundo o AIC, RMSE e ACF1 conclu�mos um melhor desempenho do modelo ARIMA

#______MODELO GARCH_______________
# Ajuste do modelo GARCH(1,1) aos res�duos
modelo_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))

modelo_garch_ajustado <- ugarchfit(data = residuos, spec = modelo_garch)

# Examine o resumo do modelo GARCH
show(modelo_garch_ajustado)

# An�lise de res�duos:
residuo_padr <- residuals(modelo_garch_ajustado, standardize = TRUE)

# Gr�fico dos residuos
plot_resid_garch <- ggplot(data = data.frame(Residuos = residuo_padr), aes(x = seq_along(Residuos), y = Residuos)) +
  geom_line(col = 'blueviolet') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Lag", y = "Residuos", title = "Plot res�duos") +
  theme_minimal()
plot_resid_garch

# ACF e PACF dos res�duos
par(op_2)
acf(residuo_padr, main = "ACF dos res�duos")
pacf(residuo_padr, main = "PACF dos res�duos")

# Box test
Box.test(residuo_padr, lag = 30,type = "Ljung-Box")

# Histograma
par(op_orig)
hist(residuo_padr, main = "Histograma do res�duo", col = 'blueviolet')
summary(residuo_padr)

# Breve conclus�o sobre a an�lise acima:

## Os gr�ficos ACF e PACF revelaram que os lags est�o dentro das bandas de confian�a, 
## indicando a aus�ncia de autocorrela��o significativa. Tamb�m confirmada pelo
## Box.test() realizado.
## Esses resultados sugerem que o modelo ajustado capturou adequadamente a estrutura de autocorrela��o presente nos dados.

## Al�m disso, os res�duos padronizados se assemelharam a um ru�do branco, 
## indicando que n�o h� padr�es significativos de autocorrela��o residual ou heterocedasticidade. 
## A normalidade dos res�duos tamb�m foi verificada e pode ser considerada.

## Com base nessas evid�ncias, podemos concluir que a an�lise dos res�duos foi 
## satisfat�ria e indica que o modelo ajustado � apropriado para descrever os dados.

# Ajustar o modelo GARCH aos res�duos
modelo_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))

modelo_garch_fit <- ugarchfit(spec = modelo_garch, data = residuos)

# Realizar previs�es com o modelo GARCH
previsoes_garch <- ugarchforecast(modelo_garch_fit, n.ahead = 7)

# Converter as previs�es para a escala da s�rie temporal original
previsoes_escaladas <- previsoes_garch@forecast$seriesFor + tail(temp$temp, 1)

# Previs�es na escala da s�rie temporal original
prev_finais <- previsoes_escaladas[1:7]

prev_dia1 <- prev_finais[1]
prev_dia2 <- prev_finais[2]
prev_dia3 <- prev_finais[3]
prev_dia4 <- prev_finais[4]
prev_dia5 <- prev_finais[5]
prev_dia6 <- prev_finais[6]
prev_dia7 <- prev_finais[7]
