# ---- Bibliotecas ----
library(astsa)
library(forecast)
library(tseries)
library(FinTS)
library(quantmod)
library(dplyr)
library(tidyverse)
library(rugarch)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)


# ---- Parte 1 ----

#### Descrição Dataset Hor #####

hor
help(hor)

# Taxa trimestral de ocupação de hotéis no Havaí (percentual de quartos 
# ocupados) do primeiro trimestre de 1982 (1982-I) ao quarto trimestre de 
# 2015 (2015-IV).

head(hor)
#Visualizar os primeiros valores da série temporal

summary(hor)
#Resumo estatístico da série

str(hor)
#Estrutura e classe dos dados

#### Ponto 2 ####

plot(hor, main = "Taxa de Ocupação de Hotéis no Havaí", 
     ylab = "Taxa de Ocupação (%)", 
     xlab = "Tempo")

# Observa-se que há tendências ao longo da série temporal. A taxa de ocupação 
# dos hotéis flutua, mas há períodos de aumento ou diminuição gradual.
# Por exemplo, há um aumento nos primeiros anos (1982 a 1990), uma queda entre
# 1990 e 2002, seguido de um crescimento até perto de 2008, depois veio a crise
# de 2008 houve uma queda e de seguida uma recuperação gradual após 2010.

#### Ponto 3 ####

# ACF e PACF para a série hor
acf(hor, main = "Função de Autocorrelação (ACF)") 

# O gráfico da ACF mostra picos significativos nos primeiros lags, mas não há 
# um padrão claro de repetição regular em múltiplos da frequência. 
# Isso sugere que, até este ponto, não há uma sazonalidade evidente que se 
# repita de forma consistente.

#Como a frequência da série é trimestral (4 observações por ano), esperávamos 
# observar picos significativos nos lags múltiplos de 4. No entanto, o gráfico 
# da ACF não apresenta picos claros nesses lags, o que indica a ausência de uma 
# sazonalidade trimestral clara.

#A análise da função de autocorrelação não sugere a presença de sazonalidade 
# na série temporal. Se a série possuir sazonalidade, ela não é suficientemente 
# forte para se refletir na função de autocorrelação de forma significativa.

pacf(hor, main = "Função de Autocorrelação Parcial (PACF)")  

#O gráfico da PACF mostra picos significativos nos primeiros lags. Contudo, os 
# lags posteriores apresentam valores muito próximos de zero e não há outros 
# picos significativos.

# Se existisse uma sazonalidade clara, esperávamos identificar picos 
# significativos no lag correspondente à frequência da série (neste caso, no 
# lag 5, devido à periodicidade trimestral).No entanto, o lag 5 não apresenta 
# um pico significativo, reforçando a ideia de que não há uma componente sazonal 
# evidente na série.

# Concluindo a análise da função de autocorrelação parcial (PACF) também não 
# sugere a existência de sazonalidade na série. Os picos nos primeiros lags 
# refletem correlações de curto prazo, mas não há padrões sazonais claros em 
# múltiplos da frequência da série (trimestral).

#### Ponto 4 ####

# Verificar a frequência e o período da série
frequency(hor)

# Média Movel Ordem 2 
MA2=stats::filter(hor, rep(1/2, 2))
plot(MA2)

# Média Movel Ordem 3
MA3=stats::filter(hor, rep(1/3, 3))
plot(MA3)

# Média Movel Ordem 4
MA4=stats::filter(hor, rep(1/4, 4))
plot(MA4)

# Média Movel Ordem 5
MA5=stats::filter(hor, rep(1/5, 5))
plot(MA5)

# Média Movel Ordem 6
MA6=stats::filter(hor, rep(1/6, 6))
plot(MA6)

# Junção das 6 séries temporais para obter um só gráfico:

Juntar6series=data.frame(hor,MA2,MA3,MA4,MA5,MA6)  # Matriz dos dados com as 6 séries por colunas
Juntar6series

TSMM23456=ts(Juntar6series,frequency=4)  # Construção da série temporal
TSMM23456

plot(TSMM23456,xlab="Ano")  # Gráfico com as 6 séries em separado

# Ao analisar os gráficos das médias móveis, concluímos que as médias móveis de 
# números ímpares (como MA3 e MA5) não são as mais adequadas para esta série 
# temporal. Isso ocorre porque as linhas resultantes apresentam muitas oscilações 
# abruptas, comprometendo a suavização dos dados. Essa característica é evidente 
# nos gráficos, onde a variabilidade permanece elevada, dificultando a 
# identificação de tendências mais claras.

# Além disso, considerando que a frequência da série temporal é trimestral 
# (ou seja, 4 observações por ano), optamos por adotar a média móvel de número 
# par, especificamente a de valor 4 (MA4). Essa escolha é justificada porque 
# uma média móvel de número igual à frequência da série temporal oferece uma 
# suavização mais equilibrada, respeitando a estrutura cíclica dos dados e 
# proporcionando uma melhor representação da tendência subjacente.

# Assim, concluímos que a média móvel de ordem 4 é a mais apropriada 
# para esta análise.

#### Ponto 5 ####

help(holt)

hor.HE <- holt(hor, h = 12)
hor.HE
fitted(hor.HE)
summary(hor.HE)

# Smoothing parameters: alpha = 0.2631 ; beta  = 1e-04 

plot(hor.HE, xlab = "Tempo", ylab = "Taxa de Ocupação (%)", lwd = 2)

# Equações de suavização:

# Equação de nível
# lt=0.2631xt+(1-0.2631)(lt-1 + Tt-1)

# Equação da tendência 
# Tt=0.0001(lt - lt-1)+(1-0.0001)Tt-1

# Equação de previsão
#




#### Ponto 6 ####

# Decomposição Aditiva, Gráfico e Valores
hor_aditiva <- decompose(hor)

print(hor_aditiva$trend)
print(hor_aditiva$seasonal)
print(hor_aditiva$random)

plot(hor_aditiva)

# Decomposição Multiplicativa, Gráfico e Valores
hor_multiplicativa <- decompose(hor, type = "multiplicative")

print(hor_multiplicativa$trend)
print(hor_multiplicativa$seasonal)
print(hor_multiplicativa$random)

plot(hor_multiplicativa)

#### Descrição Dataset Hycarb ####

help(lap)

# A série temporal hycarb consiste em uma série que registra 
# a concentração de hidrocarbonetos (Hycarb) ao longo do tempo semanalmente. 
# O dataset captura as variações nas concentrações. 
# Tem início na primeira semana de 1970 e vai até a semana 40 do ano de 1979.

hycarb<- lap[,"hycarb"]
hycarb

#### Sobre Percepções do gráfico ####
plot.ts(hycarb,
        main = "Concentração de hidrocarbonetos entre 1970 e 1979",
        ylab = "Concentração de hidrocarbonetos")

# É possível observar uma componente sazonal na série temporal, caracterizada 
# pela repetição de padrões aparentemente ano a ano. Dado que a série possui uma 
# frequência semanal, essa sazonalidade apresenta um período 
# próximo de 52 semanas.
# 
# Esta série não apresenta uma tendência clara ao longo do tempo, contudo  
# nota-se a presença de um possível ciclo entre os anos de 1970-1971 
# e entre 1977-1978, com picos significativos de concentração em comparação com
# os demais períodos da série.
# Além disso, observa-se uma variação fora do padrão, sugerindo 
# possíveis fatores externos ou fenômenos atípicos influenciando a série 
# nesses intervalos específicos.

par(mfrow = c(1,2))
Acf(hycarb, lag.max = 52*4)
Pacf(hycarb, lag.max = 52*4)
par(mfrow = c(1,1))

# Apartir da avaliação dos graficos PACF e ACF aparentemente a série não é 
# estacionária, logo possívelmente essa componente deverá ser trabalhada para 
# a composição de um modelo SARIMA.
# A avaliação nos leva a crer também que a série temporal não possui uma 
# tendência clara ou minimamente pode possuir uma tendencia de longo prazo.
# Contudo fica clara a existencia de uma 
# sazonalidade de lag 52, logo um sazonalidade ano a ano.

#### Método de Holt-Winters ####

hwM_hycarb <- HoltWinters(hycarb, seasonal = "multiplicative")
hwA_hycarb <- HoltWinters(hycarb, seasonal = "additive")

hwM_hycarb
hwA_hycarb

#Mudar o gráfico ver se com um apenas ficaria melhor
hwM_hycarb_f = forecast(hwM_hycarb, h = 12)
hwA_hycarb_f = forecast(hwA_hycarb, h = 12)

par(mfrow = c(2,1))
plot(hwM_hycarb_f, main = "Previsões e Intervalos de Confiança: Modelo Holt-Winters Multiplicativo")
plot(hwA_hycarb_f, main = "Previsões e Intervalos de Confiança: Modelo Holt-Winters Aditivo")
par(mfrow = c(1,1))

summary(hwM_hycarb_f)
# ME        *RMSE    *MAE       MPE     *MAPE    MASE       ACF1
# 0.0118323 11.98449 9.072858 -4.004883 18.8055 0.7575681 -0.1188775
summary(hwA_hycarb_f)
# ME        *RMSE    *MAE       MPE     *MAPE    MASE       ACF1
# 0.5420742 12.28347 9.246977 -2.739975 19.04204 0.7721067 -0.1211642

# Após avaliação dos métodos aditivo e multiplicativo do modelo HoltWinters
# vemos que o modelo multiplicativo seria o mais adequado. 
# Essa decisão, apesar de nãos ser clara com base nos gráficos, fica
# mais evidente uma vez que podemos comparar as medidas de erro de 
# cada um dos modelos em que temos que os valores RMSE, MAE e MAPE são menores
# para o modelo Multiplicativo em comparação com o aditivo.

#### Criação do modelo SARIMA ####

#****************** 1.º PASSO *********************************
# A série temporal apresenta outliers como é possível ver nos valores e gráficos
# abaixo contudo, por serem apenas dois valores em 508, logo 0,4% dos valores, o 
# mesmo não deve ser um problema para os modelos. 

boxplot(hycarb, main = "Estudo de possíveis outliers")
outliers <- boxplot.stats(hycarb)
outliers$out
# 100.12 e 91.47

#****************** 2.º PASSO *********************************
# Dado que a variancia ao longo da série, apesar de ligeiramente alta,
# parecer constante não há necessidade, inicialmente, de se logaritmizar 
# os dados

#****************** 3.º PASSO *********************************
# Como mencionado anteriormente, a série não parece ser estácionária
# logo, vamos realizar os testes ADF e KPSS para confirmar.

# Assumindo que:
# Hipótese nula: A série temporal não é estacionária.
# Hipótese alternativa: A série temporal é estacionária

adf.test(hycarb)
# p-value = 0.01097

# Dado que temos p-value = 0.01097 < 0,05, logo rejeita-se a hipotese nula, pois 
# há evidência estatistica de que a série seja estacionária

# Assumindo que:
# Hipótese nula: A série temporal é estacionária 
# Hipótese alternativa: A série temporal não é estacionária

kpss.test(hycarb)
# Dado que temos p-value = 0.02172 < 0,05 devemos rejeitar a hipótese nula, pois
# há evidencia estatística de que a série não seja estacionária

# Os testes de hipóteses fornecem evidências conflitantes 
# sobre a estacionaridade da série, isso pode ser devido a existencia de 
# uma tendencia a longo prazo ou devido a sazonalidade existente. 
# Logo deve-se aplicar uma diferenciação para avaliar o melhor modelo SARIMA
# por padrão vamos começar pela diferenciação sazonal, pois a mesma já pode 
# definir e ajustar o modelo.

nsdiffs(hycarb)

# A função apresentou que não é necessária aplicar nenhuma diferenciação sazonal
# para tornar a série estacionária.

ndiffs(hycarb)

# A função apresentou que é necessária aplicar pelo menos 1 diferenciação 
# ordinária para tornar a série estacionária.

hycarb_diff <- diff(hycarb, lag = 1, diff = 1)

adf.test(hycarb_diff)

kpss.test(hycarb_diff)

# Dado que para o teste ADF o valor de p-value foi menor que 0.01 e no teste 
# KPSS o valor de p-value foi maior que 0.1, podemos dizer que há 
# evidencia estatística de que o modelo é estacionário.

hycarb_est <- (hycarb_diff)
plot(hycarb_est, 
     main = "Série Temporal Diferenciada (Estacionária)", 
     xlab = "Tempo (Semanas)",  
     ylab = "Valores Diferenciados")  
     
     
# Componente diferenciação ordinária(d) = 1
# Componente diferenciação sazonal(D) = 0

#****************** 4.º PASSO *********************************
par(mfrow=c(2,1))
acfEvaluation <- Acf(hycarb_est, lag.max = 4*52)
possibleAcf <- which(acfEvaluation$acf > 0.09 | 
                    acfEvaluation$acf < -0.09)
lagsAcf <- acfEvaluation$lag[possibleAcf]
abline(v = lagsAcf, col = "red", lty = 3)
possibleAcf

# Lags Significativos
# 1,2,3,17,40,41,42,43,80,81,110,146,159,167,168,173

pacfEvaluation <- Pacf(hycarb_est, lag.max = 4*52)
possiblePacf <- which(pacfEvaluation$acf > 0.09 | 
                    pacfEvaluation$acf < -0.09)
lagsPacf <- pacfEvaluation$lag[possiblePacf]
abline(v = lagsPacf, col = "red", lty = 3)
possiblePacf
par(mfrow=c(1,1))

# Lags Significativos
# 1,2,3,4,5,10,17,22,30,33,39,41,79,157

# Possíveis Componentes AR(p) = 0,1,2,3,4,5
# Dada a avaliação dos lags significativos temos que os valores mencionados acima 
# sobre a avaliação da Pacf representam possíveis valores para se chegar a um 
# modelo SARIMA, demais lags significativos apresentam apenas uma repetição
# sazonal referente ao modelo.

# Possíveis Componentes MA(q) =  0,1,2
# Dada a avaliação dos lags significativos temos que os valores mencionados acima 
# sobre a avaliação da Acf representam possíveis valores para se chegar a um 
# modelo SARIMA, demais lags significativos apresentam apenas uma repetição
# sazonal referente ao modelo.

# Possíveis Componentes SAR(P) = 1,2
# Aparentemente há possíveis padrões de repetição ao longo dos lags pela 
# avaliação da Pacf, possivelmente a cada 38 lags, o que seria aproximadamente
# a cada 3 trimestres.

# Possíveis Componentes MA(Q) = 0
# Aparentemente não há padrões claros de repetição ao longo dos lags pela 
# avaliação da Acf.

#****************** 5.º PASSO *********************************
# Com isso temos os possíveis modelos para um SARIMA:

p<- c(0,1,2,3,4,5)
d<- c(1)
q<- c(0,1,2)
P<- c(1,2)
D<- c(0)
Q<- c(0)

possibleArima <- list()

for (p_val in p) {
  for (q_val in q) {
    for (d_val in d) {
      for (P_val in P) {
        for (Q_val in Q) {
          for (D_val in D) {

            modelo <- tryCatch({
              arima(hycarb, 
                    order = c(p_val, d_val, q_val), 
                    seasonal = list(order = c(P_val, D_val, Q_val), period = 52))
            }, error = function(e) NULL)  # Em caso de erro, retorna NULL
            
            key <- paste("p", p_val, "d", d_val, "q", q_val, 
                         "P", P_val, "D", D_val, "Q", Q_val, sep = "_")
            
            if (!is.null(modelo)) {
              possibleArima[[key]] <- modelo
            }
          }
        }
      }
    }
  }
}

# Comparativo entre modelos para validação de melhor modelo

resultArima <- data.frame(
  ModelName = character(),  # Nome do modelo
  AIC = numeric(),          # Valor do AIC
  stringsAsFactors = FALSE  # Evitar fatores para as strings
)

# Iterar pelos modelos em `possibleArima` e salvar os AICs
for (key in names(possibleArima)) {
  model <- possibleArima[[key]]
  
  # Verificar se o modelo possui o atributo AIC
  if (!is.null(model$aic)) {
    resultArima <- rbind(resultArima, data.frame(ModelName = key, AIC = model$aic))
  } else {
    warning(paste("Modelo", key, "não possui atributo AIC. Ignorado."))
  }
}

# Ordenar os resultados pelo valor de AIC
resultArima <- resultArima[order(resultArima$AIC), ]

# Selecionar os 3 melhores modelos
top3 <- head(resultArima, 3)

# Exibir os resultados
print(top3)

arima.512.200.52 <- possibleArima$p_5_d_1_q_2_P_2_D_0_Q_0
arima.512.100.52 <- possibleArima$p_5_d_1_q_2_P_1_D_0_Q_0
arima.012.200.52 <- possibleArima$p_0_d_1_q_2_P_2_D_0_Q_0

calculoBicAicc <- function(k,n,aic){
  bic <- aic + (log(n) - 2) * k
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  cat("BIC: ",bic,"\nAICc :",aicc)
  return()
}

arima.512.200.52
calculoBicAicc(length(arima.512.200.52$coef),
               length(hycarb),
               arima.512.200.52$aic
               )
# BIC:  3935.174 
# AICc : 3897.46
arima.512.100.52
calculoBicAicc(length(arima.512.100.52$coef),
               length(hycarb),
               arima.512.100.52$aic
              )
# BIC:  3932.106 
# AICc : 3898.551
arima.012.200.52
calculoBicAicc(length(arima.012.200.52$coef),
               length(hycarb),
               arima.012.200.52$aic
              )
# BIC:  3928.434 
# AICc : 3903.218

#****************** 6.º PASSO *********************************
Acf(arima.512.200.52$residuals)

# Tese dos resíduos para validação do modelo SARIMA através do teste de 
# hipótese Ljung-box em que assumimos:
#  Hipótese Nula: Residuos não possuem autocorrelação
#  Hipótese Alternativa: Residuos possuem autocorrelação

Box.test(arima.512.200.52$residuals,lag = 4, type="Ljung-Box")

# Dado o p-value = 0.9996 > alfa  não há evidência estatística para rejeitar a 
# hipótese nula de que os resíduos são independentes.

# Dado os melhores valores apresentados para o critérios AIC, AICc e BIC temos
# que o melhor modelo encontrado seria o modelo ARIMA(5,1,2)(2,0,0)[52],também 
# pode se ver isso com a representação gráfica e com a validação do teste Ljung-Box
# logo a partir deste modelo serão realizadas as previsões solicitadas

#****************** 7.º PASSO *********************************
hycarb_F12 <- forecast(arima.512.200.52, h=52)
plot(hycarb_F12)
hycarb_F12

#### Comparativo entre Modelo SARIMA e Holt-Winters Multiplicativo ####
par(mfrow = c(2,1))
plot(hwM_hycarb_f)
plot(hycarb_F12)
par(mfrow = c(1,1))

hwM_hycarb_f
#           Forecast    Lo 95     Hi 95
# 1979.769  60.63572 51.88674  69.38469
# 1979.788  51.77178 42.96823  60.57532
# 1979.808  67.55603 58.58809  76.52398
# 1979.827  53.64408 44.70894  62.57921

hycarb_F12
#           Forecast     Lo 95    Hi 95
# 1979.769  52.25895 30.586776 73.93113
# 1979.788  50.85177 29.095292 72.60825
# 1979.808  53.27660 30.750786 75.80241
# 1979.827  51.14459 28.156957 74.13223

summary(hwM_hycarb_f)
#                   ME     *RMSE      *MAE       MPE    *MAPE      MASE       ACF1
# Training set 0.0118323 11.98449 9.072858 -4.004883 18.8055 0.7575681 -0.1188775
summary(hycarb_F12)
#                   ME     *RMSE      *MAE       MPE    *MAPE     MASE       ACF1
# Training set 0.03509838 11.04655 8.645961 -3.99067 17.80713 0.721923 0.001806377

plot(hycarb)
lines(hwM_hycarb_f$upper[,"95%"],lty = 1, col = "red",lwd = 2)
lines(hycarb_F12$upper[,"95%"],lty = 1, col = "blue",lwd = 2)

# Período de previsão
horizonte_previsao <- 1:4

# Plot da série original
plot(hycarb, xlim = c(time(hycarb)[1], time(hycarb)[length(hycarb)] + length(horizonte_previsao)/52), 
     ylim = range(c(hycarb, hwM_hycarb_f$upper[, "95%"], hwM_hycarb_f$lower[, "95%"], 
                    hycarb_F12$upper[, "95%"], hycarb_F12$lower[, "95%"])),
     main = "Comparação de Previsões Intervalares", 
     ylab = "Valores", xlab = "Tempo", col = "black", lty = 1, type = "l", lwd = 2)

# Valores previstos Holt-Winters
lines(seq(time(hycarb)[length(hycarb)] + 1/52, by = 1/52, length.out = length(horizonte_previsao)), 
      hwM_hycarb_f$mean[horizonte_previsao], col = "red", lty = 1, lwd = 1)

# Valores previstos ARIMA
lines(seq(time(hycarb)[length(hycarb)] + 1/52, by = 1/52, length.out = length(horizonte_previsao)), 
      hycarb_F12$mean[horizonte_previsao], col = "blue", lty = 1, lwd = 2)

# Legenda
legend("topleft", 
       legend = c("Holt-Winters","ARIMA"), 
       col = c("red", "blue"), 
       lty = c(1, 1), 
       lwd = c(1,2))

# Apesar de os intervalos de previsão do modelo Holt-Winters serem mais 
# estreitos, o modelo ARIMA(5,1,2)(2,0,0)[52] tem um desempenho superior 
# nas métricas de precisão (RMSE, MAPE, MAE e MASE), além de apresentar 
# resíduos com menos autocorrelação. Isso sugere que o ARIMA oferece previsões 
# mais confiáveis em termos de precisão geral, mesmo com maior incerteza nos 
# intervalos. Portanto, o modelo ARIMA parece ser a melhor escolha considerando 
# a precisão do modelo.

# ---- Parte 2 ----
#### FordAdjusted ####

head(F)
summary(F)

#Procedimentos para inicialização dos dados, limpeza e primeira visualização
getSymbols('F',from = "2007-01-03",to = "2023-12-31")
fordAdjusted <- F$F.Adjusted

Retorno = CalculateReturns(fordAdjusted)

Retorno

#Ponto 2

plot(Retorno) 

#Períodos de maior volatibilidade serão inicíos de 2008 até inicíos de 2010 devido a crise 
#económica mundial da altura e em 2020 com o aparecimento do COVID-19 a nível mundial.

#Ponto 3

fordAdjusted.ARCHtest <- ArchTest(fordAdjusted, lags = 1, demean = TRUE)
fordAdjusted.ARCHtest

# Como p-value < 2.2e-16 < 0.05 logo rejeita-se H0, ou seja,
# conclui-se a presença significativa de efeitos ARCH(1).

#Ponto 4.a 

Retorno <- na.omit(Retorno)

#1 1 NORM
Modelo.spec.GARCH11.norm = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                                      distribution.model = "norm")
Modelo.spec.GARCH11.norm

Modelo.fit.GARCH11.norm = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH11.norm, out.sample=20)
Modelo.fit.GARCH11.norm

#1 2 NORM
Modelo.spec.GARCH12.norm = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), 
                                      distribution.model = "norm")
Modelo.spec.GARCH12.norm

Modelo.fit.GARCH12.norm = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH12.norm, out.sample=20)
Modelo.fit.GARCH12.norm

#2 1 NORM
Modelo.spec.GARCH21.norm = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), 
                                      distribution.model = "norm")
Modelo.spec.GARCH21.norm

Modelo.fit.GARCH21.norm = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH21.norm, out.sample=20)
Modelo.fit.GARCH21.norm

#2 2 NORM
Modelo.spec.GARCH22.norm = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), 
                                      distribution.model = "norm")
Modelo.spec.GARCH22.norm

Modelo.fit.GARCH22.norm = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH22.norm, out.sample=20)
Modelo.fit.GARCH22.norm

#1 1 STD
Modelo.spec.GARCH11.std = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                     variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                                     distribution.model = "std")
Modelo.spec.GARCH11.std

Modelo.fit.GARCH11.std = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH11.std, out.sample=20)
Modelo.fit.GARCH11.std

#1 2 STD
Modelo.spec.GARCH12.std = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                     variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), 
                                     distribution.model = "std")
Modelo.spec.GARCH12.std

Modelo.fit.GARCH12.std = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH12.std, out.sample=20)
Modelo.fit.GARCH12.std

#2 1 STD
Modelo.spec.GARCH21.std = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                     variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), 
                                     distribution.model = "std")
Modelo.spec.GARCH21.std

Modelo.fit.GARCH21.std = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH21.std, out.sample=20)
Modelo.fit.GARCH21.std

#2 2 STD
Modelo.spec.GARCH22.std = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                     variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), 
                                     distribution.model = "std")
Modelo.spec.GARCH22.std

Modelo.fit.GARCH22.std = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH22.std, out.sample=20)
Modelo.fit.GARCH22.std

#1 1 SSTD
Modelo.spec.GARCH11.sstd = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                                      distribution.model = "sstd")
Modelo.spec.GARCH11.sstd

Modelo.fit.GARCH11.sstd = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH11.sstd, out.sample=20)
Modelo.fit.GARCH11.sstd

#1 2 SSTD
Modelo.spec.GARCH12.sstd = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), 
                                      distribution.model = "sstd")
Modelo.spec.GARCH12.sstd

Modelo.fit.GARCH12.sstd = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH12.sstd, out.sample=20)
Modelo.fit.GARCH12.sstd

#2 1 SSTD
Modelo.spec.GARCH21.sstd = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), 
                                      distribution.model = "sstd")
Modelo.spec.GARCH21.sstd

Modelo.fit.GARCH21.sstd = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH21.sstd, out.sample=20)
Modelo.fit.GARCH21.sstd

#2 2 SSTD
Modelo.spec.GARCH22.sstd = ugarchspec(mean.model = list(armaOrder=c(0,0)), 
                                      variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), 
                                      distribution.model = "sstd")
Modelo.spec.GARCH22.sstd

Modelo.fit.GARCH22.sstd = ugarchfit(data = Retorno, spec = Modelo.spec.GARCH22.sstd, out.sample=20)
Modelo.fit.GARCH22.sstd

#Ponto 4.b

residuals <- residuals(Modelo.fit.GARCH12.sstd, standardize = TRUE)
hist(residuals, breaks = 30, main = "Histograma dos Resíduos Padronizados",
     xlab = "Resíduos Padronizados")
qqnorm(residuals, main = "QQ-Plot dos Resíduos Padronizados")

#Ponto 5

forecast.Modelo = ugarchforecast(fitORspec = Modelo.fit.GARCH12.sstd, n.ahead = 20)
forecast.Modelo

fitted(forecast.Modelo)    # Valores ajustados
plot(forecast.Modelo)


plot(fitted(forecast.Modelo))

sigma(forecast.Modelo)
plot(sigma(forecast.Modelo))

par(mfrow = c(1,2))
plot(forecast.Modelo, which = 1)
plot(forecast.Modelo, which = 3)
par(mfrow = c(1,1))

#### FordClose ####
#Procedimentos para inicialização dos dados, limpeza e primeira visualização
getSymbols('F',from = "2007-01-03",to = "2023-12-31")
fordClose <- F$F.Close
fordClose

plot(fordClose)

log_return <- diff(log(fordClose))
logReturnclean <- na.omit(log_return)

# Especificações de modelos EGARCH e GJR-GARCH para as ordens (1,1),(1,2)e(2,1)
spec_egarch_1_1 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                              mean.model = list(armaOrder = c(0, 0)),
                              distribution.model = "std")

spec_egarch_1_2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 2)),
                              mean.model = list(armaOrder = c(0, 0)),
                              distribution.model = "std")

spec_egarch_2_1 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2, 1)),
                              mean.model = list(armaOrder = c(0, 0)),
                              distribution.model = "std")

spec_gjr_1_1 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 0)),
                           distribution.model = "std")

spec_gjr_1_2 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 2)),
                           mean.model = list(armaOrder = c(0, 0)),
                           distribution.model = "std")

spec_gjr_2_1 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(2, 1)),
                           mean.model = list(armaOrder = c(0, 0)),
                           distribution.model = "std")

# Lista de especificações
specs <- list(spec_egarch_1_1, spec_egarch_1_2, spec_egarch_2_1,
              spec_gjr_1_1, spec_gjr_1_2, spec_gjr_2_1)

# Ajustar modelos e coletar resultados
resultGarch <- lapply(specs, function(spec) {
  modelo <- ugarchfit(spec = spec, data = logReturnclean)
  list(aic = infocriteria(modelo)["Akaike", ],
       bic = infocriteria(modelo)["Bayes", ],
       modelo = modelo)
})

# Criar uma tabela com os critérios de informação
tabela_criterios <- do.call(rbind, lapply(resultGarch, function(x) c(x$aic, x$bic)))
colnames(tabela_criterios) <- c("AIC", "BIC")
rownames(tabela_criterios) <- c("EGARCH(1,1)", "EGARCH(1,2)", "EGARCH(2,1)",
                                "GJR-GARCH(1,1)", "GJR-GARCH(1,2)", "GJR-GARCH(2,1)")

print(tabela_criterios)

par(mfrow = c(2, 3))
plot(resultGarch[[1]]$modelo, which = 8)  # EGARCH(1,1)
plot(resultGarch[[2]]$modelo, which = 8)  # EGARCH(1,2)
plot(resultGarch[[3]]$modelo, which = 8)  # EGARCH(2,1)
plot(resultGarch[[4]]$modelo, which = 8)  # GJR-GARCH(1,1)
plot(resultGarch[[5]]$modelo, which = 8)  # GJR-GARCH(1,2)
plot(resultGarch[[6]]$modelo, which = 8)  # GJR-GARCH(2,1)
par(mfrow = c(1, 1))

par(mfrow = c(2, 3))
plot(resultGarch[[1]]$modelo, which = 9)  # EGARCH(1,1)
plot(resultGarch[[2]]$modelo, which = 9)  # EGARCH(1,2)
plot(resultGarch[[3]]$modelo, which = 9)  # EGARCH(2,1)
plot(resultGarch[[4]]$modelo, which = 9)  # GJR-GARCH(1,1)
plot(resultGarch[[5]]$modelo, which = 9)  # GJR-GARCH(1,2)
plot(resultGarch[[6]]$modelo, which = 9)  # GJR-GARCH(2,1)
par(mfrow = c(1, 1))

# Selecionar o melhor modelo
bestModel <- resultGarch[[which.min(tabela_criterios[,"AIC"])]]
bestModel$modelo

# Information Criteria
# ------------------------------------
#   
# Akaike       -4.9522
# Bayes        -4.9403
# 
# Weighted Ljung-Box Test on Standardized Residuals
# ------------------------------------
#   statistic p-value
# Lag[1]                      2.747 0.09741
# Lag[2*(p+q)+(p+q)-1][2]     3.650 0.09365
# d.o.f=0
# H0 : No serial correlation
# 
# Weighted Ljung-Box Test on Standardized Squared Residuals
# ------------------------------------
#   statistic p-value
# Lag[1]                      0.0791  0.7785
# Lag[2*(p+q)+(p+q)-1][8]     1.6697  0.9062
# 
# Weighted ARCH LM Tests
# ------------------------------------
#   Statistic Shape Scale P-Value
# ARCH Lag[4]    0.9361 0.500 2.000  0.3333
# 
# Nyblom stability test
# ------------------------------------
#   Joint Statistic:  1.2084
# Individual Statistics:              
#   mu     0.05013
# omega  0.25488
# alpha1 0.06048
# alpha2 0.06792
# beta1  0.23940
# gamma1 0.20478
# gamma2 0.14058
# shape  0.42099
# 
# Sign Bias Test
# ------------------------------------
#   t-value   prob sig
# Sign Bias           0.3162 0.7519    
# Negative Sign Bias  1.0490 0.2942    
# Positive Sign Bias  0.3952 0.6927    
# Joint Effect        1.8626 0.6014    
# 
# 
# Adjusted Pearson Goodness-of-Fit Test:
#   ------------------------------------
#   group statistic p-value(g-1)
# 1    20     29.99     0.051974
# 2    30     52.48     0.004822
# 3    40     59.81     0.017629
# 4    50     74.10     0.011805

par(mfrow = c(1,2))
plot(bestModel$modelo, which = 8)
plot(bestModel$modelo, which = 9)
par(mfrow = c(1,1))
