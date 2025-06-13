library(astsa)
library(forecast)

hor




#                                          Ponto 1

help(hor)

#Taxa trimestral de ocupação de hotéis no Havaí (percentual de quartos ocupados) do primeiro trimestre 
#de 1982 (1982-I) ao quarto trimestre de 2015 (2015-IV).

head(hor)
#Visualizar os primeiros valores da série temporal

summary(hor)
#Resumo estatístico da série

str(hor)
#Estrutura e classe dos dados




#                                          Ponto 2

plot(hor, main = "Taxa de Ocupação de Hotéis no Havaí", ylab = "Taxa de Ocupação (%)", xlab = "Tempo")

#Observa-se que há tendências ao longo da série temporal. A taxa de ocupação dos hotéis flutua, mas há 
#períodos de aumento ou diminuição gradual.
#Por exemplo, há um aumento nos primeiros anos (1982 a 1990), uma queda entre 1990 e 2002, seguido 
#de um crescimento até perto de 2008, depois veio a crise de 2008 houve uma queda e de seguida uma 
#recuperação gradual após 2010.




#                                          Ponto 3

#ACF e PACF para a série hor
acf(hor, main = "Função de Autocorrelação (ACF)") 

#O gráfico da ACF mostra picos significativos nos primeiros lags, mas não há um padrão claro de 
#repetição regular em múltiplos da frequência. Isso sugere que, até este ponto, não há uma 
#sazonalidade evidente que se repita de forma consistente.
  
#Como a frequência da série é trimestral (4 observações por ano), esperávamos observar picos 
#significativos nos lags múltiplos de 4. No entanto, o gráfico da ACF não apresenta picos claros 
#nesses lags, o que indica a ausência de uma sazonalidade trimestral clara.

#A análise da função de autocorrelação não sugere a presença de sazonalidade na série temporal.
#Se a série possuir sazonalidade, ela não é suficientemente forte para se refletir na função de 
#autocorrelação de forma significativa.

pacf(hor, main = "Função de Autocorrelação Parcial (PACF)")  

#O gráfico da PACF mostra picos significativos nos primeiros lags.
#ontudo, os lags posteriores apresentam valores muito próximos de zero e não há outros picos 
#significativos.
  
#Se existisse uma sazonalidade clara, esperávamos identificar picos significativos no lag 
#correspondente à frequência da série (neste caso, no lag 5, devido à periodicidade trimestral).
#No entanto, o lag 5 não apresenta um pico significativo, reforçando a ideia de que não há uma 
#componente sazonal evidente na série.

#Concluindo a análise da função de autocorrelação parcial (PACF) também não sugere a existência de 
#sazonalidade na série.
#Os picos nos primeiros lags refletem correlações de curto prazo, mas não há padrões sazonais claros em múltiplos da frequência da série (trimestral).




#                                          Ponto 4

# Verificar a frequência e o período da série
frequency(hor)

#Média Movel Ordem 2 
MA2=filter(hor, rep(1/2, 2))
plot(MA2)

#Média Movel Ordem 3
MA3=filter(hor, rep(1/3, 3))
plot(MA3)

#Média Movel Ordem 4
MA4=filter(hor, rep(1/4, 4))
plot(MA4)

#Média Movel Ordem 5
MA5=filter(hor, rep(1/5, 5))
plot(MA5)

#Média Movel Ordem 6
MA6=filter(hor, rep(1/6, 6))
plot(MA6)

# Junção das 6 séries temporais para obter um só gráfico:

Juntar6series=data.frame(hor,MA2,MA3,MA4,MA5,MA6)  # Matriz dos dados com as 6 séries por colunas
Juntar6series

TSMM23456=ts(Juntar6series,frequency=4)  # Construção da série temporal
TSMM23456

plot(TSMM23456,xlab="Ano")  # Gráfico com as 6 séries em separado

#Ao analisar os gráficos das médias móveis, concluímos que as médias móveis de números ímpares (como 
#MA3 e MA5) não são as mais adequadas para esta série temporal. Isso ocorre porque as linhas 
#resultantes apresentam muitas oscilações abruptas, comprometendo a suavização dos dados. Essa 
#característica é evidente nos gráficos, onde a variabilidade permanece elevada, dificultando a 
#identificação de tendências mais claras.

#Além disso, considerando que a frequência da série temporal é trimestral (ou seja, 4 observações 
#por ano), optamos por adotar a média móvel de número par, especificamente a de valor 4 (MA4). 
#Essa escolha é justificada porque uma média móvel de número igual à frequência da série temporal 
#oferece uma suavização mais equilibrada, respeitando a estrutura cíclica dos dados e proporcionando 
#uma melhor representação da tendência subjacente.

#Assim, concluímos que a média móvel de ordem 4 é a mais apropriada para esta análise.




#                                          Ponto 5

help(holt)

hor.HE <- holt(hor, h = 12)
hor.HE
fitted(hor.HE)
summary(hor.HE)

# Smoothing parameters: alpha = 0.2631 ; beta  = 1e-04 

plot(hor.HE, xlab = "Tempo", ylab = "Taxa de Ocupação (%)", lwd = 2)

#Equações de suavização:

#Equação de nível
#lt=0.2631xt+(1-0.2631)(lt-1 + Tt-1)

#Equação da tendência 
#Tt=0.0001(lt - lt-1)+(1-0.0001)Tt-1

#Equação de previsão
#




#                                          Ponto 6

#Decomposição Aditiva, Gráfico e Valores
hor_aditiva <- decompose(hor)

print(hor_aditiva$trend)
print(hor_aditiva$seasonal)
print(hor_aditiva$random)

plot(hor_aditiva)

#Decomposição Multiplicativa, Gráfico e Valores
hor_multiplicativa <- decompose(hor, type = "multiplicative")

print(hor_multiplicativa$trend)
print(hor_multiplicativa$seasonal)
print(hor_multiplicativa$random)

plot(hor_multiplicativa)
