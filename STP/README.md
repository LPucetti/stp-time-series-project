# Projeto de Séries Temporais (STP)

Este repositório contém o projeto desenvolvido para a unidade curricular de **Séries Temporais** do Mestrado em Ciência dos Dados (IPLeiria), realizado por **Leonardo Nogueira Pucetti** e **Daniel Filipe**.

## 📌 Objetivo

Aplicar técnicas de modelagem de séries temporais para análise, decomposição e previsão de dados reais. O foco do trabalho foi compreender os padrões de tendência, sazonalidade e ruído e propor modelos preditivos robustos com base em ferramentas estatísticas.

## 🧪 Técnicas Utilizadas

- Análise de autocorrelação (ACF) e autocorrelação parcial (PACF)
- Decomposição sazonal
- Modelos de suavização exponencial (Holt-Winters)
- Modelos de regressão com efeitos fixos e aleatórios
- Previsão com intervalos de confiança
- Visualização de resultados com gráficos gerados no R

## ▶️ Como Executar

1. Clonar o repositório:

```bash
git clone https://github.com/LPucetti/stp-time-series-project.git
	
2. Abrir o script `LeonardoNogueira.R` no RStudio

3. Certificar de ter as seguintes bibliotecas instaladas 
install.packages(c("forecast", "tseries", "ggplot2", "zoo"))

4. Executar os blocos de código conforme comentado no script

Estrutura projeto 

STP/
├── LeonardoNogueira.R
├── ProjetoFinalSTP.R
├── data/
│   └── LeonardoNogueira.RData
├── images/
│   ├── ACF_PACF.png
│   └── Rplot01.png
├── report/
│   ├── Trabalho_LeonardoNogueira.pdf
│   └── Trabalho Escrito - STP - MCD - 2024_2025 - VFinal.pdf
