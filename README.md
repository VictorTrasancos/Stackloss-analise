# Análise de Regressão Linear Simples com R (Dataset Stack Loss)

Este repositório contém um script completo em linguagem R para a implementação e análise de um modelo de 
**Regressão Linear Simples** na biblioteca `stackloss` nativa do R.

## Sumário do Script

O código aborda as seguintes etapas de uma análise estatística de regressão:
1. **Análise Exploratória Inicial**: Resumo estatístico descritivo e visualização por boxplots individuais e gráficos de dispersão.
2. **Cálculo de Variabilidades e Coeficientes**: Obtenção manual de $S_{xx}$, $S_{yy}$, $S_{xy}$, além das estimativas dos parâmetros $\beta_0$, $\beta_1$ e da variância do erro $\sigma^2$.
3. **Análise de Resíduos**: Cálculo, tabela comparativa e construção de histogramas dos resíduos.
4. **Testes de Hipóteses e Intervalos de Confiança**: 
   - Testes t para os coeficientes $\beta_0$ e $\beta_1$.
   - Intervalos de confiança de 95% para $\beta_0$, $\beta_1$ e $\sigma^2$.
   - Intervalo de confiança para a esperança de $Y$ ($E(Y\vert{}X)$) e visualização gráfica com bandas de confiança.
   - Intervalo de predição para uma nova observação e respectivas bandas gráficas.
5. **Análise de Variância (ANOVA)**: Implementação manual da tabela ANOVA (Soma de Quadrados, Quadrados Médios, estatística $F$ de Snedecor e teste de hipótese).
6. **Validação com Funções Nativas (`lm`)**: Comparação dos resultados manuais com os obtidos pelas funções padrão do R (`lm`, `confint`, `predict`, `anova`).


##  Estrutura do Código

O fluxo principal do script executa as seguintes operações em R:

```R
# 1. Carregamento e exploração dos dados
data("stackloss")
dados <- stackloss
X <- dados$Air.Flow
Y <- dados$stack.loss

# 2. Ajuste do modelo (Manual vs lm)
beta1_hat <- Sxy / Sxx
beta0_hat <- Ybar - beta1_hat*Xbar
fit_lm <- lm(Y ~ X)

# 3. Intervalos de Confiança e Predição
# ... (cálculos e plotagens de E(Y|X) e Y_new)

# 4. Análise de Variância (ANOVA)
anova(fit_lm)
