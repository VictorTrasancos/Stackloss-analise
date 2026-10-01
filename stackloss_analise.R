rm(list=ls(all=TRUE))
   
data("stackloss")
dados <- stackloss

# X é a variável explicativa e Y a variável predita
X <- dados$Air.Flow
Y <- dados$stack.loss

# Resumo estatístico das quatro variáveis
summary(dados)


# Boxplots individuais para evitar comparação visual enganosa entre escalas diferentes
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

boxplot(dados$Air.Flow,
        main = "Fluxo de ar",
        ylab = "Air Flow",
        col = "lightblue")

boxplot(dados$Water.Temp,
        main = "Temperatura da água",
        ylab = "Water Temp",
        col = "lightgreen")

boxplot(dados$Acid.Conc.,
        main = "Concentração de ácido",
        ylab = "Acid Conc.",
        col = "khaki")

boxplot(dados$stack.loss,
        main = "Perda na chaminé",
        ylab = "stack.loss",
        col = "mistyrose")

par(mfrow = c(1, 1))


# Gráfico de dispersão entre as variáveis explicativa e predita
plot(X,Y,
     pch = 19,
     col = 'steelblue',
     xlab = 'Air Flow',
     ylab = 'Stack loss',
     main = 'Stack.loss em relação a Air Flow'
)

# Correlação linear entre X e Y
cor(X,Y)

# Cálculo de Sxx,Sxy e Syy
n <- nrow(dados)
Xbar <- mean(X)
Ybar <- mean(Y)

# Sxx mede a variabilidade total da variável X
Sxx <- sum(X^2) - n*Xbar^2

# Syy mede a variabilidade total da variável Y
Syy <- sum(Y^2) - n*Ybar^2

# Sxy é a medida de associação linear entre X e Y
Sxy <- sum((X-Xbar)*Y)

cat('Sxx =', Sxx, '\n')
cat('Syy =', Syy, '\n')
cat('Sxy =', Sxy)


# Lembrando:
# beta1 = Sxy / Sxx
# beta0 = Ybar - beta1*Xbar
# sigma2 = SQres / (n - 2)

# Estimativas de beta0 e beta1
beta1_hat <- Sxy / Sxx
beta0_hat <- Ybar - beta1_hat*Xbar

# Estimativa de sigma2 (estimador não viesado)
# A soma de quadrados dos resíduos pode ser calculada direto com Syy e Sxy
SQres <- Syy - beta1_hat*Sxy
sigma2_hat <- SQres / (n - 2)

cat('beta0 =', beta0_hat, '\n')
cat('beta1 =', beta1_hat, '\n')
cat('sigma2 =', sigma2_hat, '\n')

# Conferindo com a função lm
fit <- lm(Y ~ X)
fit$coefficients

cat('Y_hat =', round(beta0_hat,4), '+', round(beta1_hat,4), '* X' )


# Gráfico de dispersão com a reta ajustada
plot(X, Y,
     pch = 19,
     col = "steelblue",
     xlab = "Fluxo de ar ",
     ylab = "Perda na chaminé ",
     main = bquote("Reta ajustada com " ~ hat(beta)[0] == .(round(beta0_hat, 4)) ~ " e " ~ hat(beta)[1] == .(round(beta1_hat, 4))))
curve(beta0_hat + beta1_hat*x, add = T, col = "red", lwd = 2)


# Cálculo dos resíduos no modelo
y_hat <- beta0_hat + beta1_hat*X
residuos <- Y - y_hat

tabela_residuos <- data.frame(Air.Flow = X,
                              stack.loss = Y,
                              y_hat = round(y_hat, 3),
                              residuo = round(residuos, 3))
tabela_residuos

hist(residuos,
     main = "Histograma dos resíduos",
     xlab = "Resíduos",
     col = "lightblue",
     border = "black",
     breaks = 8)

# Como os resíduos se aproximam de zero, o modelo consegue prever valores próximos
# aos observados


# TESTE DE HIPÓTESES 

# Teste para B0
dp_beta0_hat <- sqrt( sigma2_hat *( (1/n) + (mean(X))^2/Sxx ))
t0_beta0 <- beta0_hat/dp_beta0_hat
dp_beta0_hat
#Rejeitamos H0 se t0_beta0 < t1 ou t0_beta0 > t2

alpha <- 0.05
t1 <- qt(alpha/2,n-2)
t2 <- qt(1-alpha/2,n-2)

if(t0_beta0 < t1 || t0_beta0>t2){
  cat("Rejeita-se H0")
} else {
  cat("Não se rejeita H0")
}


# --- Intervalo de confiança para β₀ ---
#O valor do parâmetro β₀ apenas nos mostra uma faixa de valores possíveis 
#para o valor esperado da resposta(stack.loss) quando a variável preditiva 
#for nula 
b0_min <- beta0_hat - t2*dp_beta0_hat
b0_max <- beta0_hat + t2*dp_beta0_hat
IC_beta0 <- cbind(b0_min, b0_max)
cat('Intervalo de confiança (95%) para β₀')
print(IC_beta0)

# --- Intervalo de confiança para β₁ ---
#Para cada aumento em uma unidade de fluxo de ar 
#de resfriamento(stack.loss), a perda na chaminé 
#aumenta dentro dos limites do intervalo de confiança, com 95% de confiança.
b1_min <- beta1_hat - t2*dp_beta1_hat
b1_max <- beta1_hat + t2*dp_beta1_hat
IC_beta1 <- cbind(b1_min, b1_max)
cat('Intervalo de confiança (95%)  para β₁')
print(IC_beta1)


# --- Intervalo de confiança para σ² ---
#O σ² quantifica a forma como os pontos se dispersam ao redor da reta. 
#Valores maiores para seu intervalo de confiança descrevem uma maior 
#flutuação nos dados
t1_sig <- qchisq(alpha/2, n-2)
t2_sig <- qchisq(1-alpha/2,n-2)

sig_min <- SQres/t2_sig
sig_max <- SQres/t1_sig

IC_sigma2 <- cbind(sig_min, sig_max)
cat('Intervalo de confiança (95%) para σ²')
print(IC_sigma2)

# Cálculo do intervalo de confiança para esperança de Y
x1 <- X 
X0 <- Xbar

v_medio <- beta0_hat + beta1_hat*X0
dp_v_medio <- sqrt(sigma2_hat*(1/n + (X0 - mean(x1))^2/Sxx ))

v_medio_min <- v_medio - t2*dp_v_medio
v_medio_max <- v_medio - t1*dp_v_medio

IC_v_medio <- cbind(v_medio_min, v_medio_max)
print(IC_v_medio)

# Gráfico com intervalo de confiança para esperança de Y
x_seq <- seq(min(x1), max(x1), length.out = 100)
vs <- beta0_hat + beta1_hat*x_seq
dps <- sqrt(sigma2_hat*(1/n + (x_seq - mean(x1))^2/Sxx ))
vs_min <- vs - t2*dps
vs_max <- vs - t1*dps

plot(x1,
     y = Y,
     main = bquote("Intervalo de confiança para E(X|Y)"),
     xlab = "Fluxo de ar", ylab = "Perda")
curve(beta0_hat + beta1_hat*x, add = T, col = 'red')
lines(x_seq, vs_min, col = "red", lty = 2)
lines(x_seq, vs_max, col = "red", lty = 2)

# Cálculo do intervalo de predição para uma nova observação
x1_new <- 4.5
Y0_hat <- beta0_hat + beta1_hat*x1_new

dp_Y0_hat <- sqrt(sigma2_hat*(1 + 1/n + (x1_new - mean(x1))^2/Sxx ))

Y0_hat_min <- Y0_hat - t2*dp_Y0_hat
Y0_hat_max <- Y0_hat - t1*dp_Y0_hat

IC_Y0_hat <- cbind(Y0_hat_min, Y0_hat_max)
print(IC_Y0_hat)

# Gráfico com intervalo de predição para Y0
dps_ <- sqrt(sigma2_hat*(1 + 1/n + (x_seq - mean(x1))^2/Sxx ))
vs_min_ <- vs - t2*dps_
vs_max_ <- vs - t1*dps_

plot(x1,
     y = Y,
     main = bquote("Intervalo de predição para " ~ hat(Y)[0]),
     xlab = "Fluxo de ar", ylab = "Perda")
curve(beta0_hat + beta1_hat*x, add = T, col = 'red')
lines(x_seq, vs_min_, col = "red", lty = 2)
lines(x_seq, vs_max_, col = "red", lty = 2)

# Análise de variância (ANOVA manual)
SQreg <- beta1_hat*Sxy
SQtot <- Syy

SQreg
SQres
SQtot

QMreg = SQreg/1
QMres = SQres/(n-2)
F0 <- QMreg/QMres 

f1 <- pf(F0, df1 = 1, df2 = n-2, lower.tail = F)

if(F0 > f1){
  cat("Rejeita-se H0\n")
}

##### Ajuste do mesmo modelo de regressão linear simples usando a função lm #####
fit_lm <- lm(Y ~ X)
summary(fit_lm)

plot(x1,
     y = Y,
     main = bquote("Intervalo de predição para " ~ hat(Y)[0]),
     xlab = "Fluxo de ar", ylab = "Perda na chaminé")
curve(round(fit_lm$coef[[1]],4) + round(fit_lm$coef[[2]],4)*x, add = T, col = 'red')

# Coeficientes estimados
fit_lm$coefficients

# Valores preditos pelo modelo 
y_fitted <- fit_lm$fitted.values

# Comparação
cbind(y_fitted, y_hat)

# Residuos
residuos_lm <- fit_lm$residuals

# Comparação
cbind(residuos_lm, residuos)

# Intervalos de confiança para beta0 e beta1
confint(fit_lm, level = 1-alpha)

# Intervalos de confiança para a resposta média
predict(fit_lm, interval="confidence")

# Análise de variância via R
anova(fit_lm)

# Valores extraídos da tabela ANOVA
SQreg_lm <- anova(fit_lm)[1,2]
SQres_lm <- anova(fit_lm)[2,2]
QMreg_lm <- anova(fit_lm)[1,3]
QMres_lm <- anova(fit_lm)[2,3]
F_stat_lm <- anova(fit_lm)[,4]

cat("SQreg (lm):", SQreg_lm, "\n")
cat("SQres (lm):", SQres_lm, "\n")
cat("Estatística F:", F_stat_lm[1], "\n")

