rm(list=ls())
#### Codigo limpo
library(ggplot2)
library(tidyr)
library(dplyr)
library(Metrics)
library(tidyverse)
library(caret)

# Definir novo diretório
#setwd("C:/Users/Geovane/Documents/Projeto")
#getwd()

###dados
dados <- read.csv("C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/amostras_serrinha_kernel_3x3_mediana.csv",
                  sep =';',
                  dec ='.',
                  header = TRUE)
#divisão 70/30
amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))

treino <- dados[amostras, ]
teste  <- dados[-amostras, ]
# avaliar o modelos

avaliar_modelo <- function(formula, treino, teste){
  
  modelo <- lm(formula, data = treino)
  pred <- predict(modelo, teste)
  real <-teste$clorofila
  
  #r2 <- summary(modelo)$r.squared
  r2 <- 1 - sum((real - pred)^2) / sum((real - mean(real))^2)
  rmse <- sqrt(mean((real - pred)^2))
  mae <- mean(abs(real - pred))
  bias <- mean(pred - real)
  
  return(data.frame(
    Formula = deparse(formula),
    R2 = r2,
    RMSE = rmse,
    MAE = mae,
    Bias = bias
  ))
}

variaveis <- c( 'B2','B3','B4','B5','B6','B7','B8','B8A',
                'NDCI','S1','S2','S3','S4','S5','S6','S7','S8','CI1','CI2','CI3','CI4','CI5',
                'G1','G2','R1','R2','R3','R4','R5','R6','R7','R8','R9','R10','R11','R12',
                'R13','R14','R15','N1','N2','N3','N4','N5','N6','N7','N8', 'N9','N10','N11',
                'IN1','IN2','IN3','IN4','IN5')

# Multipla e linear

resultados_total <- list()

for (seed in 1:100){
  
  set.seed(seed)
  
  amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
  
  treino <- dados[amostras, ]
  teste  <- dados[-amostras, ]
  
  resultados_seed <- list()
  
  for(k in 1:1){  # 1 variável e 2 variáveis
    
    comb <- combn(variaveis, k, simplify = FALSE)
    
    for(vars in comb){
      
      formula_linear <- as.formula(
        paste("clorofila ~", paste(vars, collapse = "+"))
      )
      
      res <- avaliar_modelo(formula_linear, treino, teste)
      
      res$seed <- seed
      res$variaveis <- paste(vars, collapse = ",")
      
      resultados_seed[[length(resultados_seed) + 1]] <- res
    }
  }
  
  resultados_total[[seed]] <- bind_rows(resultados_seed)
}

resultados_total <- bind_rows(resultados_total)

#resultados
top_modelos <- resultados_total %>%
  arrange(desc(R2),RMSE) %>%
  head(5)

print(top_modelos)

################### Grafico 
###resultado para os graficos multipla
set.seed(52)
amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
treino <- dados[amostras, ]
teste  <- dados[-amostras, ]

modelos <- list(
  M1 = lm(clorofila ~ S7 + R9, data = treino)
  # M2 = lm(clorofila ~ NDCI, data = treino)
  #  M3 = lm(clorofila ~ CI3 + N10, data = treino),
  #  M4 = lm(clorofila ~ CI3 + R14, data = treino),
  #  M5 = lm(clorofila ~ CI3 + G2, data = treino),
  # M6 = lm(clorofila ~ CI2 + CI3, data = treino)
)

treino_pred <- treino %>%
  mutate(
    conjunto = "Treino",
    M1 = predict(modelos$M1, newdata = .)
    #  M2 = predict(modelos$M2, newdata = .)
    #  M3 = predict(modelos$M3, newdata = .),
    #  M4 = predict(modelos$M4, newdata = .),
    #  M5 = predict(modelos$M5, newdata = .),
    #  M6 = predict(modelos$M5, newdata = .)
  )

teste_pred <- teste %>%
  mutate(
    conjunto = "Teste",
    M1 = predict(modelos$M1, newdata = .)
    #   M2 = predict(modelos$M2, newdata = .)
    #  M3 = predict(modelos$M3, newdata = .),
    #  M4 = predict(modelos$M4, newdata = .),
    #  M5 = predict(modelos$M5, newdata = .),
    #  M6 = predict(modelos$M5, newdata = .)
  )


resultados <- bind_rows(treino_pred, teste_pred) %>%
  pivot_longer(
    cols = M1:M1,
    names_to = "Modelo",
    values_to = "Predito"
  )
#### metricas
summary(modelos$M1)
#summary(modelos$M2)
#summary(teste_pred$B2)
#summary(treino_pred$B2)
#print(teste_pred$S7)
#max(teste_pred$S7)
# 2. Obter os resíduos
residuos <- residuals(modelos$M1)

# 3. Análise visual
hist(residuos, main="Histograma dos Resíduos")
qqnorm(residuos); qqline(residuos, col = "red")

# 4. Teste estatístico
shapiro.test(residuos)
# homocedasticidade
library(lmtest)
bptest(modelos$M2)

metricas <- do.call(rbind, lapply(names(modelos), function(m){
  
  mod <- modelos[[m]]
  pred <- predict(mod, teste)
  treino  <- teste$clorofila
  
  coefs <- coef(mod)
  
  eq <- paste0("Cl = ", round(coefs[1],2),
               paste0(" + ", round(coefs[-1],2), "*", names(coefs)[-1], collapse = ""))
  
  data.frame(
    Modelo = m,
    label = paste0(
      eq,"",
      "\nR²= ", round(summary(mod)$r.squared,4),
      "\nRMSE = ", round(sqrt(mean((pred-treino)^2)),4),
      "\nMAE = ", round(mean(abs(pred-treino)),4),
      "\nBIAS = ", round(mean(pred-treino),4)
      
    )
  )
}))

# GRÁFICO

rgm <- ggplot(resultados, aes(clorofila, Predito, color = conjunto, shape = conjunto)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", size = 1) +
  geom_smooth(
    data = subset(resultados, conjunto == "Treino"),
    method = "lm", 
    se = FALSE, # False area sombreada
    show.legend = FALSE
  ) +
  
  geom_text(
    data = metricas,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.05, 
    vjust = 1,
    inherit.aes = FALSE,
    size = 4,
    color = 'black'
  ) +
  
  facet_wrap(~Modelo, scales = "free") +
  scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(title = "Múltipla",
       x = "Clorofila Observada",
       y = "Clorofila Predita",color = "Legenda",
       shape ="Legenda",
       color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text = element_blank(),
    #  axis.title.x = element_text(size = 13),
    #  axis.title.y = element_text(size = 13),
    # axis.text.x = element_text(size = 10),
    #  axis.text.y = element_text(size = 10)
    axis.title.x = element_blank(), ### remover os eixo e deixar apenas a escala
    axis.title.y = element_blank(),# remover os eixo e deixar apenas a escala
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )
rgm
#------------------------------------------------------------ Regresão linear
set.seed(52)
amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
treino <- dados[amostras, ]
teste  <- dados[-amostras, ]

modelos <- list(
  M1 = lm(clorofila ~ S7, data = treino)
  # M2 = lm(clorofila ~ B8, data = treino)
  #  M3 = lm(clorofila ~ CI3 + N10, data = treino),
  #  M4 = lm(clorofila ~ CI3 + R14, data = treino),
  #  M5 = lm(clorofila ~ CI3 + G2, data = treino),
  # M6 = lm(clorofila ~ CI2 + CI3, data = treino)
)

treino_pred <- treino %>%
  mutate(
    conjunto = "Treino",
    M1 = predict(modelos$M1, newdata = .)
    # M2 = predict(modelos$M2, newdata = .)
    #  M3 = predict(modelos$M3, newdata = .),
    #  M4 = predict(modelos$M4, newdata = .),
    #  M5 = predict(modelos$M5, newdata = .),
    #  M6 = predict(modelos$M5, newdata = .)
  )

teste_pred <- teste %>%
  mutate(
    conjunto = "Teste",
    M1 = predict(modelos$M1, newdata = .)
    # M2 = predict(modelos$M2, newdata = .)
    #  M3 = predict(modelos$M3, newdata = .),
    #  M4 = predict(modelos$M4, newdata = .),
    #  M5 = predict(modelos$M5, newdata = .),
    #  M6 = predict(modelos$M5, newdata = .)
  )


resultados <- bind_rows(treino_pred, teste_pred) %>%
  pivot_longer(
    cols = M1:M1,
    names_to = "Modelo",
    values_to = "Predito"
  )
#### metricas
summary(modelos$M1)
summary(modelos$M2)
#summary(teste_pred$B2)
#summary(treino_pred$B2)
#print(teste_pred$S7)
#max(teste_pred$S7)
# 2. Obter os resíduos
residuos <- residuals(modelos$M1)

# 3. Análise visual
hist(residuos, main="Histograma dos Resíduos")
qqnorm(residuos); qqline(residuos, col = "red")

# 4. Teste estatístico
shapiro.test(residuos)
# homocedasticidade
bptest(modelos$M1)
plot(modelos$M1, which = 4) 
plot(modelos$M1, which = 2) # voltar aqui 

metricas <- do.call(rbind, lapply(names(modelos), function(m){
  
  mod <- modelos[[m]]
  pred <- predict(mod, teste)
  treino  <- teste$clorofila
  
  coefs <- coef(mod)
  
  eq <- paste0("Cl = ", round(coefs[1],2),
               paste0(" + ", round(coefs[-1],2), "*", names(coefs)[-1], collapse = ""))
  
  data.frame(
    Modelo = m,
    label = paste0(
      eq,"\n",
      "R²= ", round(summary(mod)$r.squared,4),
      "\nRMSE = ", round(sqrt(mean((pred-treino)^2)),4),
      "\nMAE = ", round(mean(abs(pred-treino)),4),
      "\nBIAS = ", round(mean(pred-treino),4)
      
    )
  )
}))

# GRÁFICO

rl <- ggplot(resultados, aes(clorofila, Predito, color = conjunto, shape = conjunto)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", size = 1) +
  geom_smooth(
    data = subset(resultados, conjunto == "Treino"),
    method = "lm", 
    se = FALSE, # False area sombreada
    show.legend = FALSE
  ) +
  
  geom_text(
    data = metricas,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.05, 
    vjust = 1,
    inherit.aes = FALSE,
    size = 4,
    color = 'black'
  ) +
  
  facet_wrap(~Modelo, scales = "free") +
  scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(title = "Linear",
       x = "Clorofila Observada",
       y = "Clorofila Predita",color = "Legenda",
       shape ="Legenda") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text = element_blank(),
    #  axis.title.x = element_text(size = 13),
    #  axis.title.y = element_text(size = 13),
    # axis.text.x = element_text(size = 10),
    #  axis.text.y = element_text(size = 10)
    axis.title.x = element_blank(), ### remover os eixo e deixar apenas a escala
    axis.title.y = element_blank(),# remover os eixo e deixar apenas a escala
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )
rl


#Exponecial -----------------------------------------------------------------------------------------
# FUNÇÕES NLS
# clorofila = a * exp(b1*x1 + b2*x2 + ...)
# chute inicial vindo do lm na escala log

ajustar_nls <- function(vars, data){
  ini   <- coef(lm(as.formula(paste("log(clorofila) ~", paste(vars, collapse = "+"))), data = data))
  start <- as.list(c(a = exp(ini[[1]]), setNames(ini[-1], paste0("b", seq_along(vars)))))
  f     <- as.formula(paste0("clorofila ~ a * exp(",
                             paste0("b", seq_along(vars), "*", vars, collapse = " + "), ")"))
  nls(f, data = data, start = start, control = nls.control(maxiter = 200))
}

avaliar_modeloo <- function(vars, treino, teste){
  modelo <- tryCatch(ajustar_nls(vars, treino), error = function(e) NULL)
  if (is.null(modelo)) return(NULL)          # não convergiu
  
  pred <- predict(modelo, newdata = teste)
  obs  <- teste$clorofila
  
  data.frame(
    R2   = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2),
    RMSE = sqrt(mean((obs - pred)^2)),
    MAE  = mean(abs(obs - pred))
  )
}

# =====================================================================
# divisão 70/30
# =====================================================================
resultados_exp <- list()

for (seed in 1:100){
  
  set.seed(seed)
  
  amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
  
  treino <- dados[amostras, ]
  teste  <- dados[-amostras, ]
  
  resultados_seed <- list()
  
  for(k in 1:2){  # 1 variável e 2 variáveis
    
    comb <- combn(variaveis, k, simplify = FALSE)
    
    for(vars in comb){
      
      res <- avaliar_modeloo(vars, treino, teste)
      if (is.null(res)) next
      
      res$seed <- seed
      res$variaveis <- paste(vars, collapse = ",")
      
      resultados_seed[[length(resultados_seed) + 1]] <- res
    }
  }
  
  resultados_exp[[seed]] <- bind_rows(resultados_seed)
}

resultados_exp <- bind_rows(resultados_exp)

# resultados
top_modelos <- resultados_exp %>%
  arrange(RMSE, desc(R2)) %>%
  head(5)

print(top_modelos)


# graficoExponecial (2 variáveis) ------------------------------------------------------------------

# ---------------------------
# 1. DIVISÃO TREINO / TESTE
# ---------------------------
set.seed(71)
amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
treino <- dados[amostras, ]
teste  <- dados[-amostras, ]

vars_M1 <- c("R2", "N9")

modelos <- list(
  M1 = ajustar_nls(vars_M1, treino)
  # M2 = ajustar_nls(c("N11"), treino)
)

# ---------------------------
# 3. PREDIÇÕES (nls já na escala original)
# ---------------------------
treino_pred <- treino %>%
  mutate(
    conjunto = "Treino",
    M1 = predict(modelos$M1, newdata = .)
  )

teste_pred <- teste %>%
  mutate(
    conjunto = "Teste",
    M1 = predict(modelos$M1, newdata = .)
  )

# 2. Obter os resíduos
residuos <- residuals(modelos$M1)

# 3. Análise visual
hist(residuos, main = "Histograma dos Resíduos")
qqnorm(residuos); qqline(residuos, col = "red")

# 4. Teste estatístico
shapiro.test(residuos)
# homocedasticidade (resíduo vs ajustado)
bptest(residuos ~ fitted(modelos$M1))

# ---------------------------
# 4. JUNTAR E FORMATAR
# ---------------------------
resultados <- bind_rows(treino_pred, teste_pred) %>%
  pivot_longer(
    cols = M1:M1,
    names_to = "Modelo",
    values_to = "Predito"
  )

# 5. AJUSTE EXPONENCIAL (TREINO)
coef_exp <- resultados %>%
  filter(conjunto == "Treino") %>%
  group_by(Modelo) %>%
  do({
    mod <- lm(log(Predito) ~ clorofila, data = .)
    data.frame(
      a = exp(coef(mod)[1]),
      b = coef(mod)[2]
    )
  })

# 6. DADOS PARA CURVA EXPONENCIAL
curva_exp <- resultados %>%
  filter(conjunto == "Treino") %>%
  left_join(coef_exp, by = "Modelo") %>%
  arrange(clorofila) %>%
  mutate(y_ajustado = a * exp(b * clorofila))

# métricas (teste, escala original)
metricas <- do.call(rbind, lapply(names(modelos), function(m){
  
  mod  <- modelos[[m]]
  pred <- predict(mod, teste)
  obs  <- teste$clorofila
  p    <- coef(mod)
  
  eq <- paste0("Cl = ", round(p[["a"]], 2), " * exp(",
               paste0(round(p[-1], 2), "*", vars_M1, collapse = " + "),
               ")")
  
  data.frame(
    Modelo = m,
    label = paste0(
      eq, "\n",
      "R² = ", round(1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2), 4),
      "\nRMSE = ", round(sqrt(mean((pred - obs)^2)), 4),
      "\nMAE = ", round(mean(abs(pred - obs)), 4),
      "\nBIAS = ", round(mean(pred - obs), 4)
    )
  )
}))

# 7. GRÁFICO FINAL
rg_exp <- ggplot(resultados, aes(x = clorofila, y = Predito, color = conjunto, shape = conjunto)) +
  
  # pontos
  geom_point() +
  
  # curva exponencial (ajustada no TREINO)
  geom_line(
    data = curva_exp,
    aes(x = clorofila, y = y_ajustado),
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  
  # linha 1:1
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", linewidth = 1) +
  
  # métricas
  geom_text(
    data = metricas,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.05, vjust = 1,
    inherit.aes = FALSE,
    size = 4,
    color = "black"
  ) +
  
  facet_wrap(~ Modelo, scales = "free") +
  scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(
    title = "Exponencial 2",
    x = "Clorofila Observada",
    y = "Clorofila Predita",
    color = "Legenda",
    shape = "Legenda"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text = element_blank(),
    #  axis.title.x = element_text(size = 13),
    #  axis.title.y = element_text(size = 13),
    # axis.text.x = element_text(size = 10),
    #  axis.text.y = element_text(size = 10)
    axis.title.x = element_blank(), ### remover os eixo e deixar apenas a escala
    axis.title.y = element_blank(),# remover os eixo e deixar apenas a escala
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )

rg_exp

# --------------------------- uma variável ---------------------------
set.seed(30)
amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
treino <- dados[amostras, ]
teste  <- dados[-amostras, ]

vars_M1 <- c("S1")

modelos <- list(
  M1 = ajustar_nls(vars_M1, treino)
  # M2 = ajustar_nls(c("N11"), treino)
)


# 3. PREDIÇÕES (nls já na escala original)

treino_pred <- treino %>%
  mutate(
    conjunto = "Treino",
    M1 = predict(modelos$M1, newdata = .)
  )

teste_pred <- teste %>%
  mutate(
    conjunto = "Teste",
    M1 = predict(modelos$M1, newdata = .)
  )

# 2. Obter os resíduos
residuos <- residuals(modelos$M1)

# 3. Análise visual
hist(residuos, main = "Histograma dos Resíduos")
qqnorm(residuos); qqline(residuos, col = "red")

# 4. Teste estatístico
shapiro.test(residuos)
# homocedasticidade (resíduo vs ajustado)
bptest(residuos ~ fitted(modelosa$M1))


# 4. JUNTAR E FORMATAR

resultados <- bind_rows(treino_pred, teste_pred) %>%
  pivot_longer(
    cols = M1:M1,
    names_to = "Modelo",
    values_to = "Predito"
  )

# 5. AJUSTE EXPONENCIAL (TREINO)
coef_exp <- resultados %>%
  filter(conjunto == "Treino") %>%
  group_by(Modelo) %>%
  do({
    mod <- lm(log(Predito) ~ clorofila, data = .)
    data.frame(
      a = exp(coef(mod)[1]),
      b = coef(mod)[2]
    )
  })

# 6. DADOS PARA CURVA EXPONENCIAL
curva_exp <- resultados %>%
  filter(conjunto == "Treino") %>%
  left_join(coef_exp, by = "Modelo") %>%
  arrange(clorofila) %>%
  mutate(y_ajustado = a * exp(b * clorofila))

# métricas (teste, escala original)
metricas <- do.call(rbind, lapply(names(modelos), function(m){
  
  mod  <- modelos[[m]]
  pred <- predict(mod, teste)
  obs  <- teste$clorofila
  p    <- coef(mod)
  
  eq <- paste0("Cl = ", round(p[["a"]], 2), " * exp(",
               paste0(round(p[-1], 2), "*", vars_M1, collapse = " + "),
               ")")
  
  data.frame(
    Modelo = m,
    label = paste0(
      eq, "\n",
      "R² = ", round(1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2), 4),
      "\nRMSE = ", round(sqrt(mean((pred - obs)^2)), 4),
      "\nMAE = ", round(mean(abs(pred - obs)), 4),
      "\nBIAS = ", round(mean(pred - obs), 4)
    )
  )
}))

# 7. GRÁFICO FINAL
rg_exp1 <- ggplot(resultados, aes(x = clorofila, y = Predito, color = conjunto, shape = conjunto)) +
  
  # pontos
  geom_point() +
  
  # curva exponencial (ajustada no TREINO)
  geom_line(
    data = curva_exp,
    aes(x = clorofila, y = y_ajustado),
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  
  # linha 1:1
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", linewidth = 1) +
  
  # métricas
  geom_text(
    data = metricas,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.05, vjust = 1,
    inherit.aes = FALSE,
    size = 4,
    color = "black"
  ) +
  
  facet_wrap(~ Modelo, scales = "free") +
  scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(
    title = "Exponencial",
    x = "Clorofila Observada",
    y = "Clorofila Predita",
    color = "Legenda",
    shape = "Legenda"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text = element_blank(),
    axis.title.x = element_blank(),  # remover os eixos e deixar apenas a escala
    axis.title.y = element_blank(),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )

rg_exp1

#------------------------------


#quadratica-------------------------------------------------------------------------------------------
# divisão 70/30
resultados_quad <- list()

for (seed in 1:100){
  
  set.seed(seed)
  
  amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
  
  treino <- dados[amostras, ]
  teste  <- dados[-amostras, ]
  
  resultados_seed <- list()
  
  for(k in 1:1){  # 1 variável e 2 variáveis
    
    comb <- combn(variaveis, k, simplify = FALSE)
    
    for(vars in comb){
      
      termos_quad <- c(
        vars,
        paste0("I(", vars, "^2)")
      )
      
      formula_quad <- as.formula(
        paste("clorofila ~", paste(termos_quad, collapse = "+"))
      )
      
      res <- avaliar_modeloo(formula_quad, treino, teste)
      
      res$seed <- seed
      res$variaveis <- paste(vars, collapse = ",")
      
      resultados_seed[[length(resultados_seed) + 1]] <- res
    }
  }
  
  resultados_quad[[seed]] <- bind_rows(resultados_seed)
}


resultados_quad <- bind_rows(resultados_quad)

#resultados
top_modelos <- resultados_quad %>%
  arrange(desc(R2),RMSE) %>%
  head(5)

print(top_modelos)

#### fim


#### regressão quadratica---------------------------------------------------------
set.seed(37)
amostras <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
treino <- dados[amostras, ]
teste  <- dados[-amostras, ]
modelos_quad <- list(
  M1 = lm(clorofila ~ R7 + I(R7^2), data = treino)
  #  M2 = lm(clorofila ~ B6 + IN4 + I(IN4^2) + I(B6^2), data = treino)
  # M3 = lm(clorofila ~ S7 + I(S7^2), data = treino),
  #  M4 = lm(clorofila ~ R15 + I(R15^2), data = treino),
  # M5 = lm(clorofila ~ R15 + I(R15^2), data = treino)
)

summary(modelos_quad$M1)$r.squared 
summary(modelos_quad$M2)$r.squared 
# 3. PREDIÇÕES

treino_pred <- treino %>%
  mutate(
    conjunto = "Treino",
    M1 = predict(modelos_quad$M1, newdata = .)
    #  M2 = predict(modelos_quad$M2, newdata = .),
    #    M3 = predict(modelos_quad$M3, newdata = .),
    #    M4 = predict(modelos_quad$M4, newdata = .),
    #   M5 = predict(modelos_quad$M5, newdata = .)
  )

teste_pred <- teste %>%
  mutate(
    conjunto = "Teste",
    M1 = predict(modelos_quad$M1, newdata = .)
    #  M2 = predict(modelos_quad$M2, newdata = .),
    #    M3 = predict(modelos_quad$M3, newdata = .),
    #    M4 = predict(modelos_quad$M4, newdata = .),
    #    M5 = predict(modelos_quad$M5, newdata = .)
  )
# 2. Obter os resíduos
residuos <- residuals(modelos_quad$M1)

# 3. Análise visual
hist(residuos, main="Histograma dos Resíduos")
qqnorm(residuos); qqline(residuos, col = "red")

# 4. Teste estatístico
shapiro.test(residuos)
#homovedasticidade
bptest(modelos_quad$M1)

metricas <- do.call(rbind, lapply(names(modelos), function(m){
  
  mod <- modelos_quad[[m]]
  pred <- predict(mod, teste)
  treino  <- teste$clorofila
  
  #coeficientes
  coefs <- coef(mod)
  a <- round(coefs[1], 4)  # intercepto
  b <- round(coefs[2], 4)  # linear
  c <- round(coefs[3], 4)  # quadrático
  
  #equação
  eq <- paste0("Cl = ", round(coefs[1],2), " + ", paste(paste0(round(coefs[-1],2), " * ",
                                                               names(coefs)[-1]), 
                                                        collapse = " + "
  )
  )                                    
  
  # métricas
  R2   <- round(summary(mod)$r.squared, 4)
  RMSE <- round(sqrt(mean((pred - treino)^2)), 4)
  MAE  <- round(mean(abs(pred - treino)), 4)
  BIAS <- round(mean(pred - treino), 4)
  
  data.frame(
    Modelo = m,
    label = paste0(eq,
                   "\nR² = ", R2,
                   "\nRMSE = ", RMSE,
                   "\nMAE = ", MAE,
                   "\nBIAS = ", BIAS)
  )
}))

resultados_quad <- bind_rows(treino_pred, teste_pred) %>%
  pivot_longer(
    cols = M1:M1,
    names_to = "Modelo",
    values_to = "Predito"
  )


# 5. GRÁFICO----------------------------------------------------------------------------

q <- ggplot(resultados_quad, aes(x = clorofila, y = Predito, color = conjunto, shape = conjunto)) +
  # pontos
  geom_point() +
  
  # linha 1:1
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", size = 1) +
  
  # ajuste quadrático (só treino)
  geom_smooth(
    data = resultados_quad %>% filter(conjunto == "Treino"),
    method = "lm",
    formula = y ~ x + I(x^2),
    se = FALSE,
    color = "darkgreen"
  ) +
  geom_text(
    data = metricas,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.05, vjust = 1,
    inherit.aes = FALSE,
    size = 4,
    color ="black"
  ) +
  
  facet_wrap(~ Modelo, scales = "free") +
  
  scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(
    title = "Quadrática",
    x = "Clorofila (µg/L) Observada",
    y = "Clorofila (µg/L) Predita",
    color = "Legenda",
    shape = "Legenda"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
    strip.text = element_blank(),
    #  axis.title.x = element_text(size = 13),
    #  axis.title.y = element_text(size = 13),
    # axis.text.x = element_text(size = 10),
    #  axis.text.y = element_text(size = 10)
    axis.title.x = element_blank(), ### remover os eixo e deixar apenas a escala
    axis.title.y = element_blank(),# remover os eixo e deixar apenas a escala
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )
q

rl
rgm
rg_exp
