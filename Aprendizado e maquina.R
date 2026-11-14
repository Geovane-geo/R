rm(list=ls())
#### Codigo limpo
library(ggplot2)
library(tidyr)
library(dplyr)
library(Metrics)
library(tidyverse)
library(caret)
###dados
dados <- read.csv("C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/amostras_serrinha_kernel_3x3_mediana.csv",
                  sep =';',
                  dec ='.',
                  header = TRUE)
##-------------------------------------Random forest -----------------------------------------------------
library(randomForest)
set.seed(189)  # para reprodutibilidade
particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
treino <- dados[particao, ]
teste  <- dados[-particao, ]

# Treino do modelo RF
modelo_rf <- randomForest(clorofila ~ ., data = treino,
                          ntree = 500,        # número de árvores 500 ou 1000
                          mtry = 7, #mtry = p/3 -> 20/3 = 6,67 (Ou seja, a cada nó, o RF sorteia 7 variáveis entre as 20 para escolher a melhor divisão)
                          importance = TRUE
)
print(modelo_rf)# Resultados do treino
# Importância das variáveis
#%IncMSE Mede quanto o erro (MSE) aumenta quando a variável é embaralhada.
#Mais alto = mais importante de verdade
#IncNodePurity Mede quanto a variável reduz impureza nas divisõesMais alto = mais usada nas árvores, mas não necessariamente mais importante


importance(modelo_rf)  
varImpPlot(modelo_rf)

estimado <- predict(modelo_rf, newdata = teste)# Predição no conjunto de teste
observado <- teste$clorofila
# Avaliação

R2 <- 1 - sum((observado- estimado)^2) / 
          sum((observado - mean(observado))^2)
RMSE <- sqrt(mean((observado - estimado)^2))
MAE <- mean(abs(observado - estimado))
bias <- mean(estimado - observado)

cat("R²:", R2, "\nRMSE:", RMSE, "\nMAE:", MAE, "\nBIAS:", bias)


#Grafico
df_grafico <- rbind(
  data.frame(
    Observado = treino$clorofila,
    Estimado = predict(modelo_rf, newdata = treino),
    Conjunto = "Treino"
  ),
    data.frame(
      Observado = teste$clorofila,
      Estimado = predict(modelo_rf, newdata = teste),
      Conjunto = "Teste")
)

### metrica para colocar no grafico
metricas <- paste0(
  "\nR² = ", round(R2, 4),
  "\nRMSE = ", round(RMSE, 4),
  "\nMAE = ", round(MAE, 4),
  "\nBIAS = ", round(bias, 4)
)

# Plot
rf <- ggplot(df_grafico, aes(x = Observado, y = Estimado, color = Conjunto, shape = Conjunto)) +
  geom_point() +
  
  # linha 1:1 (referência ideal) teste
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", size = 1) +
  
  geom_smooth(data = subset(df_grafico, Conjunto == "Treino"),
              method = "lm",
              se = TRUE, # False area sombreada
              show.legend = FALSE) +

  annotate("text",x = -Inf, y = Inf,label = metricas,hjust = -0.04, vjust = 0.9,size = 5,color = "black") +
  scale_color_manual(values = c("Treino" = "darkgreen", 'Teste' = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
   labs(title = "Random Forest",
       x = "Clorofila (µg/L) Observada ", 
       y = "Clorofila (µg/L) Predita", 
       color = "Legenda",
       shape= "Legenda") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
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
# visualização
rf

# como ficar visualizando com novos dados
predict(modelo_rf, newdata = teste)

#-----random forest

library(Metrics)
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))

resultados <- data.frame(Semente = integer(),
                         RMSE = numeric(),
                         Rsquared = numeric(),
                         MAE = numeric())

for (s in 1:500) {
  set.seed(s)
  
  # Partição treino-teste
  particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
  treino <- dados[particao, ]
  teste  <- dados[-particao, ]
  
  # Treinar Random Forest
  modelo_rf <- randomForest(clorofila ~ ., data = treino,
                            ntree = 500,
                            mtry = 7,
                            importance = TRUE)
  
  # Previsões
  predicoes <- predict(modelo_rf, newdata = teste)
  observado <- teste$clorofila
  
  # Métricas
  rmse_valor <- rmse(observado, predicoes)
  rsquared_valor <- 1 - sum((observado - predicoes)^2) / sum((observado - mean(observado))^2)
  mae_valor <- mae(observado, predicoes)
  bias <- mean(predicoes - observado)
 
   # Salvar resultados
  resultados <- rbind(resultados,
                      data.frame(Semente = s,
                                 RMSE = rmse_valor,
                                 Rsquared = rsquared_valor,
                                 MAE = mae_valor,
                                 bias = bias ))
  
  # Progresso
  cat("Semente", s, "-> R2:", round(rsquared_valor, 4), 
      "| RMSE:", round(rmse_valor, 2), "\n")
}

# Melhor semente
melhor_semente_R2 <- resultados[which.max(resultados$Rsquared), ]
melhor_semente_RMSE <- resultados[which.min(resultados$RMSE), ]

print(melhor_semente_R2)
print(melhor_semente_RMSE)

#---------------------------------------------Modelos SVM -------------------------------------------------------------
library(e1071)

resultados <- data.frame(Semente = integer(),
                         RMSE = numeric(),
                         Rsquared = numeric(),
                         MAE = numeric())
for (s in 1:500) {
  # Define a semente para esta iteração específica.
  # Isso afeta a partição dos dados, tornando-a reprodutível para esta semente.
  set.seed(s)
  particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
  treino <- dados[particao, ]
  teste  <- dados[-particao, ]
  modelo_svm<- svm(clorofila ~ ., data = treino,
                   kernel = "radial", #linear
                   ranges = list( # Cost= quanto você tolera erros vs. margem larga. C pequeno o modelo aceita mais erros → margem mais larga
                   cost = 2^(-1:5), # 2^(-1:5) = c(0.5, 1, 2, 4, 8, 16, 32)
                   gamma = 2^(-5:1) # 2^(-5:1) = c(0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2)
                   ))
  predicoes <- predict(modelo_svm, newdata = teste)
  observado <- teste$clorofila
  # Calcule as métricas de performance
  rmse_valor <- rmse(observado, predicoes)
  rsquared_valor <- 1 - sum((observado - predicoes)^2) / sum((observado - mean(observado))^2)
  mae_valor <- mae(observado, predicoes)
  resultados <- rbind(resultados,
                      data.frame(Semente = s,
                                 RMSE = rmse_valor,
                                 Rsquared = rsquared_valor,
                                 MAE = mae_valor))
  # Imprime o progresso no console
  cat("Semente", s, "-> R2:", round(rsquared_valor, 4), 
      "| RMSE:", round(rmse_valor, 2), "\n")
}
melhor_semente_R2 <- resultados[which.max(resultados$Rsquared), ]
melhor_semente_RMSE <- resultados[which.min(resultados$RMSE), ]

print(melhor_semente_R2)
print(melhor_semente_RMSE)

#------------------------------------- SVM--------------------------------
set.seed(189)  # para reprodutibilidade
particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
treino <- dados[particao, ]
teste  <- dados[-particao, ]

# Treino do modelo RF
modelo_svm <- svm(clorofila ~ ., data = treino,
                  kernel = "radial", #linear
                  ranges = list( # Cost= quanto você tolera erros vs. margem larga. C pequeno o modelo aceita mais erros → margem mais larga
                  cost = 2^(-1:5), # 2^(-1:5) = c(0.5, 1, 2, 4, 8, 16, 32)
                  gamma = 2^(-5:1) # 2^(-5:1) = c(0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2)
                  ))
print(modelo_svm)# Resultados do treino

estimado <- predict(modelo_svm, newdata = teste)# Predição no conjunto de teste
observado <- teste$clorofila
# Avaliação
R2 <- 1 - sum((teste$clorofila - estimado)^2) / sum((teste$clorofila - mean(teste$clorofila))^2)
RMSE <- sqrt(mean((teste$clorofila - estimado)^2))
MAE <- mean(abs(teste$clorofila - estimado))
bias <- mean(estimado - teste$clorofila)

cat("R²:", R2, "\nRMSE:", RMSE, "\nMAE:", MAE, "\nBIAS:", bias)


#Grafico
df_grafico <- rbind(
  data.frame(
    Observado = treino$clorofila,
    Estimado = predict(modelo_svm, newdata = treino),
    Conjunto = "Treino"),
  data.frame(
    Observado = teste$clorofila,
    Estimado = predict(modelo_svm, newdata = teste),
    Conjunto = "Teste")
)

### metrica para colocar no grafico
metricas <- paste0(
  "\nR² = ", round(R2, 4),
  "\nRMSE = ", round(RMSE, 4),
  "\nMAE = ", round(MAE, 4),
  "\nBIAS = ", round(bias, 4)
)

# Plot
svm <- ggplot(df_grafico, aes(x = Observado, y = Estimado, color = Conjunto, shape = Conjunto)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", size = 1) +
  
  geom_smooth(data = subset(df_grafico, Conjunto == "Treino"),
              method = "lm",
              se = TRUE, # False area sombreada
              show.legend = FALSE) +
  annotate("text",x = -Inf, y = Inf,label = metricas, hjust = -0.04, vjust = 0.9,size = 5,color = "black") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(title = "SVM", 
       x = "Clorofila (µg/L) Observada", 
       y = "Clorofila (µg/L) Predita", 
       color = "Legenda",
       shape = "Legenda") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
  #  axis.title.x = element_text(size = 13),
  #  axis.title.y = element_text(size = 13),
   # axis.text.x = element_text(size = 10),
  #  axis.text.y = element_text(size = 10)
  legend.title = element_text(size = 18, face = "bold"),
  legend.text  = element_text(size = 16),
    axis.title.x = element_blank(), ### remover os eixo e deixar apenas a escala
    axis.title.y = element_blank(), # remover os eixo e deixar apenas a escala
  legend.key.size = unit(0.8, "cm"),
  legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )
svm

# --------------------------------KNN ------------------------------------
#pacote
library(FNN)    # knn.reg


# Número de vizinhos para KNN

resultados_knn <- data.frame()

for (s in 1:500) {
  
  set.seed(s)
  
  particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
  treino   <- dados[particao, ]
  teste    <- dados[-particao, ]
  
  features <- setdiff(names(dados), "clorofila")
  
  preProc     <- preProcess(treino[, features], method = c("center", "scale"))
  treino_norm <- as.matrix(predict(preProc, treino[, features]))
  teste_norm  <- as.matrix(predict(preProc, teste[, features]))
  
  pred_knn <- knn.reg(
    train = treino_norm,
    test  = teste_norm,   
    y     = treino$clorofila,
    k     = 2 # observar a distancia isso suaviza o grafico 
  )$pred
  
  observado <- teste$clorofila
  
  resultados_knn <- rbind(resultados_knn, data.frame(
    Semente  = s,
    RMSE     = sqrt(mean((observado - pred_knn)^2)),
    Rsquared = 1 - sum((observado - pred_knn)^2) / 
      sum((observado - mean(observado))^2),
    MAE    = mean(abs(observado - pred_knn))
  ))
  
  cat("Semente", s,
      "-> R²:", round(tail(resultados_knn$Rsquared, 1), 4),
      "| RMSE:", round(tail(resultados_knn$RMSE, 1), 2), "\n")
}

melhor_R2   <- resultados_knn[which.max(resultados_knn$Rsquared), ]
melhor_RMSE <- resultados_knn[which.min(resultados_knn$RMSE), ]

print(melhor_R2)
print(melhor_RMSE)
#----------------------------------------------KNN
set.seed(364)

# ── Partição 
particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
treino   <- dados[particao, ]
teste    <- dados[-particao, ]

# ── Normalização (fit apenas no treino) 
features <- setdiff(names(dados), "clorofila")

preProc     <- preProcess(treino[, features], method = c("center", "scale"))
treino_norm <- as.matrix(predict(preProc, treino[, features]))
teste_norm  <- as.matrix(predict(preProc, teste[, features]))

# ── Modelo KNN
K <- 2

knn_pred <- function(test_mat) {
  knn.reg(train = treino_norm, 
          test = test_mat, 
          y = treino$clorofila, k = K)$pred
}

pred_treino  <- knn_pred(treino_norm)
pred_teste   <- knn_pred(teste_norm)

#para fazer predict no KNN é necessario normalizar os dados antes

# ── Métricas 
metricas <- function(obs, pred, label) {
  cat("=== ", label, " ===\n", sep = "")
  cat("R²  :", round(1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2), 4), "\n")
  cat("RMSE:", round(sqrt(mean((obs - pred)^2)), 4), "\n")
  cat("MAE :", round(mean(abs(obs - pred)), 4), "\n")
  cat("BIAS:", round(mean(pred - obs), 4), "\n\n")
}

metricas(treino$clorofila, pred_treino, "TREINO")
metricas(teste$clorofila,  pred_teste,  "TESTE")


R2_teste <- 1 - sum((teste$clorofila - pred_teste)^2) / 
  sum((teste$clorofila - mean(teste$clorofila))^2)
RMSE_teste <- sqrt(mean((teste$clorofila - pred_teste)^2))
MAE_teste  <- mean(abs(teste$clorofila - pred_teste))
bias_teste <- mean(pred_teste - teste$clorofila)

cat("R²:", R2_teste, "\nRMSE:", RMSE_teste, "\nMAE:", MAE_teste, "\nBIAS:", bias_teste)

# ── Gráfico
df_grafico <- rbind(
  data.frame(Observado = treino$clorofila, 
             Estimado = pred_treino,
             Conjunto = "Treino"),
  data.frame(Observado = teste$clorofila,  
             Estimado = pred_teste,  
             Conjunto = "Teste")
)

## metrica para colocar no grafico
metricas <- paste0(
  "\nR² = ", round(R2_teste, 4),
  "\nRMSE = ", round(RMSE_teste, 4),
  "\nMAE = ", round(MAE_teste, 4),
  "\nBIAS = ", round(bias_teste, 4)
)

knn <- ggplot(df_grafico, aes(x = Observado, y = Estimado, color = Conjunto, shape = Conjunto)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", size = 1) +
  
  geom_smooth(data = subset(df_grafico, Conjunto == "Treino"), ## %in% 
              method = "lm",
              se = TRUE, # False area sombreada
              show.legend = FALSE)  +
  annotate("text",
        x = -Inf, y = Inf,         label = metricas,
         hjust = -0.04, vjust = 0.9,
        size = 5,
         color = "black") +
  scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(title = "KNN", 
       x = "Clorofila (µg/L) Observada", 
       y = "Clorofila (µg/L) Predita", 
       color = "Legenda",
       shape = "Legenda") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
    #  axis.title.x = element_text(size = 13),
    #  axis.title.y = element_text(size = 13),
    # axis.text.x = element_text(size = 10),
    #  axis.text.y = element_text(size = 10)
    axis.title.x = element_blank(), ### remover os eixo e deixar apenas a escala
    axis.title.y = element_blank(), # remover os eixo e deixar apenas a escala
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )
knn
#-------------------------------------Xgbost-------------------------------------------------
library(xgboost)

# Data frame para resultados
resultados_xgb <- data.frame()

# Loop sobre 500 sementes
for (s in 1:500) {
  set.seed(s)
  
  # Partição treino-teste 70%-30%
  particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
  treino <- dados[particao, ]
  teste  <- dados[-particao, ]
  
  # Converter para matriz (XGBoost precisa)
  train_matrix <- as.matrix(treino[, setdiff(names(treino), "clorofila")])
  test_matrix  <- as.matrix(teste[, setdiff(names(teste), "clorofila")])
  
  dtrain <- xgb.DMatrix(data = train_matrix, label = treino$clorofila)
  dtest  <- xgb.DMatrix(data = test_matrix,  label = teste$clorofila)
  
  # Treinar modelo XGBoost
  modelo_xgb <- xgb.train(
    data = dtrain,
    nrounds = 100,               # número de árvores
    objective = "reg:squarederror",
    max_depth = 6,
    eta = 0.1,
    verbose = 0
  )
  
  # Previsão no conjunto de teste
  pred_xgb <- predict(modelo_xgb, newdata = dtest)
  
  observado <- teste$clorofila
  
  # Calcular métricas
  rmse_valor <- rmse(observado, pred_xgb)
  rsquared_valor <- 1 - sum((observado - pred_xgb)^2) / sum((observado - mean(observado))^2)
  mae_valor <- mae(observado, pred_xgb)
  
  # Salvar resultados
  resultados_xgb <- rbind(resultados_xgb,
                          data.frame(Semente = s,
                                     RMSE = rmse_valor,
                                     Rsquared = rsquared_valor,
                                     MAE = mae_valor))
  
  cat("Semente", s, "-> R2:", round(rsquared_valor, 4), 
      "| RMSE:", round(rmse_valor, 2), "\n")
}

# Melhor semente
melhor_semente_R2 <- resultados_xgb[which.max(resultados_xgb$Rsquared), ]
melhor_semente_RMSE <- resultados_xgb[which.min(resultados_xgb$RMSE), ]

print(melhor_semente_R2)
print(melhor_semente_RMSE)

#-------Xgboost

set.seed(425)  # para reprodutibilidade

# Partição treino-teste
particao <- createDataPartition(dados$clorofila, p = 0.7, list = FALSE)
treino <- dados[particao, ]
teste  <- dados[-particao, ]

# Converter para matriz (XGBoost precisa)
train_matrix <- as.matrix(treino[, setdiff(names(treino), "clorofila")])
test_matrix  <- as.matrix(teste[, setdiff(names(teste), "clorofila")])

dtrain <- xgb.DMatrix(data = train_matrix, label = treino$clorofila)
dtest  <- xgb.DMatrix(data = test_matrix,  label = teste$clorofila)

# Parâmetros do modelo
params <- list(
  objective = "reg:squarederror",  # objetivo de regressão
  max_depth  = 6,                   # profundidade máxima das árvores
  eta        = 0.1                  # taxa de aprendizado
)

# Treinar modelo XGBoost
modelo_xgb <- xgb.train(
  params = params,   # parâmetros agora dentro da lista
  data   = dtrain,
  nrounds = 100,     # número de árvores
  verbose = 0
)

# Previsão no treino e no teste
pred_xgb_treino <- predict(modelo_xgb, newdata = dtrain)
pred_xgb_teste  <- predict(modelo_xgb, newdata = dtest)

# Avaliação - Teste
observado <- teste$clorofila
RMSE_val <- rmse(observado, pred_xgb_teste)
R2_val   <- 1 - sum((observado - pred_xgb_teste)^2) / sum((observado - mean(observado))^2)
MAE_val  <- mae(observado, pred_xgb_teste)
bias_val <- mean(pred_xgb_teste - teste$clorofila)

cat("R²:", round(R2_val, 4), "\nRMSE:", round(RMSE_val, 4), "\nMAE:", round(MAE_val, 4), "\nBIAS", round(bias_val,4))

# Criar data frame para gráfico (Treino + Teste)
df_grafico <- rbind(
  data.frame(
    Observado = treino$clorofila,
    Estimado = pred_xgb_treino,
    Conjunto = "Treino"
  ),
  data.frame(
    Observado = teste$clorofila,
    Estimado = pred_xgb_teste,
    Conjunto = "Teste"
  )
)

## metrica para colocar no grafico
metricas <- paste0(
  "\nR² = ", round(R2_val, 4),
  "\nRMSE = ", round(RMSE_val, 4),
  "\nMAE = ", round(MAE_val, 4),
  "\nBIAS = ", round(bias_val, 4)
)

xg <- ggplot(df_grafico, aes(x = Observado, y = Estimado, color = Conjunto, shape = Conjunto)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "twodash", size = 1) +
  
  geom_smooth(data = subset(df_grafico, Conjunto == "Treino"), ## %in% 
              method = "lm",
              se = TRUE, # False area sombreada
              show.legend = FALSE)  +
 annotate("text",
          x = -Inf, y = Inf,
          label = metricas,
          hjust = -0.04, vjust = 0.9,
          size = 5,
          color = "black") +
 scale_color_manual(values = c("Treino" = "darkgreen", "Teste" = "blue")) +
  scale_shape_manual(values = c("Treino" = 8, "Teste" = 15)) +
  labs(title = "XGBoost", 
       x = "Clorofila (µg/L) Observada", 
       y = "Clorofila (µg/L) Predita", 
       color = "Legenda",
       shape = "Legenda") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    plot.title = element_text(size = 12,face = "bold"),
    plot.subtitle = element_text(size = 15),
    #  axis.title.x = element_text(size = 13),
    #  axis.title.y = element_text(size = 13),
    # axis.text.x = element_text(size = 10),
    #  axis.text.y = element_text(size = 10)
    axis.title.x = element_blank(), ### remover os eixo e deixar apenas a escala
    axis.title.y = element_blank(), # remover os eixo e deixar apenas a escala
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ) + guides(
    shape = guide_legend(
      override.aes = list(size = 3)
    )
  )
xg
# Seus dados Clorofila divisão quantilica


# Gerar os limites de 4 classes (0%, 25%, 50%, 75%, 100%)
limites <- quantile(dados$clorofila, probs = seq(0, 1, by = 0.25))

# Criar a coluna de classe
classe <- cut(dados$clorofila, breaks = limites, include.lowest = TRUE)
boxplot(limites)

#grafico 
hist(dados$clorofila, 
     main="Distribuição de Clorofila com Quartis",
     xlab="Clorofila-a (µg/L)", 
     ylab="Frequência (nº de amostras)",
     col="lightblue")
abline(v = limites, col = "red", lwd = 2, lty = 2)

#--------------------------------------------------- predição com imagem random forest
library(raster)
library(terra)

raster_entrada <- raster::stack("C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/todasbandas.tif")
names(raster_entrada)

# Convertendo o raster em um data.frame para predição
celulas_validas <- which(!is.na(raster::getValues(raster_entrada[[1]])))
raster_df <- as.data.frame(raster_entrada, na.rm = TRUE)

#--------------------------------------------------- random forest -----------------
# como ficar visualizando com novos dados
Predicao_modeloRF <- predict(modelo_rf, newdata = raster_df)

# Reconstrói o raster preservando os NAs
predicted_raster  <- raster::raster(raster_entrada)
valores <- rep(NA_real_, ncell(raster_entrada))
valores[celulas_validas]  <- as.numeric(Predicao_modeloRF)
raster::values(predicted_raster) <- valores
#------------------ plotagem
plot(predicted_raster,
     main   = "Predição Clorofila - Random Forest",
     col    = hcl.colors(100, "viridis"),
     legend = TRUE)

writeRaster(predicted_raster,
            "C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/predicao_RF.tif",
            overwrite = TRUE)

#--------------------------------------------------- KNN-----------------
#normalização
preProc <- preProcess(treino[, features], method = c("center", "scale"))
knn_norm <- as.matrix(predict(preProc, raster_df[, features]))
pred_knn  <- knn_pred(knn_norm)

# Reconstrói o raster preservando os NAs
predicted_raster  <- raster::raster(raster_entrada)
valores <- rep(NA_real_, ncell(raster_entrada))
valores[celulas_validas]  <- as.numeric(pred_knn)
raster::values(predicted_raster) <- valores
#------------------ plotagem
plot(predicted_raster,
     main   = "Predição Clorofila - KNN",
     col    = hcl.colors(100, "viridis"),
     legend = TRUE)

writeRaster(predicted_raster,
            "C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/predicao_KNN3x3.tif",
            overwrite = TRUE)
#--------------------------------------------------- SVM-----------------
pred_svm  <- predict(modelo_svm, newdata = raster_df)

# Reconstrói o raster preservando os NAs
predicted_raster  <- raster::raster(raster_entrada)
valores <- rep(NA_real_, ncell(raster_entrada))
valores[celulas_validas]  <- as.numeric(pred_svm)
raster::values(predicted_raster) <- valores
#------------------ plotagem
plot(predicted_raster,
     main   = "Predição Clorofila - SVM",
     col    = hcl.colors(100, "viridis"),
     legend = TRUE)

writeRaster(predicted_raster,
            "C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/predicao_SVM3x3.tif",
            overwrite = TRUE)

#--------------------------------------------------- XGBOOSt-----------------
# como ficar visualizando com novos dados
pred_xgb  <- predict(modelo_xgb, newdata = raster_df)

# Reconstrói o raster preservando os NAs
predicted_raster  <- raster::raster(raster_entrada)
valores <- rep(NA_real_, ncell(raster_entrada))
valores[celulas_validas]  <- as.numeric(pred_xgb)
raster::values(predicted_raster) <- valores
#------------------ plotagem
plot(predicted_raster,
     main   = "Predição Clorofila - XGBOOST",
     col    = hcl.colors(100, "viridis"),
     legend = TRUE)

writeRaster(predicted_raster,
            "C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/predicao_XG3x3.tif",
            overwrite = TRUE)


#--------------------------------------------- garfico ao lado do outro


library(patchwork)

graficos <- (knn | rf) /
  (svm | xg) +
  
  plot_layout(guides = "collect") &
  
  theme(
    legend.position = "right",
    legend.box = "vertical"
  )

graficos

library(cowplot) #pacote melhor de graficos 

final <- ggdraw(graficos) +
  draw_label("Clorofila (µg/L) Observada", x = 0.5, y = 0.002) +
  draw_label("Clorofila (µg/L) Predita", x = 0.002, y = 0.5, angle = 90)+
  theme(plot.margin = ggplot2::margin(15, 15, 20, 20))  # bottom e left maiores

final

#######################################################################
# ==========================================================
# PACOTES
# ==========================================================

library(ggplot2)
library(patchwork)
library(cowplot)


# ==========================================================
# 1. CONFIGURAÇÃO DOS GRÁFICOS
# ==========================================================

# --------------------------
# KNN
# --------------------------

knn <- knn +
  theme(
    # Título do gráfico
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    ),
    
    # Remover eixo X
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.8),
    
    # Aumentar números do eixo Y
    axis.text.y = element_text(size = 16),
    
    # Remover título individual do eixo Y
    axis.title.y = element_blank()
  )


# --------------------------
# RANDOM FOREST
# --------------------------

rf <- rf +
  theme(
    # Título do gráfico
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    ),
    
    # Remover eixo X
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.8),
    
    # Aumentar números do eixo Y
    axis.text.y = element_text(size = 16),
    
    # Remover título individual do eixo Y
    axis.title.y = element_blank()
  )


# --------------------------
# SVM
# --------------------------

svm <- svm +
  theme(
    # Título do gráfico
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    ),
    
    # Números dos eixos
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    
    # Remover títulos individuais
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


# --------------------------
# XGBOOST
# --------------------------

xg <- xg +
  theme(
    # Título do gráfico
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    ),
    
    # Números dos eixos
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    
    # Remover títulos individuais
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


# ==========================================================
# MONTAR OS GRÁFICOS
# ==========================================================

graficos <- (knn | rf) /
  (svm | xg) +
  
  plot_layout(
    guides = "collect"
  ) &
  
  theme(
    legend.position = "right",
    legend.box = "vertical"
  )


# ==========================================================
# FIGURA FINAL
# ==========================================================

final <- ggdraw(graficos) +
  
  draw_label(
    "Clorofila (µg/L) Observada",
    x = 0.50,
    y = 0.002,
    size = 18
  ) +
  
  draw_label(
    "Clorofila (µg/L) Predita",
    x = 0.002,
    y = 0.50,
    angle = 90,
    size = 18
  ) +
  
  theme(
    plot.margin = ggplot2::margin(
      t = 15,
      r = 15,
      b = 25,
      l = 25
    )
  )

final

