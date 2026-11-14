rm(list = ls())
#### KRIGAGEM ORDINÁRIA DA CLOROFILA-a - código comentado passo a passo

library(ggplot2)
library(sf)
library(geoR)


#  LER OS DADOS

caminho <- "C:/Users/Administrator/Desktop/Clorofila Dissertação/"

dados <- read.csv(paste0(caminho, "Dados/amostras_serrinha_pontos.csv"),
                  sep = ";", dec = ".", header = TRUE)

# Filtro de interesse e remoção de NA
dados <- dados[, c("clorofila", "Lat", "Long")]
dados <- na.omit(dados)


#  CONVERTER COORDENADAS PARA METROS (UTM)

# Lat/Long estão em graus. O semivariograma precisa de distâncias em metros,
# por isso transformamos para UTM fuso 24 Sul (EPSG 32724).
epsg <- 32724

pontos <- st_as_sf(dados, coords = c("Long", "Lat"), crs = 4326)
pontos <- st_transform(pontos, epsg)

xy <- st_coordinates(pontos)   # colunas X e Y, em metros
tabela <- data.frame(X = xy[, 1], Y = xy[, 2], clorofila = pontos$clorofila)

# A krigagem não aceita pontos com a mesma coordenada: usa-se a média
tabela <- aggregate(clorofila ~ X + Y, data = tabela, FUN = mean)


#  LER A ÁREA DE ESTUDO (SHAPEFILE)

area <- st_read(paste0("/vsizip/", caminho, "Correção/Arquivo shp da area/area_estudo.zip"),
                quiet = TRUE)
area <- st_transform(area, epsg)
area <- st_zm(area)                       # remove a dimensão Z, se existir

# Este shapefile é feito de LINHAS (contorno). Para interpolar dentro dele
# precisamos transformar as linhas em um POLÍGONO:
area <- st_union(area)                    # junta todas as linhas
area <- st_polygonize(area)               # fecha as linhas em polígono
area <- st_collection_extract(area, "POLYGON")
area <- st_union(area)                    # um único polígono

# Mapa da área com os pontos
ggplot() +
  geom_sf(data = area, fill = "lightblue", color = "black", linewidth = 0.6) +
  geom_sf(data = pontos, aes(color = clorofila), size = 2) +
  scale_color_viridis_c(name = "Clorofila-a") +
  labs(title = "Área de estudo e pontos amostrais") +
  theme_minimal()


#  ANÁLISE EXPLORATÓRIA

# Histograma
ggplot(tabela, aes(x = clorofila)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(title = "Histograma da clorofila", x = "Clorofila-a", y = "Frequência") +
  theme_minimal()

shapiro.test(tabela$clorofila)   # normalidade (p > 0.05 = normal)
summary(tabela$clorofila)        # estatísticas descritivas
var(tabela$clorofila)            # variância
sd(tabela$clorofila)             # desvio-padrão
((sd(tabela$clorofila) / mean(tabela$clorofila)) *100 ) # coeficiente de variação


# SEMIVARIOGRAMA EMPÍRICO

dados_geo <- as.geodata(tabela, coords.col = 1:2, data.col = 3)

# Distância máxima = metade da maior distância entre pontos (regra prática)
dist_max <- max(dist(dados_geo$coords)) / 2

# uvec = 10 -> número de intervalos de distância (lags)
semivariograma <- variog(dados_geo, max.dist = dist_max, uvec = 10, messages = FALSE) # colocar a direção dentro se for o caso
plot(semivariograma, ylab = "Semivariância", xlab = "Distância (m)")

# Anisotropia (o variograma muda conforme a direção?)
plot(variog4(dados_geo, max.dist = dist_max, uvec = 10, messages = FALSE),
     ylab = "Semivariância", xlab = "Distância (m)", 
     cex.lab = 1.8,
     cex.axis = 1.5,
     cex.main = 1.8,
     lwd = 2,
     col = c("#000000", "blue", "#006400", "red"),
     legend = FALSE
)

title(main = "Anisotropia", cex.main = 1.2, font.main = 1)

legend("topleft",
       legend = c("0°", "45°", "90°", "135°"),
       col = c("#000000", "blue", "#006400", "red"),
       lty = 1:4,
       lwd = 2,
       cex = 1,        # tamanho do texto (diminua para reduzir a caixa)
       y.intersp = 0.8,  # espaçamento vertical entre as linhas
       x.intersp = 0.6,  # espaço entre o símbolo e o texto
       seg.len = 1.5,    # comprimento do traço da linha na legenda
       inset = 0.02,     # distância da borda do gráfico
       bty = "o")        # "n" remove a borda da caixa, se preferir




#  AJUSTAR OS MODELOS DE SEMIVARIÂNCIA

modelos <- c("spherical", "exponential", "gaussian", "cubic")

# Valores iniciais: patamar parcial (variância) e alcance (distância)
inicio <- c(var(tabela$clorofila), dist_max / 3)

modelos_ajustados <- list()
for (i in modelos) {
  modelos_ajustados[[i]] <- variofit(semivariograma,
                                     ini.cov.pars = inicio,
                                     cov.model = i,
                                     messages = FALSE)
}
modelos_ajustados

# Gráfico de cada modelo sobre o semivariograma
par(mfrow = c(2, 2), 
    oma = c(4, 4, 2, 1), 
    mar = c(3, 3, 2, 1), 
    cex.axis = 1.2,   # Aumenta os números dos eixos (padrão é 1)
    cex.main = 1.4)   # Aumenta os títulos dos gráficos

for (i in modelos) {
  nome <- switch(i,
                 spherical   = "Esférico",
                 exponential = "Exponencial",
                 gaussian    = "Gaussiano",
                 cubic       = "Cúbico")
  # Define quais eixos devem ser ocultados para cada modelo:
  # - Esférico: remove o eixo X
  # - Exponencial: remove os eixos X e Y
  # - Cúbico: remove o eixo Y
  # - Gaussiano: mantém ambos os eixos
  xaxt_val <- if (i %in% c("spherical", "exponential")) "n" else "s"
  yaxt_val <- if (i %in% c("exponential", "cubic")) "n" else "s"
  
  plot(semivariograma, ylab = "Semivariância", xlab = "Distância (m)", 
       main = nome, pch =19,
       xaxt = xaxt_val, 
       yaxt = yaxt_val)
  lines.variomodel(modelos_ajustados[[i]])
}


# Adiciona um único rótulo centralizado para o eixo X (inferior, side = 1) e eixo Y (esquerdo, side = 2)
mtext("Distância (m)", side = 1, outer = TRUE, line = 1.5, cex = 1.2)
mtext("Semivariância", side = 2, outer = TRUE, line = 0.7, cex = 1.2)
mtext("Semivariograma", side = 3, outer = TRUE, line = 0.7, cex = 1.2, font = 2)

# Restaura os parâmetros originais do R para não afetar os próximos gráficos
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1, 
    cex.axis = 1, cex.main = 1)


#  VALIDAÇÃO CRUZADA E ESCOLHA DO MODELO

# Cada ponto é retirado, predito pelos demais e comparado ao valor real.
modelos_xvalid <- list()
for (i in modelos) {
  modelos_xvalid[[i]] <- xvalid(dados_geo, model = modelos_ajustados[[i]],
                                messages = FALSE)
}

# Tabela comparativa
comparacao <- data.frame(modelo = modelos, ME = NA, RMSE = NA, dp_erro_pad = NA)
for (k in seq_along(modelos)) {
  xv <- modelos_xvalid[[modelos[k]]]
  comparacao$ME[k]          <- mean(xv$error)          # ideal: perto de 0 BIAS
  comparacao$RMSE[k]        <- sqrt(mean(xv$error^2))  # quanto menor, melhor RMSE
  comparacao$dp_erro_pad[k] <- sd(xv$std.error)        # ideal: perto de 1 DEsvio paadrão do erro
}
print(comparacao, digits = 4)

# Modelo selecionado = menor RMSE
# (para forçar outro modelo, troque por: modelo_selecionado <- modelos_ajustados$gaussian)
nome_modelo <- comparacao$modelo[which.min(comparacao$RMSE)] 
modelo_selecionado <- modelos_ajustados[[nome_modelo]]
cat("Modelo selecionado:", nome_modelo, "\n")

plot(semivariograma, ylab = "Semivariância", xlab = "Distância (m)",
     main = paste("Modelo selecionado:", switch(nome_modelo, # SWitch para renomear
                                                spherical = "Esférico",
                                                exponential = "Exponencial",
                                                gaussian = "Gaussiano",
                                                cubic = "Cúbico",
                                                linear = "Linear")))
lines.variomodel(modelo_selecionado)

# ----------------------------------------------------------------------
# 7.1 GRÁFICOS DA VALIDAÇÃO CRUZADA

# a) Comparação dos modelos: erro médio (ideal ~ 0) e desvio do erro
#    padronizado (ideal ~ 1)
par(mfrow = c(1, 2))
text(barplot(comparacao$ME, names.arg = comparacao$modelo,
             main = "Erro médio da validação cruzada",
             ylim = range(c(0, comparacao$ME)) * 1.3),
     y = comparacao$ME, labels = round(comparacao$ME, 4), pos = 3)
text(barplot(comparacao$dp_erro_pad, names.arg = comparacao$modelo,
             main = "Desvio padrão do erro padronizado",
             ylim = c(0, max(comparacao$dp_erro_pad) * 1.3)),
     y = comparacao$dp_erro_pad, labels = round(comparacao$dp_erro_pad, 3), pos = 3)
par(mfrow = c(1, 1))

# b) Gráficos do modelo selecionado
xv <- modelos_xvalid[[nome_modelo]]

pts_xv <- data.frame(X         = dados_geo$coords[, 1],
                     Y         = dados_geo$coords[, 2],
                     observado = xv$data,
                     predito   = xv$predicted,
                     erro      = xv$error,       # observado - predito
                     erro_pad  = xv$std.error)   # erro / desvio da krigagem

# Observado x predito (pontos sobre a linha vermelha = predição perfeita)
ggplot(pts_xv, aes(observado, predito)) +
  geom_point(color = "steelblue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Validação cruzada: observado x predito",
       x = "Observado", y = "Predito") +
  theme_minimal()

# Histograma dos erros padronizados (esperado: centrado em 0, entre -2 e 2)
ggplot(pts_xv, aes(erro_pad)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(title = "Erros padronizados da validação cruzada",
       x = "Erro padronizado", y = "Frequência") +
  theme_minimal()

# Mapa dos erros nos pontos (mostra onde o modelo erra mais)
ggplot() +
  geom_sf(data = area, fill = "grey95", color = "black") +
  geom_point(data = pts_xv, aes(X, Y, color = erro), size = 3) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0, name = "Erro (obs - pred)") +
  coord_sf(crs = epsg) +
  labs(title = "Erros da validação cruzada nos pontos",
       x = "Leste (m)", y = "Norte (m)") +
  theme_minimal()



#  GRADE DE INTERPOLAÇÃO (SÓ DENTRO DA ÁREA)

# Pontos regularmente espaçados a cada 50 m, mantendo só os que caem na área
grade <- st_make_grid(area, cellsize = 50, what = "centers")
grade <- st_sf(geometry = grade)
grade <- st_filter(grade, area)

coords_grade <- st_coordinates(grade)
cat("Células a interpolar:", nrow(coords_grade), "\n")


#  KRIGAGEM ORDINÁRIA

krigagem <- krige.conv(dados_geo,
                       locations = coords_grade,
                       krige = krige.control(type.krige = "ok",
                                             obj.model = modelo_selecionado))

resultado <- data.frame(X    = coords_grade[, 1],
                        Y    = coords_grade[, 2],
                        pred = krigagem$predict,
                        dp   = sqrt(pmax(krigagem$krige.var, 0)))


# MAPAS

# Valores preditos
ggplot() +
  geom_tile(data = resultado, aes(X, Y, fill = pred)) +
  geom_sf(data = area, fill = NA, color = "black") +
  geom_sf(data = pontos, size = 1) +
  scale_fill_viridis_c(name = "Clorofila-a") +
  labs(title = "Krigagem ordinária - Clorofila-a", x = "Leste (m)", y = "Norte (m)") +
  theme_minimal()

# Incerteza (desvio-padrão da krigagem)
ggplot() +
  geom_tile(data = resultado, aes(X, Y, fill = dp)) +
  geom_sf(data = area, fill = NA, color = "black") +
  geom_sf(data = pontos, size = 1) +
  scale_fill_viridis_c(option = "magma", name = "Desvio-padrão") +
  labs(title = "Desvio-padrão da krigagem", x = "Leste (m)", y = "Norte (m)") +
  theme_minimal()

# Baixando os mapas
library(raster)
# 1. Converter o data.frame em RasterLayer usando as coordenadas X, Y e os valores
raster_pred <- rasterFromXYZ(resultado[, c("X", "Y", "pred")])
raster_dp   <- rasterFromXYZ(resultado[, c("X", "Y", "dp")])
writeRaster(raster_pred,
            "C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/krigagem.tif",
            overwrite = TRUE)
writeRaster(raster_dp,
            "C:/Users/Administrator/Desktop/Clorofila Dissertação/Dados/desvio.tif",
            overwrite = TRUE)

