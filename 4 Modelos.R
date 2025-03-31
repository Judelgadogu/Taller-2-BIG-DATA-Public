# --------------------------
# 0. LIMPIEZA Y CONFIGURACIÓN DEL ENTORNO
# --------------------------

rm(list = ls())  # Limpiar el entorno
gc()  # Limpiar la memoria
closeAllConnections()  # Cerrar conexiones abiertas

# --------------------------
# 1. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
# --------------------------

user <- Sys.getenv("USERNAME")

if (user == "judel") {
  path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/segunda parte/Taller 2/input"
} else if (user == "---") {
  path <- "-----"
} else if (user == "-evaluador-") {
  path <- "-----"
} else {
  stop("Usuario no reconocido. Ajusta el 'path' manualmente.")
}

setwd(path)  # Establecer el directorio de trabajo

# --------------------------
# 2. CARGAR LAS LIBRERÍAS NECESARIAS
# --------------------------

library(pacman)

p_load(rio, # import/export data
       tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # Creating predictive models
       scatterplot3d, # For 3D visualization
       plotly,
       doParallel)  # Visualización interactiva

# --------------------------
# 3. CARGA DE DATOS
# --------------------------

train_hogares <- readRDS("stores/train_hogares_full.Rds")  # Cargar datos de entrenamiento
test_hogares <- readRDS("stores/test_hogares_full.Rds")  # Cargar datos de prueba

train_hogares <- as.data.frame(train_hogares)  # Convertir a data.frame
test_hogares <- as.data.frame(test_hogares)  # Convertir a data.frame



cl <- makeCluster(detectCores() - 1)  # Usa todos los núcleos menos 1
registerDoParallel(cl)



# --------------------------
# 4. ENTRENAMIENTO DEL MODELO
# --------------------------

set.seed(123)  # Fijar semilla para reproducibilidad

# Convertir la variable dependiente 'Pobre' en factor para clasificación
train_hogares$Pobre <- factor(train_hogares$Pobre, levels = c(0,1), labels = c("No", "Sí"))

# Definir control de entrenamiento
fitControl <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE
)

# Ajustar el modelo con regresión logística
linear_reg_caret <- train(
  Pobre ~ P5000 + P5010 + P5090 + Npersug + Li + Lp + Depto + 
    trabajando + JH_Mujer + JH_Edad + JH_Edad2 + 
    JH_RSS_S + JH_NEduc + 
    JH_Oc + Hijos + Educ_prom + 
    Trabajadores + Subsidios + CotizaPension + Pensionado + 
    TGP + P_o, 
  data = train_hogares,  # Datos de entrenamiento
  method = "glm",  # Método de regresión logística
  family = binomial,  # Familia binomial para clasificación
  trControl = fitControl,  # Control de entrenamiento
  preProcess = c("center", "scale")  # Preprocesamiento de datos
)

# Mostrar resumen del modelo ajustado
summary(linear_reg_caret)

# --------------------------
# 5. PREDICCIÓN Y CALCULO DE CLASES
# --------------------------

# Obtener probabilidades y log-odds
train_hogares <- train_hogares %>%
  mutate(prob_hat = predict(linear_reg_caret, newdata = train_hogares, type = "prob")[, 2],  # Probabilidad clase 1
         prob_logodds = predict(linear_reg_caret, newdata = train_hogares, type = "raw"))  # Log-odds

# Regla de Bayes para asignar la clase predicha
rule <- 1/2  # Regla de Bayes
train_hogares <- train_hogares %>% mutate(Pobre_hat = ifelse(prob_hat > rule, 1, 0))  # Clase predicha

# Mostrar las primeras filas con las probabilidades y clases predichas
head(train_hogares %>% select(Pobre, prob_hat, Pobre_hat))

# --------------------------
# 6. EVALUACIÓN DE LA EXACTITUD DEL MODELO
# --------------------------

# Crear la matriz de confusión
confusion_matrix <- with(train_hogares, table(Pobre, Pobre_hat))

# Calcular la exactitud
accuracy <- (confusion_matrix[1] + confusion_matrix[4]) / sum(confusion_matrix)
accuracy  # Mostrar exactitud

# --------------------------
# 7. PREDICCIÓN SOBRE EL CONJUNTO DE PRUEBA
# --------------------------

# Realizar predicción sobre el conjunto de prueba
y_hat_reg_test <- predict(linear_reg_caret, newdata = test_hogares)
y_hat_reg_test  # Mostrar predicciones





#------ Modelo Ridge --------------------------#
ridge <- train(
  Pobre ~ P5000 + P5010 + P5090 + Npersug + Li + Lp + Depto + 
    trabajando + JH_Mujer + JH_Edad + JH_Edad2 + 
    JH_RSS_S + JH_NEduc + 
    JH_Oc + Hijos + Educ_prom + 
    Trabajadores + Subsidios + CotizaPension + Pensionado + 
    TGP + P_o, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = 0, # Ridge (alpha=0)
    lambda = seq(10000000, 20000000, by = 10000)
  ),
  preProcess = c("center", "scale")
)

# Graficar el RMSE en función de lambda
plot(ridge$results$lambda,
     ridge$results$RMSE,
     xlab = "lambda",
     ylab = "Root Mean-Squared Error (RMSE)"
)

# Obtener el mejor valor de lambda
ridge$bestTune

# Coeficientes del modelo Ridge
coef_ridge <- coef(ridge$finalModel, ridge$bestTune$lambda)
coef_ridge

# Ajustar el modelo Ridge final con el mejor lambda
modelo_ridge <- train(
  Pobre ~ P5000 + P5010 + P5090 + Npersug + Li + Lp + Depto + 
    trabajando + JH_Mujer + JH_Edad + JH_Edad2 + 
    JH_RSS_S + JH_NEduc + 
    JH_Oc + Hijos + Educ_prom + 
    Trabajadores + Subsidios + CotizaPension + Pensionado + 
    TGP + P_o, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 0, # Ridge
                         lambda = 14880000), # Lambda óptimo
  preProcess = c("center", "scale")
)

# Predicciones sobre el conjunto de prueba
y_hat_ridge <- predict(modelo_ridge, newdata = test_hogares)


#------ Modelo Lasso todo 0--------------------------#
lasso <- train(
  Pobre ~ P5000 + P5010 + P5090 + Npersug + Li + Lp + Depto + 
    trabajando + JH_Mujer + JH_Edad + JH_Edad2 + 
    JH_RSS_S + JH_NEduc + 
    JH_Oc + Hijos + Educ_prom + 
    Trabajadores + Subsidios + CotizaPension + Pensionado + 
    TGP + P_o, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = 1, # Lasso (alpha=1)
    lambda = seq(10000, 1000000, by = 1000)
  ),
  preProcess = c("center", "scale")
)

# Graficar el RMSE en función de lambda
plot(lasso$results$lambda,
     lasso$results$RMSE,
     xlab = "lambda",
     ylab = "Root Mean-Squared Error (RMSE) Lasso"
)

# Obtener el mejor valor de lambda
lasso$bestTune

# Coeficientes del modelo Lasso
coef_lasso <- coef(lasso$finalModel, lasso$bestTune$lambda)
coef_lasso

# Ajustar el modelo Lasso final con el mejor lambda
modelo_lasso <- train(
  Pobre ~ P5000 + P5010 + P5090 + Npersug + Li + Lp + Depto + 
    trabajando + JH_Mujer + JH_Edad + JH_Edad2 + 
    JH_RSS_S + JH_NEduc + 
    JH_Oc + Hijos + Educ_prom + 
    Trabajadores + Subsidios + CotizaPension + Pensionado + 
    TGP + P_o, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 1, # Lasso
                         lambda = 320000), # Lambda óptimo
  preProcess = c("center", "scale")
)

# Predicciones sobre el conjunto de prueba
y_hat_lasso <- predict(modelo_lasso, newdata = test_hogares)


#------ Modelo Elastic Net --------------------------#

EN <- train(
  Pobre ~ P5000 + P5010 + P5090 + Npersug + Li + Lp + Depto + 
    trabajando + JH_Mujer + JH_Edad + JH_Edad2 + 
    JH_RSS_S + JH_NEduc + 
    JH_Oc + Hijos + Educ_prom + 
    Trabajadores + Subsidios + CotizaPension + Pensionado + 
    TGP + P_o, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = seq(0, 1, by = 0.1),  # Variación de alpha para Elastic Net
    lambda = seq(100000, 10000000, by = 10000)
  ),
  preProcess = c("center", "scale")
)

# Obtener el mejor valor de alpha y lambda
EN$bestTune

# Coeficientes del modelo Elastic Net
coef_EN <- coef(EN$finalModel, EN$bestTune$lambda)
coef_EN

# Ajustar el modelo Elastic Net final con los mejores parámetros
modelo_EN <- train(
  Pobre ~ P5000 + P5010 + P5090 + Npersug + Li + Lp + Depto + 
    trabajando + JH_Mujer + JH_Edad + JH_Edad2 + 
    JH_RSS_S + JH_NEduc + 
    JH_Oc + Hijos + Educ_prom + 
    Trabajadores + Subsidios + CotizaPension + Pensionado + 
    TGP + P_o, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = 0.9, # Alpha óptimo para Elastic Net
    lambda = 320000), # Lambda óptimo
  preProcess = c("center", "scale")
)

# Predicciones sobre el conjunto de prueba
y_hat_EN <- predict(modelo_EN, newdata = test_hogares)
stopCluster(cl)  # Al final de la ejecución
