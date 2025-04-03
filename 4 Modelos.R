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
       car,
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
train_hogares$Pobre <- factor(train_hogares$Pobre, levels = c(0, 1))

# Calcular los pesos de las clases para balancearlas (en este caso usando la proporción de clases)
class_weights <- ifelse(train_hogares$Pobre == 1, 1 / sum(train_hogares$Pobre == 1), 1 / sum(train_hogares$Pobre == 0))

# Establecer control de entrenamiento
fitControl <- trainControl(
  method = "cv",  # Validación cruzada
  number = 10,  # Número de particiones en la validación cruzada
  allowParallel = TRUE
)

# Ajustar el modelo con regresión logística, usando los pesos de las clases
linear_reg_caret_weighted <- train(
  Pobre ~ JH_Mujer + JH_Edad + JH_NoSeguro+ JH_RSS_Subsidiado + JH_BasicaSecundaria + JH_Media +
    JH_Trabaja + JH_HorasExt + JH_Independiente + JH_Microempresa + JH_Pequena+ JH_Mediana_Grande + Hijos
    , 
  data = train_hogares,  # Datos de entrenamiento
  method = "glm",  # Método de regresión logística
  family = binomial,  # Familia binomial para clasificación
  trControl = fitControl,  # Control de entrenamiento
  preProcess = c("center", "scale")# Preprocesamiento de datos
  )

#weights = class_weights  # Usar pesos en el modelo


# Mostrar resumen del modelo ajustado
summary(linear_reg_caret_weighted)

# --------------------------
# 7. PREDICCIÓN SOBRE EL CONJUNTO DE ENTRENAMIENTO
# --------------------------

# Paso 1: Obtener las probabilidades predichas para el conjunto de entrenamiento
y_hat_prob_train <- predict(linear_reg_caret_weighted, newdata = train_hogares, type = "prob")[,2]  # Probabilidad de clase 1

# Paso 2: Definir un umbral (por defecto 0.5)
umbral <- 0.5
y_hat_class_train <- ifelse(y_hat_prob_train > umbral, 1, 0)  # Convertir probabilidades en clases (0 o 1)

# Paso 3: Crear la matriz de confusión para el conjunto de entrenamiento
confusion_matrix_train <- confusionMatrix(factor(y_hat_class_train, levels = c(0, 1)), factor(train_hogares$Pobre, levels = c(0, 1)))

# Paso 4: Extraer los valores de la matriz de confusión
confusion_matrix_table_train <- confusion_matrix_train$table
falsos_positivos_train <- confusion_matrix_table_train[2, 1]
falsos_negativos_train <- confusion_matrix_table_train[1, 2]
verdaderos_positivos_train <- confusion_matrix_table_train[2, 2]
verdaderos_negativos_train <- confusion_matrix_table_train[1, 1]

# Mostrar la tabla de resultados
resultados_train <- data.frame(
  Verdaderos_Positivos = verdaderos_positivos_train,
  Falsos_Positivos = falsos_positivos_train,
  Falsos_Negativos = falsos_negativos_train,
  Verdaderos_Negativos = verdaderos_negativos_train,
  Accuracy = confusion_matrix_train$overall['Accuracy'],
  Sensitivity = confusion_matrix_train$byClass['Sensitivity'],
  Specificity = confusion_matrix_train$byClass['Specificity'],
  Precision = confusion_matrix_train$byClass['Precision'],
  F1_Score = confusion_matrix_train$byClass['F1']
)

# Mostrar la tabla final
print(resultados_train)

# --------------------------
# 7. PREDICCIÓN SOBRE EL CONJUNTO DE PRUEBA
# --------------------------

# Realizar predicción sobre el conjunto de prueba
y_hat_reg_test <- predict(linear_reg_caret_weighted, newdata = test_hogares)
y_hat_reg_test  # Mostrar predicciones




#------ Modelo Ridge --------------------

train_hogares$weights <- ifelse(train_hogares$Pobre == 1, 0.79, 0.21)

ridge <- train(
  Pobre ~ JH_Mujer + JH_Edad + JH_NoSeguro + JH_RSS_Subsidiado + JH_BasicaSecundaria + JH_Media +
    JH_Trabaja + JH_HorasExt + JH_Independiente + JH_Microempresa + JH_Pequena + JH_Mediana_Grande + Hijos,
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = 0, # Ridge (alpha=0)
    lambda = 10^seq(-4, 4, length = 100) 
  ),
  preProcess = c("center", "scale")

)

#weights = train_hogares$weights  # Usar pesos en el modelo

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
  Pobre ~ JH_Mujer + JH_Edad + JH_NoSeguro + JH_RSS_Subsidiado + JH_BasicaSecundaria + JH_Media +
    JH_Trabaja + JH_HorasExt + JH_Independiente + JH_Microempresa + JH_Pequena + JH_Mediana_Grande + Hijos,
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 0, # Ridge
                         lambda = ridge$bestTune$lambda), # Lambda óptimo
  preProcess = c("center", "scale")

)

#weights = train_hogares$weights  # Usar pesos en el modelo


# Predicciones sobre el conjunto de prueba
y_hat_ridge <- predict(modelo_ridge, newdata = train_hogares)

# Obtener las probabilidades predichas para el conjunto de prueba
y_hat_prob_ridge <- predict(modelo_ridge, newdata = train_hogares, type = "prob")[,2]  # Probabilidad de clase 1

# Definir un umbral (por defecto 0.5)
umbral <- 0.5
y_hat_class_ridge <- ifelse(y_hat_prob_ridge > umbral, 1, 0)  # Convertir probabilidades en clases (0 o 1)

# Crear la matriz de confusión para el conjunto de prueba
confusion_matrix_ridge <- confusionMatrix(factor(y_hat_class_ridge, levels = c(0, 1)), factor(train_hogares$Pobre, levels = c(0, 1)))

# Extraer los valores de la matriz de confusión
confusion_matrix_table_ridge <- confusion_matrix_ridge$table
falsos_positivos_ridge <- confusion_matrix_table_ridge[2, 1]
falsos_negativos_ridge <- confusion_matrix_table_ridge[1, 2]
verdaderos_positivos_ridge <- confusion_matrix_table_ridge[2, 2]
verdaderos_negativos_ridge <- confusion_matrix_table_ridge[1, 1]

# Mostrar la tabla de resultados para el modelo Ridge
resultados_ridge <- data.frame(
  Verdaderos_Positivos = verdaderos_positivos_ridge,
  Falsos_Positivos = falsos_positivos_ridge,
  Falsos_Negativos = falsos_negativos_ridge,
  Verdaderos_Negativos = verdaderos_negativos_ridge,
  Accuracy = confusion_matrix_ridge$overall['Accuracy'],
  Sensitivity = confusion_matrix_ridge$byClass['Sensitivity'],
  Specificity = confusion_matrix_ridge$byClass['Specificity'],
  Precision = confusion_matrix_ridge$byClass['Precision'],
  F1_Score = confusion_matrix_ridge$byClass['F1']
)

# Mostrar la tabla final para el modelo Ridge
print(resultados_ridge)
# Predicciones sobre el conjunto de prueba
y_hat_ridge_test <- predict(modelo_ridge, newdata = test_hogares)
y_hat_ridge_test

#------ Modelo Lasso --------------------
# Ajustar el modelo Lasso con búsqueda más fina de lambda
lasso <- train(
  Pobre ~ JH_Mujer + JH_Edad + JH_NoSeguro + JH_RSS_Subsidiado + JH_BasicaSecundaria + JH_Media +
    JH_Trabaja + JH_HorasExt + JH_Independiente + JH_Microempresa + JH_Pequena + JH_Mediana_Grande + Hijos, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = 1, # Lasso (alpha=1)
    lambda = seq(0, 10, by = 0.01) # Rango más realista
  ),
  preProcess = c("center", "scale")
)

#,weights = train_hogares$weights  # Usar la misma variable de pesos que en Ridge

# Graficar RMSE en función de log(lambda)
plot(log(lasso$results$lambda),
     lasso$results$RMSE,
     xlab = "log(lambda)",
     ylab = "Root Mean-Squared Error (RMSE) Lasso"
)

# Obtener el mejor valor de lambda
lasso$bestTune

# Coeficientes del modelo Lasso
coef_lasso <- coef(lasso$finalModel, lasso$bestTune$lambda)
print(coef_lasso)

# Ajustar el modelo Lasso final con el mejor lambda
modelo_lasso <- train(
  Pobre ~ JH_Mujer + JH_Edad + JH_NoSeguro + JH_RSS_Subsidiado + JH_BasicaSecundaria + JH_Media +
    JH_Trabaja + JH_HorasExt + JH_Independiente + JH_Microempresa + JH_Pequena + JH_Mediana_Grande + Hijos, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = 1,  # Rango de alpha más reducido
    lambda = lasso$bestTune$lambda # Misma escala que en Ridge y Lasso
  ),
  preProcess = c("center", "scale")
)

#,weights = train_hogares$weights # 

# Predicciones sobre el conjunto de entrenamiento
y_hat_lasso <- predict(modelo_lasso, newdata = train_hogares)

# Obtener las probabilidades predichas
y_hat_prob_lasso <- predict(modelo_lasso, newdata = train_hogares, type = "prob")[,2]

# Definir un umbral (0.5 por defecto)
umbral <- 0.5
y_hat_class_lasso <- ifelse(y_hat_prob_lasso > umbral, 1, 0)

# Matriz de confusión en entrenamiento
confusion_matrix_lasso <- confusionMatrix(factor(y_hat_class_lasso, levels = c(0, 1)), factor(train_hogares$Pobre, levels = c(0, 1)))

# Extraer métricas
resultados_lasso <- data.frame(
  Verdaderos_Positivos = confusion_matrix_lasso$table[2, 2],
  Falsos_Positivos = confusion_matrix_lasso$table[2, 1],
  Falsos_Negativos = confusion_matrix_lasso$table[1, 2],
  Verdaderos_Negativos = confusion_matrix_lasso$table[1, 1],
  Accuracy = confusion_matrix_lasso$overall['Accuracy'],
  Sensitivity = confusion_matrix_lasso$byClass['Sensitivity'],
  Specificity = confusion_matrix_lasso$byClass['Specificity'],
  Precision = confusion_matrix_lasso$byClass['Precision'],
  F1_Score = confusion_matrix_lasso$byClass['F1']
)

# Mostrar la tabla de resultados en entrenamiento
print(resultados_lasso)

# Predicciones en el conjunto de prueba
y_hat_lasso_test <- predict(modelo_lasso, newdata = test_hogares)
y_hat_lasso_test

#------ Modelo Elastic Net -----------------

EN <- train(
  Pobre ~ JH_Mujer + JH_Edad + JH_NoSeguro + JH_RSS_Subsidiado + 
    JH_BasicaSecundaria + JH_Media + JH_Trabaja + JH_HorasExt + 
    JH_Independiente + JH_Microempresa + JH_Pequena + 
    JH_Mediana_Grande + Hijos, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = seq(0, 1, by = 0.1),  # Exploración más amplia de alpha
    lambda = seq(0, 10, by = 0.1)    # Exploración más detallada de lambda
  ),
  preProcess = c("center", "scale")
)

#,weights = train_hogares$weights

# Graficar el RMSE en función de lambda
plot(EN$results$lambda,
     EN$results$RMSE,
     xlab = "lambda",
     ylab = "Root Mean-Squared Error (RMSE) Elastic Net"
)

# Obtener el mejor valor de alpha y lambda
EN$bestTune

# Coeficientes del modelo Elastic Net
coef_EN <- coef(EN$finalModel, EN$bestTune$lambda)
coef_EN

# Ajustar el modelo Elastic Net final con los mejores parámetros encontrados
modelo_EN <- train(
  Pobre ~ JH_Mujer + JH_Edad + JH_NoSeguro + JH_RSS_Subsidiado + 
    JH_BasicaSecundaria + JH_Media + JH_Trabaja + JH_HorasExt + 
    JH_Independiente + JH_Microempresa + JH_Pequena + 
    JH_Mediana_Grande + Hijos, 
  data = train_hogares,
  method = 'glmnet', 
  trControl = fitControl,
  tuneGrid = expand.grid(
    alpha = EN$bestTune$alpha, # Mejor alpha encontrado
    lambda = EN$bestTune$lambda  # Mejor lambda encontrado
  ),
  preProcess = c("center", "scale")
)


#,weights = class_weights

# Predicciones sobre el conjunto de entrenamiento
y_hat_EN <- predict(modelo_EN, newdata = train_hogares)

# Obtener las probabilidades predichas para el conjunto de entrenamiento
y_hat_prob_EN <- predict(modelo_EN, newdata = train_hogares, type = "prob")[,2]  

# Definir un umbral (por defecto 0.5)
umbral <- 0.5
y_hat_class_EN <- ifelse(y_hat_prob_EN > umbral, 1, 0)

# Crear la matriz de confusión para el conjunto de entrenamiento
confusion_matrix_EN <- confusionMatrix(factor(y_hat_class_EN, levels = c(0, 1)), 
                                       factor(train_hogares$Pobre, levels = c(0, 1)))

# Extraer los valores de la matriz de confusión
confusion_matrix_table_EN <- confusion_matrix_EN$table
falsos_positivos_EN <- confusion_matrix_table_EN[2, 1]
falsos_negativos_EN <- confusion_matrix_table_EN[1, 2]
verdaderos_positivos_EN <- confusion_matrix_table_EN[2, 2]
verdaderos_negativos_EN <- confusion_matrix_table_EN[1, 1]

# Mostrar la tabla de resultados para el modelo Elastic Net
resultados_EN <- data.frame(
  Verdaderos_Positivos = verdaderos_positivos_EN,
  Falsos_Positivos = falsos_positivos_EN,
  Falsos_Negativos = falsos_negativos_EN,
  Verdaderos_Negativos = verdaderos_negativos_EN,
  Accuracy = confusion_matrix_EN$overall['Accuracy'],
  Sensitivity = confusion_matrix_EN$byClass['Sensitivity'],
  Specificity = confusion_matrix_EN$byClass['Specificity'],
  Precision = confusion_matrix_EN$byClass['Precision'],
  F1_Score = confusion_matrix_EN$byClass['F1']
)

# Mostrar la tabla final para el modelo Elastic Net
print(resultados_EN)

# Predicciones sobre el conjunto de prueba
y_hat_EN_test <- predict(modelo_EN, newdata = test_hogares)
y_hat_EN_test



stopCluster(cl)  # Al final de la ejecución




print(resultados_train)
print(resultados_ridge)
print(resultados_lasso)
print(resultados_EN)

# -----Kaggle----
predictSample <- data.frame(
  id = test_hogares$id,
  pobre = y_hat_ridge_test
)
head(predictSample)

# Obtener los valores de lambda y alpha del mejor modelo
lambda_str <- gsub("\\.", "_", as.character(round(modelo_ridge$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(modelo_ridge$bestTune$alpha))

# Crear el nombre del archivo
name <- paste0("Ridge_lambda_", lambda_str, "_alpha_", alpha_str, ".csv")

# Definir la ruta de salida
ruta_salida <- file.path("C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/segunda parte/Taller 2/input/kaggle", name)

# Crear el dataframe de predicciones
predictSample <- data.frame(
  id = test_hogares$id,
  pobre = y_hat_ridge_test
)

# Guardar el archivo CSV
write.csv(predictSample, ruta_salida, row.names = FALSE)

# Confirmar que el archivo se ha guardado
file.exists(ruta_salida)
