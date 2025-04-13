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
} else if (user == "e125379") {
  path <- "C:\\Users\\e125379\\OneDrive - Mastercard\\8. Uniandes\\6. Big Data\\3. Taller 2\\Data\\"
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
       tidyverse, 
       glmnet, 
       caret, 
       scatterplot3d, 
       plotly,
       car,
       doParallel, 
       skimr,
       corrplot, 
       ggplot2, 
       PROC) 

# --------------------------
# 3. CARGA DE DATOS
# --------------------------
# Bases de entrenamiento y de testeo
train_hogares <- readRDS("stores/train_hogares_full.Rds") 
test_hogares <- readRDS("stores/test_hogares_full.Rds") 

train_hogares <- as.data.frame(train_hogares) 
test_hogares <- as.data.frame(test_hogares) 

# --------------------------
# 5. Medir correlación entre variables (Optimizar variables)
# --------------------------
# Ver correlación de variables para entender relaciones entre estas
numeric_vars <- train_hogares[sapply(train_hogares, is.numeric)]
cor_matrix <- cor(numeric_vars, use = "complete.obs")

png("correlation_plot_variables.png", width = 800, height = 600)

corrplot(cor_matrix,
         method = "color", 
         type = "upper", 
         tl.cex = 0.7, 
         tl.col = "black", 
         diag = FALSE)

dev.off()

# --------------------------
# 6. Ajustes Random Forest
# --------------------------
#Semilla
set.seed(123)

#Si bien hay una base test, esta partición permite sacar métricas importantes del modelo
#Test no tiene indicador de pobreza
inTrain <- createDataPartition(
  y = train_hogares$Pobre,
  p = .7, # 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

train_muestra <- train_hogares[ inTrain,]
test_muestra  <- train_hogares[-inTrain,]

# Cambiar a factor Pobreza
train_muestra$Pobre <- factor(train_muestra$Pobre, 
                              levels = c(0, 1),
                              labels = c("No_Pobre", "Pobre"))
test_muestra$Pobre <- factor(test_muestra$Pobre, 
                              levels = c(0, 1),
                              labels = c("No_Pobre", "Pobre"))

# Ajustes del modelo
fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...),
    caret::defaultSummary(...)
  )
}

fitControl <- trainControl(
  method = "cv",  # Validación cruzada
  number = 5,
  summaryFunction = fiveStats,
  classProbs = TRUE,
  verbose = FALSE,
  savePredictions = T,
  sampling = "smote"
)

# Ajustes para modelo extenso
tunegrid_rf <- expand.grid(
  mtry = c(2, 4, 6, 8, 10, 12, 16 , 20),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10, 20)
)

# Ajustes para modelo inicial o prueba
tunegrid_rf_fast <- expand.grid(
  mtry = c(4, 8),
  splitrule = c("gini"),
  min.node.size = c(1, 10)
)
 
# --------------------------
# 7. Random Forest Prueba (Importancia Variables)
# -------------------------- 
# Random Forest inicial (Prueba)
rforest <- train(Pobre ~ JH_Mujer + JH_Edad + JH_RSS_S +
                 JH_NEduc + JH_CotizaPension + JH_Oc + JH_NoSeguro + Hijos + Educ_prom + 
                 JH_Independiente + JH_Microempresa +JH_Pequena  + JH_Mediana_Grande + Adultos + Trabajadores + Subsidios + Pensionado + JH_Trabaja +JH_HorasExt +
                 OtrosIngresos + AyudasEco + TGP + JH_Vulnerable + JH_Alim + JH_Horas_Trabajo + JH_Personas_Trabajo + JH_Vivienda +
                 JH_Segundo_Trabajo + P5000 + P5010 + P5090 + tasa_desempleo + P5130 + Nper + Lp,
               data=train_muestra, 
             trControl = fitControl,
               tuneGrid = tunegrid_rf_fast,
               method ="ranger",
               ntree = 5,
               na.action = na.pass,
               importance = "impurity")

# --------------------------
# 8. Seleccionar Variables modelo final (Importancia)
# -------------------------- 
# Importancia de variables en predicción
varImpPlot <- varImp(rforest)
plot(varImpPlot, top = 20) 

jpeg("importancia_variables.jpg", width = 800, height = 600)
plot(varImpPlot, top = 20, main = "Importancia de variables")
dev.off()

# Mejor combinación hiperparámetros
print(rforest$bestTune)
print(rforest$results)

#Matriz de Confusión
prediccion_test <- predict(rforest, newdata = test_muestra)
cm <- confusionMatrix(prediccion_test, test_muestra$Pobre, positive = "Pobre")

tabla_confusion <- as.table(cm$table)
tabla_confusion_df <- as.data.frame(cm$table)
xtable(tabla_confusion_df)

ggplot(tabla_confusion_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, size = 6, color = "white") +
  scale_fill_gradient(low = "#6baed6", high = "#08306b") +
  theme_minimal(base_size = 15) +
  labs(title = "Matriz de Confusión", x = "Valor Real", y = "Valor Predicho")


ggsave("matriz_confusion.jpeg", width = 8, height = 6, dpi = 300)

# Tabla de resultado 
accuracy <- cm$overall["Accuracy"]
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Sensitivity"]      
specificity <- cm$byClass["Specificity"]
f1 <- cm$byClass["F1"]

print(tabla_metricas)
tabla_metricas <- data.frame(
  Métrica = c("Accuracy", "Sensibilidad (Recall)", "Especificidad", "Precisión", "F1-Score"),
  Valor = c(accuracy, recall, specificity, precision, f1)
)
xtable(tabla_metricas, digits = 4)


# --------------------------
# 8. Random Forest Final 
# -------------------------- 

# Random Forest real con ajustes para mejorar rendimiento
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
rforest <- train(Pobre ~ JH_RSS_S + Hijos + Subsidios + Nper + Educ_prom + P5130 +
                   TGP + Trabajadores + JH_CotizaPension + Lp + JH_NEduc + tasa_desempleo +
                   P5000 + JH_Edad + P5090 + P5010 + Adultos + JH_NoSeguro + JH_Personas_Trabajo + AyudasEco,
                 data = train_muestra, 
                 trControl = fitControl,
                 tuneGrid = tunegrid_rf,
                 method = "ranger",
                 ntree = 300,
                 na.action = na.pass,
                 savePredictions = "final",
                 verbose = FALSE,
                 importance = "impurity")

# Importancia final de las variables
varImpPlot <- varImp(rforest)
tabla_importancia <- data.frame(Variable = rownames(varImpPlot$importance),                                Importancia = varImpPlot$importance$Overall)
xtable(tabla_importancia)

print(tabla_latex, type = "latex")
plot(varImpPlot, top = 20) 

jpeg("importancia_variables_final.jpg", width = 800, height = 600)
plot(varImpPlot, top = 20, main = "Importancia de variables")
dev.off()

stopCluster(cl)
registerDoSEQ()


# --------------------------
# 9. Métricas Importantes
# -------------------------- 
# Mejor combinación de hiperparámetros
print(rforest$bestTune)
print(rforest$results)

# Base de predicción para Kaggle
prediccion_test <- predict(rforest, newdata = test_hogares)
predictSample <- data.frame(
  id = test_hogares$id, 
  pobre = as.numeric(prediccion_test) - 1 
)
ruta_salida <- "predicciones_test_v5.csv"  
write.csv(predictSample, ruta_salida, row.names = FALSE)

# --------------------------
# 10. Análisis Adicionales
# --------------------------

# Matriz de Confusión
prediccion_test <- predict(rforest, newdata = test_muestra)
cm <- confusionMatrix(prediccion_test, test_muestra$Pobre, positive = "Pobre")

tabla_confusion <- as.table(cm$table)
tabla_confusion_df <- as.data.frame(cm$table)
xtable(tabla_confusion_df)

ggplot(tabla_confusion_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, size = 6, color = "white") +
  scale_fill_gradient(low = "#6baed6", high = "#08306b") +
  theme_minimal(base_size = 15) +
  labs(title = "Matriz de Confusión", x = "Valor Real", y = "Valor Predicho")


ggsave("matriz_confusion.jpeg", width = 8, height = 6, dpi = 300)

# Tabla de resultados FINAL
accuracy <- cm$overall["Accuracy"]
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Sensitivity"]   
specificity <- cm$byClass["Specificity"]
f1 <- cm$byClass["F1"]
kaggle <- 0.6703
print(tabla_metricas)

tabla_metricas <- data.frame(
  Métrica = c("Accuracy", "Sensibilidad (Recall)", "Especificidad", "Precisión", "F1-Score", "Kaggle"),
  Valor = c(accuracy, recall, specificity, precision, f1, kaggle)
)

xtable(tabla_metricas, digits = 4)

# --------------------------
# 11. Curva ROC (Adicional)
# --------------------------

# Análisis de AUC y curva ROC 
prediccion_test <- predict(rforest, newdata = test_muestra, type = "prob")[,2]
roc_curve <- roc(test_muestra$Pobre, prediccion_test)
print(roc_curve)
auc_value <- auc(roc_curve)
print(paste("AUC: ", round(auc_value, 2)))
jpeg("curva_ROC.jpg", width = 800, height = 600, quality = 100)
plot(roc_curve, col = "blue", 
     main = paste("Curva ROC (AUC = ", round(auc_value, 2), ")", sep = ""), 
     lwd = 2)
dev.off()


