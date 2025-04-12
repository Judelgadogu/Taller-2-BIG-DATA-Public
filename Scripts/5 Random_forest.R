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
       tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # Creating predictive models
       scatterplot3d, # For 3D visualization
       plotly,
       car,
       doParallel, 
       skimr,
       corrplot, 
       ggplot2)  # Visualización interactiva

# --------------------------
# 3. CARGA DE DATOS
# --------------------------

train_hogares <- readRDS("stores/train_hogares_full.Rds")  # Cargar datos de entrenamiento
test_hogares <- readRDS("stores/test_hogares_full.Rds")  # Cargar datos de prueba

train_hogares <- as.data.frame(train_hogares)  # Convertir a data.frame
test_hogares <- as.data.frame(test_hogares)  # Convertir a data.frame

# --------------------------
# 5. Medir correlación entre variables (Optimizar variables)
# --------------------------
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

set.seed(123)

inTrain <- createDataPartition(
  y = train_hogares$Pobre,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

train_muestra <- train_hogares[ inTrain,]
test_muestra  <- train_hogares[-inTrain,]


train_muestra$Pobre <- factor(train_muestra$Pobre, 
                              levels = c(0, 1),
                              labels = c("No_Pobre", "Pobre"))

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

tunegrid_rf <- expand.grid(
  mtry = c(2, 4, 6, 8, 10, 12, 16 , 20),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10, 20)
)

tunegrid_rf_fast <- expand.grid(
  mtry = c(4, 8),
  splitrule = c("gini"),
  min.node.size = c(1, 10)
)

#tunegrid_rf_final <- expand.grid(
#  mtry = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),  
#  splitrule = c("gini", "extratrees"),           
#min.node.size = c(1, 5, 10, 20, 30, 40)        
#)
 
# --------------------------
# 7. Random Forest Prueba (Importancia Variables)
# -------------------------- 

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
varImpPlot <- varImp(rforest)
plot(varImpPlot, top = 20) 

jpeg("importancia_variables.jpg", width = 800, height = 600)
plot(varImpPlot, top = 20, main = "Importancia de variables")
dev.off()

# --------------------------
# 8. Random Forest Final 
# -------------------------- 
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
rforest <- train(Pobre ~ JH_RSS_S + Hijos + Subsidios + Nper + Educ_prom + P5130 +
                   TGP + Trabajadores + JH_CotizaPension + Lp + JH_NEduc + tasa_desempleo +
                   P5000 + JH_Edad + P5090 + P5010 + Adultos + JH_NoSeguro + JH_Personas_Trabajo + AyudasEco,
                 data = train_muestra, 
                 trControl = fitControl,
                 tuneGrid = tunegrid_rf,
                 method = "ranger",
                 ntree = 250,
                 na.action = na.pass,
                 savePredictions = "final",
                 verbose = FALSE,
                 importance = "impurity")

varImpPlot <- varImp(rforest)
plot(varImpPlot, top = 20) 

jpeg("importancia_variables_final.jpg", width = 800, height = 600)
plot(varImpPlot, top = 20, main = "Importancia de variables")
dev.off()

stopCluster(cl)
registerDoSEQ()


# --------------------------
# 9. Métricas Importantes
# -------------------------- 
# Imprimir los mejores parámetros encontrados
print(rforest$bestTune)
print(rforest$results)

prediccion_test <- predict(rforest, newdata = test_hogares)

predictSample <- data.frame(
  id = test_hogares$id, 
  pobre = as.numeric(prediccion_test) - 1  # Convertir de factor a numérico (0 = No_Pobre, 1 = Pobre)
)

ruta_salida <- "predicciones_test_v4.csv"  
write.csv(predictSample, ruta_salida, row.names = FALSE)

# --------------------------
# 10. Análisis Adicionales
# --------------------------
prediccion_test <- predict(rforest, newdata = test_muestra)

cm <- confusionMatrix(prediccion_test, test_muestra$Pobre, positive = "1")

tabla_confusion <- as.table(cm$table)
metricas <- as.data.frame(t(cm$byClass))  # Recall, Precision, F1, etc.
globales <- as.data.frame(t(cm$overall))  # Accuracy, Kappa, etc.

write.csv(as.data.frame(tabla_confusion), "tabla_confusion.csv", row.names = FALSE)
write.csv(metricas, "metricas_por_clase.csv")
write.csv(globales, "metricas_generales.csv")
  
ggplot(cm, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5, size = 6, color = "white") +
  scale_fill_gradient(low = "#6baed6", high = "#08306b") +
  theme_minimal(base_size = 15) +
  labs(title = "Matriz de Confusión", x = "Valor Real", y = "Valor Predicho")

ggsave("matriz_confusion.jpeg", width = 8, height = 6, dpi = 300)

# --------------------------
# 11. Curva ROC
# --------------------------
install.packages("pROC")
library(pROC)
prediccion_test <- predict(rforest, newdata = test_muestra$Pobre)
print(roc_curve)
auc_value <- auc(roc_curve)
print(paste("AUC: ", round(auc_value, 2)))
plot(roc_curve, col = "blue", 
     main = paste("Curva ROC (AUC = ", round(auc_value, 2), ")", sep = ""), 
     lwd = 2)


