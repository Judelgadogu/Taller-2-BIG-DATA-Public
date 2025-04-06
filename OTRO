#Random Forest  ----------------------------------------------------------------
rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)


p_load(tidyverse,
       here,
       skimr,
       VIM,
       glmnet,    # Modelos de regresión regularizados (EN, Lasso y Ridge).
       caret,
       MLmetrics, # Calcular metricas
       MLeval,    # Evaluar modelos de clasificación
       Metrics, 
       ggplot2,
       dplyr,
       ranger,
       rio
)

## Ángel y Juan Pablo

## Cargar datos ----------------------------------------------------------------

db <- readRDS('db_final.rds') %>% 
  as_tibble()

test_hogares <- read.csv("test_hogares.csv") %>% 
  as_tibble()

# Modelo -----------------------------------------------------------------------


head(db)

Pobre_num <- db$Pobre


#Crear base train

trainRF <- db %>% 
  filter(test == 0) %>% 
  select(-test)

#Crear base test

testRF <- db %>% 
  filter(test == 1) %>% 
  select(-test)

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(trainRF))
missing_tab<-data.frame(
  Miss_val=missing_values
)
print(missing_tab)



trainRF$Pobre <- as.factor(trainRF$Pobre)
trainRF$Depto <- as.factor(trainRF$Depto)
trainRF$cabecera <- as.factor(trainRF$cabecera)
trainRF$prop_vivienda <- as.factor(trainRF$prop_vivienda)
trainRF$Dominio <- as.factor(trainRF$Dominio)

#Definir control

fitControl <- trainControl(method = 'cv', number = 10)

#Ideas: Usar prop o conteo

#Random Forest 2 ---------------------------------------------------------------

set.seed(1112) #Se fija semilla para reproducibilidad

trainRF$Pobre <- factor(trainRF$Pobre,
                        levels = c(0, 1),
                        labels = c("NoPobre", "Pobre"))


fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)


mtry_grid<-expand.grid(mtry = c(8, 16, 32, 48, 51), # 51 incluye bagging
                       min.node.size= c(1, 5, 10, 50, 100, 200, 300, 500, 1000), #controla la complejidad del arbol
                       splitrule= 'gini') # tomamos gini como splitrule 
mtry_grid



cv_RForest <- train(Pobre ~ cabecera + Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + 
                      credit_vivienda_mes + arriendo_hipotetico + arriendo + Npersonas + 
                      Nper_unidad_gasto + linea_indigencia + linea_pobreza + factor_exp + Depto + 
                      factor_ex_dep  + tamaño_hogar + prima_servicios + 
                      prima_navidad + prima_vacaciones + bonificaciones_anuales + 
                      horas_empleo_principal + cotiza_pension + empleo_secundario + 
                      horas_empleo_secundario + quiere_trabajar_mas + pensionado + trabaja_solo + 
                      microempresa + pequeña_empresa + mediana_empresa + gran_empresa + 
                      dicotom_ingxhorasextra + dicotom_primas + dicotom_bonificaciones + 
                      dicotom_subsalimentacion + dicotom_substransporte + dicotom_subsfamiliar + 
                      dicotom_subseduc + dicotom_alimentosextra + dicotom_viviendapago + 
                      dicotom_transporteempresa + dicotom_ingresosextraespecie + mujer + 
                      menor_15 + mayor_60 + edad + segur_social + segur_subsidiado + 
                      educ_sup + tiempo_empresa, 
                    data = trainRF, 
                    method = "ranger", # llamamos el paquete del metodo a utilizar
                    trControl = ctrl,
                    metric="F1", # metrica a optimizar
                    tuneGrid = mtry_grid,
                    ntree=500,
                    na.action = na.pass
                    )

cv_RForest


# Predicciones
predicciones <- predict(cv_RForest, data = testRF)

# Resultados
resultados <- data.frame(
  id = testRF$id,
  prediccion = predicciones
)

write.csv(resultados, "predicciones_por_id.csv", row.names = FALSE)
