rm(list = ls())
gc()
closeAllConnections()

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

setwd(path)

# --------------------------
# 2. CARGA DE PAQUETES
# --------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, skimr, fastDummies, labelled, parallel, doParallel, data.table)

# --------------------------
# 3. CONFIGURACIÓN PARA PROCESAMIENTO PARALELO
# --------------------------

n_cores <- detectCores() - 1
cl <- makePSOCKcluster(n_cores) 
registerDoParallel(cl)

# --------------------------
# 4. IMPORTACIÓN DE DATOS
# --------------------------

train_hogares   <- remove_val_labels(readRDS("stores/train_hogares.Rds"))
train_personas  <- remove_val_labels(readRDS("stores/train_personas.Rds"))
test_hogares    <- remove_val_labels(readRDS("stores/test_hogares.Rds"))
test_personas   <- remove_val_labels(readRDS("stores/test_personas.Rds"))

# Convertir a data.table para optimizar rendimiento
setDT(train_hogares)
setDT(train_personas)
setDT(test_hogares)
setDT(test_personas)

# --------------------------
# 5. PROCESAMIENTO DE train_personas
# --------------------------

# Número de personas por hogar
sum_Hogar <- train_personas[, .N, by = id]
setnames(sum_Hogar, "N", "num_personas")

# Número de personas trabajando
JH <- train_personas[, .(trabajando = sum(P6240 %in% c(1, 2), na.rm = TRUE)), by = id]

# Jefe de hogar y género del jefe de hogar
JH_Mujer <- train_personas[P6050 == 1, .(JH_Mujer = mean(P6020 == 2, na.rm = TRUE)), by = id]

Hijos_train <- train_personas[, .(Hijos = sum(P6040 < 18, na.rm = TRUE)), by = id]

Dpto_train <- train_personas[, .(Dpto = unique(Depto)), by = id]  

# --------------------------
# 6. UNIÓN CON train_hogares
# --------------------------

train_hogares <- merge(train_hogares, sum_Hogar, by = "id", all.x = TRUE)
train_hogares <- merge(train_hogares, JH, by = "id", all.x = TRUE)
train_hogares <- merge(train_hogares, JH_Mujer, by = "id", all.x = TRUE)
train_hogares <- merge(train_hogares, Hijos_train, by = "id", all.x = TRUE)
# --------------------------
# 7. TRANSFORMACIONES LOGARÍTMICAS SEGURAS
# --------------------------

cols_log <- c("Lp", "P5100", "P5130", "P5140", "Ingtotugarr")

train_hogares[, (cols_log) := lapply(.SD, function(x) log(ifelse(is.na(x) | x <= 0, 1, x))), .SDcols = cols_log]

# Reemplazar valores no finitos por 0
train_hogares[, (cols_log) := lapply(.SD, function(x) ifelse(is.nan(x) | x == -Inf, 0, x)), .SDcols = cols_log]

# --------------------------
# 8. IMPUTACIÓN DE NAs CON 0
# --------------------------

# Imputar NAs con 0 en todas las columnas numéricas de train_hogares
num_cols <- names(train_hogares)[sapply(train_hogares, is.numeric)]
train_hogares[, (num_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols]

# Lo mismo para test_hogares
num_cols_test <- names(test_hogares)[sapply(test_hogares, is.numeric)]
test_hogares[, (num_cols_test) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = num_cols_test]

# --------------------------
# 9. PROCESAMIENTO DE test_personas
# --------------------------

sum_Hogar_test <- test_personas[, .N, by = id]
setnames(sum_Hogar_test, "N", "num_personas")

JH_test <- test_personas[, .(trabajando = sum(P6240 %in% c(1, 2), na.rm = TRUE)), by = id]

JH_Mujer_test <- test_personas[P6050 == 1, .(JH_Mujer = mean(P6020 == 2, na.rm = TRUE)), by = id]

Hijos_test <- test_personas[, .(Hijos = sum(P6040 < 18, na.rm = TRUE)), by = id]

Dpto_test <- test_personas[, .(Dpto = unique(Depto)), by = id]  


# Unir con test_hogares
test_hogares <- merge(test_hogares, sum_Hogar_test, by = "id", all.x = TRUE)
test_hogares <- merge(test_hogares, JH_test, by = "id", all.x = TRUE)
test_hogares <- merge(test_hogares, JH_Mujer_test, by = "id", all.x = TRUE)
test_hogares <- merge(test_hogares, Hijos_test, by = "id", all.x = TRUE) 
# --------------------------
# 10. GUARDAR ARCHIVOS PROCESADOS
# --------------------------

dir.create("stores", showWarnings = FALSE)
saveRDS(train_hogares, file = "stores/train_hogares_full.rds")
saveRDS(test_hogares, file = "stores/test_hogares_full.rds")

# --------------------------
# 11. LIMPIEZA FINAL
# --------------------------

rm(train_hogares, test_hogares, train_personas, test_personas)
gc()
stopCluster(cl)
