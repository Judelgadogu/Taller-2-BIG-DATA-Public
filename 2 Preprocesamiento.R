rm(list = ls())
gc()
closeAllConnections()

# --------------------------
# 1. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
# --------------------------

user <- Sys.getenv("USERNAME")

if (user == "judel") {
  path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/segunda parte/Taller 2/input"
} else {
  stop("Usuario no reconocido. Ajusta el 'path' manualmente.")
}

setwd(path)

# --------------------------
# 2. CARGA DE PAQUETES
# --------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, skimr, fastDummies, labelled, parallel, doParallel, data.table, fastDummies)

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

setDT(train_hogares)
setDT(train_personas)
setDT(test_hogares)
setDT(test_personas)

# --------------------------
# 5. PROCESAMIENTO DE train_personas
# --------------------------

factor_variables_personas <- c('P6100', 'P6920')

train_personas[, (factor_variables_personas) := lapply(.SD, as.factor), .SDcols = factor_variables_personas]

# Generar dummies sin eliminar la variable original
train_personas <- dummy_cols(train_personas, 
                             select_columns = factor_variables_personas, 
                             remove_first_dummy = FALSE,  
                             ignore_na = TRUE, 
                             split = "_",
                             remove_selected_columns = FALSE)  #

# Verificar que las nuevas columnas se crearon correctamente
grep("P6100|P6920", names(train_personas), value = TRUE)

train_personas[, `:=` (
  P6100_3 = as.numeric(as.character(P6100_3)),
  P6920_1 = as.numeric(as.character(P6920_1)),
  P6920_3 = as.numeric(as.character(P6920_3))
)]


sum_Hogar <- train_personas[, .N, by = id]
setnames(sum_Hogar, "N", "num_personas")

JH <- train_personas[P6050 == 1, .(trabajando = sum(P6240 %in% c(1, 2), na.rm = TRUE)), by = id]
JH_Mujer <- train_personas[P6050 == 1, .(JH_Mujer = mean(P6020 == 2, na.rm = TRUE)), by = id]
JH_Edad <- train_personas[P6050 == 1, .(JH_Edad = mean(P6040, na.rm = TRUE)), by = id]
JH_Edad2 <- train_personas[P6050 == 1, .(JH_Edad2 = mean(P6040^2, na.rm = TRUE)), by = id]
JH_RSS_S <- train_personas[P6050 == 1, .(JH_RSS_S = ifelse(is.na(P6100_3), 0, P6100_3)), by = id]
JH_NEduc <- train_personas[P6050 == 1, .(JH_NEduc = mean(P6210, na.rm = TRUE)), by = id]
JH_CotizaPension <- train_personas[P6050 == 1, .(JH_CotizaPension = ifelse(is.na(P6920_1), 0, P6920_1)), by = id]
JH_Pensionado <- train_personas[P6050 == 1, .(JH_Pensionado = ifelse(is.na(P6920_3), 0, P6920_3)), by = id]
JH_Oc <- train_personas[P6050 == 1, .(JH_Oc = as.numeric(ifelse(is.na(Oc), 0, Oc))), by = id]
JH_Des <- train_personas[P6050 == 1, .(JH_Des = as.numeric(ifelse(is.na(Des), 0, Des))), by = id]
JH_Ina <- train_personas[P6050 == 1, .(JH_Ina = as.numeric(ifelse(is.na(Ina), 0, Ina))), by = id]
JH_NoSeguro <- train_personas[P6050 == 1, .(JH_NoSeguro = as.numeric(P6090 == 2)), by = id]
JH_RSS_Subsidiado <- train_personas[P6050 == 1, .(JH_RSS_Subsidiado = as.numeric(P6100 == 3)), by = id]
JH_BasicaSecundaria <- train_personas[P6050 == 1, .(JH_BasicaSecundaria = as.numeric(P6210 %in% c(1, 2, 3, 4, 9))), by = id]
JH_Media <- train_personas[P6050 == 1, .(JH_Media = as.numeric(P6210 == 5)), by = id]
JH_Trabaja <- train_personas[P6050 == 1, .(JH_Trabaja = as.numeric(P6240 == 1)), by = id]





# Variables adicionales
Educ_prom <- train_personas[, .(Educ_prom = mean(P6210, na.rm = TRUE)), by = id]
Adultos <- train_personas[, .(Adultos = sum(P6040 >= 18, na.rm = TRUE)), by = id]
Trabajadores <- train_personas[, .(Trabajadores = sum(P6240 %in% c(1, 2), na.rm = TRUE)), by = id]
Hijos <- train_personas[, .(Hijos = sum(P6040 < 18, na.rm = TRUE)), by = id]

# Nuevas variables solicitadas
Subsidios <- train_personas[, .(Subsidios = sum(P6585s1, na.rm = TRUE)), by = id]
CotizaPension <- train_personas[, .(CotizaPension = sum(P6920 == 1, na.rm = TRUE)), by = id]
Pensionado <- train_personas[, .(Pensionado = sum(P6920 == 3, na.rm = TRUE)), by = id]
Ingresos_AlquilerPensiones <- train_personas[, .(Ingresos_AlquilerPensiones = as.integer(any(P7495, na.rm = TRUE))), by = id]
OtrosIngresos <- train_personas[, .(OtrosIngresos = as.integer(any(P7505, na.rm = TRUE))), by = id]
AyudasEco <- train_personas[, .(AyudasEco = as.integer(any(P7510s3, na.rm = TRUE))), by = id]
TGP <- train_personas[, .(TGP = sum(Ina, na.rm = TRUE) / sum(Pet, na.rm = TRUE)), by = id]
P_o <- train_personas[, .(P_o = sum(Oc, na.rm = TRUE) / sum(Pet, na.rm = TRUE)), by = id]
tasa_desempleo <- train_personas[, .(tasa_desempleo = sum(Des, na.rm = TRUE) / (sum(Oc, na.rm = TRUE) + sum(Des, na.rm = TRUE))), by = id]

# --------------------------
# 6. UNIÓN CON train_hogares
# --------------------------

train_hogares_full <- Reduce(function(x, y) merge(x, y, by = "id", all.x = TRUE),
                        list(train_hogares, sum_Hogar, JH, JH_Mujer,JH_Edad, JH_Edad2, JH_RSS_S,JH_RSS_Subsidiado,
                             JH_NEduc, JH_CotizaPension, JH_Pensionado, JH_Oc, JH_Des, JH_Ina, JH_NoSeguro,JH_BasicaSecundaria, JH_Media, Hijos, Educ_prom, 
                             Adultos, Trabajadores, Subsidios, CotizaPension, Pensionado, JH_Trabaja,
                             Ingresos_AlquilerPensiones, OtrosIngresos, AyudasEco, TGP, P_o, tasa_desempleo))


train_hogares_full <- train_hogares_full %>%
  select(-P5100, -P5130, -P5140, -tasa_desempleo)

train_hogares_full$TGP[is.na(train_hogares_full$TGP)] <- 0
train_hogares_full$P_o[is.na(train_hogares_full$P_o)] <- 0
train_hogares_full$JH_NoSeguro[is.na(train_hogares_full$JH_NoSeguro)] <- 0
train_hogares_full$JH_RSS_Subsidiado[is.na(train_hogares_full$JH_RSS_Subsidiado)] <- 0




na_counts <- colSums(is.na(train_hogares_full))

na_counts[na_counts > 0]




# --------------------------
# 7. PROCESAMIENTO DE test_personas
# --------------------------

factor_variables_personas <- c('P6100', 'P6920')

test_personas[, (factor_variables_personas) := lapply(.SD, as.factor), .SDcols = factor_variables_personas]

# Generar dummies sin eliminar la variable original
test_personas <- dummy_cols(test_personas, 
                            select_columns = factor_variables_personas, 
                            remove_first_dummy = FALSE,  
                            ignore_na = TRUE, 
                            split = "_",
                            remove_selected_columns = FALSE)  

# Verificar que las nuevas columnas se crearon correctamente
grep("P6100|P6920", names(test_personas), value = TRUE)

# Convertir las dummies a numéricas
test_personas[, `:=` (
  P6100_3 = as.numeric(as.character(P6100_3)),
  P6920_1 = as.numeric(as.character(P6920_1)),
  P6920_3 = as.numeric(as.character(P6920_3))
)]

# Variables agregadas por hogar
sum_Hogar <- test_personas[, .N, by = id]
setnames(sum_Hogar, "N", "num_personas")

JH <- test_personas[P6050 == 1, .(trabajando = sum(P6240 %in% c(1, 2), na.rm = TRUE)), by = id]
JH_Mujer <- test_personas[P6050 == 1, .(JH_Mujer = mean(P6020 == 2, na.rm = TRUE)), by = id]
JH_Edad <- test_personas[P6050 == 1, .(JH_Edad = mean(P6040, na.rm = TRUE)), by = id]
JH_Edad2 <- test_personas[P6050 == 1, .(JH_Edad2 = mean(P6040^2, na.rm = TRUE)), by = id]
JH_RSS_S <- test_personas[P6050 == 1, .(JH_RSS_S = ifelse(is.na(P6100_3), 0, P6100_3)), by = id]
JH_NEduc <- test_personas[P6050 == 1, .(JH_NEduc = mean(P6210, na.rm = TRUE)), by = id]
JH_CotizaPension <- test_personas[P6050 == 1, .(JH_CotizaPension = ifelse(is.na(P6920_1), 0, P6920_1)), by = id]
JH_Pensionado <- test_personas[P6050 == 1, .(JH_Pensionado = ifelse(is.na(P6920_3), 0, P6920_3)), by = id]
JH_Oc <- test_personas[P6050 == 1, .(JH_Oc = as.numeric(ifelse(is.na(Oc), 0, Oc))), by = id]
JH_Des <- test_personas[P6050 == 1, .(JH_Des = as.numeric(ifelse(is.na(Des), 0, Des))), by = id]
JH_Ina <- test_personas[P6050 == 1, .(JH_Ina = as.numeric(ifelse(is.na(Ina), 0, Ina))), by = id]
JH_NoSeguro <- test_personas[P6050 == 1, .(JH_NoSeguro = as.numeric(P6090 == 2)), by = id]
JH_NoSeguro <- test_personas[P6050 == 1, .(JH_NoSeguro = as.numeric(P6090 == 2)), by = id]
JH_RSS_Subsidiado <- test_personas[P6050 == 1, .(JH_RSS_Subsidiado = as.numeric(P6100 == 3)), by = id]
JH_BasicaSecundaria <- test_personas[P6050 == 1, .(JH_BasicaSecundaria = as.numeric(P6210 %in% c(1, 2, 3, 4, 9))), by = id]
JH_Media <- test_personas[P6050 == 1, .(JH_Media = as.numeric(P6210 == 5)), by = id]
JH_Trabaja <- test_personas[P6050 == 1, .(JH_Trabaja = as.numeric(P6240 == 1)), by = id]



# Variables adicionales (sin filtrar por jefe de hogar)
Educ_prom <- test_personas[, .(Educ_prom = mean(P6210, na.rm = TRUE)), by = id]
Adultos <- test_personas[, .(Adultos = sum(P6040 >= 18, na.rm = TRUE)), by = id]
Trabajadores <- test_personas[, .(Trabajadores = sum(P6240 %in% c(1, 2), na.rm = TRUE)), by = id]
Hijos <- test_personas[, .(Hijos = sum(P6040 < 18, na.rm = TRUE)), by = id]

# Variables nuevas agregadas
Subsidios <- test_personas[, .(Subsidios = sum(P6585s1, na.rm = TRUE)), by = id]
CotizaPension <- test_personas[, .(CotizaPension = sum(P6920 == 1, na.rm = TRUE)), by = id]
Pensionado <- test_personas[, .(Pensionado = sum(P6920 == 3, na.rm = TRUE)), by = id]
Ingresos_AlquilerPensiones <- test_personas[, .(Ingresos_AlquilerPensiones = as.integer(any(P7495, na.rm = TRUE))), by = id]
OtrosIngresos <- test_personas[, .(OtrosIngresos = as.integer(any(P7505, na.rm = TRUE))), by = id]
AyudasEco <- test_personas[, .(AyudasEco = as.integer(any(P7510s3, na.rm = TRUE))), by = id]
TGP <- test_personas[, .(TGP = sum(Ina, na.rm = TRUE) / sum(Pet, na.rm = TRUE)), by = id]
P_o <- test_personas[, .(P_o = sum(Oc, na.rm = TRUE) / sum(Pet, na.rm = TRUE)), by = id]
tasa_desempleo <- test_personas[, .(tasa_desempleo = sum(Des, na.rm = TRUE) / (sum(Oc, na.rm = TRUE) + sum(Des, na.rm = TRUE))), by = id]
# --------------------------
# 8. UNIÓN CON test_hogares
# --------------------------

test_hogares_full <- Reduce(function(x, y) merge(x, y, by = "id", all.x = TRUE),
                        list(test_hogares, sum_Hogar, JH, JH_Mujer,JH_Edad, JH_Edad2, JH_RSS_S,JH_RSS_Subsidiado,
                             JH_NEduc, CotizaPension, Pensionado, JH_Oc, JH_Des, JH_Ina, JH_NoSeguro,JH_BasicaSecundaria, JH_Media,JH_Trabaja,
                             Hijos, Educ_prom,Adultos, Trabajadores, Subsidios, CotizaPension,
                             Pensionado,Ingresos_AlquilerPensiones, OtrosIngresos, AyudasEco, TGP,
                             P_o, tasa_desempleo))



test_hogares_full <- test_hogares_full %>%
  select(-P5100, -P5130, -P5140, -tasa_desempleo)


test_hogares_full$TGP[is.na(test_hogares_full$TGP)] <- 0
test_hogares_full$P_o[is.na(test_hogares_full$P_o)] <- 0
test_hogares_full$JH_NoSeguro[is.na(test_hogares_full$JH_NoSeguro)] <- 0
test_hogares_full$JH_RSS_Subsidiado[is.na(test_hogares_full$JH_RSS_Subsidiado)] <- 0
na_counts_test <- colSums(is.na(test_hogares_full))

# Mostrar solo las columnas con valores NA
na_counts_test[na_counts_test > 0]






# --------------------------
# 8. GUARDAR ARCHIVOS PROCESADOS
# --------------------------

saveRDS(train_hogares_full, file = "stores/train_hogares_full.rds")
saveRDS(test_hogares_full, file = "stores/test_hogares_full.rds")

# --------------------------
# 9. LIMPIEZA FINAL
# --------------------------

rm(list = ls())
gc()
stopCluster(cl)
