rm(list = ls())
gc()
closeAllConnections()

# este setwd por ahora no esta funcionando, toca que cada uno lo haga independiente mientras logro que funcione. att: Juan Jose

user <- Sys.getenv("USERNAME")

if (user == "judel") {
  path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/segunda parte/Taller 2/input"
} else if(user == "---") {
  path <- "-----"
}else if(user == "-evaluador-") {
  path <- "-----"
}

setwd(path)


pacman:: p_load(tidyverse,skimr,fastDummies,labelled,parallel,doParallel)

n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)


####IMPORTAR BASES 
train_hogares <- read.csv("train_hogares.csv", header = TRUE, sep = ",")
train_personas <- read.csv("train_personas.csv", header = TRUE, sep = ",")
test_hogares <- read.csv("test_hogares.csv", header = TRUE, sep = ",")
test_personas <- read.csv("test_personas.csv", header = TRUE, sep = ",")


# Crear carpeta de almacenamiento si no existe
if (!dir.exists("stores")) dir.create("stores")

# Guardar en formato RDS para futuras cargas rápidas
saveRDS(train_hogares, "stores/train_hogares.rds")
saveRDS(test_hogares, "stores/test_hogares.rds")
saveRDS(train_personas, "stores/train_personas.rds")
saveRDS(test_personas, "stores/test_personas.rds")
