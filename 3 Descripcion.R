# --------------------------
# 0. LIMPIEZA INICIAL
# --------------------------
rm(list = ls())
gc()
closeAllConnections()

# --------------------------
# 1. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
# --------------------------

user <- Sys.getenv("USERNAME")

if (user == "judel") {
  path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/segunda parte/Taller 2/input"
} else if(user == "e125379") {
  path <- "C:\\Users\\e125379\\OneDrive - Mastercard\\8. Uniandes\\6. Big Data\\3. Taller 2\\Data\\"
} else if(user == "Mario") {
  path <- "C:/Users/Mario/Documents/poverty"
}else if(user == "-evaluador-") {
  path <- "-----"
}

setwd(path)


# Cargar paquetes necesarios
pacman::p_load(
  sf, sp, gridExtra, cowplot, classInt, grid, lessR, RColorBrewer, tidyverse, stargazer, caret, tidymodels, glmnet,
  parallel, doParallel, MLmetrics, themis, rattle, rlang, randomForest, mlr, rpart, rpart.plot, kableExtra
)

# Cargar datos
train_hogares <- readRDS("stores/train_hogares_full.Rds")

# Distribución de la pobreza
No_Pobres <- mean(train_hogares$Pobre == 0) * 100
Pobres <- mean(train_hogares$Pobre == 1) * 100
No_Pobres <- 79.98
Pobres <- 20.02
porcentajes <- c(No_Pobres, Pobres)
pie(
  x = porcentajes,
  labels = c(paste0("No Pobres (", No_Pobres, "%)"), paste0("Pobres (", Pobres, "%)")),
  col = c("gray", "red"),
  border = "black",
  main = "Distribución de la pobreza",
  radius = 1
)

# Convertir la variable Pobre en factor para gráficos
train_hogares_2 <- train_hogares %>%
  mutate(Pobre = factor(Pobre, levels = c(0, 1), labels = c("No Pobres", "Pobres")))

# Gráfico de pobreza por sexo del jefe de hogar
ggplot(train_hogares_2, aes(x = factor(Pobre, labels = c("No Pobres", "Pobres")), 
                            fill = factor(JH_Mujer, labels = c("Hombre", "Mujer")))) +
  geom_bar(position = "fill") +  # Proporciones en vez de conteo absoluto
  scale_fill_manual(values = c("gray", "red")) +  # Mejores colores
  labs(
    title = "Distribución de la pobreza por sexo del jefe de hogar",
    x = "Condición de pobreza",
    y = "Proporción",
    fill = "Jefe de hogar"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  
    legend.position = "top"
  )




# Gráfico de pobreza por número de hijos
ggplot(train_hogares, aes(x = Hijos, fill = factor(Pobre))) +
  geom_bar(position = "fill") +  # Muestra proporción en vez de conteo absoluto
  scale_fill_manual(values = c("gray", "red")) +  # Colores más diferenciables
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +  # Limita y ordena ejes
  labs(
    title = "Distribución de la pobreza por número de hijos",
    x = "Número de hijos",
    y = "Proporción",
    fill = "Pobre"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centrar y resaltar título
    legend.position = "top"  # Mover la leyenda arriba
  )


# Gráfico de pobreza por NPER
ggplot(train_hogares, aes(x = Nper, fill = factor(Pobre))) +
  geom_bar(position = "fill") +  # Muestra proporción en vez de conteo absoluto
  scale_fill_manual(values = c("gray", "red")) +  # Colores más diferenciables
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +  # Limita y ordena ejes
  labs(
    title = "Distribución de la pobreza por número de personas",
    x = "Número de personas",
    y = "Proporción",
    fill = "Pobre"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Centrar y resaltar título
    legend.position = "top"  # Mover la leyenda arriba
  )




# Análisis por departamento
departamentos_df <- data.frame(
  Depto = c(5, 8, 11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76, 81, 85, 86, 88, 91, 94, 95, 97, 99),
  Nombre_Depto = c("ANTIOQUIA", "ATLANTICO", "BOGOTA", "BOLIVAR", "BOYACA", "CALDAS", "CAQUETA", "CAUCA", "CESAR", 
                   "CORDOBA", "CUNDINAMARCA", "CHOCO", "HUILA", "LA GUAJIRA", "MAGDALENA", "META", "NARIÑO", 
                   "NORTE SANTANDER", "QUINDIO", "RISARALDA", "SANTANDER", "SUCRE", "TOLIMA", "VALLE", "ARAUCA", 
                   "CASANARE", "PUTUMAYO", "SAN ANDRES", "AMAZONAS", "GUAINIA", "GUAVIARE", "VAUPES", "VICHADA")
)

# Calcular distribución de pobreza
pobreza_por_dpto <- train_hogares %>%
  group_by(Depto, Pobre) %>%
  summarise(Conteo = n(), .groups = "drop") %>%
  group_by(Depto) %>%
  mutate(Prop = Conteo / sum(Conteo) * 100) %>%
  ungroup()

# Unir con nombres de departamentos
pobreza_por_dpto <- pobreza_por_dpto %>%
  left_join(departamentos_df, by = "Depto")

# Ordenar departamentos por porcentaje de pobreza
pobreza_por_dpto$Nombre_Depto <- factor(pobreza_por_dpto$Nombre_Depto, 
                                        levels = pobreza_por_dpto %>%
                                          filter(Pobre == 1) %>%
                                          arrange(desc(Prop)) %>%
                                          pull(Nombre_Depto))

# Gráfico de barras apiladas con nombres de departamentos
ggplot(pobreza_por_dpto, aes(x = Nombre_Depto, y = Prop, fill = factor(Pobre))) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round(Prop, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "black") +  
  scale_fill_manual(values = c("0" = "gray", "1" = "red")) +  
  labs(title = "Distribución Porcentual de la Pobreza por Departamento",
       x = "Departamento", 
       y = "Porcentaje de hogares", 
       fill = "Pobre") +
  theme_minimal() +
  coord_flip()



# Distribución del IPUG
train_hogares_2 %>%
  filter(Pobre == "No Pobres") %>%
  ggplot(aes(Ingtotug)) +
  geom_density(fill="blue")

train_hogares_2 %>%
  filter(Pobre == "Pobres") %>%
  ggplot(aes(Ingtotug)) +
  geom_density(fill="blue")



