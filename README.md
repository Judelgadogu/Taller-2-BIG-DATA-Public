# Predicción de la pobreza de los hogares en Colombia para el año 2018 usando aprendizaje automático

## Descripción del Proyecto

El objetivo de este trabajo es encontrar el mejor modelo predictivo de pobreza a nivel de hogares.

## Contenido del Repositorio
document/: Informe final en PDF con análisis, resultados y conclusiones.
*   **document/**: Informe final en PDF con análisis, resultados y conclusiones.
*   **scripts/**: Scripts de R para la adquisición, limpieza, análisis y modelado de datos.
*   **stores/**: Conjuntos de datos utilizados. Si los archivos son grandes, se incluyen instrucciones de acceso.

## Resumen Corto
Este proyecto aplicó técnicas de aprendizaje automático, específicamente el modelo Random Forest, para predecir la pobreza en hogares colombianos utilizando datos de la Gran Encuesta de Hogares (GEIH) de 2018. El modelo Random Forest demostró ser efectivo para identificar hogares no pobres y generalizar a nuevos datos, superando en rendimiento general a modelos lineales como la regresión logística y modelos penalizados. Los factores más importantes identificados para la predicción de la pobreza incluyen la dependencia del sistema de salud subsidiado, la estructura del hogar (tamaño y número de hijos), la recepción de subsidios alimentarios y el nivel educativo promedio del hogar.

## Scripts
*   **1 Transfromación bases en rds/**: Para cargar las bases y pasarlas a formato .RDS. Este formato es más eficiente para guardar y cargar los datos.
*   **2 Preprocesamiento/**: Este código permite trabajar las bases. Primero, se transforman las variables que lo requieren, se renombran algunas por facilidad de análisis, se agrupan la base de personas por hogares y se unen las dos bases ampliando el número de variables disponibles.
*   **3 Descripción/**: Este código se utiliza para hacer un análisis de los datos pre eliminar para entender el comportamiento de las variables y su relación entre ellas.
*   **4 Modelos Logit Ridge Lasso Elastic Net/**: Este contiene los modelos de regresión logística (Logit, Ridge, Lasso y Elastic Net) y permite hacer las predicciones de pobreza. ADicional, evalua el rendimiento de cada uno de estos en dichas predicciones.
*   **5 Random_forest/**: Este código permite correr el modelo de Random Forest para generar predicciones de pobreza con las bases trabajadas. Adicional, incluye todas las tablas y gráficas que permiten evaluar el modelo. 

Datos obtenidos de: [https://microdatos.dane.gov.co/index.php/catalog/608/datafile/F1#page=F2&tab=data-dictionary](https://microdatos.dane.gov.co/index.php/catalog/608/datafile/F1#page=F2&tab=data-dictionary) (GEIH)

## Créditos

*   Mario Andrés Mercado
*   Julian Delgado
*   Ana María Rojas
*   Juan David Rincón
