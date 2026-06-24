# TFM-PISA-Finlandia-Singapur-2022-An-lisis
Código en R para el Trabajo Fin de Máster: 'Las paradojas de la perseverancia'. Análisis estadístico de los datos de PISA 2022 (Finlandia y Singapur), incluyendo la extracción manual de PCA y el cálculo de la inconsistencia (row_sd) frente al diseño de muestreo matricial.

# Las paradojas de la perseverancia (PISA 2022)

**Autor:** Zaynil Yan Zhen Ancheta  
**Fecha:** Junio 2026  
**Proyecto:** Trabajo Fin de Máster en Análisis de Datos e Inteligencia de Negocios (Universidad de Oviedo)

## Descripción
Este repositorio contiene el código íntegro en lenguaje R utilizado para los análisis estadísticos del TFM *"Las paradojas de la perseverancia: análisis comparativo de la cultura, la calibración y el rendimiento en matemáticas en Finlandia y Singapur en PISA 2022"*. 

El script principal (`PISA-2022-FRESH.R`) detalla la solución metodológica aplicada para superar el problema de pérdida masiva de datos provocado por el "muestreo matricial dentro del constructo" introducido por la OCDE en el ciclo de 2022.

## Estructura del Análisis
El código está estructurado en las siguientes fases metodológicas:
1. **Limpieza de Datos:** Importación de las bases de datos de PISA mediante el paquete `intsvy` y codificación inversa de los ítems con enunciados negativos.
2. **Análisis Dimensional (PCA):** Extracción manual de componentes principales para aislar la varianza del rasgo de perseverancia y mitigar el sesgo de método.
3. **Métrica de Inconsistencia (`row_sd`):** Cálculo de la desviación estándar intraindividual para medir la fiabilidad de las respuestas de cada alumno.
4. **Modelos de Regresión:** Modelos multivariantes para poner a prueba las distintas paradojas (la paradoja socioeconómica, la de las matemáticas aplicadas y la anomalía de los repetidores) frente a la variable dependiente de rendimiento en matemáticas (`PV1MATH`).

## Requisitos y Librerías
Para replicar este análisis, es necesario configurar el directorio local con los archivos de datos de PISA 2022 (`CY08MSP_STU_QQQ.sav` y `CY08MSP_SCH_QQQ.sav`) e instalar los siguientes paquetes en R:
* `intsvy` (versión instalada vía GitHub mediante `devtools`)
* `dplyr` y `tidyr` (manipulación de datos)
* `haven` (importación de archivos SPSS)
* `psych` (análisis PCA)
* `ggplot2` (visualización)
* `knitr` (formato de tablas)
