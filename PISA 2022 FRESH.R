## Cargar librerias
# devtools::install_github("eldafani/intsvy")
library(intsvy)
library(devtools)
library(dplyr)

## Definir directorio 

dir <- "C:/Users/znanc/Desktop/TFM/PISA Files/dir" # Directorio


## Definir variables de perseverancia (ST307Q)

pers_var <- paste0("ST307Q", sprintf("%02d", 1:10), "JA")


pisa <- pisa.select.merge(folder = dir,
                          student.file="CY08MSP_STU_QQQ.sav",
                          school.file="CY08MSP_SCH_QQQ.sav",
                          student= pers_var,
                          school = c(), 
                          countries = c("FIN", "SGP"))


## Limpieza
library(haven) # Para SPSS

# 1. Convert labelled SPSS missing values to R NAs
# zap_labels() removes the text tags, as_factor() can be messy for PCA
pisa_clean <- pisa %>%
  mutate(across(all_of(pers_var), ~as.numeric(zap_labels(.x))))

# 2. Explicitly handle the 97, 98, 99 codes
# This ensures that ANY value outside our 1-5 scale becomes NA
pisa_clean <- pisa_clean %>%
  mutate(across(all_of(pers_var), ~if_else(.x %in% 1:5, .x, NA_real_)))

# 3. Proceed with Reverse Coding for the 4 negative items
neg_items <- c("ST307Q04JA", "ST307Q06JA", "ST307Q07JA", "ST307Q10JA")
pisa_clean <- pisa_clean %>%
  mutate(across(all_of(neg_items), ~6 - .x))


## ANALISIS PCA
# Split data into countries
data_fin <- pisa_clean %>% filter(CNT == "FIN") %>% select(all_of(pers_var)) # Finlandia
data_sgp <- pisa_clean %>% filter(CNT == "SGP") %>% select(all_of(pers_var)) # Singapore

# Generate the Scree Plot
# 'pc = TRUE' shows the eigenvalues for Principal Components
# 'factors = FALSE' hides the factor analysis line to keep it clean for PCA
library(psych)

# Finland Scree Plot
scree(data_fin, pc = TRUE, factors = FALSE, main = "Scree Plot: Finland Perseverance Items")

# Singapore Scree Plot
scree(data_sgp, pc = TRUE, factors = FALSE, main = "Scree Plot: Singapore Perseverance Items")

# Parece que hay un factor muy fuerte, pero hay un caso por un segundo factor

## ALTERNATE VISUALIZATION (APA7)
# Finland Scree Plot
# We remove the 'main' title and use 'pc = TRUE'
# The 'par' function helps clean the margins and frame
par(bty = "l", family = "serif") # "l" creates an L-shaped axis; serif matches thesis font
scree(data_fin, pc = TRUE, factors = FALSE, main = "") 
abline(h = 1, lty = 2, col = "red") # Adds the Kaiser Criterion reference line

# Singapore Scree Plot
par(bty = "l", family = "serif")
scree(data_sgp, pc = TRUE, factors = FALSE, main = "")
abline(h = 1, lty = 2, col = "red")


## Probando PCA con 2 factores
# Run PCA for Finland
pca_fin <- principal(data_fin, nfactors = 2, rotate = "none", missing = TRUE, impute = "median")

# Run PCA for Singapore
pca_sgp <- principal(data_sgp, nfactors = 2, rotate = "none", missing = TRUE, impute = "median")

# Compare Loadings
results_comp <- data.frame(
  Item = pers_var,
  Finland = as.numeric(pca_fin$loadings),
  Singapore = as.numeric(pca_sgp$loadings)
)

print(results_comp)

## Visualización 
library(ggplot2)

ggplot(results_comp, aes(x = Finland, y = Singapore, label = Item)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Comparison of Perseverance Item Loadings",
       subtitle = "FIN vs SGP (Closer to the dashed line means similar behavior)") +
  theme_minimal()


## Comparando PERSEVAGR with PCA analysis 
library(tidyr)

# 1. Corrected Loading: Move all items to the student argument
pisa_with_index <- pisa.select.merge(
  folder = dir,
  student.file = "CY08MSP_STU_QQQ.sav",
  school.file = "CY08MSP_SCH_QQQ.sav",
  # Include all questions + the official index + demographics in 'student'
  student = c(pers_var, "PERSEVAGR", "ESCS", "ST004D01T", "CNT"), 
  school = c(), 
  countries = c("FIN", "SGP")
)

# 2. Clean official data (Same logic as PCA preparation)
# Ensuring PISA missing codes (97, 98, 99) are treated as NA
pisa_clean_idx <- pisa_with_index %>%
  mutate(across(all_of(pers_var), ~as.numeric(haven::zap_labels(.x)))) %>%
  mutate(across(all_of(pers_var), ~if_else(.x %in% 1:5, .x, NA_real_)))

# 3. Calculate Item-to-Index Correlations for each country
# This reveals how the index is 'formed' in each country
item_correlations <- pisa_clean_idx %>%
  group_by(CNT) %>%
  summarise(across(all_of(pers_var), 
                   ~cor(.x, PERSEVAGR, use = "pairwise.complete.obs"))) %>%
  pivot_longer(cols = -CNT, names_to = "Item", values_to = "Correlation")

# 4. Visualize the differences to spot "Specific Item Parameters"
ggplot(item_correlations, aes(x = Item, y = Correlation, fill = CNT)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Item Contribution to official PERSEVAGR Index",
       subtitle = "Significant gaps indicate country-specific item weighting (DIF)",
       y = "Correlation with PERSEVAGR",
       x = "Question Item") +
  theme_minimal()

## Parece que el indice oficial PERSEVAGR es más preciso porque ajusta mathematicamente por Finlandia

## ALTERNATE VISUALIZATION (APA7)
ggplot(item_correlations, aes(x = Item, y = Correlation, fill = CNT)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + # Keeps the horizontal orientation
  theme_classic() + # White background, no gridlines
  scale_fill_manual(values = c("steelblue", "darkorange"),
                    labels = c("Finland", "Singapore")) +
  labs(x = "Question Item",
       y = "Correlation with PERSEVAGR Index",
       fill = "Country") +
  theme(text = element_text(family = "serif"))


## Regresion lineal para ver si el PCA "manual" or PERSEVAGR predice mejor el rendimiento 
## en matematicá
# 1. Load Data
# We pull everything into 'student' to ensure consistency
pisa_analysis <- pisa.select.merge(
  folder = dir,
  student.file = "CY08MSP_STU_QQQ.sav",
  school.file = "CY08MSP_SCH_QQQ.sav",
  student = c(pers_var, "PERSEVAGR", "PV1MATH", "ESCS", "CNT"), 
  school = c(), 
  countries = c("FIN", "SGP")
)

# 2. Create 'pisa_ready' (Cleaning & Reverse Coding)
neg_items <- c("ST307Q04JA", "ST307Q06JA", "ST307Q07JA", "ST307Q10JA")

pisa_ready <- pisa_analysis %>%
  # Convert labels to numeric and handle PISA missing codes (97, 98, 99)
  mutate(across(all_of(pers_var), ~as.numeric(zap_labels(.x)))) %>%
  mutate(across(all_of(pers_var), ~if_else(.x %in% 1:5, .x, NA_real_))) %>%
  # Reverse code the negative statements
  mutate(across(all_of(neg_items), ~6 - .x))

# 3. Generate the Manual PCA Score
# We run this on the whole set or by country; here we do it to get the score column
pca_all <- principal(pisa_ready %>% select(all_of(pers_var)), nfactors = 1, missing = TRUE, 
                     impute = "median")
pisa_ready$manual_pca <- as.numeric(pca_all$scores)

# 4. Regression Analysis
# Model A: Predicting Math with your Manual PCA
model_manual <- lm(PV1MATH ~ manual_pca * CNT, data = pisa_ready)

# Model B: Predicting Math with Official OECD Index
model_official <- lm(PV1MATH ~ PERSEVAGR * CNT, data = pisa_ready)

# 5. Compare Results
summary(model_manual)
summary(model_official)

## Parece que el modelo "manual" de PCA es un indicador ligaramente mas fuerte 
## Porque tiene un R^2 ajustado de 0.2256 y lo de PERSEVAGR tiene 0.2094 
## Explica (0.0162 o 1.6%) más da las diferencias en matematicas
## Tambien es un modelo más robusto por tener una muestra de N = 16,845 


## Visualización de los regression slopes
# Create the visualization
ggplot(pisa_ready, aes(x = manual_pca, y = PV1MATH, color = CNT)) +
  # Add raw data points with low opacity to see the density
  geom_point(alpha = 0.1, size = 0.5) + 
  # Add the linear regression lines with 95% confidence intervals
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.2) +
  # Use PISA-style colors (optional)
  scale_color_manual(values = c("FIN" = "#f8766d", "SGP" = "#00bfc4")) +
  # Add descriptive labels based on your regression findings
  labs(
    title = "Persistence vs. Math Achievement: Finland vs. Singapore",
    subtitle = "Steeper slope in Finland suggests a higher achievement return on persistence.",
    x = "Manual Perseverance Score (PCA Z-score)",
    y = "Math Achievement (PV1MATH)",
    color = "Country"
  ) +
  # Clean up the look
  theme_minimal() +
  theme(legend.position = "bottom")

## ALTERNATE VISUALIZATION (APA7)
# Assuming your model data is in pisa_ready
ggplot(pisa_ready, aes(x = manual_pca, y = PV1MATH, color = CNT)) +
  geom_point(alpha = 0.1, size = 0.5) + # Keeps the scatter light to focus on lines
  geom_smooth(method = "lm", se = TRUE, size = 1.2) + 
  scale_color_manual(values = c("steelblue", "darkorange"), 
                     labels = c("Finland", "Singapore")) +
  theme_classic() + # Removes grey background and gridlines
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold"),
    text = element_text(family = "serif") # Matches standard thesis font
  ) +
  labs(
    x = "Perseverance Score (Manual PCA)",
    y = "Mathematics Achievement (PV1MATH)",
    color = "Country"
  )


###############################################################################
## Construyendo sobre el trabajo de Norberto
## 1) RESPUESTAS INCOHERENTES
# Measure the gap between positive and negative items (after reverse coding)
# High values = High Inconsistency
pisa_ready <- pisa_ready %>%
  mutate(inconsistency = abs(ST307Q01JA - ST307Q10JA) + abs(ST307Q09JA - ST307Q04JA))

# Regression including Inconsistency
model_inconsistency <- lm(PV1MATH ~ manual_pca + inconsistency + CNT, data = pisa_ready)
summary(model_inconsistency)
## Ha quitado 16472 observaciones y ha quedado con 373 que es solo un 2.2% de la
## población total. Es por el "within-construct matrix sampling design"
## Por eso, hay que buscar un modelo más robusto para incluir todos los datos
# Create a 'Variation' index: How much does a student's answer vary across all items?
pisa_ready$row_sd <- apply(pisa_ready[, pers_var], 1, sd, na.rm = TRUE)

# Test if high variation (inconsistency) predicts lower math scores
model_robust <- lm(PV1MATH ~ manual_pca + row_sd + CNT, data = pisa_ready)
summary(model_robust)

## El porcentaje de estudiantes que dan respuestas inconsistentes
# Define "Doubtful/Inconsistent" students as those in the top 20% of row variation
threshold <- quantile(pisa_ready$row_sd, 0.80, na.rm = TRUE)
pisa_ready$is_inconsistent <- pisa_ready$row_sd > threshold

# Calculate the actual percentage
mean(pisa_ready$is_inconsistent, na.rm = TRUE) * 100


## 04 mayo 2025
# Inconsistencia vs. Desviación Estándar
# 0. Recalcular la variable 'inconsistency' que se borró al recargar los datos
pisa_ready <- pisa_ready %>%
  mutate(inconsistency = abs(ST307Q01JA - ST307Q10JA) + abs(ST307Q09JA - ST307Q04JA))

# 1. Filtrar los 373 alumnos completos
datos_validacion <- pisa_ready %>% filter(!is.na(inconsistency))

# 2. Ver la correlación matemática
cor(datos_validacion$row_sd, datos_validacion$inconsistency)

# 3. Scatterplot de Validez Concurrente con Estilo Coherente
library(ggplot2)

ggplot(datos_validacion, aes(x = row_sd, y = inconsistency, color = CNT)) +
  # Puntos con transparencia para observar la densidad de la submuestra
  geom_jitter(width = 0.05, height = 0.1, alpha = 0.5, size = 2) +
  # Línea de tendencia lineal
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = TRUE) +
  facet_wrap(~ CNT, labeller = as_labeller(c("FIN" = "Finland", "SGP" = "Singapore"))) +
  # Formato de temas clásico y Serif
  theme_classic() + 
  theme(
    legend.position = "none",
    text = element_text(family = "serif"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(face = "italic", size = 11),
    plot.caption = element_text(hjust = 0, size = 9),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  # Etiquetas APA 7
  labs(
    x = "Row Standard Deviation (row_sd)",
    y = "Inconsistency Index (Absolute Difference)",
    color = "Country",
    ) +
  # Colores coherentes con el resto del estudio
  scale_color_manual(values = c("steelblue", "darkorange"), 
                     labels = c("Finland", "Singapore"))

summary(lm(inconsistency ~ row_sd, data = subset(datos_validacion, CNT == "FIN")))
summary(lm(inconsistency ~ row_sd, data = subset(datos_validacion, CNT == "SGP")))


## 2) LA RELACIÓN PARABOLICA
# Adding a squared term to the manual_pca model
# Quadratic model to test if the relationship "bends" at high levels of perseverance
model_parabola <- lm(PV1MATH ~ manual_pca + I(manual_pca^2) + row_sd + CNT, data = pisa_ready)
summary(model_parabola)


## 3) LA PARADOJA DE "MATEMATICAS APLICADAS"
# Definición de variables
pers_var      <- paste0("ST307Q", sprintf("%02d", 1:10), "JA")
formal_items  <- c("ST275Q05WA", "ST275Q07WA", "ST275Q09WA")
applied_items <- c("ST275Q01WA", "ST275Q02WA", "ST275Q03WA", "ST275Q06WA", "ST275Q08WA")

# Carga de datos (Note: Corrected ST301Q01JA)
pisa_analysis <- pisa.select.merge(
  folder = dir,
  student.file = "CY08MSP_STU_QQQ.sav",
  school.file = "CY08MSP_SCH_QQQ.sav",
  student = c(pers_var, formal_items, applied_items, 
              "PERSEVAGR", "PV1MATH", "ESCS", "CNT", "MATHEFF", "CURIOAGR"), 
  school = c(), 
  countries = c("FIN", "SGP")
)

# A. Corrected Initial Cleaning
pisa_ready <- pisa_analysis %>%
  mutate(across(c(all_of(pers_var), all_of(formal_items), all_of(applied_items), 
                  "MATHEFF", "CURIOAGR"), 
                ~as.numeric(zap_labels(.x)))) %>%
  # ONLY clean categorical ranges for the raw question items
  mutate(across(all_of(pers_var), ~if_else(.x %in% 1:5, .x, NA_real_))) %>%
  mutate(across(c(all_of(formal_items), all_of(applied_items)), ~if_else(.x %in% 1:4, .x, NA_real_)))
# Note: MATHEFF and CURIOAGR are left as-is because they are continuous indices.

# B. (Continue with PCA and Row SD as before...)
pca_all <- principal(pisa_ready %>% select(all_of(pers_var)), nfactors = 1, missing = TRUE, impute = "median")
pisa_ready$manual_pca <- as.numeric(pca_all$scores)


# C. Calculate Inconsistency (Row SD)
pisa_ready$row_sd <- apply(pisa_ready[, pers_var], 1, sd, na.rm = TRUE)

# D. Create Formal vs Applied Indices
pisa_ready <- pisa_ready %>%
  rowwise() %>%
  mutate(
    formal_score  = mean(c_across(all_of(formal_items)), na.rm = TRUE),
    applied_score = mean(c_across(all_of(applied_items)), na.rm = TRUE)
  ) %>%
  ungroup()

# E. Paradox Regression
model_paradox <- lm(PV1MATH ~ formal_score + applied_score + manual_pca + row_sd + CNT, 
                    data = pisa_ready)
summary(model_paradox)

## 3.1) Relación de rasgos no-cognitivos
pisa_ready <- pisa_ready %>%
  rename(
    self_efficacy = MATHEFF,
    interest      = CURIOAGR
  )

# 2. Correlation Matrix
cor_matrix <- pisa_ready %>%
  select(applied_score, formal_score, manual_pca, self_efficacy, interest) %>%
  cor(use = "pairwise.complete.obs")

print("Updated Correlation Matrix:")
print(round(cor_matrix, 3))

# 3. Visualization: The Engagement "Boost"
ggplot(pisa_ready, aes(x = applied_score, y = self_efficacy, color = CNT)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Applied Math as a Self-Efficacy Boost",
       subtitle = "Does frequent real-world math correlate with feeling more capable?",
       x = "Frequency of Applied Math Tasks",
       y = "Mathematics Self-Efficacy Index") +
  theme_minimal()

## ALTERNATE VISUALIZATION (APA7)
# Recommended theme adjustments
ggplot(pisa_ready, aes(x = applied_score, y = self_efficacy, color = CNT)) +
  geom_smooth(method = "lm") +
  theme_classic() + # Removes grey background and gridlines
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold"),
    text = element_text(family = "serif") # Matches standard thesis font
  ) +
  labs(x = "Frequency of Applied Mathematics Tasks",
       y = "Mathematics Self-Efficacy Index",
       color = "Country") +
  scale_color_manual(values = c("steelblue", "darkorange"), # Professional colors
                     labels = c("Finland", "Singapore"))


# Tabla de comparaciones entre Finlandia y Singapur
library(knitr)

# 1. Calculate means by country
comparison_table <- pisa_ready %>%
  group_by(CNT) %>%
  summarise(
    Avg_Math_Score    = mean(PV1MATH, na.rm = TRUE),
    Avg_Formal_Math   = mean(formal_score, na.rm = TRUE),
    Avg_Applied_Math  = mean(applied_score, na.rm = TRUE),
    Avg_Perseverance  = mean(manual_pca, na.rm = TRUE),
    Avg_Inconsistency = mean(row_sd, na.rm = TRUE)
  ) %>%
  # Round for readability
  mutate(across(where(is.numeric), ~round(.x, 3)))

# 2. Print the table
print(comparison_table)

# 3. Optional: Create a "Long" version for easier plotting
comparison_long <- comparison_table %>%
  pivot_longer(cols = -CNT, names_to = "Metric", values_to = "Value")

# Visualize the differences
ggplot(comparison_long %>% filter(Metric != "Avg_Math_Score"), 
       aes(x = Metric, y = Value, fill = CNT)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Curriculum and Trait Comparison: FIN vs SGP",
       y = "Average Score / Frequency",
       x = "") +
  theme_minimal() +
  coord_flip()



## 4) LOS REPETIDORES Y LA PARADOJA SOCIO-ECONOMICA
# Re-cargando datos
pisa_analysis <- pisa.select.merge(
  folder = dir,
  student.file = "CY08MSP_STU_QQQ.sav",
  school.file = "CY08MSP_SCH_QQQ.sav",
  # Add REPEAT and ESCS here!
  student = c(pers_var, formal_items, applied_items, 
              "PERSEVAGR", "PV1MATH", "ESCS", "CNT", "REPEAT"), 
  school = c(), 
  countries = c("FIN", "SGP")
)

# A. Initial Cleaning
pisa_ready <- pisa_analysis %>%
  mutate(across(c(all_of(pers_var), all_of(formal_items), all_of(applied_items)), 
                ~as.numeric(zap_labels(.x)))) %>%
  mutate(across(c(all_of(pers_var), all_of(formal_items), all_of(applied_items)), 
                ~if_else(.x %in% 1:5, .x, NA_real_))) # Note: ST275 items are 1-4, pers_var are 1-5

# 1. Check the distribution of repeaters in FIN and SGP
table(pisa_ready$CNT, pisa_ready$REPEAT)

# 2. Test the "Extra Effort" Paradox (Question ST307Q02JA)
# "I apply additional effort when work becomes challenging"
repeater_paradox <- pisa_ready %>%
  filter(!is.na(REPEAT)) %>%
  group_by(CNT, REPEAT) %>%
  summarise(
    Mean_Math = mean(PV1MATH, na.rm = TRUE),
    Mean_Claimed_Effort = mean(ST307Q02JA, na.rm = TRUE),
    N = n()
  )

print(repeater_paradox)

## 13 junio 2026
# Test-T
# T-test for Finland
t.test(ST307Q02JA ~ REPEAT, data = subset(pisa_ready, CNT == "FIN"))

# T-test for Singapore
t.test(ST307Q02JA ~ REPEAT, data = subset(pisa_ready, CNT == "SGP"))

# Re-cargando datos
# B. Generate Manual PCA (Perseverance)
pca_all <- principal(pisa_ready %>% select(all_of(pers_var)), nfactors = 1, missing = TRUE, 
                     impute = "median")
pisa_ready$manual_pca <- as.numeric(pca_all$scores)

# C. Calculate Inconsistency (Row SD)
pisa_ready$row_sd <- apply(pisa_ready[, pers_var], 1, sd, na.rm = TRUE)


# This model includes the Trait, SES, Inconsistency, and Country
model_final_paradox <- lm(PV1MATH ~ manual_pca + ESCS + row_sd + CNT, data = pisa_ready)
summary(model_final_paradox)


## 05 mayo 2026
## Scatterplot
# 1. Calcular el coeficiente de correlación (Opcional: puedes imprimirlo en la consola)
cor_escs <- cor(pisa_ready$row_sd, pisa_ready$ESCS, use = "pairwise.complete.obs")
print(paste("Correlación de Pearson (r):", round(cor_escs, 3)))

# 2. Crear el scatterplot con coherencia visual total
library(ggplot2)

ggplot(pisa_ready, aes(x = ESCS, y = row_sd, color = CNT)) +
  # Puntos con baja opacidad (alpha) debido a la altísima densidad de datos (N ≈ 16,000)
  geom_point(alpha = 0.1, size = 1) + 
  # Línea de tendencia por país para ver si el nivel socioeconómico afecta igual en ambos
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) + 
  # Formato APA 7 y Serif
  theme_classic() + 
  theme(
    legend.position = "bottom",
    text = element_text(family = "serif"), # Coherencia con fuente de tesis
    axis.title = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(face = "italic", size = 11),
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 10))
  ) +
  # Etiquetas en español y formato APA
  labs(
    x = "Index of Economic, Social and Cultural Status (ESCS)",
    y = "Inconsistency of Response (row_sd)",
    color = "Country",
    
  ) +
  # Colores coherentes con el resto de tu estudio
  scale_color_manual(values = c("steelblue", "darkorange"), 
                     labels = c("Finland", "Singapore"))