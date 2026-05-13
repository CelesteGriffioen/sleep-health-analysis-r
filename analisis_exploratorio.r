library(tidyverse)
library(skimr)
library(janitor)
library(corrplot)
library(ggplot2)
library(scales)

datos <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

#primera visuaizacion de la base
glimpse(datos)
print(summary(datos))
dim(datos)
head(datos)

#detectar valores faltantes/duplicados
sum(is.na(datos))
colMeans(is.na(datos))*100

#detectar duplicados
sum(duplicated(datos))
datos <- distinct(datos)

#renombrar columnas
names(datos)
datos <- clean_names(datos)
View(datos)

#limpiar registros
datos <- datos %>%
  mutate(bmi_category = ifelse(
    bmi_category == "Normal Weight",
    "Normal",
    bmi_category
  ))
View(datos)

#distribucion de genero
genero_plot <- datos %>%
  count(gender) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = paste0(
      gender, "\n",
      percent(porcentaje, accuracy = 0.1),
      " (", n, ")"
    )
  )

print(genero_plot)

ggplot(genero_plot, aes(x = "", y = n, fill = gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = "Distribución por género",
    fill = "Género"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  )

#distribucion de trastornos de sueño segun el genero
ggplot(datos, aes(x = sleep_disorder, fill = gender)) +
  geom_bar(position = "dodge") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count / sum(count)))),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Distribución de trastornos del sueño por género",
    x = "Trastorno del sueño",
    y = "Cantidad de profesionales",
    fill = ""
  ) + 
  theme_minimal()

#distribucion de trastornos del sueño por ocupacion laboral
ggplot(datos, aes(x = occupation, fill = sleep_disorder)) +
  geom_bar() +
  labs(
    title = "Frecuencia de trastornos del sueño por ocupación",
    x = "Ocupación",
    y = "Frecuencia",
    fill = "Trastorno del sueño"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#distribucion indice corporal
bmi_plot <- datos %>%
  count(bmi_category) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = paste0(
      bmi_category, "\n",
      percent(porcentaje, accuracy = 0.1),
      " (", n, ")"
    )
  )

print(bmi_plot)

ggplot(bmi_plot, aes(x = reorder(bmi_category, -n), y = n, fill = bmi_category)) +
  geom_col() +
  geom_text(
    aes(label = etiqueta),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Distribución de categorías BMI",
    x = "Categoría BMI",
    y = "Frecuencia"
  ) +
  theme_minimal()

#distribucion de trastornos del sueño
sleep_disorder_plot <- datos %>%
  count(sleep_disorder) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = paste0(
      sleep_disorder, "\n",
      percent(porcentaje, accuracy = 0.1),
      " (", n, ")"
    )
  )

print(sleep_disorder_plot)

ggplot(sleep_disorder_plot, aes(x = "", y = n, fill = sleep_disorder)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = "Distribución por Trastorno de Sueño",
    fill = "Trastorno"
  ) + 
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  )

#distribucion de trastorno de sueño por indice de masa corporal
ggplot(datos, aes(x = sleep_disorder, fill = bmi_category)) +
  geom_bar() +
  labs(
    title = "Frecuencia de trastornos del sueño por indice de masa corporal",
    x = "Trastorno del sueño",
    y = "Frecuencia",
    fill = "Indice de masa corporal"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#grafico promedio horas sueño por ocupacion
tabla1 <- datos %>%
  group_by(occupation) %>%
  summarise(promedio = mean(sleep_duration, na.rm = TRUE))

ggplot(tabla1, aes(x = occupation, y = promedio)) +
  geom_col(fill = "cornflowerblue") +
  geom_text(
    aes(label = round(promedio, 1)),
    vjust = -0.5
  ) +
  labs(
    title = "Promedio de duración del sueño por profesión",
    x = "Profesión",
    y = "Promedio duración del sueño (hs)"
  ) +
  theme_minimal()

#frecuencias profesion y nivel de estres
datos <- datos %>%
  mutate(
    categorial_stress_level = case_when(
      stress_level < 5 ~ "Low",
      stress_level >= 5 & stress_level < 8 ~ "Medium",
      stress_level >= 8 ~ "High"
    )
  )

ggplot(datos, aes(x = occupation, fill = categorial_stress_level)) +
  geom_bar() +
  labs(
    title = "Frecuencia de nivel de estres por ocupación",
    x = "Ocupación",
    y = "Frecuencia",
    fill = "Nivel de estres"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))

#modelo de regresion lineal
lmcalidaddelsueño = lm(quality_of_sleep ~ sleep_duration, data = datos)
plot(quality_of_sleep ~ sleep_duration, data=datos, xlab="Duracion del sueño", ylab="Calidad del sueño",
main="Duracion del sueño vs calidad del sueño")
abline(lmcalidaddelsueño, col="blue")

summary(lmcalidaddelsueño)

horas <- data.frame(sleep_duration=seq(1,9))
predict(lmcalidaddelsueño, horas)