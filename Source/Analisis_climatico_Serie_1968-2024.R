# =============================================================================
# ANÁLISIS CLIMÁTICO HISTÓRICO 1968-2024
# Temperatura, Precipitación y Eventos Extremos
# =============================================================================
# Paquetes necesarios (correr solo la primera vez):
 # install.packages(c("readxl", "dplyr", "lubridate", "ggplot2", "patchwork",
 #                   "scales", "tidyr", "zoo", "trend", "extRemes", "ggridges", "openxlsx", "cowplot", "magick"))
# ===========================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(scales)
library(tidyr)
library(zoo)
library(trend)
library(extRemes)
library(ggridges)
library(openxlsx)
library(cowplot)
library(magick)

dir.create("graficos",             showWarnings = FALSE)
dir.create("graficos/publicacion", showWarnings = FALSE)
dir.create("tablas",               showWarnings = FALSE)


# =============================================================================
# SECCIÓN 1 - CONFIGURACIÓN GLOBAL
# =============================================================================

CAPTION_BASE <- "Fuente: Estación Meteorológica Colonia Benítez | Datos diarios 1968-2024"

# Años excluidos del análisis por registros incompletos
# 2021-2023: interrupción operativa de la estación durante período COVID-19
# 2021: 203/365 días faltantes en temperatura y heliofanía
# 2022: solo 259/365 días registrados (año truncado)
# 2023: solo 122/365 días registrados (año truncado)
# La exclusión afecta todas las variables para mantener consistencia metodológica
ANIOS_EXCLUIR <- c(2021, 2022, 2023)

colores_era <- c(
  "1968-1989" = "#4575b4",
  "1990-2009" = "#fdae61",
  "2010-2024" = "#d73027"
)

theme_clima <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    plot.caption     = element_text(color = "gray55", size = 8, hjust = 0),
    axis.title       = element_text(size = 11),
    axis.text        = element_text(size = 10),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10, face = "bold"),
    legend.text      = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4)
  )

# -----------------------------------------------------------------------------
# Configuración del logo
# Colocar el archivo de imagen en la misma carpeta que este script
# Ajustar posición y tamaño según necesidad
# -----------------------------------------------------------------------------
RUTA_LOGO   <- "logo.png"  # nombre del archivo (imagen cuadrada)
LOGO_X      <- 0.88        # posición horizontal: 0 = izquierda, 1 = derecha
LOGO_Y      <- 0.00        # posición vertical:   0 = abajo,     1 = arriba
LOGO_WIDTH  <- 0.08        # fracción del ancho total del gráfico
LOGO_HEIGHT <- 0.06        # fracción del alto  total del gráfico

LOGO <- tryCatch(
  image_read(RUTA_LOGO),
  error = function(e) {
    message("⚠ Logo no encontrado en '", RUTA_LOGO,
            "'. Los gráficos se guardarán sin logo.")
    NULL
  }
)

# -----------------------------------------------------------------------------
# Función global de guardado
# El logo se agrega automáticamente en TODOS los gráficos
# -----------------------------------------------------------------------------
guardar <- function(plot, ruta, ancho, alto, dpi = 300) {
  if (!is.null(LOGO)) {
    p_final <- ggdraw(plot) +
      draw_image(LOGO,
                 x      = LOGO_X,
                 y      = LOGO_Y,
                 width  = LOGO_WIDTH,
                 height = LOGO_HEIGHT)
  } else {
    p_final <- plot
  }
  ggsave(ruta, p_final, width = ancho, height = alto, dpi = dpi, bg = "white")
  cat(sprintf("  ✔ %s\n", ruta))
}


# =============================================================================
# SECCIÓN 2 - CARGA Y PREPARACIÓN DE DATOS
# =============================================================================

RUTA_ARCHIVO <- "Serie Historica Varios Instrumentos1968-012025.xls"

df_raw <- read_excel(RUTA_ARCHIVO, sheet = "Datos diarios")

df <- df_raw %>%
  rename(
    Fecha      = Fecha,
    Temp_Media = Temperatura_Abrigo_150cm,
    Temp_Max   = Temperatura_Abrigo_150cm_Maxima,
    Temp_Min   = Temperatura_Abrigo_150cm_Minima,
    Precip     = Precipitacion_Pluviometrica,
    Heliofania = Heliofania_Efectiva,
    ETP        = Evapotranspiracion_Potencial
  ) %>%
  mutate(
    Fecha      = as.Date(Fecha),
    Temp_Media = as.numeric(Temp_Media),
    Temp_Max   = as.numeric(Temp_Max),
    Temp_Min   = as.numeric(Temp_Min),
    Precip     = as.numeric(Precip),
    Heliofania = as.numeric(Heliofania),
    ETP        = as.numeric(ETP),
    Year       = year(Fecha),
    Month      = month(Fecha),
    DOY        = yday(Fecha),
    Era        = case_when(
      Year <= 1989 ~ "1968-1989",
      Year <= 2009 ~ "1990-2009",
      TRUE         ~ "2010-2024"
    )
  ) %>%
  filter(!is.na(Fecha), Year >= 1968, Year <= 2024, !(Year %in% ANIOS_EXCLUIR))

cat("Registros cargados:", nrow(df), "\n")
cat("Período:", as.character(min(df$Fecha)), "a", as.character(max(df$Fecha)), "\n")


# =============================================================================
# SECCIÓN 3 - UMBRALES DE EVENTOS EXTREMOS
# =============================================================================

precip_rainy <- df$Precip[!is.na(df$Precip) & df$Precip > 0]
precip_rainy <- as.numeric(precip_rainy)
precip_rainy <- precip_rainy[!is.na(precip_rainy)]

P90 <- quantile(precip_rainy, 0.90)
P95 <- quantile(precip_rainy, 0.95)
P99 <- quantile(precip_rainy, 0.99)

cat(sprintf("Umbral P90: %.1f mm/día\n", P90))
cat(sprintf("Umbral P95: %.1f mm/día\n", P95))
cat(sprintf("Umbral P99: %.1f mm/día\n", P99))

etiq_p95 <- paste0(">=", round(P95), " mm (P95)")
etiq_p99 <- paste0(">=", round(P99), " mm (P99)")


# =============================================================================
# SECCIÓN 4 - RESUMEN ANUAL
# =============================================================================

anual <- df %>%
  group_by(Year) %>%
  summarise(
    Temp_Media_Anual  = mean(Temp_Media, na.rm = TRUE),
    Temp_Max_Media    = mean(Temp_Max,   na.rm = TRUE),
    Temp_Min_Media    = mean(Temp_Min,   na.rm = TRUE),
    Temp_Max_Abs      = max(Temp_Max,    na.rm = TRUE),
    Temp_Min_Abs      = min(Temp_Min,    na.rm = TRUE),
    Amplitud_Termica  = mean(Temp_Max - Temp_Min, na.rm = TRUE),
    Precip_Total      = sum(Precip,      na.rm = TRUE),
    Dias_Lluvia       = sum(Precip > 0,  na.rm = TRUE),
    Intensidad_Media  = Precip_Total / Dias_Lluvia,
    Dias_P90          = sum(Precip >= P90, na.rm = TRUE),
    Dias_P95          = sum(Precip >= P95, na.rm = TRUE),
    Dias_P99          = sum(Precip >= P99, na.rm = TRUE),
    Precip_Max_Diaria = max(Precip,        na.rm = TRUE),
    Precip_P95_Total  = sum(Precip[Precip >= P95], na.rm = TRUE),
    Frac_Extrema      = Precip_P95_Total / Precip_Total,
    Dias_Calor_Ext    = sum(Temp_Max >= 35, na.rm = TRUE),
    Dias_Helada       = sum(Temp_Min <= 0,  na.rm = TRUE),
    Racha_Seca_Max = {
      p <- Precip; p[is.na(p)] <- 0
      rle_res <- rle(p == 0)
      max(rle_res$lengths[rle_res$values == TRUE], 0)
    },
    Racha_Humeda_Max = {
      p <- Precip; p[is.na(p)] <- 0
      rle_res <- rle(p > 0)
      max(rle_res$lengths[rle_res$values == TRUE], 0)
    },
    .groups = "drop"
  ) %>%
  mutate(
    Rolling10 = rollmean(Temp_Media_Anual, k = 10, fill = NA, align = "center")
  )

ref_mean_global <- mean(anual$Temp_Media_Anual[anual$Year <= 1999], na.rm = TRUE)
anual <- anual %>%
  mutate(Anomalia = Temp_Media_Anual - ref_mean_global)


# =============================================================================
# SECCIÓN 5 - TESTS DE TENDENCIA
# =============================================================================

mk_test <- function(x, y, nombre) {
  mk     <- mk.test(y)
  sen    <- sens.slope(y)
  lm_res <- lm(y ~ x)
  cat(sprintf(
    "%-35s | Sen: %+.4f/año | tau=%.3f | p=%.4f | OLS: %+.4f\n",
    nombre, sen$estimates, mk$estimates["tau"], mk$p.value, coef(lm_res)[2]
  ))
  invisible(list(mk = mk, sen = sen, lm = lm_res))
}

cat("\n=== TENDENCIAS (Mann-Kendall + Sen's Slope) ===\n")
mk_test(anual$Year, anual$Temp_Media_Anual,  "Temperatura Media")
mk_test(anual$Year, anual$Temp_Max_Media,    "Temperatura Máxima Media")
mk_test(anual$Year, anual$Temp_Min_Media,    "Temperatura Mínima Media")
mk_test(anual$Year, anual$Precip_Total,      "Precipitación Total")
mk_test(anual$Year, anual$Dias_Lluvia,       "Días con Lluvia")
mk_test(anual$Year, anual$Intensidad_Media,  "Intensidad Media (mm/d)")
mk_test(anual$Year, anual$Dias_P95,          "Días P95")
mk_test(anual$Year, anual$Dias_P99,          "Días P99")
mk_test(anual$Year, anual$Precip_Max_Diaria, "Máx. Diaria")
mk_test(anual$Year, anual$Racha_Seca_Max,    "Racha Seca Máxima")
mk_test(anual$Year, anual$Dias_Calor_Ext,    "Días calor extremo (>=35°C)")

# --- Tabla exportable de Mann-Kendall (mismos cálculos, sin modificar mk_test) ---
mk_tabla <- function(x, y, nombre) {
  mk  <- mk.test(y)
  sen <- sens.slope(y)
  pval <- mk$p.value
  data.frame(
    Variable      = nombre,
    Pendiente_Sen = round(as.numeric(sen$estimates), 4),
    tau           = round(as.numeric(mk$estimates["tau"]), 3),
    p_valor       = round(pval, 4),
    Sig           = ifelse(pval < 0.001, "***",
                    ifelse(pval < 0.010, "**",
                    ifelse(pval < 0.050, "*",
                    ifelse(pval <= 0.100, "~", "")))),
    stringsAsFactors = FALSE
  )
}

tabla_mk <- rbind(
  mk_tabla(anual$Year, anual$Temp_Media_Anual,  "Temp. media anual (°C/año)"),
  mk_tabla(anual$Year, anual$Temp_Max_Media,    "Temp. máx. media (°C/año)"),
  mk_tabla(anual$Year, anual$Temp_Min_Media,    "Temp. mín. media (°C/año)"),
  mk_tabla(anual$Year, anual$Precip_Total,      "Precipitación total (mm/año)"),
  mk_tabla(anual$Year, anual$Dias_Lluvia,       "Días con lluvia (días/año)"),
  mk_tabla(anual$Year, anual$Intensidad_Media,  "Intensidad media (mm/d/año)"),
  mk_tabla(anual$Year, anual$Dias_P95,          "Días P95 (días/año)"),
  mk_tabla(anual$Year, anual$Dias_P99,          "Días P99 (días/año)"),
  mk_tabla(anual$Year, anual$Precip_Max_Diaria, "Precip. máx. diaria (mm/año)"),
  mk_tabla(anual$Year, anual$Racha_Seca_Max,    "Racha seca máx. CDD (días/año)"),
  mk_tabla(anual$Year, anual$Dias_Calor_Ext,    "Días calor extremo >=35°C (días/año)"),
  mk_tabla(anual$Year, anual$Dias_Helada,       "Días helada (días/año)")
)
cat("✔ tabla_mk lista para exportar\n")


# =============================================================================
# SECCIÓN 6 - ANÁLISIS GEV Y PERÍODOS DE RETORNO
# =============================================================================

max_anual <- anual$Precip_Max_Diaria[!is.na(anual$Precip_Max_Diaria)]
gev_fit   <- fevd(max_anual, type = "GEV", method = "MLE")

cat("\n=== AJUSTE GEV ===\n")
print(summary(gev_fit))

return_periods <- c(2, 5, 10, 25, 50, 100)
rl_vals <- sapply(return_periods, function(T) {
  as.numeric(return.level(gev_fit, return.period = T))
})

rl_df <- data.frame(
  Periodo_Retorno_anos = return_periods,
  Precip_esperada_mm   = round(rl_vals, 1)
)

cat("\n=== NIVELES DE RETORNO (mm/día) ===\n")
print(rl_df)


# =============================================================================
# SECCIÓN 7 - ÍNDICES ETCCDI
# =============================================================================

indices_etccdi <- df %>%
  group_by(Year) %>%
  summarise(
    TXx     = max(Temp_Max, na.rm = TRUE),
    TNn     = min(Temp_Min, na.rm = TRUE),
    TX90p   = sum(Temp_Max > quantile(Temp_Max, 0.9, na.rm = TRUE), na.rm = TRUE),
    TN10p   = sum(Temp_Min < quantile(Temp_Min, 0.1, na.rm = TRUE), na.rm = TRUE),
    DTR     = mean(Temp_Max - Temp_Min, na.rm = TRUE),
    RX1day  = max(Precip, na.rm = TRUE),
    RX5day  = {
      p <- Precip; p[is.na(p)] <- 0
      max(rollsum(p, k = 5, fill = NA), na.rm = TRUE)
    },
    SDII    = sum(Precip, na.rm = TRUE) / sum(Precip >= 1, na.rm = TRUE),
    R10mm   = sum(Precip >= 10,  na.rm = TRUE),
    R20mm   = sum(Precip >= 20,  na.rm = TRUE),
    R50mm   = sum(Precip >= 50,  na.rm = TRUE),
    CDD     = {
      p <- Precip; p[is.na(p)] <- 0
      rle_res <- rle(p < 1)
      max(rle_res$lengths[rle_res$values == TRUE], 0)
    },
    CWD     = {
      p <- Precip; p[is.na(p)] <- 0
      rle_res <- rle(p >= 1)
      max(rle_res$lengths[rle_res$values == TRUE], 0)
    },
    PRCPTOT = sum(Precip[Precip >= 1],   na.rm = TRUE),
    R95pTOT = sum(Precip[Precip >= P95], na.rm = TRUE),
    R99pTOT = sum(Precip[Precip >= P99], na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== ÍNDICES ETCCDI - RESUMEN ===\n")
print(summary(indices_etccdi %>% select(-Year)))

cat("\n=== TENDENCIAS EN ÍNDICES ETCCDI ===\n")
mk_test(indices_etccdi$Year, indices_etccdi$RX1day,  "RX1day")
mk_test(indices_etccdi$Year, indices_etccdi$RX5day,  "RX5day")
mk_test(indices_etccdi$Year, indices_etccdi$SDII,    "SDII")
mk_test(indices_etccdi$Year, indices_etccdi$CDD,     "CDD")
mk_test(indices_etccdi$Year, indices_etccdi$CWD,     "CWD")
mk_test(indices_etccdi$Year, indices_etccdi$R95pTOT, "R95pTOT")


# =============================================================================
# SECCIÓN 8 - CLIMATOLOGÍA MENSUAL POR ERA
# =============================================================================

temp_mensual_era <- df %>%
  group_by(Year, Month, Era) %>%
  summarise(
    Temp_Media = mean(Temp_Media, na.rm = TRUE),
    Precip     = sum(Precip,      na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  group_by(Month, Era) %>%
  summarise(
    Temp_Media_Era = mean(Temp_Media, na.rm = TRUE),
    Precip_Era     = mean(Precip,     na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  mutate(Month_Label = factor(month.abb[Month], levels = month.abb))


# =============================================================================
# SECCIÓN 9 - GRÁFICOS
# =============================================================================

# --- G1: Anomalía de temperatura ---
p1 <- ggplot(anual, aes(x = Year)) +
  geom_col(aes(y = Anomalia, fill = Anomalia > 0), alpha = 0.75, width = 0.9) +
  geom_line(aes(y = Rolling10 - ref_mean_global),
            color = "black", linewidth = 1.2, na.rm = TRUE) +
  geom_smooth(aes(y = Anomalia), method = "lm", se = TRUE, color = "darkred",
              linetype = "dashed", linewidth = 1, alpha = 0.15) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5) +
  scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#4575b4"), guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(
    title    = "Anomalía de Temperatura Media Anual",
    subtitle = paste0("Referencia: media 1968-1999 = ", round(ref_mean_global, 2),
                      " °C | Línea negra: media móvil 10 años"),
    x = NULL, y = "Anomalía (°C)", caption = CAPTION_BASE
  ) +
  theme_clima

# --- G2: Temperatura máxima, media y mínima ---
p2 <- anual %>%
  select(Year, Temp_Max_Media, Temp_Min_Media, Temp_Media_Anual) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Valor") %>%
  mutate(Variable = recode(Variable,
    Temp_Max_Media   = "Máxima Media",
    Temp_Min_Media   = "Mínima Media",
    Temp_Media_Anual = "Media"
  )) %>%
  ggplot(aes(x = Year, y = Valor, color = Variable)) +
  geom_line(alpha = 0.6, linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.2) +
  scale_color_manual(values = c(
    "Máxima Media" = "#d73027",
    "Mínima Media" = "#4575b4",
    "Media"        = "gray40"
  )) +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(
    title    = "Temperatura Máxima, Media y Mínima Anual",
    subtitle = "Líneas punteadas: tendencia OLS",
    x = NULL, y = "Temperatura (°C)", color = NULL, caption = CAPTION_BASE
  ) +
  theme_clima

# --- G3: Regresión lineal de temperatura (con fórmula) ---
lm_temp   <- lm(Temp_Media_Anual ~ Year, data = anual)
intercept <- round(coef(lm_temp)[1], 2)
slope     <- round(coef(lm_temp)[2], 4)
r2        <- round(summary(lm_temp)$r.squared, 3)
pval      <- round(summary(lm_temp)$coefficients[2, 4], 4)

etiq_reg <- paste0(
  "y = ", intercept, " + ", slope, " x\n",
  "R² = ", r2, "  |  p = ", pval, "\n",
  "Incremento: +", slope, " °C/año"
)

p3 <- ggplot(anual, aes(x = Year, y = Temp_Media_Anual)) +
  geom_point(aes(color = case_when(
    Year <= 1989 ~ "1968-1989",
    Year <= 2009 ~ "1990-2009",
    TRUE         ~ "2010-2024"
  )), size = 2.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred",
              fill = "red", alpha = 0.12, linewidth = 1.2) +
  geom_line(aes(y = Rolling10), color = "black", linewidth = 1, na.rm = TRUE) +
  geom_hline(yintercept = mean(anual$Temp_Media_Anual, na.rm = TRUE),
             linetype = "dashed", color = "gray50", linewidth = 0.6) +
  annotate("text",
    x     = min(anual$Year) + 1,
    y     = max(anual$Temp_Media_Anual, na.rm = TRUE) - 0.05,
    label = etiq_reg,
    hjust = 0, vjust = 1,
    size  = 3.5, color = "darkred", fontface = "bold"
  ) +
  annotate("text",
    x     = 2018,
    y     = mean(anual$Temp_Media_Anual, na.rm = TRUE) + 0.1,
    label = paste0("Media: ", round(mean(anual$Temp_Media_Anual, na.rm = TRUE), 2), " °C"),
    size  = 3, color = "gray40"
  ) +
  scale_color_manual(values = colores_era, name = "Era") +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  labs(
    title    = "Regresión Lineal — Incremento de Temperatura Media Anual",
    subtitle = "Banda roja: IC 95% | Línea negra: media móvil 10 años | Puntos: valor anual",
    x = "Año", y = "Temperatura Media Anual (°C)", caption = CAPTION_BASE
  ) +
  theme_clima

# --- G4: Precipitación anual ---
p4 <- ggplot(anual, aes(x = Year, y = Precip_Total)) +
  geom_col(aes(fill = Precip_Total > mean(Precip_Total, na.rm = TRUE)),
           alpha = 0.8, width = 0.9) +
  geom_hline(yintercept = mean(anual$Precip_Total, na.rm = TRUE),
             color = "navy", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred",
              linetype = "dashed", linewidth = 1, alpha = 0.15) +
  geom_text(
    data = anual %>% filter(Precip_Total < 700),
    aes(label = paste0(Year, "\n", round(Precip_Total), "mm")),
    vjust = -0.3, size = 3, color = "darkred", fontface = "bold"
  ) +
  scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#f4a582"), guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(
    title    = "Precipitación Total Anual",
    subtitle = paste0("Media histórica: ", round(mean(anual$Precip_Total, na.rm = TRUE)),
                      " mm | Azul: sobre la media | Naranja: bajo la media"),
    x = NULL, y = "Precipitación (mm)", caption = CAPTION_BASE
  ) +
  theme_clima

# --- G5: Días P95 y P99 ---
p5 <- anual %>%
  select(Year, Dias_P95, Dias_P99) %>%
  pivot_longer(-Year, names_to = "Umbral", values_to = "Dias") %>%
  mutate(Umbral = recode(Umbral, Dias_P95 = etiq_p95, Dias_P99 = etiq_p99)) %>%
  ggplot(aes(x = Year, y = Dias, fill = Umbral)) +
  geom_col(position = "identity", alpha = 0.75, width = 0.9) +
  geom_smooth(aes(color = Umbral), method = "lm", se = FALSE,
              linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values  = c(setNames("#4292c6", etiq_p95),
                                setNames("#08306b", etiq_p99))) +
  scale_color_manual(values = c(setNames("#4292c6", etiq_p95),
                                setNames("#08306b", etiq_p99))) +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(
    title    = "Días con Precipitación Extrema por Año",
    subtitle = "Eventos por encima del percentil 95 y 99 (calculados sobre días lluviosos)",
    x = NULL, y = "Número de días", fill = "Umbral", color = "Umbral",
    caption = CAPTION_BASE
  ) +
  theme_clima

# --- G6: Paradoja menos días más intensidad (Versión mejorada) ---
p6a <- ggplot(anual, aes(x = Year, y = Dias_Lluvia)) +
  geom_col(fill = "steelblue", alpha = 0.6, width = 0.9) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue4",
              linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(title = "Días con Lluvia por Año", x = NULL, y = "Días (Precip > 0 mm)") +
  theme_clima

p6b <- ggplot(anual, aes(x = Year, y = Intensidad_Media)) +
  geom_line(color = "#d73027", linewidth = 0.8) +
  geom_point(color = "#d73027", size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred",
              linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(title = "Intensidad Media (mm por día lluvioso)",
       x = "Año", y = "mm / día lluvioso") +
  theme_clima

# Crear el gráfico compuesto con patchwork
p6 <- (p6a / p6b) +
  plot_annotation(
    title    = "Intensificación: Menos Días pero más Intensos",
    subtitle = "La lluvia se concentra en menos episodios de mayor intensidad",
    caption  = CAPTION_BASE,
    theme    = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 10),
      plot.caption = element_text(color = "gray55", size = 8, hjust = 0)
    )
  )

# Aplicar el tema a todos los componentes del patchwork
p6 <- p6 & theme_clima

# --- G7: CDD rachas secas ---
p7 <- ggplot(indices_etccdi, aes(x = Year, y = CDD)) +
  geom_col(aes(fill = CDD), alpha = 0.85, width = 0.9) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred",
              linetype = "dashed", linewidth = 1) +
  scale_fill_gradient(low = "#fee090", high = "#d73027", name = "Días") +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(
    title    = "CDD: Máxima Racha de Días Secos Consecutivos (Precip < 1 mm)",
    subtitle = "Índice ETCCDI — mayor valor = sequías más largas",
    x = "Año", y = "Días consecutivos secos", caption = CAPTION_BASE
  ) +
  theme_clima

# --- G8: RX1day y RX5day ---
p8 <- indices_etccdi %>%
  select(Year, RX1day, RX5day) %>%
  pivot_longer(-Year, names_to = "Indice", values_to = "mm") %>%
  ggplot(aes(x = Year, y = mm, color = Indice)) +
  geom_line(alpha = 0.7, linewidth = 0.9) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
              linewidth = 1, alpha = 0.15) +
  scale_color_manual(
    values = c("RX1day" = "#2166ac", "RX5day" = "#d73027"),
    labels = c("RX1day" = "Máx. 1 día", "RX5day" = "Máx. 5 días consecutivos")
  ) +
  scale_x_continuous(breaks = seq(1970, 2024, 5)) +
  labs(
    title    = "RX1day y RX5day: Máxima Precipitación en 1 y 5 Días",
    subtitle = "Índices ETCCDI para intensidad de eventos extremos",
    x = "Año", y = "Precipitación (mm)", color = NULL, caption = CAPTION_BASE
  ) +
  theme_clima

# --- G9: Distribución mensual de extremos por era ---
p9 <- df %>%
  filter(!is.na(Precip), !is.na(Era)) %>%
  mutate(
    Es_Extremo  = Precip >= P95,
    Month_Label = factor(month.abb[Month], levels = month.abb)
  ) %>%
  filter(Es_Extremo) %>%
  count(Month_Label, Era) %>%
  mutate(n_norm = case_when(
    Era == "1968-1989" ~ n / 22,
    Era == "1990-2009" ~ n / 20,
    Era == "2010-2024" ~ n / 15
  )) %>%
  ggplot(aes(x = Month_Label, y = n_norm, fill = Era)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = colores_era) +
  labs(
    title    = paste0("Eventos Extremos (>=", round(P95), " mm/día) por Mes y Era"),
    subtitle = "Frecuencia promedio anual dentro de cada período",
    x = "Mes", y = "Eventos por año (promedio)", fill = NULL, caption = CAPTION_BASE
  ) +
  theme_clima

# --- G10: Scatter temperatura vs precipitación ---
p10 <- ggplot(anual, aes(x = Precip_Total, y = Temp_Media_Anual, color = Year)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "gray30", linetype = "dashed") +
  geom_text(
    data = anual %>% filter(Precip_Total < 700 | Temp_Media_Anual > 22.5),
    aes(label = Year), vjust = -0.8, size = 3, fontface = "bold"
  ) +
  scale_color_gradient2(
    low      = "#4575b4",
    mid      = "#ffffbf",
    high     = "#d73027",
    midpoint = 1996,
    name     = "Año",
    breaks   = seq(1970, 2020, by = 10),
    labels   = seq(1970, 2020, by = 10)
  ) +
  guides(color = guide_colorbar(
    barwidth       = unit(6, "cm"),
    barheight      = unit(0.4, "cm"),
    title.position = "top",
    title.hjust    = 0.5
  )) +
  labs(
    title    = "Temperatura vs Precipitación Anual",
    subtitle = paste0("r = ", round(cor(anual$Temp_Media_Anual, anual$Precip_Total,
                                        use = "complete.obs"), 3)),
    x = "Precipitación Total (mm)", y = "Temperatura Media Anual (°C)",
    caption = CAPTION_BASE
  ) +
  theme_clima

# --- G11: Ridge plot por era ---
p11 <- df %>%
  filter(Precip > 1, !is.na(Era)) %>%
  ggplot(aes(x = Precip, y = Era, fill = Era)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5,
                      quantile_lines = TRUE, quantiles = c(0.5, 0.95)) +
  scale_fill_manual(values = colores_era, guide = "none") +
  scale_x_log10(
    breaks = c(1, 5, 10, 25, 50, 100, 200),
    labels = c(1, 5, 10, 25, 50, 100, 200)
  ) +
  labs(
    title    = "Distribución de Precipitación Diaria por Era (escala log)",
    subtitle = "Líneas verticales: mediana y P95 | Solo días lluviosos (>1 mm)",
    x = "Precipitación diaria (mm)", y = NULL, caption = CAPTION_BASE
  ) +
  theme_clima

# --- G12: Heatmap combinado temperatura y precipitación mensual ---
heatmap_data <- df %>%
  group_by(Year, Month) %>%
  summarise(
    Temp_Media_Mes  = mean(Temp_Media, na.rm = TRUE),
    Precip_Mes      = sum(Precip,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Month) %>%
  mutate(
    Temp_Anomalia   = Temp_Media_Mes - mean(Temp_Media_Mes, na.rm = TRUE),
    Precip_Anomalia = Precip_Mes     - mean(Precip_Mes,     na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Month_Label = factor(month.abb[Month], levels = rev(month.abb)))

h1 <- ggplot(heatmap_data, aes(x = Year, y = Month_Label, fill = Temp_Anomalia)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#4575b4", mid = "white", high = "#d73027",
    midpoint = 0, name = "Anomalía\n(°C)",
    limits = c(-6, 6), oob = scales::squish
  ) +
  scale_x_continuous(breaks = seq(1970, 2024, 5), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Anomalía de Temperatura Mensual (°C)",
    subtitle = "Desviación respecto a la media histórica de cada mes",
    x = NULL, y = NULL
  ) +
  theme_clima +
  theme(
    axis.text.x       = element_blank(),
    legend.key.height = unit(1.2, "cm"),
    panel.grid        = element_blank()
  )

h2 <- ggplot(heatmap_data, aes(x = Year, y = Month_Label, fill = Precip_Anomalia)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#b35806", mid = "white", high = "#1a6e3c",
    midpoint = 0, name = "Anomalía\n(mm)",
    limits = c(-200, 200), oob = scales::squish
  ) +
  scale_x_continuous(breaks = seq(1970, 2024, 5), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Anomalía de Precipitación Mensual (mm)",
    subtitle = "Desviación respecto a la media histórica de cada mes",
    x = "Año", y = NULL, caption = CAPTION_BASE
  ) +
  theme_clima +
  theme(
    legend.key.height = unit(1.2, "cm"),
    panel.grid        = element_blank()
  )

p12 <- h1 / h2 +
  plot_annotation(
    title    = "Heatmap Climático Mensual 1968-2024",
    subtitle = "Rojo = por encima de la media | Azul/Marrón = por debajo de la media",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  )


# =============================================================================
# SECCIÓN 10 - GUARDAR GRÁFICOS
# Todos pasan por guardar() → logo automático en absolutamente todos
# =============================================================================

cat("\n--- Guardando paneles ---\n")

panel_temp <- (p1 / p2 / p3) +
  plot_annotation(
    title = "Análisis de Temperatura Histórica 1968-2024",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )
guardar(panel_temp, "graficos/01_temperatura.png", ancho = 14, alto = 18)

panel_precip <- (p4 / p5 / p7) +
  plot_annotation(
    title = "Precipitación y Eventos Extremos 1968-2024",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )
guardar(panel_precip, "graficos/02_precipitacion_extremos.png", ancho = 14, alto = 18)

panel_patron <- (p6 | p10) / (p8 | p11) +
  plot_annotation(
    title = "Patrones, Intensificación y Correlaciones",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )
guardar(panel_patron, "graficos/03_patrones.png", ancho = 16, alto = 14)

panel_heatmap <- p12 +
  plot_annotation(
    title = "Heatmap Climático Mensual 1968-2024",
    theme = theme(plot.title = element_text(face = "bold", size = 15))
  )
guardar(panel_heatmap, "graficos/04_heatmap.png", ancho = 16, alto = 10)

cat("\n--- Guardando gráficos individuales ---\n")
guardar(p3,  "graficos/05_regresion_temperatura.png",  ancho = 14, alto = 7)
guardar(p9,  "graficos/06_extremos_mensuales.png",     ancho = 12, alto = 6)
guardar(p11, "graficos/07_ridge_distribucion.png",     ancho = 12, alto = 6)
guardar(p6,  "graficos/08_dias_lluvia_intensidad.png", ancho = 12, alto = 10)

cat("\n✔ Gráficos guardados en ./graficos/\n")


# =============================================================================
# SECCIÓN 11 - TABLAS EN EXCEL
# =============================================================================

tabla_descriptivos <- anual %>%
  select(
    Temp_Media_Anual, Temp_Max_Media, Temp_Min_Media,
    Amplitud_Termica, Precip_Total, Dias_Lluvia,
    Intensidad_Media, Dias_P95, Dias_P99,
    Precip_Max_Diaria, Dias_Calor_Ext, Dias_Helada,
    Racha_Seca_Max, Racha_Humeda_Max
  ) %>%
  summarise(across(everything(), list(
    Media  = ~ round(mean(., na.rm = TRUE), 2),
    SD     = ~ round(sd(., na.rm = TRUE), 2),
    Min    = ~ round(min(., na.rm = TRUE), 2),
    P25    = ~ round(quantile(., 0.25, na.rm = TRUE), 2),
    Median = ~ round(median(., na.rm = TRUE), 2),
    P75    = ~ round(quantile(., 0.75, na.rm = TRUE), 2),
    Max    = ~ round(max(., na.rm = TRUE), 2)
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", "Estadistico"),
               names_sep = "__") %>%
  pivot_wider(names_from = Estadistico, values_from = value)

tabla_anual <- anual %>%
  select(
    Year, Temp_Media_Anual, Temp_Max_Media, Temp_Min_Media,
    Amplitud_Termica, Anomalia, Precip_Total, Dias_Lluvia,
    Intensidad_Media, Dias_P90, Dias_P95, Dias_P99,
    Precip_Max_Diaria, Frac_Extrema, Dias_Calor_Ext,
    Dias_Helada, Racha_Seca_Max, Racha_Humeda_Max
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

tabla_decadas <- anual %>%
  mutate(Decada = paste0(floor(Year / 10) * 10, "s")) %>%
  group_by(Decada) %>%
  summarise(
    N_anos             = n(),
    Temp_Media         = round(mean(Temp_Media_Anual,  na.rm = TRUE), 2),
    Temp_Max_Media     = round(mean(Temp_Max_Media,    na.rm = TRUE), 2),
    Temp_Min_Media     = round(mean(Temp_Min_Media,    na.rm = TRUE), 2),
    Amplitud_Termica   = round(mean(Amplitud_Termica,  na.rm = TRUE), 2),
    Precip_Total_Media = round(mean(Precip_Total,      na.rm = TRUE), 1),
    Precip_Total_SD    = round(sd(Precip_Total,        na.rm = TRUE), 1),
    Dias_Lluvia_Media  = round(mean(Dias_Lluvia,       na.rm = TRUE), 1),
    Intensidad_Media   = round(mean(Intensidad_Media,  na.rm = TRUE), 2),
    Dias_P95_Media     = round(mean(Dias_P95,          na.rm = TRUE), 1),
    Dias_P99_Total     = sum(Dias_P99,                 na.rm = TRUE),
    Precip_Max_Abs     = round(max(Precip_Max_Diaria,  na.rm = TRUE), 1),
    Dias_Calor_Ext     = round(mean(Dias_Calor_Ext,    na.rm = TRUE), 1),
    Dias_Helada        = round(mean(Dias_Helada,       na.rm = TRUE), 1),
    Racha_Seca_Max     = round(mean(Racha_Seca_Max,    na.rm = TRUE), 1),
    .groups = "drop"
  )

tabla_eras <- anual %>%
  mutate(Era = case_when(
    Year <= 1989 ~ "1968-1989",
    Year <= 2009 ~ "1990-2009",
    TRUE         ~ "2010-2024"
  )) %>%
  group_by(Era) %>%
  summarise(
    N_anos             = n(),
    Temp_Media         = round(mean(Temp_Media_Anual,  na.rm = TRUE), 2),
    Temp_Max_Media     = round(mean(Temp_Max_Media,    na.rm = TRUE), 2),
    Temp_Min_Media     = round(mean(Temp_Min_Media,    na.rm = TRUE), 2),
    Precip_Total_Media = round(mean(Precip_Total,      na.rm = TRUE), 1),
    Precip_Total_SD    = round(sd(Precip_Total,        na.rm = TRUE), 1),
    Dias_Lluvia_Media  = round(mean(Dias_Lluvia,       na.rm = TRUE), 1),
    Intensidad_Media   = round(mean(Intensidad_Media,  na.rm = TRUE), 2),
    Dias_P95_Media     = round(mean(Dias_P95,          na.rm = TRUE), 1),
    Dias_P99_Total     = sum(Dias_P99,                 na.rm = TRUE),
    Precip_Max_Abs     = round(max(Precip_Max_Diaria,  na.rm = TRUE), 1),
    Dias_Calor_Ext     = round(mean(Dias_Calor_Ext,    na.rm = TRUE), 1),
    Dias_Helada        = round(mean(Dias_Helada,       na.rm = TRUE), 1),
    Racha_Seca_Max_Med = round(mean(Racha_Seca_Max,    na.rm = TRUE), 1),
    .groups = "drop"
  )

tabla_top_eventos <- df %>%
  filter(!is.na(Precip), Precip > 0) %>%
  arrange(desc(Precip)) %>%
  slice_head(n = 20) %>%
  select(
    Fecha, Year, Month,
    Precip_mm      = Precip,
    Temp_Media_dia = Temp_Media,
    Temp_Max_dia   = Temp_Max,
    Temp_Min_dia   = Temp_Min
  ) %>%
  mutate(
    Rango_Percentil = case_when(
      Precip_mm >= P99 ~ "P99+",
      Precip_mm >= P95 ~ "P95-P99",
      Precip_mm >= P90 ~ "P90-P95",
      TRUE             ~ "<P90"
    ),
    across(where(is.numeric), ~ round(., 1))
  )

tabla_etccdi  <- indices_etccdi %>% mutate(across(where(is.numeric), ~ round(., 2)))
tabla_mensual <- temp_mensual_era %>% mutate(across(where(is.numeric), ~ round(., 2)))
tabla_heatmap <- heatmap_data %>%
  select(Year, Month, Month_Label, Temp_Media_Mes, Precip_Mes,
         Temp_Anomalia, Precip_Anomalia) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

wb <- createWorkbook()

estilo_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#2166ac",
  halign = "CENTER", textDecoration = "bold", border = "TopBottomLeftRight"
)
estilo_datos <- createStyle(border = "TopBottomLeftRight", halign = "CENTER", numFmt = "0.00")
estilo_zebra <- createStyle(fgFill = "#dce6f1", border = "TopBottomLeftRight",
                            halign = "CENTER", numFmt = "0.00")

agregar_hoja <- function(wb, nombre_hoja, datos, descripcion = "") {
  addWorksheet(wb, nombre_hoja)
  fila_inicio <- 1
  if (descripcion != "") {
    writeData(wb, nombre_hoja, descripcion, startRow = 1, startCol = 1)
    addStyle(wb, nombre_hoja,
             createStyle(fontColour = "#555555", fontSize = 9,
                         textDecoration = "italic"),
             rows = 1, cols = 1)
    fila_inicio <- 3
  }
  writeData(wb, nombre_hoja, datos,
            startRow = fila_inicio, startCol = 1,
            headerStyle = estilo_header)
  n_filas <- nrow(datos)
  n_cols  <- ncol(datos)
  for (i in seq_len(n_filas)) {
    estilo <- if (i %% 2 == 0) estilo_zebra else estilo_datos
    addStyle(wb, nombre_hoja, estilo,
             rows = fila_inicio + i, cols = 1:n_cols, gridExpand = TRUE)
  }
  setColWidths(wb, nombre_hoja, cols = 1:n_cols, widths = "auto")
}

agregar_hoja(wb, "T1_Descriptivos",    tabla_descriptivos,
             "Estadísticos descriptivos generales por variable (1968-2024)")
agregar_hoja(wb, "T2_Resumen_Anual",   tabla_anual,
             "Resumen anual completo — una fila por año")
agregar_hoja(wb, "T3_Por_Decada",      tabla_decadas,
             "Comparación de variables climáticas por década")
agregar_hoja(wb, "T4_Por_Era",         tabla_eras,
             "Comparación por eras: 1968-1989 / 1990-2009 / 2010-2024")
agregar_hoja(wb, "T5_Top20_Eventos",   tabla_top_eventos,
             "Top 20 eventos diarios extremos de precipitación del registro")
agregar_hoja(wb, "T6_ETCCDI_Anual",    tabla_etccdi,
             "Índices climáticos ETCCDI (IPCC) calculados anualmente")
agregar_hoja(wb, "T7_Periodos_Retorno", rl_df,
             "Niveles de retorno GEV — precipitación esperada cada T años (mm/día)")
agregar_hoja(wb, "T8_Climatol_Mensual", tabla_mensual,
             "Temperatura y precipitación mensual promedio por era")
agregar_hoja(wb, "T9_Heatmap_Datos",   tabla_heatmap,
             "Valores mensuales de temperatura y precipitación + anomalías (base del heatmap)")

# --- T10: Heliofanía y Evapotranspiración potencial anuales ---
# Los años 2021-2023 ya fueron excluidos del dataframe df (ver Sección 1)
# Se agregan como filas NA para mantener el registro histórico completo
tabla_helio_etp <- df %>%
  group_by(Year) %>%
  summarise(
    Heliofania_Total_hs = round(sum(Heliofania, na.rm = TRUE), 1),
    ETP_Total_mm        = round(sum(ETP,        na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  bind_rows(
    data.frame(
      Year                = ANIOS_EXCLUIR,
      Heliofania_Total_hs = NA_real_,
      ETP_Total_mm        = NA_real_
    )
  ) %>%
  arrange(Year) %>%
  mutate(
    Nota = case_when(
      Year %in% ANIOS_EXCLUIR ~ "Excluido — registros incompletos período COVID-19",
      TRUE                     ~ ""
    )
  )

agregar_hoja(wb, "T10_Heliofania_ETP", tabla_helio_etp,
             "Totales anuales de heliofanía efectiva (hs/año) y evapotranspiración potencial (mm/año) | 2021-2023 excluidos por registros incompletos")
# --- T10b: Descriptivos de Heliofanía y ETP (base de Tabla 4 del manuscrito) ---
tabla_helio_etp_desc <- tabla_helio_etp %>%
  filter(is.na(Nota) | Nota == "") %>%          # excluye los años NA
  summarise(
    across(
      c(Heliofania_Total_hs, ETP_Total_mm),
      list(
        Media   = ~round(mean(.,   na.rm = TRUE), 1),
        DE      = ~round(sd(.,     na.rm = TRUE), 1),
        Minimo  = ~round(min(.,    na.rm = TRUE), 1),
        P25     = ~round(quantile(., 0.25, na.rm = TRUE), 1),
        Mediana = ~round(median(., na.rm = TRUE), 1),
        P75     = ~round(quantile(., 0.75, na.rm = TRUE), 1),
        Maximo  = ~round(max(.,    na.rm = TRUE), 1)
      )
    )
  )

agregar_hoja(wb, "T10b_Helio_ETP_Desc", tabla_helio_etp_desc,
             "Estadísticos descriptivos de heliofanía y ETP — base de Tabla 4 del manuscrito")

agregar_hoja(wb, "T11_MannKendall", tabla_mk,
             "Mann-Kendall + pendiente de Sen — variables anuales principales | ~ p<=0.10 | * p<0.05 | ** p<0.01 | *** p<0.001")

saveWorkbook(wb, file = "tablas/Resumen_Climatico_Completo.xlsx", overwrite = TRUE)
cat("\n✔ Tablas guardadas en: tablas/Resumen_Climatico_Completo.xlsx (T1 a T10)\n")


# =============================================================================
# SECCIÓN 12 - FIGURAS PARA PUBLICACIÓN
# 180mm = 1 columna | 360mm = 2 columnas — estándar journals
# Logo incluido automáticamente a través de guardar()
# =============================================================================

guardar_pub <- function(plot, nombre, ancho_mm = 180, alto_mm = 120) {
  guardar(
    plot  = plot,
    ruta  = file.path("graficos", "publicacion", paste0(nombre, ".png")),
    ancho = ancho_mm / 25.4,
    alto  = alto_mm  / 25.4,
    dpi   = 300
  )
}

# -----------------------------------------------------------------------------
# Exportación vectorial PDF (sin logo — para maquetación editorial)
# -----------------------------------------------------------------------------
dir.create("graficos/publicacion/pdf", showWarnings = FALSE)

guardar_pub_pdf <- function(plot, nombre, ancho_mm = 180, alto_mm = 120) {
  ggsave(
    filename = file.path("graficos", "publicacion", "pdf", paste0(nombre, ".pdf")),
    plot     = plot,
    width    = ancho_mm / 25.4,
    height   = alto_mm  / 25.4,
    device   = "pdf"
  )
  cat(sprintf("  ✔ PDF: %s\n", nombre))
}

cat("\n--- Exportando figuras para publicación ---\n")

figuras_pub <- list(
  list(p = p1,  n = "Fig01_anomalia_temperatura",                      w = 180, h = 110),
  list(p = p2,  n = "Fig02_temperatura_max_min",                       w = 180, h = 110),
  list(p = p3,  n = "Fig03_regresion_temperatura",                     w = 360, h = 130),
  list(p = p4,  n = "Fig04_precipitacion_anual",                       w = 180, h = 110),
  list(p = p5,  n = "Fig05_dias_extremos_P95_P99",                     w = 180, h = 110),
  list(p = p6,  n = "Fig06_paradoja_intensificacion",                  w = 180, h = 160),
  list(p = p7,  n = "Fig07_CDD_rachas_secas",                          w = 180, h = 110),
  list(p = p8,  n = "Fig08_RX1day_RX5day",                             w = 180, h = 110),
  list(p = p9,  n = "Fig09_extremos_mensuales_por_era",                w = 360, h = 120),
  list(p = p10, n = "Fig10_scatter_temp_vs_precip",                    w = 180, h = 140),
  list(p = p11, n = "Fig11_ridgeplot_distribucion",                    w = 180, h = 120),
  list(p = p12, n = "Fig12_heatmap_mensual_temperatura_precipitacion", w = 360, h = 200)
)

cat("  → PNG 300 DPI (con logo):\n")
for (f in figuras_pub) guardar_pub(f$p, f$n, ancho_mm = f$w, alto_mm = f$h)

cat("  → PDF vectorial (sin logo — para maquetación editorial):\n")
for (f in figuras_pub) guardar_pub_pdf(f$p, f$n, ancho_mm = f$w, alto_mm = f$h)

cat("\n✔ Figuras guardadas en graficos/publicacion/\n")
cat("  PNG 300 dpi (con logo) → graficos/publicacion/*.png\n")
cat("  PDF vectorial (sin logo) → graficos/publicacion/pdf/*.pdf\n")
cat("  Anchos: 180 mm (1 columna) y 360 mm (2 columnas)\n")

# =============================================================================
#                               * FIN *
# =============================================================================
