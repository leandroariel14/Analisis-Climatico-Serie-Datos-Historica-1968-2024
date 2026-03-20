# Análisis Climático Histórico — Estación Meteorológica Colonia Benítez (1968–2024)

Análisis estadístico de la serie histórica de la Estación Meteorológica Convencional (EMC)

de Colonia Benítez, Chaco, Argentina. El trabajo caracteriza el comportamiento climático

local a través de indicadores de temperatura, precipitación, heliofanía y evapotranspiración

potencial, con énfasis en la detección de tendencias y eventos extremos.

---

## Contexto

En Colonia Benítez los primeros registros climáticos corresponden a mediciones pluviométricas

iniciadas en 1925. En 1968 se estableció la EMC, permitiendo ampliar la cobertura de variables

observadas. La serie analizada comprende más de 20.000 registros diarios y abarca

aproximadamente 54 años efectivos de observaciones continuas (1968–2024, excluyendo 2021–2023

por registros incompletos).

El objetivo es aportar información cuantitativa que reduzca la incertidumbre asociada al

impacto del clima sobre los sistemas productivos —particularmente agricultura y ganadería—

y sobre las dinámicas socioeconómicas locales.

---

## Estructura del repositorio


├── data/

│   └── Serie\_Historica\_Varios\_Instrumentos1968-012025.xls   # Datos fuente (SIGA-INTA)

│

├── source/

│   └── analisis\_climatico\_FINAL.R                           # Script principal de análisis

│

├── docs/

│   ├── graficos/                                            # Figuras generadas (PNG)

│   │   └── publicacion/                                     # Figuras a 300 dpi para publicación

│   ├── tablas/

│   │   └── Resumen\_Climatico\_Completo.xlsx                  # Tablas de resultados (T1–T10)

│   └── Publicación\_Alejandro\_Juan\_Eda.docx                  # Documento de publicación

│

└── README.md

---

## Datos

Los datos fueron obtenidos del **Sistema de Información y Gestión Agrometeorológica del INTA**

(SIGA): https://siga.inta.gob.ar

**Variables analizadas:**

- Temperatura media, máxima y mínima diaria (abrigo meteorológico, 150 cm)

- Precipitación pluviométrica diaria

- Heliofanía efectiva (horas de radiación solar directa)

- Evapotranspiración potencial (ETP)

**Período:** 1968–2024 (años 2021, 2022 y 2023 excluidos por registros incompletos)

---

## Requisitos

- **R** versión 4.5.1 o superior

- **RStudio** 2025.09.1+401 "Cucumberleaf Sunflower" (recomendado)

### Paquetes R necesarios

install.packages(c(

&#x20; "readxl", "dplyr", "lubridate", "ggplot2", "patchwork",

&#x20; "scales", "tidyr", "zoo", "trend", "extRemes",

&#x20; "ggridges", "openxlsx", "cowplot", "magick"

))

---

## Uso

1. Clonar el repositorio

2. Colocar el archivo de datos en la carpeta data/

3. Colocar el archivo logo.png en la carpeta source/ (opcional — el script funciona sin logo)

4. Abrir source/analisis\_climatico\_FINAL.R en RStudio

5. Ajustar RUTA\_ARCHIVO en la Sección 2 si es necesario

6. Ejecutar el script completo

Los resultados se generan automáticamente en docs/graficos/ y docs/tablas/.

---

## Resultados generados

| Archivo | Contenido |

|---|---|

| docs/graficos/01\_temperatura.png | Panel de temperatura histórica |

| docs/graficos/02\_precipitacion\_extremos.png | Panel de precipitación y eventos extremos |

| docs/graficos/03\_patrones.png | Patrones, intensificación y correlaciones |

| docs/graficos/04\_heatmap.png | Heatmap climático mensual |

| docs/graficos/publicacion/Fig01–Fig12 | Figuras individuales a 300 dpi |

| docs/tablas/Resumen\_Climatico\_Completo.xlsx | 10 hojas con todos los resultados |

---

## Nota metodológica

Los años 2021, 2022 y 2023 fueron excluidos del análisis por presentar registros incompletos

asociados a la interrupción operativa de la estación durante el período COVID-19.

Los valores de esos años se ubican entre 5 y 15 desviaciones estándar por debajo de la

media histórica, descartando su carácter como variabilidad climática real.

Se mantienen como NA en la Tabla T10 (heliofanía y ETP) para transparencia del registro.

---

## Asistencia de IA

La optimización y depuración del código R, así como la redacción de descripciones de tablas

y figuras, contaron con asistencia del modelo Claude (Anthropic, claude.ai).

El análisis estadístico, la interpretación de los resultados y las conclusiones son de

exclusiva responsabilidad del autor.

---

## Autor

**Alejandro Juan Marcelo Leandro**

Estación Experimental Agropecuaria Colonia Benítez — INTA

Chaco, Argentina

---

## Fuente de datos

INTA — Sistema de Información y Gestión Agrometeorológica (SIGA)

https://siga.inta.gob.ar
