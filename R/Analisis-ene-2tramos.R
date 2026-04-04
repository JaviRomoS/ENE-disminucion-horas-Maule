rm(list = ls())
options(scipen = 999)

# 1. Cargar librerias -----------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, rio,scales, tidyr, sjlabelled, here, sjmisc, srvyr,survey, 
               janitor, knitr, kableExtra, writexl)


# 2. abrir base de datos --------------------------------------------------

ene <- readRDS("output/ene_long22-25_proc.rds")


# 3. Filtrar --------------------------------------------------------------

ene <- ene %>% filter(informal!=1)

subset_maule <- ene %>% filter(region == "Maule")

subset_maule_agro <- subset_maule %>% filter(rama == "Agricultura, ganadería, silvicultura y pesca")
subset_maule_comercio <- subset_maule %>% filter(rama == "Comercio")
subset_maule_manufactura <- subset_maule %>% filter(rama == "Manufactura")

# =======================================================================
# 4. --------------------------- ANALISIS -------------------------------
# =======================================================================

## 4.1. Subempleo ---------------------------------------------------------

# a. Por region

tabla1 <- ene %>%
  tabyl(region, subempleo) %>%          
  adorn_percentages("row") %>%         
  adorn_pct_formatting(digits = 1) %>%  
  adorn_ns()                           

tabla1 %>%
  kable(format = "html", caption = "Subempleo por región (%)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

# b. Por rama en el Maule 

tabla2 <- subset_maule %>%
  tabyl(rama, subempleo) %>%          
  adorn_percentages("row") %>%         
  adorn_pct_formatting(digits = 1) %>%  
  adorn_ns()                           

tabla2 %>%
  kable(format = "html", caption = "Subempleo por rama (%) en el Maule") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


## 4.2. Horas habituales (tramos) 2022 a 2025 -----------------------------

### a. País y todos los sectores ----

ene_pond <- as_survey_design(
  .data = ene,
  ids = 1,
  strata = estrato,
  weights = fact_anual
)

tabla3 <- ene_pond %>%  
  group_by(ano_trimestre, mes_central, hab_tramos) %>%  
  summarise(
    total_trabajadores = survey_total(na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ano_trimestre, mes_central) %>% 
  mutate(
    prop = total_trabajadores / sum(total_trabajadores) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(prop), !is.na(hab_tramos)) %>%
  mutate(
    periodo = make_date(
      year = ano_trimestre,
      month = mes_central,
      day = 1
    ),
    hab_tramos = factor(
      hab_tramos,
      levels = c("1 a 30", "31 a 39", "40", "41 a 44", "45", "46 o más")
    )
  )

plot1 <- ggplot(
  tabla3,
  aes(x = periodo, y = prop, color = hab_tramos, group = hab_tramos)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "1 a 30"   = "#f8766d",
      "31 a 39"  = "#c68d40",
      "40"       = "#bc0bbd",
      "41 a 44"  = "#399564",
      "45"       = "#619CFF",
      "46 o más" = "#f564e3"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 90)
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Porcentaje de trabajadores por tramo de horas (2020-2025)",
    subtitle = "Total país",
    x = NULL,
    y = "%",
    color = "Tramo de horas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

### b. Maule Agroindustria ----

subset_maule_agro_pond <- as_survey_design(
  .data = subset_maule_agro,
  ids = 1,
  strata = estrato,
  weights = fact_anual
)

tabla4 <- subset_maule_agro_pond %>%  
  group_by(ano_trimestre, mes_central, hab_tramos) %>%  
  summarise(
    total_trabajadores = survey_total(na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ano_trimestre, mes_central) %>% 
  mutate(
    prop = total_trabajadores / sum(total_trabajadores) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(prop), !is.na(hab_tramos)) %>%
  mutate(
    periodo = make_date(
      year = ano_trimestre,
      month = mes_central,
      day = 1
    ),
    hab_tramos = factor(
      hab_tramos,
      levels = c("1 a 30", "31 a 39", "40", "41 a 44", "45", "46 o más")
    )
  )

plot2 <- ggplot(
  tabla4,
  aes(x = periodo, y = prop, color = hab_tramos, group = hab_tramos)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "1 a 30"   = "#f8766d",
      "31 a 39"  = "#c68d40",
      "40"       = "#bc0bbd",
      "41 a 44"  = "#399564",
      "45"       = "#619CFF",
      "46 o más" = "#f564e3"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 90)
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Porcentaje de trabajadores por tramo de horas (2020-2025)",
    subtitle = "Agricultura, ganadería y pesca en la Región del Maule",
    x = NULL,
    y = "%",
    color = "Tramo de horas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

### c. Maule Comercio ----

subset_maule_comercio_pond <- as_survey_design(
  .data = subset_maule_comercio,
  ids = 1,
  strata = estrato,
  weights = fact_anual
)

tabla5 <- subset_maule_comercio_pond %>%  
  group_by(ano_trimestre, mes_central, hab_tramos) %>%  
  summarise(
    total_trabajadores = survey_total(na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ano_trimestre, mes_central) %>% 
  mutate(
    prop = total_trabajadores / sum(total_trabajadores) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(prop), !is.na(hab_tramos)) %>%
  mutate(
    periodo = make_date(
      year = ano_trimestre,
      month = mes_central,
      day = 1
    ),
    hab_tramos = factor(
      hab_tramos,
      levels = c("1 a 30", "31 a 39", "40", "41 a 44", "45", "46 o más")
    )
  )

plot3 <- ggplot(
  tabla5,
  aes(x = periodo, y = prop, color = hab_tramos, group = hab_tramos)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "1 a 30"   = "#f8766d",
      "31 a 39"  = "#c68d40",
      "40"       = "#bc0bbd",
      "41 a 44"  = "#399564",
      "45"       = "#619CFF",
      "46 o más" = "#f564e3"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 90)
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Porcentaje de trabajadores por tramo de horas (2020-2025)",
    subtitle = "Comercio en la Región del Maule",
    x = NULL,
    y = "%",
    color = "Tramo de horas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

### d. Maule Manufactura ----

subset_maule_manufactura_pond <- as_survey_design(
  .data = subset_maule_manufactura,
  ids = 1,
  strata = estrato,
  weights = fact_anual
)

tabla6 <- subset_maule_manufactura_pond %>%  
  group_by(ano_trimestre, mes_central, hab_tramos) %>%  
  summarise(
    total_trabajadores = survey_total(na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ano_trimestre, mes_central) %>% 
  mutate(
    prop = total_trabajadores / sum(total_trabajadores) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(prop), !is.na(hab_tramos)) %>%
  mutate(
    periodo = make_date(
      year = ano_trimestre,
      month = mes_central,
      day = 1
    ),
    hab_tramos = factor(
      hab_tramos,
      levels = c("1 a 30", "31 a 39", "40", "41 a 44", "45", "46 o más")
    )
  )

plot4 <- ggplot(
  tabla6,
  aes(x = periodo, y = prop, color = hab_tramos, group = hab_tramos)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "1 a 30"   = "#f8766d",
      "31 a 39"  = "#c68d40",
      "40"       = "#bc0bbd",
      "41 a 44"  = "#399564",
      "45"       = "#619CFF",
      "46 o más" = "#f564e3"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 90)
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Porcentaje de trabajadores por tramo de horas (2020-2025)",
    subtitle = "Manufactura en la Región del Maule",
    x = NULL,
    y = "%",
    color = "Tramo de horas"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# GUARDAR -----------------------------------------------------------------

# Tablas

write_xlsx(
  list(
    tabla3 = tabla3,
    tabla4 = tabla4,
    tabla5 = tabla5,
    tabla6 = tabla6
  ),  "output/analisis_tramos/datos_tabulados.xlsx"
)

#Graficos

graficos <- list(
  plot_totalpais = plot1,
  plot_maule_agro = plot2,
  plot_maule_comercio = plot3,
  plot_maule_manufactura = plot4)

purrr::iwalk(
  graficos,
  ~ ggsave(
    filename = paste0("output/analisis_tramos/plots/", .y, ".png"),
    plot = .x,
    width = 10, height = 6, dpi = 200
  )
)
