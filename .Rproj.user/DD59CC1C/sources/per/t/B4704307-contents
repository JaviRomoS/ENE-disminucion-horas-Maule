rm(list = ls())
options(scipen = 999)
options(survey.lonely.psu = "adjust")

# 1. Cargar librerias -----------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, rio,scales, tidyr, sjlabelled, here, sjmisc, srvyr,survey, 
               janitor, knitr, kableExtra, patchwork, writexl, ggeffects)


# 2. abrir base de datos --------------------------------------------------

ene <- readRDS("output/ene_long22-25_proc.rds")
ene <- ene %>% filter(informal != 1)

# Centrar ano_encuesta

ene$year_c <- ene$ano_encuesta - 2022

#Objeto encuesta

ene_pond <- as_survey_design(
  .data = ene,
  ids = 1,
  strata = estrato,
  weights = fact_anual)

# 3. Crear subsets --------------------------------------------------------

subset1 <- subset(ene_pond, region == "Maule")

# 4. Modelos --------------------------------------------------------------


   ## 4.1. Modelo 1 (habituales ~ rama * anio) ----------------------------

model1 <- lm(habituales ~ rama * year_c, data = subset1)

pred <- ggpredict(model1,
                  terms = c("year_c [0:3]", "rama"))

pred <- pred %>% 
  filter(group %in% c("Comercio",
                      "Agricultura, ganadería, silvicultura y pesca",
                      "Manufactura"))

plotm1 <- ggplot(pred, aes(x = x + 2022, y = predicted, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15, color = NA) +
  scale_x_continuous(breaks = c(2022, 2023, 2024, 2025)) +
  labs(
    x = "Año",
    y = "Horas habituales",
    color = "Sector",
    fill = "Sector"
  ) +
  theme_bw()

   ## 4.2. Modelo 2 (habituales ~ rama * anio + mujer) --------------------

model2 <- lm(habituales ~ rama * year_c + mujer + year_c:mujer, data = subset1)

pred <- ggpredict(model2,
                  terms = c("year_c [0:3]", "rama", "mujer"))

pred <- pred %>% 
  filter(group %in% c("Comercio",
                      "Agricultura, ganadería, silvicultura y pesca",
                      "Manufactura"))

pred$mujer <- factor(pred$facet,
                     labels = c("Hombres", "Mujeres"))

plotm2 <- ggplot(pred, aes(x = x + 2022, y = predicted, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15, color = NA) +
  facet_wrap(~ mujer) +
  scale_x_continuous(breaks = c(2022, 2023, 2024, 2025)) +
  labs(
    x = "Año",
    y = "Horas habituales predichas",
    color = "Sector",
    fill = "Sector"
  ) +
  theme_bw()

     ## 4.3. Modelo 3 (habituales ~ rama * anio + anio:mujer + controles individuales) --------------------

model3 <- lm(habituales ~ rama * year_c +
               mujer +
               antiguedad +
               con_pareja +
               tipo_contrato +
               year_c:mujer, data = subset1)

pred3_hombres <- ggpredict(
  model3,
  terms = c("year_c [0:2]", "rama"),
  condition = list(mujer = 0)
) %>%
  filter(group %in% c("Comercio",
                      "Agricultura, ganadería, silvicultura y pesca",
                      "Manufactura"))

pred3_mujeres <- ggpredict(
  model3,
  terms = c("year_c [0:2]", "rama"),
  condition = list(mujer = 1)
) %>%
  filter(group %in% c("Comercio",
                      "Agricultura, ganadería, silvicultura y pesca",
                      "Manufactura"))

plotm3_h <- ggplot(pred3_hombres, aes(x = x + 2022, y = predicted, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15, color = NA) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    x = "Año",
    y = "Horas habituales predichas",
    color = "Sector",
    fill = "Sector",
    title = "Hombres"
  ) +
  ylim(30,43)+
  theme_bw()

plotm3_m <- ggplot(pred3_mujeres, aes(x = x + 2022, y = predicted, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15, color = NA) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    x = "Año",
    y = "Horas habituales predichas",
    color = "Sector",
    fill = "Sector",
    title = "Mujeres"
  ) +
  ylim(30,43)+
  theme_bw()

plotm3_hym <- plotm3_h + plotm3_m +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom")

## 4.4. Modelo 4 (habituales ~ rama * anio + anio:mujer + controles individuales + estructurales) --------------------

model4 <- lm(habituales ~ rama * year_c +
               mujer +
               antiguedad +
               con_pareja +
               tipo_contrato +
               tamano +
               year_c:mujer, data = subset1)


## 4.5. Modelo 5 (habituales ~ est_conyugal * anio) --------------------

model_ec_mujer <- lm(
  habituales ~ con_pareja * year_c,
  data = subset1 %>% dplyr::filter(mujer == 1))

model_ec_hombre <- lm(
  habituales ~ con_pareja * year_c,
  data = subset1 %>% dplyr::filter(mujer == 0))

pred_h <- ggpredict(model_ec_hombre,
                    terms = c("year_c [0:3]", "con_pareja"))
pred_h$sexo <- "Hombres"

pred_m <- ggpredict(model_ec_mujer,
                    terms = c("year_c [0:3]", "con_pareja"))
pred_m$sexo <- "Mujeres"

pred_m5 <- bind_rows(pred_h, pred_m)

ggplot(pred_m5, aes(x = x + 2022, y = predicted, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.15, color = NA) +
  facet_wrap(~ sexo) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  labs(
    x = "Año",
    y = "Horas habituales predichas",
    color = "Tiene pareja",
    fill = "Tiene pareja"
  ) +
  theme_bw()

# 5. Tablas cruzadas ------------------------------------------------------

# Promedio de horas habituales desagregada por género en tres actividades de interés

generoxrama <- ene %>%
  filter(
    region == "Maule",
    rama %in% c("Agricultura, ganadería, silvicultura y pesca",
                "Comercio",
                "Manufactura"),
    !is.na(habituales)
  ) %>%
  mutate(rama = case_when(rama == "Agricultura, ganadería, silvicultura y pesca" ~ "Agricultura, ganadería,\n silvicultura y pesca",
                          TRUE ~ rama))%>% 
  group_by(rama, mujer) %>%
  summarise(
    n = n(),
    media = weighted.mean(habituales, fact_anual, na.rm = TRUE),
    sd = sd(habituales, na.rm = TRUE),
    se = sd / sqrt(n),
    li = media - 1.96 * se,
    ls = media + 1.96 * se,
    .groups = "drop"
  ) %>%
 mutate(sexo = ifelse(mujer == 1, "Mujeres", "Hombres")) %>%
  select(rama, sexo, media, li, ls, n)


plot1 <- ggplot(generoxrama,
                     aes(x = rama, y = media, shape = sexo, group = sexo)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = li, ymax = ls),
                position = position_dodge(width = 0.4),
                width = 0.2) +
  labs(
    x = "",
    y = "Promedio de horas trabajadas",
    shape = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

# Genero y estado conyugal

generoxrama <- ene %>%
  filter(
    region == "Maule",
    rama %in% c("Agricultura, ganadería, silvicultura y pesca",
                "Comercio",
                "Manufactura"),
    !is.na(habituales_rec),
    !is.na(mujer),
    !is.na(est_conyugal)
  ) %>%
  mutate(
    sexo = ifelse(mujer == 1, "Mujeres", "Hombres"),
    est_conyugal2 = case_when(
      est_conyugal %in% c("Casado/a", "Conviviente") ~ "Con pareja",
      est_conyugal %in% c("Soltero/a", "Viudo/a") ~ "Solteros/as o viudos/as"
    ),
    grupo = paste(sexo, est_conyugal2, sep = " - "),
    rama = case_when(
      rama == "Agricultura, ganadería, silvicultura y pesca" ~ "Agricultura, ganadería,\nsilvicultura y pesca",
      TRUE ~ rama
    )
  ) %>%
  group_by(rama, sexo, est_conyugal2, grupo) %>%
  summarise(
    n = n(),
    media = weighted.mean(habituales_rec, fact_anual, na.rm = TRUE),
    sd = sd(habituales_rec, na.rm = TRUE),
    se = sd / sqrt(n),
    li = media - 1.96 * se,
    ls = media + 1.96 * se,
    .groups = "drop"
  )

plot1 <- ggplot(generoxrama,
                aes(x = rama, y = media, color = grupo, group = grupo)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(ymin = li, ymax = ls),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  labs(
    x = "",
    y = "Promedio de horas trabajadas",
    color = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )
