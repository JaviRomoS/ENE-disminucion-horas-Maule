rm(list = ls())
options(scipen = 999)

# =========================================================================
# Codigo para procesar bases anualizadas de la ene de 2022 a 2025, para usarlas 
# en la construiccion de gráficos por tramos
# =========================================================================

# 1. Cargar librerias -----------------------------------------------------

library(pacman)
pacman::p_load(tidyverse, rio,scales, tidyr, sjlabelled, here, sjmisc, srvyr,survey, writexl)


# 2. abrir base de datos --------------------------------------------------

ene22 <- read.csv2("input/ene-anualizadas/ano-2022.csv", encoding = "UTF-8")
ene23 <- read.csv2("input/ene-anualizadas/ano-2023.csv", encoding = "UTF-8")
ene24 <- read.csv2("input/ene-anualizadas/ano-2024.csv", encoding = "UTF-8")
ene25 <- read.csv2("input/ene-anualizadas/ano-2025.csv", encoding = "UTF-8")


# 3. Recodificar ----------------------------------------------------------

ene22 <- ene22 %>% select(ano_trimestre,ano_encuesta,mes_central,estrato,conglomerado,idrph,
                          region, sexo,fact_anual,
                          ocup_form,r_p_rev4cl_caenes,categoria_ocupacion, habituales,
                          b15_1, asocia, c10,
                          edad, cine97, categoria_ocupacion,b17_ano, b8, b9, ocup_honorarios,
                          est_conyugal,tramo_edad) 

ene23 <- ene23 %>% select(ano_trimestre,ano_encuesta,mes_central,estrato,conglomerado,idrph,
                          region, sexo,fact_anual,
                          ocup_form,r_p_rev4cl_caenes,categoria_ocupacion, habituales,
                          b15_1, asocia, c10,
                          edad, cine97, categoria_ocupacion,b17_ano, b8, b9, ocup_honorarios,
                          est_conyugal,tramo_edad
                          )

ene24 <- ene24 %>% select(ano_trimestre,ano_encuesta,mes_central,estrato,conglomerado,idrph,
                          region, sexo,fact_anual,
                          ocup_form,r_p_rev4cl_caenes,categoria_ocupacion, habituales,
                          b15_1, asocia, c10,
                          edad, cine97, categoria_ocupacion,b17_ano, b8, b9, ocup_honorarios,
                          est_conyugal,tramo_edad
                          ) 

ene25 <- ene25 %>% select(ano_trimestre,ano_encuesta,mes_central,estrato,conglomerado,idrph,
                          region, sexo,fact_anual,
                          ocup_form,r_p_rev4cl_caenes,categoria_ocupacion, habituales,
                          b15_1, asocia, c10,
                          edad, cine97, categoria_ocupacion,b17_ano, b8, b9, ocup_honorarios,
                          est_conyugal,tramo_edad
                          )

ene <- bind_rows(ene22, ene23, ene24, ene25)

# Recodificar

ene <- ene %>% mutate(habituales = case_when(habituales == 888 ~ NA_real_,
                                             habituales == 999 ~ NA_real_,
                                             TRUE ~ habituales),
                      
                      informal = case_when(ocup_form == 1 ~ 0,
                                           ocup_form == 2 ~ 1,
                                           TRUE ~ NA_real_),
                      
                      sexo = case_when(sexo == 1 ~ sexo,
                                       sexo == 2 ~ sexo,
                                       TRUE ~ NA_real_),
                      
                      mujer = case_when(sexo == 2 ~ 1,
                                        sexo == 1 ~ 0,
                                        TRUE ~ NA_real_),
                      
                      subempleo = case_when (habituales <= 30 & c10 == 1 ~ 1,
                                             TRUE ~ 0),
                      
                      rama = case_when(
                        r_p_rev4cl_caenes == 1 ~ "Agricultura, ganadería, silvicultura y pesca",
                        r_p_rev4cl_caenes == 2 ~ "Minería",
                        r_p_rev4cl_caenes == 3 ~ "Manufactura",
                        r_p_rev4cl_caenes %in% c(4, 5) ~ "Electricidad, gas y agua; desechos",
                        r_p_rev4cl_caenes == 6 ~ "Construcción",
                        r_p_rev4cl_caenes == 7 ~ "Comercio",
                        r_p_rev4cl_caenes %in% c(8, 10) ~ "Transporte, almacenamiento,\ninformación y comunicaciones",
                        r_p_rev4cl_caenes == 9 ~ "Alojamiento y servicio de comidas",
                        r_p_rev4cl_caenes == 11 ~ "Actividades financieras y de seguros",
                        r_p_rev4cl_caenes %in% c(13, 14) ~ "Actividades profesionales y técnicas",
                        r_p_rev4cl_caenes == 16 ~ "Enseñanza",
                        r_p_rev4cl_caenes == 17 ~ "Salud y asistencia social",
                        r_p_rev4cl_caenes %in% c(18, 19, 20, 21, 12, 15) ~ "Actividades artísticas; Otros servicios"),
                      
                      tamano = case_when(b15_1 == 1 ~ "Microempresas",
                                         b15_1 == 2 ~ "Microempresas",
                                         b15_1 == 3 ~ "Pequeña",
                                         b15_1 == 4 ~ "Mediana",
                                         b15_1 == 5 ~ "Grande",
                                         TRUE ~ NA_character_),
                      tamano = as.factor(tamano),
                      
                      region = as.numeric(region),
                      region = factor(region, levels = 1:16, labels = c(
                        "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
                        "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
                        "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
                        "Arica y Parinacota", "Ñuble")),
                      
                      hab_tramos = case_when(habituales >= 1 & habituales < 31 ~ "1 a 30",
                                             habituales > 30 & habituales < 40 ~ "31 a 39",
                                             habituales == 40 ~ "40",
                                             habituales > 40 & habituales < 45 ~ "41 a 44",
                                             habituales == 45 ~ "45",
                                             habituales > 45 ~ "46 o más"),
                      hab_tramos = as.factor(hab_tramos),
                      
                      habituales_rec = case_when(habituales <= 31 ~ 31,
                                                 habituales >= 46 ~ 46,
                                                 TRUE ~ habituales),
                      
                      educ = case_when(cine97 == 9 ~ NA,
                                       TRUE ~ cine97),
                      educ = as.factor(educ),
                      
                      categoria_ocupacion = case_when(categoria_ocupacion %in% c(0, 1, 2, 7) ~ NA,
                                                      TRUE ~ categoria_ocupacion),
                      categoria_ocupacion = as.character(categoria_ocupacion),
                      
                      b17_ano = case_when(b17_ano == 8888 ~ NA,
                                          b17_ano == 9999 ~ NA,
                                          TRUE ~ b17_ano),
                      antiguedad = (as.numeric(ano_encuesta) - as.numeric(b17_ano)),
                      
                      tipo_contrato = case_when(ocup_honorarios == 1 ~ 1,
                                                b9 == 1 ~ 2,
                                                b9 == 2 ~ 3,
                                                TRUE ~ NA_real_),
                      tipo_contrato = as.factor(tipo_contrato),
                      
                      est_conyugal = case_when(est_conyugal == 1 ~ "Casado/a",
                                               est_conyugal == 2 ~ "Conviviente",
                                               est_conyugal == 3 ~ "Soltero/a",
                                               est_conyugal == 4 ~ "Viudo/a",
                                               TRUE ~ NA_character_),
                      con_pareja = case_when(est_conyugal == "Casado/a" ~ 1,
                                             est_conyugal == "Conviviente" ~ 1,
                                             est_conyugal == "Soltero/a" ~ 0,
                                             est_conyugal == "Viudo/a" ~ 0)) 

ene <- ene %>% select(-c(r_p_rev4cl_caenes, b15_1)) 

saveRDS(ene, file = "output/ene_long22-25_proc.rds")
