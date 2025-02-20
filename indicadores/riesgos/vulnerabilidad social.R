#LIBRERIAS----
library(readxl)
library(tidyverse)
library(units)
library(sf)
library(dplyr)
library(utils)
library(zip)
library(survey)
library(srvyr)
library(stats)


#DIRECTORIO----
setwd("C:/Users/brenp/Desktop/INDICADORES PLAN GENERAL CDMX/RIESGOS")
##DATOS DEL CENSO DE POBLACIÓN Y VIVIENDA (PRINCIPALES RESULTADOS POR MANZANA)----
url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/RESAGEBURB_09_2020_xlsx.zip"
options(timeout = 500)
download.file(url, 'RESAGEBURB_09_2020_xlsx.zip', mode = "wb")

RESAGEBURB_09_2020_xlsx <- unzip('RESAGEBURB_09_2020_xlsx.zip', files = "RESAGEBURB_09XLSX20.xlsx")

RESAGEBURB_09XLSX20 <- readxl::read_excel("RESAGEBURB_09XLSX20.xlsx")|>
  mutate(CVEGEO = paste0(ENTIDAD, MUN, LOC, AGEB),
         CVE_MUN = paste0(ENTIDAD, MUN))|>
  mutate_all(~ gsub("\\*", "", .)) |> 
  mutate(across(9:230, ~ as.numeric(., na.rm = TRUE)))|>
  subset(NOM_LOC == "Total AGEB urbana")

##MARCO GEOESTADÍSTICO NACIONAL 2020----
url <-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/09_ciudaddemexico.zip"
download.file(url, '09_ciudaddemexico.zip', mode = "wb")

zip::unzip("09_ciudaddemexico.zip", exdir = "09_ciudaddemexico")

ageb_ur_mgn_2020 <- st_read("09_ciudaddemexico/conjunto_de_datos/09a.shp")|>
  st_transform(crs = 6362)|>
  mutate(TIPO = "Urbana")

#ESTIMACIÓN DE LA VULNERABILIDAD SOCIAL  (INDICADORES DE CENAPRED)----
##VIVIENDAS CON PAREDES DE MATERIAL DE DESECHO----
url <-"https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/Censo2020_CA_cdmx_csv.zip"
options(timeout = 500)
download.file(url, 'Censo2020_CA_cdmx_csv.zip', mode = "wb")

ca_viviendas <- unzip('Censo2020_CA_cdmx_csv.zip', files = "Viviendas09.csv")

viviendas_09 <- read_csv("Viviendas09.csv", show_col_types = FALSE)|>
  mutate(CVE_MUN = paste0(ENT, MUN))|>
  select(ENT, MUN, CVE_MUN, LOC50K, ID_VIV, PAREDES, PISOS,TOTCUART,NUMPERS, COBERTURA, ESTRATO, UPM, FACTOR)|>
  mutate(across(all_of(1:6), ~ as.character(., na.rm = TRUE)))

viviendas_09_paredes<-viviendas_09|>
  mutate(pared = ifelse(PAREDES %in% c("1", "2"), 1, 0))

dstrat <- viviendas_09_paredes|>
  as_survey_design(strata=ESTRATO,
                   weights = FACTOR,
                   ids=UPM,
                   nest=TRUE)

porcentaje_pared<-dstrat|>
  group_by(CVE_MUN,pared)|>
  summarise(viviendas=survey_total(vartype="cv"),
            porcentaje=survey_prop(vartype="cv"))|>
  filter(pared==1)|>
  mutate(PPAREDES=porcentaje*100)|>
  select(CVE_MUN, PPAREDES)|>
  distinct(CVE_MUN, .keep_all = TRUE)

##POBLACIÓN OCUPADA QUE RECIBE MENOS DE DOS SALARIOS MÍNIMOS----

ca_personas <- unzip('Censo2020_CA_cdmx_csv.zip', files = "Personas09.csv")

Personas_09 <- read_csv("Personas09.csv", show_col_types = FALSE)|>
  mutate(CVE_MUN = paste0(ENT, MUN))|>
  select(ENT, MUN, CVE_MUN, LOC50K, ID_VIV, EDAD, OCUPACION_C,CONACT, INGTRMEN,COBERTURA, ESTRATO, UPM, FACTOR)|>
  mutate(across(all_of(1:5), ~ as.character(., na.rm = TRUE)))

psalarios <- Personas_09|>
  mutate(edades = ifelse(EDAD >=15 & EDAD<=130, 1, 0), 
       trabajo = ifelse(CONACT %in% c("10", "13", "14", "15", "16", "17", "18", "19", "20", "30"), 1, 0), 
       salarios = ifelse(INGTRMEN < (3696.6)*2, 1, 0), 
       psalmin = ifelse(edades == 1 & trabajo & salarios == 1, 1, 0))%>%
  filter(edades==1, trabajo==1)

dstrat <- psalarios|>
  as_survey_design(strata=ESTRATO,
                   weights = FACTOR,
                   ids=UPM,
                   nest=TRUE)

porcentaje_psalar<-dstrat|>
  group_by(CVE_MUN,psalmin)|>
  summarise(psalariominimo=survey_total(vartype="cv"),
            porcentaje=survey_prop(vartype="cv"))|>
  filter(psalmin==1)|>
  mutate(PSALARIOSMINIMOS=porcentaje*100)|>
  select(CVE_MUN, PSALARIOSMINIMOS)|>
  distinct(CVE_MUN, .keep_all = TRUE)

##LISTA DE IDICADORES VULNERABILIDAD SOCIAL----
#Más información consultar el siguente manual https://www.cenapred.unam.mx/es/Publicaciones/archivos/57.pdf
lista_indicadores <- readxl::read_excel("lista_de_indicadore.xlsx")
nom_variables <- lista_indicadores$NOM_CENSO

##INDICADOR DE SECRETARIA DE SALUD----
datos_salud <- readxl::read_excel("SECRETARIADESALUDCDMX.xlsx")|>
  select(CVE_MUN, Médicos_2019, MI_2020)|>
  rename(Médicos = Médicos_2019)|>
  distinct(CVE_MUN, .keep_all = TRUE)

##INDICADORES CON PRINCIPALES RESULTADOS POR AGEB----
RESAGEBURB_09XLSX20 <- RESAGEBURB_09XLSX20[, names(RESAGEBURB_09XLSX20) %in% nom_variables] |>
  mutate(PT6_14aAE = ((P_6A11+P_12A14)-(P6A11_NOA+P12A14NOA)),
         PT6_14a = P_6A11+P_12A14)  

POBTOTALCAL <- RESAGEBURB_09XLSX20 |>
  group_by(CVE_MUN) |>
  summarise(POBALCAL = sum(POBTOT, na.rm = TRUE), 
            VIVTOTALCAL = sum(TVIVPARHAB, na.rm = TRUE))|>
  ungroup()


DIPO <- RESAGEBURB_09XLSX20 |>
  filter(POBTOT < 2500) |>
  transmute(CVEGEO, CVE_MUN,
            POB2500M = POBTOT,
            PM2500 = ifelse(POB2500M == 0, 0, 1))|>
  distinct(CVEGEO, .keep_all = TRUE)


ageb_ur_mgn_2020_vulne<- ageb_ur_mgn_2020|>
  right_join(RESAGEBURB_09XLSX20, by = "CVEGEO")|>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6, 
         DP = ifelse(POBTOT == 0, 0, POBTOT/area_km2))|>
  as_tibble()|>
  select(CVEGEO, DP)

RESAGEBURB_09XLSX20 <- RESAGEBURB_09XLSX20 |>
  mutate(PPND = (PSINDER / POBTOT) * 100,
         PANF = (P15YM_AN / P_15YMAS) * 100,
         DEB = (PT6_14aAE / PT6_14a) * 100,
         GPE = GRAPROES,
         PVNDAE = (VPH_AGUAFV / TVIVPARHAB) * 100,
         PVND = (VPH_NODREN / TVIVPARHAB) * 100,
         PVNDE = (VPH_S_ELEC / TVIVPARHAB) * 100,
         PVPT = (VPH_PISOTI / TVIVPARHAB) * 100,
         RD = ((POB0_14 + POB65_MAS) / POB15_64) * 100,
         TDA = (PDESOCUP / PEA) * 100,
         PPI = (P5_HLI / P_5YMAS) * 100)|>
  right_join(POBTOTALCAL, by = "CVE_MUN")|> 
  right_join(porcentaje_pared, by = "CVE_MUN")|>
  right_join(datos_salud, by = "CVE_MUN")|>
  mutate(PM = (Médicos / POBALCAL) * 1000)|> 
  right_join(porcentaje_psalar, by = "CVE_MUN")|>
  right_join(DIPO, by = "CVEGEO")|>
  mutate(PMED = (PM * POBTOT) / POBALCAL,
         PVMD = (PPAREDES * TVIVPARHAB) / VIVTOTALCAL,
         POSALMIN = (PSALARIOSMINIMOS * POBTOT) / POBALCAL,
         PDIPO = ifelse(coalesce(PM2500, 0) == 1, (POB2500M / POBALCAL) * 100, 0))

##INDICADORES A NIVEL AGEB----
RESAGEBURB_09XLSX20_vulne<- RESAGEBURB_09XLSX20|>
  right_join(ageb_ur_mgn_2020_vulne, by ="CVEGEO")|>
  select(CVEGEO, PMED, PPND, PANF, DEB, GPE, PVNDAE, PVND, PVNDE, PVMD, PVPT, PRO_OCUP_C, POSALMIN, RD, TDA, DP, PPI, PDIPO)|>
  mutate(VPM = case_when(PMED <= 0.004 ~ 1, 
                         PMED > 0.004 & PMED <= 0.007 ~ 0.75,
                         PMED > 0.007 & PMED <= 0.012 ~ 0.50,
                         PMED > 0.012 & PMED <= 0.025 ~ 0.25, 
                         PMED > 0.025 ~ 0),
         VPPND = case_when(PPND <= 21.17 ~ 0, 
                           PPND > 21.17 & PPND <= 24.65 ~ 0.25,
                           PPND > 24.65 & PPND <= 27.77 ~ 0.50,
                           PPND > 27.77 & PPND <= 32.06 ~ 0.75, 
                           PPND > 32.06 ~ 1),
         VPANF = case_when(PANF <=0.51 ~ 0, 
                           PANF > 0.51 & PANF <= 0.97 ~ 0.25,
                           PANF > 0.97 & PANF <= 1.43 ~ 0.50,
                           PANF > 1.43 & PANF <= 2.08 ~ 0.75, 
                           PANF > 2.08 ~ 1),
         VDEB = case_when(DEB <= 92.72 ~ 1, 
                          DEB > 92.72 & DEB <= 94.82 ~ 0.75,
                          DEB > 94.82 & DEB <= 96.10 ~ 0.50,
                          DEB > 96.10 & DEB <= 97.32 ~ 0.25, 
                          DEB > 97.32 ~ 0), 
         VGPE = case_when(GPE <= 10.12 ~ 1, 
                          GPE > 10.12 & GPE <=10.93  ~ 0.75,
                          GPE > 10.93 & GPE <= 11.83 ~ 0.50,
                          GPE > 11.83 & GPE <= 13.53 ~ 0.25, 
                          GPE > 13.53  ~ 0),   
         VPVNDAE = case_when(PVNDAE <= 0.09 ~ 0, 
                             PVNDAE > 0.09 & PVNDAE <= 0.30 ~ 0.25,
                             PVNDAE > 0.30 & PVNDAE <= 0.61 ~ 0.50,
                             PVNDAE > 0.61 & PVNDAE <= 2.74 ~ 0.75, 
                             PVNDAE > 2.74 ~ 1),
         VPVND = case_when(PVND == 0 ~ 0,
                           PVND > 0 ~ 1), 
         VPVNDE = case_when(PVNDE == 0 ~ 0, 
                            PVNDE > 0 ~ 1),
         VPARED = case_when(PVMD <=0.0003 ~ 0,
                            PVMD > 0.0003 & PVMD <= 0.0005 ~ 0.25,
                            PVMD > 0.0005 & PVMD <= 0.0008 ~ 0.50,
                            PVMD > 0.0008 & PVMD <= 0.0017 ~ 0.75,
                            PVMD > 0.0017 ~ 1),
         VPVPT = case_when(PVPT <= 0.11 ~ 0,
                           PVPT > 0.11 & PVPT <= 0.36 ~ 0.25,
                           PVPT > 0.36 & PVPT <= 0.58 ~ 0.50,
                           PVPT > 0.58 & PVPT <= 1.06 ~ 0.75, 
                           PVPT > 1.06 ~ 1),
         VPROCUOP = case_when(PRO_OCUP_C <= 0.61 ~ 0, 
                              PRO_OCUP_C > 0.61 & PRO_OCUP_C <= 0.77 ~ 0.25, 
                              PRO_OCUP_C > 0.77 & PRO_OCUP_C <= 0.87 ~ 0.50,
                              PRO_OCUP_C > 0.87 & PRO_OCUP_C <= 0.98 ~ 0.75,
                              PRO_OCUP_C > 0.98 ~ 1),
         VPOSALMIN = case_when(POSALMIN <= 0.04 ~ 0,
                               POSALMIN > 0.04 & POSALMIN <= 0.07 ~ 0.25,
                               POSALMIN > 0.07 & POSALMIN <= 0.11 ~ 0.50,
                               POSALMIN > 0.11 & POSALMIN <= 0.22 ~ 0.75, 
                               POSALMIN > 0.22 ~ 1),
         VRD = case_when(RD <= 37.67 ~ 0,
                         RD > 37.67 & RD <= 40.87 ~ 0.25,
                         RD > 40.87 & RD <= 43.01 ~ 0.50,
                         RD > 43.01 & RD <= 45.53 ~ 0.75, 
                         RD > 45.53 ~ 1), 
         VTDA = case_when(TDA <= 1.59 ~ 0,
                          TDA > 1.59 & TDA <= 2.03 ~ 0.25,
                          TDA > 2.03 & TDA <= 2.43 ~ 0.50,
                          TDA > 2.43 & TDA <= 2.96 ~ 0.75, 
                          TDA > 2.96 ~ 1),
         VDP = case_when(DP <= 7764 ~ 0, 
                         DP > 7764 & DP <= 13247 ~ 0.25, 
                         DP > 13247 & DP <= 18365 ~ 0.50,
                         DP > 18365 & DP <= 24284 ~ 0.75,
                         DP > 24284~ 1),
         VPPI = case_when(PPI <= 0.59 ~ 0,
                          PPI > 0.59 & PPI <= 0.85 ~ 0.25,
                          PPI > 0.85 & PPI <= 1.20 ~ 0.50,
                          PPI > 1.20 & PPI <= 1.88 ~ 0.75, 
                          PPI > 1.88 ~ 1), 
         VDIPO= case_when(PDIPO <= 0.19 ~ 0,
                          PDIPO > 0.19 & PDIPO <= 0.27 ~ 0.25,
                          PDIPO > 0.27 & PDIPO <= 0.34 ~ 0.50,
                          PDIPO > 0.34 & PDIPO <= 0.41 ~ 0.75, 
                          PDIPO > 0.41 ~ 1))


#LIBRERIAS ACP----
library(Amelia)
library(corrplot)
library(PerformanceAnalytics)
library(psych)


##ANÁLISIS DE COMPONENTES PRINCIPALES----
#https://www.rpubs.com/Csanchez15/551258

#----
datos_pca<-RESAGEBURB_09XLSX20_vulne|>
  select(VPM, VPPND, VPANF, VDEB, VGPE, VPVNDAE, VPVND, VPVNDE, VPARED, VPVPT, VPROCUOP, VPOSALMIN, VRD, VTDA, VDP, VPPI, VDIPO)

datos_pca <- as.data.frame(scale(datos_pca))
datos_pca[is.na(datos_pca)] <- colMeans(datos_pca, na.rm = TRUE) 

missing(datos_pca)

correlaciones=datos_pca

cor_matrix <- cor(correlaciones)
corrplot(cor_matrix)
chart.Correlation(correlaciones,histogram = F ,pch = 19)
kmo_result <- KMO(correlaciones)
kmo_table <- tibble(Variable = names(kmo_result$MSAi),KMO_Value = kmo_result$MSAi)

#Grafico de Sedimentacion
scree(correlaciones,main ="Grafico de Sedimentacion")
fa.parallel(correlaciones,fa="pc")

#Componentes
componentes<-prcomp(correlaciones, scale=TRUE,center = TRUE)
componentes
summary(componentes)
componentes_principales<-componentes$x
componentes_principales<-componentes_principales[,1]

# Obtener la varianza explicada
varianza_explicada <- summary(componentes)$importance[3,]

pc1_contrib <- componentes$rotation[,1] ^ 2
pc1_varianza <- tibble( Variable = names(pc1_contrib),
                        Varianza_PC1 = pc1_contrib)

print(kmo_table)
print(pc1_varianza)






