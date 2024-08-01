rm(list = ls())
setwd("C:/Users/e0306829/OneDrive - Ecopetrol S.A/Escritorio")

library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)
library(writexl)

CostoVPU <- read_excel("Edwin/Datos.xlsm", sheet = "Costo Real Energía VUP",
                       range = "A1:CT143")

CantidadVPU <- read_excel("Edwin/Datos.xlsm", sheet = "Cant Real Energía VUP",
                          range = "A1:CT143")

CostoVPU <- CostoVPU %>%
  mutate(across(8:dim(CostoVPU)[2], ~ ifelse(.x < 0, 0, .x)))
CantidadVPU <- CantidadVPU %>%
  mutate(across(8:dim(CantidadVPU)[2], ~ ifelse(.x < 0, 0, .x)))

GRB <- read_excel("Edwin/Datos.xlsm", sheet = "Driver Energía GRB",
                  range = "A27:CH50")

GRC <- read_excel("Edwin/Datos.xlsm", sheet = "Driver Energía RCSA",
                  range = "A29:CH52")

Essentia <- read_excel("Edwin/Datos.xlsm", sheet = "Driver Energía Essentia",
                  range = "A27:CH50")

Ecodiesel <- read_excel("Edwin/Datos.xlsm", sheet = "Driver Energía Ecodiesel",
                       range = "A14:CH25")

MID_ODL <- read_excel("Edwin/Datos.xlsm", sheet = "MID-ODL",
                      range = "A2:CH16")

MID_BIC <- read_excel("Edwin/Datos.xlsm", sheet = "MID-BIC",
                      range = "A2:CH16")

MID_OCENSA <- read_excel("Edwin/Datos.xlsm", sheet = "MID-OCENSA",
                         range = "A2:BV16")

MID_ODC <- read_excel("Edwin/Datos.xlsm", sheet = "MID-ODC",
                      range = "A2:BV21")

MID_CENIT <- read_excel("Edwin/Datos.xlsm", sheet = "REAL MID CENIT",
                        range = "A1:L16785")

CostoVPU <- CostoVPU |> 
  select(- c(`DRIVER ENERGÍA`, TIPO, starts_with("TOTAL"))) |> 
  rename(Centro = GERENCIA, Energetico = ENERGÉTICO ) |> 
  mutate(across(c(`NOMBRE CECO`), substr, 9, nchar(`NOMBRE CECO`))) |> 
  mutate(Energetico = case_when(
    Energetico %in% c("Solar", "Geotermia") ~ "Renovables",
    Energetico %in% c("GLP", "Crudo") ~ "Combustoleos",
    Energetico %in% c("Gas") ~ "Gas",
    Energetico %in% c("Compra") ~ "Red + T&D",
    is.na(Energetico) ~ NA,
    TRUE ~ "Red + T&D"), 
    Centro = case_when(VICEPRESIDENCIA %in% c("VRO") ~ "VRO",
                       VICEPRESIDENCIA %in% c("VPI") ~ "VPI",
                       VICEPRESIDENCIA %in% c("VRC") ~ "VRC",
                       TRUE ~ Centro)) |> 
  select(-VICEPRESIDENCIA) |>
  pivot_longer(-c(CECO, `NOMBRE CECO`, Energetico, Centro),
               names_to = c("Mes", "Año"), names_sep = "-", values_to = "Costo") 
CostoVPU <- CostoVPU |> mutate(Año = as.double(paste0(20,Año)),
                               Mes = as.double(rep_len(1:12, length.out=dim(CostoVPU)[1])))

CantidadVPU <- CantidadVPU |> 
  select(- c(`DRIVER ENERGÍA`, TIPO, starts_with("TOTAL"))) |>
  rename(Centro = GERENCIA, Energetico = ENERGÉTICO) |> 
  mutate(across(c(`NOMBRE CECO`), substr, 9, nchar(`NOMBRE CECO`))) |> 
  mutate(Energetico = case_when(
    Energetico %in% c("Solar", "Geotermia") ~ "Renovables",
    Energetico %in% c("GLP", "Crudo") ~ "Combustoleos",
    Energetico %in% c("Gas") ~ "Gas",
    Energetico %in% c("Compra") ~ "Red + T&D",
    is.na(Energetico) ~ NA,
    TRUE ~ "Red + T&D"), 
    Centro = case_when(VICEPRESIDENCIA %in% c("VRO") ~ "VRO",
                       VICEPRESIDENCIA %in% c("VPI") ~ "VPI",
                       VICEPRESIDENCIA %in% c("VRC") ~ "VRC",
                       TRUE ~ Centro)) |> 
  select(-VICEPRESIDENCIA) |> 
  pivot_longer(-c(CECO, `NOMBRE CECO`, Energetico, Centro),
               names_to = c("Mes", "Año"), names_sep = "-", values_to = "Cantidad")
CantidadVPU <- CantidadVPU |> mutate(Año = as.double(paste0(20,Año)),
                                     Mes = as.double(rep_len(1:12, length.out=dim(CostoVPU)[1])))

Energia_VUP <- cbind(CostoVPU, CantidadVPU[, "Cantidad"]) |> 
  select(Centro, Energetico, Año, Costo, Cantidad)

Energia_VUP <- Energia_VUP |> group_by(Centro, Energetico, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE))

GRB <- GRB |> rename(Energia = `COSTO GRB REAL`) |> 
  filter(Energia %in% c("COSTO TOTAL", "Energía kWh")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |>
  mutate(Energia = case_when(Energia == "COSTO TOTAL" ~ "Costo",
                             Energia == "Energía kWh" ~ "Cantidad"),
         Centro = "GRB") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
  mutate(Energetico = "Gas") |> 
  relocate(Energetico, .before = Año) |> relocate(Centro, .before = Energetico) |> 
  group_by(Centro, Energetico, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE))

GRC <- GRC |> rename(Energia = `PRESUPUESTO GRC REAL`) |> 
  filter(Energia %in% c("COSTO TOTAL", "Energía kWh")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |>
  mutate(Energia = case_when(Energia == "COSTO TOTAL" ~ "Costo",
                             Energia == "Energía kWh" ~ "Cantidad"),
         Centro = "GRC") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
  mutate(Energetico = "Gas") |> 
  relocate(Energetico, .before = Año) |> relocate(Centro, .before = Energetico) |> 
  group_by(Centro, Energetico, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE))

Energia <- rbind(Energia_VUP, GRB, GRC)
Energia <- Energia |> group_by(Centro, Energetico, Año) |> 
  mutate(CostoXkWh = ifelse(Cantidad == 0, NA, Costo / Cantidad)) |> 
  as.data.frame()

Tabla <- function(variable, año){
  Energia |>  
    filter(Año == año) |> 
    select(Centro, Energetico, variable) |> 
    pivot_wider(names_from = Energetico, values_from = variable)
}

costo2019 <- Tabla("Costo", 2019) |> mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) |> 
  adorn_totals("row")
costo2023 <- Tabla("Costo", 2023)|> mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) |> 
  adorn_totals("row")
cantidad2019 <- Tabla("Cantidad", 2019)|> mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) |> 
  adorn_totals("row")
cantidad2023 <- Tabla("Cantidad", 2023)|> mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) |> 
  adorn_totals("row")
precio2019 <- Tabla("CostoXkWh", 2019) 
precio2023 <- Tabla("CostoXkWh", 2023)

write_xlsx(list("Costo 2019" = costo2019, "Energia 2019" = cantidad2019, "Precio 2019" = precio2019,
                "Costo 2023" = costo2023, "Energia 2023" = cantidad2023, "Precio 2023" = precio2023),
           "aa.xlsx", col_names = TRUE, format_headers = TRUE)

Essentia <- Essentia |> rename(Energia = `COSTO Esenttia REAL`) |> 
  filter(Energia %in% c("COMPRA DE ENERGÍA", "AUTOGENERACIÓN FOTOVOLTÁICA", "AUTOGENERACIÓN GAS",
                        "Energía kWh - Compra", "Energía kWh - Fotovoltáica", "Energía kWh - Gas")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |> 
  mutate(Energia = case_when(
    Energia %in% c("COMPRA DE ENERGÍA") ~ "Red + T&D-Costo",
    Energia %in% c("AUTOGENERACIÓN FOTOVOLTÁICA") ~ "Renovables-Costo",
    Energia %in% c("AUTOGENERACIÓN GAS") ~ "Gas-Costo",
    Energia %in% c("Energía kWh - Compra") ~ "Red + T&D-Cantidad",
    Energia %in% c("Energía kWh - Fotovoltáica") ~ "Renovables-Cantidad",
    Energia %in% c("Energía kWh - Gas") ~ "Gas-Cantidad")) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> 
  pivot_longer(-c(Mes, Año),
               names_to = c("Energetico", "Tipo"), names_sep = "-", values_to = "Valor") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Tipo, values_from = Valor) |> 
  mutate(Centro = "OTROS") |> select(-Mes) |> 
  group_by(Centro, Año, Energetico) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE))
# |> 
#   filter(Año==2019)
  
  
  
# |>
#   pivot_wider(names_from = Energia, values_from = Valor) |> 
#   pivot_longer(c("COMPRA DE ENERGÍA", "AUTOGENERACIÓN FOTOVOLTÁICA", "AUTOGENERACIÓN GAS"),
#                names_to = "Energetico", values_to = "Costo")  |> 
#   pivot_longer(c("Energía kWh - Compra", "Energía kWh - Fotovoltáica", "Energía kWh - Gas"),
#                names_to = "Energetico2", values_to = "Cantidad") |> 
#   group_by(Mes, Año, Energetico)
#   
#   mutate(Energia = case_when(Energia == "COSTO TOTAL" ~ "Costo",
#                              Energia == "Energía kWh - Total" ~ "Cantidad"),
#          Centro = "OTROS") |> 
#   mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
#   pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
#   relocate(Centro, .before = Año) |> 
#   group_by(Centro, Año) |> 
#   summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE))

Ecodiesel <- Ecodiesel |> rename(Energia = `COSTO Ecodiesel REAL`) |> 
  filter(Energia %in% c("COSTO TOTAL", "Energía kWh - PRODUCTOR MARGINAL")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |>
  mutate(Energia = case_when(Energia == "COSTO TOTAL" ~ "Costo",
                             Energia == "Energía kWh - PRODUCTOR MARGINAL" ~ "Cantidad"),
         Centro = "OTROS") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
  relocate(Centro, .before = Año) |> 
  group_by(Centro, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE)) |> 
  mutate(Energetico = "Otros") |> relocate(Energetico, .before = Costo)

MID_ODL <- MID_ODL |> rename(Energia = REAL) |> 
  filter(Energia %in% c("Costo Total Energía", "Cantidad Total Energía")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |>
  mutate(Energia = case_when(Energia == "Costo Total Energía" ~ "Costo",
                             Energia == "Cantidad Total Energía" ~ "Cantidad"),
         Centro = "OTROS") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
  relocate(Centro, .before = Año) |> 
  group_by(Centro, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE)) |> 
  mutate(Energetico = "Red + T&D") |> relocate(Energetico, .before = Costo)

MID_BIC <- MID_BIC |> rename(Energia = REAL) |> 
  filter(Energia %in% c("Costo Total Energía", "Cantidad Total Energía")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |>
  mutate(Energia = case_when(Energia == "Costo Total Energía" ~ "Costo",
                             Energia == "Cantidad Total Energía" ~ "Cantidad"),
         Centro = "OTROS") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
  relocate(Centro, .before = Año) |> 
  group_by(Centro, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE)) |> 
  mutate(Energetico = "Red + T&D") |> relocate(Energetico, .before = Costo)

MID_OCENSA <- MID_OCENSA |> rename(Energia = REAL) |> 
  filter(Energia %in% c("Costo Total Energía", "Cantidad Total Energía (KWh)")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |>
  mutate(Energia = case_when(Energia == "Costo Total Energía" ~ "Costo",
                             Energia == "Cantidad Total Energía (KWh)" ~ "Cantidad"),
         Centro = "OTROS") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
  relocate(Centro, .before = Año) |> 
  group_by(Centro, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE)) |> 
  mutate(Energetico = "Red + T&D") |> relocate(Energetico, .before = Costo)

MID_ODC <- MID_ODC |> rename(Energia = REAL) |> 
  filter(Energia %in% c("Costo Total Energía", "Cantidad Total Energía")) |> 
  select(-starts_with("Total")) |> 
  pivot_longer(-c(Energia), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha))) |> 
  mutate(Mes = month(Fecha), Año = year(Fecha)) |> select(-Fecha) |>
  mutate(Energia = case_when(Energia == "Costo Total Energía" ~ "Costo",
                             Energia == "Cantidad Total Energía" ~ "Cantidad"),
         Centro = "OTROS") |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-Mes) |> 
  relocate(Centro, .before = Año) |> 
  group_by(Centro, Año) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE)) |> 
  mutate(Energetico = "Red + T&D") |> relocate(Energetico, .before = Costo)

MID_CENIT <- MID_CENIT |> select(Tipo, Subcategoría, Mes, Real) |>
  rename(Energia = Tipo, Energetico = Subcategoría, Valor = Real) |> 
  mutate(Energia = replace(Energia, Energia == "cantidad", "Cantidad")) |> 
  filter(Energia %in% c("Costo", "Cantidad")) |> 
  mutate(Energetico = case_when(
    Energetico %in% c("ENERGÍA SOLAR", "PCH") ~ "Renovables",
    .default = "Red + T&D")) |> 
  mutate(Año = year(Mes), Mes = month(Mes), Centro = "OTROS") |> select(-Mes) |> 
  mutate(Valor = ifelse(Valor < 0, 0, Valor)) |> 
  mutate(row = row_number()) |>
  pivot_wider(names_from = Energia, values_from = Valor) |> select(-row) |> 
  relocate(Centro, .before = Año) |> relocate(Costo, .before = Cantidad) |> relocate(Energetico, .after = Año) |> 
  group_by(Centro, Año, Energetico) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE)) 

otros <- rbind(Essentia, Ecodiesel, MID_ODL, MID_BIC, MID_ODC, MID_OCENSA, MID_CENIT) |> 
  select(-Centro) |> group_by(Año, Energetico) |> 
  summarise(Costo = sum(Costo, na.rm = TRUE), Cantidad = sum(Cantidad, na.rm = TRUE)) |> 
  filter(Año %in% c(2019, 2023)) |> 
  mutate(CostoXkWh = ifelse(Cantidad == 0, NA, Costo / Cantidad))
  mutate(Costo = round(Costo,2), Cantidad = round(Cantidad,2))
otros |> View()

costo2019 <- Tabla("Costo", 2019) |> mutate(Otros = NA)
costo2019

