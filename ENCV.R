setwd("C:/Users/e0306829/Downloads/OneDrive_2024-06-14/DANE ECV 2023_CM")
rm(list=ls())

library(foreign)
library(readxl)
library(tidyverse)
library(writexl)

DatosViviendas <- read.spss("Datos de la vivienda/Datos de la vivienda.sav", to.data.frame=TRUE)

Municipios <- read_xls("CodMpio.xls") |> 
  select(Departamento=NOMBRE_DEPTO,CodMpio=CODIGO_MUNICIPIO,Municipio=Nombre)

fill_merged <- function(dat, column){ #Este código es para separar las celdas combinadas en la columna "Departamento"
  for(n in 1:nrow(dat)){
    if(dat[[column]][n] == '' || is.na(dat[[column]][n])){
      dat[[column]][n] <- dat[[column]][n - 1]
    }
  }
  return(dat)
}

Municipios <- fill_merged(Municipios, "Departamento") |> na.exclude()

DatosViviendas <- DatosViviendas |> select(CoDep=P1_DEPARTAMENTO,CoMun=P1_MUNICIPIO,Sector=CLASE,
                         Acueducto=P8520S5,FactorExpansion=FEX_C) |> 
  unite("CodMpio",c(CoDep,CoMun), sep = "") |> 
  group_by(CodMpio,Sector,Acueducto) |> 
  summarise(Hogares = sum(FactorExpansion)) |> 
  mutate(Sector = case_when(Sector == 1 ~ "Cabecera",
                        TRUE ~ "Centros Poblados"),
         Acueducto = case_when(Acueducto == 1 ~ "Si",
                            TRUE ~ "No"))

Resultados <- DatosViviendas |> left_join(Municipios,by = "CodMpio") |> 
  relocate(Municipio, .after = CodMpio) |> relocate(Departamento, .before = CodMpio)

writexl::write_xlsx(Resultados,"Resultados.xlsx")


