getwd()
library(readxl)
library(tidyverse)
library(janitor)
library(writexl)

hidro <- read_excel("hidro-termica-renovables.xlsx", sheet = "Hidro")

hidro <- hidro |> pivot_longer(-c(Generacion, Embalse, Serie), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha)))

# write.csv(hidro, "hidro_BD.csv", row.names = FALSE)

hidro1 <- hidro |> group_by(Generacion, Embalse, Fecha) |> 
  summarise(minimo = min(Valor), promedio = mean(Valor), maximo = max(Valor)) 

hidro2 <- hidro |> group_by(Generacion, Embalse, Serie) |> summarise(Total = sum(Valor)) |> 
  arrange(Total, .by_group = TRUE)

# writexl::write_xlsx(hidro1, "hidro1.xlsx")
# writexl::write_xlsx(hidro2, "hidro2.xlsx")

termica <- read_excel("hidro-termica-renovables.xlsx", sheet = "Termica")

termica <- termica |> pivot_longer(-c(Generacion, Termica, Serie), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha)))

# write.csv(termica, "hidro_BD.csv", row.names = FALSE)

termica1 <- termica |> group_by(Generacion, Termica, Fecha) |> 
  summarise(minimo = min(Valor), promedio = mean(Valor), maximo = max(Valor))

termica2 <- termica |> group_by(Generacion, Termica, Serie) |> summarise(Total = sum(Valor)) |> 
  arrange(Total, .by_group = TRUE)

# writexl::write_xlsx(termica1, "termica1.xlsx")
# writexl::write_xlsx(termica2, "termica2.xlsx")


renovables <- read_excel("hidro-termica-renovables.xlsx", sheet = "Renovables")

renovables <- renovables |> pivot_longer(-c(Generacion, Proyecto, ETAPA), names_to = c("Fecha"), values_to = "Valor") |> 
  mutate(Fecha = janitor::excel_numeric_to_date(as.numeric(Fecha)))

# writexl::write_xlsx(renovables, "renovables_BD.xlsx")

renovables2 <- renovables |> group_by(Proyecto) |>
  summarise(`Energía Media` = mean(Valor, na.rm = TRUE),
            AñoDeEntrada = Fecha[which(Valor != 0)[1]]) |> 
  mutate(Tipo = case_when(
    startsWith(as.character(Proyecto), "S_") ~ "Sol",
    startsWith(as.character(Proyecto), "E_") ~ "Viento",
    startsWith(as.character(Proyecto), "SOLARES") ~ "Sol",
    str_detect(Proyecto, "SOLAR") ~ "Sol", 
    str_detect(Proyecto, "EOL") ~ "Viento", 
    TRUE ~ "NA")) |> arrange(Tipo) |> 
  relocate(Proyecto, Tipo, AñoDeEntrada, `Energía Media`) |> 
  arrange(Proyecto)

# writexl::write_xlsx(renovables2, "www.xlsx")


renovables2 <- read_excel("hidro-termica-renovables.xlsx", sheet = "Renovables2")
OEF <- read_excel("hidro-termica-renovables.xlsx", sheet = "OEF")

OEF |>
  left_join(renovables2, by = c("ID", "Proyecto", "Tipo"), keep = NULL) |>
  select(ID, Proyecto, Tipo) |> 
  View()

P1 <- OEF %>% filter(ID %in% renovables2$ID) |> select(Proyecto, Tipo)
P2 <- OEF %>% filter(!ID %in% renovables2$ID) |> select(Proyecto, Tipo)
P3 <- renovables2 %>% filter(ID %in% OEF$ID) |> select(Proyecto, Tipo)
P4 <- renovables2 %>% filter(!ID %in% OEF$ID) |> select(Proyecto, Tipo)

writexl::write_xlsx(P1, "P1.xlsx")
writexl::write_xlsx(P2, "P2.xlsx")
writexl::write_xlsx(P3, "P3.xlsx")
writexl::write_xlsx(P4, "P4.xlsx")


# -------------------------------------------------------------------------

hidro |> group_by(Serie) |> summarise(EnergiaTotal = sum(Valor)) |> arrange(EnergiaTotal) |> head(5) |> writexl::write_xlsx("A1.xlsx")
hidro |> group_by(Serie) |> summarise(EnergiaTotal = sum(Valor)) |> arrange(desc(EnergiaTotal)) |> head(5) |> writexl::write_xlsx("A2.xlsx")

termica |> group_by(Serie) |> summarise(EnergiaTotal = sum(Valor)) |> arrange(EnergiaTotal) |> head(5) |> writexl::write_xlsx("A3.xlsx")
termica |> group_by(Serie) |> summarise(EnergiaTotal = sum(Valor)) |> arrange(desc(EnergiaTotal)) |> head(5) |>  writexl::write_xlsx("A4.xlsx")


hidro3 <- hidro |> group_by(Embalse, Serie) |> summarise(EnergiaTotal = sum(Valor)) |>
  arrange(EnergiaTotal, .by_group = TRUE)

hidro3 <- hidro3 %>% 
  group_by(Embalse) %>%
  summarise(minimo = first(Serie), Mediana = nth(Serie, 50), maximo = last(Serie), EnergiaTotal = sum(EnergiaTotal)) |>
  arrange(EnergiaTotal) |> print(n=100)

writexl::write_xlsx(hidro3, "hidro3.xlsx")

termica3 <- termica |> group_by(Termica, Serie) |> summarise(EnergiaTotal = sum(Valor)) |>
  arrange(EnergiaTotal, .by_group = TRUE)

termica3 <- termica3 %>% 
  group_by(Termica) %>%
  summarise(minimo = first(Serie), Mediana = nth(Serie, 50), maximo = last(Serie), EnergiaTotal = sum(EnergiaTotal)) |>
  arrange(EnergiaTotal) |> print(n=100)

writexl::write_xlsx(termica3, "termica3.xlsx")


hidro3 |> group_by(minimo) |> summarise(conteo = n()) |> arrange(desc(conteo)) |> head() |> writexl::write_xlsx("B1.xlsx")
hidro3 |> group_by(Mediana) |> summarise(conteo = n()) |> arrange(desc(conteo)) |> head() |> writexl::write_xlsx("B2.xlsx")
hidro3 |> group_by(maximo) |> summarise(conteo = n()) |> arrange(desc(conteo)) |> head() |> writexl::write_xlsx("B3.xlsx")

termica3 |> group_by(minimo) |> summarise(conteo = n()) |> arrange(desc(conteo)) |> head() |> writexl::write_xlsx("B4.xlsx")
termica3 |> group_by(Mediana) |> summarise(conteo = n()) |> arrange(desc(conteo)) |> head() |> writexl::write_xlsx("B5.xlsx")
termica3 |> group_by(maximo) |> summarise(conteo = n()) |> arrange(desc(conteo)) |> head() |> writexl::write_xlsx("B6.xlsx")


hidro[1:987700,] |> writexl::write_xlsx("hidroo1.xlsx")
hidro[(987700+1):dim(hidro)[1],] |> writexl::write_xlsx("hidroo2.xlsx")

termica[1:785400,] |> writexl::write_xlsx("termicaa1.xlsx")
termica[(785400+1):dim(termica)[1],] |> writexl::write_xlsx("termicaa2.xlsx")
