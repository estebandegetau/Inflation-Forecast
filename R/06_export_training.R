#### Inflation forecast ########################################################
#' 
#' @name 06_export_training.R
#' 
#' @description
#' Export training predicted values and observed inflation.
#' 
#' @author Esteban Degetau
#' 
#' @created 2024-04-02
#' 
#### Export training ###########################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, here)

#---- Load ---------------------------------------------------------------------

load(here("data/models.RData"))

load(here("data/inputs.RData"))

train_months <- timeline |>
  filter(kind == "Test") |>
  pull(fecha)

#---- Wrangle ------------------------------------------------------------------

intervals <- models |>
  filter(name != "NNETAR") |>
  mutate(fecha = list(train_months)) |>
  select(name, forecast, fecha) |>
  mutate(forecast = map(forecast, as_tibble)) |>
  unnest(c(forecast, fecha)) |>
  pivot_longer(
    cols = -c("name", "fecha"),
    names_to = "estimate"
  ) |>
  
  pivot_wider(
    names_from = c("name", "estimate"),
    names_sep = " ",
    id_cols = "fecha"
  ) 

point <- models |>
  filter(name == "NNETAR") |>
  mutate(fecha = list(train_months)) |>
  select(name, forecast, fecha) |>
  mutate(forecast = map(forecast, "mean")) |>
  unnest(c(forecast, fecha)) |>
  select(
    fecha, 
    NNETAR = forecast
  )

forecasts <- intervals |>
  left_join(point, by = "fecha") 

#---- Inflation ----------------------------------------------------------------

observed <- inputs |>
  filter(fecha %in% train_months) |>
  select(fecha, Observada = infl_mensual)

trained_data <- forecasts |>
  left_join(observed, by = "fecha")

#---- Export -------------------------------------------------------------------

write_csv(trained_data, here("data/tests.csv"))