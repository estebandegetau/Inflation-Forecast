#### Inflation Forecast ########################################################
#' 
#' @name 05_export_forecast.R
#' 
#' @description Export the inflation forecast to a csv file.
#' 
#' @author Esteban Degetau
#' 
#' @created 2024-04-02
#' 
#### Export forecast ############################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here)

#---- Load data ---------------------------------------------------------------

load(here("data/models.RData"))

predict_months <- timeline |>
    filter(str_detect(kind, "Predict")) |>
    pull(fecha)

#---- Wrangle ----------------------------------------------------------------

intervals <- models |>
    filter(name != "NNETAR") |>
    mutate(fecha = list(predict_months)) |>
    select(name, full_forecast, fecha) |>
    mutate(full_forecast = map(full_forecast, as_tibble)) |>
    unnest(c(full_forecast, fecha)) |>
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
    mutate(fecha = list(predict_months)) |>
    select(name, full_forecast, fecha) |>
    mutate(full_forecast = map(full_forecast, "mean")) |>
    unnest(c(full_forecast, fecha)) |>
    select(
        fecha, 
        NNETAR = full_forecast
    )

forecasts <- intervals |>
    left_join(point, by = "fecha") 


#---- Write ------------------------------------------------------------------

write_csv(forecasts, here("data/forecasts.csv"))
