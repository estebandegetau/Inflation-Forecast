#### Inflation forecast ########################################################
#' 
#' @name 07_export_survey.R
#' 
#' @description
#' Export ex-ante forecast from Banxico survey.
#' 
#' @author Esteban Degetau
#' 
#' @created 2024-04-03
#' 
#### Export training ###########################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here)

#---- Load data ---------------------------------------------------------------

load(here("data/fcast_svy.RData"))

load(here("data/models.RData"))

predict_months <- timeline |>
    filter(str_detect(kind, "Predict")) |>
    pull(fecha)

#---- Wrangle data ------------------------------------------------------------

fcast_svy |>
    filter(month %in% predict_months) |>
    pivot_wider(
        names_from = "stat",
        values_from = "value",
        id_cols = "month"
    ) |>
    rename(fecha = month) |>
    write_csv(here("data/banxico_survey.csv"))
