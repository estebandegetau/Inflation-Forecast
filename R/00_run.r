#### Inflation Forecast #######################################################
#' 
#' @name 00_run.R
#' 
#' @description Run the entire inflation forecast process.
#' 
#' @author Esteban Degetau
#' 
#' @created 2024-04-02
#' 
#### Run ######################################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse)

#---- Set up -----------------------------------------------------------------

.run_01_gather_data      <- 1
.run_02_forecast_survey  <- 1
.run_03_model_univariate <- 1
.run_04_yearly_survey    <- 1

#---- Run --------------------------------------------------------------------

if(.run_01_gather_data) {
  source(here("R/01_gather_data.R"), encoding = "UTF-8")
}

if(.run_02_forecast_survey) {
  source(here("R/02_forecast_survey.R"), encoding = "UTF-8")
}

if(.run_03_model_univariate) {
  source(here("R/03_model_univariate.R"), encoding = "UTF-8")
}

if(.run_04_yearly_survey) {
  source(here("R/04_yearly_survey.R"), encoding = "UTF-8")
}