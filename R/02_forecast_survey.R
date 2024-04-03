#### Inflation forecast ########################################################
#' 
#' @name 02_forecast_survey.R
#' 
#' @description Pulls the survey answers for the analysts' inflation forecast.
#' 
#' @author Esteban Degetau
#' 
#' @created 2024-03-25
#' 
#### Forecast survey ###########################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(
    tidyverse,
    here,
    siebanxicor,
    labelled
)

#---- Pull data ----------------------------------------------------------------

setToken("574b5dedd17af5b0866d78637c2213ec3a6d8272babe9e41964f6b75ecb13903")

t <- today() |> floor_date("month") 
t <- t - months(1)

months <- tibble(
  month = seq(t, t + months(12), by = "months"),
  stat = list(c("Media", 
                "Mediana", 
                "Primer cuartil", 
                "Tercer cuartil",
                "Mínimo",
                "Máximo",
                "Desviación estándar",
                "Número de respuestas"))
  ) |>
  unnest(stat) |>
  mutate(
    id = c(
      # Month t
      "SR14222",
      "SR14223",
      "SR14224",
      "SR14225",
      "SR14226",
      "SR14227",
      "SR14228",
      "SR16088",
      # Month t + 1
      "SR14229",
      "SR14230",
      "SR14231",
      "SR14232",
      "SR14233",
      "SR14234",
      "SR14235",
      "SR16089",
      # Month t + 2
      "SR14236",
      "SR14237",
      "SR14238",
      "SR14239",
      "SR14240",
      "SR14241",
      "SR14242",
      "SR16090",
      # Month t + 3
      "SR14243",
      "SR14244",
      "SR14245",
      "SR14246",
      "SR14247",
      "SR14248",
      "SR14249",
      "SR16091",
      # Month t + 4
      "SR14250",
      "SR14251",
      "SR14252",
      "SR14253",
      "SR14254",
      "SR14255",
      "SR14256",
      "SR16092",
      # Month t + 5
      "SR14257",
      "SR14258",
      "SR14259",
      "SR14260",
      "SR14261",
      "SR14262",
      "SR14263",
      "SR16093",
      # Month t + 6
      "SR14264",
      "SR14265",
      "SR14266",
      "SR14267",
      "SR14268",
      "SR14269",
      "SR14270",
      "SR16094",
      # Month t + 7
      "SR14271",
      "SR14272",
      "SR14273",
      "SR14274",
      "SR14275",
      "SR14276",
      "SR14277",
      "SR16095",
      # Month t + 8
      "SR14278",
      "SR14279",
      "SR14280",
      "SR14281",
      "SR14282",
      "SR14283",
      "SR14284",
      "SR16096",
      # Month t + 9
      "SR14285",
      "SR14286",
      "SR14287",
      "SR14288",
      "SR14289",
      "SR14290",
      "SR14291",
      "SR16097",
      # Month t + 10
      "SR14292",
      "SR14293",
      "SR14294",
      "SR14295",
      "SR14296",
      "SR14297",
      "SR14298",
      "SR16098",
      # Month t + 11
      "SR14299",
      "SR14300",
      "SR14301",
      "SR14302",
      "SR14303",
      "SR14304",
      "SR14305",
      "SR16099",
      # Month t + 12
      "SR14306",
      "SR14307",
      "SR14308",
      "SR14309",
      "SR14310",
      "SR14311",
      "SR14312",
      "SR16100"
      
      
    )
  ) |>
  mutate(
   data_raw = map(id, getSeriesData),
   data_f = map2(data_raw, id, getSerieDataFrame)
  )

#---- Clean data ----------------------------------------------------------------

current_month <- today() |> floor_date("month")

t_0 <- current_month - months(1)

fcast_svy <- months |>
  select(month, stat, data_f) |>
  unnest(data_f) |>
  group_by(month, stat) |>
  summarise(
    value = value[which.max(date)]
  ) 

survey_history <- months |>
  select(month, stat, data_f) |>
  unnest(data_f) |>
  group_by(stat) |>
  mutate(t = (month - t_0) |> dense_rank() - 1
  ) |>
  ungroup() |>
  select(
    t,
    reference_date = date,
    stat,
    value
  ) |>
  arrange(t, stat, reference_date) 
  

#---- Save data ----------------------------------------------------------------

save(fcast_svy, file = here("data", "fcast_svy.RData"))

save(survey_history, file = here("data", "survey_history.RData"))

