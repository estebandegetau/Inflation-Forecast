#### Inflation Forecast ########################################################
#'
#' @name 04_yearly_survey.R
#'
#' @description Get yearly survey data from Banxico SIE
#'
#' @author Esteban Degetau
#'
#' @created 2024-04-02
#'
#### Yearly survey #############################################################

rm(list = ls())
gc()

#---- Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here, siebanxicor, labelled)

#---- Pull data ---------------------------------------------------------------

setToken("574b5dedd17af5b0866d78637c2213ec3a6d8272babe9e41964f6b75ecb13903")

year_t <- today() |> year()

years <- tibble(
    t = seq(0, 3),
    year = year_t + t,
    stat = list(c(
        "Media",
        "Mediana",
        "Primer cuartil",
        "Tercer cuartil",
        "Mínimo",
        "Máximo",
        "Desviación estándar",
        "Número de respuestas"
    ))
) |>
    unnest(stat) |>
    mutate(
        id = c(
            # Year t
            "SR14138",
            "SR14139",
            "SR14140",
            "SR14141",
            "SR14142",
            "SR14143",
            "SR14144",
            "SR16076",
            # Year t + 1
            "SR14145",
            "SR14146",
            "SR14147",
            "SR14148",
            "SR14149",
            "SR14150",
            "SR14151",
            "SR16077",
            # Year t + 2
            "SR14152",
            "SR14153",
            "SR14154",
            "SR14155",
            "SR14156",
            "SR14157",
            "SR14158",
            "SR16078",
            # Year t + 3
            "SR14159",
            "SR14160",
            "SR14161",
            "SR14162",
            "SR14163",
            "SR14164",
            "SR14165",
            "SR16079"
        )
    ) |>
    mutate(
        data_raw = map(id, getSeriesData),
        data_f = map2(data_raw, id, getSerieDataFrame)
    )

years |> glimpse()

#---- Clean data --------------------------------------------------------------

latest_survey_y <- years |>
    select(t, stat, data_f) |>
    unnest(data_f) |>
    summarise(max_date = max(date)) |>
    pull(max_date)

yearly_svey <- years |>
    select(t, stat, data_f) |>
    unnest(data_f) |>
    group_by(t, stat) |>
    summarise(
        value = value[which.max(date)]
    ) |>
    ungroup() |>
    mutate(year = year(latest_survey_y) + t) |>
    select(year, stat, value)

save(yearly_svey, latest_survey_y, file = here::here("data/yearly_survey.RData"))
