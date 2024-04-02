#### Inflation Forecast ########################################################
#'
#' @title 01_gather_data.R
#'
#' @description Gather data from several sources to create the inflation
#'              forecast.
#'
#' @author Esteban Degetau
#'
#' @created 2024-03-13
#'
#### Gather data ###############################################################

rm(list = ls())
gc()

updated_data <- lubridate::now()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(
  here,
  tidyverse,
  siebanxicor,
  gt,
  httr,
  jsonlite,
  rjson,
  rvest,
  labelled,
  readxl,
  gtrendsR
)




#---- Banxico ------------------------------------------------------------------

setToken("574b5dedd17af5b0866d78637c2213ec3a6d8272babe9e41964f6b75ecb13903")

banxico_ids <- c(
    "SF29652",  # Base monetaria (Stock, Millones MXN)
    "SP1",     # INPC (Índice)
    "SP6",     # INPP (Excluye petróleo, Índice)
    "SP74625", # INPC subyacente (Índice)
    "SP30603", # Precios importaciones (variación anual, %)
    "SL11298", # Salario Mínimo General (MXN por día)
    "SG1",     # Government expenditure (Acumulado, Millones MXN)
    "SG193",   # Government debt (Stock, Miles de Millones MXN)
    "SF283",   # Tasa de interés interbancaria 28 días (% anual)
    # "SF17801", # Tasa de interés interbancaria 91 días (% anual)
    "SF17906", # Tipo de cambio (Pesos por dólar, FIX)
    # "SR16734", # IGAE (Índice)
    "SL1"      # Tasa de desocupación (Porcentaje)
)

banxico_metadata <- getSeriesMetadata(banxico_ids)



today <- today() |> as.character()

banxico <- getSeriesData(
    series = banxico_ids,
    "1990-01-01",
    today
)


banxico_tb <- tibble(
    serie = names(banxico),
    fecha = map(banxico, 1),
    valor = map(banxico, 2)
)



banxico_clean <- banxico_tb |>
    unnest(!serie) |>
    arrange(serie, desc(fecha)) |>
    # Replace serie name with descriptions above
        mutate(
        serie = case_when(
            serie == "SF29652" ~ "base_monetaria",
            serie == "SP30603" ~ "precios_importaciones",
            serie == "SG1" ~ "gasto_publico",
            serie == "SG193" ~ "deuda_publica",
            serie == "SF283" ~ "tiie_28",
            serie == "SF17801" ~ "tiie_91",
            serie == "SF221962" ~ "tiie_182",
            serie == "SF17906" ~ "tipo_cambio",
            serie == "SP1" ~ "infl_gen",
            serie == "SP74625" ~ "infl_sub",
            serie == "SL11439" ~ "wage_industry",
            serie == "SP6" ~ "inpp",
            serie == "SL11298" ~ "min_wage",
            serie == "SR16734" ~ "igae",
            serie == "SL1" ~ "tasa_desocupacion",
            T ~ serie
        ),
        valor = case_when(
          serie == "inpp" ~ ((valor / lead(valor, n = 12)) - 1) * 100,
          T ~ valor
        ),
        valor = case_when(
            serie == "tasa_desocupacion" & fecha >= ymd(20050101) ~ NA,
            serie == "inpp" & fecha >= ymd(20100101) ~ NA,
            T ~ valor
        ),
        fuente = "Banco de México"
    ) |>
    drop_na()

inpp_base <- banxico_clean |>
  filter(serie == "inpp", fecha == ymd(20031101)) |>
  pull(valor)




banxico_clean |>
drop_na() |>
group_by(serie) |>
summarise(
    min = min(fecha),
    max = max(fecha))

#---- IMSS ---------------------------------------------------------------------

jobs <- read_csv(here("data/empleo.csv"), skip = 2, 
                 col_names = c("fecha", "valor"),
                 locale = locale("es"),
                 col_types = "cn"
                 ) |>
  mutate(
    serie = "empleo",
    fuente = "IMSS"
  )

jobs_l <- nrow(jobs)

jobs <- jobs |>
  mutate(
    fecha = seq(ym(199707), ym(199707) + months(jobs_l - 1), by = "1 month")
  )


wages <- read_csv(here("data/salario.csv"), skip = 2, 
                  col_names = c("fecha", "valor"),
                  col_types = "cn",
                  locale = locale("es")) |>
  mutate(
    serie = "salario",
    fuente = "IMSS"
  )

wages_l <- nrow(wages)

wages <- wages |>
  mutate(
    fecha = seq(ym(199707), ym(199707) + months(wages_l - 1), by = "1 month")
  ) 

imss <- bind_rows(jobs, wages)

#---- INEGI --------------------------------------------------------------------




get_inegi_url <- function(id) {
    token <- "ac23a0f8-1207-f404-3f62-9d669769b298"

    str_c(
        "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/",
        id,
        "/es/0700/false/BIE/2.0/",
        token,
        "?type=json"
    )

}

# Usage example:
url <- get_inegi_url("737121")

gather_data <- function(id) {
    url <- get_inegi_url(id)


    respuesta <- GET(url)
    datosGenerales <- content(respuesta, "text")
    flujoDatos <- paste(datosGenerales, collapse = " ")

    flujoDatos <- fromJSON(flujoDatos)

    flujoDatos <- flujoDatos$Series

    flujoDatos <- flujoDatos[[1]]$OBSERVATIONS


    inegi <- tibble(
        fecha = map_chr(flujoDatos, "TIME_PERIOD"),
        valor = map_chr(flujoDatos, "OBS_VALUE")
    ) |>
        mutate(
            fecha = ym(fecha),
            valor = as.numeric(valor)
        )

    return(inegi)
}


inegi <- tibble(
    id = c(
        "454168", # Confianza del consumidor (Índice)
        # "214302", # Asegurados trabajadores permanentes
        "737121", # IGAE (Índice volumen físico)
        "673095",  # Precios al productor
        "740946", # Consumo interno (Índice)
        # "628194", # Inflación general,
        # "628216", # Inflación subyacente
        # "701400", # Confianza empresarial
        "444603", # Tasa desocupación
        "444602" # Tasa participación
        # "702103" # Remuneraciones medias reales
    )
) |>
mutate(
    data = map(id, gather_data)
)


inegi_open <- inegi |>
  unnest(!id) |>
  mutate(
    serie = case_when(
      id == "454168" ~ "conf_cons",
      id == "214302" ~ "aseg_trab_perm",
      id == "737121" ~ "igae",
      id == "673095" ~ "inpp",
      id == "740946" ~ "cons_interno",
      id == "628194" ~ "infl_gen",
      id == "628216" ~ "infl_sub",
      id == "701400" ~ "conf_empresarial",
      id == "444603" ~ "tasa_desocupacion",
      id == "444602" ~ "tasa_participacion",
      id == "214318" ~ "tiie",
      id == "702103" ~ "remuneraciones_reales",
      T ~ id
    ),
    fuente = "INEGI",
    valor = case_when(
      serie == "inpp" ~ ((valor / lead(valor, n = 12)) - 1) * 100,
      T ~ valor
    ),
    valor = case_when(
      serie == "inpp" & fecha < ymd(20100101) ~ NA,
      T ~ valor
    )
  ) |>
  drop_na()

inegi_open |>
  group_by(serie) |>
  summarise(
    min = min(fecha),
    max = max(fecha)) 

banxico_clean |>
  group_by(serie) |>
  summarise(
    min = min(fecha),
    max = max(fecha)) 

#---- Google Trends ------------------------------------------------------------
# 
# gtrends(
#   key = c("trabajo", "inflacion"),
#   geo = "MX",
#   gprop = "web",
#   time = "all",
#   hl = "es"
#   )

#---- Merge data ---------------------------------------------------------------


# inputs_long <- bind_rows(
#     banxico_clean,
#     inegi_open
# ) |>
#     arrange(serie, fecha) 



# stop()

inputs_long <- bind_rows(banxico_clean, imss, inegi_open) |>
  arrange(desc(fecha))

inputs_long |>
group_by(serie) |>
summarise(
    min = min(fecha),
    max = max(fecha)) 

inputs_long %>%
  dplyr::group_by(fecha, serie) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

inputs <- inputs_long |>
    arrange(serie) |>
    pivot_wider(
        id_cols = "fecha",
        names_from = "serie",
        values_from = "valor"
    ) |>
    arrange((fecha)) |>
    # Make all variables percentage change
    mutate(
        infl_mensual = ((infl_gen / lag(infl_gen, n = 1)) - 1) * 100,
        across(!matches("fecha|desocup|tiie|conf|particip|cons_int|importaciones|inpp|infl_mens"),
        ~ ((.x / lag(.x, n = 12)) - 1) * 100
        )
    ) |>
    # Add labels
    set_variable_labels(
        fecha = "Fecha",
        base_monetaria = "Base monetaria",
        precios_importaciones = "Precios importaciones",
        gasto_publico = "Gasto público",
        deuda_publica = "Deuda pública",
        tiie_28 = "TIIE 28 días (%)",
        # tiie_91 = "TIIE 91 días (%)",
        # tiie_182 = "TIIE 182 días (%)",
        tipo_cambio = "Tipo de cambio",
        tasa_desocupacion = "Tasa de desocupación (%)",
        tasa_participacion = "Tasa de participación (%)",
        conf_cons = "Confianza del consumidor (Índice)",
        empleo = "Empleo formal",
        salario = "Salario promedio",
        igae = "IGAE",
        cons_interno = "Consumo interno",
        infl_gen = "INPC",
        infl_sub = "Precios subyacentes",
        infl_mensual = "Inflación mensual",
        # conf_empresarial = "Confianza empresarial (Índice)",
        min_wage = "Salario mínimo general",
        inpp = "INPP"


    ) |>
    filter(year(fecha) >= 1994)

labels <- inputs |>
    generate_dictionary() |>
    as_tibble() |>
    select(
        serie = variable,
        descripcion = label
    )

metadata <- inputs_long |>
    group_by(serie, fuente) |>
    summarise(
        min = min(fecha),
        max = max(fecha)
    ) |>
    arrange(serie) |>
    left_join(labels)


save(inputs, updated_data, metadata, file = here("data", "inputs.RData"))


