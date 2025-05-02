#### Forecast inflation ########################################################
#' 
#' @name 03_model_univariate.R
#' 
#' @description
#' Model monthly inflation using several univariate methods.
#' 
#' @author Esteban Degetau
#' 
#' @created 2024-03-25
#' 
#### Model #####################################################################

rm(list = ls())
gc()

set.seed(20240401)

#---- Libraries ----------------------------------------------------------------

pacman::p_load(
    here,
    tidyverse,
    labelled,
    forecast
)

#---- Load data ----------------------------------------------------------------

load(here::here("data/inputs.RData"))

inputs <- inputs |>
  drop_na(infl_mensual)

last_obs <- inputs |>
  select(fecha, infl_mensual) |>
  drop_na() |> 
  pull(fecha) |>
  max()

#---- Predict months -----------------------------------------------------------


next_year <- ceiling_date(today() + years(1), "year") - months(1)

predict_months <- seq(last_obs + months(1), next_year, by = "month")

h <- length(predict_months)

max_train <- last_obs - months(h)

train_months <- inputs$fecha |> 
  as_date() |> 
  keep(~ . <= max_train)

test_months <- inputs$fecha |> 
  as_date() |> 
  keep(~ . > max_train)

#---- Data ---------------------------------------------------------------------

train <- inputs |>
  filter(fecha %in% train_months)

test <- inputs |>
  filter(fecha %in% test_months)

m_ts <- ts(train$infl_mensual, start = 1994, frequency = 12)

#---- Univariate models --------------------------------------------------------

arima <- auto.arima(m_ts)

ets <- ets(m_ts)

lm <- tslm(m_ts ~ trend + season)

nnetar <- nnetar(m_ts)

eval_models <- tibble(
  name = c("ARIMA", "ETS", "LM", "NNETAR"),
  model = list(arima, ets, lm, nnetar),
  forecast = map(model, forecast, h = h),
  inside_accuracy = map(forecast, accuracy)
)



#---- Test ---------------------------------------------------------------------

forecast <- eval_models$forecast[[1]]

test_model <- function(forecast) {
  
  pred <- forecast$mean
  test <- test$infl_mensual
  
  error <- pred - test
  
  mae <- mean(abs(error))
  rmse <- sqrt(mean(error^2))
  me <- mean(error)
  mpe <- 100 * mean(error / test)
  
  tibble(
    ME = me,
    RMSE = rmse,
    MAE = mae,
    MPE = mpe
  )
  
  
}


models_test <- eval_models |>
  mutate(test = map(forecast, test_model))

#---- Full models --------------------------------------------------------------

m_ts <- ts(inputs$infl_mensual, start = 1994, frequency = 12)

arima <- auto.arima(m_ts)

ets <- ets(m_ts)

lm <- tslm(m_ts ~ trend + season)

nnetar <- nnetar(m_ts, repeats = 100)


models <- models_test |>
  mutate(
    full_model = list(arima, ets, lm, nnetar),
    full_forecast = map(full_model, ~forecast(.x, h = h)),
  )

#---- Save ---------------------------------------------------------------------

all_dates <- c(train_months, test_months, predict_months) |>
  unique() 

timeline <- tibble(
  fecha = all_dates,
  train = fecha %in% train_months,
  test = fecha %in% test_months,
  predict = fecha %in% predict_months
) |>
arrange(fecha) |>
distinct() 


timeline_check <- timeline |>
  mutate(check = train + test + predict) |>
  filter(check > 1)

if (timeline_check |> nrow() > 0) stop("Check timelines")

timeline <- timeline |>
  mutate(
    kind = case_when(
      train ~ "Train",
      test  ~ "Test",
      predict ~ "Predict Ex-Ante"
    )
  ) |>
  select(fecha, kind)

save(models, timeline, file = here::here("data/models.RData"))

