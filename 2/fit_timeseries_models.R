source("./2/utility.R")
source("./2/holt.R")

STEPS_PREDICTED = 3
STEPS_SMOOTHED = END_YEAR - START_YEAR - STEPS_PREDICTED

holt_on_location <- function(mat, location_index, typ="additive", damp=FALSE, a=0.2, b=0.1057){
  smoothed_vector = unlist(mat[seq(1:1 + STEPS_SMOOTHED), 1532])
  forecasted_vector = unlist(mat[seq(2 + STEPS_SMOOTHED:2 + STEPS_SMOOTHED + STEPS_PREDICTED), 1532])
  
  model = Holt(smoothed_vector, type=typ, damped=damp, alpha=a, beta=b, lead=STEPS_PREDICTED)
  smoothed_mape = model$accurate["MAPE"]
  smoothed_mpe = model$accurate["MPE"]
  forecast_metrics = accurate(forecasted_vector, model$pred, 2, output=FALSE)
  forecasted_mape = forecast_metrics["MAPE"]
  forecasted_mpe = forecast_metrics["MPE"]
}

df = load_combined_areas_data()
mat = transposed_time_series_matrix(df, "POPESTIMATE")
holt_on_location(mat, 1532)
