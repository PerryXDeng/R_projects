#!/usr/bin/Rscript
source("./2/utility.R")
source("./2/holt.R")
source("./2/visualization.R")

holt_on_location <- function(mat, location_index, attribute_name, typ="additive", damp=FALSE, a=0.2, b=0.1057){
  smoothed_vector = unlist(mat[seq(1,1 + STEPS_SMOOTHED), location_index])
  forecasted_vector = unlist(mat[seq(2 + STEPS_SMOOTHED, 1 + STEPS_SMOOTHED + STEPS_PREDICTED), location_index])
  
  model = Holt(smoothed_vector, type=typ, damped=damp, alpha=a, beta=b, lead=STEPS_PREDICTED, plot=FALSE)
  smoothed_mape = model$accurate["MAPE"]
  smoothed_mpe = model$accurate["MPE"]
  forecast_metrics = accurate(forecasted_vector, model$pred, 2, output=FALSE)
  forecasted_mape = forecast_metrics["MAPE"]
  forecasted_mpe = forecast_metrics["MPE"]
  p = forecast_scatterplot(mat[seq(1, NUM_STEPS), location_index], model$estimate, model$pred, attribute_name, location)
  plot(p)
}

main <- function(){
  df = load_combined_areas_data()
  mat = transposed_time_series_matrix(df, "POPESTIMATE")
  holt_on_location(mat, 1532, "POPESTIMATE")
}

if (sys.nframe() == 0) {
  main()
}
