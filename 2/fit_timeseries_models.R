#!/usr/bin/Rscript
# for CPU parallelism
#library(foreach)
#library(doParallel)
#library(arm)
#registerDoMC(4)

source("./2/utility.R")
source("./2/holt.R")
source("./2/visualization.R")

holt_on_location <- function(mat, location_index, location_name, attribute_name, typ="additive", damp=FALSE, a=0.2, b=0.1057){
  # WARNING: multiplicative version is broken  on forecasts
  smoothed_vector = unlist(mat[seq(1,1 + STEPS_SMOOTHED), location_index])
  forecasted_vector = unlist(mat[seq(2 + STEPS_SMOOTHED, 1 + STEPS_SMOOTHED + STEPS_PREDICTED), location_index])
  
  model = Holt(smoothed_vector, type=typ, damped=damp, alpha=a, beta=b, lead=STEPS_PREDICTED, plot=FALSE)
  smoothed_mape = model$accurate["MAPE"]
  smoothed_mpe = model$accurate["MPE"]
  forecast_metrics = accurate(forecasted_vector, model$pred, 2, output=FALSE)
  forecasted_mape = forecast_metrics["MAPE"]
  forecasted_mpe = forecast_metrics["MPE"]
  # for titling
  if (typ == "additive"){
    typ = "Additive"
  }else{
    typ = "Multiplicative"
  }
  titl = sprintf("Holt's %s on %s's %s, α=%.2f, β=%.2f, damped=%s", typ, location_name, attribute_name, a, b, damp)
  p = forecast_scatterplot(mat[seq(1, NUM_STEPS), location_index], model$estimate, model$pred, attribute_name, titl)
  return(list(c(smoothed_mape, smoothed_mpe, forecasted_mape, forecasted_mpe), p))
}

holt_on_all_locations <- function(mat, loc_names, attribute_name, typ="additive", damp=FALSE, a=0.2, b=0.1057){
  metrics = as.data.frame(matrix(ncol=4, nrow=ncol(mat)))
  names(metrics) = c("Smoothed MAPE", "Smoothed MPE", "Forecast MAPE", "Forecast MPE")
  plots = vector("list", ncol(mat))
  for (location_index in seq(1, ncol(mat))){
  #for (location_index in seq(1, 5)){
  #foreach(location_index=seq(1, 2)) %dopar% {
    loc_name = loc_names[location_index]
    result = holt_on_location(mat, location_index, loc_name, attribute_name, typ, damp, a, b)
    metrics[location_index, 1:4] = unlist(result[1])
    plots[[location_index]] = result[2] # double bracket because plots are lists, so needed to be assigned like whole columns
  }
  # for titling
  if (typ == "additive"){
    typ = "Additive"
  }else{
    typ = "Multiplicative"
  }
  title = sprintf("Holt's %s on %s,α=%.2f,β=%.2f,damped=%s,AMAPE=%.2f,AMPE=%.2f", typ, attribute_name, a, b, damp, mean(metrics[,3], na.rm=TRUE), mean(metrics[,4], na.rm=TRUE))
  box_plot = ggplot(stack(metrics), aes(x = factor(ind, levels = names(metrics)), y = values)) +
    geom_boxplot() +
    ggtitle(title)
  return(list("metrics" = metrics, "plots" = plots, "boxplot" = box_plot))
}

main <- function(){
  df = load_combined_areas_data()
  attribute_name = "POPESTIMATE"
  mat = transposed_time_series_matrix(df, attribute_name)
  out = holt_on_all_locations(mat, get_location_names(df), attribute_name, a=0.9, b=0.9)
  metrics = out$metrics
  plot(out$boxplot)
  
  plots = out$plots # can be plotted using print() function
  for (rand in sample(1:ncol(mat), 10, replace=F)){
    # randomly plots a few of them
    print(plots[rand])
  }
  print(metrics)
}

if (sys.nframe() == 0) {
  main()
}
