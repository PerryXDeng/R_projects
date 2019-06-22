#!/usr/bin/Rscript
library(ggplot2)
source("./2/utility.R")


time_series_scatterplots <- function(df, attribute_name, number_of_locations){
  # plots over lapping scatter plots of attribute_name over years
  # each location is a series
  # randomly chooses number_of_locations areas from dataset
  df = df[sample(nrow(df), number_of_locations), ]
  df = time_indexed_dataframe(df, attribute_name)
  p = ggplot(df, aes(x=date, y=attribute_name)) +
      geom_point(aes(color=location)) +
      geom_line(aes(color=location), size=1) +
      ylab(attribute_name) +
      theme_minimal()
  plot(p)
}


forecast_scatterplot <- function(original, smoothed, forecast, attribute_name, titl){
  # return the plot for the vector datapoints
  
  # there should be 2 times # of years of datapoints (one original, one smoothed and forecast)
  df = as.data.frame(matrix(ncol=2, nrow=2*(NUM_STEPS)))
  names(df) = c("series", attribute_name)

  df$date = c(YEAR_SEQUENCE, YEAR_SEQUENCE)
  
  df[1:NUM_STEPS, "series"] = "original"
  df[(NUM_STEPS + 1):(NUM_STEPS + 1 + STEPS_SMOOTHED), "series"] = "smoothed"
  df[(NUM_STEPS + 2 + STEPS_SMOOTHED):(2*NUM_STEPS), "series"] = "forecast"
  
  df[1:NUM_STEPS, attribute_name] = original
  df[(NUM_STEPS + 1):(NUM_STEPS + 1 + STEPS_SMOOTHED), attribute_name] = smoothed
  df[(NUM_STEPS + 2 + STEPS_SMOOTHED):(2*NUM_STEPS), attribute_name] = forecast

  p = ggplot(df, aes_string(x="date", y=attribute_name)) +
    geom_point(aes(color=series)) +
    geom_line(aes(color=series), size=1) +
    ylab(attribute_name) +
    scale_x_date(breaks=YEAR_SEQUENCE) +
    ggtitle(titl) +
    theme_minimal()

  return(p)
}


main <- function(){
  combined_data = load_combined_areas_data()
  time_series_scatterplots(combined_data, "POPESTIMATE", 30)
}


if (sys.nframe() == 0) {
  main()
}
