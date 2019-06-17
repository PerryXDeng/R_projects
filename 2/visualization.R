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


main <- function(){
  combined_data = load_combined_areas_data()
  time_series_scatterplots(combined_data, "POPESTIMATE", 30)
}


main()
