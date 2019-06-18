setwd("C:/Users/themo/Documents/csci620_big_data_homework")
source("./2/utility.R")
#install.packages("plotly")
library(plotly)
library(dplyr)
library(textclean)
library(stringi)

## Set up fake df_pop_county data frame
## df_pop_county <- data.frame(region=county.fips$fips)
## df_pop_county$value <- county.fips$fips
## y <- df_pop_county$value
## df_pop_county$color <- gray(y / max(y))

## merge population data with county.fips to make sure color column is
## ordered correctly.
## counties <- county.fips %>% left_join(df_pop_county, by=c('fips'='region'))
## map("county", fill=TRUE, col=counties$color)

create_heatmap <- function(combined_data){
  county_data <- subset(combined_data, LSAD == "County or equivalent")
  county_data$subregion <- stri_trans_tolower(county_data$NAME)
  county_data$subregion %<>%
  gsub(",.*", "", .) %>%
  gsub(" county", "", .) %>%
  gsub(" parish", "", .) %>%
  gsub(" ", "", .) %>%
  gsub("[.]", "", .)
  
  county_df <- map_data("county")
  state_df <- map_data("state")
  
  choropleth <- inner_join(county_df, county_data, by = "subregion")
  choropleth <- choropleth[!duplicated(choropleth$order), ]

  plot_cols = list("INTERNATIONALMIG2014", "INTERNATIONALMIG2015", "INTERNATIONALMIG2016", "INTERNATIONALMIG2017", "INTERNATIONALMIG2018")
  
  for(plot_col in plot_cols) {
    print(paste("PLOT_COL:", plot_col))
    print_heatmap(choropleth, state_df, plot_col)
  }
  
  county_data
}

print_heatmap <- function(choropleth, state_df, col_string) {
  p <- ggplot(choropleth, aes(long, lat, group = group)) +
    geom_polygon(aes_string(fill = col_string), 
                 colour = alpha("black", 1/2), size = 0.1)  +
    geom_polygon(data = state_df, colour = "black", fill = NA)  +
    theme_void()
  p <- ggplotly(p, tooltip = 'text') %>% 
    layout(
      hovermode = 'x',
      margin = list(
        t = 20,
        b = 20,
        l = 20,
        r = 20),
      legend = list(
        orientation = 'h',
        x = 0.5,
        y = 1.01,
        xanchor = 'center'))
  
  # use style to modify layer
  p <- style(p, hoverinfo = 'none', traces = c(3))
  
  # use plotly_build to modify layer
  p <- plotly_build(p)
  str(p$x$layout$annotations) # check annotations
  p$x$layout$annotations = NULL # remove annotation
  print(p)
}

main <- function(){
  combined_data = load_combined_areas_data()
  combined_data
  create_heatmap(combined_data)
}


main()