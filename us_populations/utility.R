#!/usr/bin/Rscript
library(readr)
library(tidyr)

START_YEAR = 2010
END_YEAR = 2017
YEAR_SEQUENCE = seq(as.Date("2010/7/1"), as.Date("2017/7/1"), "years")

NUM_STEPS = END_YEAR - START_YEAR + 1

# for splitting the time series
STEPS_PREDICTED = 3
STEPS_SMOOTHED = END_YEAR - START_YEAR - STEPS_PREDICTED # first step isn't smoothed

#HW_DIR <- "./"
HW_DIR <- "./2/"
DATA_DIR <- paste(HW_DIR, "datasets/", sep="")


load_data <- function(){
  return(read_csv(paste(DATA_DIR, "metro_and_micro_areas/cbsa-est2018-alldata.csv",sep="")))
}


load_combined_areas_data <- function(){
  return(read_csv(paste(DATA_DIR, "combined_areas/csa-est2018-alldata.csv",sep="")))
}


get_yearly_column_names <- function(attribute_name){
  names <- c()
  for (year in START_YEAR:END_YEAR){
    yearly_name = paste(attribute_name, year, sep="")
    names <- c(names, yearly_name)
  }
  return(names)
}


time_indexed_dataframe <- function(df, attribute_name){
  # new dataframe becomes tuples of (time, location, value)
  # transposes the original dataframe and index observations by year instead of locations
  # easier to scatter plot than original dataframe
  
  yearly_attribute_names = get_yearly_column_names(attribute_name)
  transformed = as.data.frame(t(df[yearly_attribute_names]))
  # column names become location names
  names(transformed) = unlist(df["NAME"])
  # add date column
  transformed = cbind(date = YEAR_SEQUENCE, transformed)
  # turn location into a variable with its own column
  # similar to pandas.dataframe.melt()
  transformed = gather(transformed, key = "location", value = attribute_name, -date)
  return(transformed)
}


transposed_time_series_matrix <- function(df, attribute_name){
  # converts original dataframe into many vectors of same size, forming a matrix
  # each column vector is a timeseries of the attribute of a location
  # returns the matrix
  yearly_attribute_names = get_yearly_column_names(attribute_name)
  transformed = as.data.frame(t(df[yearly_attribute_names]))
  return(transformed)
}


get_location_names <- function(df){
  # turns the locations into a vector of strings
  # use it with the matrix to identify the location name of a time series column
  locations = unlist(df["NAME"])
  return(locations)
}
