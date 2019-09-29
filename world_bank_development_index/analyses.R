library(readr)
HW_DIR = "./3/"
DATA_DIR = paste(HW_DIR, "WDI_csv/", sep="")
FILEPATH = paste(DATA_DIR, "WDIData.csv", sep="")
FILTERED_FILEPATH = paste(HW_DIR, "PrimaryEnrollment2016.csv", sep="")
MAP_FILEPATH = paste(HW_DIR, "map.png", sep="")
BARS_FILEPATH = paste(HW_DIR, "bars.png", sep="")

filter_data <- function(){
  df = read_csv(FILEPATH)
  df = df[c("Country Code", "Indicator Name", "2016")]
  filtered = subset(df, df["Indicator Name"] == "Children out of school (% of primary school age)")
  filtered = filtered[c("Country Code", "2016")]
  write_csv(filtered, FILTERED_FILEPATH)
}

if (!require("rworldmap")) {
  install.packages("rworldmap")
  library(rworldmap)
}
# ggplot2, ggFlags, dplyr are needed for the bar charts
library(ggplot2)
library(dplyr)
if (!require("ggflags")) {
  devtools::install_github("rensa/ggflags")
  library(ggflags)
}

# csv file with each person as a row and containing a column with the header Origin and
# countries in 3-letter ISO format (change joinCode for other formats)
df1 <- read_csv(FILTERED_FILEPATH)
matched <- joinCountryData2Map(df1, joinCode="ISO3", nameJoinColumn="Country Code")
# make png of the map
png(file = MAP_FILEPATH,
    width = 1024, height = 768)
par(mai=c(0,0,0.2,0))
mapCountryData(matched,
               nameColumnToPlot="2016",
               mapTitle= "Percentage of Primary School Aged Children Not Attending School (Missing Data from White Colored Countries)",
               catMethod = "pretty",
               colourPalette = "heat",
               oceanCol="lightblue",
               missingCountryCol="white",
               addLegend = TRUE,
               lwd = 1)
dev.off()