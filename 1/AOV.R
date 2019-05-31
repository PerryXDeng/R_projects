library(readr)
# plotmeans
library("gplots")
# for plotting correlation
#library(corrplot, quietly=TRUE)
# grouping
library(dplyr)

DATA_DIR = "./1/datasets/"
FILEPATH = paste(DATA_DIR, "AppleStore.csv", sep="")
FILTERED_CLT_TOTAL_FILEPATH = paste(DATA_DIR, "FilteredByTotalRatingsCLT.csv", sep="")
FILTERED_CLT_VERSION_FILEPATH = paste(DATA_DIR, "FilteredByVersionRatingsCLT.csv", sep="")
CLT_THRESHOLD = 30

load_data <- function() {
  # opens and returns the R dataframe for dataset
  # Summary of the columns in AappleStore.csv
  # "id" : App ID
  # "track_name": App Name
  # "size_bytes": Size (in Bytes)
  # "currency": Currency Type
  # "price": Price amount
  # "rating_count_tot": User Rating counts (for all version)
  # "rating_count_ver": User Rating counts (for current version)
  # "user_rating" : Average User Rating value (for all version)
  # "user_rating_ver": Average User Rating value (for current version)
  # "ver" : Latest version code
  # "cont_rating": Content Rating
  # "prime_genre": Primary Genre
  # "sup_devices.num": Number of supporting devices
  # "ipadSc_urls.num": Number of screenshots showed for display
  # "lang.num": Number of supported languages
  # "vpp_lic": Vpp Device Based Licensing Enabled
  # read_csv is faster than read.csv
  return(read_csv(FILEPATH))
}

filter_by_total_ratings <- function(dataframe) {
  # returns a filtered dataset that satisfies the CLT rule of thumb
  return(subset(dataframe, rating_count_tot >= CLT_THRESHOLD, names(dataframe)))
}

filter_by_version_ratings <- function(dataframe) {
  # returns a filtered dataset that satisfies the CLT rule of thumb
  return(subset(dataframe, rating_count_ver >= CLT_THRESHOLD, names(dataframe)))
}

save_filtered_datasets <- function(){
  # saves the filtered datasets so we can play with them in rattle
  appstore <- load_data()
  filtered_tot <- filter_by_total_ratings(appstore)
  filtered_ver <- filter_by_version_ratings(appstore)
  write.csv(filtered_tot, file=FILTERED_CLT_TOTAL_FILEPATH)
  write.csv(filtered_ver, file=FILTERED_CLT_VERSION_FILEPATH)
}

sam_exploration <- function() {
  # Sam's initial exploration
  # visualization of data and analysis of variance
  applestore <- load_data()
  filtered_tot <- filter_by_total_ratings(applestore)
  # app_correlation <- cor(applestore, use="pairwise", method="pearson")
  # the %>% is a piping operator
  group_by(filtered_tot, prime_genre)%>%
    summarise(
      count = n(),
      mean = mean(user_rating, na.rm = TRUE),
      sd = sd(user_rating, na.rm = TRUE)
    )
  # Visualize the data
  boxplot(user_rating ~ prime_genre, data = filtered_tot,
          xlab = "Genre", ylab = "Rating",
          frame = FALSE)
  plotmeans(user_rating ~ prime_genre, data = filtered_tot, frame = FALSE,
            xlab = "Genre", ylab = "Rating",) 
  
  #ANOVA = AOV
  variance <- aov(user_rating ~ prime_genre, data = filtered_tot)
  summary(variance)
  #Homogeneity of data
  plot(variance, 1)
  #Normality of data
  plot(variance, 2)
}

main <- function(variables) {
  #save_filtered_datasets()
}

main()
