library(readr)
# plotmeans
library("gplots")
# for plotting correlation
#library(corrplot, quietly=TRUE)
# grouping
library(dplyr)
library(magrittr)

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
  write_csv(filtered_tot, FILTERED_CLT_TOTAL_FILEPATH)
  write_csv(filtered_ver, FILTERED_CLT_VERSION_FILEPATH)
}

summary_by_genre <- function(df){
  # gets the size, mean, and standard deviation of each group
  return(summarise(group_by(df, prime_genre), count = n(), 
                   mean = mean(user_rating, na.rm = TRUE), sd = sd(user_rating, na.rm = TRUE)))
}

sam_exploration <- function() {
  # Sam's initial exploration
  # visualization of data and analysis of variance
  applestore <- load_data()
  filtered_tot <- filter_by_total_ratings(applestore)
  # app_correlation <- cor(applestore, use="pairwise", method="pearson")
  summary_by_genre(filtered_tot)
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

plot_overlapping_density <- function(df, bandwidth="nrd0"){
  # Use ggplot2 to generate density for user_rating, grouped by genres
  # nrd0 is the default bandwidth selector - see Silverman (1986, page 48, eqn (3.31))
  # Generate the plot.
  if (bandwidth == "nrd0"){
    title <- paste("Distribution of Average User Ratings by Primary Genres,", "Default Bandwidth (Silverman 1986)")
  }else{
    title <- paste("Distribution of Average User Ratings by Primary Genres, Bandwidth =", bandwidth)
  }
  p01 <-
    dplyr::mutate(df, prime_genre=as.factor(prime_genre)) %>%
    dplyr::select(user_rating, prime_genre) %>%
    # rattle generated code, cant customize legend and x axis names
    ggplot2::ggplot(ggplot2::aes(x=user_rating)) +
    # dashed line for the overall population
    ggplot2::geom_density(lty=3, bw=bandwidth) +
    # different subpopulations
    ggplot2::geom_density(ggplot2::aes(fill=prime_genre, colour=prime_genre), alpha=0.2, bw=bandwidth) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(fill="prime_genre", y="Density")
  # Display the plots.
  plot(p01)
}

main <- function(variables) {
  # sam_exploration()
  # save_filtered_datasets()
  
  dataframe <- read_csv(FILTERED_CLT_TOTAL_FILEPATH)
  View(summary_by_genre(dataframe))
  # plot_overlapping_density(dataframe)
  # plot_overlapping_density(dataframe, bandwidth=0.25)
}

main()
