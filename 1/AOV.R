library(readr)
library(ggplot2)
# plotmeans
library("gplots")
# for plotting correlation
#library(corrplot, quietly=TRUE)
# grouping
library(dplyr)
library(magrittr)

#HW_DIR = "./csci620_big_data_homework/1/"
HW_DIR = "./1/"
DATA_DIR = paste(HW_DIR, "datasets/", sep="")
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
  return(summarise(group_by(df, prime_genre),
                   count = n(), 
                   mean = mean(user_rating, na.rm = TRUE), 
                   sd = sd(user_rating, na.rm = TRUE),
                   shapiro_p = shapiro.test(user_rating)[2])) # p value from normality test
}

save_genre_summary <- function(df){
  sdats = summary_by_genre(df)
  write_csv(sdats, paste(HW_DIR, "genre_stats.csv", sep=""))
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

plot_overlapping_ratings_density <- function(df, bandwidth="nrd0"){
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

plot_ratings_distribution_histograms_by_groups <- function(df){
  subpopulations <- dplyr::select(dplyr::mutate(df, prime_genre=as.factor(prime_genre)), user_rating, prime_genre)
  sdats <- dplyr::mutate(summary_by_genre(df), prime_genre)
  for (i in 1:nrow(sdats)){
    category <- sdats[i, 1]
    n <- sdats[i, 2]
    m <- sdats[i, 3]
    sd <- sdats[i, 4]
    p <- sdats[i, 5]
    subpopulation <- filter(subpopulations, prime_genre %in% category)
    title <- sprintf("Distribution of Average User Ratings, %s, n = %s, m = %.2f, sd = %.2f, p < 0.01: %s", category, n, m, sd, p < 0.01)
    p <- ggplot2::ggplot(subpopulation, ggplot2::aes(x=user_rating)) +
         ggplot2::geom_histogram(ggplot2::aes(x=user_rating)) + 
         ggplot2::ggtitle(title) +
         ggplot2::scale_x_continuous(limits=c(1,5))
    plot(p)
  }
}

drew_fun_with_languages <- function() {
  applestore <- load_data()
  p1 <- ggplot(applestore, aes(y = user_rating, x = lang.num)) + 
    geom_point(shape=1) + 
    geom_smooth(method=lm) +
    ggtitle("User Rating vs # of Languages")
  plot(p1)
  print(paste("User Rating vs # of Languages Correlation: ", cor(applestore$user_rating, applestore$lang.num)))
  
  p2 <- ggplot(applestore, aes(y = rating_count_tot, x = lang.num)) + 
    geom_point(shape=1) + 
    geom_smooth(method=lm) +
    ggtitle("Rating Count vs # of Languages")
  plot(p2)
  print(paste("Rating Count vs # of Languages Correlation: ", cor(applestore$rating_count_tot, applestore$lang.num)))

  p3 <- ggplot(applestore, aes(y = user_rating_ver, x = lang.num)) +
    ggtitle("User Rating this Version vs # of Languages") + 
    geom_point(shape=1) + 
    geom_smooth(method=lm) 
  plot(p3)
  print(paste("User Rating this Version vs # of Languages Correlation: ", cor(applestore$user_rating_ver, applestore$lang.num)))
  
  p4 <- ggplot(applestore, aes(y = rating_count_ver, x = lang.num)) + 
    geom_point(shape=1) + 
    geom_smooth(method=lm) +
    ggtitle("Rating Count this Version vs # of Languages")
  plot(p4)
  print(paste("Rating Count this Version vs # of Languages Correlation: ", cor(applestore$rating_count_ver, applestore$lang.num)))
  
  
  applestore_grouped <- group_by(applestore, prime_genre)
  
  s <- dplyr::summarise(applestore_grouped, cor(user_rating, lang.num))
  View(s)
  write.table(s, paste(HW_DIR, "user_rating_lang_num.tsv"), sep="\t")
  s <- dplyr::summarise(applestore_grouped, cor(rating_count_tot, lang.num))
  View(s)
  write.table(s, paste(HW_DIR, "rating_count_tot_lang_num.tsv"), sep="\t")
  s <- dplyr::summarise(applestore_grouped, cor(user_rating_ver, lang.num))
  View(s)
  write.table(s, paste(HW_DIR, "user_rating_ver_lang_num.tsv"), sep="\t")
  s <- dplyr::summarise(applestore_grouped, cor(rating_count_ver, lang.num))
  View(s)
  write.table(s, paste(HW_DIR, "rating_count_ver_lang_num.tsv"), sep="\t")
}

main <- function(variables) {
  # sam_exploration()
  # save_filtered_datasets()
  
  dataframe <- read_csv(FILTERED_CLT_TOTAL_FILEPATH)
  # ruskal.test(user_rating ~ prime_genre, data = dataframe)
  plot_ratings_distribution_histograms_by_groups(dataframe)
  # plot_overlapping_ratings_density(dataframe)
  # plot_overlapping_ratings_density(dataframe, bandwidth=0.25)
  # save_genre_summary(dataframe)
  
  # drew_fun_with_languages()
  
  
}

main()
