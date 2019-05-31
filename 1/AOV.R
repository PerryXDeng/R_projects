library(readr)
# plotmeans
library("gplots")
# for plotting correlation
#library(corrplot, quietly=TRUE)
# grouping
library(dplyr)

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
applestore <- read_csv("AppleStore.csv")

#filtering of the data
filtered_tot <- subset(applestore, rating_count_tot >= 30, names(applestore))
filtered_ver <- subset(applestore, rating_count_ver >= 30, names(applestore))
#View(filtered_tot)
#View(filtered_ver)
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

