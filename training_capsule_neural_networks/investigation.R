library(readr)
library(ggplot2)

# distributions of elapsed time, plotted on one graph
one_boxplot_10_epochs <- function(df, attribute_name, architecture){
  p = ggplot(df, aes_string(x="epoch", y=attribute_name))
  p = p + geom_boxplot()
  p = p + ggtitle(sprintf("distribution of %s by epochs, architecture: %s", attribute_name, architecture))
  print(p)
}

one_boxplot_10_epochs_accuracy <- function(df, attribute_name, architecture){
  p = ggplot(df, aes_string(x="epoch", y=attribute_name))
  p = p + geom_boxplot()
  p = p + ggtitle(sprintf("distribution of %s by epochs, architecture: %s", attribute_name, architecture))
  p = p + scale_y_continuous(limits=c(0, 1))
  print(p)
}

ten_density_plots_10_epochs <- function(df, attribute_name, architecture){
  for (ep in unique(df$epoch)){
    titl = sprintf("distribution of %s, epoch: %d, architecture: %s", attribute_name, ep, architecture)
    data = df[df$epoch == ep,]
    p = ggplot(data, aes_string(y=attribute_name)) + geom_density() + ggtitle(titl)
    print(p)
  }
}

main <- function(){
  architecture = "smaller"
  target_file = sprintf("./%s/%s_subset.csv", architecture, architecture)
  #attribute_name = "hours_elapsed"
  attribute_name = "hours_elapsed"
  df = read_csv(target_file)
  df$epoch <- as.factor(df$epoch)
  one_boxplot_10_epochs(df, attribute_name, architecture)
}

if (sys.nframe() == 0) {
  main()
}