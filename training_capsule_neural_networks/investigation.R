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
    titl = sprintf("distribution of %s, epoch: %s, architecture: %s", attribute_name, ep, architecture)
    data = df[df$epoch == ep,]
    p = ggplot(data, aes_string(x=attribute_name)) + geom_density() + ggtitle(titl)
    print(p)
  }
}

ten_observations <- function(architecture, attribute_name){
  target_file = sprintf("./%s/%s_subset.csv", architecture, architecture)
  df = read_csv(target_file)
  df$epoch <- as.factor(df$epoch)
  ten_density_plots_10_epochs(df, attribute_name, architecture)
}

hundred_observations <- function(attribute_name){
  affine_file = sprintf("./%s/%s_mins.csv", "affine", "affine")
  smaller_file = sprintf("./%s/%s_mins.csv", "smaller", "smaller")
  affine_df = read_csv(affine_file)
  smaller_df = read_csv(smaller_file)
  affine_df$architecture = rep("affine", nrow(affine_df))
  smaller_df$architecture = rep("original", nrow(smaller_df))
  union = rbind(affine_df, smaller_df)
  titl = sprintf("mins of %s over time, two samples of size 10 over 100 epochs", attribute_name)
  p = ggplot(union, aes_string(x="epoch", y=attribute_name)) + geom_line(aes(color=architecture)) + ggtitle(titl)
  print(p)
}

differences_in_mean_observations <- function(){
  file = "./affine_minus_smaller_accuracies.csv"
  df = read_csv(file)
  
  titl = sprintf("differences in mean accuracies over time (affine - original), two samples of size 10 over 100 epochs")
  p = ggplot(df, aes(x=epoch, y=validation_accuracy)) + geom_line() + ggtitle(titl) + scale_y_continuous(limits=c(-0.04, 0.04))
  print(p)
  
  explaination = "sw_p is p value for shapiro wilk test against normality; t_p is p value for right tailed one sample t test against zero mean"
  
  shapiro_p = shapiro.test(df$validation_accuracy)$p.value
  t_p = t.test(df$validation_accuracy, mu=0, alternative="greater")$p.value
  titl = sprintf("distribution of differences in mean accuracies (affine - original), sw_p=%.3f, t_p=%.3f",shapiro_p, t_p)
  p = ggplot(df, aes(x=validation_accuracy)) + geom_density() + ggtitle(titl) + labs(caption=explaination)
  print(p)
  
  df = subset(df, epoch>30)
  shapiro_p = shapiro.test(df$validation_accuracy)$p.value
  t_p = t.test(df$validation_accuracy, mu=0, alternative="greater")$p.value
  titl = sprintf("distribution of differences in mean accuracies (affine - original), epoch>30, sw_p=%.3f, t_p=%.3f",shapiro_p, t_p)
  p = ggplot(df, aes(x=validation_accuracy)) + geom_density() + ggtitle(titl) + labs(caption=explaination)
  print(p)
  
  df = subset(df, epoch>70)
  shapiro_p = shapiro.test(df$validation_accuracy)$p.value
  t_p = t.test(df$validation_accuracy, mu=0, alternative="greater")$p.value
  titl = sprintf("distribution of differences in mean accuracies (affine - original), epoch>70, sw_p=%.3f, t_p=%.3f",shapiro_p, t_p)
  p = ggplot(df, aes(x=validation_accuracy)) + geom_density() + ggtitle(titl) + labs(caption=explaination)
  print(p)
  
  explaination = "sw_p is p value for shapiro wilk test against normality; t_p is p value for right tailed one sample t test against 0.2% mean"
  df = read_csv(file)
  shapiro_p = shapiro.test(df$validation_accuracy)$p.value
  t_p = t.test(df$validation_accuracy, mu=0.002, alternative="greater")$p.value
  titl = sprintf("distribution of differences in mean accuracies (affine - original), sw_p=%.3f, t_p=%.3f",shapiro_p, t_p)
  p = ggplot(df, aes(x=validation_accuracy)) + geom_density() + ggtitle(titl) + labs(caption=explaination)
  print(p)
  
  df = subset(df, epoch>30)
  shapiro_p = shapiro.test(df$validation_accuracy)$p.value
  t_p = t.test(df$validation_accuracy, mu=0.002, alternative="greater")$p.value
  titl = sprintf("distribution of differences in mean accuracies (affine - original), epoch>30, sw_p=%.3f, t_p=%.3f",shapiro_p, t_p)
  p = ggplot(df, aes(x=validation_accuracy)) + geom_density() + ggtitle(titl) + labs(caption=explaination)
  print(p)
  
  df = subset(df, epoch>70)
  shapiro_p = shapiro.test(df$validation_accuracy)$p.value
  t_p = t.test(df$validation_accuracy, mu=0.002, alternative="greater")$p.value
  titl = sprintf("distribution of differences in mean accuracies (affine - original), epoch>70, sw_p=%.3f, t_p=%.3f",shapiro_p, t_p)
  p = ggplot(df, aes(x=validation_accuracy)) + geom_density() + ggtitle(titl) + labs(caption=explaination)
  print(p)
}

main <- function(){
  #attribute_name = "hours_elapsed"
  attribute_name = "validation_accuracy"
  
  #architecture = "smaller"
  #architecture = "affine"
  #ten_observations(architecture, attribute_name)
  
  #hundred_observations(attribute_name)
  differences_in_mean_observations()
}

if (sys.nframe() == 0) {
  main()
}