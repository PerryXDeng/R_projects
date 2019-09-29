library(pastecs)

df = read.csv("./gpa2.csv")

stats = stat.desc(df[,c("colgpa", "sat")], basic=TRUE)
select_stats = stats[c("mean", "median", "std.dev", "min", "max", "nbr.na"),]

agg = aggregate(colgpa~athlete, data=df, mean)

dum_lm = lm(colgpa~athlete, data=df)

samp_sizes = table(df$athlete)

multivariate_lm = lm(colgpa~athlete+sat, data=df)

errors = resid(multivariate_lm)
mean_error = mean(errors)
error_sat_covariance = cov(errors, df$sat)

cat("Select Statistics on College GPA and SAT Scores\n")
print(select_stats)
cat("\n\n\nSample Mean College GPA, Athletes and Non-Athletes\n")
print(agg)
cat("\n\n\nLinear Regression of College GPA (y) and Athelete Status (x)\n")
print(summary(dum_lm))
cat("\n\n\nSample Size of Non Athletes and Athletes\n")
cat(samp_sizes)
cat("\n\n\nLinear Regression of College GPA (y), Athelete Status (x1), SAT (x2)\n")
print(summary(multivariate_lm))
cat("Mean Residual\n")
cat(mean_error)
cat("\nSample Covariance, Linear Regression Residual, SAT\n")
cat(error_sat_covariance)
