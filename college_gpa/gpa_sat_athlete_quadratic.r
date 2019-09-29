df = read.csv("./gpa2.csv")

quadratic_lm = lm(colgpa~sat+I(sat^2)+athlete, data=df)

tp_sat = coefficients(quadratic_lm)["sat"]/(-2*coefficients(quadratic_lm)["I(sat^2)"])
proportion_sample_greater_than_tp = sum(df$sat>tp_sat)/length(df$sat)
mean_sat = mean(df$sat)
mean_sat_gradient = coefficients(quadratic_lm)["sat"] + 2*coefficients(quadratic_lm)["I(sat^2)"]*mean_sat

cat("\n\n\nQuadratic Regression of College GPA (y), SAT, Athelete Status\n")
print(summary(quadratic_lm))
cat("SAT Relationship Turning Point\n")
cat(tp_sat)
cat("\nProportion of Sample with SAT Greater than Turning Point\n")
cat(proportion_sample_greater_than_tp)
cat("\nMean SAT\n")
cat(mean_sat)
cat("\nQuadratic GPA Gradient of SAT at Mean SAT\n")
cat(mean_sat_gradient)
