df = read.csv("./wage2b.csv")

naive_lm = lm(wage~tenure+urban+exper+educ+age, data=df)
urban_lm = lm(wage~tenure+urban+exper+educ+age+I(educ*urban), data=df)

cat("\n\n\nLinear Regression of Wage (y), Tenure, Urban Status, Years of Experience, Years of Education, Age\n")
print(summary(naive_lm))

cat("\n\n\nLinear Regression of Wage (y), Tenure, Urban Status, Years of Experience, Years of Education (Factored by Urban Status), Age\n")
print(summary(urban_lm))
