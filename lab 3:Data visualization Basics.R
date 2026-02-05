my_people <- data.frame(
  name    = c("Anu", "Ram", "Shyam", "Hari", "Davis"),
  age     = c(28, 34, 25, 30, 27),
  gender  = c("male", "male", "male", "female", "female"),
  commute = c(30, 45, 20, 35, 25)
)

# Look at the data
head(my_people)
str(my_people)
summary(my_people)

#Basic scatter plot
plot(
  my_people$age,
  my_people$commute,
  xlab = "Age",
  ylab = "Commute (minutes)",
  main = "Age vs Commute"
)


#Correlation
cor.test(my_people$age, my_people$commute)

#Boxplot
boxplot(
  commute ~ gender,
  data = my_people,
  xlab = "Gender",
  ylab = "Commute (minutes)",
  main = "Commute by Gender"
)

#Simple regression
model <- lm(commute ~ age, data = my_people)
summary(model)
plot(my_people$age, my_people$commute)
abline(model, col = "red", lwd = 2)

#T-test
t.test(my_people$age, my_people$commute)

#Anova
my_people$gender <- factor(my_people$gender)

anova_res <- aov(commute ~ gender, data = my_people)
summary(anova_res)

