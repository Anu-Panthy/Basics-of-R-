#Basic Visualization and Statistics
#Vector+dataframe
my_people <- data.frame(  
  name = c("Anu", "Ram", "shyam", "Hari", "Davis"),  
  age = c(28, 34, 25, 30, 27),  
  gender = c("male", "male", "male", "female", "female"),  
  commute = c(30, 45, 20, 35, 25))
#plot
plot (my_people$age, my_people$commute)
plot(my_people$age,my_people$commute, xlab)
plot(my_people$age, my_people$commute,     
     xlab = "Age",      
     ylab = "Commute",     
     main = "Age vs Commute",     
     pch = ifelse(my_people$name == "Anu", 15, 15),     
     col = ifelse(my_people$name == "Anu", "red", "green"))  

legend("bottomright", c("Anu", "Others"),        
       col = c("red", "green"),        
       pch = c(15, 15))

scatter.smooth(x=mtcars$mpg, y=mtcars$disp, main="Disp ~ mpg")
cor.test(mtcars$mpg, mtcars$disp) 

my_people$male <- ifelse(my_people$gender == "male", 1, 0)

scatter.smooth(x = my_people$male, y = my_people$commute, main = "Commute ~ Male(1)/Female(0)")
plot(commute ~ gender, data = my_people, main="Commute by Gender", xlab="Gender", ylab="Commute")

plot(my_people$age, my_people$commute, xlab = "Age", ylab = "Commute")

cor.test(my_people$age, my_people$commute)

scatter.smooth(
  x = my_people$age,
  y = my_people$commute,
  xlab = "Age",
  ylab = "Commute time",
  main = "Commute vs Age"
)


my_people$male <- ifelse(my_people$gender == "male", 1, 0)

scatter.smooth(
  x = my_people$male,
  y = my_people$commute,
  xlab = "Gender (1 = Male, 0 = Female)",
  ylab = "Commute time",
  main = "Commute vs Gender"
)

scatter.smooth(
  my_people$age[my_people$gender == "male"],
  my_people$commute[my_people$gender == "male"],
  col = "blue",
  pch = 16,
  xlab = "Age",
  ylab = "Commute",
  main = "Commute vs Age by Gender"
)

scatter.smooth(
  my_people$age[my_people$gender == "female"],
  my_people$commute[my_people$gender == "female"],
  col = "red",
  pch = 17,
  add = TRUE
)

legend("topleft",
       legend = c("Male", "Female"),
       col = c("blue", "red"),
       pch = c(16, 17))
cor.test(my_people$age, my_people$commute)

scatter.smooth(
  x = my_people$age,
  y = my_people$commute
)

scatter.smooth(
  x = my_people$age,
  y = my_people$commute,
  lwd = 3,
  col = "blue"
)

scatter.smooth(my_people$age, my_people$commute)
plot(my_people$age, my_people$commute)
abline(lm(commute ~ age, data = my_people), col = "red", lwd = 2)

scatter.smooth(x=mtcars$mpg, y=mtcars$disp, main="Disp ~ mpg")
cor.test(mtcars$mpg, mtcars$disp) 
cor_result <- cor.test(mtcars$mpg, mtcars$disp)
cor_result$estimate
cor_result$p.value 
library(corrplot)
corrplot(cor(mtcars)) # add more options for practice (?corrplot)

#Vector+dataframe-------Ttest
my_people <- data.frame(  
  age = c(28, 34, 25, 30, 27),  
  commute = c(30, 45, 20, 35, 25))

my_people <- data.frame( 
  age = c(28, 34, 25, 30, 27), 
  commute = c(30, 45, 20, 35, 25)
)
t_test_result <-t.test(my_people$age, my_people$commute)
print(t_test_result)
t_test_result$p.value

#Vector+dataframe------ANova
my_people <- data.frame(  
  age = c(28, 34, 25, 30, 27),
  Family_number=c(5,6,7,8,9),
  commute = c(30, 45, 20, 35, 25))
res_aov<-aov(age~commute, data = my_people)
summary(res_aov)

linear_mod <- lm(age ~ commute, data = my_people)
summary(linear_mod)
linear_mod2 <- lm(age ~ commute + Family_number, data = my_people)
summary(linear_mod2) 
data()




















# =========================
# 1) Create the dataframe
# =========================
my_people <- data.frame(
  name    = c("Anu", "Ram", "shyam", "Hari", "Davis"),
  age     = c(28, 34, 25, 30, 27),
  gender  = c("male", "male", "male", "female", "female"),
  commute = c(30, 45, 20, 35, 25)
)

# Make gender a factor (good habit for plots/ANOVA)
my_people$gender <- factor(my_people$gender)

# Optional: numeric coding for gender (male=1, female=0)
my_people$male <- ifelse(my_people$gender == "male", 1, 0)


# =========================
# 2) Basic scatter plot: Age vs Commute
# =========================
is_anu <- my_people$name == "Anu"
pt_col <- ifelse(is_anu, "red", "green")

plot(
  x = my_people$age,
  y = my_people$commute,
  xlab = "Age",
  ylab = "Commute (min)",
  main = "Age vs Commute",
  pch  = 15,
  col  = pt_col
)

legend(
  "bottomright",
  legend = c("Anu", "Others"),
  col    = c("red", "green"),
  pch    = c(15, 15)
)


# =========================
# 3) Scatter + smooth line (Age vs Commute)
# =========================
scatter.smooth(
  x = my_people$age,
  y = my_people$commute,
  xlab = "Age",
  ylab = "Commute (min)",
  main = "Commute vs Age (with smooth line)"
)

# Correlation test (Age vs Commute)
cor_age_commute <- cor.test(my_people$age, my_people$commute)
cor_age_commute


# =========================
# 4) Commute by Gender (boxplot) + smooth (male=0/1)
# =========================
plot(
  commute ~ gender,
  data = my_people,
  main = "Commute by Gender",
  xlab = "Gender",
  ylab = "Commute (min)"
)

scatter.smooth(
  x = my_people$male,
  y = my_people$commute,
  xlab = "Gender (male=1, female=0)",
  ylab = "Commute (min)",
  main = "Commute vs Gender (coded 0/1)"
)


# =========================
# 5) Age vs Commute by Gender (two groups on one plot)
# =========================
plot(
  commute ~ age,
  data = my_people,
  col  = ifelse(my_people$gender == "male", "blue", "red"),
  pch  = ifelse(my_people$gender == "male", 16, 17),
  xlab = "Age",
  ylab = "Commute (min)",
  main = "Commute vs Age by Gender"
)

legend(
  "topleft",
  legend = c("Male", "Female"),
  col    = c("blue", "red"),
  pch    = c(16, 17)
)

# Add regression line (overall)
abline(lm(commute ~ age, data = my_people), col = "black", lwd = 2)


# =========================
# 6) Practice with mtcars (scatter + correlation)
# =========================
scatter.smooth(x = mtcars$mpg, y = mtcars$disp, main = "Disp ~ mpg")
cor_mtcars <- cor.test(mtcars$mpg, mtcars$disp)
cor_mtcars

# Optional correlation matrix plot (requires corrplot)
# install.packages("corrplot")   # run once if needed
library(corrplot)
corrplot(cor(mtcars))


# =========================
# 7) T-test (NOTE: usually used for comparing groups)
# =========================
# This compares mean(age) vs mean(commute) which is different units,
# but it's okay as practice
t_test_result <- t.test(my_people$age, my_people$commute)

t_test_result
t_test_result$p.value


# =========================
# 8) ANOVA + Regression (with Family_number added)
# =========================
my_people2 <- data.frame(
  age           = c(28, 34, 25, 30, 27),
  Family_number = c(5, 6, 7, 8, 9),
  commute       = c(30, 45, 20, 35, 25)
)

# ANOVA (commute is numeric; ANOVA is usually for categorical predictors)
# This will run, but regression is more appropriate here.
res_aov <- aov(age ~ commute, data = my_people2)
summary(res_aov)

# Regression models
linear_mod  <- lm(age ~ commute, data = my_people2)
summary(linear_mod)

linear_mod2 <- lm(age ~ commute + Family_number, data = my_people2)
summary(linear_mod2)


# =========================
# 9) See built-in datasets (optional)
# =========================
data()   # lists built-in datasets
str(my_people)
summary(my_people)
head(my_people)
my_people$gender <- factor(my_people$gender)
my_people$male   <- ifelse(my_people$gender == "male", 1, 0)
plot(age ~ commute, data = my_people)
scatter.smooth(age ~ commute, data = my_people)
boxplot(commute ~ gender, data = my_people)
cor.test(my_people$age, my_people$commute)
lm(age ~ commute, data = my_people)
t.test(commute ~ gender, data = my_people)
plot(linear_mod)
shapiro.test(residuals(linear_mod))
plot(commute ~ age, data = my_people)
abline(lm(commute ~ age, data = my_people))

#Data → Clean → Look → Question → Test → Check → Explain → Show → Conclude


names(df)

df$Tray_Friday <- factor(df$Tray_Friday)

names(df) <- c(
  "Treatment",
  "Harvest_weight_g",
  "DF",
  "Count_1",
  "Count_2",
  "Average_100uL",
  "CFU_90mL",
  "CFU_g",
  "log_CFU_g"
)
df$Treatment <- factor(df$Treatment)


aov_res <- aov(log_CFU_g ~ Treatment, data = df)
summary(aov_res)
TukeyHSD(aov_res)

aov_yield <- aov(Harvest_weight_g ~ Treatment, data = df)
summary(aov_yield)

boxplot(log_CFU_g ~ Treatment,
        data = df,
        ylab = "log CFU/g",
        main = "Salmonella reduction across treatments")

boxplot(Harvest_weight_g ~ Treatment,
        data = df,
        ylab = "Harvest weight (g)",
        main = "Effect of PDI on yield")

names(df)
str(df)
levels(df$Tray_Friday)











############################################################
# PDI Data Analysis Script (Lab Report Q1)
# Author: <your name>
# Purpose: Summarize PDI dataset with basic cleaning, plots,
#          and ANOVA/Tukey tests.
############################################################

# ---------------------------
# 0) Set up
# ---------------------------

# (Optional) clear environment
rm(list = ls())

# Set working directory (edit to your folder)
# setwd("D:/Second Sem R Learning/Second Sem")

# Load needed packages (base R works without these)
# install.packages("dplyr")    # run once if needed
# install.packages("ggplot2")  # run once if needed
library(dplyr)

# ---------------------------
# 1) Load the data
# ---------------------------

# IMPORTANT: Put your file in your working directory OR use full path
df <- read.csv("pdi_data.csv", stringsAsFactors = FALSE)

# Quick look
cat("\n--- Column names ---\n")
print(names(df))

cat("\n--- Data structure ---\n")
str(df)

cat("\n--- First rows ---\n")
print(head(df))

cat("\n--- Summary ---\n")
print(summary(df))


# ---------------------------
# 2) Clean column names
# ---------------------------

# Your dataset has names like "Harvest.Weight..g." and "log.CFU.g"
# Let's rename them to clean names for easier coding.

names(df) <- c(
  "Treatment",         # was Tray_Friday
  "Harvest_weight_g",  # was Harvest.Weight..g.
  "DF",                # was DF
  "Count_1",           # was Count.1
  "Count_2",           # was Count.2
  "Average_100uL",     # was Average..100ul
  "CFU_90mL",          # was CFU.90mL
  "CFU_g",             # was CFU.gm
  "log_CFU_g"          # was log.CFU.g
)

cat("\n--- Cleaned column names ---\n")
print(names(df))

# Convert Treatment to factor (groups)
df$Treatment <- factor(df$Treatment)

cat("\n--- Treatment levels ---\n")
print(levels(df$Treatment))

# ---------------------------
# 3) Check missing values (VERY IMPORTANT)
# ---------------------------

cat("\n--- Missing values per column ---\n")
print(colSums(is.na(df)))

# Check if Harvest weight exists for each treatment
cat("\n--- Harvest weight NA check by Treatment ---\n")
print(with(df, table(Treatment, is.na(Harvest_weight_g))))

# This explains why TSA may not show in the yield plot:
# If all Harvest_weight_g values for TSA are NA, R will not draw TSA boxplot.


# ---------------------------
# 4) Plot 1: Microbial outcome (log CFU/g) by Treatment
# ---------------------------

# Basic boxplot
boxplot(
  log_CFU_g ~ Treatment,
  data = df,
  ylab = "log CFU/g",
  xlab = "Treatment / Media (Tray_Friday)",
  main = "Effect of PDI on Microbial Load (log CFU/g)"
)

# Add individual points for clarity
stripchart(
  log_CFU_g ~ Treatment,
  data = df,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  add = TRUE
)

# Save plot at 300 DPI (recommended for reports)
tiff("Figure1_logCFU_by_Treatment.tiff", width = 6, height = 4, units = "in", res = 300)
boxplot(
  log_CFU_g ~ Treatment,
  data = df,
  ylab = "log CFU/g",
  xlab = "Treatment / Media (Tray_Friday)",
  main = "Effect of PDI on Microbial Load (log CFU/g)"
)
stripchart(
  log_CFU_g ~ Treatment,
  data = df,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  add = TRUE
)
dev.off()


# ---------------------------
# 5) Plot 2: Yield outcome (Harvest weight) by Treatment
# ---------------------------

# Remove rows where harvest weight is missing
df_yield <- df %>% filter(!is.na(Harvest_weight_g))

# If TSA has no harvest data, it won't appear here (correct behavior)
boxplot(
  Harvest_weight_g ~ Treatment,
  data = df_yield,
  ylab = "Harvest weight (g)",
  xlab = "Treatment / Media (Tray_Friday)",
  main = "Effect of PDI on Yield"
)

stripchart(
  Harvest_weight_g ~ Treatment,
  data = df_yield,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  add = TRUE
)

# Save yield plot 300 DPI
tiff("Figure2_Yield_by_Treatment.tiff", width = 6, height = 4, units = "in", res = 300)
boxplot(
  Harvest_weight_g ~ Treatment,
  data = df_yield,
  ylab = "Harvest weight (g)",
  xlab = "Treatment / Media (Tray_Friday)",
  main = "Effect of PDI on Yield"
)
stripchart(
  Harvest_weight_g ~ Treatment,
  data = df_yield,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  add = TRUE
)
dev.off()


# ---------------------------
# 6) ANOVA + Tukey (Microbial: log CFU/g)
# ---------------------------

aov_micro <- aov(log_CFU_g ~ Treatment, data = df)
cat("\n--- ANOVA (log CFU/g ~ Treatment) ---\n")
print(summary(aov_micro))

cat("\n--- Tukey HSD (log CFU/g) ---\n")
print(TukeyHSD(aov_micro))


# ---------------------------
# 7) ANOVA + Tukey (Yield: Harvest weight)
# ---------------------------

# Only run if there are at least 2 treatments with yield data
if (length(unique(df_yield$Treatment)) >= 2) {
  aov_yield <- aov(Harvest_weight_g ~ Treatment, data = df_yield)
  cat("\n--- ANOVA (Yield ~ Treatment) ---\n")
  print(summary(aov_yield))
  
  cat("\n--- Tukey HSD (Yield) ---\n")
  print(TukeyHSD(aov_yield))
} else {
  cat("\nNot enough groups with yield data to run ANOVA for yield.\n")
}


# ---------------------------
# 8) Optional: Quick summary table (means by Treatment)
# ---------------------------

summary_table <- df %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    mean_logCFU = mean(log_CFU_g, na.rm = TRUE),
    sd_logCFU   = sd(log_CFU_g, na.rm = TRUE),
    mean_yield  = mean(Harvest_weight_g, na.rm = TRUE),
    sd_yield    = sd(Harvest_weight_g, na.rm = TRUE)
  )

cat("\n--- Summary table by Treatment ---\n")
print(summary_table)

# Save summary table to CSV for your report
write.csv(summary_table, "SummaryTable_by_Treatment.csv", row.names = FALSE)

cat("\nDONE ✅ Plots + ANOVA + summary table created.\n")
############################################################


































############################################################
# PDI Microgreen Data (from your Excel/CSV) - Full Script
############################################################

rm(list = ls())

# 1) Load data ---------------------------------------------------------------
df_raw <- read.csv("pdi_data.csv", stringsAsFactors = FALSE)

cat("\n--- Original column names ---\n")
print(names(df_raw))
cat("\n--- Rows, Cols ---\n")
print(dim(df_raw))
cat("\n--- Structure ---\n")
str(df_raw)

# 2) Create a clean working copy --------------------------------------------
df <- df_raw

# Rename columns safely (only if those columns exist)
# (This matches the names you showed in R: Tray_Friday, Harvest.Weight..g., log.CFU.g, CFU.gm etc.)
name_map <- c(
  "Tray_Friday"        = "Treatment",
  "Harvest.Weight..g." = "Harvest_weight_g",
  "Count.1"            = "Count_1",
  "Count.2"            = "Count_2",
  "Average..100ul"     = "Average_100uL",
  "CFU.90mL"           = "CFU_90mL",
  "CFU.gm"             = "CFU_g",
  "log.CFU.g"          = "log_CFU_g",
  "DF"                 = "DF"
)

# Apply rename only for columns that exist
existing <- intersect(names(name_map), names(df))
names(df)[match(existing, names(df))] <- name_map[existing]

cat("\n--- Cleaned column names (working df) ---\n")
print(names(df))

# 3) Ensure Treatment is a factor -------------------------------------------
if (!"Treatment" %in% names(df)) stop("Treatment column not found. Check your file/column names.")
df$Treatment <- factor(df$Treatment)

cat("\n--- Treatment levels ---\n")
print(levels(df$Treatment))
cat("\n--- Counts per Treatment ---\n")
print(table(df$Treatment))

# 4) FIX: Make sure Harvest weight is numeric (this often causes TSA to disappear)
# Excel sometimes imports as character because of blanks or symbols.
if ("Harvest_weight_g" %in% names(df)) {
  df$Harvest_weight_g <- as.numeric(trimws(as.character(df$Harvest_weight_g)))
}

# Also make log_CFU numeric if needed
if ("log_CFU_g" %in% names(df)) {
  df$log_CFU_g <- as.numeric(trimws(as.character(df$log_CFU_g)))
}

# 5) PROOF check: TSA yield values exist? -----------------------------------
# This shows you exactly what R sees for TSA in Harvest_weight_g
cat("\n--- TSA Harvest weights (what R sees) ---\n")
print(df[df$Treatment == "TSA", c("Treatment", "Harvest_weight_g")])

cat("\n--- Missing harvest weight by treatment ---\n")
print(with(df, table(Treatment, is.na(Harvest_weight_g))))

# 6) Plot A: Yield by Treatment ---------------------------------------------
# TSA will show ONLY if it has at least 1 non-NA numeric value
df_yield <- df[!is.na(df$Harvest_weight_g), ]

if (nrow(df_yield) > 0) {
  boxplot(Harvest_weight_g ~ Treatment,
          data = df_yield,
          ylab = "Harvest weight (g)",
          xlab = "Treatment (Tray_Friday)",
          main = "Effect of PDI on Yield")
  
  stripchart(Harvest_weight_g ~ Treatment,
             data = df_yield,
             vertical = TRUE, method = "jitter",
             pch = 16, add = TRUE)
  
  # Save 300 DPI
  tiff("Figure_Yield_by_Treatment.tiff", width = 6, height = 4, units = "in", res = 300)
  boxplot(Harvest_weight_g ~ Treatment,
          data = df_yield,
          ylab = "Harvest weight (g)",
          xlab = "Treatment (Tray_Friday)",
          main = "Effect of PDI on Yield")
  stripchart(Harvest_weight_g ~ Treatment,
             data = df_yield,
             vertical = TRUE, method = "jitter",
             pch = 16, add = TRUE)
  dev.off()
} else {
  cat("\nNo non-NA Harvest_weight_g values found, so yield plot cannot be made.\n")
}

# 7) Plot B: log CFU/g by Treatment -----------------------------------------
df_cfu <- df[!is.na(df$log_CFU_g), ]

if (nrow(df_cfu) > 0) {
  boxplot(log_CFU_g ~ Treatment,
          data = df_cfu,
          ylab = "log CFU/g",
          xlab = "Treatment (Tray_Friday)",
          main = "Effect of PDI on Microbial Load (log CFU/g)")
  
  stripchart(log_CFU_g ~ Treatment,
             data = df_cfu,
             vertical = TRUE, method = "jitter",
             pch = 16, add = TRUE)
  
  # Save 300 DPI
  tiff("Figure_logCFU_by_Treatment.tiff", width = 6, height = 4, units = "in", res = 300)
  boxplot(log_CFU_g ~ Treatment,
          data = df_cfu,
          ylab = "log CFU/g",
          xlab = "Treatment (Tray_Friday)",
          main = "Effect of PDI on Microbial Load (log CFU/g)")
  stripchart(log_CFU_g ~ Treatment,
             data = df_cfu,
             vertical = TRUE, method = "jitter",
             pch = 16, add = TRUE)
  dev.off()
} else {
  cat("\nNo non-NA log_CFU_g values found, so log CFU plot cannot be made.\n")
}

# 8) ANOVA + Tukey: log CFU -------------------------------------------------
if (nrow(df_cfu) > 0 && length(unique(df_cfu$Treatment)) >= 2) {
  aov_micro <- aov(log_CFU_g ~ Treatment, data = df_cfu)
  cat("\n--- ANOVA: log_CFU_g ~ Treatment ---\n")
  print(summary(aov_micro))
  
  cat("\n--- TukeyHSD for log_CFU_g ---\n")
  print(TukeyHSD(aov_micro))
} else {
  cat("\nNot enough groups/data to run ANOVA for log_CFU_g.\n")
}

# 9) ANOVA + Tukey: Yield ---------------------------------------------------
if (nrow(df_yield) > 0 && length(unique(df_yield$Treatment)) >= 2) {
  aov_yield <- aov(Harvest_weight_g ~ Treatment, data = df_yield)
  cat("\n--- ANOVA: Harvest_weight_g ~ Treatment ---\n")
  print(summary(aov_yield))
  
  cat("\n--- TukeyHSD for Harvest_weight_g ---\n")
  print(TukeyHSD(aov_yield))
} else {
  cat("\nNot enough groups/data to run ANOVA for yield.\n")
}

# 10) Summary table (nice for your report) ----------------------------------
summary_table <- data.frame(
  Treatment = levels(df$Treatment),
  n_total   = as.integer(table(df$Treatment)[levels(df$Treatment)]),
  mean_yield = tapply(df$Harvest_weight_g, df$Treatment, mean, na.rm = TRUE),
  sd_yield   = tapply(df$Harvest_weight_g, df$Treatment, sd,   na.rm = TRUE),
  mean_logCFU = tapply(df$log_CFU_g, df$Treatment, mean, na.rm = TRUE),
  sd_logCFU   = tapply(df$log_CFU_g, df$Treatment, sd,   na.rm = TRUE)
)

cat("\n--- Summary table ---\n")
print(summary_table)

write.csv(summary_table, "PDI_SummaryTable.csv", row.names = FALSE)

cat("\nDONE ✅ Figures saved + ANOVA done + summary table exported.\n")
############################################################

