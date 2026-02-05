#create csv file and load csv file  after confirming working

#Data frame and function
getwd()

#Make Data frame
my_dataframe<-read.csv("pdi_data.csv")
print(my_dataframe)
summary(my_dataframe)
my_df<-data.frame(Tray=c("XLT4","TSA", "DBRC"), Harvest_weight=c(10,12,9))

#View dataframe
my_df$Harvest_weight
my_df[2,]


#activity1 : create your own data frame
my_Homie<-c("Padma", "Kalpana", "Sapana", "Kamala", "Sangita", "Anu", "Safal")
my_Homie<-data.frame(name=my_Homie, age=c(55,50,32,30,26,24,21), gender=c("M","F","F","F","F","F","M"), commute=c(40, 0, 0, 20, 20, 20,20))
mean(my_Homie$age)
mean(my_Homie$commute)
min(my_Homie$age)
max(my_Homie$age)
summary(my_Homie)
head(df)
class(my_Homie)
str(df)
dim(my_Homie)
names(my_Homie)


#Data Transformation
#installation of dplyr package

# -----------------------------------------
# 0) SETUP (install once, then library)
# -----------------------------------------
# install.packages("dplyr")   # run only once if not installed
library(dplyr)

# -----------------------------------------
# 1) LOAD + CHECK
# -----------------------------------------
df <- read.csv("pdi_data.csv")

glimpse(df)
names(df)
summary(df)
unique(df$Tray_Friday)

# -----------------------------------------
# 2) FILTER  (keep only XLT4)
# -----------------------------------------
df_XLT4 <- df %>%
  filter(Tray_Friday == "XLT4")

nrow(df_XLT4)
head(df_XLT4)

# -----------------------------------------
# 3) ARRANGE  (sort by highest log.CFU.g)
# -----------------------------------------
df_XLT4_sorted <- df_XLT4 %>%
  arrange(desc(`log.CFU.g`))

df_XLT4_sorted %>% head(10)

# -----------------------------------------
# 4) SELECT  (keep only useful columns)
# -----------------------------------------
df_XLT4_selected <- df_XLT4_sorted %>%
  select(Tray_Friday, Harvest.Weight..g., Count.1, Count.2, CFU.gm, log.CFU.g)

df_XLT4_selected %>% head(10)

# -----------------------------------------
# 5) MUTATE  (create new columns)
# -----------------------------------------
df_XLT4_mutated <- df_XLT4_selected %>%
  mutate(
    Plate_mean = (Count.1 + Count.2) / 2,
    log_CFU_calc = ifelse(!is.na(CFU.gm) & CFU.gm > 0, log10(CFU.gm), NA),
    CFU_per_g_weight = CFU.gm / Harvest.Weight..g.
  )

df_XLT4_mutated %>% head(10)


# -----------------------------------------
# 6) SUMMARIZE  (1 tray summary)
# -----------------------------------------
xlt4_summary <- df_XLT4_mutated %>%
  summarize(
    mean_logCFU_calc = mean(log_CFU_calc, na.rm = TRUE),
    sd_logCFU_calc   = sd(log_CFU_calc, na.rm = TRUE),
    mean_weight      = mean(Harvest.Weight..g., na.rm = TRUE),
    n                = n()
  )

xlt4_summary

# -----------------------------------------
# 7) SUMMARIZE (all trays, compare)
# -----------------------------------------
tray_summary <- df %>%
  group_by(Tray_Friday) %>%
  summarize(
    mean_logCFU = mean(log.CFU.g, na.rm = TRUE),
    sd_logCFU   = sd(log.CFU.g, na.rm = TRUE),
    n           = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_logCFU))

tray_summary


# -----------------------------------------
# 8) SAVE (don’t overwrite files)
# -----------------------------------------
write.csv(df_XLT4_mutated, "pdi_XLT4_mutated.csv", row.names = FALSE)
write.csv(xlt4_summary, "pdi_XLT4_summary.csv", row.names = FALSE)
write.csv(tray_summary, "pdi_tray_summary.csv", row.names = FALSE)



#using dplyr

# ----------------------------------
# 0) SETUP
# ----------------------------------
library(dplyr)
library(nycflights13)

# ----------------------------------
# 1) LOAD + CHECK DATA
# ----------------------------------
glimpse(flights)
summary(flights)

# ----------------------------------
# 2) FILTER
# Flights in summer (July, Aug, Sep)
# ----------------------------------
summer_flights <- flights %>%
  filter(month == 7 | month == 8 | month == 9)

nrow(summer_flights)
head(summer_flights)

# ----------------------------------
# 3) ARRANGE
# ----------------------------------

# A) Most delayed flights
most_delayed <- flights %>%
  filter(!is.na(dep_delay)) %>%
  arrange(desc(dep_delay))

most_delayed %>%
  select(year, month, day, carrier, flight, origin, dest, dep_delay) %>%
  head(10)

# B) Flights that left earliest
left_earliest <- flights %>%
  filter(!is.na(dep_time)) %>%
  arrange(dep_time)

left_earliest %>%
  select(year, month, day, carrier, flight, origin, dest, dep_time) %>%
  head(10)

# ----------------------------------
# 4) SELECT
# ----------------------------------
mdt <- flights %>%
  select(month, day, dep_time)

head(mdt)

# ----------------------------------
# 5) MUTATE
# ----------------------------------
flights_speed <- flights %>%
  mutate(
    speed = ifelse(!is.na(air_time) & air_time > 0,
                   distance / air_time * 60,
                   NA)
  )

flights_speed %>%
  select(distance, air_time, speed) %>%
  head(10)

# ----------------------------------
# 6) SUMMARIZE
# ----------------------------------
avg_delay_by_origin <- flights %>%
  group_by(origin) %>%
  summarize(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    n_flights = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_dep_delay))

avg_delay_by_origin

# ----------------------------------
# 7) (OPTIONAL) SAVE RESULTS
# ----------------------------------
write.csv(avg_delay_by_origin, "avg_delay_by_origin.csv", row.names = FALSE)




#Activity 2

# install.packages("dplyr")  # run once only if needed
library(dplyr)

# 1) Create data
my_people <- data.frame(
  name = c("padma","kalpana","sapana","kamala","sangita","anu","safal"),
  age = c(55,50,32,30,26,24,21),
  sex = c("M","F","F","F","F","F","M"),
  sleeptime = c(40,10,10,20,20,20,20)
)

# 2) FILTER
p_filtered <- my_people %>% filter(age > 30)

# 3) ARRANGE
p_arranged <- p_filtered %>% arrange(desc(sleeptime))

# 4) SELECT
p_selected <- p_arranged %>% select(name, age, sex, sleeptime)

# 5) MUTATE
p_mutated <- p_selected %>%
  mutate(
    sleep_per_day = sleeptime / 7,
    age_group = ifelse(age >= 30, "30_plus", "under_30")
  )

# 6) SUMMARIZE
summary_by_sex <- p_mutated %>%
  group_by(sex) %>%
  summarize(
    avg_age = mean(age),
    avg_sleeptime = mean(sleeptime),
    n_people = n(),
    .groups = "drop"
  )

summary_by_sex

# 7) SAVE (save the final transformed data + the summary)
write.csv(p_mutated, "people_transformed.csv", row.names = FALSE)
write.csv(summary_by_sex, "people_summary_by_sex.csv", row.names = FALSE)


