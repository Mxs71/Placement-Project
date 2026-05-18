library(readxl)
library(tidyverse)
library(ggplot2)

set_1 <- read_excel("data_raw/XML/Set 1.xlsx")
set_2 <- read_excel("data_raw/XML/Set 2.xlsx")
set_3 <- read_excel("data_raw/XML/Set 3.xlsx")

Demo <- read_delim("data_raw/ASCII/DEMO26Q1.txt", delim = "$")
Drug <- read_delim("data_raw/ASCII/DRUG26Q1.txt", delim = "$")
Indi <- read_delim("data_raw/ASCII/INDI26Q1.txt", delim = "$")
Outc <- read_delim("data_raw/ASCII/OUTC26Q1.txt", delim = "$")
Reac <- read_delim("data_raw/ASCII/REAC26Q1.txt", delim = "$")
Rpsr <- read_delim("data_raw/ASCII/RPSR26Q1.txt", delim = "$")
Ther <- read_delim("data_raw/ASCII/THER26Q1.txt", delim = "$")


set_1c <- set_1
set_1c <- set_1c %>% replace_na(list(reporttype = 4)) #4 is the value for null
set_1c <- set_1c %>% replace_na(list(duplicate = 0)) #Replace all nulls with 0, showing case priority
set_1c <- set_1c %>% filter(if_all(starts_with("serious"), ~!is.na(.))) #Removed all nulls from serious- categories
summary(set_1c)

set_2c <- set_2
set_2c <- set_2c %>% replace_na(list(reporttype = 4))
set_2c <- set_2c %>% replace_na(list(duplicate = 0)) #Same as set 1
set_2c <- set_2c %>% filter(if_all(starts_with("serious"), ~!is.na(.))) #Same as set 1
summary(set_2c)

set_3c <- set_3
set_3c <- set_3c %>% replace_na(list(reporttype = 4)) #Same as other sets
set_3c <- set_3c %>% replace_na(list(duplicate = 0)) #Same again
set_3c <- set_3c %>% filter(if_all(starts_with("serious"), ~!is.na(.))) #Same again
summary(set_3c)

#Isolate names of common columns
common_cols <- Reduce(intersect, list(names(set_1c), names(set_2c), names(set_3c)))
#Use the common columns variable to filter how the sets are joined to prevent duplicates
full_set <- set_1c %>%
    left_join(set_2c, by = common_cols) %>%
    left_join(set_3c, by = common_cols)
summary(full_set)


Demo_c <- Demo
Demo_c <- Demo_c %>%
#Standardises length of age into years
  mutate(age = case_when(
    age_cod == "HR" ~ age / (24 * 365),
    age_cod == "DY" ~ age / 365,
    age_cod == "WK" ~ age / 52,
    age_cod == "MON" ~ age / 12,
    age_cod == "YR" ~ age,
    age_cod == "DEC" ~ age * 10,
    TRUE ~ age
  ), 
  age_cod = "YR") %>%
#Standardises ranges of age groups and sorts them
  mutate(age_grp = case_when(
    age <= 1 / 12 ~ "N",
    age > 1 / 12 & age <= 2 ~ "I",
    age > 2 & age <= 12 ~ "C",
    age > 12 & age <= 17 ~ "T",
    age >= 18 & age <= 64 ~ "A",
    age >= 65 ~ "E",
    TRUE ~ age_grp
    )) %>%
#Replace missing values with median values of age group
  mutate(age = case_when(
    age_grp == "N" & is.na(age) ~ 0.00274,
    age_grp == "I" & is.na(age) ~ 1,
    age_grp == "C" & is.na(age) ~ 8,
    age_grp == "T" & is.na(age) ~ 15,
    age_grp == "A" & is.na(age) ~ 49,
    age_grp == "E" & is.na(age) ~ 74,
    TRUE ~ age
    )) %>%
#Replace missing values with median values of age group
  mutate(wt = case_when(
    age_grp == "N" & is.na(wt) ~ 2.9,
    age_grp == "I" & is.na(wt) ~ 9.09,
    age_grp == "C" & is.na(wt) ~ 26.6,
    age_grp == "T" & is.na(wt) ~ 55,
    age_grp == "A" & is.na(wt) ~ 75,
    age_grp == "E" & is.na(wt) ~ 71,
    wt_cod == "LBS" ~ wt * 0.4526,
    wt_cod == "GMS" ~ wt / 1000,
    TRUE ~ wt
    ),
    wt_cod = "KG") %>%
#Sets all values to within probable range
  mutate(wt = case_when(
    age_grp == "N" & !is.na(wt) & wt >= 1000 ~ wt / 1000,
    age_grp == "N" & !is.na(wt) & wt >= 100 ~ wt / 100,
    age_grp == "N" & !is.na(wt) & wt >= 10 ~ wt / 10,
    age_grp == "I" & !is.na(wt) & wt >= 100 ~ wt / 10,
    age_grp == "I" & !is.na(wt) & wt >= 20 ~ wt / 10,
    age_grp == "C" & !is.na(wt) & wt >= 100 ~ wt / 10,
    age_grp == "C" & !is.na(wt) & wt >= 50 ~ wt / 10,
    age_grp == "T" & !is.na(wt) & wt >= 200 ~ wt / 10,
    age_grp == "A" & !is.na(wt) & wt >= 1000 ~ wt / 1000,
    age_grp == "A" & !is.na(wt) & wt >= 500 ~ wt / 10,
    age_grp == "E" & !is.na(wt) & wt >= 1000 ~ wt / 1000,
    TRUE ~ wt
  ))
summary(Demo_c)

