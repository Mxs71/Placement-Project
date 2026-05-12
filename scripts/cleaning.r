library(readxl)
library(tidyverse)

set_1 <- read_excel("data_raw/XML/Set 1.xlsx")
set_2 <- read_excel("data_raw/XML/Set 2.xlsx")
set_3 <- read_excel("data_raw/XML/Set 3.xlsx")

Demo <- read_delim("data_raw/ASCII/DEMO26Q1.txt", delim="$")
Drug <- read_delim("data_raw/ASCII/DRUG26Q1.txt", delim="$")
Indi <- read_delim("data_raw/ASCII/INDI26Q1.txt", delim="$")
Outc <- read_delim("data_raw/ASCII/OUTC26Q1.txt", delim="$")
Reac <- read_delim("data_raw/ASCII/REAC26Q1.txt", delim="$")
Rpsr <- read_delim("data_raw/ASCII/RPSR26Q1.txt", delim="$")
Ther <- read_delim("data_raw/ASCII/THER26Q1.txt", delim="$")


set_1c <- set_1
set_1c <- set_1c %>% filter(!is.na(reporttype))
set_1c <- set_1c %>% replace_na(list(duplicate = 0)) #Replace all nulls with 0, showing no duplicates
set_1c <- set_1c %>% filter(if_all(starts_with("serious"), ~!is.na(.))) #Removed all nulls from serious- categories
summary(set_1c)

set_2c <- set_2
set_2c <- set_2c %>% filter(!is.na(reporttype))
set_2c <- set_2c %>% replace_na(list(duplicate = 0)) #Same as set 1
set_2c <- set_2c %>% filter(if_all(starts_with("serious"), ~!is.na(.))) #Same as set 1
summary(set_2c)

set_3c <- set_3
set_3c <- set_3c %>% filter(!is.na(reporttype)) #Same as other sets
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
Demo_c <- Demo_c %>% replace_na(list(age = 59))
Demo_c <- Demo_c %>% replace_na(list(age_cod = "YR"))
summary(Demo_c)

