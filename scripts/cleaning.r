library(readxl)
library(tidyverse)

set_1 <- read_excel("data_raw/XML/Set 1.xlsx")
set_2 <- read_excel("data_raw/XML/Set 2.xlsx")
set_3 <- read_excel("data_raw/XML/Set 3.xlsx")
summary(set_1)
summary(set_2)
summary(set_3)

Demo <- read_delim("data_raw/ASCII/DEMO26Q1.txt", delim="$")
Drug <- read_delim("data_raw/ASCII/DRUG26Q1.txt", delim="$")
Indi <- read_delim("data_raw/ASCII/INDI26Q1.txt", delim="$")
Outc <- read_delim("data_raw/ASCII/OUTC26Q1.txt", delim="$")
Reac <- read_delim("data_raw/ASCII/REAC26Q1.txt", delim="$")
Rpsr <- read_delim("data_raw/ASCII/RPSR26Q1.txt", delim="$")
Ther <- read_delim("data_raw/ASCII/THER26Q1.txt", delim="$")


set_1c <- set_1
set_1c <- set_1c %>% replace_na(list(duplicate = 0))

summary(set_1c)
