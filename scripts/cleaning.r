library(tidyverse)
library(ggplot2)


Demo <- read_delim("data_raw/ASCII/DEMO26Q1.txt", delim = "$")
Drug <- read_delim("data_raw/ASCII/DRUG26Q1.txt", delim = "$")
Indi <- read_delim("data_raw/ASCII/INDI26Q1.txt", delim = "$")
Outc <- read_delim("data_raw/ASCII/OUTC26Q1.txt", delim = "$")
Reac <- read_delim("data_raw/ASCII/REAC26Q1.txt", delim = "$")
Rpsr <- read_delim("data_raw/ASCII/RPSR26Q1.txt", delim = "$")
Ther <- read_delim("data_raw/ASCII/THER26Q1.txt", delim = "$")


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
    age > 127 ~ age / 10,
    TRUE ~ age
    )) %>%
#Replace missing values with median values of age group
  mutate(wt = case_when(
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
  )) %>%
  mutate(wt = case_when(
    age_grp == "N" & is.na(wt) ~ 2.9,
    age_grp == "I" & is.na(wt) ~ 9.09,
    age_grp == "C" & is.na(wt) ~ 26.6,
    age_grp == "T" & is.na(wt) ~ 55,
    age_grp == "A" & is.na(wt) ~ 75,
    age_grp == "E" & is.na(wt) ~ 71,
    TRUE ~ wt
  )) %>%
  replace_na(list(age = 58)) %>%
  replace_na(list(wt = 75)) %>%
  replace_na(list(age_grp = "A"))


#Lots of missing data, perticularly info about the drugs rather than what the actual drug is
#Cannot replace the missing data
#Should still be usable as analysis needs the primarily names and groups
Drug_c <- Drug

#Columns missing data
#Drug - prod_ai - 35272
#Drug - route - 788704
#Drug - cum_dose_chr - 1671961
#1Drug - cum_dose_unit - 671961
#Drug - dechal - 714633
#Drug - rechal - 139551
#Drug - dose_amt - 1036943
#Drug - dose_unit - 1036943
#Drug - dose_form - 1069364
#Drug - dose_freq - 1338498

#Drug - prod_ai - 

Indi_c <- Indi
#Indication dataset is already clean

Outc_c <- Outc
#Outcomes dataset is already clean

Reac_c <- Reac
#Dataset mostly clean, Drug recur action data has large ammount missing data
#cannot replace so will leave as it

Rpsr_c <- Rpsr
#Dataset is already clean

Ther_c <- Ther
#Lot's of missing data, but no way to replace
#Most of the data is not needed for final analysis in any case


demo_reac <- Demo_c %>%
  left_join(Reac_c, by = "primaryid")

demo_outc <- Demo_c %>%
  left_join(Outc_c, by = "primaryid")

demo_drug <- Demo_c %>%
  left_join(Drug_c, by = c("primaryid", "caseid")) %>%
  left_join(Indi_c, by = c("primaryid", "caseid", "drug_seq" = "indi_drug_seq"))

demo_drug_ther <- demo_drug %>%
  left_join(Ther_c, by = c("primaryid", "caseid", "drug_seq" = "dsg_drug_seq"))

demo_rpsr <- Demo_c %>%
  left_join(Rpsr_c, by = "primaryid")

write.csv(demo_reac, "data_clean/demo_reac.csv", row.names = FALSE)
write.csv(demo_outc, "data_clean/demo_outc.csv", row.names = FALSE)
write.csv(demo_drug, "data_clean/demo_drug.csv", row.names = FALSE)
write.csv(demo_drug_ther, "data_clean/demo_drug_ther.csv", row.names = FALSE)
write.csv(demo_rpsr, "data_clean/demo_rpsr.csv", row.names = FALSE)
