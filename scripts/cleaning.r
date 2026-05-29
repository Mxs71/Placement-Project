library(tidyverse)
library(stringr)
library(data.table)
library(stringdist)
library(fuzzyjoin)
library(readxl)
library(fst)


quarters <- list(
  list(year = "25", qs = c("Q1", "Q2", "Q3", "Q4")),
  list(year = "26", qs = c("Q1"))
)

read_faers <- function(table, year, q) {
  path <- paste0("data_raw/ASCII/", table, year, q, ".txt")
  if (file.exists(path)) read_delim(path, delim = "$") else NULL
}

load_table <- function(table) {
  map_dfr(quarters, function(yr) {
    map_dfr(yr$qs, ~ read_faers(table, yr$year, .x))
  })
}

Demo <- load_table("DEMO")
Drug <- load_table("DRUG")
Indi <- load_table("INDI")
Outc <- load_table("OUTC")
Reac <- load_table("REAC")
Rpsr <- load_table("RPSR")
Ther <- load_table("THER")


Demo_c <- Demo
Demo_c <- Demo_c %>%
  arrange(desc(fda_dt)) %>%
  distinct(caseid, .keep_all = TRUE) %>%
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
  mutate(age_grp = case_when(
    age <= 1 / 12 ~ "N",
    age > 1 / 12 & age <= 2 ~ "I",
    age > 2 & age <= 12 ~ "C",
    age > 12 & age <= 17 ~ "T",
    age >= 18 & age <= 64 ~ "A",
    age >= 65 ~ "E",
    TRUE ~ age_grp
  )) %>%
  mutate(wt = case_when(
    wt_cod == "LBS" ~ wt * 0.4526,
    wt_cod == "GMS" ~ wt / 1000,
    TRUE ~ wt
  ),
  wt_cod = "KG")

age_med <- Demo_c %>%
  filter(age > 0, age <= 120) %>%
  group_by(age_grp) %>%
  summarise(median_age = median(age, na.rm = TRUE), .groups = "drop") %>%
  { setNames(.$median_age, .$age_grp) }

wt_med <- Demo_c %>%
  filter(wt > 0, wt <= 300) %>%
  group_by(age_grp) %>%
  summarise(median_wt = median(wt, na.rm = TRUE), .groups = "drop") %>%
  { setNames(.$median_wt, .$age_grp) }

Demo_c <- Demo_c %>%
  mutate(age = case_when(
    age_grp == "N" & is.na(age) ~ age_med[["N"]],
    age_grp == "I" & is.na(age) ~ age_med[["I"]],
    age_grp == "C" & is.na(age) ~ age_med[["C"]],
    age_grp == "T" & is.na(age) ~ age_med[["T"]],
    age_grp == "A" & is.na(age) ~ age_med[["A"]],
    age_grp == "E" & is.na(age) ~ age_med[["E"]],
    age > 127 ~ age / 10,
    TRUE ~ age
  )) %>%
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
    age_grp == "N" & is.na(wt) ~ wt_med[["N"]],
    age_grp == "I" & is.na(wt) ~ wt_med[["I"]],
    age_grp == "C" & is.na(wt) ~ wt_med[["C"]],
    age_grp == "T" & is.na(wt) ~ wt_med[["T"]],
    age_grp == "A" & is.na(wt) ~ wt_med[["A"]],
    age_grp == "E" & is.na(wt) ~ wt_med[["E"]],
    TRUE ~ wt
  )) %>%
  replace_na(list(age = unname(age_med[["A"]]),
                  wt = unname(wt_med[["A"]]),
                  age_grp = "A"))


#Lots of missing data, perticularly info about the drugs rather than what the actual drug is
#Cannot replace the missing data
#Should still be usable as analysis needs the primarily names and groups
Drug_c <- Drug
Drug_c[, id := .I]
atc <- read.csv("data_raw/WHO ATC-DDD 2026-04-25.csv")
ref <- data.table(name = atc$atc_name)

setDT(Drug_c)
setDT(ref)

normalize_name <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "\\b(\\d+mg|\\d+g|\\d+ml|\\d+mcg)\\b", "")
  x <- str_replace_all(x, "\\b(iv|po|im|sc|topical|oral)\\b", "")
  x <- str_replace_all(x, "[^a-z0-9 ]", " ")
  x <- str_squish(x)
  return(x)
}

Drug_c[, normalized := normalize_name(drugname)]
ref[, name_norm := normalize_name(name)]

ref_names <- unique(ref$name_norm)

Drug_c[, exact_match := normalized %in% ref_names]
Drug_c[, matched_exact := ifelse(exact_match, normalized, NA)]

drug_unmatched <- Drug_c[exact_match == FALSE, .(primaryid, normalized)]

ref_split <- split(ref$name_norm, substr(ref$name_norm, 1, 1))

best_match <- function(x) {
  # If x is empty or NA
  if (is.na(x) || x == "") return(NA_character_)
  first <- substr(x, 1, 1)
  candidates <- ref_split[[first]]
  if (is.null(candidates) || length(candidates) == 0) {
    return(NA_character_)
  }
  d <- stringdist(x, candidates, method = "lv")
  if (length(d) == 0 || all(is.na(d))) {
    return(NA_character_)
  }
  candidates[which.min(d)]
}

unique_unmatched <- unique(drug_unmatched$normalized)
unique_unmatched_dt <- data.table(normalized = unique_unmatched)
unique_unmatched_dt[, fuzzy := vapply(normalized, best_match, FUN.VALUE = character(1))]

drug_unmatched[unique_unmatched_dt, on = "normalized", fuzzy := i.fuzzy]

Drug_c[drug_unmatched, on = "primaryid", final_name := i.fuzzy]


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



demo_outc <- Demo_c %>%
  left_join(Outc_c, by = c("primaryid", "caseid"))

Drug_c <- as.data.frame(Drug_c)

demo_drug <- Demo_c %>%
  left_join(Drug_c, by = c("primaryid", "caseid")) %>%
  left_join(Indi_c, by = c("primaryid", "caseid", "drug_seq" = "indi_drug_seq")) %>%
  select(
    # Join keys
    primaryid, caseid, drug_seq,
    # Drug identity
    drugname, normalized,
    # Time (Module 1 — reporting trends)
    fda_dt,
    # Drug role — needed to document multi-drug report handling per brief
    role_cod,
    # Indication (Module 2 — reaction profile context)
    indi_pt,
    # Demographics (Module 4 — population context)
    age, age_grp, sex, wt,
    # Reporter metadata (Module 4 — optional filters)
    occp_cod, reporter_country
  )

demo_drug_ther <- demo_drug %>%
  left_join(Ther_c, by = c("primaryid", "caseid", "drug_seq" = "dsg_drug_seq"))

demo_drug_reac <- demo_drug %>%
  left_join(Reac_c, by = "primaryid")

demo_rpsr <- Demo_c %>%
  left_join(Rpsr_c, by = "primaryid")


write_fst(demo_outc, "data_clean/demo_outc.fst", compress = 50)
write_fst(demo_drug, "data_clean/demo_drug.fst", compress = 50)
write_fst(demo_drug_ther, "data_clean/demo_drug_ther.fst", compress = 50)
write_fst(demo_drug_reac, "data_clean/demo_drug_reac.fst", compress = 50)
write_fst(demo_rpsr, "data_clean/demo_rpsr.fst", compress = 50)
