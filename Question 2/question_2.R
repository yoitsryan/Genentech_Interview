# Question 2: Deriving variables for ADSL dataset

library(admiral)
library(dplyr)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(tidyverse)

dm <- pharmaversesdtm::dm # demographics
ds <- pharmaversesdtm::ds # disposition
ex <- pharmaversesdtm::ex # exposure
ae <- pharmaversesdtm::ae # adverse effects
vs <- pharmaversesdtm::vs # vitals

# Convert blanks to NA (missing)
dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

# DM (demographics) dataset is used as the basis of the ADSL.
adsl <- dm %>%
  select(-DOMAIN)

# AGEGR9 variable
adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      (AGE >= 18) & (AGE <= 50) ~ "18 - 50",
      AGE > 50 ~ ">50"
    )
  )


# AGEGR9N variable
adsl <- adsl %>%
  mutate(
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      (AGE >= 18) & (AGE <= 50) ~ 2,
      AGE > 50 ~ 3
    )
  )


# TRTSDTM variable
# This is going to take a lot of work.

# First, we filter to valid entries from EX
# ... which is all of them!
ex_valid <- ex %>%
  filter(EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT, ignore.case = TRUE)))
# (A valid dose is 0 for a placebo, or above 0 for treaement)

# Function to impute time in EXSTDTC.
impute_time <- function(dt) {
  # Start with replacing T with space
  dt <- gsub("T", " ", dt)
  
  # If completely missing → NA (filter later)
  # If only date YYYY-MM-DD → add 00:00:00
  dt <- ifelse(nchar(dt) == 10, paste0(dt, " 00:00:00"), dt)
  
  # If partial time
  dt <- ifelse(nchar(dt) == 13, paste0(dt, ":00:00"), dt)   # HH
  dt <- ifelse(nchar(dt) == 16, paste0(dt, ":00"), dt)      # HH:MM
  
  return(dt)
}

# Apply vectorized function in mutate
ex_valid <- ex_valid %>%
  filter(!is.na(substr(EXSTDTC, 1, 10))) %>%  # ensure date part complete
  mutate(
    EXSTDTC_IMPUTED = impute_time(EXSTDTC),
    EXSTDTM_NUM = as.POSIXct(EXSTDTC_IMPUTED, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

# Find first valid exposure per patient
trtsdtm <- ex_valid %>%
  group_by(USUBJID) %>%
  arrange(EXSTDTM_NUM) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(USUBJID, TRTSDTM = EXSTDTM_NUM)

# Merge with ADSL
adsl <- adsl %>%
  left_join(trtsdtm, by = "USUBJID")


# ITTFL variable
# Set to "Y" if [DM.ARM] not equal to missing Else set to "N"
adsl <- adsl %>%
  mutate(
    ITTFL = ifelse(!is.na(ARM), "Y", "N")
  )


# LSTAVLDT variable

# Vitals last complete date
vs_last <- vs %>%
  filter(
    !(is.na(VSSTRESN) & is.na(VSSTRESC)),
    !is.na(substr(VSDTC, 1, 10))
  ) %>%
  mutate(VSDT = as.Date(substr(VSDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(VS_LASTDT = max(VSDT, na.rm = TRUE), .groups = "drop")

# Adverse effects last onset date
ae_last <- pharmaversesdtm::ae %>%
  filter(!is.na(substr(AESTDTC, 1, 10))) %>%
  mutate(AEDT = as.Date(substr(AESTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(AE_LASTDT = max(AEDT, na.rm = TRUE), .groups = "drop")

# Disposition last date
ds_last <- pharmaversesdtm::ds %>%
  filter(!is.na(substr(DSSTDTC, 1, 10))) %>%
  mutate(DSDT = as.Date(substr(DSSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(DS_LASTDT = max(DSDT, na.rm = TRUE), .groups = "drop")

# Treatment last date (from ADSL)
adsl <- adsl %>%
  mutate(TRT_LASTDT = as.Date(TRTSDTM))

# Combine everything and derive LSTAVLDT
adsl <- adsl %>%
  left_join(vs_last, by = "USUBJID") %>%
  left_join(ae_last, by = "USUBJID") %>%
  left_join(ds_last, by = "USUBJID") %>%
  mutate(
    LSTAVLDT = pmax(
      VS_LASTDT,
      AE_LASTDT,
      DS_LASTDT,
      TRT_LASTDT,
      na.rm = TRUE
    )
  )

# Oops, let's drop those other new columns, as we don't need to keep them!
adsl <- adsl %>%
  select(-TRT_LASTDT, -VS_LASTDT, -AE_LASTDT, -DS_LASTDT)

write_csv(adsl, "ADSL.csv")