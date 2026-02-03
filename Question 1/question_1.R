# Question 1: Create SDTM domain dataset from raw data

library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)
library(tidyverse)
library(lubridate)

# First start with the raw data file
ds_raw = pharmaverseraw::ds_raw
study_ct = read.csv("sdtm_ct.csv")

# I have never worked with SDTM data before, so bear with me here
# We need 12 variables in our cleaned dataset

ds = ds_raw %>%
  # There are 6 columns already present in the raw data that simply need renaming
  rename(
    STUDYID = STUDY,
    USUBJID = PATNUM,
    DSTERM = IT.DSTERM,
    VISIT = INSTANCE,
    DSDTC = DSDTCOL,
    DSSTDTC = IT.DSSTDAT
  ) %>%
  # The DOMAIN is DS (disposition)
  mutate(DOMAIN = "DS") %>%
  # We need unique identifiers for each record in the dataset (DSSEQ)
  mutate(DSSEQ = paste0("R", row_number()))

# The values for DSDECOD are listed in either one column or the other.
ds$DSDECOD = coalesce(ds$IT.DSDECOD, ds$OTHERSP)

# The category should either be Protocol Milestone or Disposition Event.
# Randomization and informed consent are the only two protocol milestones that exist.
# Informed consent is not listed in the dataset anywhere.
ds$DSCAT <- ifelse(ds$DSDECOD == "Randomized",
                   "Protocol Milestone",
                   "Disposition Event")

# I included people who had Screen Failures. They are going to have visit numbers of 0.1
# For unscheduled visits, the visit number will have a decimal after it.
# Set VISITNUM's for each visit
ds <- ds %>%
  mutate(
    VISITNUM = case_when(
      DSDECOD == "Randomized" ~ 1,
      DSDECOD == "Completed" ~ 2,
      DSDECOD == "Final Lab Visit" ~ 3,
      DSDECOD == "Final Retrieval Visit" ~ 4,
      DSDECOD == "Screen Failure" ~ 0.1,
      TRUE ~ 1.1
    )
  )

# Finally, DSSTDY should be the number of days that pass from the first recorded
# date for each individual USUBJID.
ds <- ds %>%
  mutate(DSDTC = as.Date(DSDTC, format = "%m-%d-%Y")) %>%     # ensure Date type
  group_by(USUBJID) %>%
  mutate(
    DSSTDY = as.numeric(DSDTC - min(DSDTC, na.rm = TRUE))
  ) %>%
  ungroup()

# With the new columns, output the final data frame
cleaned_ds <- ds[, c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", 
             "DSDECOD", "DSCAT", "VISITNUM", "VISIT", 
             "DSDTC", "DSSTDTC", "DSSTDY")]

# But wait! Change the format of DSDTC back to the way it originally was
cleaned_ds <- cleaned_ds %>%
  mutate(DSDTC = format(DSDTC, "%m-%d-%Y"))

cleaned_ds

write_csv(cleaned_ds, "DS_domain.csv")