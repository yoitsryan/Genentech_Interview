# Question 3, part 1: Adverse events summary table

# Load libraries & data -------------------------------------
library(pharmaverseadam)
library(gtsummary)
library(dplyr)
library(gt)

pharma_adae = pharmaverseadam::adae
pharma_adsl = pharmaverseadam::adsl


# Step 1: Filter for treatment-emergent AEs
adae_te <- pharma_adae %>%
  filter(TRTEMFL == "Y")  # Only treatment-emergent AEs

# Step 2: Join with ADSL to get treatment information (if not already present)
# Note: ACTARM should already be in ADAE, but double-check
# adae_te <- adae_te %>%
#   left_join(pharma_adsl %>% select(USUBJID, ACTARM), by = "USUBJID")

# Step 2: Create summary table (count and percent)
ae_summary <- adae_te %>%
  # Count subjects with each AE by treatment
  group_by(ACTARM, AETERM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  # Calculate percentages within each treatment group
  left_join(
    pharma_adsl %>%
      group_by(ACTARM) %>%
      summarise(N = n(), .groups = "drop"),
    by = "ACTARM"
  ) %>%
  mutate(percent = round(n / N * 100, 1),
         n_pct = paste0(n, " (", percent, "%)"))

# Step 3: Reshape into wide format for gtsummary
ae_table_wide <- ae_summary %>%
  select(AETERM, ACTARM, n_pct) %>%
  tidyr::pivot_wider(names_from = ACTARM, values_from = n_pct) %>%
  # Optional: Add total column
  left_join(
    adae_te %>%
      group_by(AETERM) %>%
      summarise(Total = paste0(n_distinct(USUBJID), " (",
                               round(n_distinct(USUBJID) / nrow(pharma_adsl) * 100, 1),
                               "%)"), .groups = "drop"),
    by = "AETERM"
  ) %>%
  # Sort by descending total frequency
  arrange(desc(as.numeric(gsub("\\s\\(.*\\)", "", Total))))

# Step 4: Calculate Ns per treatment arm
treatment_n <- pharma_adsl %>%
  group_by(ACTARM) %>%
  summarise(N = n(), .groups = "drop") %>%
  mutate(header = paste0(ACTARM, " (N=", N, ")"))

# Step 5: Also calculate total N
total_n <- nrow(pharma_adsl)
total_header <- paste0("Total (N=", total_n, ")")

# Step 6: Build gt table from your wide AE table
ae_table_wide %>%
  gt() %>%
  tab_header(title = "Summary of Treatment-Emergent Adverse Events (TEAEs)") %>%
  cols_label(AETERM = "Adverse Event") %>%
  fmt_missing(columns = everything(), missing_text = "0 (0%)") %>%
  # Step 7: Rename columns to include Ns
  cols_label(
    `Placebo` = treatment_n$header[treatment_n$ACTARM == "Placebo"],
    `Xanomeline High Dose` = treatment_n$header[treatment_n$ACTARM == "Xanomeline High Dose"],
    `Xanomeline Low Dose` = treatment_n$header[treatment_n$ACTARM == "Xanomeline Low Dose"],
    `Total` = total_header
  )
