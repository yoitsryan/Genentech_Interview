# Question 3, part 2: Visualizations with ggplot2


# PLOT 1: Stacked bar chart
library(dplyr)
library(ggplot2)
library(pharmaverseadam)

# Filter for treatment-emergent AEs
adae_te <- pharmaverseadam::adae %>%
  filter(TRTEMFL == "Y") %>%
  select(ACTARM, AESEV)

# Count AEs (EVENTS, not subjects)
ae_severity_counts <- adae_te %>%
  group_by(ACTARM, AESEV) %>%
  summarise(
    n = n(),   # <-- raw AE counts
    .groups = "drop"
  )

# Stacked bar chart with AE COUNTS
ggplot(ae_severity_counts, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Treatment Arm",
    y = "Number of Adverse Events",
    fill = "AE Severity",
    title = "AE Severity Distribution by Treatment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank()
  )


# PLOT 2: Top 10 most frequent adverse effects
library(ggplot2)
library(dplyr)
library(pharmaverseadam)
library(binom)  # for confidence intervals

# Count subjects with each AE
ae_counts <- pharmaverseadam::adae %>%
  group_by(AETERM) %>%
  summarise(n = n_distinct(USUBJID), .groups="drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)  # Top 10 AEs

# Merge denominator (total subjects)
total_n <- nrow(pharmaverseadam::adsl)

ae_counts <- ae_counts %>%
  mutate(
    proportion = n / total_n,
    ci = binom.confint(n, total_n, methods = "wilson")
  )

# Prepare data for ggplot
ae_counts <- ae_counts %>%
  mutate(AETERM = factor(AETERM, levels = rev(AETERM)))  # reverse for plotting

# Plot with 95% CI
ggplot(ae_counts, aes(x = proportion, y = AETERM)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = ci$lower, xmax = ci$upper), height = 0.2, color = "steelblue") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Incidence Rate (%)",
    y = "Adverse Event",
    title = "Top 10 Most Frequent Adverse Events"
  ) +
  theme_minimal()