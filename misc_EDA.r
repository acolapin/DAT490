library(readxl)
library(dplyr)

# Read the data
ed = emergency_department_volume_and_capacity_2021_2023

# Inspect unique years and categories
unique(ed$year)
unique(ed$Category)
###############################################################################
library(ggplot2)

# Specify order of bed-size categories
bed_order <- c("1-49", "50-99","100-149", "150-199", "200-299", "300-499","500+")

ed_all <- ed %>%
  filter(Category == "All ED Visits") %>%
  mutate(LICENSED_BED_SIZE = factor(LICENSED_BED_SIZE, levels = bed_order),Year = factor(year))

ggplot(ed_all, aes(x = LICENSED_BED_SIZE, y = Tot_ED_NmbVsts, fill = Year)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total ED visits by bed size and year", x = "Licensed bed size", 
       y = "Total ED visits", fill = "Year") +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot")


# Violin plots By Diagnosis Catgory
# Filter for specific conditions
ed_sub = ed %>% filter(Category != "All ED Visits")
options(scipen = 999)
ggplot(ed_sub, aes(x = Category, y = Visits_Per_Station, fill = Category)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.size = 0.5) +
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000),
                labels = c("0.1", "1", "10", "100", "1000")) + 
  labs(title = "Distribution of Visits per Station by Diagnosis Category (2021-2023)",
       x = "Diagnosis Category", y = "Visits per Station (log)") +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

library(dplyr)

# One record per hospital
ed_facilities <- ed %>% 
  distinct(oshpd_id, HospitalOwnership, UrbanRuralDesi)

tbl <- table(ed_facilities$HospitalOwnership, ed_facilities$UrbanRuralDesi)
#HOSPITALS BALLOON PLOT
library(gplots)
# Transpose so that the urban/rural categories appear on the x-axis
balloonplot(
  t(tbl),
  main = "Contingency Table of Facility Ownership and Urban/Rural Designation",
  xlab = "Urban/Rural designation",
  ylab = "Hospital ownership",
  label = TRUE,          # show the counts inside the balloons
  label.size = 1,        # size of the text inside the balloons
  label.color = "cornflowerblue",
  text.size = 0.75,     # size of the tick labels
  show.margins = TRUE  )  # display row/column totals on the margins
chisq.test(tbl)
chi <- chisq.test(tbl)
exp_counts <- chi$expected   # expected frequencies under independence
obs_counts <- chi$observed   # observed frequencies


#####################################################################################

ed_all <- ed %>% 
  filter(Category == "All ED Visits") %>%
  distinct(oshpd_id, year, HospitalOwnership, UrbanRuralDesi, Tot_ED_NmbVsts)

tbl_counts <- xtabs(Tot_ED_NmbVsts ~ HospitalOwnership + UrbanRuralDesi, data = ed_all)
chisq.test(tbl_counts)

chisq = chisq.test(tbl_counts)
exp_counts <- chisq$expected   
obs_counts <- chisq$observed   
chisq.test(tbl_counts)

#VISITS BALLOON PLOT
library(gplots)
# Transpose so that the urban/rural categories appear on the x-axis
balloonplot(
  t(tbl_counts),
  main = "Contingency Table of Total Visits By Facility Ownership and Urban/Rural Designation",
  xlab = "Urban/Rural designation",
  ylab = "Hospital ownership",
  label = TRUE,          # show the counts inside the balloons
  label.size = 1,        # size of the text inside the balloons
  label.color = "cornflowerblue",
  text.size = 0.75,     # size of the tick labels
  show.margins = TRUE    # display row/column totals on the margins
)

# Poisson Plots
library(scales)
ggplot(ed, aes(x = EDStations, y = EDDXCount)) +
  geom_point(alpha = 0.3) +
  stat_smooth(
    method = "glm",
    method.args = list(family = poisson(link = "log")),
    se = FALSE,
    colour = "darkorange"
  ) +
  facet_wrap(~ Category, scales = "free_y") +
  labs(
    x = "Number of ED stations",
    y = "Diagnosis-specific visit count",
    title = "Per-category Poisson Regression Curves on Visitor Counts vs Treatment Stations"
  ) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

################################################################################

# Utilization Tot_ED_NmbVsts vs EDStations (hospital-year level)
overall_util <- ed %>%
  dplyr::distinct(oshpd_id, year, EDStations, Tot_ED_NmbVsts) %>%
  dplyr::mutate(year = as.factor(year))

ggplot(overall_util, aes(x = EDStations, y = Tot_ED_NmbVsts, color = year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "ED Throughput vs Capacity (Per Facility Per Year)",
    x = "ED Treatment Stations",
    y = "Total ED Visits (all categories)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")


