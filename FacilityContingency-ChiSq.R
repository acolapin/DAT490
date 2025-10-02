library(readxl)
ed <- read_excel("Downloads/emergency-department-volume-and-capacity-2021-2023.xlsx")
View(ed)

library(dplyr)

# one row per hospital with its PCSA + Urban/Rural label
hospitals <- ed %>%
  filter(Category == "All ED Visits") %>%
  distinct(oshpd_id, PrimaryCareShortageArea, UrbanRuralDesi)

# Count hospitals by PCSA × Urban/Rural

tbl <- table(hospitals$PrimaryCareShortageArea, hospitals$UrbanRuralDesi)
#HOSPITALS BALLOON PLOT
library(gplots)
# Transpose so that the urban/rural categories appear on the x-axis
balloonplot(
  t(tbl),
  main = "Observed Counts of Urbanicity and PCSA Status",
  xlab = "Urban Rural Designation",
  ylab = "Primary Care Shortage Area",
  label = TRUE,          
  label.size = 1,      
  label.color = "cornflowerblue",
  text.size = 0.75,     # size of the tick labels
  show.margins = TRUE  )  #
chisq.test(tbl)
chi <- chisq.test(tbl)
exp <- chi$expected   # expected frequencies under independence
obs <- chi$observed   # observed frequencies

##############Expected counts table
library(ggplot2)

# chi from your code
chi <- chisq.test(tbl)

# transpose to match balloon orientation
exp_tbl  <- t(chi$expected)
exp_long <- as.data.frame(as.table(exp_tbl))
names(exp_long) <- c("PCSA", "UrbanRuralDesi", "Expected")

ggplot(exp_long, aes(x = PCSA, y = UrbanRuralDesi, fill = Expected)) +
  geom_tile(color = "white") +
  geom_text(aes(label = format(round(Expected), big.mark = ",")), size = 3) +
  scale_fill_gradient(name = "Expected",
                      low = "grey90", high = "steelblue", guide = "none") +
  labs(
    title   = "Expected Counts under Independence",
    x       = "Urbanicity",
    y       = "PCSA",
    caption = sprintf("From chi-square on tbl: X^2(%d) = %.0f, p %s",
                      chi$parameter, chi$statistic,
                      ifelse(chi$p.value < .001, "< .001", paste0("= ", signif(chi$p.value, 3))))
  ) +
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank())
