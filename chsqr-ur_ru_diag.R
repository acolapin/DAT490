
library(dplyr)
library(effectsize) # for cramers_v()
library(tibble)
library(ggplot2)

# Diagnosis × PCSA
by_diagnosis <- ed %>%
  filter(Category != "All ED Visits") %>%
  group_by(PrimaryCareShortageArea, Category) %>%
  summarise(EDDXCount = sum(EDDXCount), .groups = "drop")

diagnosis_counts <- xtabs(EDDXCount ~ PrimaryCareShortageArea + Category,
                          data = by_diagnosis)

chisq_diag <- chisq.test(diagnosis_counts)
chisq_diag   # overall test
chisq_diag$observed
chisq_diag$expected
chisq_diag$stdres        # standardized residuals
cramers_v(diagnosis_counts)  # effect size

# Urban/Rural × PCSA
by_urbanrural <- ed %>%
  filter(Category == "All ED Visits") %>%
  group_by(PrimaryCareShortageArea, UrbanRuralDesi) %>%
  summarise(EDDXCount = sum(EDDXCount), .groups = "drop")

urbanrural_counts <- xtabs(EDDXCount ~ PrimaryCareShortageArea + UrbanRuralDesi,
                           data = by_urbanrural)

chisq_ur <- chisq.test(urbanrural_counts)
chisq_ur
chisq_ur$observed
chisq_ur$expected
chisq_ur$stdres
cramers_v(urbanrural_counts)


diag_share <- prop.table(diagnosis_counts, margin = 1)  # row-wise: within PCSA=Yes/No
diff_diag  <- diag_share["Yes", ] - diag_share["No", ]
head(sort(diff_diag, decreasing = TRUE), 8)   # biggest over-represented in PCSA=Yes
head(sort(diff_diag, decreasing = FALSE), 8)  # biggest under-represented in PCSA=Yes


# Top For diagnoses
diag_std <- as_tibble(as.table(chisq_diag$stdres)) %>%
  rename(PCSA = PrimaryCareShortageArea, Category = Category, stdres = n) %>%
  mutate(abs_stdres = abs(stdres)) %>%
  arrange(desc(abs_stdres))

head(diag_std, 12)

# Top For urban/rural
ur_std <- as_tibble(as.table(chisq_ur$stdres)) %>%
  rename(PCSA = PrimaryCareShortageArea, UrbanRuralDesi = UrbanRuralDesi, stdres = n) %>%
  mutate(abs_stdres = abs(stdres)) %>%
  arrange(desc(abs_stdres))

ur_std

over_in_yes <- diag_std |>
  filter(PCSA == "Yes", stdres > 0) |>
  arrange(desc(stdres)) |>
  select(Category, stdres)

over_in_yes

# Use the chi-square for diagnoses:

#keep only the PCSA = "Yes" row
diag_std <- as_tibble(as.table(chisq_diag$stdres)) %>%
  rename(PCSA = PrimaryCareShortageArea, Category = Category, stdres = n) %>%
  filter(PCSA == "Yes") %>%
  mutate(
    direction = ifelse(stdres > 0, "Over in PCSA = Yes", "Over in PCSA = No")
  )

# effect size for the caption
V_diag <- cramers_v(diagnosis_counts)
V_val  <- suppressWarnings(as.numeric(V_diag$Cramers_v))[1]

#Caption
cap_txt <- paste0(
  "Bars show standardized Pearson residuals from a chi-square test of independence on EDDXCount (PCSA × Diagnosis). ",
  "Residual = (Observed − Expected) / √Expected. ",
  "Bars = over-represented in PCSA = Yes",
  "Test: χ²(", chisq_diag$parameter, ") = ", round(chisq_diag$statistic, 0),
  ", p ", ifelse(chisq_diag$p.value < .001, "< .001", paste0("= ", signif(chisq_diag$p.value, 3))),
  if (is.finite(V_val)) paste0("; Cramér’s V = ", round(V_val, 2), ".") else "."
)

# Plot
diag_std <- as_tibble(as.table(chisq_diag$stdres)) %>%
  rename(PCSA = PrimaryCareShortageArea, Category = Category, stdres = n) %>%
  filter(PCSA == "Yes", stdres > 0) %>%     
  arrange(desc(stdres))
ggplot(diag_std, aes(x = reorder(Category, stdres), y = stdres)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
  coord_flip() +
  labs(
    title   = "Diagnoses Overrepresented in PCSA status",
    x       = "Diagnosis category",
    y       = "Standardized Pearson residual",
    fill    = NULL,
    caption = cap_txt
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )


#URBAN_RURAL
# Using: urbanrural_counts, chisq_ur 
ur_std <- as_tibble(as.table(chisq_ur$stdres)) %>%
  rename(PCSA = PrimaryCareShortageArea, UrbanRuralDesi = UrbanRuralDesi, stdres = n) %>%
  filter(PCSA == "Yes") %>%
  mutate(direction = ifelse(stdres > 0, "Over in PCSA = Yes", "Over in PCSA = No"))

V_ur  <- cramers_v(urbanrural_counts)
Vu    <- suppressWarnings(as.numeric(V_ur$Cramers_v))[1]
cap_ur <- paste0(
  "Standardized Pearson residuals from chi-square on EDDXCount (PCSA × Urban/Rural). ",
  "Positive bars = over-represented in PCSA = Yes; negative = over-represented in PCSA = No. ",
  "Test: χ²(", chisq_ur$parameter, ") = ", round(chisq_ur$statistic, 0),
  ", p ", ifelse(chisq_ur$p.value < .001, "< .001", paste0("= ", signif(chisq_ur$p.value, 3))),
  if (is.finite(Vu)) paste0("; Cramér’s V = ", round(Vu, 2), ".") else "."
)

ggplot(ur_std, aes(x = reorder(UrbanRuralDesi, stdres), y = stdres, fill = direction)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
  coord_flip() +
  labs(
    title   = "Urbanicity differences by PCSA status",
    x       = "Urban–Rural category",
    y       = "Standardized Pearson residual",
    fill    = NULL,
    caption = cap_ur
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

