library(readr)
pcsa <- read_csv("Downloads/Primary_Care_Shortage_Area_(PCSA) (1).csv")

library(dplyr)
library(ggplot2)

#add ratio and row_id; compute undefined count
pcsa <- pcsa %>%
  mutate(
    ratio  = ifelse(EST_Provid > 0, EST_FNPPA / EST_Provid, NA_real_),
    row_id = row_number())

undef_n <- sum(is.na(pcsa$ratio))  # count undefined
pcsa_plot <- pcsa %>%
  filter(!is.na(ratio)) %>%
  group_by(PCSA) %>%
  arrange(ratio, .by_group = TRUE) %>%
  mutate(order_id = row_number()) %>%
  ungroup()

# plot ratio
ggplot(pcsa_plot, aes(x = order_id, y = ratio, fill = PCSA)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ PCSA, ncol = 2, scales = "free_y") +   # side-by-side
  labs(title = "Ratio of Midlevel to All Providers", x = "Row (MSSA areas)", 
       y = "FNPPA / EST_PROVID", caption = paste("Excluded as undefined (EST_PROVID = 0):", undef_n)
  ) + theme_minimal() + theme(axis.text.x = element_blank())



