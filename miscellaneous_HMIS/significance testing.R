library(tidyverse)
library(ggridges)
library(patchwork)
library(broom)
# library(brms)
library(rstatix)
library(ggpubr)


assessment_data <- read.csv(file.choose())

clean_assessment_data <- assessment_data %>%
  filter(Ethnicity == "Hispanic/Latin(a)(o)(x) (HUD)" |
           (!str_detect(Race, "Client") &
              !str_detect(Race, "Data"))) %>%
  mutate(group = as.factor(if_else(Race == "White (HUD)" &
                                     Ethnicity != "Hispanic/Latin(a)(o)(x) (HUD)",
                                   "White",
                                   "BIPOC")))

factor <- "Basic.Needs"

# Make a custom theme
# I'm using Asap Condensed; download from 
# https://fonts.google.com/specimen/Asap+Condensed
theme_fancy <- function() {
  theme_minimal(base_family = "Asap Condensed") +
    theme(panel.grid.minor = element_blank())
}

eda_boxplot <- ggplot(clean_assessment_data, aes(x = group, y = Basic.Needs, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0288b7", "#a90010"), guide = FALSE) + 
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  labs(x = NULL, y = "Basic.Needs") +
  theme_fancy()

eda_histogram <- ggplot(clean_assessment_data, aes(x = Basic.Needs, fill = group)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_manual(values = c("#0288b7", "#a90010"), guide = FALSE) + 
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(y = "Count", x = "Basic.Needs") +
  facet_wrap(~ group, nrow = 2) +
  theme_fancy() +
  theme(panel.grid.major.x = element_blank())

eda_ridges <- ggplot(clean_assessment_data, aes(x = Basic.Needs, y = fct_rev(group), fill = group)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 3, color = "white") + 
  scale_fill_manual(values = c("#0288b7", "#a90010"), guide = FALSE) + 
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  labs(x = "Basic.Needs", y = NULL,
       subtitle = "White line shows median Basic.Needs") +
  theme_fancy()

(eda_boxplot | eda_histogram) / 
  eda_ridges + 
  plot_annotation(title = "Do white folks score higher on Basic.Needs than BIPOC folks?",
                  subtitle = "Uploaded data from OR BoS",
                  theme = theme(text = element_text(family = "Asap Condensed"),
                                plot.title = element_text(face = "bold",
                                                          size = rel(1.5))))


mydata.long <- clean_assessment_data %>%
  select(c(group, colnames(select_if(clean_assessment_data, is.numeric)))) %>%
  pivot_longer(-group, names_to = "variables", values_to = "value")

stat.test <- mydata.long %>%
  group_by(variables) %>%
  t_test(value ~ group, 
         p.adjust.method = "none",
         detailed = TRUE
         ) %>%
  ##  refer to https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-2-8
  # adjust_pvalue(method = "none") %>%
  add_significance()
stat.test

# Create the plot
myplot <- ggboxplot(
  mydata.long, x = "group", y = "value",
  fill = "group", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables)
# Add statistical test p-values
stat.test <- stat.test %>% add_xy_position(x = "group")
myplot + stat_pvalue_manual(stat.test, label = "p.signif")


min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

numeric_cols <- colnames(select_if(clean_assessment_data, is.numeric))

test <- clean_assessment_data %>%
  mutate(across(
    all_of(numeric_cols),
    ~ min_max_norm(.))) %>%
  select(c(group, all_of(numeric_cols))) %>%
  pivot_longer(-group, names_to = "variables", values_to = "value") %>%
  filter(!is.na(value))

stat.test <- test %>%
  group_by(variables) %>%
  t_test(value ~ group, 
         p.adjust.method = "none",
         detailed = TRUE
  ) %>%
  ##  refer to https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-2-8
  # adjust_pvalue(method = "none") %>%
  add_significance()
stat.test

myplot <- ggboxplot(
  test, x = "group", y = "value",
  fill = "group", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables)
