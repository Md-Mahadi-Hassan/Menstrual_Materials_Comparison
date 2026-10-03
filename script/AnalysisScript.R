# Loading Libraries
library(tidyverse)
library(gtsummary)
library(gt)

# Visualization
library(flextable)
library(ggpubr)
library(ggthemes)
library(cowplot)

# Loading data
data <- readxl::read_excel("data/data_mkpn.xlsx", sheet = 2)

# String -> Factors
data <- data |> 
  mutate_if(is.character, as.factor)

# Checking conversion
glimpse(data)

# ---- 1. Tables ----
# Table 01: Demographics and Menstrual Features

data |> 
  select(2:23) |> 
  tbl_summary(
    type = everything() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no") |> 
  as_gt() |> 
  gtsave("table/All.docx")

## ---- Linear Regression ----

data |>
  select(Menstrual_material, RI) |>
  tbl_uvregression(
    y = RI,
    method = lm
  ) |>
  bold_p(t = 0.05)

## ---- End ----

# Table 02: Menstrual characteristics

data |> 
  select(10:18) |> 
  tbl_summary(
    type = everything() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) |> 
  as_gt() |> 
  gtsave("table/Menstrual_characteristics.docx")

# Table 03: Association with Demographics

data |> 
  select(1:10) |> 
  tbl_summary(by = `Menstrual Hygiene Materials`) |>
  add_p() |> 
  bold_p() |> 
  as_gt() |> 
  gtsave("table/Demographics_Materials.docx")

# Table 04: Association with Menstrual Features

data |> 
  select(10:18) |> 
  tbl_summary(
    type = everything() ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no",
    by = `Menstrual Hygiene Materials`) |>
  add_p() |> 
  bold_p() |> 
  as_gt() |> 
  gtsave("table/Menstruation_Materials.docx")

# Table 05: T-test
data |> 
  select(Menstrual_Hygiene_Materials, 55:60) |> 
  tbl_summary(
    by = Menstrual_Hygiene_Materials,
    statistic = all_continuous() ~ "{mean} ± {sd}"
  ) |> 
  add_difference(test = all_continuous() ~ "t.test") |> 
  as_gt() |> 
  gtsave("table/T-test.docx")

# Calculate t-values for the above table
results <- lapply(names(data)[55:60], function(var) {
  formula <- as.formula(paste(var, "~ Menstrual_Hygiene_Materials"))
  test <- t.test(formula, data = data)
  tibble(
    variable = var,
    t_value = round(test$statistic, 2),
    p_value = round(test$p.value, 4)
  )
}) |> bind_rows()

# Regression
# Factor Relevel
data$Menstrual_Hygiene_Materials <- relevel(data$Menstrual_Hygiene_Materials, ref = "Reusable")

# Table 06: Socio-demographic factors associated with Disposable Materials Use
# Model
sd_mv <- glm(Menstrual_Hygiene_Materials ~ Age + Educational_status + Permanent_residence + Fathers_educational_status + Fathers_occupation + Mothers_educational_status + Mothers_occupation
             + Family_structure + Family_condition, data = data, family = binomial(link = "logit"))

# Regression table
sd_mv |> 
  tbl_regression(exponentiate = T) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("table/Demographic_regression.docx")

# Table 07: Menstrual factors associated with Disposable Materials Use
# Model
mc_mv <- glm(Menstrual_Hygiene_Materials ~ Menstrual_information_before_first_bleeding + First_menstrual_bleeding + Menstrual_complication + Family_history_of_dysmenorrhea + Menstrual_bleeding_duration + Medication_for_menstruation,
             data = data, family = binomial(link = "logit"))

# Regression table
mc_mv |> 
  tbl_regression(exponentiate = T) |>
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("table/Menstrual_regression.docx")

# ---- 2. Bar plot - Main Source of Information ----
# Wide -> Long Data
data_long <- data |> 
  pivot_longer(cols = c(`Main source of menstrual information`),
               names_to = "Variable",
               values_to = "Level")

# Calculating Percentages for Labels
data_long <- data_long |> 
  group_by(Variable, Level) |> 
  summarise(count = n()) |> 
  mutate(percentage = (count / sum(count)) * 100)  # Calculate percentages

# Customized Plot
ggplot(data_long, aes(x = Level, fill = Level)) +
  geom_bar(stat = "identity", aes(y = count), color = "black", width = 0.8) +  # wider bars to reduce space
  geom_text(aes(y = count, label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 4) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(color = "black"),         # x-axis label text color
    axis.text.y = element_text(color = "black"),         # y-axis for consistency
    axis.ticks.length = unit(0, "pt")                    # optional: remove tick space
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # no space between bars and x-axis
  labs(x = "Main Source of Menstrual Information", y = "Number of Participants")

# Saving the plot
ggsave("figure/Source_of_information.jpg", width = 15, height = 8, dpi = 1000)

# ---- 3. Box plot for Comparison ----
# 3.1. Material and Home Environment Needs

fig1 <- ggboxplot(data, "Menstrual_Hygiene_Materials", "Material_and_Home_Environment_Needs", fill = "Menstrual_Hygiene_Materials",
          palette = c("#31a354","#de2d26"),
          xlab = "", ylab = "Material and Home Environment Needs")+ 
  geom_jitter(width = 0.2, alpha = 1, show.legend = FALSE)+
  geom_hline(yintercept = mean(data$Material_and_Home_Environment_Needs), linetype = 2)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 9))

# 3.2 Transport and Educational Environment Needs

fig2 <- ggboxplot(data, "Menstrual_Hygiene_Materials", "Transport_and_Educational_Environment_Needs", fill = "Menstrual_Hygiene_Materials",
          palette = c("#31a354","#de2d26"),
          xlab = "", ylab = "Transport and Educational Environment Needs")+ 
  geom_jitter(width = 0.2, alpha = 1, show.legend = FALSE)+
  geom_hline(yintercept = mean(data$Transport_and_Educational_Environment_Needs), linetype = 2)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 9))

# 3.3 Material_Reliability_Concerns
fig3 <- ggboxplot(data, "Menstrual_Hygiene_Materials", "Material_Reliability_Concerns", fill = "Menstrual_Hygiene_Materials",
          palette = c("#31a354","#de2d26"),
          xlab = "", ylab = "Material Reliability Concerns")+ 
  geom_jitter(width = 0.2, alpha = 1, show.legend = FALSE)+
  geom_hline(yintercept = mean(data$Material_Reliability_Concerns), linetype = 2)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 9))

# 3.4 Change and Disposal Insecurity
fig4 <- ggboxplot(data, "Menstrual_Hygiene_Materials", "Change_and_Disposal_Insecurity", fill = "Menstrual_Hygiene_Materials",
          palette = c("#31a354","#de2d26"),
          xlab = "", ylab = "Change and Disposal Insecurity")+ 
  geom_jitter(width = 0.2, alpha = 1, show.legend = FALSE)+
  geom_hline(yintercept = mean(data$Change_and_Disposal_Insecurity), linetype = 2)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 9))

# Combined plot
plot_all <- plot_grid(fig1, fig2, fig3, fig4, nrow = 2)

plot_all

# Save
ggsave("figure/Needs_Scores.jpg", units="in", width=15, height=8, dpi=1000)


# 3.5 Overall Score
ggboxplot(data, "Menstrual_Hygiene_Materials", "Overall_score", fill = "Menstrual_Hygiene_Materials",
          palette = c("#31a354","#de2d26"),
          xlab = "Menstrual Hygiene Materials", ylab = "Overall Score")+
  geom_jitter(width = 0.2, alpha = 1, show.legend = T)+
  geom_hline(yintercept = mean(data$Overall_score), linetype = 2)+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

# Saving the plot
ggsave("figure/Overall_Score.jpg", units="in", width=10, height=6, dpi=1000)










































