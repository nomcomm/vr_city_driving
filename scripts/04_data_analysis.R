rm(list = ls())
cat("\014")
#dev.off()
library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
#library(WebPower)
library(readr)
library(lmerTest)
library(lmtest)
library(lme4)
library(emmeans)
library(performance)
library(car)
library(stringr)
#library(psych)

setwd("/Users/suelim/Documents/Research/vr_city_driving/data/04_data_analysis")
df_total <- read_csv('00_all_data.csv')
names(df_total)[names(df_total) == 'billboard#_fixated'] <- 'fixated'
names(df_total)[names(df_total) == 'billboard#_fixated_count'] <- 'fixation_count'
names(df_total)[names(df_total) == 'billboard#_duration'] <- 'fixation_duration'

df_total <- df_total %>%
  mutate(gaze_behavior = case_when(
    fixation_count == 0 ~ "not_fixated",
    fixation_count == 1 ~ "fixated",
    fixation_count > 1 ~ "very_fixated",
    TRUE ~ NA_character_  # This line is optional, handles cases outside the specified conditions
  ))

df_total$position_index <- as.factor(df_total$position_index)
df_total$fixated <- as.factor(df_total$fixated)
df_total$condition <- as.factor(df_total$condition)
df_total$subject <- as.factor(df_total$subject)
df_total$billboard_image_at_position <- as.factor(df_total$billboard_image_at_position)
df_total$billboard_attention_level <- as.factor(df_total$billboard_attention_level)
df_total$gaze_behavior <- factor(df_total$gaze_behavior, levels = c("not_fixated", "fixated", "very_fixated"))

model1 <-
  glmer(recall ~ fixation_duration + (1|subject), data = df_total, family = "binomial")
summary(model1)
model1_predict <-
  emmeans(model1, ~ fixation_duration, at = list(fixation_duration = c(1, 2, 3, 4, 5, 6, 7)))
model1_predict_regrid <- regrid(model1_predict, transform = "response")
model1_predict_df <- as.data.frame(model1_predict_regrid)
ggplot(model1_predict_df , aes(x = fixation_duration, y = prob)) +
  geom_line(size = 1.25) +
  labs(
    x = "Fixation Duration",
    y = "Predicted Likelihood of Recall"
  ) +
  theme_classic(base_size = 20) +
  ylim(0,1)

model2 <-
  glmer(recognition ~ fixation_duration + (1|subject), data = df_total, family = "binomial")
summary(model2)
model2_predict <-
  emmeans(model2, ~ fixation_duration, at = list(fixation_duration = c(1, 2, 3, 4, 5, 6, 7)))
model2_predict_regrid <- regrid(model2_predict, transform = "response")
model2_predict_df <- as.data.frame(model2_predict_regrid)
ggplot(model2_predict_df , aes(x = fixation_duration, y = prob)) +
  geom_line(size = 1.25) +
  labs(
    x = "Fixation Duration",
    y = "Predicted Likelihood of Recognition"
  ) +
  theme_classic(base_size = 20) +
  ylim(0,1)

model3 <-
  glm(recall ~ position_index, data = df_total, family = "binomial")
summary(model3)
Anova(model3)

model4 <-
  glm(recognition ~ position_index, data = df_total, family = "binomial")
summary(model4)
Anova(model4)


model01 <-
  lmer(fixation_duration ~ billboard_attention_level + (1|subject), data = df_total)
summary(model01)

model02 <-
  lmer(fixation_duration ~ condition + (1|subject), data = df_total)
summary(model02)

model03 <-
  lmer(fixation_duration ~ position_index + (1|subject), data = df_total)
summary(model03)
Anova(model03)

model04 <-
  lmer(fixation_duration ~ billboard_attention_level*condition + (1|subject), data = df_total)
summary(model04)
model04_predict <-
  emmeans(model04, ~ billboard_attention_level*condition)
model04_predict_df <- as.data.frame(model04_predict)
ggplot(model04_predict_df, aes(x = condition, y = emmean, color= billboard_attention_level)) +
  geom_point(size=5, position = position_dodge(0.3)) +
  geom_line(aes(group = billboard_attention_level), position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    size=1,
    position = position_dodge(0.3)) +
  labs(
    x = "Condition",
    y = "Fixation Duration",
    color = "Attention Level") +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.8, 0), # This tries to center the legend at the bottom inside the plot
        legend.justification = c(0.5, 0), # Adjusts the anchor point of the legend
        legend.box.margin = margin(0, 0, 20, 0), # Adjusts the margin around the legend box, potentially useful for fine-tuning
        legend.margin = margin(-10, 0, 0, 0) # Negative values can push the legend into the plot area
  ) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention", "more_attention" = "More Attention")) +
  scale_x_discrete(labels=c(Count_Sales_Signs = "Count Sales Signs", Free_Viewing = "Free Viewing")) +
  ylim(0,2)

ggplot(model04_predict_df, aes(x = billboard_attention_level, y = emmean, color= condition)) +
  geom_point(size=5, position = position_dodge(0.3)) +
  geom_line(aes(group = condition), position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    size=1,
    position = position_dodge(0.3)) +
  labs(
    x = "Attention Level",
    y = "Fixation Duration",
    color = "Condition") +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.8, 0), # This tries to center the legend at the bottom inside the plot
        legend.justification = c(0.5, 0), # Adjusts the anchor point of the legend
        legend.box.margin = margin(0, 0, 20, 0), # Adjusts the margin around the legend box, potentially useful for fine-tuning
        legend.margin = margin(-10, 0, 0, 0) # Negative values can push the legend into the plot area
  ) +
  scale_color_manual(values = c("Count_Sales_Signs" = "darkgreen", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sign Counting", "Free_Viewing" = "Free Viewing")) +
  scale_x_discrete(labels=c(less_attention = "Less Attention", more_attention = "More Attention")) +
  ylim(0,2)
emmeans(model04, pairwise~ billboard_attention_level*condition, adjust="none")

model001 <-
  glm(fixated ~ position_index , data = df_total, family = "binomial")
summary(model001)
model001_predict <-
  emmeans(model001, ~ position_index)
model001_predict_regrid <- regrid(model001_predict, transform = "response")
model001_predict_df <- as.data.frame(model001_predict_regrid)
ggplot(model001_predict_df, aes(x = position_index, y = prob)) +
  geom_point() +
  labs(
    x = "Position",
    y = "Predicted Likelihood of Fixation"
  ) +
  theme_classic(base_size = 20) +
  ylim(0,1)

model002 <-
  glmer(fixated ~ billboard_attention_level + (1|subject), data = df_total, family = "binomial")
summary(model002)

model003 <-
  glmer(fixated ~ condition + (1|subject), data = df_total, family = "binomial")
summary(model003)
model003_predict <-emmeans(model003, ~ condition)
model003_predict_regrid <- regrid(model003_predict, transform = "response")
model003_predict_df <- as.data.frame(model003_predict_regrid)
ggplot(model003_predict_df, aes(x = condition, y = prob)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.2,
    position = position_dodge(0.3)) +
  labs(
    x = "Condition",
    y = "Predicted Likelihood of Fixation") +
  theme_classic(base_size = 20) +
  ylim(0,1)

model004 <-
  glmer(fixated ~ billboard_attention_level * condition + (1|subject), data = df_total, family = "binomial")
summary(model004)

colnames(data)
data <- 
  df_total %>%
  group_by(billboard_attention_level, gaze_behavior) %>%
  summarise(
    total_recall = sum(recall, na.rm = TRUE),
    total_recognition = sum(recognition, na.rm = TRUE)
  )

ggplot(data, aes(x = gaze_behavior, y = total_recall, color = billboard_attention_level)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = billboard_attention_level)) +
  labs(
    x = "Gaze Behavior",
    y = "# of Billboards Recalled",
    color = "Attention Level") +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.8, 0), # This tries to center the legend at the bottom inside the plot
        legend.justification = c(0.5, 0), # Adjusts the anchor point of the legend
        legend.box.margin = margin(0, 0, 20, 0), # Adjusts the margin around the legend box, potentially useful for fine-tuning
        legend.margin = margin(-10, 0, 0, 0) # Negative values can push the legend into the plot area
  ) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention", "more_attention" = "More Attention")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) + 
  scale_y_continuous(breaks = seq(floor(min(data$total_recall)), ceiling(max(data$total_recall)), by = 3))

ggplot(data, aes(x = gaze_behavior, y = total_recognition, color = billboard_attention_level)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = billboard_attention_level)) +
  labs(
    x = "Gaze Behavior",
    y = "# of Billboards Recognition",
    color = "Attention Level"
  ) +
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.8, 0),  # This tries to center the legend at the bottom inside the plot
        legend.justification = c(0.5, 0),  # Adjusts the anchor point of the legend
        legend.box.margin = margin(0, 0, 20, 0),  # Adjusts the margin around the legend box, potentially useful for fine-tuning
        legend.margin = margin(-10, 0, 0, 0)  # Negative values can push the legend into the plot area
  ) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention", "more_attention" = "More Attention")) +
  scale_x_discrete(labels = c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) +
  scale_y_continuous(breaks = seq(floor(min(data$total_recognition)), ceiling(max(data$total_recognition)), by = 10)) + 
  ylim(0,50)
  