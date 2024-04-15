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

setwd("/Users/suelim/Documents/Research/vr_city_driving/data")
df <- read_csv('04_data_analysis/00_all_data.csv')
names(df)[names(df) == 'billboard#_fixated'] <- 'fixated'
names(df)[names(df) == 'billboard#_fixated_count'] <- 'fixation_count'
names(df)[names(df) == 'billboard#_duration'] <- 'fixation_duration'
names(df)[names(df) == 'billboard_image_at_position'] <- 'billboard_name'
df_total_mani_checked <- df %>%
  filter(!subject %in% c("sub020", "sub017")) %>%
  mutate(gaze_behavior = case_when(
    fixation_count == 0 ~ "not_fixated",
    fixation_count == 1 ~ "fixated",
    fixation_count > 1 ~ "very_fixated",
    TRUE ~ NA_character_  # This line is optional, handles cases outside the specified conditions
  ))

df_total_mani_checked$position_index <- as.factor(df_total_mani_checked$position_index)
df_total_mani_checked$fixated <- as.factor(df_total_mani_checked$fixated)
df_total_mani_checked$condition <- as.factor(df_total_mani_checked$condition)
df_total_mani_checked$subject <- as.factor(df_total_mani_checked$subject)
df_total_mani_checked$billboard_name <- as.factor(df_total_mani_checked$billboard_name)
df_total_mani_checked$billboard_attention_level <- as.factor(df_total_mani_checked$billboard_attention_level)
df_total_mani_checked$gaze_behavior <- factor(df_total_mani_checked$gaze_behavior, levels = c("not_fixated", "fixated", "very_fixated"))

data <- 
  df_total_mani_checked %>%
  group_by(condition, billboard_attention_level, gaze_behavior) %>%
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
    color = "Visual Message Design via AI-Prompting") +
  facet_wrap(~condition) +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) + 
  scale_y_continuous(breaks = seq(floor(min(data$total_recall)), ceiling(max(data$total_recall)), by = 3))

ggplot(data, aes(x = gaze_behavior, y = total_recognition, color = billboard_attention_level)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = billboard_attention_level)) +
  labs(
    x = "Gaze Behavior",
    y = "# of Billboards Recognized",
    color = "Visual Message Design via AI-Prompting") +
  facet_wrap(~condition) +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) + 
  scale_y_continuous(breaks = seq(floor(min(data$total_recognition)), ceiling(max(data$total_recognition)), by = 10)) + 
  ylim(0,105)

data2 <- 
  df_total_mani_checked %>%
  group_by(condition, billboard_attention_level) %>%
  summarise(
    total_recall = sum(recall, na.rm = TRUE),
    total_recognition = sum(recognition, na.rm = TRUE)
  )

ggplot(data2, aes(x = condition, y = total_recall, color = billboard_attention_level)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = billboard_attention_level)) +
  labs(
    x = "Condition",
    y = "# of Billboards Recalled",
    color = "Visual Message Design via AI-Prompting") +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) + 
  ylim(0,60)
  
ggplot(data2, aes(x = condition, y = total_recognition, color = billboard_attention_level)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = billboard_attention_level)) +
  labs(
    x = "Condition",
    y = "# of Billboards Recognized",
    color = "Visual Message Design via AI-Prompting") +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1"))

model1 <-
  glmer(recall ~ fixation_duration + condition + (1|subject) + (1|billboard_name), data = df_total_mani_checked, family = "binomial")
Anova(model1)
model1_predict <-
  emmeans(model1, ~ fixation_duration + condition, at = list(fixation_duration = c(1, 2, 3, 4, 5, 6, 7)))
model1_predict_regrid <- regrid(model1_predict, transform = "response")
model1_predict_df <- as.data.frame(model1_predict_regrid)
ggplot(model1_predict_df , aes(x = fixation_duration, y = prob, color = condition)) +
  geom_point(size = 1.25) +
  geom_line()+
  labs(
    x = "Fixation Duration",
    y = "Predicted Likelihood of Recall"
  ) +
  theme_classic(base_size = 20) + 
  ylim(0,1)

model2 <-
  glmer(recognition ~ fixation_duration + condition + (1|subject) + (1|billboard_name), data = df_total_mani_checked, family = "binomial")
Anova(model2)
model2_predict <-
  emmeans(model2, ~ fixation_duration + condition, at = list(fixation_duration = c(1, 2, 3, 4, 5, 6, 7)))
model2_predict_regrid <- regrid(model2_predict, transform = "response")
model2_predict_df <- as.data.frame(model2_predict_regrid)
ggplot(model2_predict_df , aes(x = fixation_duration, y = prob, color = condition)) +
  geom_point(size = 1.25) +
  geom_line()+
  labs(
    x = "Fixation Duration",
    y = "Predicted Likelihood of Recognition"
  ) +
  theme_classic(base_size = 20) +
  ylim(0,1)

model3 <-
  lmer(fixation_duration ~ condition + position_index + billboard_attention_level+ (1|subject) + (1|billboard_name), data = df_total_mani_checked)
Anova(model3)
summary(model3)
model4 <-
  lmer(fixation_duration ~ condition * position_index * billboard_attention_level+ (1|subject) + (1|billboard_name), data = df_total_mani_checked)
Anova(model4)
lrtest(model3, model4)

# df_survey <- read_csv('02_qualtrics_data/recognition_survey_vr_city_cleaned.csv')
# df_survey_filter <- df_survey %>%
#   filter(!Subject %in% c("sub020", "sub017"))
# manipulation_check <- df_survey_filter %>%
#   select(ends_with("_a")) %>%
#   summarise(across(everything(), ~mean(.x == 1) * 100))
# mani_check_pivot <- manipulation_check %>%
#   pivot_longer(
#     cols = everything(),
#     names_to = "billboard_name",
#     values_to = "percent_pass"
#   ) %>%
#   mutate(billboard_name = str_remove(billboard_name, "_a$")) %>%
#   mutate(billboard_name = case_when(
#     billboard_name == "healthsunprotect" ~ "health_sunprotection",
#     billboard_name == "health_mentalhealt" ~ "health_mentalhealth",
#     TRUE ~ billboard_name  # Keep all other values unchanged
#   ))
# print(mani_check_pivot, n=40)
# 
# mani_check_pivot %>%
#   filter(percent_pass >= 75) %>%
#   nrow()
# 
# mani_checked_billboards <-
#   mani_check_pivot %>%
#   filter(percent_pass >= 75)
# 
# df_total_mani_checked_mani_checked <-
#   df_total_mani_checked[df_total_mani_checked$billboard_name %in% mani_checked_billboards$billboard_name, ]
# 
# # not_in_mani_checked <- setdiff(df_total_mani_checked_mani_checked$billboard_name, mani_checked_billboards$billboard_name)
# # not_in_total_mani_checked <- setdiff(mani_checked_billboards$billboard_name, df_total_mani_checked_mani_checked$billboard_name)
# # print(not_in_mani_checked)
# # print(not_in_total_mani_checked)
# 
# 
# df_total_mani_checked_mani_checked$position_index <- as.factor(df_total_mani_checked_mani_checked$position_index)
# df_total_mani_checked_mani_checked$fixated <- as.factor(df_total_mani_checked_mani_checked$fixated)
# df_total_mani_checked_mani_checked$condition <- as.factor(df_total_mani_checked_mani_checked$condition)
# df_total_mani_checked_mani_checked$subject <- as.factor(df_total_mani_checked_mani_checked$subject)
# df_total_mani_checked_mani_checked$billboard_name <- as.factor(df_total_mani_checked_mani_checked$billboard_name)
# df_total_mani_checked_mani_checked$billboard_attention_level <- as.factor(df_total_mani_checked_mani_checked$billboard_attention_level)
# df_total_mani_checked_mani_checked$gaze_behavior <- factor(df_total_mani_checked_mani_checked$gaze_behavior, levels = c("not_fixated", "fixated", "very_fixated"))
# 
# data01 <-
#   df_total_mani_checked_mani_checked %>%
#   group_by(condition, billboard_attention_level, gaze_behavior) %>%
#   summarise(
#     total_recall = sum(recall, na.rm = TRUE),
#     total_recognition = sum(recognition, na.rm = TRUE)
#   )
# 
# ggplot(data01, aes(x = gaze_behavior, y = total_recall, color = billboard_attention_level)) +
#   geom_point(size=5) +
#   geom_line(size=1, aes(group = billboard_attention_level)) +
#   labs(
#     x = "Gaze Behavior",
#     y = "# of Billboards Recalled",
#     color = "Visual Message Design via AI-Prompting") +
#   facet_wrap(~condition) +
#   theme_classic(base_size = 20) +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(ncol = 1)) +
#   scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
#                      labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) +
#   scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) +
#   scale_y_continuous(breaks = seq(floor(min(data$total_recall)), ceiling(max(data$total_recall)), by = 3))
# 
# ggplot(data01, aes(x = gaze_behavior, y = total_recognition, color = billboard_attention_level)) +
#   geom_point(size=5) +
#   geom_line(size=1, aes(group = billboard_attention_level)) +
#   labs(
#     x = "Gaze Behavior",
#     y = "# of Billboards Recognized",
#     color = "Visual Message Design via AI-Prompting") +
#   facet_wrap(~condition) +
#   theme_classic(base_size = 20) +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(ncol = 1)) +
#   scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
#                      labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) +
#   scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) +
#   scale_y_continuous(breaks = seq(floor(min(data$total_recognition)), ceiling(max(data$total_recognition)), by = 10)) +
#   ylim(0,105)
# 
# data02 <-
#   df_total_mani_checked_mani_checked %>%
#   group_by(condition, billboard_attention_level) %>%
#   summarise(
#     total_recall = sum(recall, na.rm = TRUE),
#     total_recognition = sum(recognition, na.rm = TRUE)
#   )
# 
# ggplot(data02, aes(x = condition, y = total_recall, color = billboard_attention_level)) +
#   geom_point(size=5) +
#   geom_line(size=1, aes(group = billboard_attention_level)) +
#   labs(
#     x = "Condition",
#     y = "# of Billboards Recalled",
#     color = "Visual Message Design via AI-Prompting") +
#   theme_classic(base_size = 20) +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(ncol = 1)) +
#   scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
#                      labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) +
#   scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1")) +
#   ylim(0,60)
# 
# ggplot(data02, aes(x = condition, y = total_recognition, color = billboard_attention_level)) +
#   geom_point(size=5) +
#   geom_line(size=1, aes(group = billboard_attention_level)) +
#   labs(
#     x = "Condition",
#     y = "# of Billboards Recognized",
#     color = "Visual Message Design via AI-Prompting") +
#   theme_classic(base_size = 20) +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(ncol = 1)) +
#   scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
#                      labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) +
#   scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1"))
# 
# model01 <-
#   glmer(recall ~ fixation_duration + condition + (1|subject) + (1|billboard_name), data = df_total_mani_checked, family = "binomial")
# Anova(model01)
# model01_predict <-
#   emmeans(model01, ~ fixation_duration + condition, at = list(fixation_duration = c(1, 2, 3, 4, 5, 6, 7)))
# model01_predict_regrid <- regrid(model01_predict, transform = "response")
# model01_predict_df <- as.data.frame(model01_predict_regrid)
# ggplot(model01_predict_df , aes(x = fixation_duration, y = prob, color = condition)) +
#   geom_point(size = 1.25) +
#   geom_line()+
#   labs(
#     x = "Fixation Duration",
#     y = "Predicted Likelihood of Recall"
#   ) +
#   theme_classic(base_size = 20) +
#   ylim(0,1)
# 
# model02 <-
#   glmer(recognition ~ fixation_duration + condition + (1|subject) + (1|billboard_name), data = df_total_mani_checked, family = "binomial")
# Anova(model02)
# model02_predict <-
#   emmeans(model02, ~ fixation_duration + condition, at = list(fixation_duration = c(1, 2, 3, 4, 5, 6, 7)))
# model02_predict_regrid <- regrid(model02_predict, transform = "response")
# model02_predict_df <- as.data.frame(model02_predict_regrid)
# ggplot(model02_predict_df , aes(x = fixation_duration, y = prob, color = condition)) +
#   geom_point(size = 1.25) +
#   geom_line()+
#   labs(
#     x = "Fixation Duration",
#     y = "Predicted Likelihood of Recognition"
#   ) +
#   theme_classic(base_size = 20) +
#   ylim(0,1)
# 
# model03 <-
#   lmer(fixation_duration ~ condition + position_index + billboard_attention_level+ (1|subject) + (1|billboard_name), data = df_total_mani_checked)
# Anova(model03)
# summary(model03)
# model04 <-
#   lmer(fixation_duration ~ condition * position_index * billboard_attention_level+ (1|subject) + (1|billboard_name), data = df_total_mani_checked)
# Anova(model04)
# lrtest(model03, model04)
# 
# Anova(model1)
# Anova(model01)
# Anova(model2)
# Anova(model02)
# Anova(model3)
# Anova(model03)
# Anova(model4)
# Anova(model04)
