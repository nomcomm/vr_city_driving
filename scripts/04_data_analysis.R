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
library(glmmTMB)
#library(psych)

setwd("/Users/suelim/Documents/Research/vr_city_driving/data")
df <- read_csv('04_data_analysis/00_all_data.csv')
names(df)[names(df) == 'billboard#_fixated'] <- 'fixated'
names(df)[names(df) == 'billboard#_fixated_count'] <- 'fixation_count'
names(df)[names(df) == 'billboard#_duration'] <- 'fixation_duration'
names(df)[names(df) == 'billboard_image_at_position'] <- 'billboard_name'

df_total_mani_checked <- df %>%
  filter(!subject %in% c("sub020", "sub017")) %>%
  mutate(
    median_fixation = median(fixation_count, na.rm = TRUE),  # Calculate median of fixation_count
    gaze_behavior = case_when(
      fixation_count == 0 ~ "not_fixated",
      fixation_count == median_fixation ~ "fixated",  # Use the median for "fixated"
      fixation_count > median_fixation ~ "very_fixated",
      TRUE ~ NA_character_  # This line is optional, handles cases outside the specified conditions
    )
  )


df_total_mani_checked$position_index <- as.factor(df_total_mani_checked$position_index)
df_total_mani_checked$fixated <- as.factor(df_total_mani_checked$fixated)
df_total_mani_checked$condition <- as.factor(df_total_mani_checked$condition)
df_total_mani_checked$subject <- as.factor(df_total_mani_checked$subject)
df_total_mani_checked$billboard_name <- as.factor(df_total_mani_checked$billboard_name)
df_total_mani_checked$billboard_attention_level <- as.factor(df_total_mani_checked$billboard_attention_level)
df_total_mani_checked$gaze_behavior <- factor(df_total_mani_checked$gaze_behavior, levels = c("not_fixated", "fixated", "very_fixated"))

#overview data visualization
data <- 
  df_total_mani_checked %>%
  group_by(condition, gaze_behavior) %>%
  summarise(
    total_recall = sum(recall, na.rm = TRUE),
    total_recognition = sum(recognition, na.rm = TRUE)
  )

ggplot(data, aes(x = gaze_behavior, y = total_recall, color = condition)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = condition)) +
  #facet_wrap(~condition) +
  theme_classic(base_size = 20) +
  #theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = Median", very_fixated = "Fixation > Median")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks = seq(0, 60, by = 10)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

ggplot(data, aes(x = gaze_behavior, y = total_recognition, color = condition)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = condition)) +
  #facet_wrap(~condition) +
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = Median", very_fixated = "Fixation > Median")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 225), breaks = seq(0, 225, by = 25)) +  # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

data2 <- 
  df_total_mani_checked %>%
  group_by(position_index, condition) %>%
  summarise(
    total_gaze_duration = sum(fixation_duration, na.rm = TRUE)
  )

ggplot(data2, aes(x = position_index, y = total_gaze_duration, color = condition)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = condition)) +
  #facet_wrap(~condition) +
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks = seq(0, 60, by = 10)) +  # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible


ggplot(data2, aes(x = condition, y = total_recognition, color = billboard_attention_level)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = billboard_attention_level)) +
  theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("less_attention" = "darkgreen", "more_attention" = "black"),
                     labels = c("less_attention" = "Less Attention-Grabbing", "more_attention" = "More Attention-Grabbing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = 1", very_fixated = "Fixation > 1"))

#hypothesis and research question testing
recall <-
  glmer(recall ~ fixation_duration+condition + (1|billboard_name), data = df_total_mani_checked, family = "binomial")
Anova(recall)
summary(recall)
performance(recall)
recall_predict1 <-
  emmeans(recall, ~ fixation_duration + condition, at = list(fixation_duration = c(0, 1, 2, 3, 4, 5, 6, 7)))
recall_predict1_regrid <- regrid(recall_predict1, transform = "response")
recall_predict1_df <- as.data.frame(recall_predict1_regrid)
ggplot(recall_predict1_df , aes(x = fixation_duration, y = prob, color = condition)) +
  geom_point(size = 5) +
  geom_line(size = 1)+
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8), breaks = seq(0, 7, by = 1)) +  # Prevents the y-axis from extending below zero
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = .25)) +  # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

recognition <-
  glmer(recognition ~ fixation_duration+condition + (1|billboard_name), data = df_total_mani_checked, family = "binomial")
Anova(recognition)
summary(recognition)
performance(recognition)
recognition_predict1 <-
  emmeans(recognition, ~ fixation_duration + condition, at = list(fixation_duration = c(0, 1, 2, 3, 4, 5, 6, 7)))
recognition_predict1_regrid <- regrid(recognition_predict1, transform = "response")
recognition_predict1_df <- as.data.frame(recognition_predict1_regrid)
ggplot(recognition_predict1_df , aes(x = fixation_duration, y = prob, color = condition)) +
  geom_point(size = 5) +
  geom_line(size = 1)+
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8), breaks = seq(0, 7, by = 1)) +  # Prevents the y-axis from extending below zero
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = .25)) +  # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

gazeduration <- glmmTMB(fixation_duration ~ position_index * condition *billboard_attention_level +
                          (1 | subject) + (1 | billboard_name),
                        ziformula = ~ 1,  # Formula for zero-inflation component
                        family = ziGamma(link = "log"),  # Specify zero-inflated Gamma family
                        data = df_total_mani_checked)
Anova(gazeduration, type="3")
summary(gazeduration)
gazeduration_predict1 <-
  emmeans(gazeduration, ~ position_index * condition)
gazeduration_predict1_regrid <- regrid(gazeduration_predict1, transform = "response")
gazeduration_predict1_df <- as.data.frame(gazeduration_predict1_regrid)

ggplot(gazeduration_predict1_df, aes(x = position_index, y = response, color = condition)) +
  geom_point(size = 5) +
  geom_line(size = 1, aes(group = condition))+
  #facet_wrap(~condition, labeller = labeller(condition = c(Count_Sales_Signs = "Sales Sign Counting", Free_Viewing = "Free Viewing"))) +
  theme_classic(base_size = 20) +
  #theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3), breaks = seq(0, 3, by = 1)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

gazeduration_predict2 <-
  emmeans(gazeduration, ~ position_index * billboard_attention_level)
gazeduration_predict2_regrid <- regrid(gazeduration_predict2, transform = "response")
gazeduration_predict2_df <- as.data.frame(gazeduration_predict2_regrid)
ggplot(gazeduration_predict2_df, aes(x = position_index, y = response, color = billboard_attention_level)) +
  geom_point(size = 5) +
  geom_line(size = 1, aes(group = billboard_attention_level))+
  #facet_wrap(~condition, labeller = labeller(condition = c(Count_Sales_Signs = "Sales Sign Counting", Free_Viewing = "Free Viewing"))) +
  theme_classic(base_size = 20) +
  #theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("less_attention" = "forestgreen", "more_attention" = "#013220"),
                     labels = c("less_attention" = "Less Attention-Grabbing", "Free_Viewing" = "More Attention-Grabbing")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3), breaks = seq(0, 7, by = 1)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visiblegazeduration_predict2 <-

gazeduration_predict3 <-
  emmeans(gazeduration, ~ position_index)
gazeduration_predict3_regrid <- regrid(gazeduration_predict3, transform = "response")
gazeduration_predict3_df <- as.data.frame(gazeduration_predict3_regrid)
ggplot(gazeduration_predict3_df, aes(x = position_index, y = response)) +
  geom_point(size = 5) +
  #facet_wrap(~condition, labeller = labeller(condition = c(Count_Sales_Signs = "Sales Sign Counting", Free_Viewing = "Free Viewing"))) +
  theme_classic(base_size = 20) +
  #theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3), breaks = seq(0, 7, by = 1)) + # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))

df_survey <- read_csv('02_qualtrics_data/recognition_survey_vr_city_cleaned.csv')
df_survey_filter <- df_survey %>%
  filter(!Subject %in% c("sub020", "sub017"))
attentiongrabbingcheck <- df_survey_filter %>%
  select(ends_with("_a")) %>%
  summarise(across(everything(), ~mean(.x == 1) * 100))
agcheck_pivot <- attentiongrabbingcheck %>%
  pivot_longer(
    cols = everything(),
    names_to = "billboard_name",
    values_to = "percent_pass"
  ) %>%
  mutate(billboard_name = str_remove(billboard_name, "_a$")) %>%
  mutate(billboard_name = case_when(
    billboard_name == "healthsunprotect" ~ "health_sunprotection",
    billboard_name == "health_mentalhealt" ~ "health_mentalhealth",
    TRUE ~ billboard_name  # Keep all other values unchanged
  ))
print(agcheck_pivot, n=40)
