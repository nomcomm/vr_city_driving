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
#library(psych)

setwd("/Users/suelim/Documents/Research/vr_city_driving/data/04_data_analysis")
df_total <- read_csv('00_all_data.csv')

colnames(df_total)

names(df_total)[names(df_total) == 'billboard#_fixated'] <- 'fixated'
names(df_total)[names(df_total) == 'billboard#_fixated_count'] <- 'fixation_count'
names(df_total)[names(df_total) == 'billboard#_duration'] <- 'fixation_duration'

model1 <-
  lmer(recall ~ fixation_duration + (1|billboard_image_at_position), data = df_total)
summary(model1)

model2 <-
  lmer(recognition ~ fixation_duration + (1|billboard_image_at_position), data = df_total)
summary(model2)

model01 <-
  lm(recall ~ position_index, data = df_total)
summary(model01)

model02 <-
  lm(recognition ~ position_index, data = df_total)
summary(model02)

model03 <-
  lm(fixation_duration ~ billboard_attention_level, data = df_total)
summary(model03)

model04 <-
  lm(fixation_duration ~ condition, data = df_total)
summary(model04)

model05 <-
  lm(fixation_duration ~ position_index, data = df_total)
summary(model05)

model06 <-
  glm(fixated ~ position_index, data = df_total, family = "binomial")
summary(model06)

model07 <-
  glm(fixated ~ billboard_attention_level, data = df_total, family = "binomial")
summary(model07)

model08 <-
  glm(fixated ~ condition, data = df_total, family = "binomial")
summary(model08)

model09 <-
  glm(fixated ~ position_index * condition, data = df_total, family = "binomial")
summary(model09)

model10 <-
  lm(fixation_count ~ position_index, data = df_total)
summary(model10)

model11 <-
  lmer(fixation_count ~ billboard_attention_level, data = df_total)
summary(model11)

model11 <-
  lm(fixation_count ~ condition, data = df_total)
summary(model11)

model3 <-
  lmer(recall ~ fixation_duration * condition + (1|billboard_image_at_position), data = df_total)
summary(model3)

model4 <-
  lmer(recognition ~ fixation_duration * condition + (1|billboard_image_at_position), data = df_total)
summary(model4)

model5 <-
  lmer(recall ~ fixation_duration * billboard_attention_level + (1|billboard_image_at_position), data = df_total)
summary(model5)

model6 <-
  lmer(recognition ~ fixation_duration * billboard_attention_level +(1|billboard_image_at_position), data = df_total)
summary(model6)

model7 <-
  lmer(recall ~ fixation_duration * billboard_attention_level * condition + (1|billboard_image_at_position), data = df_total)
summary(model7)

model8 <-
  lmer(recognition ~ fixation_duration * billboard_attention_level * condition + (1|billboard_image_at_position), data = df_total)
summary(model8)
