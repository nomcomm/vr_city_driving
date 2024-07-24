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
library(mclogit)
library(janitor)
library(kableExtra)
library(broom.mixed)
#library(psych)

setwd("/Users/suelim/Documents/Research/VR_City/vr_city_driving/data")
df <- read_csv('04_data_analysis/00_all_data.csv')
names(df)[names(df) == 'billboard#_fixated'] <- 'fixated'
names(df)[names(df) == 'billboard#_fixated_count'] <- 'fixation_count'
names(df)[names(df) == 'billboard#_duration'] <- 'fixation_duration'
names(df)[names(df) == 'billboard_image_at_position'] <- 'billboard_name'
df2 <- read_csv('04_data_analysis/01_position_index.csv')
df_merged <- df %>%
  left_join(df2, by = c("position_index" = "Position"))

#calculated median fixation count 
fixation_med_calculation <- df_merged %>%
  select(subject, condition, fixation_count) %>%
  filter(!subject %in% c("sub020", "sub017")) %>%
  group_by(subject) %>%
  summarise(
    median_fixation_count = median(fixation_count, na.rm = TRUE)
  )
median(fixation_med_calculation$median_fixation_count) #fixation count = 1

#update dataframe with fixation count categories
updated_data <- df_merged %>%
  filter(!subject %in% c("sub020", "sub017")) %>%
  mutate(
    gaze_behavior = case_when(
      fixation_count == 0 ~ "not_fixated",
      fixation_count == 1 ~ "fixated",  # Use the median for "fixated"
      fixation_count > 1 ~ "very_fixated",
      TRUE ~ NA_character_  # This line is optional, handles cases outside the specified conditions
    ),
    billboard_location = case_when(
      position_index >= 1 & position_index <= 8 ~ "beginning",
      position_index > 8 & position_index <= 16 ~ "mid_beginning",
      position_index > 16 & position_index <= 24 ~ "middle",
      position_index > 24 & position_index <= 32 ~ "mid_end",
      position_index > 32 & position_index <= 40 ~ "end",
      TRUE ~ NA_character_  
    )
  )

#descriptive analysis
data0 <- 
  updated_data %>%
  group_by(subject,condition) %>%
  summarise(
    count = n(),
    total_fixation = sum(fixated, na.rm = TRUE),
    total_recall = sum(recall, na.rm = TRUE),
    total_recognition = sum(recognition, na.rm = TRUE)
  )
data0_freeviewing <- 
  data0 %>% filter(condition == "Free_Viewing")
data0_count <- 
  data0 %>% filter(condition == "Count_Sales_Signs")
mean(data0$total_fixation)
mean(data0$total_recall)
mean(data0$total_recognition)
mean(data0_freeviewing$total_fixation)
mean(data0_freeviewing$total_recall)
mean(data0_freeviewing$total_recognition)
mean(data0_count$total_fixation)
mean(data0_count$total_recall)
mean(data0_count$total_recognition)

#data management
updated_data$position_index <- as.factor(updated_data$position_index)
updated_data$condition <- as.factor(updated_data$condition)
updated_data$subject <- as.factor(updated_data$subject)
updated_data$billboard_name <- as.factor(updated_data$billboard_name)
updated_data$billboard_attention_level <- as.factor(updated_data$billboard_attention_level)
updated_data$gaze_behavior <- factor(updated_data$gaze_behavior, levels = c("not_fixated", "fixated", "very_fixated"))
updated_data$billboard_location <- factor(updated_data$billboard_location, levels = c("beginning", "mid_beginning", "middle", "mid_end", "end"))
updated_data$topic <- 
  ifelse(grepl("^ad_", updated_data$billboard_name), "Commercial", 
         ifelse(grepl("^health_", updated_data$billboard_name), "Health", NA))

#hypothesis and research question testing
cor.test(updated_data$fixation_count, updated_data$fixation_duration)
data <- 
  updated_data %>%
  group_by(condition, gaze_behavior) %>%
  summarise(
    count = n(),
    total_recall = sum(recall, na.rm = TRUE),
    total_recognition = sum(recognition, na.rm = TRUE),
    mean_recall = mean(recall, na.rm = TRUE),
    mean_recognition = mean(recognition, na.rm = TRUE)
  )
ggplot(data, aes(x = gaze_behavior, y = mean_recall, color = condition)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = condition)) +
  #facet_wrap(~condition) +
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  #scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = Median", very_fixated = "Fixation > Median")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, .2)) +  # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible
ggplot(data, aes(x = gaze_behavior, y = mean_recognition, color = condition)) +
  geom_point(size=5) +
  geom_line(size=1, aes(group = condition)) +
  #facet_wrap(~condition) +
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  scale_x_discrete(labels=c(not_fixated = "Fixation = 0", fixated = "Fixation = Median", very_fixated = "Fixation > Median")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, .6), breaks = seq(0, .6, by = .2)) +  # Prevents the y-axis from extending below zero
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

recall <-
  glmer(recall ~ gaze_behavior+fixation_duration+condition + (1|billboard_name), data = updated_data, family = "binomial")
Anova(recall)
summary(recall)
performance(recall)
vif(recall)
ggplot(updated_data, aes(x = fixation_duration, y = recall, color = condition)) +
  geom_point(alpha = 0.5) +  # Scatter plot with semi-transparent points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression curves
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

recognition <-
  glmer(recognition ~ gaze_behavior+fixation_duration+condition + (1|billboard_name), data = updated_data, family = "binomial")
Anova(recognition)
summary(recognition)
performance(recognition)
vif(recognition)
ggplot(updated_data, aes(x = fixation_duration, y = recognition, color = condition)) +
  geom_point(alpha = 0.5) +  # Scatter plot with semi-transparent points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic regression curves
  theme_classic(base_size = 20) +
  guides(color = guide_legend(ncol = 1)) +
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free_Viewing" = "Free Viewing")) + 
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

gazeduration <- 
  glmmTMB(fixation_duration ~ condition+billboard_attention_level+position_index + 
            (1 | billboard_name)+ (1| subject), 
          ziformula = ~ 1,  # Formula for zero-inflation component
          family = ziGamma(link = "log"),  # Specify zero-inflated Gamma family
          data = updated_data)

gazeduration2 <- 
  glmmTMB(fixation_duration ~ condition+billboard_attention_level+position_index + 
            (1 | billboard_name)+ (1| subject), 
          ziformula = ~ (1 | billboard_name)+ (1| subject),  # Formula for zero-inflation component
          family = ziGamma(link = "log"),  # Specify zero-inflated Gamma family
          data = updated_data)

gazeduration3 <- 
  glmmTMB(fixation_duration ~ condition+billboard_attention_level+position_index + 
            (1 | billboard_name)+ (1| subject), 
          ziformula = ~ condition+billboard_attention_level+position_index,  # Formula for zero-inflation component
          family = ziGamma(link = "log"),  # Specify zero-inflated Gamma family
          data = updated_data)
gazeduration4 <- 
  glmmTMB(fixation_duration ~ condition+billboard_attention_level+position_index + 
            (1 | billboard_name)+ (1| subject), 
          ziformula = ~ condition+billboard_attention_level+position_index  + 
            (1 | billboard_name)+ (1| subject),  # Formula for zero-inflation component
          family = ziGamma(link = "log"),  # Specify zero-inflated Gamma family
          data = updated_data)
lrtest(gazeduration3,gazeduration4)
Anova(gazeduration4)
summary(gazeduration4)

Anova(gazeduration, type="3")
summary(gazeduration)
performance(gazeduration)
data2_1 <- 
  updated_data %>%
  group_by(condition) %>%
  get_summary_stats(fixation_duration, type = "full")
ggplot(data2_1, aes(x = condition, y = mean, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                    labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free Viewing" = "Free Viewing")) + 
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                     labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free Viewing" = "Free Viewing")) + 
  theme_classic(base_size = 20) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +  
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),  
        # axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black")) 
data2_1 <- 
  updated_data %>%
  group_by(position_index) %>%
  get_summary_stats(fixation_duration, type = "median")
ggplot() +
  geom_point(data = data2_1, aes(x = position_index, y = median), size = 3) + 
  geom_line(data = data2_1, aes(x = position_index, y = median, group=1), size = 2, color = "gray0") +
  theme_classic(base_size = 20) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +  
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        # axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),  
        # axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black")) 

fixationcount <- 
  mblogit(formula = gaze_behavior ~ condition+billboard_attention_level+position_index, 
          random = c(~1|billboard_name, ~1|subject), data = updated_data)
summary(fixationcount)
performance(fixationcount)
data3_1 <- updated_data %>%
  group_by(condition, gaze_behavior) %>%
  summarise(
    count = n()
  ) %>%
  mutate(total_count = sum(count),
         proportion = count / total_count)
ggplot(data3_1, aes(x = condition, y = proportion, fill = gaze_behavior)) +
  geom_bar(stat = "identity", position = "stack", size = 2) +
  scale_fill_manual(values = c("not_fixated" = "darkgray", "fixated" = "gray40", "very_fixated" = "gray20")) + 
  scale_color_manual(values = c("Count_Sales_Signs" = "navyblue", "Free_Viewing" = "black"),
                    labels = c("Count_Sales_Signs" = "Sale Sign Counting", "Free Viewing" = "Free Viewing")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, by = .25)) +  # Prevents the y-axis from extending below zero
  theme_classic(base_size = 20) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible
data3_2 <- 
  updated_data %>%
  group_by(position_index, gaze_behavior) %>%
  summarise(
    count = n()
  ) %>%
  mutate(total_count = sum(count),
         proportion = count / total_count)
ggplot(data3_2, aes(x = position_index, y = proportion, fill = gaze_behavior)) +
  geom_bar(stat = "identity", position = "stack", size = 2) +
  scale_fill_manual(values = c("not_fixated" = "darkgray", "fixated" = "gray40", "very_fixated" = "gray20")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1, by = .25)) +  # Prevents the y-axis from extending below zero
  theme_classic(base_size = 20) +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),  # Removes x axis labels
        #axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),  # Removes x axis labels
        #axis.ticks.y=element_blank(),
        axis.line.x=element_line(color="black"))  # Ensure the x-axis line is visible

data4 <- 
  updated_data %>%
  group_by(position_index) %>%
  summarise(
    count = n(),
    total_fixation = sum(fixated, na.rm = TRUE),
    total_recall = sum(recall, na.rm = TRUE),
    total_recognition = sum(recognition, na.rm = TRUE),
    total_gaze_duration = sum(fixation_duration, na.rm = TRUE),
    total_fixation_count = sum(fixation_count, na.rm = TRUE),
    fixation_rate = total_fixation/count,
    recall_rate = total_recall/count,
    recognition_rate = total_recognition/count,
    median_gaze_duration = median(fixation_duration, na.rm = TRUE),
    median_fixation_count = median(fixation_count, na.rm = TRUE)
  ) 
colnames(data4)
  
ggplot() +
  geom_point(data = data4 , aes(x = position_index, y = recall_rate), size = 2) + 
  geom_line(data = data4 , aes(x = position_index, y = recall_rate, group=1), size = 1) +
  geom_point(data = data4 , aes(x = position_index, y = recognition_rate), size = 2) + 
  geom_line(data = data4, aes(x = position_index, y = recognition_rate, group=1), size = 1, linetype="twodash") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        # axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),  
        # axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black")) 

ggplot() +
  geom_point(data = data4 , aes(x = position_index, y = median_gaze_duration), size = 2) + 
  geom_line(data = data4, aes(x = position_index, y = median_gaze_duration, group=1), size = 1) +
  geom_point(data = data4 , aes(x = position_index, y = median_fixation_count), size = 2) + 
  geom_line(data = data4, aes(x = position_index, y = median_fixation_count, group=1), size = 1, linetype="dashed") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        # axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),  
        # axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black")) 

#Mediation - not included in paper yet
install.packages("bmlm")
library(bmlm)

mediation_data <- updated_data
mediation_data$condition <- 
  ifelse(mediation_data$condition == "Count_Sales_Signs", 0, 
         ifelse(mediation_data$condition == "Free_Viewing", 1, mediation_data$condition))
colnames(updated_data)

fit <- mlm(d = mediation_data,
           id = "subject",
           x = "condition",
           m = "fixation_duration",
           y = "recall",
           binary_y = TRUE,
           priors = list(b=1),
           cores=4,
           iter = 10000,
           control = list(adapt_delta = 0.99))
mlm_summary(fit)

fit2 <- mlm(d = mediation_data,
           id = "subject",
           x = "condition",
           m = "fixation_count",
           y = "recall",
           binary_y = TRUE,
           priors = list(b=1),
           cores=4,
           iter = 10000,
           control = list(adapt_delta = 0.99))
mlm_summary(fit2)


#Percent indicated as attention-grabbing
df_survey <- read_csv('02_qualtrics_data/recognition_survey_vr_city_cleaned.csv')
colnames(df_survey)
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
  )) %>%
  arrange(desc(percent_pass))
print(agcheck_pivot, n=40)

#demographic information
mean(df_survey_filter$Age, na.rm = TRUE)
sd(df_survey_filter$Age, na.rm = TRUE)
table(df_survey_filter$Gender)
21/40




