# Install (if not installed) and load the 'car' package for Levene's test
if(!require(car)) install.packages("car", dependencies = TRUE)
library(car)

if(!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
library(dplyr)

# load datasets
if(!require(readxl)) install.packages("readxl", dependencies = TRUE)
library(readxl)
df_first     <- read_excel("/Users/annabelcourt/Documents/FYP_Rubric_Qualitative_Data.xlsx", sheet = "pilot2")
df_blind     <- read_excel("/Users/annabelcourt/Documents/FYP_Rubric_Qualitative_Data.xlsx", sheet = "blind")
df_academic  <- read_excel("/Users/annabelcourt/Documents/FYP_Rubric_Qualitative_Data.xlsx", sheet = "Steve1")

head(df_first)
head(df_blind)
head(df_academic)

df_first$Marker   <- "First"
df_blind$Marker   <- "Blind"
df_academic$Marker <- "Academic"

df_combined <- bind_rows(df_first, df_blind, df_academic)
head(df_combined)
str(df_combined)
names(df_combined)


df_overall <- df_combined %>%
  group_by(`Essay Title`, Name, `Essay Type`, Marker) %>% 
  summarise(
    OverallScore = sum(Score, na.rm = TRUE),  # or mean(), as needed
    .groups = "drop"
  )
names(df_overall)

df_overall$Marker <- as.factor(df_overall$Marker)
levene_result <- leveneTest(OverallScore ~ Marker, data = df_overall)
print(levene_result)

unique(df_subset$`Essay Type`)
table(df_subset$`Essay Type`)
df_subset$`Essay Type` <- factor(df_subset$`Essay Type`, levels = c("Broad", "Specific"))
levels(df_subset$`Essay Type`)
table(df_subset$`Essay Type`)


str(df_combined$Score)
summary(df_combined$Score)
df_combined$Score <- as.numeric(df_combined$Score)


df_overall <- df_combined %>%
  group_by(`Essay Title`, Name, `Essay Type`, Marker) %>%
  summarise(
    OverallScore = sum(Score, na.rm = TRUE),
    .groups = "drop"
  )

df_overall %>% 
  group_by(`Essay Type`) %>% 
  summarise(
    Count = n(),
    Mean = mean(OverallScore, na.rm = TRUE),
    SD = sd(OverallScore, na.rm = TRUE)
  )

df_overall2 <- df_overall %>% 
  filter(!is.na(`Essay Type`))

unique(df_overall2$`Essay Type`)

t_test_topic <- t.test(OverallScore ~ `Essay Type`, 
                       data = df_overall2, 
                       var.equal = TRUE)
print(t_test_topic)


criteria_stats <- df_combined %>%
  group_by(Criteria) %>%
  summarise(
    Count = n(),
    Mean = mean(Level, na.rm = TRUE),
    SD = sd(Level, na.rm = TRUE),
    .groups = "drop"
  )

print(criteria_stats)

library(tidyr)
df_first <- df_first %>%
  group_by(`Essay Number`) %>%  # Group by the essay number
  mutate(OverallScore = sum(Level, na.rm = TRUE)) %>%  # Compute the sum of Level for each essay
  ungroup()

names(df_first)
df_wide <- df_first %>%
  pivot_wider(
    id_cols = c(`Essay Number`, `Type`, Marker, OverallScore),
    names_from = Criteria,
    values_from = Level
  )

head(df_wide)

cor_test_accuracy <- cor.test(df_wide$OverallScore, df_wide$`Accuracy of Feedback`, 
                              method = "pearson", 
                              use = "pairwise.complete.obs")
print(cor_test_accuracy)

cor_test_accuracy <- cor.test(df_wide$OverallScore, df_wide$`Constructive`, 
                              method = "pearson", 
                              use = "pairwise.complete.obs")
print(cor_test_accuracy)

cor_test_accuracy <- cor.test(df_wide$OverallScore, df_wide$`Feedforward and Critical`, 
                              method = "pearson", 
                              use = "pairwise.complete.obs")
print(cor_test_accuracy)

cor_test_accuracy <- cor.test(df_wide$OverallScore, df_wide$`Focus on content`, 
                              method = "pearson", 
                              use = "pairwise.complete.obs")
print(cor_test_accuracy)

cor_test_accuracy <- cor.test(df_wide$OverallScore, df_wide$`Motivational`, 
                              method = "pearson", 
                              use = "pairwise.complete.obs")
print(cor_test_accuracy)

cor_test_accuracy <- cor.test(df_wide$OverallScore, df_wide$`Specificity, Clarity, Detail`, 
                              method = "pearson", 
                              use = "pairwise.complete.obs")
print(cor_test_accuracy)

criteria_cols <- c("Accuracy of Feedback", 
                   "Constructive", 
                   "Feedforward and Critical", 
                   "Focus on content", 
                   "Motivational", 
                   "Specificity, Clarity, Detail")

# Calculate the correlation matrix using pairwise complete observations
cor_matrix <- cor(df_wide[, criteria_cols], 
                  use = "pairwise.complete.obs", 
                  method = "pearson")

print(cor_matrix)
