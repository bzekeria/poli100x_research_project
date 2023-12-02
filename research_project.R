# Baraa Zekeria

# libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(MatchIt)

# Data Exploration
df <- read_excel("CELHouse117LES2.xls")

str(df)

## Data Cleaning

### Subset Data to Columns of Interest

relevant_columns <- c(
  # dependent
  "Number substantive bills passing House (inc in other leg vehicles)",
  "Number substantive bills becoming law (inc in other leg vehicles)", 
  # independent
  "1 = served in state legislature",
  # covariates
  "1 = Democrat",
  "Seniority, number of terms served counting current",
  "First-dimension DW-NOMINATE score",
  "Second-dimension DW-NOMINATE score",
  "1 = female",
  # control
  "Two-letter state code",
  "Congress number",
  "1= Speaker",
  "1 = committee chair, according to Almanac of American Politics",
  "1 = Majority party leadership",
  "1 = Minority party leadership",
  "Congressional district number",
  "Year first elected to House",
  "Absolute distance from floor median (DW-NOMINATE)",
  "Absolute distance from majority party median (DW-NOMINATE)"
)

subset_df <- df[, relevant_columns]

### Manipulate Variables

#### Dependent Variable

# dependent 
subset_df$performance_ratio <- subset_df$`Number substantive bills becoming law (inc in other leg vehicles)` /
  subset_df$`Number substantive bills passing House (inc in other leg vehicles)`

#### Independent Variable

names(subset_df)[names(subset_df) == "1 = served in state legislature"] <- "prior_experience"

#### Covariates 

names(subset_df)[names(subset_df) == "1 = Democrat"] <- "party_affiliation"

names(subset_df)[names(subset_df) == "Seniority, number of terms served counting current"] <- "seniority"

subset_df$ideological_position <- rowMeans(subset_df[, c("First-dimension DW-NOMINATE score", "Second-dimension DW-NOMINATE score")], na.rm = TRUE)

names(subset_df)[names(subset_df) == "1 = female"] <- "gender"

#### Control 

names(subset_df)[names(subset_df) == "Two-letter state code"] <- "state"

names(subset_df)[names(subset_df) == "Congress number"] <- "congress_num"

names(subset_df)[names(subset_df) == "1= Speaker"] <- "speaker"

names(subset_df)[names(subset_df) == "1 = committee chair, according to Almanac of American Politics"] <- "comm_chair"

names(subset_df)[names(subset_df) == "1 = Majority party leadership"] <- "majority_ldp"

names(subset_df)[names(subset_df) == "1 = Minority party leadership"] <- "minority_ldp"

names(subset_df)[names(subset_df) == "Congressional district number"] <- "district"

names(subset_df)[names(subset_df) == "Year first elected to House"] <- "first_yr_elec"

names(subset_df)[names(subset_df) == "Absolute distance from floor median (DW-NOMINATE)"] <- "distance_from_floor_median"

names(subset_df)[names(subset_df) == "Absolute distance from majority party median (DW-NOMINATE)"] <- "distance_from_majority_party_median"

# Final data
final_df <- subset_df %>%
  select(
    performance_ratio,
    prior_experience,
    party_affiliation,
    seniority,
    ideological_position,
    gender,
    state,
    congress_num,
    speaker,
    comm_chair,
    majority_ldp,
    minority_ldp,
    district,
    first_yr_elec,
    distance_from_floor_median,
    distance_from_majority_party_median
  )

# Check for missing values

names(final_df[colSums(is.na(final_df)) > 0])

# Impute numerical cols with missing values with mean 

final_df$performance_ratio <- ifelse(is.na(final_df$performance_ratio), mean(final_df$performance_ratio, na.rm = TRUE), final_df$performance_ratio)

final_df$ideological_position <- ifelse(is.na(final_df$ideological_position), mean(final_df$ideological_position, na.rm = TRUE), final_df$ideological_position)

final_df$distance_from_floor_median <- ifelse(is.na(final_df$distance_from_floor_median), mean(final_df$distance_from_floor_median, na.rm = TRUE), final_df$distance_from_floor_median)

final_df$distance_from_majority_party_median <- ifelse(is.na(final_df$distance_from_majority_party_median), mean(final_df$distance_from_majority_party_median, na.rm = TRUE), final_df$distance_from_majority_party_median)


## Descriptive Statistics

summary(final_df)

# Distribution of Prior State Legislative Experience 
ggplot(final_df, aes(x = factor(prior_experience), fill = factor(prior_experience))) +
  geom_bar() +
  labs(title = "Distribution of Prior State Legislative Experience",
       x = "Prior State Legislative Experience",
       y = "Count") +
  scale_x_discrete(breaks = c("0", "1")) +  
  theme_minimal()

ggplot(final_df, aes(x = performance_ratio)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Performance Ratio",
       x = "Performance Ratio",
       y = "Count") +
  theme_minimal()


## Causal Inference

# Propensity score matching using MatchIt
ps_model <- glm(prior_experience ~ party_affiliation + seniority +
                  ideological_position + gender, 
                data = final_df, family = "binomial")

matched_data <- matchit(prior_experience ~ party_affiliation + seniority +
                          ideological_position + gender, 
                        data = final_df, method = "nearest")

# Check balance
summary(matched_data)

# P-value Bar Chart
p_values <- summary(ps_model)$coefficients[, "Pr(>|z|)"]

p_values_df <- data.frame(Variable = names(p_values), P_Value = p_values)

ggplot(p_values_df, aes(x = Variable, y = P_Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "P-Values of Covariates after Propensity Score Matching",
       x = "Covariates",
       y = "P-Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Covaraite Balance Graph

covariate_cols <- c("party_affiliation", "seniority", "ideological_position", "gender")

matched_data_df <- match.data(matched_data)

par(mfrow = c(2, 2))
for (covariate in covariate_cols) {
  boxplot(formula = as.formula(paste(covariate, "~ prior_experience")), data = final_df, main = covariate, col = "lightblue", border = "black")
  boxplot(formula = as.formula(paste(covariate, "~ prior_experience")), data = matched_data_df, main = covariate, col = "lightgreen", border = "black")
}
```

### Estimate Treatment Effects

# Convert matched_data to a dataset
matched_data <- match.data(matched_data)

# Compare means before and after matching
summary(final_df$performance_ratio)
summary(matched_data$performance_ratio)

# Perform statistical tests (e.g., t-test) to assess significance
t.test(matched_data$performance_ratio ~ matched_data$prior_experience)

# Linear Regression
model <- lm(performance_ratio ~ prior_experience + party_affiliation + seniority +
              ideological_position + gender, 
            data = matched_data)

summary(model)

plot(model)

# Calculate means and standard errors
mean_before <- mean(final_df$performance_ratio)
mean_after <- mean(matched_data_df$performance_ratio)
se_before <- sd(final_df$performance_ratio) / sqrt(length(final_df$performance_ratio))
se_after <- sd(matched_data_df$performance_ratio) / sqrt(length(matched_data_df$performance_ratio))

plot_data <- data.frame(
  Group = c("Before Matching", "After Matching"),
  Mean = c(mean_before, mean_after),
  SE = c(se_before, se_after)
)

ggplot(plot_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), position = position_dodge(width = 0.7), width = 0.25) +
  labs(title = "Comparison of Means Before and After Matching",
       x = "Matching Status",
       y = "Performance Ratio") +
  theme_minimal()
