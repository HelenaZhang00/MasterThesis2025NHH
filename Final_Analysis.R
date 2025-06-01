library(readr) # handling csv files
library(tidyverse) # data preparation
library(gtsummary) # summary stats
library(effectsize) # effect size for tests
library(broom)
library(rlang)
library(corrplot)
library(stargazer) # nice regression tables
library(nnet)
library(knitr) # generate latex table
library(kableExtra) # generate latex table
library(psych) # for corr.test


# Preprocessing ----

## Data Cleaning ----

# the values are not that helpful - difficult to see whether positive or negative prompt is chosen
# raw_values <- read_csv("data_raw_values.csv")[-c(1, 2), ]

# the labels are more helpful since we can see the actual AI prompt chosen then
raw_labels <- read_csv("FinalVersionRaw.csv")[-c(1, 2), ]

# we continue to work with the labels data

# The column names are a bit weird - not very descriptive
# also, depending on which company the person chooses to read first about,
# different questions are shown (same questions but different ID in qualtrix) -->
# this means different columns in the dataset - to have one column for each
# Phoenix prompt question and one for each Quantum prompt question, we "merge"
# below are columns (questions) that "belong" together

# Phoenix1 and Q203 belong together

# Q62 and Q206 belong together

# Q67 and Q209 belong together


# Quantum 1 and Q213 belong together

# Q73 and Q216 belong together

# Q77 and Q219 belong together


# Some initial cleaning and renaming
data <- raw_labels %>% 
  # for every question, there is a first click, last click, click count column: exclude those
  select(-matches("First Click|Last Click|Click Count")) %>% 
  # filter attention and other check
  filter(Attention_Check == "Pass",
         Q104 == "Financial services") %>% 
  # here we merge these columns now and create nicer names for the columns
  mutate(phoenix1 = ifelse(is.na(Phoenix1), Q203, Phoenix1),
         phoenix2 = ifelse(is.na(Q62), Q206, Q62),
         phoenix3 = ifelse(is.na(Q67), Q209, Q67),
         quantum1 = ifelse(is.na(Quantum1), Q213, Quantum1),
         quantum2 = ifelse(is.na(Q73), Q216, Q73),
         quantum3 = ifelse(is.na(Q77), Q219, Q77),
         # time spent on the prompts chosen (not including the page where you have to choose the prompt)
         phoenix1_time = as.numeric(coalesce(`Q154_Page Submit`, `Q155_Page Submit`, `Q185_Page Submit`, `Q186_Page Submit`)),
         phoenix2_time = as.numeric(coalesce(`Q157_Page Submit`, `Q158_Page Submit`, `Q188_Page Submit`, `Q189_Page Submit`)),
         phoenix3_time = as.numeric(coalesce(`Q159_Page Submit`, `Q160_Page Submit`, `Q191_Page Submit`, `Q192_Page Submit`)),
         quantum1_time = as.numeric(coalesce(`Q167_Page Submit`, `Q168_Page Submit`, `Q176_Page Submit`, `Q177_Page Submit`)),
         quantum2_time = as.numeric(coalesce(`Q170_Page Submit`, `Q171_Page Submit`, `Q179_Page Submit`, `Q180_Page Submit`)),
         quantum3_time = as.numeric(coalesce(`Q173_Page Submit`, `Q174_Page Submit`, `Q182_Page Submit`, `Q183_Page Submit`))) %>%
  # we exclude the initial columns, they are double and unnecessary now
  select(-c(Q203, Q62, Q206,
            Q67, Q209,
            Q213, Q73, Q216,
            Q77, Q219)) %>%
  # exclude unanswered questions (encoded as -99)
  filter(!if_any(c(phoenix1, phoenix2, phoenix3, quantum1, quantum2, quantum3), ~ . == "-99")) %>% 
  # after creating all timing columns we can exclude all page submit columns
  select(-matches("Page Submit")) %>% 
  # rename the allocated amount columns to more descriptive names
  rename(amount_phoenix_initial = Q107_1,
         amount_quantum_initial = Q107_4,
         amount_phoenix_final = Q91_1,
         amount_quantum_final = Q91_2,
         read_first_about_company = Q51,
         s_confidence_initial = Q150,
         s_confidence_final = Q151,
         n_perceived_knowledge = Q153
         ) %>% 
  # All columns are type character, so we transform the amount columns to numeric
  mutate(amount_phoenix_initial = as.numeric(amount_phoenix_initial),
         amount_quantum_initial = as.numeric(amount_quantum_initial),
         amount_phoenix_final = as.numeric(amount_phoenix_final),
         amount_quantum_final = as.numeric(amount_quantum_final),
         n_perceived_knowledge = as.integer(n_perceived_knowledge)) %>% 
  # encode gender that is not male or female as "other"
  mutate(
    Gender = ifelse(Gender == "Male" | Gender == "Female", Gender, "Other")
  ) %>% 
  # lastly, we only keep fully completed answers
  filter(Progress == "100")



## Data Transformation ----

# creating important variables

# manually define positive and negative answers
data %>% pull(phoenix1) %>% unique()
P1_pos <- "Phoenix Financial Group Reports Record Earnings Amid Economic Uncertainty"
data %>% pull(phoenix2) %>% unique()
P2_pos <- "Phoenix Named Top Low-Risk Investment Option for 3rd Consecutive Year"
data %>% pull(phoenix3) %>% unique()
P3_pos <- "Phoenix Financial Sees Steady Growth with Global Expansion into Digital Solutions"
data %>% pull(quantum1) %>% unique()
Q1_pos <- "QuantumTech Innovations Leads Quantum Computing Race with New Processor"
data %>% pull(quantum2) %>% unique()
Q2_pos <- "Strategic Partnerships Drive QuantumTech’s Market Expansion"
data %>% pull(quantum3) %>% unique()
Q3_pos <- "How QuantumTech's AI-Powered Security Solutions Are Shaping the Future"

# now lets do some data transformations
data_cleaned <- data %>%
  # Create a variable for initially preferred company
  mutate(preferred_company = ifelse(amount_phoenix_initial > 25000, "Phoenix", "Quantum")) %>% 
  # filter out everyone who does not have a preference
  filter(amount_phoenix_initial != 25000) %>% 
  # dummies for positive chosen answers (positive yes/no)
  # 1 if positive is chosen, 0 if negative
  mutate(p1_pos = as.integer(phoenix1 == P1_pos),
         p2_pos = as.integer(phoenix2 == P2_pos),
         p3_pos = as.integer(phoenix3 == P3_pos),
         q1_pos = as.integer(quantum1 == Q1_pos),
         q2_pos = as.integer(quantum2 == Q2_pos),
         q3_pos = as.integer(quantum3 == Q3_pos)) %>%
  # dummies for negative chosen answers (negative yes/no)
  # 1 if negative is chosen, 0 if positive
  mutate(p1_neg = as.integer(phoenix1 != P1_pos),
         p2_neg = as.integer(phoenix2 != P2_pos),
         p3_neg = as.integer(phoenix3 != P3_pos),
         q1_neg = as.integer(quantum1 != Q1_pos),
         q2_neg = as.integer(quantum2 != Q2_pos),
         q3_neg = as.integer(quantum3 != Q3_pos)) %>% 
  # percentage of confirming news read
  # confirming news = positive news about preference + negative news about other company
  mutate(confirming_share = ifelse(preferred_company == "Phoenix", (p1_pos + p2_pos + p3_pos + q1_neg + q2_neg + q3_neg) / 6,
                                   (p1_neg + p2_neg + p3_neg + q1_pos + q2_pos + q3_pos) / 6)) %>%
  # create variables for time spent on confirming prompts in total
  mutate(confirming_time = ifelse(preferred_company == "Phoenix",
                                  (
                                    p1_pos*phoenix1_time
                                    + p2_pos*phoenix2_time
                                    + p3_pos*phoenix3_time
                                    + q1_neg*quantum1_time
                                    + q2_neg*quantum2_time
                                    + q3_neg*quantum3_time
                                  ),
                                  (
                                    p1_neg*phoenix1_time
                                    + p2_neg*phoenix2_time
                                    + p3_neg*phoenix3_time
                                    + q1_pos*quantum1_time
                                    + q2_pos*quantum2_time
                                    + q3_pos*quantum3_time
                                  ))) %>% 
  # take the log of confirming time to adjust for outliers
  mutate(log_confirming_time = log(0.001 + confirming_time)) %>% 
  # create variable for time spent on disconfirming prompts in total
  mutate(disconfirming_time = (phoenix1_time + phoenix2_time + phoenix3_time + quantum1_time + quantum2_time + quantum3_time) - confirming_time) %>% 
  # create a variable for the difference between confirming time and disconfirming time (conf-disconf)
  mutate(confirming_time_diff = confirming_time - disconfirming_time) %>% 
  # create variable for proportion of time spent on confirming prompts
  mutate(confirming_time_p = confirming_time / (confirming_time + disconfirming_time)) %>% 
  # create variable for proportion of time spent on disconfirming prompts
  mutate(disconfirming_time_p = 1 - confirming_time_p) %>% 
  # Create a binary biased variable
  mutate(biased = ifelse(confirming_share > 0.5, 1, 0)) %>% 
  # Create a variable for Bias reinforcement
  mutate(bias_reinforcement = ifelse(
    preferred_company == "Phoenix", 
    amount_phoenix_final - amount_phoenix_initial,
    (50000 - amount_phoenix_final) - (50000 - amount_phoenix_initial)
  )) %>%
  mutate(bias_reinforcement_cat = case_when(
    bias_reinforcement > 0 ~ "Reinforced",
    bias_reinforcement < 0 ~ "Reversed",
    TRUE ~ "Unchanged"
  )) %>% 
  # create variable final amount allocated to initially preferred company
  mutate(preferred_company_amount_final = ifelse(
    preferred_company == "Phoenix",
    amount_phoenix_final, amount_quantum_final
  )) %>% 
  # create numerical variables for confidence (1-5)
  mutate(
    n_confidence_initial = case_when(
      s_confidence_initial == "Not at all confident" ~ 1,
      s_confidence_initial == "Slightly confident" ~ 2,
      s_confidence_initial == "Somewhat confident" ~ 3,
      s_confidence_initial == "Very confident" ~ 4,
      s_confidence_initial == "Extremely confident" ~ 5),
    n_confidence_final = case_when(
      s_confidence_final == "Not at all confident" ~ 1,
      s_confidence_final == "Slightly confident" ~ 2,
      s_confidence_final == "Somewhat confident" ~ 3,
      s_confidence_final == "Very confident" ~ 4,
      s_confidence_final == "Extremely confident" ~ 5)) %>% 
  # create a variable for confidence change (final - initial)
  mutate(n_confidence_change = n_confidence_final - n_confidence_initial) %>% 
  # just some basic transformations
  mutate(Progress = as.numeric(Progress),
         Condition = as.factor(Condition)) %>% 
  # select only necessary columns to create a nice dataset without unnecessarily many columns
  select(StartDate, EndDate, Progress, `Duration (in seconds)`, Age, Gender, read_first_about_company, Q102,
         Condition, preferred_company,
         amount_phoenix_initial, amount_quantum_initial,
         phoenix1, phoenix2, phoenix3, quantum1, quantum2, quantum3,
         amount_phoenix_final, amount_quantum_final, Q92,
         p1_pos, p2_pos, p3_pos, q1_pos, q2_pos, q3_pos, p1_neg, p2_neg, p3_neg, q1_neg, q2_neg, q3_neg,
         phoenix1_time, phoenix2_time, phoenix3_time, quantum1_time, quantum2_time, quantum3_time,
         confirming_time, log_confirming_time, disconfirming_time, confirming_time_diff, confirming_time_p, disconfirming_time_p,
         confirming_share, biased, bias_reinforcement, bias_reinforcement_cat, preferred_company_amount_final,
         s_confidence_initial, s_confidence_final, n_confidence_initial, n_confidence_final, n_confidence_change,
         n_perceived_knowledge
         )


# Save data_cleaned as a csv file on computer
write_csv(data_cleaned, file = "data_cleaned.csv")
# Load the data
data_cleaned <- read_csv("data_cleaned.csv")




# CHI-SQUARE test ----

# Chi-Square Test for Distribution of Preferred_Company
# Create a contingency table of Preferred_Company by Condition
contingency_table <- table(data_cleaned$preferred_company, data_cleaned$Condition)

# Perform chi-square test
chi_square_test <- chisq.test(contingency_table)

# Print results
print(contingency_table)
print(chi_square_test)


# Test whether the proportion of males vs. females is balanced across the Control and Treatment groups.
# Create a contingency table of Gender by Condition
gender_table <- table(data_cleaned$Gender, data_cleaned$Condition)

# Perform chi-square test
chi_square_test_gender <- chisq.test(gender_table)

# Print results
print(gender_table)
print(chi_square_test_gender)



#Test whether age groups are balanced across the Control and Treatment groups
age_table <- table(data_cleaned$Age, data_cleaned$Condition)

# Perform chi-square test
chi_square_test_age <- chisq.test(age_table)

# Print results
print(age_table)
print(chi_square_test_age)




# Statistical Analysis ----

# Section H1, H2 and H3 in this code are not included in the Thesis and constitute the
# thought process behind the results
# All results included in the thesis are obtained under SUMMARY STATISTICS and 
# subsequent sections in this code.

## H1 ----

# create a separate dataset in case we want to apply more filters just for H1
# we could exclude outliers to get a normal distribution
df_h1 <- data_cleaned #%>% 
  #filter(confirming_time_diff >= -150, confirming_time_diff <= 150)

# check distribution to see if normality assumption holds
# we can see that confirming_time_diff follows approximately a normal distribution
# confirming_time_p does not follow a normal distribution at all
df_h1 %>% 
  ggplot(aes(x = confirming_time_diff, fill = Condition)) +
  geom_density(alpha = 0.5)

df_h1 %>% 
  ggplot(aes(x = confirming_time_p, fill = Condition)) +
  geom_density(alpha = 0.5)

# we can also statistically test normality:
# for conforming_time_diff, the test confirms normality (ONLY when outliers are excluded!)
# for confirming_time_p, the test does not confirm normality
df_h1 %>%
  group_by(Condition) %>%
  summarise(shapiro_p = shapiro.test(confirming_time_diff)$p.value)

# first visual check if there is a difference in means
# looks indeed like the two means are different as we suspect!
df_h1 %>% 
  ggplot(aes(x = Condition, y = confirming_time_p)) +
  geom_boxplot() +
  labs(title = "Proportion of Time Spent on Confirming Prompts",
       y = "Proportion of Time", x = "Condition (Control vs Treatment)")

df_h1 %>% 
  ggplot(aes(x = Condition, y = confirming_time_diff)) +
  geom_boxplot() +
  labs(title = "Difference of Time Spent on Confirming Prompts",
       y = "Difference of Time (Confirming-Disconfirming)", x = "Condition (Control vs Treatment)")

# for confirming_time_p, we should definitely use the wilcox test (non-normality)
# we are not able to reject the null hypothesis (p > 0.05)
# --> the two means are not significantly different from each other
wilcox.test(confirming_time_p ~ Condition, data = df_h1)

# for confirming_time_diff, we can use the t-test (assume normality)
# alternatively, the wilcox test if we do not exlcude outliers and normality does not hold
# depending on how many outliers are exlcuded, we can reject the nullhypothesis
t.test(confirming_time_p ~ Condition, data = df_h1)
wilcox.test(confirming_time_diff ~ Condition, data = df_h1)


## H2 ----

# create a seperate dataframe that contains only the necessary variables for H2
# in case we want to filter anything additional for only H2, we can do this here
df_h2 <- data_cleaned %>% 
  select(Condition, bias_reinforcement, confirming_share) %>% 
  mutate(bias_reinforcement_binary = as.integer(bias_reinforcement >= 0))

# check distribution to see if normality assumption holds
# on first look, bias_reinforcement seems to be approx. nonrmally distributed
df_h2 %>% 
  ggplot(aes(x = bias_reinforcement, fill = Condition)) +
  geom_density(alpha = 0.5)

# we can also statistically test normality:
# according to this test, we CANNOT confirm normality (p < 0.05), nullhypothesis is normal distribution
df_h2 %>%
  group_by(Condition) %>%
  summarise(shapiro_p = shapiro.test(bias_reinforcement)$p.value)

t.test(bias_reinforcement ~ Condition, data = df_h2, var.equal = FALSE)
wilcox.test(bias_reinforcement ~ Condition, data = df_h2)
    

## H3 ----

# create a seperate dataframe that contains only the necessary variables for H3
# in case we want to filter anything additional for only H3, we can do this here
df_h3 <- data_cleaned %>% 
  select(Condition, n_confidence_initial, n_confidence_final)

# check distribution to see if normality assumption holds
df_h3 %>% 
  ggplot(aes(x = n_confidence_final, fill = Condition)) +
  geom_density(alpha = 0.5)

wilcox.test(n_confidence_initial ~ Condition, data = df_h3)



## SUMMARY STATISTICS ----

# this function takes creates a neat summary to test our hypotheses
# see output explanation below
get_summary_for_var <- function(var, df) {
  data <- df %>%
    select(Condition, variable = !!sym(var))
  
  mean_sd <- data %>%
    group_by(Condition) %>% 
    summarise(mean = round(mean(variable), 2),
              sd = round(sd(variable), 2),
              median = round(median(variable), 2)) %>% 
    mutate(`mean (sd)` = paste0(as.character(mean), ' (', as.character(sd), ')'))
  
  # test normality assumption
  if (
    (data %>%
    group_by(Condition) %>%
    summarise(shapiro_p = shapiro.test(variable)$p.value) %>% pull(shapiro_p) %>% max()) > 0.05
  ) {
    test <- t.test(variable ~ Condition, data = data)
    test_name <- "Welch's t-test"
    stat <- paste0("t(", as.character(round(as.numeric(test$parameter),1)), ")=", as.character(round(test$statistic, 3)))
    effect <- cohens_d(variable ~ Condition, data = data)$Cohens_d
  } else {
    test <- wilcox.test(variable ~ Condition, data = data)
    test_name <- "Wilcoxon"
    stat <- as.character(test$statistic)
    effect <- rank_biserial(variable ~ Condition, data = data)$r_rank_biserial
  }
  
  results <- mean_sd %>% 
    select(Condition, `mean (sd)`, median) %>% 
    pivot_wider(names_from = Condition,
                values_from = c(`mean (sd)`, median))
  
  cbind(var, results, p = round(test$p.value, 3), test_name, statistic = stat, effect = round(effect, 3))
  
}

# this uses the function above and creates a nice summary table that contains
# everything needed as results for three hypothesis
# Mean (SD) for control and treatment group
# Median for control and treatment group
# p = p-value
# test_name: Wilcoxon or Welch's t-test depending on normality of variable
# test statistic
# effect-size: how big is the effect? cohens d or rank biserial depending on which test
summary_table <- rbind(
  get_summary_for_var("confirming_time_diff", data_cleaned),
  get_summary_for_var("confirming_time", data_cleaned),
  get_summary_for_var("log_confirming_time", data_cleaned),
  get_summary_for_var("confirming_time_p", data_cleaned),
  get_summary_for_var("preferred_company_amount_final", data_cleaned),
  get_summary_for_var("bias_reinforcement", data_cleaned),
  get_summary_for_var("n_confidence_initial", data_cleaned),
  get_summary_for_var("n_confidence_final", data_cleaned),
  get_summary_for_var("n_confidence_change", data_cleaned),
  get_summary_for_var("confirming_share", data_cleaned)
)



## Correlation Analysis ----

# add all variables here that should be included in the correlation analysis
# names of columns in cleaned data
vars <- c(
  "confirming_time_diff",
  "confirming_time",
  "log_confirming_time",
  "confirming_time_p",
  "bias_reinforcement",
  "preferred_company_amount_final",
  "n_confidence_initial",
  "n_confidence_final",
  "n_confidence_change",
  "confirming_share",
  "amount_phoenix_initial",
  "amount_phoenix_final",
  "n_perceived_knowledge"
)

# names of variables as they should appear in the thesis
variable_names <- c(
  "Confirming Time Diff",
  "Confirming Time (sec)",
  "Log Confirming Time",
  "Confirming Time (%)",
  "Bias Reinforcement",
  "Final Allocation",
  "Initial Confidence",
  "Final Confidence",
  "Change in Confidence",
  "Confirming Share",
  "Initial Amount (Phoenix)",
  "Final Amount (Phoenix)",
  "Perceived Knowledge"
)

df_corr <- na.omit(data_cleaned[vars])

# Compute correlations and p-values
corr_result <- corr.test(df_corr, use = "pairwise.complete.obs", adjust = "none")

r <- corr_result$r
p <- corr_result$p

# rename the columns and rows
colnames(r) <- variable_names
rownames(r) <- variable_names

# Function to add stars in table
format_corr <- function(r, p) {
  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01, "**",
                         ifelse(p < 0.05, "*", "")))
  val <- sprintf("%.2f", r)
  paste0(val, stars)
}

# Apply formatting of correlation matrix
r_stars <- matrix("", nrow = nrow(r), ncol = ncol(r))
for (i in 2:nrow(r)) {
  for (j in 1:(i - 1)) {
    r_stars[i, j] <- format_corr(r[i, j], p[i, j])
  }
}
colnames(r_stars) <- colnames(r)
rownames(r_stars) <- rownames(r)

# make overleaf table
df_corr_matrix <- as.data.frame(r_stars)
df_corr_matrix <- rownames_to_column(df_corr_matrix, var = "Variable")

# Generate the LaTeX table
rotated_colnames <- c("Variable", paste0("\\rotatebox{90}{", colnames(r_stars)[-1], "}"))
colnames(r_stars) <- rotated_colnames

kbl(r_stars, format = "latex", booktabs = TRUE, escape = FALSE,
    caption = "Correlation Matrix with Vertical Column Names") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))



## Regression Models ----

### Model 1 (H2) ----

# multinomial regression (3 different categories)
model1 <- multinom(bias_reinforcement_cat ~ Condition, data = data_cleaned)
model1_summary <- summary(model1)

# show results, disregard the intercept estimates
tidy(model1, conf.int = TRUE, exponentiate = TRUE)

# create a nice output for latex
stargazer(model1,
          type = "text",
          #title = "Linear Regression Predicting Bias Reinforcement with Interaction Term",
          label = "tab:reg_model2_results",
          style = "default",
          omit.stat = c("f", "ser", "adj.rsq"),
          digits = 2,
          ci = TRUE)



### Model 2 (H2) ----

# now trying to understand the effect of confirming_share

model2 <- lm(bias_reinforcement ~ 
               Condition * confirming_share +
               Age +
               Gender+
               n_confidence_change,
             data = data_cleaned)
summary(model2)

# create a nice output for latex
stargazer(model2,
          type = "latex",
          #title = "Linear Regression Predicting Bias Reinforcement with Interaction Term",
          label = "tab:reg_model2_results",
          style = "default",
          dep.var.labels = "Bias Reinforcement",
          covariate.labels = c("Treatment (vs Control)",
                               "Confirming Share",
                               "Age 25-30", "Age 31-40", "Age 41-50", "Age 50+",
                               "Gender Male", "Gender Other",
                               "Confidence Change",
                               "Treatment × Confirming Share"
                               ),
          omit.stat = c("f", "ser", "adj.rsq"),
          digits = 0,
          ci = TRUE)


### Model 3 (H3) ----

# mediating effect of confirming share on confidence change

# this is with perceived knowledge control

model3 <- lm(n_confidence_change ~ Condition * confirming_share +
               n_perceived_knowledge +
               Age +
               Gender,
             data = data_cleaned)
summary(model3)

# create a nice regression table for latex
stargazer(model3,
          type = "latex",
          #title = "Linear Regression Predicting...",
          label = "tab:reg_model3_results",
          style = "default",
          dep.var.labels = c("Confidence Change"),
          covariate.labels = c("Treatment (vs Control)",
                               "Confirming Share",
                               "Perceived Knowledge",
                               "Age 25-30", "Age 31-40", "Age 41-50", "Age 50+",
                               "Gender Male", "Gender Other",
                               "Treatment × Confirming Share"),
          omit.stat = c("f", "ser", "adj.rsq"),
          digits = 2,
          ci = TRUE)


