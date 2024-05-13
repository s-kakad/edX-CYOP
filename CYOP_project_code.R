########## edX CYOP: LOADING LIBRARIES & DATASETS ##########
# Set working directory
setwd("~/projects/edX-CYOP")

# Load libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggplot2)
library(knitr)
library(tinytex)

# Upload datasets
fw_df_solutions_2021 <- read.csv("edX_ReFED_2021.csv")
fw_df_solutions_2022 <- read.csv("edX_ReFED_2022.csv")
fw_df_policies <- read.csv("edX_ReFED_policies.csv")

# Merge 2021 and 2022 dataframes into single 'mother' solutions dataframe
fw_df_solutions <- merge(x = fw_df_solutions_2021, y = fw_df_solutions_2022[ , c("state", "solution_name", "solution_diversion_potential_tons_2022")],
      by = c("state", "solution_name"), all.x = TRUE)
remove(fw_df_solutions_2021, fw_df_solutions_2022)

# Check structure
str(fw_df_solutions)
str(fw_df_policies)

########## edX CYOP: DATA WRANGLING ##########
# Convert dataframes to appropriate class
fw_df_solutions$solution_policy_category <- as.factor(fw_df_solutions$solution_policy_category)
fw_df_solutions$solution_intervention_category <- as.factor(fw_df_solutions$solution_intervention_category)
fw_df_solutions$solution_name <- as.factor(fw_df_solutions$solution_name)
fw_df_solutions$state <- as.factor(fw_df_solutions$state)
fw_df_solutions$solution_diversion_potential_tons_2021 <- as.numeric(fw_df_solutions$solution_diversion_potential_tons_2021)
fw_df_solutions$solution_diversion_potential_tons_2022 <- as.numeric(fw_df_solutions$solution_diversion_potential_tons_2022)

fw_df_policies$state <- as.factor(fw_df_policies$state)
fw_df_policies$solution_policy_category <- as.factor(fw_df_policies$solution_policy_category)
fw_df_policies$population_2021 <- as.numeric(fw_df_policies$population_2021)
fw_df_policies$population_2022 <- as.numeric(fw_df_policies$population_2022)
fw_df_policies$total_fw_tons_2021 <- as.numeric(fw_df_policies$total_fw_tons_2021)
fw_df_policies$total_fw_tons_2022 <- as.numeric(fw_df_policies$total_fw_tons_2022)
fw_df_policies$policy_score <- as.factor(fw_df_policies$policy_score)

# Reassign policy type for solution Livestock Feed from Recycling to Animal Feed
# Note: this is because Livestock Feed is considered as a separate policy category in the policy dataframe
fw_df_solutions <- fw_df_solutions %>%
  mutate(solution_policy_category = ifelse(solution_name == "Livestock Feed", "Animal Feed", as.character(solution_policy_category)))
fw_df_solutions$solution_policy_category <- as.factor(fw_df_solutions$solution_policy_category)

# Reorder factor order for policy categories
fw_df_solutions$solution_policy_category <- factor(fw_df_solutions$solution_policy_category, levels = c("Prevention", "Rescue", "Animal Feed", "Recycling"))
order(levels(fw_df_solutions$solution_policy_category))
fw_df_policies$solution_policy_category <- factor(fw_df_policies$solution_policy_category, levels = c("Prevention", "Rescue", "Animal Feed", "Recycling"))

# States are named differently across both dataframes, which we need to align given that
# we will be merging dataframes based on the columns "state" and "solution_policy_category"
sum(levels(fw_df_solutions$state) == state.name) # TRUE for all states
sum(levels(fw_df_policies$state) == state.abb) # TRUE only for 26 states

# Reorder the two-letter state abbreviations according to the built-in state.abb data set
fw_df_policies$state <- factor(fw_df_policies$state, levels = state.abb)
sum(levels(fw_df_policies$state) == state.abb) # now TRUE for all states

# Rename the factors using the state.abb data set (same order as state.abb)
levels(fw_df_solutions$state) <- state.abb

# Check that levels for both "state" and "solution_policy_category" align 
sum(levels(fw_df_solutions$state) == levels(fw_df_policies$state))
levels(fw_df_solutions$solution_policy_category) == levels(fw_df_policies$solution_policy_category)

# Combine data frames
combined_df <- left_join(fw_df_solutions, fw_df_policies, by = c("state", "solution_policy_category"))
remove(fw_df_policies, fw_df_solutions)

# Check structure
str(combined_df)

########## edX CYOP: BUILD TRAINING AND TEST SETS ##########
# Create test and train set
set.seed(1, sample.kind = "Rounding") # using R 3.6 or later (R 4.1.1)
test_index <- createDataPartition(combined_df$total_fw_tons_2022, times = 1, p = 0.2, list = FALSE)
test_temp <- combined_df[test_index, ] #this is the temporary test set
train_temp <- combined_df[-test_index, ] #this is the temporary train set

#ensure combinations of state x policy_category in the test set are also in the train set (if not, move to the trains set)
test_set <- test_temp %>% 
  semi_join(train_temp, by = "state") %>%
  semi_join(train_temp, by = "solution_policy_category") #this is now the updated test set

removed <- anti_join(test_temp, test_set)
train_set <- rbind(train_temp, removed) #this is now the updated train set
remove(test_index, test_temp, train_temp, removed)

options(digits = 4)

########## edX CYOP: MACHINE LEARNING ALGORITHMS ##########
# Here, we will consider a 20% above or below the real value as acceptable

# Just the average
solutions_avg <- mean(combined_df$solution_diversion_potential_tons_2022)
avg_acc <- mean(solutions_avg >= 0.80*test_set$solution_diversion_potential_tons_2022 & solutions_avg <= 1.20*test_set$solution_diversion_potential_tons_2022)
avg_acc

# Linear regression model
train_glm <- train(solution_diversion_potential_tons_2022 ~ solution_diversion_potential_tons_2021, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
glm_acc <- mean(glm_preds >= 0.80*test_set$solution_diversion_potential_tons_2022 & glm_preds <= 1.20*test_set$solution_diversion_potential_tons_2022)
glm_acc

train_glm_all <- train(solution_diversion_potential_tons_2022 ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
glm_all_acc <- mean(glm_all_preds >= 0.80*test_set$solution_diversion_potential_tons_2022 & glm_all_preds <= 1.20*test_set$solution_diversion_potential_tons_2022)
glm_all_acc

# kNN model
train_knn <- train(solution_diversion_potential_tons_2022 ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(1, 50, 1)))
train_knn$bestTune

knn_preds <- predict(train_knn, test_set)
knn_acc <- mean(knn_preds >= 0.80*test_set$solution_diversion_potential_tons_2022 & knn_preds <= 1.20*test_set$solution_diversion_potential_tons_2022)
knn_acc

# Cross-validation model
train_knn_cv <- train(solution_diversion_potential_tons_2022 ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(1, 50, 1)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.1))
train_knn_cv$bestTune

knn_cv_preds <- predict(train_knn_cv, test_set)
knn_cv_acc <- mean(knn_cv_preds >= 0.80*test_set$solution_diversion_potential_tons_2022 & knn_cv_preds <= 1.20*test_set$solution_diversion_potential_tons_2022)
knn_cv_acc

# Classification tree model
train_rpart <- train(solution_diversion_potential_tons_2022 ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune

rpart_preds <- predict(train_rpart, test_set)
rpart_acc <- mean(rpart_preds >= 0.80*test_set$solution_diversion_potential_tons_2022 & rpart_preds <= 1.20*test_set$solution_diversion_potential_tons_2022)
rpart_acc

# Random forest model
train_rf <- train(solution_diversion_potential_tons_2022 ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 150,
                  tuneGrid = data.frame(mtry = seq(1, 100, 5)))
train_rf$bestTune

rf_preds <- predict(train_rf, test_set)
rf_acc <- mean(rf_preds >= 0.80*test_set$solution_diversion_potential_tons_2022 & rf_preds <= 1.20*test_set$solution_diversion_potential_tons_2022)
rf_acc

# Ensemble model (attempt with rpart and rf)
ensemble_preds <- (rpart_preds + rf_preds)/2
ensemble_acc <- mean(ensemble_preds >= 0.80*test_set$solution_diversion_potential_tons_2022 & ensemble_preds <= 1.20*test_set$solution_diversion_potential_tons_2022)
ensemble_acc

# Collate outputs and build summary table
accuracies <- c(avg_acc, glm_acc, glm_all_acc, knn_acc, knn_cv_acc, rpart_acc, rf_acc, ensemble_acc)
models <- c("mean", "glm-single", "glm-all", "kNN", "kNN cross-validation", "classification tree", "random forest", "ensemble (classification tree + random forest)")

final_table <- data.frame(models, accuracies) %>%
  rename("model" = models, "accuracy (+/- 20%)" = accuracies)
final_table

########## edX CYOP: END ##########