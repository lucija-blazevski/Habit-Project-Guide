# Preprocessing pipeline for AHT
# Author: Lucija Blazevski (l.blazevski@uva.nl), University of Amsterdam
# Date: 23-01-2024

################################################################################
#                     Prepare the workspace                                    #
################################################################################
# Set working directory (replace with your actual working directory path)
setwd("C:/Users/Lucija/Desktop/AHT_analysis")

# Clean the work space
rm(list=ls()) 

# Round to 3 decimals
options(digits = 3)

# Load libraries
load_and_install_packages <- function(packages) {
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  lapply(packages, require, character.only = TRUE)
}

# List of packages
pkg <- c("ggplot2", "dplyr", "tidyr")

# Load and install (if necessary) all packages
load_and_install_packages(pkg)

# Load data
files <- list.files(path = "./data", pattern = "\\.csv$")
data_list <- lapply(paste0("./data/", files), read.csv)

# Define a function to extract CR meanings and update rating_condition
extract_cr_meanings <- function(df) {
  cr1_meaning <- df[1, 'CR1']
  cr2_meaning <- df[1, 'CR2']
  cr3_meaning <- df[1, 'CR3']
  
  df$rating_condition <- sapply(df$rating_condition, function(x) {
    if (x == 'CR1') cr1_meaning
    else if (x == 'CR2') cr2_meaning
    else if (x == 'CR3') cr3_meaning
    else x
  })
  
  return(df)
}

# Define the analysis function
process_data <- function(df) {
  
  # Extract participant ID
  participant_id <- df$participant[1] 
  
  # Extract deval indicator and determine disallowed keys
  deval_indicator <- na.omit(df$deval)[1]
  disallowed_key_training_after_deval <- ifelse(deval_indicator == 1, "x", "m")
  disallowed_key_after_deval <- ifelse(deval_indicator == 1, "m", "x")
  
  # Training Block Analysis
  training_data <- df %>% 
    filter(phase == 'training') %>% 
    mutate(is_safe = condition_new_t == 'safe')
  training_accuracy_overall <- mean(training_data$correct, na.rm = TRUE)
  training_accuracy_without_safe <- mean(training_data$correct[!training_data$is_safe], na.rm = TRUE)
  training_safe_keypresses <- sum(training_data$is_safe & training_data$key_resp_trial_training.keys != '')
  training_rt_overall <- mean(training_data$key_resp_trial_training.rt, na.rm = TRUE)
  training_rt_without_safe <- mean(training_data$key_resp_trial_training.rt[!training_data$is_safe], na.rm = TRUE)
  training_participant <- training_data$participant
  
  # Training After Devaluation Block Analysis
  # Assuming similar analysis as training block, adjusted for training after devaluation
  training_after_deval_data <- df %>% 
    filter(phase == 'training_after_deval') %>% 
    mutate(is_safe = condition_new_t2 == 'safe')
  training_after_deval_rt_overall <- mean(training_after_deval_data$key_resp_trial_training.rt, na.rm = TRUE)
  training_after_deval_accuracy_overall <- mean(training_after_deval_data$correct, na.rm = TRUE)
  training_after_deval_accuracy_without_safe <- mean(training_after_deval_data$correct[!training_after_deval_data$is_safe], na.rm = TRUE)
  training_after_deval_safe_keypresses <- sum(training_after_deval_data$is_safe & training_after_deval_data$key_resp_trial_training.keys != '')
  training_after_deval_wrong_keypress_count <- sum(training_after_deval_data$key_resp_trial_training.keys == disallowed_key_training_after_deval)
  training_after_deval_total_keypresses <- training_after_deval_data %>% 
    filter(!is.na(key_resp_trial_training.keys) & key_resp_trial_training.keys != '') %>%
    nrow()
  training_after_deval_wrong_keypress_proportion <- training_after_deval_wrong_keypress_count / training_after_deval_total_keypresses
  training_after_deval_participant <- training_after_deval_data$participant
  
  # Real Task Block Analysis
  real_task_data <- df %>%
    filter(phase == 'real_task') %>% 
    mutate(is_safe = condition_new == 'safe')
  real_task_accuracy_overall <- mean(real_task_data$correct, na.rm = TRUE)
  real_task_accuracy_without_safe <- mean(real_task_data$correct[real_task_data$condition_new != 'safe'], na.rm = TRUE)
  real_task_safe_keypresses <- sum(real_task_data$is_safe & real_task_data$key_resp_trial.keys != '')
  real_task_rt_overall <- mean(real_task_data$key_resp_trial.rt, na.rm = TRUE)
  real_task_rt_without_safe <- mean(real_task_data$key_resp_trial.rt[!real_task_data$is_safe], na.rm = TRUE)
  real_task_participant <- real_task_data$participant
  
  # Training After Devaluation Block Analysis
  # Assuming similar analysis as training block, adjusted for training after devaluation
  real_task_after_deval_data <- df %>%
    filter(phase == 'real_task_after_deval') %>% 
    mutate(is_safe = condition_new == 'safe')
  real_task_after_deval_rt_overall <- mean(real_task_after_deval_data$key_resp_trial.rt, na.rm = TRUE)
  real_task_after_deval_accuracy_overall <- mean(real_task_after_deval_data$correct, na.rm = TRUE)
  real_task_after_deval_accuracy_without_safe <- mean(real_task_after_deval_data$correct[!real_task_after_deval_data$is_safe], na.rm = TRUE)
  real_task_after_deval_safe_keypresses <- sum(real_task_after_deval_data$is_safe & real_task_after_deval_data$key_resp_trial.keys != '')
  real_task_after_deval_wrong_keypress_count <- sum(real_task_after_deval_data$key_resp_trial.keys == disallowed_key_after_deval)
  real_task_after_deval_total_keypresses <- real_task_after_deval_data %>% 
    filter(!is.na(key_resp_trial.keys) & key_resp_trial.keys != '') %>%
    nrow()
  real_task_after_deval_wrong_keypress_proportion <- real_task_after_deval_wrong_keypress_count / real_task_after_deval_total_keypresses
  real_task_after_participant <- real_task_after_deval_data$participant
  
  # Urge Analysis per Stimulus
  urge_responses <- df %>%
    filter(!is.na(urge_stim)) %>%
    summarize(participant = participant_id, urge_response = mean(slider_urge_rating.response, na.rm = TRUE))
  
  # Urge Suppression Analysis per Stimulus
  urge_suppression_responses <- df %>%
    filter(!is.na(urge_stim) | !(urge_stim == '')) %>%
    summarize(participant = participant_id, urge_suppression_response = mean(slider_urge_sup_rating.response, na.rm = TRUE))
  
  # Update the data frame with CR meanings
  df <- extract_cr_meanings(df)
  
  expectancy_ratings <- df %>%
    filter(!(rating_condition == '')) %>%
    group_by(rating_condition) %>%
    mutate(rating_order = ifelse(phase == 'rating_expectancy', 'after_deval', as.character(row_number()))) %>%
    mutate(slider_response = case_when(rating_order == 'after_deval'~ slider_exp_rating.response, TRUE ~ slider.response)) %>% 
    select(participant, rating_order, rating_condition, slider_response) %>% 
    mutate (disallowed_key_after_deval = disallowed_key_after_deval)
  
  # Return the results as separate elements in a list
  return(list(
    training_results = data.frame(
      participant = participant_id,  
      training_accuracy_overall = training_accuracy_overall,
      training_accuracy_without_safe = training_accuracy_without_safe,
      training_safe_keypresses = training_safe_keypresses,
      training_rt_overall = training_rt_overall,
      training_after_deval_accuracy_overall = training_after_deval_accuracy_overall,
      training_after_deval_rt_overall = training_after_deval_rt_overall,
      training_after_deval_accuracy_without_safe = training_after_deval_accuracy_without_safe,
      training_after_deval_safe_keypresses = training_after_deval_safe_keypresses, 
      training_after_deval_wrong_keypress_count = training_after_deval_wrong_keypress_count,
      training_after_deval_total_keypresses = training_after_deval_total_keypresses,
      training_after_deval_wrong_keypress_proportion = training_after_deval_wrong_keypress_proportion),
    real_task_results = data.frame(
      participant = participant_id, 
      real_task_accuracy_overall = real_task_accuracy_overall,
      real_task_accuracy_without_safe = real_task_accuracy_without_safe,
      real_task_safe_keypresses = real_task_safe_keypresses,
      real_task_rt_overall = real_task_rt_overall,
      real_task_rt_without_safe = real_task_rt_without_safe,
      real_task_after_deval_accuracy_overall = real_task_after_deval_accuracy_overall,
      real_task_after_deval_rt_overall =  real_task_after_deval_rt_overall,
      real_task_after_deval_accuracy_without_safe = real_task_after_deval_accuracy_without_safe, 
      real_task_after_deval_safe_keypresses = real_task_after_deval_safe_keypresses, 
      real_task_after_deval_wrong_keypress_count = real_task_after_deval_wrong_keypress_count, 
      real_task_after_deval_total_keypresses = real_task_after_deval_total_keypresses, 
      real_task_after_deval_wrong_keypress_proportion = real_task_after_deval_wrong_keypress_proportion),
    expectancy_ratings = expectancy_ratings, 
    urge_responses = urge_responses,
    urge_suppression_responses = urge_suppression_responses
  ))
}

# Apply the process_data function to each dataframe in the list
results_list <- lapply(data_list, process_data)

# Extract and combine results into separate dataframes
combined_training_results <- do.call(rbind, lapply(results_list, function(x) x$training_results))
combined_real_task_results <- do.call(rbind, lapply(results_list, function(x) x$real_task_results))
combined_expectancy_ratings <- do.call(rbind, lapply(results_list, function(x) x$expectancy_ratings))
combined_urge_responses <- do.call(rbind, lapply(results_list, function(x) x$urge_responses))
combined_urge_suppression_responses <- do.call(rbind, lapply(results_list, function(x) x$urge_suppression_responses))

# View the combined results
print(combined_training_results)
print(combined_real_task_results)
print(combined_expectancy_ratings)
print(combined_urge_responses)
print(combined_urge_suppression_responses)

# Assuming combined_training_results and combined_real_task_results have 'participant' column now
accuracy_data <- rbind(
  data.frame(phase = 'Training', accuracy = combined_training_results$training_accuracy_overall, participant = combined_training_results$participant),
  data.frame(phase = 'Training after deval', accuracy = combined_training_results$training_after_deval_accuracy_overall, participant = combined_training_results$participant),
  data.frame(phase = 'Real task', accuracy = combined_real_task_results$real_task_accuracy_overall, participant = combined_real_task_results$participant),
  data.frame(phase = 'Real task after deval', accuracy = combined_real_task_results$real_task_after_deval_accuracy_overall, participant = combined_real_task_results$participant)
)
accuracy_data$phase <- factor(accuracy_data$phase, levels = c("Training", "Training after deval", "Real task", "Real task after deval"))

ggplot(accuracy_data, aes(x = phase, y = accuracy, color = participant, group = participant)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Trend of participant accuracy across phases", x = "Phase", y = "Accuracy", color = 'Participant')
ggsave("C:/Users/Lucija/Desktop/AHT_analysis/accuracy_plot_curve.png", plot = last_plot(), width = 10, height = 6)

average_accuracy_data <- accuracy_data %>%
  group_by(phase) %>%
  summarize(mean_accuracy = mean(accuracy))

# Add a spacer in the factor levels
average_accuracy_data$phase <- factor(average_accuracy_data$phase,
                                      levels = c("Training", 
                                                 "Training after deval", 
                                                 "Spacer",  # Spacer between groups
                                                 "Real task", 
                                                 "Real task after deval"))

# Add a row for the spacer in the data
spacer_row <- data.frame(phase = "Spacer", mean_accuracy = NA)
average_accuracy_data <- rbind(average_accuracy_data, spacer_row)

ggplot(average_accuracy_data, aes(x = phase, y = mean_accuracy, fill = phase)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE) +
  scale_fill_manual(values = c("Training" = "skyblue", 
                               "Training after deval" = "steelblue",
                               "Spacer" = "transparent",  # Make spacer transparent
                               "Real task" = "lightgreen", 
                               "Real task after deval" = "darkgreen"),
                    breaks = c("Training", "Training after deval", "Real task", "Real task after deval")) +
  theme_minimal() +
  labs(title = "Average accuracy across phases", x = "Phase", y = "Average accuracy", fill = 'Phase') +
  scale_x_discrete(breaks = c("Training", "Training after deval", "Real task", "Real task after deval")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text angle for better visibility
ggsave("C:/Users/Lucija/Desktop/AHT_analysis/accuracy_plot.png", plot = last_plot(), width = 10, height = 6)

# RT ---------------------------------------------------------------------------
# Assuming combined_training_results and combined_real_task_results have 'participant' column now
rt_data <- rbind(
  data.frame(phase = 'Training', rt = combined_training_results$training_rt_overall, participant = combined_training_results$participant),
  data.frame(phase = 'Training after deval', rt = combined_training_results$training_after_deval_rt_overall, participant = combined_training_results$participant),
  data.frame(phase = 'Real task', rt = combined_real_task_results$real_task_rt_overall, participant = combined_real_task_results$participant),
  data.frame(phase = 'Real task after deval', rt = combined_real_task_results$real_task_after_deval_rt_overall, participant = combined_real_task_results$participant)
)
rt_data$phase <- factor(rt_data$phase, levels = c("Training", "Training after deval", "Real task", "Real task after deval"))

ggplot(rt_data, aes(x = phase, y = rt, color = participant, group = participant)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Trend of participant RT across phases", x = "Phase", y = "RT", color = 'Participant')
ggsave("C:/Users/Lucija/Desktop/AHT_analysis/rt_plot_curve.png", plot = last_plot(), width = 10, height = 6)

average_rt_data <- rt_data %>%
  group_by(phase) %>%
  summarize(mean_rt = mean(rt))

# Add a spacer in the factor levels
average_rt_data$phase <- factor(average_rt_data$phase,
                                      levels = c("Training", 
                                                 "Training after deval", 
                                                 "Spacer",  # Spacer between groups
                                                 "Real task", 
                                                 "Real task after deval"))

# Add a row for the spacer in the data
spacer_row <- data.frame(phase = "Spacer", mean_rt = NA)
average_rt_data <- rbind(average_rt_data, spacer_row)

ggplot(average_rt_data, aes(x = phase, y = mean_rt, fill = phase)) +
  geom_bar(stat = "identity", position = position_dodge(), na.rm = TRUE, width = 0.5) +
  scale_fill_manual(values = c("Training" = "skyblue", 
                               "Training after deval" = "steelblue",
                               "Spacer" = "transparent",  # Make spacer transparent
                               "Real task" = "lightgreen", 
                               "Real task after deval" = "darkgreen"),
                    breaks = c("Training", "Training after deval", "Real task", "Real task after deval")) +
  theme_minimal() +
  labs(title = "Average RT across phases", x = "Phase", y = "Average RT", fill = 'Phase') +
  scale_x_discrete(breaks = c("Training", "Training after deval", "Real task", "Real task after deval")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text angle for better visibility
ggsave("C:/Users/Lucija/Desktop/AHT_analysis/rt_plot.png", plot = last_plot(), width = 10, height = 6)

# Safe responses
# Average responses for safe trials in training and real task
avg_safe_responses_training <- mean(combined_training_results$training_safe_keypresses, na.rm = TRUE)
avg_safe_responses_real_task <- mean(combined_real_task_results$real_task_safe_keypresses, na.rm = TRUE)

# Create a data frame for plotting
avg_safe_responses_df <- data.frame(
  Phase = c("Training", "Real task"),
  avg_safe_responses = c(avg_safe_responses_training, avg_safe_responses_real_task)
)

# Collapsed across Training and Real Task
ggplot(avg_safe_responses_df, aes(x = Phase, y = avg_safe_responses, fill = Phase)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = "Average safe trial responses (before devaluation)", x = "Phase", y = "Average number of responses")
ggsave("C:/Users/Lucija/Desktop/AHT_analysis/safe_trial_responses_plot.png", plot = last_plot(), width = 10, height = 6)

# Calculate non-devalued (correct and not safe) keypresses
combined_real_task_results$non_devalued_keypresses <- combined_real_task_results$real_task_after_deval_total_keypresses - 
  combined_real_task_results$real_task_after_deval_safe_keypresses -
  combined_real_task_results$real_task_after_deval_wrong_keypress_count

# Average number of devalued keypresses
avg_devalued_keypresses <- mean(combined_real_task_results$real_task_after_deval_wrong_keypress_count, na.rm = TRUE)

# Average number of non-devalued keypresses
avg_non_devalued_keypresses <- mean(combined_real_task_results$non_devalued_keypresses, na.rm = TRUE)

# Create a data frame for plotting
avg_keypresses_data <- data.frame(
  keypress_type = c("Devalued", "Non-devalued"),
  avg_keypresses = c(avg_devalued_keypresses, avg_non_devalued_keypresses)
)
# Bar Plot for Average Keypresses
ggplot(avg_keypresses_data, aes(x = keypress_type, y = avg_keypresses, fill = keypress_type)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Average devalued vs non-devalued keypresses after devaluation", x = "CS", y = "Average number of keypresses", fill = 'CS')
ggsave("C:/Users/Lucija/Desktop/AHT_analysis/devalued_non_devalued_keypresses_plot.png", plot = last_plot(), width = 10, height = 6)
