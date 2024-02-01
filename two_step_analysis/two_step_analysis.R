# Preprocessing pipeline for Two-step task
# Author: Lucija Blazevski (l.blazevski@uva.nl), University of Amsterdam
# Date: 09-11-2023

# If you run into any issues, please send an e-mail to l.blazevski@uva.nl


################################################################################
#                     Prepare the workspace                                    #
################################################################################
# Clean the work space
rm(list=ls()) 

# Set working directory (replace with your actual working directory path)

# Name output folder
output_folder <- getwd()

# Round to 3 decimals
options(digits = 3)

# Load libraries
load_and_install_packages <- function(packages) {
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  lapply(packages, require, character.only = TRUE)
}

# List of packages
pkg <- c("ggplot2", "dplyr", "tidyr", "rempsyc", "glue")

# Load and install (if necessary) all packages
load_and_install_packages(pkg)

# Load data
files <- list.files(path = "./data_ny", pattern = "\\.csv$")
data_list <- lapply(paste0("./data_ny/", files), read.csv)

clean_df <- function(df) {
  df$participant <- as.character(df$participant)
  learning_df <- df %>% filter(phase == "learning")
  probe_df <- df %>% filter(phase == "probe")
  posttask_df <- df %>% filter(phase == "post")
  learning_df <- learning_df %>% select("participant", "date", "payoff_list", "phase",
                                    "rare_transition", "rand_sp", 
                                    "planet", "key_resp_exp.keys","key_resp_exp.rt", "p1",
                                    "p2", "p3", 
                                    "p4", "left_alien", "left_probabilty", "right_alien",
                                    "right_probabilty", "key_resp_exp_alien.keys",
                                    "key_resp_exp_alien.rt", "chosen_alien", "rand_money",
                                    "totalgold", "trialgain", "probs")
  probe_df <- probe_df %>% select("participant", "date", "payoff_list", "phase","probe_la",
                                        "static_probability_left", "probe_ra","static_probability_right", 
                                        "key_resp_probe.keys", "key_resp_probe.rt","accuracy")
  post_df <- posttask_df %>% select("participant", "date", "payoff_list", "phase", 
                                  "key_resp.keys", "key_resp.rt", "slider.response", "slider.rt")
  return(list(learning = learning_df, probe = probe_df, post = posttask_df))
}

data_list_clean <- lapply(data_list, clean_df)

# Probe phase analysis ---------------------------------------------------------
probe_df <- function(df) {
  df$participant <- as.character(df$participant)
  probe_df <- df %>% filter(phase == "probe")
  probe_df <- probe_df %>% select("participant", "date", "payoff_list", "phase","probe_la",
                                  "static_probability_left", "probe_ra","static_probability_right", 
                                  "key_resp_probe.keys", "key_resp_probe.rt","accuracy")
  return(probe = as.data.frame(probe_df))
}
all_probe <- lapply(data_list, probe_df)
all_probe <- do.call(rbind, all_probe)

all_probe <- all_probe %>% 
  mutate(accuracy = ifelse(accuracy == "1"|accuracy == "0", accuracy, NA)) %>% 
  mutate(accuracy = as.integer(accuracy)) 

summary_probe <- all_probe %>% 
  group_by(participant) %>% 
  summarize(Accuracy = mean(accuracy, na.rm = T))

large_dif <- all_probe %>% 
  filter((static_probability_left == 0.3 & static_probability_right == 0.7) | (static_probability_left == 0.7 & static_probability_right == 0.3)
         | (static_probability_left == 0.4 & static_probability_right == 0.6) | (static_probability_left == 0.6 & static_probability_right == 0.4)) %>% 
  group_by(participant) %>% 
  summarize(Accuracy = mean(accuracy, na.rm = T))

colnames(summary_probe) <- c('Participant', 'Accuracy')
colnames(large_dif) <- c('Participant', 'Accuracy')

nice_table(summary_probe, title = c("Table 1", "Average accuracy per participant"))
nice_table(large_dif, title = c("Table 2", "Average accuracy per participant for original alien pairs"))

paste0('Average accuracy is: ', all_probe %>% 
  summarize(Accuracy = mean(accuracy, na.rm = T)))

paste0('Average accuracy for original pairs: ', all_probe %>% 
         filter((static_probability_left == 0.3 & static_probability_right == 0.7) | (static_probability_left == 0.7 & static_probability_right == 0.3)
                | (static_probability_left == 0.4 & static_probability_right == 0.6) | (static_probability_left == 0.6 & static_probability_right == 0.4)) %>% 
         summarize(Accuracy = mean(accuracy, na.rm = T)))


# Learning phase ---------------------------------------------------------------
learn_df <- function(df) {
  df$participant <- as.character(df$participant)
  learning_df <- df %>% filter(phase == "learning")
  learning_df <- learning_df %>% select("participant", "date", "payoff_list", "phase",
                                        "rare_transition", "rand_sp", 
                                        "planet", "key_resp_exp.keys","key_resp_exp.rt", "p1",
                                        "p2", "p3", 
                                        "p4", "left_alien", "left_probabilty", "right_alien",
                                        "right_probabilty", "key_resp_exp_alien.keys",
                                        "key_resp_exp_alien.rt", "chosen_alien", "rand_money",
                                        "totalgold", "trialgain", "probs")
  return(learning = as.data.frame(learning_df))
}
all_learn <- lapply(data_list, learn_df)
all_learn <- do.call(rbind, all_learn)

# Add columns for previous reward, previous transition type, and "stay"
all_learn <- all_learn %>%
  group_by(participant) %>%
  mutate(previous_reward = lag(trialgain),
         previous_rare_transition = lag(rare_transition),
         previous_choice = lag(key_resp_exp.keys)) %>%
  ungroup() %>%
  mutate(stay = case_when(previous_choice == key_resp_exp.keys ~ 1,
                          previous_choice != key_resp_exp.keys ~ 0),
         previous_transition = case_when(previous_rare_transition == 1 ~ 'Rare',
                                         previous_rare_transition == 0 ~ 'Common'),
         previous_reward_factor = case_when(previous_reward == 1 ~ 'Reward',
                                            previous_reward == 0 ~ 'No reward'))

# Sanity checks
transition_probs <- all_learn %>%
  group_by(rare_transition) %>%
  summarize(N = n())

reward_probs <- all_learn %>%
  group_by(trialgain) %>%
  summarize(N = n())

theme_set(theme_minimal(base_size = 18))

all_learn$previous_reward_factor <- factor(all_learn$previous_reward_factor,ordered = F)
all_learn$previous_reward_factor <- relevel(all_learn$previous_reward_factor, "Reward")
 
# First plot individual subs
stay_stats <- all_learn %>%
  group_by(previous_reward_factor, previous_transition, participant) %>%
  summarize(mean_stay = mean(stay, na.rm = T),
            n = n()) %>%
  drop_na 

stay_plot_subs <- ggplot(stay_stats, aes(x = previous_reward_factor, 
                                         y = mean_stay,
                                         fill = previous_transition)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  xlab("Outcome of Previous Trial") +
  ylab("Probability of First-Stage Stay") +
  scale_fill_manual(values = c("royalblue4", "firebrick2"), name = "Previous Trial Transition") +
  theme_minimal() +
  facet_wrap(~participant, ncol = 3) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = .2),
        legend.position = "top")
stay_plot_subs

# Save individual sub plot
ggsave(filename = glue("{output_folder}/stay_plot_subs.png"), plot = last_plot(), width = 14, height = 12, units = "in", dpi = 300)

# All together
stay_stats_all <- stay_stats %>%
  group_by(previous_reward_factor, previous_transition) %>%
  summarize(stay_prop = mean(mean_stay),
            sd_stay = sd(mean_stay, na.rm = T),
            N = n(),
            se_stay = sd_stay/sqrt(N))

stay_plot <- ggplot(stay_stats_all, aes(x = previous_reward_factor, 
                                          y = stay_prop,
                                          fill = previous_transition)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_errorbar(position = position_dodge(width = .9), aes(ymin = stay_prop - se_stay, ymax = stay_prop + se_stay), width = 0) + 
  xlab("Outcome of Previous Trial") +
  ylab("Proportion of of First-Stage Stays") +
  coord_cartesian(ylim = c(.5, 1)) +
  scale_fill_manual(values = c("royalblue4", "firebrick2"), name = "Previous Trial Transition") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size = .2),
        legend.position = "top",
        strip.text.x = element_text(size = 10.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9.5)
  )

stay_plot

# Save big plot
ggsave(filename = glue("{output_folder}/stay_plot.png"), plot = last_plot(), width = 14, height = 12, units = "in", dpi = 300)

