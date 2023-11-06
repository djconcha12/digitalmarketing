####################################################
#### 15.570 - Digital Marketing                 ####          
#### Project Group: Domingo, Teresita, Marga    #### 
####                                            ####
####################################################

# Clear existing data
rm(list=ls())

library(MatchIt)
library(AER)
library(dplyr)

# Load data-set
highnote_db_curated <- file.choose()
df_full <- read.csv(highnote_db_curated)
df_full

# Checking structure of data
str(df_full) # No strings on data

# We define groups of variables to work with the Datasets
# Pre variables are defined based on delta_pre + pre = current
# Post variables are defined based on current + delta_post = post

general_vars <- c('age','male','tenure','adopter','net_user')

current_period_vars <- c('good_country','shouts','playlists','posts','lovedTracks',
                         'songsListened','subscriber_friend_cnt','friend_country_cnt',
                         'avg_friend_male','avg_friend_age','friend_cnt')

delta_pre_period_vars <- c('delta1_good_country','delta1_shouts','delta1_playlists',
                     'delta1_posts','delta1_lovedTracks','delta1_songsListened',
                     'delta1_subscriber_friend_cnt','delta1_friend_country_cnt',
                     'delta1_avg_friend_male','delta1_avg_friend_age','delta1_friend_cnt')

delta_post_period_vars <- c('delta2_good_country','delta2_shouts','delta2_playlists',
                      'delta2_posts','delta2_lovedTracks','delta2_songsListened',
                      'delta2_subscriber_friend_cnt','delta2_friend_country_cnt',
                      'delta2_avg_friend_male','delta2_avg_friend_age','delta2_friend_cnt')

pre_period_vars <- c('good_country_pre','shouts_pre','playlists_pre','posts_pre',
                     'lovedTracks_pre','songsListened_pre','subscriber_friend_cnt_pre',
                     'friend_country_cnt_pre','avg_friend_male_pre','avg_friend_age_pre','friend_cnt_pre')


post_period_vars <- c('good_country_post','shouts_post','playlists_post','posts_post',
                      'lovedTracks_post','songsListened_post','subscriber_friend_cnt_post',
                      'friend_country_cnt_post','avg_friend_male_post','avg_friend_age_post','friend_cnt_post')

# Define variables to be considered on each df
pre_vars <- c(general_vars, pre_period_vars)
current_vars <- c(general_vars, current_period_vars)
post_vars <- c(general_vars, post_period_vars)

# Define df for each period based on variables
df_pre <- df_full[, pre_vars]
df_current <-df_full[, current_vars]
df_post <- df_full[, post_vars]

# Define a function to calculate summary statistics
summary_stats <- function(data) {
  data_summary <- sapply(data, function(x) {
    if(is.numeric(x)) {
      c(mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        missing = sum(is.na(x)))
    } else {
      return(NA)
    }
  })
  return(data_summary)
}

# Define a function to print results in a table

print_table_stats <- function(data) {
  # Separate data between adopters and non-adopters
  adopter_groups <- split(data, data$adopter)
  
  # Apply the function to each subset of the data
  grouped_summary <- lapply(adopter_groups, summary_stats)
  
  # Convert each named vector into a data frame
  grouped_summary <- lapply(grouped_summary, as.data.frame)
  
  # Combine the results into a single data frame
  combined_summary <- do.call(rbind, grouped_summary)
  
  # Convert row names to a column if they are meaningful (e.g., the levels of 'adopters')
  combined_summary <- data.frame(Adopters = rownames(combined_summary), combined_summary, row.names = NULL)
  
  # Return the combined summary
  return(combined_summary)
}

# Function to get correlation of variables with 'adopter' in df 'data'
correlation_sorted <- function(data){
  correlation_pre <- sapply(df_pre_filtered, function(x) cor(data$adopter, x))  
  sorted_correlation_pre <- sort(correlation_pre, decreasing = TRUE)
  return(sorted_correlation_pre)
}

# Is important to mention that the delta variables do have value to predict behaviour
# We will recall to this later making new datasets that include both the absulte and delta variables

# Exclude the variables



################################################
######### PRE TIME-LINE ####################
################################################

# Calculating averages
table_stats_pre <- print_table_stats(df_pre)
View(table_stats_pre)

# Filtered non-numeric variables
df_pre_filtered <- select(df_pre, -net_user)

# Covariance Matrix
cov_matrix_pre <- cov(df_pre_filtered)
View(cov_matrix_pre)

# Correl Matrix
corr_matrix_pre <- cor(df_pre_filtered)
View(corr_matrix_pre)

# Correl Vector
correl_df_pre_adopter <- correlation_sorted(df_pre_filtered)
View(correl_df_pre_adopter)

# Now we build regression models to predict what are the most important variables to be an adopter
# IMPORTANT: CAN WE RUN THIS EXPERIMENTS IN MIT SERVERS?

# We get a sample to reduce the size of the experiment for now
df_pre_sample <- df_pre[sample(1:nrow(df_pre), 10000, replace=FALSE),]

# We run a Logistic Model since adopter is a binary variable
LogisticModel_pre = glm(adopter ~ lovedTracks_pre + friend_country_cnt_pre + songsListened_pre + subscriber_friend_cnt_pre + friend_cnt_pre + shouts_pre + avg_friend_age_pre, data = df_pre_sample, family = binomial)
summary(LogisticModel_pre)

# We filter variable with minimal importance
LogisticModel_pre_2 = glm(adopter ~ lovedTracks_pre + songsListened_pre + subscriber_friend_cnt_pre + friend_cnt_pre, data = df_pre_sample, family = binomial)
summary(LogisticModel_pre_2)

exp(coef(LogisticModel_pre_2))
# From the Odds ratio for each independent variable, we see that subscriber_friend_cnt_pre make a big difference
# in odds of success of the dependent variable, given a unit # increase. I.e. the more friends a user have being 
# subscribers, the more probability the user will become a subscriber

#### Propensity Matching Score #####

df_pre_sample <- replace(df_pre_sample, is.na(df_pre_sample), 0)

# Do the propensity match just with the variables that are important in the glm model
matchprocess_pre = matchit(adopter ~ lovedTracks_pre + songsListened_pre + subscriber_friend_cnt_pre + friend_cnt_pre, 
                       data = df_pre_sample, method = "nearest", ratio = 1)

summary(matchprocess_pre)

# Export the matched observations to a new dataframe
matchdata_pre <- match.data(matchprocess_pre) 

# See how this new 'matched' dataset now has half adopters and half non adopters
table(matchdata_pre$adopter)
View(matchdata_pre)

# Explanation of results

# Summary of Balance for Matched Data: This section shows the means, SMDs, variance ratios, eCDF statistics, and standardized pair distances for each covariate after matching.
# 
# Means Treated/Control: These columns show the means of each covariate for the treated and control groups in the matched sample.
# Std. Mean Diff.: This column shows the SMD for each covariate in the matched sample. A smaller SMD indicates better balance between the groups.
# Var. Ratio: This column shows the ratio of the variances between the treated and control groups in the matched sample for each covariate. A value close to 1 indicates good balance.
# eCDF Mean/Max: These columns show the mean and maximum values of the eCDF statistics in the matched sample. Smaller values indicate better balance.
# Std. Pair Dist.: This column shows the standardized pair distance, which measures the distance between matched pairs. A smaller value indicates better balance.

# for more information run 
help(matchit)

#### Instrumental Variables #####

# Explanation
help(ivreg)
# https://en.wikipedia.org/wiki/Instrumental_variables_estimation

# Essentially we want to find variables that are correlated with the endogenous variables of the regression
# But are not correlated with the error term, in essence does not have an impact on clients being adopters
# We can find this variables in the covariance matrix calculated at the beggining

reg_model_subscriber_friends <- lm(subscriber_friend_cnt_pre ~ shouts_pre + friend_country_cnt_pre + avg_friend_age_pre, data = df_pre_sample)
summary(reg_model_subscriber_friends)

# friend_country_cnt_pre is a good instrumental variable for subscriber_friend_cnt_pre
# Then we re-do the regression for adopters with the instrumental variable

# You must have at least the same number of IV than variables (or covariants)

ivmodel<- ivreg(formula = adopter ~ lovedTracks_pre + songsListened_pre + subscriber_friend_cnt_pre |
                  shouts_pre + friend_country_cnt_pre + avg_friend_age_pre, data = df_pre_sample)

summary(ivmodel)

# I still don't know how to interpret the results of instrumental variables


################################################
######### CURRENT TIME-LINE ####################
################################################

# Repeat same process

table_stats_current <- print_table_stats(df_current)
View(table_stats_current)

################################################
######### POST TIME-LINE ####################
################################################

# Repeat same process

table_stats_post <- print_table_stats(df_post)
View(table_stats_post)

################################################
######### PRE TIME-LINE + DELTA PRE VARIABLES ##
################################################

# We will see what specific change in 
