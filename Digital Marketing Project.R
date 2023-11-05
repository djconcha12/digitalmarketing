####################################################
#### 15.570 - Digital Marketing                 ####          
#### Project Group: Domingo, Teresita, Marga    #### 
####                                            ####
####################################################

library(MatchIt)
library(AER)

# Clear existing data
rm(list=ls())

# Load data-set
highnote_readable <- file.choose()
df_full <- read.csv(highnote_readable)
df_full

# Selecting variables of interest
selected_vars <- c("age", "male", "friend_cnt", "avg_friend_age", "avg_friend_male",
                   "friend_country_cnt", "subscriber_friend_cnt", "songsListened",
                   "lovedTracks", "posts", "playlists", "shouts", "adopter")

df <- df_full[, selected_vars]

# Checking structure of data
str(df) # No strings on data

# Separate data between adopters and non-adopters
split_data <- split(df, df$adopter)

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

# Apply the function to each subset of the data
grouped_summary <- lapply(split_data, summary_stats)

# Combine the results into a single data frame
combined_summary <- do.call(rbind, grouped_summary)

# Convert row names to a column if they are meaningful (e.g., the levels of 'adopters')
combined_summary <- data.frame(Adopters = rownames(combined_summary), combined_summary, row.names = NULL)

# Now use the View() function to open a new tab in RStudio with this table
View(combined_summary)


help(lapply)