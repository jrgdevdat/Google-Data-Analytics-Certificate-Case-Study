# Load cleaned dataset files data -------------------------------------------------

load_cleaned_dataset_files_data <- function(cleaned_dataset_dir_path, cleaned_dataset_files_relevant_metadata) {
  
  # Get list of files of the cleaned dataset directory
  cleaned_dataset_dir_files <- list.files(cleaned_dataset_dir_path)
  
  # Define variable that holds the data
  cleaned_dataset_files_data <- list()
  
  # Loop each file
  for (file_index in 1:length(cleaned_dataset_dir_files)) {
    
    # Get file name in cleaned dataset directory
    cleaned_dataset_dir_file <- cleaned_dataset_dir_files[file_index]
    
    # Get dataset file metadata
    cleaned_dataset_file_relevant_metadata <- cleaned_dataset_files_relevant_metadata[[cleaned_dataset_dir_file]]
    
    # Build the complete file path of the cleaned dataset file
    cleaned_dataset_dir_file_path <- paste(cleaned_dataset_dir_path, "/", cleaned_dataset_dir_file, sep = "")
    # Print file path
    # cat(paste("\n\n**********", file_index, cleaned_dataset_dir_file_path, "\n"))
    
    # Get relevant metadata values
    col_types_value <- cleaned_dataset_files_relevant_metadata[["col_types_options"]]
    
    # Load file content
    cleaned_dataset_dir_file_df <- read_csv(
      cleaned_dataset_dir_file_path, col_types = col_types_value)
    
    # Save data
    cleaned_dataset_files_data[[cleaned_dataset_dir_file]] <- cleaned_dataset_dir_file_df 
    
  }
  
  # Sort list alphabetically according to the elements of the list
  cleaned_dataset_files_data <- cleaned_dataset_files_data[sort(names(cleaned_dataset_files_data))]
  
  # Returns variable holding the data
  return(cleaned_dataset_files_data)
  
}


# Perform analysis and generate the visualizations -------------------------------------------------

perform_analysis <- function(dataset_files_data, analysis_dir_path) {
  
  ####################### Analysis for daily data
  daily_data_df <- dataset_files_data[["daily_data.csv"]]
  
  ## Add new computed variables
  daily_data_transformed_df <- daily_data_df %>%
    mutate(day_of_the_week = wday(Time, label = TRUE, week_start = 1, locale = "en-US.utf8"),
           very_active_hours = VeryActiveMinutes / 60,
           fairly_active_hours = FairlyActiveMinutes / 60,
           lightly_active_hours = LightlyActiveMinutes / 60,
           sedentary_hours = SedentaryMinutes / 60,
           total_hours_asleep = TotalMinutesAsleep / 60,
           total_hours_in_bed = TotalTimeInBed / 60)
  ## Save transformed data
  analysis_file_path <- paste(analysis_dir_path, "/", "daily_data_transformed.csv", sep = "")
  write_csv(daily_data_transformed_df, analysis_file_path)
  
  #### 1.1. Calories summary data per day
  daily_data_analysis <- daily_data_transformed_df %>%
    group_by(day_of_the_week) %>%
    summarize(days_count = n(),
              min_cal = min(Calories),
              fst_quantile_cal = quantile(Calories, probs = c(0.25), names = FALSE),
              median_cal = median(Calories),
              mean_cal = mean(Calories), max_cal = max(Calories),
              third_quantile_cal = quantile(Calories, probs = c(0.75), names = FALSE),
              iqr = quantile(Calories, probs = c(0.75), names = FALSE) - 
                quantile(Calories, probs = c(0.25), names = FALSE),
              max_cal = max(Calories),
              max_diff_cal = max(Calories) - min(Calories),
              sd_cal = sd(Calories))
  ## Save results
  #print("Daily data analysis calculations")
  #print(daily_data_analysis, width = Inf)
  analysis_file_path <- paste(analysis_dir_path, "/", "daily_data_analysis.csv", sep = "")
  write_csv(daily_data_analysis, analysis_file_path)
  
  ## Plot daily calories burned distribution by day
  ggplot(daily_data_transformed_df, aes(y = Calories)) +
    geom_boxplot() +
    # Add ticks with 100 calories separation
    scale_y_continuous(breaks = seq(0, 2600, by = 100), limits = c(0, 2600)) +    
    # Set Y-axis limits
    #ylim(0, 2700) +
    # Hide X-axis
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    # Create several plots per each day of the week
    facet_wrap(~day_of_the_week, nrow = 1) +
    # Give title and subtitle and caption
    labs(title="Calories burned boxplots per day", subtitle="Data for id 6962181067")
  
  plot_file_path <- paste(analysis_dir_path, "/", "boxplots_calories_days.png", sep="")
  ggsave(plot_file_path)
  
  #### 1.2. Activty types summary data per day
  daily_data_activity_hours_per_day <- daily_data_transformed_df %>%
    group_by(day_of_the_week) %>%
    summarize(very_active_hours_median = median(very_active_hours),
              fairly_active_hours_median = median(fairly_active_hours),
              lightly_active_hours_median = median(lightly_active_hours),
              sedentary_hours_median = median(sedentary_hours),
              total_hours_asleep_median = median(total_hours_asleep),
              total_hours_in_bed_median = median(total_hours_in_bed))
  #print("Activity types summary data per day")
  #print(daily_data_activity_hours_per_day, width = Inf)
  
  ## Make activity types summary data long
  daily_data_activity_hours_per_day_long <- daily_data_activity_hours_per_day %>%
    pivot_longer(!day_of_the_week, names_to = "activity_type", values_to = "activity_time")
  
  daily_data_activity_hours_per_day_long$activity_type <- fct(
    daily_data_activity_hours_per_day_long$activity_type,
    levels = c("sedentary_hours_median", "lightly_active_hours_median", "fairly_active_hours_median",
               "very_active_hours_median", "total_hours_asleep_median", "total_hours_in_bed_median"))
  #print(daily_data_activity_hours_per_day_long, n = Inf)
  #glimpse(daily_data_activity_hours_per_day_long)
  
  ## Plot activity types per day
  ggplot(daily_data_activity_hours_per_day_long) +
    geom_col(aes(day_of_the_week, activity_time, fill = activity_type), position = "dodge") +
    scale_y_continuous(breaks = seq(0, 12, by = 1), limits = c(0, 12.5)) +
    # Change activity types labels text in the plot
	  scale_fill_discrete(labels = c(
	    "Sedentary", "Lightly active", "Fairly active", "Very active", "Asleep", "In bed")) +
	  facet_wrap(~day_of_the_week, nrow = 1, scales = "free_x") +
	  theme(axis.title.x = element_blank(),
	        axis.text.x = element_blank(),
	        axis.ticks.x = element_blank(),
	        legend.position = "top") +
	  labs(title = "Activity types median duration per day",
	       subtitle = "Data for id 6962181067",
	       y = "Activity time (hours)",
	       fill = "Activity type") +
	  scale_fill_discrete(labels = c(
	    "Sedentary", "Lightly active", "Fairly active", "Very active", "Asleep", "In bed"))
  
  plot_file_path <- paste(analysis_dir_path, "/", "activity_types_plot.png", sep="")
  ggsave(plot_file_path)
  
  ## Print number of hours of heavy activity (fairly active + very active)
  daily_heavy_activity_hours_per_day <- daily_data_transformed_df %>%
    group_by(day_of_the_week) %>%
    summarize(heavy_activity_hours_median = median(fairly_active_hours + very_active_hours))
  print(daily_heavy_activity_hours_per_day, n = Inf)
  
  ####################### Analysis for hourly data
  hourly_data_df <- dataset_files_data[["hourly_data.csv"]]
  
  ## Add new computed variables
  hourly_data_transformed_df <- hourly_data_df %>%
    mutate(hour_of_the_day = hour(Time))
  
  ## Save transformed data
  analysis_file_path <- paste(analysis_dir_path, "/", "hourly_data_transformed.csv", sep = "")
  write_csv(hourly_data_transformed_df, analysis_file_path)
  
  #### 2.1. Calories summary data per hour
  
  ## Group by hour of the day
  hourly_data_analysis <- hourly_data_transformed_df %>%
    group_by(hour_of_the_day) %>%
    summarize(hours_count = n(),
              min_cal = min(Calories),
              fst_quantile_cal = quantile(Calories, probs = c(0.25), names = FALSE),
              median_cal = median(Calories), 
              mean_cal = mean(Calories),
              third_quantile_cal = quantile(Calories, probs = c(0.75), names = FALSE),
              iqr = quantile(Calories, probs = c(0.75), names = FALSE) - 
                quantile(Calories, probs = c(0.25), names = FALSE),
              max_cal = max(Calories),
              max_diff_cal = max(Calories) - min(Calories),
              sd_cal = sd(Calories))
  ## Print results
  #print("Hourly data analysis calculations")
  #print(hourly_data_analysis, width = Inf, n = 24)
  analysis_file_path <- paste(analysis_dir_path, "/", "hourly_data_analysis.csv", sep = "")
  write_csv(hourly_data_analysis, analysis_file_path)
  
  ## Plot hourly calories distributions
  ggplot(hourly_data_transformed_df, aes(y = Calories)) +
    geom_boxplot() +
    # Add ticks with 100 calories separation
    # scale_y_continuous(breaks = seq(0, 2600, by = 100), limits = c(0, 2600)) +    
    # Set Y-axis limits
    ylim(0, 400) +
    # Hide X-axis
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    # Create several plots per each day of the week
    facet_wrap(~hour_of_the_day, nrow = 4) +
    # Give title and subtitle and caption
    labs(title="Calories burned boxplots per hour", subtitle="Data for id 6962181067")
  
  plot_file_path <- paste(analysis_dir_path, "/", "boxplots_calories_hours.png", sep="")
  ggsave(plot_file_path)
  
  #### 2.2. Total intensity vs. Calories burned
  ggplot(data = hourly_data_transformed_df, aes(x = TotalIntensity, y = Calories)) + 
    geom_point() +
    # Set lower limit for x axis
    xlim(0, NA) +
    # Set limits and ticks for y axis (burned calories)
    scale_y_continuous(breaks = seq(0, 375, by = 50), limits = c(0, 375)) +
    labs(
      title="Total intensity vs. Calories burned",
      subtitle="Hourly data for id 6962181067",
      x = "Total intensity",
      y = "Calories burned")
  
  plot_file_path <- paste(analysis_dir_path, "/", "total_intensity_vs_calories.png", sep="")
  ggsave(plot_file_path)
  
  #### 2.3. Average intensity vs. Calories burned
  ggplot(data = hourly_data_transformed_df, aes(x = AverageIntensity, y = Calories)) + 
    geom_point() +
    # Set lower limit for x axis
    xlim(0, NA) +    
    # Set limits and ticks for y axis (burned calories)
    scale_y_continuous(breaks = seq(0, 375, by = 50), limits = c(0, 375)) +
    labs(
      title="Average intensity vs. Calories burned",
      subtitle="Hourly data for id 6962181067",
      x = "Average intensity",
      y = "Calories burned")
  
  plot_file_path <- paste(analysis_dir_path, "/", "average_intensity_vs_calories.png", sep="")
  ggsave(plot_file_path)
  
  #### 2.4. Total steps vs. Calories burned
  ggplot(data = hourly_data_transformed_df, aes(x = StepTotal, y = Calories)) + 
    geom_point() +
    # Set lower limit for x axis
    xlim(0, NA) +    
    # Set limits and ticks for y axis (burned calories)
    scale_y_continuous(breaks = seq(0, 375, by = 50), limits = c(0, 375)) +
    labs(
      title="Total steps vs. Calories burned",
      subtitle="Hourly data for id 6962181067",
      x = "Total steps",
      y = "Calories burned")
  
  plot_file_path <- paste(analysis_dir_path, "/", "total_steps_vs_calories.png", sep="")
  ggsave(plot_file_path)
  
  #### 2.5. Average heartrate vs. Calories burned
  ggplot(data = hourly_data_transformed_df, aes(x = AverageHeartrate, y = Calories)) + 
    geom_point() +
    # Set lower limit for x axis
    xlim(0, NA) +    
    # Set limits and ticks for y axis (burned calories)
    scale_y_continuous(breaks = seq(0, 375, by = 50), limits = c(0, 375)) +
    labs(
      title="Average heart rate vs. Calories burned",
      subtitle="Hourly data for id 6962181067",
      x = "Average heart rate (beats per minute)",
      y = "Calories burned")
  
  plot_file_path <- paste(analysis_dir_path, "/", "average_heartrate_vs_calories.png", sep="")
  ggsave(plot_file_path)
  
  #### 2.6. Average MET (Metabolic Equivalent of Task) vs. Calories burned
  ggplot(data = hourly_data_transformed_df, aes(x = AverageMETs, y = Calories)) + 
    geom_point() +
    # Set lower limit for x axis
    xlim(0, NA) +    
    # Set limits and ticks for y axis (burned calories)
    scale_y_continuous(breaks = seq(0, 375, by = 50), limits = c(0, 375)) +
    labs(
      title="Average Metabolic Equivalent of Task vs. Calories burned",
      subtitle="Hourly data for id 6962181067",
      x = "Average Metabolic Equivalent of Task",
      y = "Calories burned")
  
  plot_file_path <- paste(analysis_dir_path, "/", "average_mets_vs_calories.png", sep="")
  ggsave(plot_file_path)
  
}


# Define variable that holds relevant metadata for the cleaned dataset files ---------------------------------------------------------------
cleaned_dataset_files_relevant_metadata <- list(
  "daily_data.csv" = list(
    col_types_options = list(
      Time = col_date(format = "%Y-%m-%d"),
      .default = col_double())
  ),
  "hourly_data.csv" = list(
    col_types_options = list(
      Time = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
      .default = col_double())
  )
)
