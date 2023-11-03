# Show dataset files info -------------------------------------------------

print_dataset_files_info <- function(dataset_files_data, show_unique_id_values = TRUE) {
  
  # Print datasets info
  for (file_index in 1:length(dataset_files_data)) {
    
    # Print file name
    dataset_dir_file <- names(dataset_files_data)[file_index]
    cat(paste("\n\n**********", file_index, "-", dataset_dir_file, "\n"))
    
    # Show internal structure with glimpse
    dataset_dir_file_df <- dataset_files_data[[file_index]] 
    cat("\n* Output of glimpse command:\n")
    glimpse(dataset_dir_file_df)
    
    # Print list of Id values sorted alphabetically (1 per line) and how many they are
    if (show_unique_id_values) {
      unique_Id_file_values <- dataset_dir_file_df$Id[!duplicated(dataset_dir_file_df$Id)]
      sorted_unique_Id_file_values <- sort(unique_Id_file_values, na.last = FALSE)
      cat(paste("\n* List of Id values sorted alphabetically (", length(sorted_unique_Id_file_values), "):\n"))
      cat(sorted_unique_Id_file_values, sep = "\n")
    }
    
    # Show number of records for each date value
    dataset_dir_file_date_groups <- dataset_dir_file_df %>%
      group_by(as_date(Time)) %>%
      summarize(count_date_records = n())    
    print(dataset_dir_file_date_groups, n = 100)
    
  }
  
}


# Load dataset files data -------------------------------------------------

load_dataset_files_data <- function(dataset_dir_path, dataset_files_relevant_metadata) {
  
  # Get list of files of the dataset directory
  dataset_dir_files <- list.files(dataset_dir_path)
  
  # Define variable that holds the data
  dataset_files_data <- list()
  
  # Loop each file
  for (file_index in 1:length(dataset_dir_files)) {
    
    # Get file name in dataset directory
    dataset_dir_file <- dataset_dir_files[file_index]
    
    # Get dataset file metadata
    dataset_file_relevant_metadata <- dataset_files_relevant_metadata[[dataset_dir_file]]
    
    # Check if file should be loaded. If not, go to next iteration
    if (!dataset_file_relevant_metadata[["load_data"]]) {
      next
    }
    
    # Build the complete file path of the dataset file
    dataset_dir_file_path <- paste(dataset_dir_path, "/", dataset_dir_file, sep = "")
    # Print file path
    # cat(paste("\n\n**********", file_index, dataset_dir_file_path, "\n"))
    
    # Get relevant metadata values
    col_types_value <- dataset_file_relevant_metadata[["col_types_options"]]
    time_col_name <- dataset_file_relevant_metadata[["time_col_name"]]
    
    # Load file content
    dataset_dir_file_df <- read_csv(
      dataset_dir_file_path, col_types = col_types_value,
      # Select columns while also renaming one of them as Time in order to standarize time-related names
      col_select = c(all_of(c(Time = time_col_name)), everything()))
    
    # Save data
    dataset_files_data[[dataset_dir_file]] <- dataset_dir_file_df 
    
  }
  
  # Sort list alphabetically according to the elements of the list
  dataset_files_data <- dataset_files_data[sort(names(dataset_files_data))]
  
  # Returns variable holding the data
  return(dataset_files_data)
  
}


# Check for missing values in the datasets --------------------------------

check_missing_values <- function(dataset_files_data) {
  
  for (file_index in 1:length(dataset_files_data)) {
    
    # Get dataset file data
    dataset_file_df <- dataset_files_data[[file_index]]
    
    # Check for missing values in dataset file data
    dataset_file_df_missing_values_results <- sapply(dataset_file_df, anyNA)
    
    # Print file name
    dataset_dir_file <- names(dataset_files_data)[file_index]
    cat(paste("\n\n**********", file_index, "-", dataset_dir_file, "\n"))
    
    # Print info about the columns with NA values found
    columns_with_NA_values <- names(dataset_file_df_missing_values_results)[dataset_file_df_missing_values_results]
    if (length(columns_with_NA_values) > 0) {
      cat("***** Columns with NA values:\n")
      for (col_index in 1:length(columns_with_NA_values)) {
        # Get column name
        col_with_NA_values <- columns_with_NA_values[col_index]
        # Print column name
        print(col_with_NA_values)
        # Get logical vector with the positions of elements with NA values for the respective column
        column_indexes_with_NA_values <- is.na(dataset_file_df[[col_with_NA_values]])
        # Print Time values for which the column checked has NA values and their number
        time_values_with_associated_NA_values <- dataset_file_df[["Time"]][column_indexes_with_NA_values]
        number_of_time_values <- length(time_values_with_associated_NA_values)
        cat(paste("Times associated with NA values for the column", col_with_NA_values, "(", number_of_time_values, ")\n"))
        print(time_values_with_associated_NA_values)
      }
    } else {
      print("No columns with missing values found in dataset file")
    }
    
  }
  
}


# Get Id values present in all of the files -------------------------------

get_Id_values_in_all_files <- function(dataset_files_data) {
  
  unique_Id_values_present_in_all_files_so_far <- c()
  
  # Loop each file
  for (file_index in 1:length(dataset_files_data)) {
    
    # Get unique Id values in dataset file
    dataset_file_df <- dataset_files_data[[file_index]]
    dataset_file_Id_groups_df <- dataset_file_df %>%
      group_by(Id) %>%
      group_keys()
    dataset_file_Id_groups <- dataset_file_Id_groups_df[[1]]
    
    # In the first iteration all the dataset file Id groups should be included
    if (file_index == 1) {
      unique_Id_values_present_in_all_files_so_far <- dataset_file_Id_groups
      next
    }
    
    # Check which unique Id values present in all files so far are also in the newest one
    unique_Id_values_indexes_present_in_all_files <- 
      unique_Id_values_present_in_all_files_so_far %in% dataset_file_Id_groups
    
    # Update vector with all the unique Id values present in all files so far
    unique_Id_values_present_in_all_files_so_far <- 
      unique_Id_values_present_in_all_files_so_far[unique_Id_values_indexes_present_in_all_files]
    
    # If the newest file has no common Id values break loop
    if (length(unique_Id_values_present_in_all_files_so_far) == 0) {
      break
    }
    
    #cat("File number:", file_index, "-", length(unique_Id_values_present_in_all_files_so_far), "\n")
    
  }
  
  return(unique_Id_values_present_in_all_files_so_far)
  
}


# Only keep records of the Id values specified ----------------------------

keep_only_records_from_Ids <- function(dataset_files_data, allowed_Ids) {
  
  for (file_index in 1:length(dataset_files_data)) {
    
    # Get dataset file data
    dataset_file_df <- dataset_files_data[[file_index]]
    
    # Update dataset file data with only the records who got one of the Ids specified
    dataset_files_data[[file_index]] <- dataset_file_df %>%
      filter(Id %in% allowed_Ids)
    
  }
  
  return(dataset_files_data)
  
}


# Get biggest common date range among the dataset files and Ids -----------

get_biggest_common_date_range <- function(dataset_files_data) {
  
  biggest_common_date_range <- c()
  
  for (file_index in 1:length(dataset_files_data)) {
    
    # Get time column name for dataset file
    dataset_dir_file <- names(dataset_files_data)[file_index]
    
    # Get dataset file data
    dataset_file_df <- dataset_files_data[[file_index]]
    
    # Get min and max times for each Id in dataset file
    dataset_file_Id_groups_times <- dataset_file_df %>%
      group_by(Id) %>%
      summarize(min_time=min(Time), max_time=max(Time))
    
    # Get min and max common dates for the dataset file
    min_date_dataset_file <- as_date(max(dataset_file_Id_groups_times$min_time))
    max_date_dataset_file <- as_date(min(dataset_file_Id_groups_times$max_time))
    
    # Print info
    # cat(paste("\n\n**********", file_index, "-", dataset_dir_file, "\n"))
    # print(dataset_file_Id_groups_times)
    # print(paste("Min date dataset file:", min_date_dataset_file,
    #             "- Max date dataset file:", max_date_dataset_file))
    
    # If this is the first file, set the min and max dates as the ones from this file
    if (file_index == 1) {
      biggest_common_date_range[1] <- min_date_dataset_file
      biggest_common_date_range[2] <- max_date_dataset_file
      next
    }
    
    # Update min common date
    if (min_date_dataset_file > biggest_common_date_range[1]) {
      biggest_common_date_range[1] <- min_date_dataset_file
    }
    
    # Update max common date
    if (max_date_dataset_file < biggest_common_date_range[2]) {
      biggest_common_date_range[2] <- max_date_dataset_file
    }
    
  }
  
  return(biggest_common_date_range)
  
}


# Only keep records within given date range -------------------------------

keep_only_records_within_given_date_range <- function(date_range, dataset_files_data) {
  
  min_date_allowed <- date_range[1]
  max_date_allowed <- date_range[2]
  
  for (file_index in 1:length(dataset_files_data)) {
    
    # Get dataset file data
    dataset_file_df <- dataset_files_data[[file_index]]
    
    # Get time column name for dataset file
    dataset_dir_file <- names(dataset_files_data)[file_index]
    
    # Update dataset file data with only the records within the given date range
    dataset_files_data[[file_index]] <- dataset_file_df %>%
      filter(as_date(Time) >= min_date_allowed & as_date(Time) <= max_date_allowed)
    
  }
  
  return(dataset_files_data)
  
}


# Join dataset files for daily dataframe ----------------------------------

join_dataset_files_for_daily_data <- function(dataset_files_data) {
  
  # Dataframe for daily data
  #cat("\n\n********** daily_data dataframe\n")
  
  # Joining dailyActivity_merged.csv with sleepDay_merged.csv
  daily_activity_merged_df <- dataset_files_data[["dailyActivity_merged.csv"]]
  sleep_day_merged_df <- dataset_files_data[["sleepDay_merged.csv"]]
  # Cast values of the Time column to date objects
  sleep_day_merged_df$Time <- as_date(sleep_day_merged_df$Time)
  daily_data_join_result <- left_join(daily_activity_merged_df, sleep_day_merged_df, by = join_by(Id, Time))
  
  # Joining joined data with weightLogInfo_merged.csv
  weight_log_info_merged_df <- dataset_files_data[["weightLogInfo_merged.csv"]]
  # Cast values of the Time column to date objects
  weight_log_info_merged_df$Time <- as_date(weight_log_info_merged_df$Time)
  daily_data_df <- left_join(daily_data_join_result, weight_log_info_merged_df, 
                             by = join_by(Id, Time))
  
  # Exclude unwanted columns
  daily_data_df <- daily_data_df %>%
    mutate(Id = NULL, TrackerDistance = NULL, LoggedActivitiesDistance = NULL, TotalSleepRecords = NULL,
           WeightPounds = NULL, Fat = NULL, IsManualReport = NULL, LogId = NULL)
  
  # Print resulting dataframe
  #glimpse(daily_data_df)
  
  return(daily_data_df)
}


# Join dataset files for hourly dataframe ---------------------------------

join_dataset_files_for_hourly_data <- function(dataset_files_data) {
  
  # Dataframe for minute data
  #cat("\n\n********** minute_data dataframe\n")
  
  # Joining hourlyCalories_merged.csv with hourlyIntensities_merged.csv
  hourly_calories_merged_df <- dataset_files_data[["hourlyCalories_merged.csv"]]
  hourly_intensities_merged_df <- dataset_files_data[["hourlyIntensities_merged.csv"]]
  minute_data_join_result <- left_join(hourly_calories_merged_df, hourly_intensities_merged_df,
                                       by = join_by(Id, Time))
  
  # Joining joined data with hourlySteps_merged.csv
  hourly_steps_merged_df <- dataset_files_data[["hourlySteps_merged.csv"]]
  minute_data_join_result_2 <- left_join(minute_data_join_result, hourly_steps_merged_df,
                                         by = join_by(Id, Time))
  
  # Group data from the heartrate_seconds_merged.csv dataframe
  heartrate_seconds_merged_df <- dataset_files_data[["heartrate_seconds_merged.csv"]]
  heartrate_seconds_merged_grouped_df <- heartrate_seconds_merged_df %>% 
    group_by(Rounded_Time=floor_date(Time, unit = "hour"), Id) %>%
    summarize(AverageHeartrate = mean(Value))    
  #print(heartrate_seconds_merged_grouped_df, n=50)
  
  # Joining joined data with the grouped data from heartrate_seconds_merged.csv
  minute_data_join_result_3 <- left_join(minute_data_join_result_2, heartrate_seconds_merged_grouped_df,
                                         by = join_by(Id, Time == Rounded_Time))
  
  # Group data from the minuteMETsNarrow_merged.csv dataframe
  minute_mets_narrow_merged_df <- dataset_files_data[["minuteMETsNarrow_merged.csv"]]
  minute_mets_narrow_merged_grouped_df <- minute_mets_narrow_merged_df %>% 
    group_by(Rounded_Time = floor_date(Time, unit = "hour"), Id) %>%
    summarize(AverageMETs = mean(METs))    
  #print(minute_mets_narrow_merged_grouped_df, n=50)
  
  # Joining joined data with the grouped data from minuteMETsNarrow_merged.csv
  hourly_data_df <- left_join(minute_data_join_result_3, minute_mets_narrow_merged_grouped_df,
                              by = join_by(Id, Time == Rounded_Time))
  
  # Filter out excluded columns
  hourly_data_df <- hourly_data_df %>%
    mutate(Id = NULL)
  
  # Print resulting dataframe
  #glimpse(hourly_data_df)
  
  return(hourly_data_df)
}


# Join dataset files data -------------------------------------------------

join_dataset_files_data <- function(dataset_files_data) {
  
  # Define variable that holds the joined data
  joined_dataset_files_data <- list()
  
  # Create daily data dataframe
  daily_data_df <- join_dataset_files_for_daily_data(dataset_files_data)
  joined_dataset_files_data[["daily_data.csv"]] <- daily_data_df
  
  # Create hourly data dataframe
  hourly_data_df <- join_dataset_files_for_hourly_data(dataset_files_data)
  joined_dataset_files_data[["hourly_data.csv"]] <- hourly_data_df
  
  return(joined_dataset_files_data)
}


# Plot cleaned data -------------------------------------------------------

plot_cleaned_data <- function(cleaned_dataset_files_data, dest_folder_path) {
  
  # Loop dataset files
  for (dataset_file in names(cleaned_dataset_files_data)) {
    
    # Get dataset data
    dataset_file_data <- cleaned_dataset_files_data[[dataset_file]]
    # Get dataset file name
    length_dataset_file <- nchar(dataset_file, type="chars")
    dataset_file_name <- substr(dataset_file, 1, length_dataset_file - 4)
    # Prepare generic plot subtitle
    dataset_plots_subtitle <- paste("From the ", dataset_file_name, " dataframe", sep="")
    
    # Loop variables in dataset file
    for (var_index in 1:length(dataset_file_data)) {
      
      # Prepare variable details for plot
      var_name <- names(dataset_file_data)[var_index]
      var_values <- dataset_file_data[[var_name]]
      boxplot_title <- paste("Boxplot for", var_name)
      var_min_val <- min(var_values, na.rm = TRUE)
      var_max_val <- max(var_values, na.rm = TRUE)
      caption_text <- paste("min:", var_min_val, "max:", var_max_val)
      
      # Plot variable
      ggplot(dataset_file_data, aes(x = .data[[var_name]])) +
        geom_boxplot() +
        labs(title=boxplot_title, subtitle=dataset_plots_subtitle,
             caption=caption_text)
      
      # Save plot
      plot_file_name <- paste(dataset_file_name, "-", var_index, "-", var_name, ".png", sep="")
      plot_file_path <- paste(dest_folder_path, plot_file_name, sep="")
      ggsave(plot_file_path)
      
    }
    
  }
  
}


# Save cleaned data -------------------------------------------------------

save_cleaned_data <- function(cleaned_data, cleaned_dataset_dir_path) {
  
  for (file_index in 1:length(cleaned_data)) {
    
    # Get cleaned dataset file data
    cleaned_dataset_file_data <- cleaned_data[[file_index]]
    
    # Get dataset file name
    cleaned_dataset_file <- names(cleaned_data)[file_index]
    
    # Save dataset file data
    cleaned_dataset_file_path <- paste(cleaned_dataset_dir_path, "/", cleaned_dataset_file, sep = "")
    write_csv(cleaned_dataset_file_data, cleaned_dataset_file_path)
    
  }
  
}


# Define variable that holds relevant metadata for the dataset files ---------------------------------------------------------------
dataset_files_relevant_metadata <- list(
  "dailyActivity_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      ActivityDate = col_date(format = "%m/%d/%Y"),
      .default = col_double()),
    time_col_name = "ActivityDate"
  ),
  "dailyCalories_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityDay = col_date(format = "%m/%d/%Y"),
      .default = col_double()),
    time_col_name = "ActivityDay"
  ),
  "dailyIntensities_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityDay = col_date(format = "%m/%d/%Y"),
      .default = col_double()),
    time_col_name = "ActivityDay"
  ),
  "dailySteps_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(), ActivityDay = col_date(format = "%m/%d/%Y"),
      .default = col_double()),
    time_col_name = "ActivityDay"
  ),
  "heartrate_seconds_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      Time = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "Time"
  ),
  "hourlyCalories_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityHour"
  ),
  "hourlyIntensities_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityHour"
  ),
  "hourlySteps_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityHour"
  ),
  "minuteCaloriesNarrow_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityMinute"
  ),
  "minuteCaloriesWide_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityHour"
  ),
  "minuteIntensitiesNarrow_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityMinute"
  ),
  "minuteIntensitiesWide_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityHour"
  ),
  "minuteMETsNarrow_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityMinute"
  ),
  "minuteSleep_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      date = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "date"
  ),
  "minuteStepsNarrow_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityMinute = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityMinute"
  ),
  "minuteStepsWide_merged.csv" = list(
    load_data = FALSE,
    col_types_options = list(
      Id = col_character(),
      ActivityHour = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "ActivityHour"
  ),
  "sleepDay_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      SleepDay = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      .default = col_double()),
    time_col_name = "SleepDay"
  ),
  "weightLogInfo_merged.csv" = list(
    load_data = TRUE,
    col_types_options = list(
      Id = col_character(),
      Date = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
      IsManualReport = col_logical(),
      .default = col_double()),
    time_col_name = "Date"
  )
)
