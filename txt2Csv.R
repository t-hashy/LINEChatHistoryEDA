# library(tidyverse)
library(dplyr) # Manipulate the data frame
library(lubridate)
library(stringr)

convert_txt_to_df <- function(input_file) {
  # 0. Make a data frame with the columns of [date, time, name, message].
  df <- data.frame(
    date = character(),
    time = character(),
    name = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  
  # 1. Skip first 2 lines and start from the third line.
  lines <- readLines(input_file, encoding = "UTF-8")
  lines <- tail(lines, -2)
  
  date <- NA_character_ # Initialize date outside the loop
  
  # Read lines one by one
  for (line in lines) {
    # 2. If the line is blank, skip it.
    line <- trimws(line)
    if (line == "") {
      next
    }
    
    # 3. If the line starts with shortened weekday abbreviation (Mon, Tue, etc.),
    # extract date from the line which written in the format of day/mon/year
    # (e.g. 12/12/2024) and set the date, and add a new row of on the data frame and set the date.
    date_pattern <- "^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), (\\d{2})/(\\d{2})/(\\d{4})$"
    date_match <- str_match(line, date_pattern)
    if (!is.na(date_match[1, 1])) {
      date <- paste(date_match[1, 5], date_match[1, 4], date_match[1, 3], sep = "-") # Store date in YYYY-MM-DD
      next
    }
    
    # 4. If the line starts with time format of hour:minute (e.g. 18:51),
    # and add it as a time. Then extract the person name written after the time
    # and white spaces, and add it as a name. Then, extract the text as a message
    # which is written after a name and whitespaces, and add it as a message.
    time_pattern <- "^(\\d{2}:\\d{2})\\s+(Name|MyName)\\s+(.+)"
    time_match <- str_match(line, time_pattern)
    if (!is.na(time_match[1, 1]) && !is.na(date)) {
      time <- time_match[1, 2]
      name <- time_match[1, 3]
      message <- time_match[1, 4]
      
      new_row <- data.frame(date = date, time = time, name = name, message = message, stringsAsFactors = FALSE)
      df <- rbind(df, new_row)
      next
    }
    
    # 5. If the line does not match all the conditions above, extract the text
    # as a message and paste it after the message extracted on the 4. step,
    # and update the message value.
   df[nrow(df), "message"] <- paste(df[nrow(df), 'message'], line, sep = " ")
  }
  
  # Mutate the data types and values
  data <- df |>
    mutate(
      date = ymd(date),
      datetime = ymd_hm(paste(date, time, sep = " ")),
      term = cumsum(c(1, ifelse(date[-1] != date[-length(date)] | name[-1] != name[-length(name)], 1, 0))),
      .keep  = "unused"
    )
  
  # Add term column
  
  
  return(data)
}

# Example Usage
input_file_path <- "Chatname.txt" # Replace with your input file path
output_df <- convert_txt_to_df(input_file_path)

# Save the dataframe as an rds file.
output_file_path <- "Chat.rds" # Replace with your desired output file path
saveRDS(output_df, output_file_path)

# Print the first few rows of the dataframe
head(output_df)
glimpse(output_df)
