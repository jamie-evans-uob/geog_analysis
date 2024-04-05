
### This file prepares a CSV with the total number of crimes of different types in each LSOA (2021 LSOAs) for a given year (2023). It does this by looping through monthly data for every police force in the country.

# Police open data should be downloaded and unzipped into the input folder from: https://data.police.uk/data/. I have used data for every month of 2023, so there are 12 folders within the Input folder directory. I selected 'all forces' so there are approx. 40 spreadsheets within each sub-folder.
# Please note that it contains data for England, Wales and N. Ireland but not Scotland. See the change log for known data issues: https://data.police.uk/changelog/


# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,readr,tidyverse)


# Set the directory where your CSV files are located
wd_input <- ""  # Input folder to be assigned
wd_output <- ""        # Output folder to be assigned
setwd(wd_input)


# Function to aggregate data from each CSV file
aggregate_csv <- function(file_path) {
  # Read CSV into DataFrame
  df <- read_csv(file_path,col_names=T,col_select=c("LSOA code","Month","Crime type"))
  
  # Aggregate number of crimes by crime type and LSOA code
  aggregated_df <- df %>%
    group_by(`LSOA code`,`Month`,`Crime type`) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = `Crime type`, values_from = count, values_fill = 0)
  
  return(aggregated_df)
}


# Get a list of all directories in the current folder
setwd(wd_input)
folders <- list.dirs()
folders <- folders[-1]

# Iterate through each folder
for (folder in folders) {
  
  # List to store aggregated DataFrames
  dfs <- list()
  
  for (file_name in list.files(folder, pattern = "\\.csv$", full.names = TRUE)) {
    
      # Aggregate data from current CSV file
      aggregated_df <- aggregate_csv(file_name)
      # Append aggregated DataFrame to list
      dfs[[length(dfs) + 1]] <- aggregated_df
    }
    
    # Combine all DataFrames in the list
    final_df <- bind_rows(dfs) %>%
      group_by(`LSOA code`,`Month`) %>%
      summarise(across(everything(), function(x) ifelse(is.na(x), 0, x))) %>%
      summarise(across(everything(), sum))
    rm(dfs)
    
    # Dynamically construct the new dataframe name
    new_df_name <- paste("df_",(head(final_df$`Month`,1)),sep="")
    
    # Rename the dataframe
    assign(new_df_name, final_df, envir = .GlobalEnv)
  }

rm(aggregated_df,final_df)



## Combine monthly datasets
all_dataframes <- ls() # Get names of all dataframes in the environment
matching_dataframes <- grep("^df_", all_dataframes, value = TRUE) # Filter dataframes that begin with "df_"
dataframe_list <- mget(matching_dataframes) # Retrieve these dataframes as a list
comb_monthly <- bind_rows(dataframe_list) # Bind rows of all dataframes together


# Export
setwd(wd_output)
write_csv(comb_monthly, "Combined monthly crime data.csv")


## Aggregate to yearly values
comb_annual <- 
  comb_monthly %>%
  select(-`Month`) %>%
  group_by(`LSOA code`) %>%
  summarise(across(everything(), function(x) ifelse(is.na(x), 0, x))) %>%
  summarise(across(everything(), sum))

setwd(wd_output)
comb_annual <- read.csv("Combined annual crime data.csv",header=T,check.names=F)


## Sum of all crime
comb_annual <- comb_annual %>%
  mutate(Total = rowSums(across(where(is.numeric)), na.rm=TRUE))


## Produce ranks (for total crime and vehicle crime)
order.scores<-order(comb_annual$Total, decreasing=T)
comb_annual$rank_Total <- NA
comb_annual$rank_Total[order.scores] <- 1:nrow(comb_annual)
comb_annual$rank_Total <- comb_annual$rank_Total - 1  # Subtract one from ranking to account for the NA group

order.scores<-order(comb_annual$`Vehicle crime`, decreasing=T)
comb_annual$rank_VehicleCrime <- NA
comb_annual$rank_VehicleCrime[order.scores] <- 1:nrow(comb_annual)
comb_annual$rank_VehicleCrime <- comb_annual$rank_VehicleCrime - 1  # Subtract one from ranking to account for the NA group


## Export
write_csv(comb_annual, "Combined annual crime data.csv")



