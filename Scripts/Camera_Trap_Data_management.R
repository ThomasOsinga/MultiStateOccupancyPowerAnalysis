library(tidyverse)
  ##read in raw data
Beyond_Moose <- read_csv("C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Data/Raw/observationsBeyondMoose.csv")
deployments <- read_csv("C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Data/Raw/deployments.csv")
species <- c('Cervus elaphus',
             "Capreolus capreolus",
             "Alces alces",
             "Dama dama")


## create dates for deployments
deployments <- deployments %>% 
  mutate(state = as.Date(start),
         end = as.Date(end))
## join deployments and raw sightings data by deploymentID
Beyond_Moose_updated <- Beyond_Moose %>%
  mutate(date = as.Date(Beyond_Moose$timestamp)) %>% 
  left_join(deployments %>% select(deploymentID, start, end),
            by = "deploymentID")
Beyond_Moose$date <- as.Date(Beyond_Moose$timestamp)
#keep only the unique deployments with start and end times
Beyond_Moose_unique <- Beyond_Moose_updated %>%
  select(deploymentID, start, end)  %>% 
  distinct(deploymentID, .keep_all = TRUE)


# Determine the overall timeframe (This is sour study period, and relatively arbitrary chosen)
overall_start <- as.Date("2017-06-01")
overall_end <- as.Date("2017-08-30")

# Add a column to indicate if deployment is within the overall timeframe and filter only the deployments with Yes
Beyond_Moose_unique  <- Beyond_Moose_unique %>%
  mutate(
    WithinOverallTimeframe = if_else(start <= overall_end & end >= overall_start, "Yes", "No")
  ) %>% 
  filter(WithinOverallTimeframe == "Yes")

# Expand the deployments to have one row per day
expanded_deployments <- Beyond_Moose_unique %>%
  select(deploymentID, start, end)  %>% 
  rowwise() %>%
  mutate(day = list(seq(overall_start, overall_end, by="day"))) %>%
  unnest(day) %>%
  ungroup()


# Add an 'active' column, indicating if the camera was operational
expanded_deployments <- expanded_deployments %>%
  mutate(active = if_else(day >= as.Date(start) & day <= end, 1, 0))

## MultiState ## Create a occurrence column which indicates the max state for a day
#state 1 (occupied) 
#state 2 (occupied with calf)
#state 3 (Occupied with 2 calves)
#lastly filter out the males and unknowns from state 1
SightSum <- list()
for (i in species) {
  SightSum[[i]] <- Beyond_Moose %>%
    filter(scientificName == i) %>%
    mutate(state = case_when(
      lifeStage %in% c("juvenile", "offspring") & count == 1 ~ 2,
      lifeStage %in% c("juvenile", "offspring") & count  >=2 ~ 3,
      #lifeStage %in% c("juvenile", "offspring") & count >= 3 ~ 4,
      TRUE ~ 1  # Presence of "Dama dama" without calves
    )) %>%
    filter(!(state == 1 & sex %in% c("unknown", "male"))) %>% 
    group_by(deploymentID, date) %>%
    summarize(occurrence = max(state), .groups = 'drop')
}


##Adding NA's for nonactive days and 0's for active days with no occurence. 
results <- list()
for (i in species){
results[[i]] <- expanded_deployments %>%
  left_join(SightSum[[i]], by = c("deploymentID" = "deploymentID", "day" = "date")) %>%
  mutate(active = case_when(
    active == 0 ~ NA_real_, # Replace 0 in active with NA
    active == 1 & is.na(occurrence) ~ 0, # If active is 1 but occurrence is NA, replace with 0
    active == 1 ~ occurrence, # If active is 1, use the occurrence value
    TRUE ~ active # Fallback to keep active as is
  )) %>%
  select(-occurrence) 
}

### create a matrix structure with a column per day and deployments per row.
  PresAbsL <- list()
for (i in species) {
  # Combine deployment_daily with each species' SightSum, corrected for deployment period
  PresAbsL[[i]] <- results[[i]] %>% 
  pivot_wider(names_from = day, values_from = active, values_fill = list(occurrence = 0)) %>%  #values_fill is a remnant but shouldnt influence the output.
  select(-c(start, end))
}


moose <- PresAbsL[[1]]

##savefiles
setwd("C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Data/ModelFiles")
write.csv(PresAbsL$`Alces alces`, "Alcesalces.csv", row.names = FALSE)
write.csv(PresAbsL$`Cervus elaphus`, "Cervuselaphus.csv", row.names = FALSE)
write.csv(PresAbsL$`Dama dama`, "Damadama.csv", row.names = FALSE)
write.csv(PresAbsL$`Capreolus capreolus`, "Capreolus capreolus.csv", row.names = FALSE)

# Initialize an empty list to store the summary for each species
SummaryList <- list()
StateCountList <- list()
state_levels <- c(0, 1, 2, 3)
state_labels <- c("Absence", "Present w/o calves", "Present with 1 calf", "Present with =>2 calves")

for (i in names(PresAbsL)) {
  # For each species data frame in PresAbsL
  species_data <- PresAbsL[[i]]
  
  # Summarize to get the maximum state for each deployment
  summary_df <- species_data %>%
    mutate(max_state = pmax(!!!select(., -deploymentID), na.rm = TRUE)) %>%
    select(deploymentID, max_state)

  # Add the summarized data frame to the SummaryList
  SummaryList[[i]] <- summary_df
  
  species_summary <- SummaryList[[i]]
  
  # Group by max_state and count the number of rows in each group
  state_count <- species_summary %>%
    group_by(max_state) %>%
    summarize(count = n(), .groups = 'drop')
  
  # Add the state count data frame to StateCountList
  StateCountList[[i]] <- state_count
  StateCountList[[i]] <- StateCountList[[i]] %>%
    mutate(max_state = factor(max_state, levels = state_levels, labels = state_labels))
}



StateCountList


library(tidyverse)
SummaryOccupiedSites <- list()  # Initialize an empty list to store summary data frames

for (i in names(PresAbsL)) {
  species_data <- PresAbsL[[3]]
  
  # Transform and summarize the data
  species_summary <- max_values_by_plot %>%
    pivot_longer(-plotID, names_to = "date", values_to = "state") %>%
    group_by(plotID, state) %>%
    summarize(count = n(), .groups = 'drop') %>%
    filter(state > 0) %>%
    pivot_wider(names_from = state, values_from = count, names_prefix = "state", values_fill = list(count = 0))
  
  # Store the summarized data in the SummaryList
  SummaryOccupiedSites[[i]] <- species_summary
}

SummaryOccupiedSites

# Summarize yms by site and year
tapply(species_data[-1], list(as.factor(species_data[[1]]), species_data[-1]), max)
##############################
species_data <- species_data %>%
  rowwise() %>%
  mutate(max_value = max(c_across(everything()), na.rm = TRUE))

species_data$max_value <- apply(species_data[-1], 1, max, na.rm = TRUE)
# Check data types and exclude non-numeric columns
numeric_cols <- sapply(species_data, is.numeric)
sapply(species_data, class)
species_data$max_value <- apply(species_data[, numeric_cols], 1, max, na.rm = TRUE)
species_data$max_value

## WEEKLY ###


weekly_PresAbs <- list()
for (name in names(PresAbsL)) {
  data <- PresAbsL[[name]]
  # Convert column names to date format, ignoring the first column which is deployment
  col_dates <- names(data)[-1]
  names(data)[-1] <- as.Date.numeric(col_dates)
  data_long <- tidyr::pivot_longer(data, cols = -deploymentID, names_to = "Date", values_to = "Count")
      # Ensure the Date is of type Date
  data_long$Date <- as.Date(data_long$Date)
  # Summarize data by week
  data_weekly <- data_long %>%
    group_by(deploymentID, Week = floor_date(Date, unit = "week", week_start = 1)) %>%
    summarize(
      Weekly_Count = if (all(is.na(Count))) NA else max(Count, na.rm = TRUE),
      .groups = 'drop'
    )
  # Pivot data to wide format where each week becomes a column
  data_wide <- data_weekly %>%
    pivot_wider(names_from = Week, values_from = Weekly_Count, values_fill = list(Weekly_Count = 0))
  
  weekly_PresAbs[[name]] <- data_wide
}


state_levels <- c(0, 1, 2, 3)
state_labels <- c("Absence", "Present w/o calves", "Present with 1 calf", "Present with >=2 calves")
WeeklyStateCountList <- list()
for (i in names(weekly_PresAbs)) {
  # For each species data frame in weekly_PresAbs
  species_data <- weekly_PresAbs[[i]]
  summary_df <- species_data %>%
    mutate(max_state = pmax(!!!select(., -deploymentID), na.rm = TRUE)) %>%
    filter(!is.na(max_state)) %>%
    select(deploymentID, max_state)
  # Convert numeric states to factor with labels
 # summary_df$max_state <- factor(summary_df$max_state, levels = state_levels, labels = state_labels)
  summary_df$max_state <- factor(summary_df$max_state, levels = state_levels, labels = state_labels, exclude = NULL)
  # Group by max_state and count the number of rows in each group
  state_count <- summary_df %>%
    group_by(max_state) %>%
    summarize(count = n())
  
  # Add the state count data frame to WeeklyStateCountList
  WeeklyStateCountList[[i]] <- state_count
}

# Now WeeklyStateCountList contains summarized count of states for each species
WeeklyStateCountList
StateCountList

##savefiles
setwd("C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Data/ModelFiles")
write.csv(weekly_PresAbs$`Alces alces`, "WeeklyAlcesalces.csv", row.names = FALSE)
write.csv(weekly_PresAbs$`Cervus elaphus`, "WeeklyCervuselaphus.csv", row.names = FALSE)
write.csv(weekly_PresAbs$`Dama dama`, "WeeklyDamadama.csv", row.names = FALSE)
write.csv(weekly_PresAbs$`Capreolus capreolus`, "WeeklyCapreolus capreolus.csv", row.names = FALSE)



## BiWeekly ###


BiWeekly_PresAbs <- list()
for (name in names(PresAbsL)) {
  data <- PresAbsL[[name]]
  # Convert column names to date format, ignoring the first column which is deployment
  col_dates <- names(data)[-1]
  names(data)[-1] <- as.Date.numeric(col_dates)
  data_long <- tidyr::pivot_longer(data, cols = -deploymentID, names_to = "Date", values_to = "Count")
  # Ensure the Date is of type Date
  data_long$Date <- as.Date(data_long$Date)
  # Summarize data by week
  data_BiWeekly <- data_long %>%
    mutate(
      BiWeek = as.integer((as.numeric(Date - min(Date)) / 14) + 1)  # Compute biweekly periods
    ) %>%
    group_by(deploymentID, BiWeek) %>%
      summarize(
        BiWeekly_Count = if (all(is.na(Count))) NA else max(Count, na.rm = TRUE),
        .groups = 'drop'
      )
  # Pivot data to wide format where each week becomes a column
  data_wide <- data_BiWeekly %>%
    pivot_wider(names_from = BiWeek, values_from = BiWeekly_Count, values_fill = list(BiWeekly_Count = 0))
  
  BiWeekly_PresAbs[[name]] <- data_wide
}


state_levels <- c(0, 1, 2, 3)
state_labels <- c("Absence", "Present w/o calves", "Present with 1 calf", "Present with >=2 calves")
BiWeeklyStateCountList <- list()
for (i in names(BiWeekly_PresAbs)) {
  # For each species data frame in BiWeekly_PresAbs
  species_data <- BiWeekly_PresAbs[[i]]
  summary_df <- species_data %>%
    mutate(max_state = pmax(!!!select(., -deploymentID), na.rm = TRUE)) %>%
    filter(!is.na(max_state)) %>%
    select(deploymentID, max_state)
  # Convert numeric states to factor with labels
  # summary_df$max_state <- factor(summary_df$max_state, levels = state_levels, labels = state_labels)
  summary_df$max_state <- factor(summary_df$max_state, levels = state_levels, labels = state_labels, exclude = NULL)
  # Group by max_state and count the number of rows in each group
  state_count <- summary_df %>%
    group_by(max_state) %>%
    summarize(count = n())
  
  # Add the state count data frame to BiWeeklyStateCountList
  BiWeeklyStateCountList[[i]] <- state_count
}

# Now BiWeeklyStateCountList contains summarized count of states for each species
BiWeeklyStateCountList
StateCountList

##savefiles
setwd("C:/Users/Thoma/OneDrive/Documenten/Forest and Nature Conservation/Thesis/Occupancy_Thesis2/Data/ModelFiles")
write.csv(BiWeekly_PresAbs$`Alces alces`, "BiWeeklyAlcesalces.csv", row.names = FALSE)
write.csv(BiWeekly_PresAbs$`Cervus elaphus`, "BiWeeklyCervuselaphus.csv", row.names = FALSE)
write.csv(BiWeekly_PresAbs$`Dama dama`, "BiWeeklyDamadama.csv", row.names = FALSE)
write.csv(BiWeekly_PresAbs$`Capreolus capreolus`, "BiWeeklyCapreolus capreolus.csv", row.names = FALSE)



