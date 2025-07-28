---
title: "Cyclistic bike-share company Analysis"
author: "Elizaveta Barabash"
date: "2025-07-25"
output: html_document
---

Step 1: Loading packages

#Install necessary packages: "tidyverse" for data manipulation, "plotly" for interactive charts
```{r}
install.packages("tidyverse")
install.packages("plotly")
```

#Load libraries for data wrangling, visualization, and date/time processing
```{r}
library(tidyverse)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(conflicted)
library(lubridate)
```

#Resolve function conflicts by prioritizing "dplyr" versions of "filter" and "lag"\
```{r}
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
```

Step 2: Data loading & Duplicate check

#Load June 2025 trip data from CSV file
```{r}
data_2025_06 <- read_csv("~/Rstudio/Cyclistic/202506-divvy-tripdata.csv")
```

#Identify duplicate ride IDs - these should normally be unique. This checks for data integrity
```{r}
data_2025_06 %>% 
  group_by(ride_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
```

Step 3: Loading multiply monthly datasets

#Each dataset represents a different month of Cyclistic trip data
```{r}
data_2025_05 <- read_csv("~/Rstudio/Cyclistic/202505-divvy-tripdata.csv")
data_2025_04 <- read_csv("~/Rstudio/Cyclistic/202504-divvy-tripdata.csv")
data_2025_03 <- read_csv("~/Rstudio/Cyclistic/202503-divvy-tripdata.csv")
data_2025_02 <- read_csv("~/Rstudio/Cyclistic/202502-divvy-tripdata.csv")
data_2025_01 <- read_csv("~/Rstudio/Cyclistic/202501-divvy-tripdata.csv")
data_2024_12 <- read_csv("~/Rstudio/Cyclistic/202412-divvy-tripdata.csv")
data_2024_11 <- read_csv("~/Rstudio/Cyclistic/202411-divvy-tripdata.csv")
data_2024_10 <- read_csv("~/Rstudio/Cyclistic/202410-divvy-tripdata.csv")
data_2024_09 <- read_csv("~/Rstudio/Cyclistic/202409-divvy-tripdata.csv")
data_2024_08 <- read_csv("~/Rstudio/Cyclistic/202408-divvy-tripdata.csv")
data_2024_07 <- read_csv("~/Rstudio/Cyclistic/202407-divvy-tripdata.csv")
data_2024_06 <- read_csv("~/Rstudio/Cyclistic/202406-divvy-tripdata.csv")
```

Step 4: Column Removal

#Define a function to remove all station name/id columns from a list of dataframes
```{r}
remove_station_columns <- function(df_list) 
  {cols_to_remove <- c("start_station_name", "start_station_id", 
                      "end_station_name", "end_station_id")
  lapply(df_list, function(df) df %>% select(-all_of(cols_to_remove)))}
```

#Apply the cleaning function to all datasets
```{r}
dfs <- list(data_2024_06, data_2024_07, data_2024_08, data_2024_09, data_2024_10,data_2024_11, data_2024_12, data_2025_01, data_2025_02, data_2025_03, data_2025_04, data_2025_05, data_2025_06)
dfs_clean <- remove_station_columns(dfs)
```

#Name and load cleaned dataframes into the global environment
```{r}
names(dfs_clean) <- c("data_2024_06", "data_2024_07", "data_2024_08", "data_2024_09", "data_2024_10", "data_2024_11", "data_2024_12", "data_2025_01", "data_2025_02", "data_2025_03", "data_2025_04", "data_2025_05", "data_2025_06")
list2env(dfs_clean, envir = .GlobalEnv)
```

Step 5: NA handling

#For each dataset, count missing values per column
```{r}
check_na_counts <- function(df_names) 
  {lapply(df_names, function(name) 
    {df <- get(name, envir = .GlobalEnv)
    colSums(is.na(df))}) %>%
    setNames(df_names)}
df_names <- c("data_2024_06", "data_2024_07", "data_2024_08", "data_2024_09", "data_2024_10", "data_2024_11", "data_2024_12", "data_2025_01", "data_2025_02", "data_2025_03", "data_2025_04", "data_2025_05", "data_2025_06")
na_counts <- check_na_counts(df_names)
na_counts
```

#Remove rows with missing geographic coordinates ("end_lat", "end_lng")
```{r}
remove_na_coords <- function(df_names) 
  {for (name in df_names) 
    {df <- get(name, envir = .GlobalEnv)
    df <- df[!is.na(df$end_lat) & !is.na(df$end_lng), ]
    assign(name, df, envir = .GlobalEnv)}}
remove_na_coords(df_names)
```

Step 6: Duplicate check

#For each dataset, count fully duplicated rows
```{r}
count_duplicated <- function(df_names) 
  {result <- lapply(df_names, function(name)
    {df <- get(name, envir = .GlobalEnv)
    sum(duplicated(df))})
  setNames(result, df_names)}
sum_duplicated <- count_duplicated(df_names)
sum_duplicated
```

Step 7: Combine data

#Merge all monthly datasets into a single dataset for full-year analysis
```{r}
data_summary <- bind_rows(data_2024_06,data_2024_07,data_2024_08,data_2024_09,data_2024_10,data_2024_11,data_2024_12,data_2025_01,data_2025_02,data_2025_03,data_2025_04,data_2025_05,data_2025_06)
```

Step 8: Date and time processing

#Convert timestamps and extract date components: year, month, day, weekday
```{r}
Sys.setlocale("LC_TIME", "English")
data_summary$date <- as.Date(data_summary$started_at)
data_summary$month <- format(as.Date(data_summary$date), "%m")
data_summary$day <- format(as.Date(data_summary$date), "%d")
data_summary$year <- format(as.Date(data_summary$date), "%Y")

data_summary <- data_summary %>% mutate(day_of_week = weekdays(as.Date(date, "%m/%d/%Y")))
data_summary$day_of_week <- format(as.Date(data_summary$date), "%A")
```

#Calculate ride duration in seconds, remove short rides (<60 secs)
```{r}
data_summary$ride_duration_secs <- difftime(data_summary$ended_at, data_summary$started_at)
data_summary <- data_summary %>%
  mutate(ride_duration_secs = as.numeric(gsub(" secs", "", ride_duration_secs)))
sum(data_summary$ride_duration_secs < 60)
data_summary <- data_summary[data_summary$ride_duration_secs > 60,]

data_summary <- data_summary %>%
  mutate(start_time = format(as.POSIXct(started_at), "%H:%M:%S"))
data_summary <- data_summary %>%
  mutate(end_time = format(as.POSIXct(ended_at), "%H:%M:%S"))
```

#Format ride duration into HH:MM:SS format
```{r}
data_summary$ride_duration_hms <- format(.POSIXct(data_summary$ride_duration_secs, tz = "UTC"), "%H:%M:%S")
```

#Derive hour of start time and classify into time-of-day categories
```{r}
data_summary <- data_summary %>%
  mutate(start_hour = as.numeric(substr(start_time, 1, 2)))
data_summary <- data_summary %>% mutate(part_of_day = case_when(start_hour >= 6 & start_hour < 12 ~ "morning", start_hour >= 12 & start_hour < 18 ~ "afternoon", start_hour >= 18 & start_hour < 22 ~ "evening", TRUE ~ "night"))
data_summary$start_hour <- NULL
```

Step 9: Separate casual VS member users

#Split dataset into casual users and members
```{r}
data_casual <- data_summary[data_summary$member_casual == "casual",]
data_member <- data_summary[data_summary$member_casual == "member",]
```

Step 10: Descriptive stats: Ride duration (casual)

#Compute min, max, mean, median and standard deviation of ride duratioin for casual users
```{r}
format_hms <- function(seconds) 
  {hours   <- seconds %/% 3600
   minutes <- (seconds %% 3600) %/% 60
   sec     <- floor(seconds %% 60)
   sprintf("%02d:%02d:%02d", hours, minutes, sec)}

min_val <- min(data_casual$ride_duration_secs, na.rm = TRUE)
format_hms(min_val)
max_val <- max(data_casual$ride_duration_secs, na.rm = TRUE)
format_hms(max_val)
mean_val <- mean(data_casual$ride_duration_secs, na.rm = TRUE)
format_hms(mean_val)
median_val <- median(data_casual$ride_duration_secs, na.rm = TRUE)
format_hms(median_val)
sd_val <- sd(data_casual$ride_duration_secs, na.rm = TRUE)
format_hms(sd_val)

names <- c("min_ride_duration","max_ride_duration","mean_ride_duration","median_ride_duration","sd_ride_duration")
values <- c (format_hms(min_val),format_hms(max_val),format_hms(mean_val),format_hms(median_val),format_hms(sd_val))
casual_ride_duration_values <- data.frame(names, values)
```

Step 11: Distance calculation (casual)

#Use Haversine formula to compute trip distance in meters
```{r}
data_casual <- data_casual %>% 
  mutate(distance_m = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat)))
```

#Remove zero-distance or implausibly short trips
```{r}
sum(data_casual$distance_m == 0)
data_casual <- data_casual[data_casual$distance_m > 0, ]
data_casual <- data_casual %>% mutate(distance_km = distance_m/1000)
data_casual$distance_m <- NULL
data_casual <- data_casual %>%
  filter(distance_km > 0.01)
```

Step 12: Frequency tables

#Compute frequency tables by user type, rideable type, weekday, part of day, and month
```{r}
table(data_summary$member_casual)
count_member_casual <- data.frame(table(data_summary$member_casual))

table(data_casual$rideable_type)
casual_stat_rideable_type <- data.frame(table(data_casual$rideable_type))

table(data_casual$day_of_week)
casual_count_day_of_week <- data.frame(table(data_casual$day_of_week))

table(data_casual$part_of_day)
casual_part_of_day <- data.frame(table(data_casual$part_of_day))

table(data_casual$month)
casual_stat_month <- data.frame(table(data_casual$month))
```

Step 13: Descriptive stats: Ride duration (member)

#Same as above but for members
```{r}
min_val <- min(data_member$ride_duration_secs, na.rm = TRUE)
format_hms(min_val)
max_val <- max(data_member$ride_duration_secs, na.rm = TRUE)
format_hms(max_val)
mean_val <- mean(data_member$ride_duration_secs, na.rm = TRUE)
format_hms(mean_val)
median_val <- median(data_member$ride_duration_secs, na.rm = TRUE)
format_hms(median_val)
sd_val <- sd(data_member$ride_duration_secs, na.rm = TRUE)
format_hms(sd_val)

names <- c("min_ride_duraion","max_ride_duraion","mean_ride_duraion","median_ride_duraion","sd_ride_duraion")
values <- c(format_hms(min_val),format_hms(max_val),format_hms(mean_val),format_hms(median_val),format_hms(sd_val))
member_ride_duration_values <- data.frame(names,values)
```

Step 14: Distance calculation (member)

#Use Haversine formula to compute trip distance in meters
```{r}
data_member <- data_member %>% 
  mutate(distance_m = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat)))
```

#Remove zero-distance or implausibly short trips
```{r}
sum(data_member$distance_m == 0)
data_member <- data_member[data_member$distance_m > 0, ]
data_member <- data_member %>% mutate(distance_km = distance_m/1000)
data_member$distance_m <- NULL
data_member <- data_member %>% filter(distance_km > 0.01)
```

Step 15: Pivoted summary tables

#Compute frequency tables by rideable type, weekday, part of day, and month
```{r}
table(data_member$rideable_type)
member_stat_rideable_type <- data.frame(table(data_member$rideable_type))

table(data_member$day_of_week)
member_count_day_of_week <- data.frame(table(data_member$day_of_week))

table(data_member$part_of_day)
member_part_of_day <- data.frame(table(data_member$part_of_day))

table(data_member$month)
member_stat_month <- data.frame(table(data_member$month))
```


#Combine casual and member stats side-by-side for comparison
```{r}
data_ride_duration <- bind_cols(casual_ride_duration_values, member_ride_duration_values)
data_ride_duration$names...3 <- NULL
colnames(data_ride_duration) <- c("values","casual","member")

data_count_day_of_week <- bind_cols(casual_count_day_of_week,member_count_day_of_week)
data_count_day_of_week$Var1...3 <- NULL
colnames(data_count_day_of_week) <- c("day_of_week","casual","member")

data_stat_month <- bind_cols(casual_stat_month, member_stat_month)
data_stat_month$Var1...3 <- NULL
colnames(data_stat_month) <- c("month","casual","member")

data_part_of_day <- bind_cols(casual_part_of_day, member_part_of_day)
data_part_of_day$Var1...3 <- NULL
colnames(data_part_of_day) <- c("part_of_day","casual","member")

data_rideable_type <- bind_cols(casual_stat_rideable_type, member_stat_rideable_type)
data_rideable_type$Var1...3 <- NULL
colnames(data_rideable_type) <- c("rideable_type","casual","member")
```

Step 16: Ride duration by Hour brackets

#Group rides by duration brackets (0-1h, ..., 24-25h) for both user types
```{r}
data_casual <- data_casual %>% 
  mutate(ride_duration_hours = ride_duration_secs/3600)

data_member <- data_member %>% 
  mutate(ride_duration_hours = ride_duration_secs/3600)

for (i in 0:24) 
  {assign(paste0("data_casual_", i, "_", i+1),
         data_casual %>% filter(ride_duration_hours >= i & ride_duration_hours < i + 1))}

for (i in 0:24) 
  {assign(paste0("data_member_", i, "_", i+1),
        data_member %>% filter(ride_duration_hours >= i & ride_duration_hours < i + 1))}
```

#Create a summary table of ride counts by hourly duration for each user type
```{r}
names <- c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23","23-24","24-25")

values <- c(length(data_casual_0_1$ride_duration_hours),length(data_casual_1_2$ride_duration_hours),length(data_casual_2_3$ride_duration_hours),length(data_casual_3_4$ride_duration_hours),length(data_casual_4_5$ride_duration_hours),length(data_casual_5_6$ride_duration_hours),length(data_casual_6_7$ride_duration_hours),length(data_casual_7_8$ride_duration_hours),length(data_casual_8_9$ride_duration_hours),length(data_casual_9_10$ride_duration_hours),length(data_casual_10_11$ride_duration_hours),length(data_casual_11_12$ride_duration_hours),length(data_casual_12_13$ride_duration_hours),length(data_casual_13_14$ride_duration_hours),length(data_casual_14_15$ride_duration_hours),length(data_casual_15_16$ride_duration_hours),length(data_casual_16_17$ride_duration_hours),length(data_casual_17_18$ride_duration_hours),length(data_casual_18_19$ride_duration_hours),length(data_casual_19_20$ride_duration_hours),length(data_casual_20_21$ride_duration_hours),length(data_casual_21_22$ride_duration_hours),length(data_casual_22_23$ride_duration_hours),length(data_casual_23_24$ride_duration_hours),length(data_casual_24_25$ride_duration_hours))

stat_hours <- data.frame(names, values)

stat_hours <- stat_hours %>% 
  mutate(member = c(length(data_member_0_1$ride_duration_hours),length(data_member_1_2$ride_duration_hours),length(data_member_2_3$ride_duration_hours),length(data_member_3_4$ride_duration_hours),length(data_member_4_5$ride_duration_hours),length(data_member_5_6$ride_duration_hours),length(data_member_6_7$ride_duration_hours),length(data_member_7_8$ride_duration_hours),length(data_member_8_9$ride_duration_hours),length(data_member_9_10$ride_duration_hours),length(data_member_10_11$ride_duration_hours),length(data_member_11_12$ride_duration_hours),length(data_member_12_13$ride_duration_hours),length(data_member_13_14$ride_duration_hours),length(data_member_14_15$ride_duration_hours),length(data_member_15_16$ride_duration_hours),length(data_member_16_17$ride_duration_hours),length(data_member_17_18$ride_duration_hours),length(data_member_18_19$ride_duration_hours),length(data_member_19_20$ride_duration_hours),length(data_member_20_21$ride_duration_hours),length(data_member_21_22$ride_duration_hours),length(data_member_22_23$ride_duration_hours),length(data_member_23_24$ride_duration_hours),length(data_member_24_25$ride_duration_hours)))

colnames(stat_hours) <- c("number of hours","casual","member")
```


Step 17: Aggregation by Weekday

#Calculate average and median ride durations per day of week, separately for members and casuals
```{r}
summary_casual_member <- bind_rows(data_casual,data_member)

summary_casual_member$day_of_week <- ordered(summary_casual_member$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

duration_vs_day_mean <- data.frame(aggregate(summary_casual_member$ride_duration_secs ~ summary_casual_member$member_casual + summary_casual_member$day_of_week, FUN = mean))
colnames(duration_vs_day_mean) <- c("type","day_of_week","duration_secs_mean")

duration_vs_day_median <- data.frame(aggregate(summary_casual_member$ride_duration_secs ~ summary_casual_member$member_casual + summary_casual_member$day_of_week, FUN = median))
colnames(duration_vs_day_median) <- c("type","day_of_week","duration_secs_median")
```

#Reshape the dataset from wide to long format so that each row corresponds to a user type (casual or member) and a count value. This is necessary for grouped visualizations (e.g., ggplot with fill = type)
```{r}
data_count_day_of_week <- data_count_day_of_week %>%
  pivot_longer(cols = c(casual, member),
               names_to = "type",
               values_to = "count")

data_part_of_day <- data_part_of_day %>%
  pivot_longer(cols = c(casual, member),
               names_to = "type",                  
               values_to = "count")

data_ride_duration <- data_ride_duration %>%
  pivot_longer(cols = c(casual, member),
               names_to = "type",                  
               values_to = "count")

data_rideable_type <- data_rideable_type %>%
  pivot_longer(cols = c(casual, member),
               names_to = "type",                  
               values_to = "count")

data_stat_month <- data_stat_month %>%
  pivot_longer(cols = c(casual, member),
               names_to = "type",                  
               values_to = "count")

stat_hours <- stat_hours %>%
  pivot_longer(cols = c(casual, member),
               names_to = "type",                  
               values_to = "count")
```


Step 18: Visualizations

#Create a piechart of user type distribution with "plotly"
```{r}
colnames(count_member_casual) <- c("type","value")

colors <- c("#66CDAA", "#6495ED")
member_casual_piechart <- plot_ly(count_member_casual, labels = ~type, values = ~value, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(value, ' people'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               showlegend = FALSE)

member_casual_piechart <- plotly::layout(
  member_casual_piechart,
  title = list(
          text = "The percentage of members and casuals out of the total number of users<br><span style='font-weight:normal;font-size:13px;'>in the Cyclistic company for the period 2024.06 – 2025.06</span>",
          font = list(
          family = "Arial",
          size = 20,
          color = "#111111"),
      x = 0.5,               
      xanchor = "center",
      y = 0.95,              
      yanchor = "top"),
    margin = list(t = 100),  
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
member_casual_piechart  
```

#Generate multiple bar charts comparing: Rides by weekday; Rides by part of day; Rides by ride type; Rides by month; Rides by duration bracket (with and without log scale)
```{r}
data_count_day_of_week %>% ggplot(aes(x = day_of_week, y = count, fill = type)) + 
  geom_col(position = "dodge") + 
  labs(title = "Number of rides by User type and Day of week",
             x = "Day of week",
             y = "Number of rides", fill = "User type") +
  scale_fill_manual(values = c("member" = "#6495ED", "casual" = "#66CDAA"))

data_part_of_day %>% ggplot(aes(x = part_of_day, y = count, fill = type)) + 
  geom_col(position = "dodge") +
  labs(title = "Number of rides by User type and Part of day",
         x = "Part of day",
         y = "Number of rides", fill = "User type") +
  scale_fill_manual(values = c("member" = "#6495ED", "casual" = "#66CDAA"))

data_rideable_type %>% ggplot(aes(x = rideable_type, y = count, fill = type)) + 
  geom_col(position = "dodge") + 
  labs(title = "Number of rides by User type and Rideable type",
             x = "Rideable type",
             y = "Number of rides", fill = "User type") +
  scale_fill_manual(values = c("member" = "#6495ED", "casual" = "#66CDAA"))

data_stat_month %>% ggplot(aes(x = month, y = count, fill = type)) + 
  geom_col(position = "dodge") + 
  labs(title = "Number of rides by User type and Month",
             x = "Month",
             y = "Number of rides", fill = "User type") +
  scale_fill_manual(values = c("member" = "#6495ED", "casual" = "#66CDAA"))

ggplot(stat_hours, aes(x = `number of hours`, y = count, fill = type)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides by User type and Number of hours",
             x = "Number of hours", y = "Number of rides", fill = "User type") +
  scale_fill_manual(values = c("members" = "#6495ED", "casuals" = "#66CDAA")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(stat_hours, aes(x = `number of hours`, y = count, fill = type)) +
  geom_col(position = "dodge") +
  scale_y_log10() +
  labs(title = "Number of rides by User type and Number of hours",
             x = "Number of hours", y = "Number of rides (log scale)", fill = "User type") +
  scale_fill_manual(values = c("member" = "#6495ED", "casual" = "#66CDAA")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
