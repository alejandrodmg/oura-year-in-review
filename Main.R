# ----------------------------------#
# Oura Year-in-Review               #
# CS710 Advanced Data Visualization #
#                                   #
# Author: Alejandro de Miguel       #
#                                   #
# Last updated: 2023-05-14          #
#                                   #
# CONTENT:                          #        
#                                   #
# 1. Average sleep                  #
# 2. Sleep score                    #
# 3. Wake-up and bed times          #
# 4. Rose chart                     #
#-----------------------------------#

# Install and load packages
packages <- c("dplyr", "reshape2", "ggplot2", 
              "lubridate", "knitr", "logging")
for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

loginfo("Loading dataset...")
data = read.csv("sleep_data.csv")

# Drop the days in which the ring ran out of battery
# and there's no data available
data = data %>% na.omit()

#------------------#
# 1. Average sleep #
#------------------#

loginfo("Calculating average sleep stages...")
kable(data %>% 
  summarize(total_sleep = mean(Total.Sleep.Duration)/(60*60),
            deep_sleep = mean(Deep.Sleep.Duration)/(60*60),
            rem_sleep = mean(REM.Sleep.Duration)/(60*60),
            light_sleep = mean(Light.Sleep.Duration)/(60*60),
            awake_time = mean(Awake.Time)/(60*60)))

#-----------------#
# 2. Sleep score  #
#-----------------#

# Define a mode function
getmode = function(v) {
  # Compute unique values
  uniqv = unique(v)
  # Get the most frequent unique value
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

loginfo("Calculating summary stats for sleep score...")
kable(data %>% 
  summarize(most_common = getmode(Sleep.Score),
            min_score = min(Sleep.Score),
            max_score = max(Sleep.Score)))

#--------------------------#
# 3. Wake-up and bed times #
#--------------------------#

# Because there are bedtimes in both the AM and PM it is not possible to simply
# average all the times. What we do instead is add "n" hours to every bedtime
# to make them in the AM, calculate the average time, and subtract "n" hours
# to return the actual mean value. 
mean_time = function(dates, add) {
  # Format dates
  d = as.POSIXct(dates, format="%Y-%m-%dT%H:%M:%S") 
  # Add n hours to every datetime to make them in the AM
  am = d + hours(add)
  # Extract the time
  time_only = format(am, format="%H:%M:%S")
  # Calculate the average of the times
  avg_time = mean(as.POSIXct(time_only, format="%H:%M:%S"))
  # Subtract the n hours
  time = format(avg_time - hours(add), format="%H:%M:%S")
  return(time)
}

# Create a map of months to seasons
seasons_map =  setNames(rep(
  c("Winter", "Spring", "Summer", "Fall"), each = 3), month.name)

loginfo("Calculating bed and wake-up times...")
kable(data %>% 
  mutate(month = months(as.Date(date)),
         season = seasons_map[month]) %>%
  group_by(season) %>%
  summarize(bedtime = mean_time(Bedtime.Start, 5),
            wakeup_time = mean_time(Bedtime.End, 0)))

#---------------#
# 4. Rose chart #
#---------------#

loginfo("Preprocessing data for plotting a rose chart...")
# Calculate average sleep for each month and sleep stage
sleep = data %>% 
  mutate(total = Total.Bedtime/(60*60),
         awake = Awake.Time/(60*60),
         rem = REM.Sleep.Duration/(60*60),
         light = Light.Sleep.Duration/(60*60),
         deep = Deep.Sleep.Duration/(60*60),
         month = months(as.Date(date))) %>%
  group_by(month) %>%
  summarize(awake = mean(awake),
            deep = mean(deep),
            rem = mean(rem),
            light = mean(light)) %>%
  mutate(month = factor(month, levels = month.name)) %>%
  select(month, deep, rem, light, awake)

loginfo("Calculating worst nightmare...")
# This is a hypothetical month in which you get the lowest deep, rem and light
# sleep of the worst months of the year (in terms of sleep time).
kable(sleep %>% 
  summarize(worst_nightmare = min(deep) + min(rem) + min(light)))

# Calculate the additional hours of sleep from the worst nightmare
# (i.e. how many hours of extra sleep from the month with the lowest value)
addional_sleep = sleep %>% 
  mutate(deep = deep - min(deep),
         rem = rem - min(rem),
         light = light - min(light),
         awake = awake - min(awake)) %>% 
  # Convert columns into rows
  melt(id = "month")

loginfo("Plotting rose chart...")
# The rose chart is basically a bar chart using the 
# polar coordinate system, and that's how it's built in ggplot
rose_chart = ggplot(data = addional_sleep, 
                    aes(x = month, y = value, fill = variable)) +
  geom_bar(stat = "identity", 
           colour = "black", 
           size = 0.5, 
           alpha = 0.6) +
  coord_polar() +
  # We use the blues palette and re-arrange the labels so it
  # goes from dark blue (deep sleep) to white (awake)
  scale_fill_brewer(palette = "Blues", 
                    direction = -1, 
                    name = "",
                    labels = c("Deep", "REM", "Light", "Awake")) +
  xlab("") + 
  ylab("") + 
  theme_minimal() +
  # Remove most of the text and ticks, and use a "beige" background
  # to match the PowerPoint's background
  theme(plot.background=element_rect(fill = "floralwhite"),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.margin = unit(c(10, 10, 10, 10), "pt"))

# Create labels to add on top of each bar
label_data = addional_sleep %>% group_by(month) %>% summarize(Tot=sum(value))
angle = round(90 - 360 * (c(1:nrow(label_data)) - 0.5) /nrow(label_data), 0)     
label_data$hjust = ifelse(angle < -90, 1, 0)
label_data$angle = ifelse(angle < -90, angle + 180, angle)

# Add the labels to the chart and plot it
rose_chart + 
  geom_text(data = label_data, 
            aes(x = month, y = Tot + 0.1, label = month, hjust = hjust),
            color = "black", 
            size = 3, 
            angle = label_data$angle, 
            inherit.aes = FALSE)

loginfo("Script execution completed!")
