
# Installing all the required packages for the project
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(readr)

install.packages("skimr") 
library(skimr)

install.packages("janitor")
library(janitor)

install.packages("here")
library(here)

install.packages("dplyr")
library(dplyr)




daily_activity <- read.csv("dailyActivity_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
minutes_sleep <- read.csv("minuteSleep_merged.csv")
hourly_calories <- read.csv("hourlyCalories_merged.csv")


# Inspecting the datasets that were loaded
head(daily_activity)
str(daily_activity)
summary(daily_activity)

head(hourly_steps)
str(hourly_steps)
summary(hourly_steps)

head(minutes_sleep)
str(minutes_sleep)
summary(minutes_sleep)

head(hourly_calories)
str(hourly_calories)
summary(hourly_calories)

glimpse(daily_activity)
glimpse(hourly_steps)
glimpse(minutes_sleep)
glimpse(hourly_calories)



# Process Step

# We must convert the column names to lowercase and fix the date formats so that they are uniform.

daily_activity <- daily_activity %>%
  rename_with(tolower) %>%
  mutate(activitydate = as.Date(activitydate, format = "%m/%d/%Y"))
str(daily_activity)

hourly_steps <- hourly_steps %>%
  rename_with(tolower) %>%
  mutate(activityhour = as.POSIXct(activityhour, format = "%m/%d/%Y %I:%M:%S %p"))
str(hourly_steps)

# the date and time were separated into 2 columns
hourly_steps <- hourly_steps %>%
  mutate(
    date = as.Date(activityhour),               # Extract date
    time = format(activityhour, "%H:%M:%S")     # Extract time as text
  )
head(hourly_steps)

hourly_calories <- hourly_calories %>%
  rename_with(tolower) %>%
  mutate(
    activityhour = as.POSIXct(activityhour, format = "%m/%d/%Y %I:%M:%S %p"),
    date = as.Date(activityhour),
    time = format(activityhour, "%H:%M:%S")
  )
head(hourly_calories)


minutes_sleep <- minutes_sleep %>%
  rename_with(tolower) %>%
  mutate(
    datetime = as.POSIXct(datetime, format = "%m/%d/%Y %I:%M:%S %p"),
    date = as.Date(datetime),
    time = format(datetime, "%H:%M:%S")
  )
head(minutes_sleep)


# Analyze Phase

# Checking the number of unique users in each dataset
n_distinct(daily_activity$id)
n_distinct(hourly_steps$id)
n_distinct(hourly_calories$id)
n_distinct(minutes_sleep$id)

summary(daily_activity$totalsteps)
summary(daily_activity$calories)
summary(daily_activity$sedentaryminutes)

# Total Steps:

# The average daily step count is 6,547, with a median of 5,986.

# Step counts range from 0 to a maximum of 28,497.

# This wide range indicates some users are highly active, while others may be inactive on some days.
 
# Calories Burned:
   
#   Users burn an average of 2,189 calories per day, with a median of 2,062.
 
# The maximum recorded is 4,562 calories, which likely reflects high-activity days.
 
# Sedentary Minutes:
   
#   The average user is sedentary for about 995 minutes per day (~16.5 hours).
 
# The maximum value is 1,440 minutes, indicating some users may have had entirely inactive days.


# Share Phase

# 1. This plot shows the relationship between the number of steps taken in a day and the total calories burned.

# Each dot represents one day of activity for a user.
# The red line is a linear regression trend line showing the direction and strength of the relationship.


ggplot(daily_activity, aes(x = totalsteps, y = calories)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship Between Steps and Calories Burned",
    x = "Total Steps",
    y = "Calories Burned"
  ) +
  theme_minimal()

# Interpretation:
# There is a clear positive correlation between total steps and calories burned.
# This means that as users take more steps during the day, they tend to burn more calories.
# The relationship is fairly strong, as the dots are distributed around the line.
# This supports the hypothesis that daily movement directly contributes to energy expenditure.

# Recommendation:
# Bellabeat can promote regular walking and step goals using their wearable devices and mobile app insights.

# 2. This plot examines whether the amount of "very active" minutes during a day has a stronger relationship to calories burned.

# "Very active minutes" are logged when users engage in intense physical activity like running or HIIT workouts.

# The orange points represent each user-day record, and the dark red line is the trend line.

ggplot(daily_activity, aes(x = veryactiveminutes, y = calories)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Calories Burned vs. Very Active Minutes",
    x = "Very Active Minutes",
    y = "Calories Burned"
  ) +
  theme_minimal()

# Interpretation:
# There is a strong positive relationship between very active minutes and calories burned.
# The trend line here is steeper than in the "Steps vs Calories" plot, suggesting that high-intensity activity has a stronger calorie-burning effect than just walking more.
# However, many users have 0 very active minutes, and still show variation in calorie burn—likely due to lower-intensity or passive activity.

# Recommendation:
# Bellabeat should emphasize high-intensity workouts in their wellness programs and app, especially for users looking for efficient calorie-burning strategies.



# 3. This plot investigates whether longer sleep durations result in more activity (measured as total steps) the next day.

# We group sleep data by user and day, and join it with the matching activity data.

# Each point represents a user's total steps for a day, plotted against the amount of sleep they got the night before.



sleep_summary <- minutes_sleep %>%
  group_by(id, date) %>%
  summarise(total_sleep_minutes = sum(value), .groups = "drop")

daily_activity <- daily_activity %>%
  rename(date = activitydate)

activity_sleep <- inner_join(daily_activity, sleep_summary, by = c("id", "date"))

ggplot(activity_sleep, aes(x = total_sleep_minutes, y = totalsteps)) +
  geom_point(alpha = 0.5, color = "seagreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Sleep Duration vs. Steps",
    x = "Total Sleep (Minutes)",
    y = "Total Steps"
  ) +
  theme_minimal()

# Interpretation:
# The regression line shows a very slight negative trend, indicating that more sleep does NOT necessarily lead to more steps the next day.
# In fact, there may be a tendency for users to walk slightly less after longer sleep—possibly due to rest days, illness, or weekend habits.

# Recommendation:
# Bellabeat might explore personalized activity suggestions based on recent sleep patterns, or offer reminders to stay active even on well-rested days.



# 4. This plot tests a reverse hypothesis: do users tend to sleep more on the night following a high-calorie-burning day?

# To analyze this, we shift the sleep data forward by one day so that it aligns with the previous day's activity data.

# The purple points show calories burned on one day vs. sleep duration on the following night.



sleep_summary_next_day <- sleep_summary %>%
  mutate(prev_day = date - 1)

activity_sleep_shifted <- inner_join(
  daily_activity,
  sleep_summary_next_day,
  by = c("id", "date" = "prev_day")
)

ggplot(activity_sleep_shifted, aes(x = calories, y = total_sleep_minutes)) +
  geom_point(alpha = 0.5, color = "mediumpurple") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(
    title = "Calories Burned (Previous Day) vs Sleep Duration (Next Day)",
    x = "Calories Burned (Previous Day)",
    y = "Total Sleep (Next Day, Minutes)"
  ) +
  theme_minimal()

# Interpretation:
# The regression line shows a slight downward trend, suggesting that higher calorie burn does not lead to more sleep the next night.
# This contradicts the common assumption that physically demanding days lead to longer recovery sleep.
# It's possible that other factors (like stress, lifestyle, or external schedules) play a bigger role in determining sleep duration.

# Limitation:
# This result may be influenced by the small size and potential incompleteness of the sleep dataset (only 23 users).
# With a larger or cleaner dataset, the trend might change.

# Recommendation:
# Bellabeat should continue to promote healthy sleep habits as a standalone wellness goal, rather than assuming it's always tied to physical exertion.


# 5. This bar chart shows the average number of steps taken during each hour of the day, across all users and days.

# We extract the hour from the 'activityhour' column and compute the average steps taken in each hour slot.

# The goal is to understand what time of day users are typically most active.

library(lubridate)

hourly_steps %>%
  mutate(hour = hour(activityhour)) %>%                         # Extract hour from datetime
  group_by(hour) %>%
  summarise(avg_steps = mean(steptotal), .groups = "drop") %>% # Calculate average steps per hour
  ggplot(aes(x = factor(hour), y = avg_steps)) +                # Use factor(hour) for discrete x-axis
  geom_bar(stat = "identity", fill = "steelblue") +             # Bar chart
  labs(
    title = "Average Steps by Hour of Day",
    x = "Hour (0–23)",
    y = "Average Steps"
  ) +
  theme_minimal()

# Interpretation:
# This bar chart illustrates the average number of steps taken during each hour of the day.
# It shows that step counts are minimal during the early morning hours (12 AM to 6 AM), likely due to sleep.
# Step activity begins increasing rapidly after 6 AM and peaks around 11 AM to 12 PM.
# There is a noticeable dip between 2 PM to 4 PM, possibly due to rest or work hours.
# Activity picks up again in the early evening, peaking again around 6 PM to 7 PM.
# After 9 PM, activity sharply declines as users begin winding down for the day.

# Recommendation:
# Bellabeat can leverage this pattern by:
# - Scheduling activity reminders or motivational push notifications around 7 AM and 5–6 PM,
#   which align with natural peaks in user activity.
# - Sending mid-afternoon alerts (~3 PM) encouraging light movement or a quick walk during the lull.
# - Promoting morning and evening workout routines during users' most active windows.

# 6. This bar chart shows the average number of calories burned during each hour of the day.

# We extract the hour from the timestamp and calculate the average calories burned for each hour slot.

# This helps identify the times of day when users are most physically exertive.

hourly_calories %>%
  mutate(hour = hour(activityhour)) %>%                             # Extract hour
  group_by(hour) %>%
  summarise(avg_calories = mean(calories), .groups = "drop") %>%    # Average calories per hour
  ggplot(aes(x = factor(hour), y = avg_calories)) +                 # Convert hour to factor for discrete bar chart
  geom_bar(stat = "identity", fill = "firebrick") +                 # Bar chart
  labs(
    title = "Average Calories Burned by Hour of Day",
    x = "Hour (0–23)",
    y = "Average Calories Burned"
  ) +
  theme_minimal()

# Interpretation:
# This bar chart displays the average number of calories burned by users each hour.
# Similar to the step plot, calorie burn is lowest between midnight and early morning (12 AM to 6 AM).
# From 7 AM onward, calorie burn increases steadily, peaking between 11 AM and 1 PM.
# There is a sustained calorie-burning plateau from mid-day to early evening (12 PM to 8 PM),
# indicating consistent physical activity or energy expenditure throughout this window.
# After 9 PM, average calorie burn gradually declines again.

colnames(read.csv("minuteSleep_merged.csv"))


# Recommendation:
# Bellabeat can use these insights to:
# - Promote workout programs and nutrition tracking features during the peak calorie burn hours (11 AM – 8 PM).
# - Encourage movement in the early morning hours to jumpstart energy expenditure.
# - Support recovery or mindfulness activities during the late evening, when calorie burn naturally declines.