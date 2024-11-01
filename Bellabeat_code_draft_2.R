install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

glimpse(minutecalories1)
glimpse(minuteintensities1)
glimpse(minutesteps1)

minutecalories1$Id <- as.numeric(minutecalories1$Id)
class(minutecalories1$Id)
minuteintensities1$Id <- as.numeric(minuteintensities1$Id)
class(minuteintensities1$Id)
minutesteps1$Id <- as.numeric(minutesteps1$Id)
class(minutesteps1$Id)

#changed Id data type from dbl to numeric.

minutecalories1$ActivityMinute <- mdy_hms(minutecalories1$ActivityMinute)
class(minutecalories1$ActivityMinute)
minuteintensities1$ActivityMinute <- mdy_hms(minuteintensities1$ActivityMinute)
class(minuteintensities1$ActivityMinute)
minutesteps1$ActivityMinute <- mdy_hms(minutesteps1$ActivityMinute)
class(minutesteps1$ActivityMinute)

#changed ActivityMinute data type from chr to POSIXct POSIXt (datetime).

minutecalories1 <- arrange(minutecalories1, Id, ActivityMinute)
minuteintensities1 <- arrange(minuteintensities1, Id, ActivityMinute)
minutesteps1 <- arrange(minutesteps1, Id, ActivityMinute)

#ordered the datasets by Id and then by ActivityMinute in ascending order.

clean_names(minutecalories1)
clean_names(minuteintensities1)
clean_names(minutesteps1)

sum(duplicated(minutecalories1))
sum(duplicated(minuteintensities1))
sum(duplicated(minutesteps1))

#no duplicates found in the datasets

minutecalories1 %>% 
  filter(!complete.cases(.))
minuteintensities1 %>% 
  filter(!complete.cases(.))
minutesteps1 %>% 
  filter(!complete.cases(.))

#no blank data found

unique(minutecalories1$Id)
unique(minuteintensities1$Id)
unique(minutesteps1$Id)
       
#all datasets contain the same number of the same Id. 
#given the same amount of data entries per dataset, safe to guess same ActivityMinutes as well?

glimpse(minutecalories1)
glimpse(minuteintensities1)
glimpse(minutesteps1)

merged_mindata <- bind_rows(minutecalories1, minuteintensities1, minutesteps1)
View(merged_mindata)

merged_mindata <- merge(minutecalories1, minuteintensities1, minutesteps1, by = "Id")

#data merged to one df, but did not put all data columns into one row, how to??

merged_calint <- merge(minutecalories1, minuteintensities1, by = c("Id", "ActivityMinute"))
merged_min <- merge(merged_calint, minutesteps1, by = c("Id", "ActivityMinute"))
View(merged_min)

#all data is merged!

merged_min <- merged_min %>%
  arrange(Id, ActivityMinute)

#data is now all merged, and is in ascending order by Id, then ActivityMinute.
#data has ALOT of zero data points in intensities and steps. 

merged_min <- merged_min %>%
  rename(`Activity Minute` = ActivityMinute)

#cleaned up the names of the columns

sum(is.na(merged_min))

#no na data!

numeric_cor_data <- merged_min %>% 
  select(Calories, Intensity, Steps)
num_cor_matrix <- cor(numeric_cor_data, use = "complete.obs")
print(num_cor_matrix)

#created a correlation matrix called num_cor_matrix.

install.packages("corrplot")
library(corrplot)

corrplot(num_cor_matrix, method = "circle", title = "Correlation Matrix of FitBit Tracking Data",
         subtitle = "Relationship between Calories, Intensity, and Steps",
         mar = c(0, 0, 3, 0))

#created a heatmap of the correlations found between each variable, saved as Correlation_Matrix_of_FitBit_Tracking_Data. 
#every variable has a strong positive correlation. calories and intensity correlates to 0.8951278,
#calories and steps correlate to 0.8326218, intensity and steps correlate to 0.8098369. 
#because of this we know that the more steps a person takes, the more calories they burn, and 
#the higher the intensity is for their activity. this seems obvious and makes sense. how can we 
#use this knowledge to boost marketing? can we promote the wear time on the jewelry option products
#as items that can be worn even when a woman isn't working out, or doing casual things?

#can I figure out how many calories are burned at rest vs. during exercise? 
#I predict this would show more calories burned at rest (23hrs per day) over the 
#time individuals are actively exercising
#what about using the dailyactivity dataframe to determine calories burned 
#daily in sedentary vs. low vs. medium vs. high intensities?

unique(merged_min$Intensity)

#shows that the different values for intensity are 0, 1,2, and 3. this must 
#mean that 0 is sedentary, 1 is low intensity, 2 medium, and 3 high.

merged_min$Time <- format(as.POSIXct(merged_min$`Activity Minute`), format = "%H:%M:%S")
merged_min$Date <- as.Date(merged_min$`Activity Minute`)
View(merged_min)

#now have Date and Time columns added from ActivityMinute

daily_calories_by_intensity <- merged_min %>%
  group_by(Id, Date, Intensity) %>%
  summarize(Total_Calories = sum(Calories, na.rm = TRUE)) %>%
  ungroup()
View(daily_calories_by_intensity)

total_calories_per_intensity <- daily_calories_by_intensity %>%
  group_by(Intensity) %>%
  summarize(Total_Calories = sum(Total_Calories))
View(total_calories_per_intensity)

#can see that the total calories burned for each intensity is highest for 
#sedentary intensity, then light, followed by high and last, medium.
#this tells me that most calories are burnt in a sedentary state, which
#means individuals aren't working out. if they only wear a fitness tracker
#during exercise or leisure time, they will miss out on alot of stats about 
#their health. because Bellabeat has a jewelry option tracker, this means 
#women can wear a tracker without looking like they are wearing one.
#think at work, at a wedding, during a night on the town...women are tracking
#their health through the Leaf, gaining a more complete picture of their
#health than can be provided by other fitness tracking brands. 

ggplot(total_calories_per_intensity, aes(x = factor(Intensity), y = Total_Calories, fill = factor(Intensity))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Calories Burned at Each Intensity Level",
    subtitle = "From March 12, 2016 to May 12, 2016",
    x = "Intensity Level", y = "Total Calories Burned",
    fill = "Intensity") +
  scale_fill_manual(values = c("0" = "blue", "1" = "green", "2" = "yellow", "3" = "red")) +
  theme_minimal()

ggplot(total_calories_per_intensity, aes(x = factor(Intensity), y = Total_Calories, fill = factor(Intensity))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Calories Burned at Each Intensity Level",
    subtitle = "Fitbit Data From March 12, 2016 to May 12, 2016",
    x = "Intensity Level",
    y = "Total Calories Burned",
    fill = "Intensity"
  ) +
  scale_x_discrete(labels = c("0" = "Sedentary", "1" = "Light Intensity", "2" = "Medium Intensity", "3" = "High Intensity")) +
  scale_fill_manual(
    values = c("0" = "red", "1" = "orange", "2" = "yellow", "3" = "green"),
    labels = c("0" = "Sedentary", "1" = "Light Intensity", "2" = "Medium Intensity", "3" = "High Intensity")
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

#created a bar chart showing the total calories burned at each intensity level

average_calories_by_intensity <- merged_min %>%
  group_by(Id, Date, Intensity) %>%
  summarize(Average_Calories = mean(Calories, na.rm = TRUE)) %>%
  ungroup()
View(average_calories_by_intensity)

average_calories_per_intensity <- average_calories_by_intensity %>%
  group_by(Intensity, Date) %>%
  summarize(Daily_Average_Calories = mean(Average_Calories)) %>%
  ungroup()
View(average_calories_per_intensity)

#these show the average calories burned per day per intensity

library(ggplot2)

ggplot(average_calories_per_intensity, aes(x = Date, y = Daily_Average_Calories, color = factor(Intensity))) +
  geom_line(size = 1) +
  labs(
    title = "Average Daily Calories Burned per Intensity Level",
    subtitle = "Fitbit Data From March 12, 2016 to May 12, 2016",
    x = "Date",
    y = "Average Daily Calories Burned",
    color = "Intensity"
  ) +
  scale_color_manual(
    values = c("0" = "yellow", "1" = "orange", "2" = "red", "3" = "purple"),
    labels = c("0" = "Sedentary", "1" = "Light Intensity", "2" = "Medium Intensity", "3" = "High Intensity")
  ) +
  theme_minimal()

#looking at this line plot, it appears that the most average daily calories were burned in high
#intensity and decreased as intensity decreased. Also, the most stable line is sedentary,
#indicating that the amount of calories burned varied more as the intensity increased.


