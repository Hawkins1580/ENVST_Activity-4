# Activity #4 & Homework #4

# Start of tutorial #4

# install.packages(c("dplyr","ggplot2","lubridate"))
library(dplyr)
library(ggplot2)
library(lubridate)

# Reading in data
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")

metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")

sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")


# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()


# Adding a column to weather for QC
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation


# Creating flag
weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero


#interval data: look at first 2 observations as interval
# Time 1 %--% Time 2
weather$dateF[1] %--% weather$dateF[2]

# look at the interval length from the first to the second observation:
int_length(weather$dateF[1] %--% weather$dateF[2])

# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row

# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]


#create function for checking for irregular intervals that
# deviate from 900 seconds
# only argument is x, a vector of POSIXct formatted dates to be checked
# with an expected interval of 900 seconds
# the output is all observations that deviate from 900 seconds
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)

# Creating average that automatically excludes N/A
Average_AirTemp <- function(x){
  x.no = na.omit(x)
  sum(x.no)/length(x.no)
}
Average_AirTemp(weather$AirTemp)




# Start of in-class prompts

# Prompt #1
# You want to see if the solar radiation measurements experienced any issues with build up or accumulation on the sensor in May and June of 2021

# Examining Solar Radiation using a bar plot for May
ggplot(data=weather[weather$doy > 121 & weather$doy < 151 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

# Examining Solar Radiation using a bar plot for June
ggplot(data=weather[weather$doy > 151 & weather$doy < 180 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

# Examining Solar Radiation using a bar plot for September
ggplot(data=weather[weather$doy > 244 & weather$doy < 274 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()

# WE BELIEVE THAT THE DATA IS OKAY



# Prompt #2 
# Check for any date time issues using the function created in the tutorial
# Creating function hows intervals that are not equal to 15 minutes
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}

# run on weather data
timeCheck900(weather$dateF)



# Starting Homework

# Question #1
# You want to exclude any precipitation occurring in temperatures below zero. 
# You also want to check that no precipitation measurements are used if the X and Y level observations are more than 2 degrees.

# Creating and adding a column to weather excluding freezing temps and if x/y degrees > +/- 2
weather$Precip_QC<- ifelse(weather$AirTemp < 0 | # if air temp < 0
                          weather$XLevel < -2 | # checking if x level is +/- 2 degrees
                          weather$XLevel > 2 | # checking if x level is +/- 2 degrees
                          weather$YLevel < -2 | # checking if y level is +/- 2 degrees
                          weather$YLevel > 2, # checking if y level is +/- 2 degrees
                          NA, # value if true
                          weather$precip.QC) # value if false: uses original precipitation observation

# Plotting old precipitation
ggplot(data=weather,
       aes(x=dateF,
           y=precip.QC))+
  geom_col(color="royalblue4")+
  labs(x="Date", y="Precipitation (in mm)")+ # make axis labels
  ggtitle("Precipitation Levels at Hamilton College:",
          subtitle = "Including Frozen Temps & X/Y Level Malfunctions")+
  theme_classic()


# Plotting new precipitation
ggplot(data=weather,
       aes(x=dateF,
           y=Precip_QC))+
  geom_col(color="turquoise4")+
  labs(x="Date", y="Precipitation (in mm)")+ # make axis labels
  ggtitle("Precipitation Levels at Hamilton College:",
          subtitle = "Excluding Frozen Temps & X/Y Level Malfunctions")+
  theme_classic()

# Indicate how many missing precipitation values are in your data
sum(is.na(weather$Precip_QC))


# Question #2
# Create a data flag that warns a user if the battery voltage falls below 8.5 Volts

# Converting mV to V
weather$BatVolt_InVOLTS <- weather$BatVolt / 1000

weather$BatVoltFLAG <- ifelse(weather$BatVolt_InVOLTS < 8.5, # check if below 8.5
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero


# Question #3
# You should also create a function that checks for observations that are in 
# unrealistic data ranges in air temperature and solar radiation.

# Air Temperature

# Finding standard deviation and omitted NA
StdDev_AirTemp <- sd(weather$AirTemp,na.rm=TRUE) 
StdDev_AirTemp

OUTLIERS_AirTemp <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  upper <- (3*sd(x) + mean(x))
  lower <- (mean(x) - 3*sd(x))
  replace(x, x > upper | x < lower, NA)
}

OUTLIERS_AirTemp(weather$AirTemp)

min(weather$AirTemp, na.rm = TRUE)

# Plotting Air Temp to visualize
ggplot(data=weather,
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()


# Solar Radiation




# Question #4
# Make a plot of winter air temperatures in Jan - Mar of 2021. 
# Check for persistence issues that might indicate snow accumulation on the sensor.

# Examining Winter Air Temps using a bar plot for January to March 2021
ggplot(data=weather[weather$doy > 1 & weather$doy < 90 & weather$year == 2021,],
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="darkslategray3")+
  ggtitle("Air Temperature at Hamilton College in 2021:",
          subtitle = "Another Cold Winter & Crazy Spring")+
  labs(x="Month", y="Air Temperature (in Celcuis)")+ # make axis labels
  theme_classic()











