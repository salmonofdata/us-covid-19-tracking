library(tidyverse)
library(lubridate)
library(usmap)
library(ggplot2)
library(scales)

# ----------------------- #
# Change this  if desired #
# ----------------------- #

# Set the number of days in the past that you want to run the analysis for
# If this is left at zero it will use the most recent day's data in the dataset
start_day <- 0

# ----------------------- #
#    Code starts here     #
# ----------------------- #

# Import States data from the NY Times COVID-19 github page
# https://github.com/nytimes/covid-19-data
us_counties = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv', header = TRUE)

# Import county population data
county_population = read.csv('https://github.com/salmonofdata/us-covid-19-tracking/raw/master/county_population.csv', header = TRUE)

# Sort by FIPS and date
us_counties =  us_counties[with(us_counties, order(date , fips)), ]

# Fill in missing data
us_counties = complete(us_counties, fips, date )
us_counties <- us_counties[!is.na(us_counties$fips),]
us_counties$cases[is.na(us_counties$cases)] <- 0
us_counties$deaths[is.na(us_counties$deaths)] <- 0

# Calculate the daily movement
us_counties$cases_diff <- ave(us_counties$cases, factor(us_counties$fips), FUN=function(x) c(NA,diff(x)))
us_counties$deaths_diff <- ave(us_counties$deaths, factor(us_counties$fips), FUN=function(x) c(NA,diff(x)))
us_counties$cases[is.na(us_counties$cases_diff)] <- 0
us_counties$deaths[is.na(us_counties$deaths_diff)] <- 0

# Format the date
us_counties$date <- ymd(us_counties$date)

# Get today's date and the date a week before

max_day <- max(us_counties$date, na.rm = TRUE) 
max_day <- as.Date(max_day)- start_day
week_before <- max_day - 7

# Get data from max day and a week before
max_day_counties <- subset(us_counties, date == max_day)
week_before_counties <- subset(us_counties, date == week_before)
week_before_counties <- rbind(week_before_counties, max_day_counties)
week_before_counties =  week_before_counties[with(week_before_counties, order(fips, date)), ]
week_before_counties$weekly_diff <- ave(week_before_counties$cases, factor(week_before_counties$fips), FUN=function(x) c(NA,diff(x)))
county_weekly_movement <- week_before_counties[!is.na(week_before_counties$weekly_diff),]

# Get population data for county week case data

county_weekly_movement <- merge(x = county_weekly_movement, y = county_population[ , c("POPESTIMATE2019" , 'FIPS')] , by.x='fips', by.y='FIPS', all.x = TRUE)
county_weekly_movement$weekly_cases_100k <- ( county_weekly_movement$weekly_diff / county_weekly_movement$POPESTIMATE2019 ) * 100000
county_weekly_movement$weekly_cases_100k <- ifelse(county_weekly_movement$weekly_cases_100k < 0, 0, county_weekly_movement$weekly_cases_100k)

# Optional - export files to CSV
# write.csv(x=county_weekly_movement, file="county_weekly_movement.csv")
# write.csv(x=us_counties, file="us_counties_data.csv") 

# Plot the maps

states2 <- plot_usmap("states", 
                      color = "black",
                      fill = alpha(0.01)) #this parameter is necessary to get counties to show on top of states

counties2 <- plot_usmap(data = county_weekly_movement, values = "weekly_cases_100k" , color='grey')

ggplot() +
  counties2$layers[[1]] + 
  states2$layers[[1]] +
  counties2$theme + 
  coord_equal() +
  ggtitle("Covid-19 new cases per 100k people          ") + theme(plot.title = element_text(size = 30, face = "bold")) +
  annotate("text", x=1900000, y=-2500000, label= "Source: NY Times Covid-19 data") + 
  scale_fill_distiller(palette = "Spectral", name = 'Cases/100k' ,
                       limits = c(0,100), oob=squish)
  theme(legend.position="none") 

