#### 0. INCLUDES ####
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi, dplyr,magrittr, tidyr, reshape2, readxl, stringi,
               ggplot2,caret,corrplot,rpart,e1071,arules,arulesViz,gdata,chron,RMySQL,lubridate)

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#DATA SETS
## Create a database connection
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!',
                dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#learn the attributes associated with the yr_2006 table
dbListFields(con, 'yr_2006')

#downloading yr_2006 table
yr_2006<- dbGetQuery(con, "SELECT * FROM yr_2006")

#downloading yr_2007 table
yr_2007<- dbGetQuery(con, "SELECT * FROM yr_2007")

#downloading yr_2008 table
yr_2008<- dbGetQuery(con, "SELECT * FROM yr_2008")

#downloading yr_2009 table
yr_2009<- dbGetQuery(con, "SELECT * FROM yr_2009")

#downloading yr_2010 table
yr_2010<- dbGetQuery(con, "SELECT * FROM yr_2010")


#investigating the data
str(yr_2006)
summary(yr_2010)
head(yr_2007)
tail(yr_2010)


#create a Multi-Year data frame. Combine tables into one dataframe using dplyr
entire_years<- bind_rows(yr_2007, yr_2008, yr_2009)

## Combine Date and Time attribute values in a new attribute column
entire_years <-cbind(entire_years,paste(entire_years$Date,entire_years$Time), stringsAsFactors=FALSE)

## Give the new attribute (in the 11th column) a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(entire_years)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
entire_years <- entire_years[,c(ncol(entire_years), 1:(ncol(entire_years)-1))]
head(entire_years)

#After converting from POSIXlt to POSIXct we will add the time zone and we'll prevent warning messages.
## Convert DateTime from POSIXlt to POSIXct
entire_years$DateTime <- as.POSIXct(entire_years$DateTime, "%Y/%m/%d %H:%M:%S")

### Add the time zone
attr(entire_years$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(entire_years)


## Create "year" attribute with lubridate
entire_years$year<-year(entire_years$DateTime)

#Create "quarter" attribute
entire_years$quarter<-quarter(entire_years$DateTime)

#Create "month" attribute
entire_years$month<-month(entire_years$DateTime)

#Create "week" attribute
entire_years$week<-week(entire_years$DateTime)

#Create "weekday" attribute
entire_years$weekday<-weekdays(entire_years$DateTime)

#Create "day" attribute
entire_years$day<-day(entire_years$DateTime)

#Create "hour" attribute
entire_years$hour<-hour(entire_years$DateTime)

#Create "minute" attribute
entire_years$minute<-minute(entire_years$DateTime)

str(entire_years)

# creating residual active energy feature (global_active_power*1000/60 - sub_metering_1 -
#sub_metering_2 - sub_metering_3)
entire_years$residual <- ((entire_years$Global_active_power*1000/60) - entire_years$Sub_metering_1 - 
                            entire_years$Sub_metering_2 - entire_years$Sub_metering_3)

is.na(entire_years)
summary(entire_years)

#exporting summary results in a .txt file
summary_results <- summary(entire_years)
capture.output(summary_results, file = "summary_results.txt")

mean(entire_years$Sub_metering_1)
mode(entire_years$Sub_metering_1)

entire_years %>% 
  summarise(avg_activepower = mean(Global_active_power), 
            min_activepower = min(Global_active_power),
            max_activepower = max(Global_active_power),
            total = n())


#global power by weekdays
entire_years %>% 
  group_by(weekdays(entire_years$DateTime)) %>% 
  summarise(Globalenergy = sum(Global_active_power))

#global power by months
entire_years %>% 
  group_by(months(entire_years$DateTime)) %>% 
  summarise(Globalenergy = sum(Global_active_power))

#global power by weeks
weeks <- entire_years %>%
  group_by(week(entire_years$DateTime)) %>% 
  summarise(Globalenergy = sum(Global_active_power))


#global power day by day from january 2007 to december 2009
dayly_global <- entire_years %>%
  group_by(year, month, day, weekday) %>%
  select(Global_active_power) %>%
  summarise(
    energy = sum(Global_active_power)
  )


#### PLOTTING ####

#global by years
ggplot(dayly_global,
       aes(year, energy))+
  geom_point(color="red")

#global by months
ggplot(dayly_global,
       aes(month, energy))+
  geom_point(color="red")

#global by weekday
ggplot(dayly_global,
       aes(weekday, energy))+
  geom_point(color="red")


