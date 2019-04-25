#### 0. INCLUDES ####
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}


pacman::p_load(rstudioapi, dplyr,magrittr, tidyr, reshape2, readxl, stringi,
               ggplot2,caret,corrplot,rpart,gdata,chron,
               esquisse,RMySQL,lubridate,padr,httr)



#Setting my Plotly API
#Sys.setenv("plotly_username"="kikusanchez")
#Sys.setenv("plotly_api_key"="Uky3F2ELrykJSTQHGBiP")

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)


#DATA SETS

#Create a database connection
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!',
                dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List the tables contained in the database
#dbListTables(con)

# ## Lists attributes contained in a table
#dbListFields(con,'iris')

#Use asterisk to specify all attributes for download
#irisALL <- dbGetQuery(con, "SELECT * FROM iris")

# Use attribute names to specify specific attributes for download
#irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#learn the attributes associated with the yr_2006 table
#dbListFields(con, 'yr_2006')

# #downloading yr_2006 table
#yr_2006<- dbGetQuery(con, "SELECT * FROM yr_2006")

#downloading yr_2007 table
yr_2007<- dbGetQuery(con, "SELECT * FROM yr_2007")

#downloading yr_2008 table
yr_2008<- dbGetQuery(con, "SELECT * FROM yr_2008")

#downloading yr_2009 table
yr_2009<- dbGetQuery(con, "SELECT * FROM yr_2009")

#downloading yr_2010 table
#yr_2010<- dbGetQuery(con, "SELECT * FROM yr_2010")


#### 1. PRE-PROCESS ####

#investigating the data
str(yr_2007)
summary(yr_2008)
head(yr_2009)
tail(yr_2009)

#create a Multi-Year data frame only with complete years. Combine tables into one dataframe using dplyr
entire_years<- bind_rows(yr_2007, yr_2008, yr_2009)

## Combine Date and Time attribute values in a new attribute column
entire_years <-cbind(entire_years,paste(entire_years$Date,entire_years$Time), stringsAsFactors=FALSE)

## Give the new attribute (in the 11th column) a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(entire_years)[11] <-"DateTime"

#remove Date and Time columns
entire_years$Date <- NULL
entire_years$Time <- NULL

# Move the DateTime attribute to the first position, from the last position, within the dataset
entire_years <- entire_years[,c(ncol(entire_years), 1:(ncol(entire_years)-1))]

## Convert DateTime from POSIXlt to POSIXct
entire_years$DateTime <- as.POSIXct(entire_years$DateTime, "%Y/%m/%d %H:%M:%S")

### Add the time zone
# attr(entire_ok$DateTime, "tzone") <- "Europe/London"

#Fix time attribute (without time zone)
#entire_years$DateTime <- ymd_hms(entire_years$DateTime)

#converting values into Kw/h -> standard market measure
entire_years <- entire_years %>% mutate(Global_active_power = Global_active_power/60) #kw into kw/h
entire_years <- entire_years %>% mutate(Global_reactive_power = Global_reactive_power/60) #kw into kw/h
entire_years <- entire_years %>% mutate(Sub_metering_1 = Sub_metering_1/1000) #w/h into kw/h
entire_years <- entire_years %>% mutate(Sub_metering_2 = Sub_metering_2/1000) #w/h into kw/h
entire_years <- entire_years %>% mutate(Sub_metering_3 = Sub_metering_3/1000) #w/h into kw/h


#creating a sequence to check how many rows would be in a complete dataset
length(seq(from=ymd_hm("2007-01-01 00:00"), to=ymd_hm("2009-12-31 23:59"), by=60)) #-> 1578240

#complete the data adding rows where missing rows are
entire_years <- pad(entire_years, by="DateTime", break_above = 2)

## Creating new attributes (with lubridate)
#year
entire_years$year<-year(entire_years$DateTime)

#quarter
entire_years$quarter<-quarter(entire_years$DateTime)

#month
entire_years$month<-month(entire_years$DateTime)

#week
entire_years$week<-week(entire_years$DateTime)

#weekday
entire_years$weekday<-weekdays(entire_years$DateTime)

#day
entire_years$day<-day(entire_years$DateTime)

#hour
entire_years$hour<-hour(entire_years$DateTime)

#minute
entire_years$minute<-minute(entire_years$DateTime)

# creating residual active energy feature (global_active_power - sub_metering_1 -
#sub_metering_2 - sub_metering_3)
entire_years$residual <- (entire_years$Global_active_power - entire_years$Sub_metering_1 - 
                            entire_years$Sub_metering_2 - entire_years$Sub_metering_3)


#Assigning day and night rates: 0.123€ from 22:31-6:29 & 0.158€ from 6:30-22:30
#creating a column to paste hour and minutes
entire_years$hour_minute <-hm (paste(entire_years$hour, entire_years$minute))

#creating a column with the rate for each period of time (between 6:30 & 22:30, true, false)
entire_years$rate <- ifelse (entire_years$hour_minute >= hm("6:30") & entire_years$hour_minute <= hm("22:30"),
                             0.158, 0.123)

#calculating total cost, residual cost and for each submeter
entire_years$cost_total<- entire_years$Global_active_power*entire_years$rate
entire_years$cost_residual<- entire_years$residual*entire_years$rate
entire_years$cost_sub1<- entire_years$Sub_metering_1*entire_years$rate
entire_years$cost_sub2<- entire_years$Sub_metering_2*entire_years$rate
entire_years$cost_sub3<- entire_years$Sub_metering_3*entire_years$rate



# #creating monthly prices
# #day rates are from 06h - 22h
# entire_day <- filter(entire_years, hour %in% c(6:22))
# 
# #night rates are from 23h - 06h
# entire_night <- filter(entire_years, hour %in% c(0,1,2,3,4,5,23))
# 
# 
# #creating cost column in both df
# entire_day<-mutate(entire_day, cost_total = Global_active_power * 0.158)
# entire_night<-mutate(entire_night, cost_total = Global_active_power * 0.123)
# 
# #calculating daily rates for each submeter in day rate
# entire_day <- entire_day %>% mutate(cost_sub1 = Sub_metering_1*0.158)
# entire_day <- entire_day %>% mutate(cost_sub2 = Sub_metering_2*0.158)
# entire_day <- entire_day %>% mutate(cost_sub3 = Sub_metering_3*0.158)
# entire_day <- entire_day %>% mutate(cost_residual = residual*0.158)
# 
# #calculating nightly rates for each submeter in night rate
# entire_night <- entire_night %>% mutate(cost_sub1 = Sub_metering_1*0.123)
# entire_night <- entire_night %>% mutate(cost_sub2 = Sub_metering_2*0.123)
# entire_night <- entire_night %>% mutate(cost_sub3 = Sub_metering_3*0.123)
# entire_night <- entire_night %>% mutate(cost_residual = residual*0.123)
# 
# 
# #merging night and day data frames into a unique dataframe
# entire_ok<-bind_rows(entire_day, entire_night)
# 
# #arranging data frame for DateTime
# entire_ok<-arrange(entire_ok, DateTime)


#removing unnecesary data frames
remove(con, yr_2007, yr_2008, yr_2009)

#saving pre-processed data frame
save(entire_years, file="entire_ok.Rda")


#load data frame
load("../Task1.IoT_Exploratory_data_analysis/datasets/entire_ok.Rda")




#### 2. PLOTTING ####

#GGPLOT

#global power by weekdays
weekdays_energy <- entire_ok %>% 
  group_by(weekday) %>% 
  summarise(energy = sum(Global_active_power))

ggplot(weekdays_energy,
       aes(x = weekday, y = energy, fill = energy)) +
  geom_bar(stat = "summary", position = "dodge", fun.y = "mean") + 
  scale_fill_gradient(low = "orange", high = "red")


#global power by months
months_energy <- entire_ok %>% 
  group_by(month)  %>% 
  summarise(energy = sum(Global_active_power))

ggplot(months_energy,
       aes(x = as.factor(month), y = energy, fill = energy)) +
  geom_bar(stat = "summary", position = "dodge", fun.y = "mean") + 
  scale_fill_gradient(low = "orange", high = "red") + 
  scale_x_discrete(name ="Month") + 
  scale_y_discrete(name= "Energy (KW/h)")


#global power by years
years_energy <- entire_ok %>% 
  group_by(year)  %>% 
  summarise(energy = sum(Global_active_power))

ggplot(years_energy,
       aes(x = year, y = energy, fill = energy)) +
  geom_bar(stat = "summary", position = "dodge", fun.y = "mean") + 
  scale_fill_gradient(low = "orange", high = "red") + 
  scale_x_discrete(name ="Year") + 
  scale_y_discrete(name= "Energy (KW/h)")



#-Subset data by month and summarise total energy usage across submeters
housePWR_mnth <- entire_ok %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(Sub_Meter_1=round(sum(Sub_metering_1), 3),
            Sub_Meter_2=round(sum(Sub_metering_2), 3),
            Sub_Meter_3=round(sum(Sub_metering_3), 3),
            Rest_energy=round(sum(residual), 3))

#-Look at top several rows of new monthly data set
head(housePWR_mnth)


#-Create monthly time series
housePWR_mnthTS <- ts(housePWR_mnth[,3:6],
                      frequency = 12,
                      start=c(2007,1),
                      end=c(2010,12))

#-Plot monthly time series
plot(housePWR_mnthTS, 
     plot.type='s',
     xlim=c(2007, 2011),
     col=c('red', 'yellow', 'blue', 'green'),
     main='Total Monthly kWh Consumption',
     xlab='Year/Month', ylab = 'kWh')
minor.tick(nx=12)
#-Create legend
b <- c('Sub-meter-1', 'Sub-meter-2', 'Sub-meter-3', 'Rest_energy')
legend('topleft', b, col=c('red', 'yellow', 'blue', 'green'), lwd=1, bty='y')




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

#consumption by hours and submeters
hours <- entire_years%>%
  group_by(hour)%>%
  summarise(consumption = sum(Global_active_power),
            sub1 = sum(Sub_metering_1),
            sub2 = sum(Sub_metering_2),
            sub3 = sum(Sub_metering_3),
            sub4 = sum(residual))


#energy by months and submeters in kw/h
months <- entire_years%>%
  group_by(month)%>%
  summarise(consumption = sum(Global_active_power),
            sub1 = sum(Sub_metering_1),
            sub2 = sum(Sub_metering_2),
            sub3 = sum(Sub_metering_3),
            sub4 = sum(residual))

#cost by months and submeters in price (all years)
month_price <- entire_ok%>%
  group_by(month)%>%
  summarise(cost = sum(cost_total),
            sub1 = sum(cost_sub1),
            sub2 = sum(cost_sub2),
            sub3 = sum(cost_sub3),
            sub4 = sum(cost_residual))

ggplot(month_price,
       aes(month,consumption))+
  geom_smooth()


#cost by months and submeters in price (2007)
yr_2007 <- filter(entire_ok, year==2007)

monthly_price2007 <- entire_2007%>%
  group_by(month)%>%
  summarise(cost = sum(cost_total),
            sub1 = sum(cost_sub1),
            sub2 = sum(cost_sub2),
            sub3 = sum(cost_sub3),
            sub4 = sum(cost_residual))

ggplot(monthly_price2007,
       aes(x = as.factor(month), y = cost, fill = cost)) +
  geom_bar(stat = "summary", position = "dodge", fun.y = "mean")



#cost by months and submeters in price (2008)
yr_2008 <- filter(entire_ok, year==2008)

monthly_price2008 <- yr_2008%>%
  group_by(month)%>%
  summarise(cost = sum(cost_total),
            sub1 = sum(cost_sub1),
            sub2 = sum(cost_sub2),
            sub3 = sum(cost_sub3),
            sub4 = sum(cost_residual))

ggplot(monthly_price2008,
       aes(x = month, y = cost, fill = cost)) +
  geom_bar(stat = "summary", position = "dodge", fun.y = "mean")


#cost by months and submeters in price (2009)
yr_2009 <- filter(entire_ok, year==2009)

monthly_price2009 <- yr_2009%>%
  group_by(month)%>%
  summarise(cost = sum(cost_total),
            sub1 = sum(cost_sub1),
            sub2 = sum(cost_sub2),
            sub3 = sum(cost_sub3),
            sub4 = sum(cost_residual))

ggplot(monthly_price2009,
       aes(x = month, y = cost, fill = cost)) +
  geom_bar(stat = "summary", position = "dodge", fun.y = "mean") +
  scale_fill_gradient(low = "orange", high = "red")



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


ggplot(entire_years,
       aes(weekday, Sub_metering_1))+
  geom_bar(stat = "identity")


#plot3dimension
#all variables
Plot_hours <- melt(hours, id.vars = "hour")


ggplot(Plot_hours,
       aes(x = hour, y = value, fill = variable)) +
  geom_bar(stat = "summary", position = "dodge", fun.y = "mean")


#submeters by hours
entire_ok%>%
  group_by(hour)%>%
  gather(Submeters,Kw,residual, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  ggplot(aes(x = hour, y = Kw, fill=Submeters))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) #+
  #coord_flip() +
  #facet_grid(~key)


#submeters by weekdays
entire_2009%>%
  group_by(weekday)%>%
  gather(Submeters,Kw,residual, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  ggplot(aes(x = weekday, y = Kw, fill=Submeters))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) #+
#coord_flip() +
#facet_grid(~key)


#submeters by months
entire_2009%>%
  group_by(month)%>%
  gather(Submeters,Kw,residual, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  ggplot(aes(x = as.factor(month), y = Kw, fill=Submeters))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) #+
#coord_flip() +
#facet_grid(~key)

#submeters by years (all submeters)
entire_ok%>%
  group_by(year)%>%
  gather(Submeters,Kw,residual, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  ggplot(aes(x = year, y = Kw, fill=Submeters))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) #+
#coord_flip() +
#facet_grid(~key)

#submeters by years (total cost and residual)
entire_ok%>%
  group_by(year)%>%
  gather(Cost,Kw, cost_total,cost_residual) %>%
  ggplot(aes(x = year, y = Cost, fill=Cost))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) #+
#coord_flip() +
#facet_grid(~key)


entire_ok%>%
  group_by(year)%>%
  gather(Submeters,Kw,residual, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  ggplot(aes(x = year, y = Kw, fill=Submeters))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE))

a<-entire_ok%>%
  filter(year!=2010)%>%
  group_by(year)%>%
  summarise(Total_active = sum(Global_active_power),
            Total_residual = sum(residual))
  a<-melt(a, id.vars = "year")
  
  ggplot(a,
         aes(x=year, y=value, fill=variable))+
    geom_bar(position = "dodge", stat="identity")
  
  
  ggplot(aes(x=year, y=Global_active_power))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_bar(aes(x=year, y=residual))



#average cost per month
monthly_avg_cost <- entire_ok%>%
  dplyr::group_by(year, month)%>%
  dplyr::summarise(cost = sum(cost_total)) %>%
  dplyr::group_by(month)%>%
  dplyr::summarise(avg_cost=mean(cost))
            # sub1 = sum(cost_sub1),
            # sub2 = sum(cost_sub2),
            # sub3 = sum(cost_sub3),
            # sub4 = mean(cost_residual))

entire_ok$month<- as.factor(entire_ok$month)




#rest of the consumption by months
#downloading a france map png
mypngfile <- download.file("https://upload.wikimedia.org/wikipedia/commons/thumb/e/e4/France_Flag_Map.svg/612px-France_Flag_Map.svg.png", 
                           destfile = "france.png", mode = 'wb') 
img <- png::readPNG('france.png')
#2007
ggplot(entire_2007,
       aes(x=month, y=residual))+
  background_image(img)+
  geom_smooth(color="blue")
 
#2008
ggplot(yr_2008,
       aes(x=month, y=residual))+
  #background_image(img)+
  geom_smooth(color="blue")

#2009
ggplot(yr_2009,
       aes(x=month, y=residual))+
  #background_image(img)+
  geom_smooth(color="blue")


ggplot(entire_years, aes(x=month, y=residual, color=year))+
  geom_smooth()


ggplot(entire_years,
       aes(x = month, y = residual, color=year)) + 
  geom_smooth()
  #scale_x_discrete(breaks = entire_years$month)


#calendar heat
yr_2007<- entire_ok_complete_rows%>%
  filter(year==2007)

yr_2007%>%
  group_by(month)%>%
  summarise(cost=sum(cost_total))%>%
ggplot(yr_2007,
       aes(month,weekday, fill = yr_2007cost_total)+
         geom_tile(colour="blue")+
         facet_grid(year(yr_2007$DateTime),~month))
       





#PLOTLY
p <- plot_ly(data = dayly_global,
             x = weekday, y = energy,
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2))) %>%
      layout(title = 'Styled Scatter',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

p


# Create a shareable link to your chart
chart_link = api_create(p, filename="scatter-styled")
chart_link




p <- plot_ly(entire_years,
  x = Sub_metering_1,y = year,
  name = "By days",
  type = "bar"
)
p



#### 3. TESTS ####


TEST <- CdataW %>% group_by(year, quarter, month, weekday, hour) %>%
  dplyr::summarize(Total=sum(Total), Residual=sum(Residual), Kitchen=sum(Kitchen), Laundry=sum(Laundry), Heater=sum(Heater), ISNA=sum(ISNA))

# From num to factor
TEST <- TEST[,c(1, 4, 5, 8)]

TEST$weekday <- factor(TEST$weekday, labels=c("Monday","Tuesday","Wednesday",
                                              "Thursday","Friday", "Saturday",
                                              "Sunday"), ordered=TRUE)

TEST$month <-factor(TEST$month, levels=as.character(1:12),
                    labels=c("Jan","Feb","Mar","Apr","May",
                             "Jun","Jul","Aug","Sep","Oct",
                             "Nov","Dec"),ordered=TRUE)

TEST$year <- factor(TEST$year, ordered = TRUE)



# Plot

TEST <- TEST %>% filter(year == 2007)

ggplot(TEST, aes(weekday, hour)) +
  geom_tile(aes(fill=TEST$Kitchen), color = "black") +
  facet_grid(TEST$year ~ .)+
  scale_fill_gradient(low="orange", high="red")



