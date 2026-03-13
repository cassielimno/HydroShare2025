#NMLNField QAQC
rm(list=ls())

library(readxl)
library(lubridate)

#Data Setup ####
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")

#Load Data
data <- data.frame(read_excel("NMLN_Field_EDD_25_cr.xlsx",
                              sheet="Result"))

#Extract site from activity ID
for(i in 1:length(data$Data_Logger_Line_ID)){
  end <- unlist(gregexpr("_\\d+-\\d+", data$Activity_ID[i]))
  data$site[i] <- substr(data$Activity_ID[i], 1, end-1)
}

#Extract date from activity ID
for(i in 1:length(data$Activity_ID)){
  start <- unlist(gregexpr("_\\d+-\\d+", data$Activity_ID[i]))
  end2 <- unlist(gregexpr("_\\d+:\\d+", data$Activity_ID[i]))
  data$date[i] <- substr(data$Activity_ID[i], start+1, end2-1)
}

#Remove column descriptions
data <- data[-1 ,]
unique(data$site)
#Create data frame of sites and symbol plotting information ####
#COMMENTED OUT THE 1:54 PART IDK WHAT IT DOES
sites <- data.frame(sort(unique(data$site), decreasing = TRUE)) #c(1:54, 1:54, 1:54))
sites$color <- ""

#Assign colors to plot by
sites[c(1:19), 3] <- "black"
sites[c(20:38), 3] <- "blue"
sites[c(39:56), 3] <- "darkorange3"


#Rename columns
colnames(sites)[1] <- "site"
colnames(sites)[2] <- "pch"

#Merge sites data to main data frame
data <- merge(data, sites, by="site")

#Convert all mS/cm to uS/cm
data$Result_Value <- as.numeric(data$Result_Value)

for(i in 1:length(data$Result_Value)){
  
  if(data$Characteristic_Name[i] == "Specific conductance"
     & data$Result_Value_Unit[i] == "mS/cm"){
    
    data$Result_Value[i] <- data$Result_Value[i]*1000
    data$Result_Value_Unit[i] <- "uS/cm"
    
  }
  
}

#Create list of parameters and units for plotting
param <- unique(data[c("Characteristic_Name", "Result_Value_Unit")])

#THIS IS TEMP TAKE OFF LAKE PARAM BECAUSE IT IS BEING CAUSED BY A WEIRD HYDROLAB ERROR ####
param<-param[1:9,]

#Function for plotting x axis with years and months
#Requires vector with POSIXct start and end date for axis
x.axis.year.month <- function(Xrange = c(model.start,model.end), # vector, length 2, POSIXct format
                              year.cex = 2, # size of text for years
                              month.cex = 1, # size of text for months
                              tick.length.yr = -0.5, # length of tick for year
                              tick.length.m = -0.01, # length of tick for months
                              month.line = -0.5, # distance month label from axis
                              year.line = 3, # distance year label from axis
                              axis.line = -0.5, # position of axis line in y direction
                              year.lab.doy = 180, # day of year for year label location
                              months.labeled = c(1,4,7,10), # numbers of months to label
                              month.letters = 1) # month abbreviation if not 1, defaults to 3
{
  date.values <- seq(Xrange[1], Xrange[2], "day")
  # year labels
  years.all <- floor_date(date.values, "year") # floor all days to year
  years <- data.frame(unique(years.all)) # get unique list of years
  colnames(years)[which(names(years) == 
                          "unique.years.all.")] <- "date.day1"
  years$date.year.label <- years$date.day1 + 60*60*24*year.lab.doy # calc year label days
  years <- years[years$date.year.label  >= Xrange[1] & # subset for mid years in range
                   years$date.year.label  <= Xrange[2],]
  years$date.year.label <- floor_date(years$date.year.label, "day")
  years$year.txt <- substr(as.character(years$date.day1),1,4) # years as text
  mtext(years$year.txt, at = years$date.year.label, side=1, font=2, col='gray20', 
        cex=year.cex, line=year.line) # print years at July 1)
  
  # month labels
  months.all <- floor_date(date.values, "month")
  months <- data.frame(unique(months.all))
  colnames(months)[which(names(months) == 
                           "unique.months.all.")] <- "date.day1"
  months$date.mid.month <- months$date.day1 + 15*24*60*60 # +15 days worth of secs to first day of months
  months <- months[months$date.mid.month >= Xrange[1] &
                     months$date.mid.month <= Xrange[2],]   
  months$month.name1 <- format(months$date.day1, "%b")  
  months$month.name2 <- substr(months$month.name1,1,1)
  months$month.number <- month(months$date.day1)
  # str(months)
  # head(months)
  
  # month labels
  if(month.letters == 1){
    axis(side = 1, at=months[months$month.number %in% months.labeled, 'date.mid.month'], 
         labels=months[months$month.number %in% months.labeled,'month.name2'], 
         cex.axis= month.cex, tck=0, 
         line=month.line, lwd=1, xaxs = "i")
  } else
    axis(side = 1, at=months[months$month.number %in% months.labeled, 'date.mid.month'],
         labels=months[months$month.number %in% months.labeled,'month.name1'], 
         cex.axis= month.cex, tck=0, 
         line=month.line, lwd=0, xaxs = "i", las = 2)
  # month ticks
  axis(side = 1, at=months$date.day1, 
       labels=FALSE, cex.axis= month.cex, tck=tick.length.m, 
       line=axis.line, lwd=1, xaxs = "i")  
  # year ticks    
  axis(side = 1, at=years$date.day1, 
       labels=FALSE, cex.axis= year.cex, tck=tick.length.yr, 
       line=axis.line, lwd=1, xaxs = "i")  
}


#Convert dates to POSIX format and results to numeric
glimpse(data)
data$date<-ymd(data$date)
data$date<-as.POSIXct(data$date)
data$Result_Value <- as.numeric(data$Result_Value)

#Initial Plots ####

#Loop to produce general plots for each parameter
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2023/QAGraphs2023")
for(i in 1:length(param$Characteristic_Name)){
  
  #Subset data to parameter of interest
  data.temp <- data[data$Characteristic_Name == param$Characteristic_Name[i] ,]
  
  #Set up .png to QA Results folder
  png(paste0(filename = "NMLNLakes", param$Characteristic_Name[i], ".png"),
      width= 600, height = 600)
  
  #Set margins
  par(mar=c(5,4,4,6))
  
  #Plot time series of data for parameter symbolized by site
  plot(data.temp$Result_Value ~ data.temp$date,
       main = paste("NMLN Lakes", param$Characteristic_Name[i]),
       xaxt= "n",
       xlab= "",
       ylab= param$Result_Value_Unit[i],
       pch= data.temp$pch,
       col= data.temp$col)
  
  #Store date information for x axis function
  datemin <- min(data.temp$date)
  datemax <- max(data.temp$date)
  
  #Add month and year labels to x axis
  # x.axis.year.month(Xrange= c(as.POSIXct(datemin), as.POSIXct(datemax)),
  #                   year.cex = 1, # size of text for years
  #                   month.cex = 0.5, # size of text for months
  #                   tick.length.yr = -0.05, # length of tick for year
  #                   tick.length.m = -0.01, # length of tick for months
  #                   month.line = -0.5, # distance month label from axis
  #                   year.line = 1.5, # distance year label from axis
  #                   axis.line = 0, # position of axis line in y direction
  #                   year.lab.doy = 180, # day of year for year label location
  #                   months.labeled = c(4,7,10), # numbers of months to label
  #                   month.letters = 1)
  
  #Add legend
  # par(xpd= TRUE)
  # legend(bty= "n", "topright", inset= c(-0.18, 0), 
  #        legend= sites$site, pch= sites$pch, col= sites$col, cex=0.64)
  
  #dev.off()
}



#Plots for Specific QA Concerns ####
data$Result_Depth_Height_Measure <- as.numeric(data$Result_Depth_Height_Measure)

#Dissolved Oxygen Exploration 
#Not plotting, just aggregating data by site and date to look for patterns
do <- data[data$Characteristic_Name == "Dissolved oxygen (DO)" ,]
do.high <- do[do$Result_Value > 11.6 ,]

#Figure out which lakes have most high dissolved oxygen
#Add percent of high DO data points by lake
test <- aggregate(do.high$Result_Value, by=list(do.high$site), length)
total <- sum(test$x)
test$percent <- (test$x/total)*100

#Repeat process with very high dissolved oxygen levels
do.veryhi <- do[do$Result_Value > 14.6 ,]

test2 <- aggregate(do.veryhi$Result_Value, by=list(do.veryhi$site), length)
total2 <- sum(test2$x)
test2$percent <- (test2$x/total2)*100

#Create site lists for lakes with very high dissolved oxygen data
unique(do.high$site)
hisites <- unique(do.veryhi$site)

#Create data frame for when and what depth in lake high dissolved oxygen was measured
where <- unique(do.veryhi[c("site","Result_Depth_Height_Measure")])
when <- unique(do.veryhi[c("site", "date")])


#Looking at data for specific lakes based on where there was consistently high DO

#Regular very high oxygen between 8-12 m in Tetrau 
tet <- do[do$site == "TETRAU" ,]
tethi <- tet[tet$Result_Depth_Height_Measure >8
             & tet$Result_Depth_Height_Measure < 12 ,]
#Compare medians for whole lake and specific depths
median(tet$Result_Value)
median(tethi$Result_Value)

#Regular high dissolved oxygen at Lake Five between 7-11 meters
five <- do[do$site == "LAKEFI" ,]
fivehi <- five[five$Result_Depth_Height_Measure >7
               & five$Result_Depth_Height_Measure < 11 ,]
#Find medians for whole lake and specific depths
median(five$Result_Value)
median(fivehi$Result_Value)

#Regular high DO at Halfmoon between 4 and 6 meters
half <- do[do$site == "HALFM" ,]
halfhi <- half[half$Result_Depth_Height_Measure >4
               & half$Result_Depth_Height_Measure < 6 ,]
#Compare medians
median(half$Result_Value)
median(halfhi$Result_Value)

#Regular high DO at Foys between 7 and 10 meters
foys <- do[do$site == "FOYS" ,]
foyshi <- foys[foys$Result_Depth_Height_Measure >7
               & foys$Result_Depth_Height_Measure < 10 ,]
#Compare medians
median(foys$Result_Value)
median(foyshi$Result_Value)

#Dissolved Oxygen and Chlorophyll regression

#Subset data for dissolved oxygen and chlorophyll a
chla <- data[data$Characteristic_Name == "Chlorophyll a (probe)" ,]
do <- data[data$Characteristic_Name == "Dissolved oxygen (DO)" ,]

#Merge based on activity IDs
compare <- merge(do, chla, by= c("Activity_ID", "pch", "color"))

#Rename columns to reflect which result data is for
colnames(compare)[15] <- "DO_Result"
colnames(compare)[53] <- "CHLA_Result"

#Save png to NMLN Lakes folder
png(filename= "CHLA-DO_Regression.png",
    width= 500, height= 500)

#Plot regression
plot(compare$CHLA_Result ~ compare$DO_Result,
     main= "NMLN Lakes CHLA vs DO",
     #ylim= c(0,10), #turn on to plot without outliers
     pch= compare$pch,
     col= compare$color,
     xlab= "Dissolved oxygen (mg/l)",
     ylab= "Chlorophyll a (ug/l)")

dev.off()

#Create pH plots with indications of probe maintenance over time

png(filename = "pH_WithMaintenance.png", 
    width= 600, height = 600)

#Set plot margins
par(mar=c(5,4,4,6))

#Subset just pH data
ph <- data[data$Characteristic_Name == "pH" ,]

#Time series plot by site
plot(ph$Result_Value ~ ph$date,
     main= "NMLN pH",
     xlab= "",
     xaxt= "n",
     ylab= "",
     pch= ph$pch,
     col= ph$color)

#Store dates
datemin <- min(ph$date)
datemax <- max(ph$date)


#Add months and years to x axis
x.axis.year.month(Xrange= c(as.POSIXct(datemin), as.POSIXct(datemax)),
                  year.cex = 1, # size of text for years
                  month.cex = 0.5, # size of text for months
                  tick.length.yr = -0.05, # length of tick for year
                  tick.length.m = -0.01, # length of tick for months
                  month.line = -0.5, # distance month label from axis
                  year.line = 1.5, # distance year label from axis
                  axis.line = 0, # position of axis line in y direction
                  year.lab.doy = 180, # day of year for year label location
                  months.labeled = c(4,7,10), # numbers of months to label
                  month.letters = 1)

#Add legend
par(xpd= TRUE)
legend(bty= "n", "topright", inset= c(-0.18, 0), 
       legend= sites$site, pch= sites$pch, col= sites$col, cex=0.64)

#Date for sensor replacement ####
# sensor <- as.POSIXct("2020-04-13")
# #Dates for junction maintenance or replacement
# junction <- c(as.POSIXct("2017-07-05"), as.POSIXct("2018-11-06"), as.POSIXct("2016-07-21"))
# 
# #Add vertical lines at these dates
# segments(x0= sensor, x1= sensor, y0= -0.4, y1= 11.52, col="blue")
# segments(x0= junction, x1= junction, y0= -0.4, y1= 11.52, col= "darkgreen")
# 
# dev.off()

#Create plot for ORP with indications of maintenance over time

png(filename = "ORP_WithMaintenance.png",
    width= 600, height = 600)

#Set margins
par(mar=c(5,4,4,6))

#Subset just ORP data
orp <- data[data$Characteristic_Name == "Oxidation reduction potential (ORP)" ,]

#Plot time series of data by site
plot(orp$Result_Value ~ orp$date,
     main= "NMLN ORP",
     xlab= "",
     xaxt= "n",
     ylab= "",
     pch= orp$pch,
     col= orp$color)

#Store dates for x axis function
datemin <- min(orp$date)
datemax <- max(orp$date)

#Add year and month labels to x axis
x.axis.year.month(Xrange= c(as.POSIXct(datemin), as.POSIXct(datemax)),
                  year.cex = 1, # size of text for years
                  month.cex = 0.5, # size of text for months
                  tick.length.yr = -0.05, # length of tick for year
                  tick.length.m = -0.01, # length of tick for months
                  month.line = -0.5, # distance month label from axis
                  year.line = 1.5, # distance year label from axis
                  axis.line = 0, # position of axis line in y direction
                  year.lab.doy = 180, # day of year for year label location
                  months.labeled = c(4,7,10), # numbers of months to label
                  month.letters = 1)
#Add legend
par(xpd= TRUE)
legend(bty= "n", "topright", inset= c(-0.15, 0), 
       legend= sites$site, pch= sites$pch, col= sites$col, cex=0.7)

#Date of sensor replacement
sensor <- as.POSIXct("2020-04-13")
#Dates of junction maintenance or replacement
junction <- c(as.POSIXct("2017-07-05"), as.POSIXct("2018-11-06"), as.POSIXct("2016-07-21"))

#Add lines for maintenance dates
segments(x0= sensor, x1= sensor, y0= -220, y1= 770, col="blue")
segments(x0= junction, x1= junction, y0= -0.4, y1= 11.52, col= "darkgreen")

dev.off()
