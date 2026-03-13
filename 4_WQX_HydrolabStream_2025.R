# setup ####
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace

library(stringr)
library(readxl)
library(plyr)
library(openxlsx)
library(tidyverse)
#Function for removing NAs from data frame, leaves empty cells
clear.na <- function(dataframe) {
  for(i in 1:length(colnames(dataframe))){
    
    list1 <- dataframe[, i]
    
    for(j in 1:length(list1)){
      
      if(is.na(list1[j])==TRUE){
        list1[j] <- ""
      }
    }
    
    dataframe[, i] <- list1
  }
  return(dataframe)
}

# Data Import ####
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
data <- read.csv('HydrolabCompilationTribsTurb2025.csv') %>% clean_names()

data$date<- as.POSIXct(data$date, format= "%Y-%m-%d")
#rename site to station id
names(data)[names(data) == 'date'] <- 'ActivityDate'

#Remove Unneeded Data####
#Parameters we aren't uploading
#make sure these match what they are actually called #####
unique(data$parameter)

remove4 <- which(is.na(data$parameter)==TRUE
                 |data$parameter == "Res"
                 |data$parameter == "Sal"
                 |data$parameter == "Total dissolved solids"
                 |data$parameter == "refPAR"
                 |data$parameter == "BPSvr4"
                 |data$parameter == "Dep200"
                 |data$parameter == "TDS" )
test4 <- data[remove4 ,]
#test this worked correctly
unique(test4$parameter)

data <- data[-remove4 ,]

# Create WQX style activity ID ####

#Add activity type as field measure/observation
data$ActivityType <- "F-MSR/OBS"

#Replace any missing times with midnight
#this will bring up an error if there are no missing times
for(i in 1:length(data$ActivityTime)){
  if(data$ActivityTime[i] == ""){
    data$ActivityTime[i] <- "00:00:00"
  }
}

#Remove depths, which weren't calibrated for stream samples
#make sure this is right col
data <- data[, -8]

#Remove any duplicate hydrolab files
data <- unique(data[c(colnames(data))])

#Create WQX style activity ID
data$my.activty <- paste0(data$site_id, "_",
                          data$ActivityDate,"_", 
                          data$time,"_",
                          substr(data$ActivityType,1,1))

# check length is less than 35 characters
data$activity_length <- nchar(data$my.activty)
range(data$activity_length)

# test for why activity ids are not unique ####
data$temp <- paste0(data$my.activty,"__",
                    data$parameter)
test5 <- aggregate(data$parameter, 
                   by = list(data$temp),
                   length)
test5[test5$x > 1,]


#remove duplicates not sure where these came from CHECK TO SEE THAT THIS WORKS ####
data <- data[!duplicated(data$temp),]

# Create Activity Worksheet ####
activity.wsheet.all <- data[,c('my.activty', 'ActivityType',
                               'ActivityDate', 'time',
                               "site_id")]
str(activity.wsheet.all)

activity.wsheet <- unique(activity.wsheet.all[,
                                              c('my.activty', 'ActivityType',
                                                'ActivityDate', 'time',
                                                "site_id")])


head(activity.wsheet)
str(activity.wsheet)

#Add columns to activity worksheet for medium and organization information
activity.wsheet$Medium <- "Water"
activity.wsheet$Medium_Subdivision <- 'SW'
activity.wsheet$Activity_Conducting_Organization <- 
  'Whitefish Lake Inst'
activity.wsheet$Personnel <- "WLI Staff"

#Add time zones based on dates
activity.wsheet$ActivityDate <- as.POSIXct(activity.wsheet$ActivityDate, format= "%Y-%m-%d")

for(i in 1:length(activity.wsheet$ActivityDate)){
  ifelse(activity.wsheet$ActivityDate[i] > as.POSIXct("2007-03-11") & activity.wsheet$ActivityDate[i] < as.POSIXct("2007-11-04")
         | activity.wsheet$ActivityDate[i] > as.POSIXct("2008-03-09") & activity.wsheet$ActivityDate[i] < as.POSIXct("2008-11-02")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2009-03-08") & activity.wsheet$ActivityDate[i] < as.POSIXct("2009-11-01")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2010-03-14") & activity.wsheet$ActivityDate[i] < as.POSIXct("2010-11-07")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2011-03-13") & activity.wsheet$ActivityDate[i] < as.POSIXct("2011-11-06")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2012-03-11") & activity.wsheet$ActivityDate[i] < as.POSIXct("2012-11-04")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2013-03-10") & activity.wsheet$ActivityDate[i] < as.POSIXct("2013-11-03")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2014-03-09") & activity.wsheet$ActivityDate[i] < as.POSIXct("2014-11-02")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2015-03-08") & activity.wsheet$ActivityDate[i] < as.POSIXct("2015-11-01")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2016-03-13") & activity.wsheet$ActivityDate[i] < as.POSIXct("2016-11-06")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2017-03-12") & activity.wsheet$ActivityDate[i] < as.POSIXct("2017-11-05")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2018-03-11") & activity.wsheet$ActivityDate[i] < as.POSIXct("2018-11-04")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2019-03-10") & activity.wsheet$ActivityDate[i] < as.POSIXct("2019-11-03")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2020-03-08") & activity.wsheet$ActivityDate[i] < as.POSIXct("2020-11-01")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2021-03-14") & activity.wsheet$ActivityDate[i] < as.POSIXct("2021-11-07")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2022-03-13") & activity.wsheet$ActivityDate[i] < as.POSIXct("2022-11-06")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2023-03-12") & activity.wsheet$ActivityDate[i] < as.POSIXct("2023-11-05")
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2025-03-09") & activity.wsheet$ActivityDate[i] < as.POSIXct("2025-11-02"),
         yes= activity.wsheet$Activity_Start_Timezone[i] <- "MDT",
         no= activity.wsheet$Activity_Start_Timezone[i] <- "MST")
  
}

#check that this worked ####
unique(activity.wsheet$Activity_Start_Timezone)

#Add Project ID
activity.wsheet$Project_ID <- "WWQMP"

# read in WQX template activity column names 
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2022/HydroShareIntermediateData2022")
activity.template <- data.frame(read_excel("MT-eWQX_EDD_20170221.xlsx", sheet= "Activity"))
colnames(activity.template)[1] <- "Project_ID"

colnames(activity.wsheet)
#Adjust column names to match template
colnames(activity.wsheet)[5] <- "Station_ID"
colnames(activity.wsheet)[1] <- "Activity_ID"
colnames(activity.wsheet)[2] <- "Activity_Type"
colnames(activity.wsheet)[3] <- "Activity_Start_Date"
colnames(activity.wsheet)[4] <- "Activity_Start_Time"
colnames(activity.wsheet)[10] <- "Activity_Start_Time_Zone"

str(activity.wsheet)
str(activity.template)

#Change date format to character string to match template
activity.wsheet$Activity_Start_Date <- as.character(activity.wsheet$Activity_Start_Date)

# rbind.fill() to merge working dataframe to the template; note on rbind.fill- if column structures don't match in addition to names, it won't work
activity <- rbind.fill(activity.template, activity.wsheet)

#Remove NAs from activity data frame
activity <- clear.na(activity)

# Create Results Worksheet ####

#Read in results template
result.template <- data.frame(read_excel("MT-eWQX_EDD_20170221.xlsx", sheet= "Result"))
colnames(result.template)[1] <- "Activity_ID"

data.result <- data
#remove added col
data.result <- data.result %>% select(-"x1", -"activity_length")

#Check that activity IDs match between activity and results sheets
which(data.result$my.activty %in% activity$Activity_ID==FALSE)
which(activity$Activity_ID %in% data.result$my.activty==FALSE) #1 is okay, from headings row

colnames(result.template)
glimpse(data.result)
#Adjust column names to match template
#change these to make sure they match ####
colnames(data.result)[5] <- "Characteristic_Name"
colnames(data.result)[7] <- "Result_Value_Unit"
colnames(data.result)[6] <- "Result_Value"
colnames(data.result)[3] <- "Analysis_Start_Date"
colnames(data.result)[9] <- "Activity_Type"
colnames(data.result)[10] <- "Activity_ID"


#Replace parameters with DEQ characteristic name
unique(data.result$Characteristic_Name)




data.result$Characteristic_Name <- sub("LDO%", "Dissolved oxygen saturation", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("LDO", "Dissolved oxygen (DO)", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("PAR", "Light, photosynthetic active radiation (PAR)", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("ORP", "Oxidation reduction potential (ORP)", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("SpCond", "Specific conductance", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("Temp", "Temperature, water", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("BP", "Barometric pressure", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("CHL", "Chlorophyll a (probe)", data.result$Characteristic_Name)

#Add Characteristic ID from DEQ
for(i in 1:length(data.result$Characteristic_Name)){
  if(data.result$Characteristic_Name[i] == "Barometric pressure"){
    data.result$Characteristic_ID[i] <- "BAR-PRESSURE"
  }
  if(data.result$Characteristic_Name[i] == "Chlorophyll a (probe)"){
    data.result$Characteristic_ID[i] <- "CHL-A-P"
  }
  if(data.result$Characteristic_Name[i] == "Dissolved oxygen (DO)"){
    data.result$Characteristic_ID[i] <- "DO"
  }
  if(data.result$Characteristic_Name[i] == "Dissolved oxygen saturation"){
    data.result$Characteristic_ID[i] <- "DO-SAT"
  }
  if(data.result$Characteristic_Name[i] == "Oxidation reduction potential (ORP)"){
    data.result$Characteristic_ID[i] <- "ORP"
  }
  if(data.result$Characteristic_Name[i] == "pH"){
    data.result$Characteristic_ID[i] <- "PH"
  }
  if(data.result$Characteristic_Name[i] == "Specific conductance"){
    data.result$Characteristic_ID[i] <- "SC"
  }
  if(data.result$Characteristic_Name[i] == "Turbidity"){
    data.result$Characteristic_ID[i] <- "TURB"
  }
  if(data.result$Characteristic_Name[i] == "Light, photosynthetic active radiation (PAR)"){
    data.result$Characteristic_ID[i] <- "PAR"
  }
  if(data.result$Characteristic_Name[i] == "Chlorophyll a (probe)"){
    data.result$Characteristic_ID[i] <- "CHL-A-P"
  }
  if(data.result$Characteristic_Name[i] == "Dissolved oxygen (DO)"){
    data.result$Characteristic_ID[i] <- "DO"
  }
  if(data.result$Characteristic_Name[i] == "Dissolved oxygen saturation"){
    data.result$Characteristic_ID[i] <- "DO-SAT"
  }
  if(data.result$Characteristic_Name[i] == "Oxidation reduction potential (ORP)"){
    data.result$Characteristic_ID[i] <- "ORP"
  }
  if(data.result$Characteristic_Name[i] == "pH"){
    data.result$Characteristic_ID[i] <- "PH"
  }
  if(data.result$Characteristic_Name[i] == "Specific conductance"){
    data.result$Characteristic_ID[i] <- "SC"
  }
  if(data.result$Characteristic_Name[i] == "Turbidity"){
    data.result$Characteristic_ID[i] <- "TURB"
  }
  if(data.result$Characteristic_Name[i] == "Temperature, water"){
    data.result$Characteristic_ID[i] <- "TEMP-W"
  }
  if(data.result$Characteristic_Name[i] == "Light, photosynthetic active radiation (PAR)"){
    data.result$Characteristic_ID[i] <- "PAR"
  }
  if(data.result$Characteristic_Name[i] == "Depth, Secchi disk depth"){
    data.result$Characteristic_ID[i] <- "DEPTH-SECCHI"
  }
}

#Reformat units

unique(data.result$Result_Value_Unit) 

#units have expressed differently between PC and mac, these are from PC
data.result$Result_Value_Unit <- sub("æg/l", "ug/l", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("Sat", "%", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("Units", "None", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("æS/cm", "uS/cm", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("øF", "deg F", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("Ntu", "NTU", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("TDS", "NTU", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("N TU", "NTU", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("æE/s/mý", "umol/m2/s", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("æg/l", "ug/l", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("Êg/l", "ug/l", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("Sat", "%", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("Units", "None", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("æS/cm", "uS/cm", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("ÊS/cm", "uS/cm", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("øF", "deg F", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("¯F", "deg F", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("^NT$", "NTU", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("æE/s/mý", "uE/m2/sec", data.result$Result_Value_Unit)
data.result$Result_Value_Unit <- sub("Ã¦E/s/mÃ½", "uE/m2/sec", data.result$Result_Value_Unit)
data.result$Result_Value_Unit[which(str_detect(data.result$Result_Value_Unit, "ÊE/s/m"))] <- "uE/m2/sec"

#add other needed columns
data.result$Value_Type <- "Actual"
data.result$Analytical_Method_ID <- "NA"
data.result$Sample_Fraction <- "NA"

str(data.result)

data.result$Result_Value <- as.character(data.result$Result_Value)
data.result$Analysis_Start_Date <- as.character(data.result$Analysis_Start_Date)

#Bind working results dataframe to template
result.wsheet <- rbind.fill(result.template, data.result)


#Remove NAs
result.wsheet <- clear.na(result.wsheet)

# Build Excel Workbook ####
#Read in lake activity and result information from previous script
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
lake.act <- read.csv( "WWQMPLakes_Field_Activity_25_cr.csv")
lake.res <- read.csv( "WWQMPLakes_Field_Result_25_cr.csv")

#Merge stream and lake activity and result data
activity <- rbind.fill(activity, lake.act)
result <- rbind.fill(result.wsheet, lake.res)

#Remove unneeded columns
result <- result[, -c(38:43)]

#Read in project information
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2022/HydroShareIntermediateData2022")
project <- read_excel("Project_Station_ForWQX.xlsx",
                      sheet= "Project_WWQMP")

#Read in station information 
stations <- read_excel("Project_Station_ForWQX.xlsx",
                       sheet= "Stations_WWQMP")

stations$Latitude <- as.numeric(stations$Latitude) #NAs introduced fine, just for heading
stations$Longitude <- as.numeric(stations$Longitude)

#Start excel for this upload using project information
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
write.xlsx(project, "WWQMP_Field_EDD_25_cr.xlsx",
           sheetName= "Project")

wwqmp.field <- loadWorkbook("WWQMP_Field_EDD_25_cr.xlsx")

#Add station worksheet
addWorksheet(wb= wwqmp.field, "Stations")

#Fill station worksheet with data
writeData(wb= wwqmp.field, sheet= "Stations", x= stations)

#Add activity worksheet
addWorksheet(wb= wwqmp.field, "Activity")

#Fill activity worksheet
writeData(wb= wwqmp.field, sheet= "Activity", x= activity)

#Add result worksheet
addWorksheet(wb= wwqmp.field, "Result")

#Fill result worksheet
writeData(wb= wwqmp.field, "Result", x= result)

#Write out final excel spreadsheet for WWQMP program
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
saveWorkbook(wwqmp.field, "WWQMP_Field_EDD_25_cr.xlsx",
             overwrite = TRUE)


