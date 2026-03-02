#script to process NMLN hydrolab data
#previous script is NMLNHYdrolab2023


# setup ####
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace

library(stringr)
library(readxl)
library(plyr)
library(openxlsx)
library(janitor)
library(lubridate)
library(tidyverse)

#Function to remove NAs from data frame, leaving empty cells
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
data <- read.csv("HydrolabCompilationLakesSecchiTurb2025.csv") %>% clean_names()
#remove notes and x
data<-data[,2:9]
datanmln<-read.csv("HydrolabCompilationNMLN2025.csv") %>% clean_names()
#combine these together
data<-rbind(data, datanmln)
glimpse(data)
data$date<- ymd(data$date)
#other option to change date
# data$date <- as.POSIXct(data$date, format= "%m-%d-%Y")
#rename site to station id
names(data)[names(data) == 'site_id'] <- 'Station_ID'



#Parameters not being uploaded
unique(data$parameter)

remove4 <- which(is.na(data$parameter)==TRUE
                 |data$parameter == "Res"
                 |data$parameter == "Sal"
                 |data$parameter == "PAR.1"
                 |data$parameter == "refPAR"
                 |data$parameter == "BPSvr4"
                 |data$parameter == "TDS"
)
test4 <- data[remove4 ,]

data <- data[-remove4 ,]

data <- unique(data[c("result", "parameter", "units", "depth",
                      "time", "depth_units", "date", "Station_ID")])


# create WQX style activity ID ####
#Add activity type as field measure/observation
data$ActivityType <- "F-MSR/OBS"


#Add midnight as a default time if time is missing
#error if there are no times missing (which is fine)
for(i in 1:length(data$time)){
  if(data$time[i] == ""){
    data$time[i] <- "00:00:00"
  }
}

#Round times to nearest second if necessary
#error if there is no rounding needed
for(i in 1:length(data$time)){
  if(nchar(data$time[i]) > 8){
    data$time[i] <- substr(data$time[i], 1, 8)
  }
}

#Create activity ID
data$my.activty <- paste0(data$Station_ID, "_",
                          data$date,"_", 
                          data$time,"_",
                          substr(data$ActivityType,1,1))

# check length is less than 35 characters
data$activity_length <- nchar(data$my.activty)
range(data$activity_length)

# Test Why Activity IDs Aren't Unique ####
#One duplicate for negative depth which will be removed later in this process
#duplicates for turbidity are allowed when for one site on lake and time was estimated
data$temp <- paste0(data$my.activty,"__",
                    data$parameter)
test5 <- aggregate(data$parameter, 
                   by = list(data$temp),
                   length)
test5[test5$x > 1,]

# Create Activity Worksheet ####
activity.wsheet.all <- data[,c('my.activty', 'ActivityType',
                               'date', 'time',
                               "Station_ID")]
str(activity.wsheet.all)

activity.wsheet <- unique(activity.wsheet.all[,
                                              c('my.activty', 'ActivityType',
                                                'date', 'time',
                                                "Station_ID")])



head(activity.wsheet)
str(activity.wsheet)


#Add columns to activity worksheet for medium and organization information
activity.wsheet$Medium <- "Water"
activity.wsheet$Medium_Subdivision <- 'SW'
activity.wsheet$Activity_Conducting_Organization <- 
  'Whitefish Lake Inst'
activity.wsheet$Personnel <- "WLI Staff"


#Add time zones based on date
activity.wsheet$ActivityDate <- as.POSIXct(activity.wsheet$date, format= "%Y-%m-%d")

#this is for daylight savings be sure to add new years date for daylight savings ####
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
         |activity.wsheet$ActivityDate[i] > as.POSIXct("2023-03-12") & activity.wsheet$ActivityDate[i] < as.POSIXct("2023-11-05"),
         yes= activity.wsheet$Activity_Start_Timezone[i] <- "MDT",
         no= activity.wsheet$Activity_Start_Timezone[i] <- "MST")
  
}

#Add project IDs based on site MAKE SURE THESE ACTUALLY MATCH WHAT IS NEEDED####
activity.wsheet$Project_ID <- ifelse(str_detect(activity.wsheet$Station_ID, "WF-LK-IP1")== TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WF-LK-IP2")== TRUE
                                     | str_detect(activity.wsheet$Station_ID, "TALLY")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WF.LK.IP")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "SWIFT-CRK-OLN")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "SWIFT-CRK-DEL")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "SMITH-CRK-ELD")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "LAZY-CRK-S")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WF-R-SPB")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WALKER-CRK-MR")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "VIKING-CRK-WA")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "LOGAN")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "BEAV-CRK-RR")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WF-R-2S")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WF-R-H40")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WF-R-JPR")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "COW-CRK-PA")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "WF-R-JPR")==TRUE
                                     | str_detect(activity.wsheet$Station_ID, "COW-CRK-RR")==TRUE,
                                     
                                     yes= "WWQMP",
                                     no= "NMLN")

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
colnames(activity.wsheet)[11] <- "Activity_Start_Time_Zone"

str(activity.wsheet)
str(activity.template)

#Convert dates to character strings to match template
activity.wsheet$Activity_Start_Date <- as.character(activity.wsheet$Activity_Start_Date)


# rbind.fill() to merge working dataframe to the template; note on rbind.fill- if column structures don't match in addition to names, it won't work
activity <- rbind.fill(activity.template, activity.wsheet)
activity <- activity[, -28]



#Clear NAs from activity data frame
activity <- clear.na(activity)

#Create Result Worksheet ####

# read in template for results worksheet
result.template <- data.frame(read_excel("MT-eWQX_EDD_20170221.xlsx", sheet= "Result"))
colnames(result.template)[1] <- "Activity_ID"
data.result <- data
#Check that activity IDs match between activity and result sheet
which(data.result$my.activty %in% activity$Activity_ID==FALSE)
which(activity$Activity_ID %in% data.result$my.activty==FALSE) #1 is for template headings, not a problem
colnames(result.template)
#Adjust column names to match template
#check to make sure these are correct ####
colnames(data.result)[2] <- "Characteristic_Name"
colnames(data.result)[3] <- "Result_Value_Unit"
colnames(data.result)[1] <- "Result_Value"
colnames(data.result)[7] <- "Analysis_Start_Date"
colnames(data.result)[10] <- "Activity_ID"
colnames(data.result)[4] <- "Result_Depth_Height_Measure"
colnames(data.result)[6] <- "Result_Depth_Height_Measure_Unit"

#Remove unneeded columns
data.result <- data.result[, -c(11:12)]

#Format parameters to match DEQ characteristic names
unique(data.result$Characteristic_Name)

data.result$Characteristic_Name <- sub("BP", "Barometric pressure", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("CHL", "Chlorophyll a (probe)", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("^LDO$", "Dissolved oxygen (DO)", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("LDO%", "Dissolved oxygen saturation", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("LDO.", "Dissolved oxygen saturation", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("LDO", "Dissolved oxygen saturation", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("ORP", "Oxidation reduction potential (ORP)", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("^Turb$", "Turbidity", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("^Turb.$", "Turbidity", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("SpCond", "Specific conductance", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("PAR", "Light, photosynthetic active radiation (PAR)", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("Secchi Depth", "Depth, Secchi disk depth", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("Temp", "Temperature, water", data.result$Characteristic_Name)
data.result$Characteristic_Name <- sub("DEPTH-SECCHI", "Depth, Secchi disk depth", data.result$Characteristic_Name)
#Add characteristic ID based on Characteristic name
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


unique(data.result$Result_Value_Unit)

#Reformat units, units have expressed differently on Mac and PC, these are from PC
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

for(i in 1:length(data.result$Result_Value_Unit)){
  if(is.na(data.result$Result_Value_Unit[i])==TRUE){
    data.result$Result_Value_Unit[i] <- "NTU"
  }
}


#add other needed columns
data.result$Value_Type <- "Actual"
data.result$Analytical_Method_ID <- "NA"
data.result$Sample_Fraction <- "NA"

#Format date columns as character strings to match template
str(data.result)
data.result$Result_Value <- as.character(data.result$Result_Value)
data.result$Analysis_Start_Date <- as.character(data.result$Analysis_Start_Date)

#Bind results with template
result.wsheet <- rbind.fill(result.template, data.result)

#Removing Negative Depths ####
#find activities where profiles with negative depths greater than 0.5 meter (half a meter above lake surface)
#pretty sure na introduction is fine its just row one that says "text"
result.wsheet$Result_Depth_Height_Measure <- as.numeric(result.wsheet$Result_Depth_Height_Measure)
glimpse(result.wsheet$Result_Depth_Height_Measure)
test5 <- which(result.wsheet$Result_Depth_Height_Measure < (-0.5))


#Store activity IDs for removed profiles
remove5 <- result.wsheet[test5 ,]
removeid <- unique(remove5$Activity_ID)

#Activities are specific to time, to get whole profile exclude time from ID
for(i in 1:length(removeid)){
  removeid[i] <- substr(removeid[i], 1, nchar(removeid[i])- 11)
}
print(removeid)

#create column in activity ID sheet to flag for removal
activity$remove <- ""

#Loop for flagging activity IDs to be removed
#If there is nothing to be removed this won't work
for(i in 1:length(removeid)){
  
  id <- removeid[i]
  
  for(j in 1:length(activity$Activity_ID)){
    
    if(str_detect(activity$Activity_ID[j], id)){
      
      activity$remove[j] <- "yes"
    }
    
  }
  
}

#Keep only activity IDs that aren't flagged
check <- activity[activity$remove == "yes" ,]
allid <- unique(check$Activity_ID)

activity <- activity[activity$remove != "yes" ,]


#Remove profiles for greater that 0.5 negative depths from result sheet
result.wsheet$remove <- ""

for(i in 1:length(allid)){
  
  id <- allid[i]
  
  for(j in 1:length(result.wsheet$Activity_ID)){
    
    if(str_detect(result.wsheet$Activity_ID[j], id)){
      
      result.wsheet$remove[j] <- "yes"
    }
    
  }
  
}

#checking
check5 <- result.wsheet[result.wsheet$remove == "yes" ,]
result.wsheet <- result.wsheet[result.wsheet$remove != "yes" ,]

#Find activities for remaining negative depths
#These specific IDs will be removed, not whole profile
test6 <- which(result.wsheet$Result_Depth_Height_Measure < (0))
check6 <- result.wsheet[test6 ,]
#remove mary ronan w from check 2 because all of its activity ids are the same
#that means that this process will just take the whole profile out
check6<- check6 %>% filter(!Station_ID == "MARYRON-W")
removeid2 <- unique(check6$Activity_ID)

#Loop for flagging individual activity IDs to be removed
for(i in 1:length(removeid2)){
  
  id <- removeid2[i]
  
  for(j in 1:length(activity$Activity_ID)){
    
    if(activity$Activity_ID[j] == id){
      
      activity$remove[j] <- "yes"
    }
    
  }
  
  
}


#Keep only activity IDs that aren't flagged
check2 <- activity[activity$remove == "yes" ,]
#remove mary ronan w from check 2 because all of its activity ids are the same
#that means that this process will just take the whole profile out
check2<- check2 %>% filter(!Station_ID == "MARYRON-W")


activity <- activity[activity$remove != "yes" ,]

#remove mary ronan w from check 2 because all of its activity ids are the same
#that means that this process will just take the whole profile out
check2<- check2 %>% filter(!Station_ID == "MARYRON-W")

#Remove profiles for greater that 0 negative depths from result sheet
for(i in 1:length(removeid2)){
  
  id <- removeid2[i]
  
  for(j in 1:length(result.wsheet$Activity_ID)){
    
    if(result.wsheet$Activity_ID[j] == id){
      
      result.wsheet$remove[j] <- "yes"
    }
    
  }
  
  
}

#checking
check5 <- result.wsheet[result.wsheet$remove == "yes" ,]
result.wsheet <- result.wsheet[result.wsheet$remove != "yes" ,]


#need to hand remove the negative mary ronan values ####
#there is one depth that has 8 parameters at -0.01
glimpse(result.wsheet)

result.wsheet<- result.wsheet %>% filter(!Result_Depth_Height_Measure %in% c(-0.01))


#Remove column for flagging depths
#make sure this is referencing right col #####
result.wsheet <- result.wsheet[, -41]

#Clear NAs from results sheet
result.wsheet <- clear.na(result.wsheet)

#Generate Formatted Files ####

#Separate by project
activity.nmln <- activity[activity$Project_ID == "NMLN" ,]
activity.wwqmp <- activity[activity$Project_ID == "WWQMP" ,]

#Remove column for flagging negative depths
activity.nmln <- activity.nmln[, -28]
activity.wwqmp <- activity.wwqmp[, -28]


#Separate results by project
unique(result.wsheet$Activity_ID)
baseline <- which(str_detect(result.wsheet$Activity_ID, "WF-LK-IP") == TRUE 
                  | str_detect(result.wsheet$Activity_ID, "TALLY") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "WF.LK.") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "BEAV-CRK-RR") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "COW-CRK-PA") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "HASK-CRK-MR") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "HELLR-CRK-T") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "LAZY-CRK-S") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "LOGAN") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "SMITH-CRK-ELD") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "SWIFT-CRK-DEL") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "SWIFT-CRK-OLN") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "VIKING-CRK-WA") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "WALKER-CRK-WA") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "WALKER-CRK-MR") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "WF-R-SPB") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "HELLR-CRK-T") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "HASK-CRK-MR") == TRUE
                  | str_detect(result.wsheet$Activity_ID, "WALKER-CRK-WA") == TRUE)

wwqmp <- result.wsheet[baseline ,]
nmln <- result.wsheet[-baseline ,]



#DO NOT RUN UNTIL YOU HAVE NEW DATA FOR THESE ####

#Write csvs for Whitefish and Tally Lakes (will be combined with streams for final excel)
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
write.csv(activity.wwqmp, "WWQMPLakes_Field_Activity_25_cr.csv",
          row.names = FALSE)

write.csv(wwqmp, "WWQMPLakes_Field_Result_25_cr.csv",
          row.names= FALSE)

#now go to 4_WQX_HydrolabsStream_year_CR script to build final WWQMP work book

#THIS IS ONLY FOR NMLN ########
#Read in project information (NMLN Only)
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2022/HydroShareIntermediateData2022")
project <- read_excel("Project_Station_ForWQX.xlsx",
                      sheet= "Project_NMLN")

#Read in station information (NMLN Only)
stations <- read_excel("Project_Station_ForWQX.xlsx",
                       sheet= "Stations_NMLN")

stations$Latitude <- as.numeric(stations$Latitude) #NAs introduced fine, just for heading
stations$Longitude <- as.numeric(stations$Longitude)

#Start excel for this upload using project information
write.xlsx(project, "NMLN_Field_EDD_25_cr.xlsx",
           sheetName= "Project")

nmln.field <- loadWorkbook("NMLN_Field_EDD_23_cr.xlsx")

#Add station worksheet
addWorksheet(wb= nmln.field, "Stations")

#Fill station worksheet with data
writeData(wb= nmln.field, sheet= "Stations", x= stations)

#Add activity worksheet
addWorksheet(wb= nmln.field, "Activity")

#Fill activity worksheet with data
writeData(wb= nmln.field, sheet= "Activity", x= activity.nmln)

#Add results worksheet
addWorksheet(wb= nmln.field, "Result")

#Fill results worksheet with results
writeData(wb= nmln.field, sheet= "Result", x= nmln)


#Write final excel spreadsheet for NMLN field data
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
saveWorkbook(wb= nmln.field, "NMLN_Field_EDD_25_cr.xlsx",
             overwrite = TRUE)


