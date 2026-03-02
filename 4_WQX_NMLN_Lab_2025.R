# setup ####
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace

library(stringr)
library(readxl)
library(plyr)
library(openxlsx)
library(dplyr)
#Function for removing NAs from empty cells
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
#Merge data frames from different labs
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
data.all <- read.csv("FLBS_NMLNEDD_26-02-26_cr.csv")

#Format date as POSIX
data.all$Activity_Date <- as.POSIXct(data.all$Activity_Date, format= "%Y-%m-%d")

#Add time as midnight if time is missing
for(i in 1:length(data.all$Activity_Time)){
  if(data.all$Activity_Time[i]==""){
    data.all$Activity_Time[i] <- "00:00:00"
  }
}

#Remove spaces from times
data.all$Activity_Time <- sub(" ", "", data.all$Activity_Time)

#Convert mg/l to ug/l to match FLBS units
#List units
units <- unique(data.all[c("Characteristic_Name", "Result_Value_Unit")])

#standardize parameter names
data.all$Characteristic_Name <- sub("Total Nitrogen, mixed forms", 
                                    "Total nitrogen, mixed forms", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("Nitrate \\+ Nitrite", 
                                    "Inorganic nitrogen (nitrate and nitrite)", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("^Nitrite$", 
                                    "Inorganic nitrogen (nitrate and nitrite)", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("Ferrous Iron", 
                                    "Iron", data.all$Characteristic_Name)


#Convert results to numbers from characters
data.all$Result_Value <- as.numeric(data.all$Result_Value)

#Loop for multiplying result by 1000 and changing units to ug/l
#Not for all parameters because some like TSS are mg/l at both labs
for(i in 1:length(data.all$Result_Value_Unit)){
  if(data.all$Characteristic_Name[i] == "Inorganic nitrogen (nitrate and nitrite)" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  if(data.all$Characteristic_Name[i] == "Iron" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Manganese" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Nitrite" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Nitrate + Nitrite" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Nitrate + Nitrite" 
     & data.all$Result_Value_Unit[i] == "mg/L"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Nutrient-nitrogen" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Ammonia" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Ammonia" 
     & data.all$Result_Value_Unit[i] == "mg/L"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Orthophosphate" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Orthophosphate" 
     & data.all$Result_Value_Unit[i] == "mg/L"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Phosphate-phosphorus" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Sulfate" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Total nitrogen, mixed forms" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Total nitrogen, mixed forms" 
     & data.all$Result_Value_Unit[i] == "mg/L"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Total Phosphorus, mixed forms" 
     & data.all$Result_Value_Unit[i] == "mg/l"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
  if(data.all$Characteristic_Name[i] == "Total Phosphorus, mixed forms" 
     & data.all$Result_Value_Unit[i] == "mg/L"){
    
    data.all$Result_Value[i] <- data.all$Result_Value[i] *1000
    data.all$Result_Value_Unit[i] <- "ug/l"
    
  }
  
}




#Start excel spreadsheet and add project and station information ####
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2022/HydroShareIntermediateData2022")
#Read in project information
project <- read_excel("Project_Station_ForWQX.xlsx",
                      sheet= "Project_NMLN")

#Read in station information
stations <- read_excel("Project_Station_ForWQX.xlsx",
                       sheet= "Stations_NMLN")

stations$Latitude <- as.numeric(stations$Latitude) #NAs introduced fine, just for heading
stations$Longitude <- as.numeric(stations$Longitude)

#Start excel for this upload using project information
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2024/HydroShareIntermediateData2024")
write.xlsx(project, "NMLN_Lab_EDD_25-04-01_cr.xlsx",
           sheetName= "Project")

nmln.lab <- loadWorkbook("NMLN_Lab_EDD_25-04-01_cr.xlsx")

#Add station worksheet
addWorksheet(wb= nmln.lab, "Stations")

#Fill station worksheet with data
writeData(wb= nmln.lab, sheet= "Stations", x= stations)


#Create Activity Worksheet ####

#Create smaller dataframe, keep only needed columns
colnames(data.all)


data <- data.all[,c("Activity_ID", "Activity_Date", 
                    "Activity_Time",  
                    "Characteristic_Name", "Result_Value",
                    "Site_ID", "Activity_Type",
                    "Result_Depth_Height_Measure" )]

# #remove 2011 (except Swan N and S) because it is already in NWQP
# remove<- which(str_detect(data$Activity_Date, "2011-08-") & data$SiteID != "SWAN-N" & data$SiteID != "SWAN-S"
#                |str_detect(data$Activity_Date, "2011-09-") 
#                |str_detect(data$Activity_Date, "2011-11-"))
# 
# check <- data[remove ,]
# check2 <- data.all[remove ,]
# 
# data <- data[!remove ,]
# data.all <- data.all[-remove ,] #remove from original dataframe as well

#Remove NAs from data frame
data <- clear.na(data)

#add activity method
for(i in 1:length(data$Result_Depth_Height_Measure)){
  ifelse(data$Result_Depth_Height_Measure[i] != "",
         yes= data$Sample_Collection_Method_ID[i] <- "VAN_DORN",
         no= data$Sample_Collection_Method_ID[i] <- "IWS")
}


# create WQX style activity ID ####
data$my.activty <- paste0(data$Site_ID, "_",
                          data$Activity_Date,"_", 
                          data$Activity_Time,"_",
                          #data$Result_Depth_Height_Measure, "_",
                          substr(data$Activity_Type,1,1))

#Create WQX activity IDs for results data frame as well
data.all$my.activity <- paste0(data.all$Site_ID, "_",
                               data.all$Activity_Date,"_", 
                               data.all$Activity_Time,"_",
                               #data$Result_Depth_Height_Measure, "_",
                               substr(data.all$Activity_Type,1,1))

# #Create WQX activity ID for 2011 data for HydroShare upload
# check2$Activity_ID <- paste0(check2$SiteID, "_",
#                              check2$Activity_Date,"_", 
#                              check2$Activity_Time,"_",
#                              substr(check2$Activity_Type,1,1))
# check2 <- clear.na(check2)

# #Write csv for 2011 data for Hydroshare upload
# write.csv(check2, "../4_FormattingResults/2011LakesLabData.csv",
#           row.names = FALSE)

# check length is less than 35 characters
data$activity_length <- nchar(data$my.activty)
range(data$activity_length)

#swan specific QAQC not needed ####
#Add indication of depth integrated samples from Swan
#These samples are from the same time as a specific depth sample,
#but are not from that depth so should not be with same Activity ID
# swan <- data.all[which(str_detect(data.all$SiteID, "SWAN")
#                        & is.na(data.all$Result_Depth_Height_Measure)
#                        & str_detect(data.all$Activity_ID, "-")
#                        & str_detect(data.all$Activity_ID, "Blank") == FALSE) ,]
# 
# #Activities for depths that are depth integrated
# swan.ids <- unique(swan$Activity_ID)
# 
# #Loop for adding "INT" to swan activity IDs in activity worksheet
# for(i in 1:length(swan.ids)){
#   
#   id <- swan.ids[i]
#   
#   for(j in 1:length(data$my.activty)){
#     
#     if(data$Activity_ID[j] == id){
#       
#       data$my.activty[j] <- paste0(data$my.activty[j], "_INT")
#     }
#     
#   }
#   
# }
# 
# #Loop for adding "INT" to swan activity IDs in result worksheet
# for(i in 1:length(swan.ids)){
#   
#   id <- swan.ids[i]
#   
#   for(j in 1:length(data.all$my.activity)){
#     
#     if(data.all$Activity_ID[j] == id){
#       
#       data.all$my.activity[j] <- paste0(data.all$my.activity[j], "_INT")
#     }
#     
#   }
#   
# }

# test for why activity ids are not unique
#Multiple depths in lakes, this is not a problem

data$temp <- paste0(data$my.activty,"__",
                    data$Characteristic_Name)
test2 <- aggregate(data$Characteristic_Name, 
                   by = list(data$temp),
                   length)
test2[test2$x > 1,]

#Create data frame with just needed columns
colnames(data)
activity.wsheet.all <- data[,c('my.activty', 'Activity_Type',
                               'Activity_Date', 'Activity_Time',
                               "Site_ID" , "Sample_Collection_Method_ID" 
)]
str(activity.wsheet.all)

#Create unique list of activity IDs
activity.wsheet <- unique(activity.wsheet.all[,
                                              c('my.activty', 'Activity_Type',
                                                'Activity_Date', 'Activity_Time',
                                                "Site_ID" , "Sample_Collection_Method_ID"
                                              )])


# replace Activity_Type with DEQ accepted values
head(activity.wsheet)
str(activity.wsheet)

activity.wsheet$Activity_Type <- sub("Routine", "S-ROUTINE", activity.wsheet$Activity_Type)
activity.wsheet$Activity_Type <- sub("Blank", "QC-FB", activity.wsheet$Activity_Type)
activity.wsheet$Activity_Type <- sub("Duplicate", "QC-FD", activity.wsheet$Activity_Type)

#fill in other required columns (medium, organization information)
activity.wsheet$Medium <- "Water"
activity.wsheet$Medium_Subdivision <- 'SW'
activity.wsheet$Activity_Conducting_Organization <- 
  'Whitefish Lake Inst'
activity.wsheet$Personnel <- "WLI Staff"

#determine if it is MST or MDT for the timezone
#add most current years here #####
activity.wsheet$Activity_Date <- as.POSIXct(activity.wsheet$Activity_Date, format= "%Y-%m-%d")

for(i in 1:length(activity.wsheet$Activity_Date)){
  ifelse(activity.wsheet$Activity_Date[i] > as.POSIXct("2007-03-11") & activity.wsheet$Activity_Date[i] < as.POSIXct("2007-11-04")
         | activity.wsheet$Activity_Date[i] > as.POSIXct("2008-03-09") & activity.wsheet$Activity_Date[i] < as.POSIXct("2008-11-02")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2009-03-08") & activity.wsheet$Activity_Date[i] < as.POSIXct("2009-11-01")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2010-03-14") & activity.wsheet$Activity_Date[i] < as.POSIXct("2010-11-07")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2011-03-13") & activity.wsheet$Activity_Date[i] < as.POSIXct("2011-11-06")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2012-03-11") & activity.wsheet$Activity_Date[i] < as.POSIXct("2012-11-04")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2013-03-10") & activity.wsheet$Activity_Date[i] < as.POSIXct("2013-11-03")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2014-03-09") & activity.wsheet$Activity_Date[i] < as.POSIXct("2014-11-02")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2015-03-08") & activity.wsheet$Activity_Date[i] < as.POSIXct("2015-11-01")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2016-03-13") & activity.wsheet$Activity_Date[i] < as.POSIXct("2016-11-06")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2017-03-12") & activity.wsheet$Activity_Date[i] < as.POSIXct("2017-11-05")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2018-03-11") & activity.wsheet$Activity_Date[i] < as.POSIXct("2018-11-04")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2019-03-10") & activity.wsheet$Activity_Date[i] < as.POSIXct("2019-11-03")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2020-03-08") & activity.wsheet$Activity_Date[i] < as.POSIXct("2020-11-01")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2021-03-14") & activity.wsheet$Activity_Date[i] < as.POSIXct("2021-11-07")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2022-03-13") & activity.wsheet$Activity_Date[i] < as.POSIXct("2022-11-06")
         |activity.wsheet$Activity_Date[i] > as.POSIXct("2023-03-12") & activity.wsheet$Activity_Date[i] < as.POSIXct("2023-11-05"),
         yes= activity.wsheet$Activity_Start_Timezone[i] <- "MDT",
         no= activity.wsheet$Activity_Start_Timezone[i] <- "MST")
  
}





#Add equipment
activity.wsheet$Sample_Collection_Equipment_Name <- "Van Dorn Bottle"

#Add project ID
activity.wsheet$Project_ID <- "NMLN"

# read in WQX template activity column names
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2022/HydroShareIntermediateData2022")
activity.template <- data.frame(read_excel("MT-eWQX_EDD_20170221.xlsx", sheet= "Activity"))
colnames(activity.template)[1] <- "Project_ID"

colnames(activity.wsheet)

#Rename column names to match template
colnames(activity.wsheet)[5] <- "Station_ID"
colnames(activity.wsheet)[1] <- "Activity_ID"
colnames(activity.wsheet)[3] <- "Activity_Start_Date"
colnames(activity.wsheet)[4] <- "Activity_Start_Time"
colnames(activity.wsheet)[11] <- "Activity_Start_Time_Zone"

str(activity.wsheet)
str(activity.template)

#Make dates into character string (to match template format)
activity.wsheet$Activity_Start_Date <- as.character(activity.wsheet$Activity_Start_Date)

# rbind.fill() to merge working dataframe to the template; note on rbind.fill- if column structures don't match in addition to names, it won't work
activity <- rbind.fill(activity.template, activity.wsheet)

#Add start and end depths for Swan integrated samples
for(i in 1:length(activity$Activity_ID)){
  
  if(str_detect(activity$Activity_ID[i], "INT")){
    
    activity$Activity_Upper_Depth[i] <- "0"
    activity$Activity_Upper_Depth_Units[i] <- "m"
    activity$Activity_Lower_Depth[i] <- "30"
    activity$Activity_Lower_Depth_Units[i] <- "m"
  }
  
}


#Clear NAs
activity <- clear.na(activity)

#Add activity worksheet to workbook
addWorksheet(wb= nmln.lab, sheetName= "Activity")

#Fill worksheet with data
writeData(wb= nmln.lab, sheet= "Activity", x= activity)

#Create Results Worksheet ####

# read in results worksheet template
result.template <- data.frame(read_excel("MT-eWQX_EDD_20170221.xlsx", sheet= "Result"))
colnames(result.template)[1] <- "Activity_ID"

#Move Activity IDs to original column and delete temporary column
data.all$Activity_ID <- data.all$my.activity
data.all <- data.all[, -43]

data.result <- data.all

#Make sure activity IDs match between activity and result worksheets
which(data.result$Activity_ID %in% activity$Activity_ID==FALSE)
which(activity$Activity_ID %in% data.result$Activity_ID==FALSE) #1 is okay, from column headings

#Remove unnecessary columns
#this was deleating activity ID so changed from 2 to 1 for deleated col ####
data.result <- data.result[, -c(1, 39:42)]

names1 <- colnames(data.result)
names2 <- colnames(result.template)

which(names1 %in% names2==FALSE)

#Rename columns that don't match template
colnames(data.result)[4] <- "Biological_Taxonomic_Name"
colnames(data.result)[5] <- "Biological_Unidentified_Species_Name"
colnames(data.result)[6] <- "Biological_Tissue_Anatomy_Name"
colnames(data.result)[8] <- "Characteristic_Name"
colnames(data.result)[9] <- "Method_Speciation_Name"
colnames(data.result)[19] <- "Laboratory_Name"
colnames(data.result)[37] <- "Result_File_Name"

#Bind template and data
result.wsheet <- rbind.fill(result.template, data.result)

#Remove NAs
result.wsheet <- clear.na(result.wsheet)

#Create result worksheet
addWorksheet(wb= nmln.lab, sheetName= "Result")

#Fill worksheet with results
writeData(wb= nmln.lab, sheet = "Result", x= result.wsheet)

#Write final excel of results
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
saveWorkbook(wb= nmln.lab, file= "NMLN_Lab_EDD_26-02-27_cr.xlsx",
             overwrite = TRUE)

