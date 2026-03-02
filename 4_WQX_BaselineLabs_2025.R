#WQX baseline labs cr 2025 data
# setup ####
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace

library(stringr)
library(readxl)
library(plyr)
library(openxlsx)

#Function to remove NAs from empty cells
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
#data.energy <- read.csv('../3_CompilationResults/EnergyEDD_Baseline_22-01-11_mr.csv')
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025")
data.flbs <- read.csv("HydroShareIntermediateData2025/FLBS_BaselineEDD_26-02-24_cr.csv")


#Edits to 2014 data: 
#all activity times missing (assume midnight)
#all activity types routine
# data.2014$Activity_Time <- "00:00:00"
# data.2014$Activity_Type <- "Routine"

#Merge all baseline lab data
data.all <- rbind.fill(data.flbs)

data.all <- clear.na(data.all)

#temp make data.all flbs only ####
data.all<-data.flbs

#Replace missing times with midnight
#doesn't run because no missing time ####
# for(i in 1:length(data.all$Activity_Time)){
#   if(is.na(data.all$Activity_Time[i])){
#     data.all$Activity_Time[i] <- "00:00:00"
#   }
# }
#this works for when there are no NAs
for(i in 1:length(data.all$Activity_Time)){
  if(data.all$Activity_Time[i] == ""){
    data.all$Activity_Time[i] <- "00:00:00"
  }
}

#Format dates and times
data.all$Activity_Date <- as.POSIXct(data.all$Activity_Date, format= "%Y-%m-%d")
data.all$Activity_Time <- format(as.POSIXct(data.all$Activity_Time, format= "%H:%M:%S"), 
                                 format= "%H:%M:%S")

#Remove spaces from times
data.all$Activity_Time <- sub(" ", "", data.all$Activity_Time)

#Convert mg/l to ug/l to match FLBS units
#List units
units <- unique(data.all[c("Characteristic_Name", "Result_Value_Unit")])

#standardize parameter names
data.all$Characteristic_Name <- sub("Ammonia-nitrogen", 
                                    "Ammonia", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("Total Nitrogen, mixed forms", 
                                    "Total nitrogen, mixed forms", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("Inorganic Nitrogen \\(Nitrate\\)", 
                                    "Inorganic nitrogen (nitrate and nitrite)", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("Nitrate \\+ Nitrite", 
                                    "Inorganic nitrogen (nitrate and nitrite)", data.all$Characteristic_Name)


data.all$Characteristic_Name <- sub("^Nitrite$", 
                                    "Inorganic nitrogen (nitrate and nitrite)", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("Total suspended solids", 
                                    "Total Suspended Solids", data.all$Characteristic_Name)

data.all$Characteristic_Name <- sub("Organic Carbon", 
                                    "Organic carbon", data.all$Characteristic_Name)

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
  
  if(data.all$Characteristic_Name[i] == "Inorganic nitrogen (nitrate and nitrite)" 
     & data.all$Result_Value_Unit[i] == "mg/L"){
    
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
                      sheet= "Project_WWQMP")

#Read in station information
stations <- read_excel("Project_Station_ForWQX.xlsx",
                       sheet= "Stations_WWQMP")

stations$Latitude <- as.numeric(stations$Latitude) #NAs introduced fine, just for heading
stations$Longitude <- as.numeric(stations$Longitude)



#Start excel for this upload using project information
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025")
write.xlsx(project, "HydroShareIntermediateData2025/WWQMP_Baseline_EDD_26-02-24_cr.xlsx",
           sheetName= "Project")

wwqmp.lab <- loadWorkbook("HydroShareIntermediateData2025/WWQMP_Baseline_EDD_26-02-24_cr.xlsx")

#Add station worksheet
addWorksheet(wb= wwqmp.lab, "Stations")

#Fill station worksheet with data
writeData(wb= wwqmp.lab, sheet= "Stations", x= stations)

# create WQX style activity ID ####

#Smaller dataframe with relevant columns for activity sheet development
colnames(data.all)
data <- data.all[,c("Activity_ID", "Activity_Date", 
                    "Activity_Time",  
                    "Characteristic_Name", "Result_Value",
                    "Site_ID", "Activity_Type",
                    "Result_Depth_Height_Measure" )]

#clear nas from data
data<-clear.na(data)

data$my.activty <- paste0(data$Site_ID, "_",
                          data$Activity_Date,"_", 
                          data$Activity_Time,"_",
                          data$Result_Depth_Height_Measure, "_", #this line may or may not work/need to be commented out ####
                          substr(data$Activity_Type,1,1))

#FIx any double _ that may exist due to missing depths
data$my.activty<-sub("__", "_", data$my.activty)

#If time is missing at a lake, do include depth
#again none missing so doesn't run? ####
for(i in 1:length(data$Activity_Time)){
  
  if(data$Activity_Time[i] == "00:00:00"
     & data$Site_ID[i] == "WF-LK-IP1"){
    
    data$my.activty[i] <- paste0(data$Site_ID[i],"_",
                                 data$Activity_Date[i],"_", 
                                 data$Activity_Time[i],"_",
                                 substr(data$Activity_Type[i],1,1), "_",
                                 data$Result_Depth_Height_Measure[i]
    )
  }
  
}

# check length is less than 35 characters
data$activity_length <- nchar(data$my.activty)
range(data$activity_length)


# test for why activity ids are not unique ####

#Most repeat activities are for different depths without different times
#Including depth prevents duplicate IDs, but depth isn't needed for upload
#So these duplicates are not a problem
#The rest of the repeat IDs are for different fractions (dissolved vs total)
#This is also not a problem

data$temp <- paste0(data$my.activty,"__",
                    data$Characteristic_Name)
test2 <- aggregate(data$Characteristic_Name, 
                   by = list(data$temp),
                   length)
test2[test2$x > 1,]


# Create Activity Worksheet ####

#add activity methods
data$Sample_Collection_Method_ID <- ""

#THIS WAS CHANGED FROM ORIGINAL ####
for(i in 1:length(data$Result_Depth_Height_Measure)){
  if(is.na(data$Result_Depth_Height_Measure[i])){
    data$Sample_Collection_Method_ID[i] <- "VAN_DORN"
  }
  if(data$Sample_Collection_Method_ID[i] != "VAN_DORN" & data$Site_ID[i] == "TALLY"){
    data$Sample_Collection_Method_ID[i] <- "IWS"
  }
  if(data$Sample_Collection_Method_ID[i] != "VAN_DORN"& data$Site_ID[i] != "TALLY"){
    data$Sample_Collection_Method_ID[i] <- "EWI"
  }
}

#Create unique list of activity IDs 
activity.wsheet.all <- data[,c('my.activty', 'Activity_Type',
                               'Activity_Date', 'Activity_Time',
                               "Site_ID", "Sample_Collection_Method_ID")]
str(activity.wsheet.all)

activity.wsheet <- unique(activity.wsheet.all[,
                                              c('my.activty', 'Activity_Type',
                                                'Activity_Date', 'Activity_Time',
                                                "Site_ID", "Sample_Collection_Method_ID")])

# Add activity type names
head(activity.wsheet)
str(activity.wsheet)

activity.wsheet$Activity_Type <- sub("Routine", "S-ROUTINE", activity.wsheet$Activity_Type)
activity.wsheet$Activity_Type <- sub("Blank", "QC-FB", activity.wsheet$Activity_Type)
activity.wsheet$Activity_Type <- sub("Duplicate", "QC-FD", activity.wsheet$Activity_Type)

#Add medium
activity.wsheet$Medium <- "Water"
activity.wsheet$Medium_Subdivision <- 'SW'

#Add organization information
activity.wsheet$Activity_Conducting_Organization <- 
  'Whitefish Lake Inst'
activity.wsheet$Personnel <- "WLI Staff"

#Assign time zones
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

#Remove cow creek data, already uploaded
#I think we no longer want to do this ####
# remove <- which(str_detect(activity.wsheet$SiteID, "COW-CRK-PA")
#                 & str_detect(activity.wsheet$Activity_Date, "2021-")==FALSE)
# activity.wsheet <- activity.wsheet[-remove ,]

# read in WQX template activity column names 
#Read in DEQ activity sheet as template
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

#Format dates as character string to match template
activity.wsheet$Activity_Start_Date <- as.character(activity.wsheet$Activity_Start_Date)

# rbind.fill() to merge working dataframe to the template; note on rbind.fill- if column structures don't match in addition to names, it won't work
activity <- rbind.fill(activity.template, activity.wsheet)

#Add project ID
activity$Project_ID <- "WWQMP"


#Add activity collection equipment
for(i in 1:length(activity$Sample_Collection_Method_ID)){
  if(activity$Sample_Collection_Method_ID[i] == "IWS" | activity$Sample_Collection_Method_ID[i] == "VAN_DORN"){
    activity$Sample_Collection_Equipment_Name[i] <- "Van Dorn Bottle"
  }
  if(activity$Sample_Collection_Method_ID[i] == "EWI"){
    activity$Sample_Collection_Equipment_Name[i] <- "Water Bottle"
  }
}

activity <- clear.na(activity)

#Add activity sheet to workbook
addWorksheet(wb= wwqmp.lab, "Activity")

#Fill station worksheet with data
writeData(wb= wwqmp.lab, sheet= "Activity", x= activity)

#Create Results Worksheet ####
# Read in Result worksheet headings
result.template <- data.frame(read_excel("MT-eWQX_EDD_20170221.xlsx", sheet= "Result"))
colnames(result.template)[1] <- "Activity_ID"

#Copy data to results dataframe
data.result <- data.all

#Create Activity ID that matches activity worksheet
data.result$Activity_ID <- paste0(data.result$Site_ID, "_",
                                  data.result$Activity_Date,"_", 
                                  data.result$Activity_Time,"_",
                                  data$Result_Depth_Height_Measure, "_",
                                  substr(data.result$Activity_Type,1,1))

#And for lakes without times
for(i in 1:length(data.result$Activity_Time)){
  
  if(data.result$Activity_Time[i] == "00:00:00"
     & data.result$Site_ID[i] == "WF-LK-IP1"){
    
    data.result$Activity_ID[i] <- paste0(data.result$Site_ID[i], "_",
                                         data.result$Activity_Date[i],"_", 
                                         data.result$Activity_Time[i],"_",
                                         substr(data.result$Activity_Type[i],1,1), "_",
                                         data.result$Result_Depth_Height_Measure[i]
    )
  }
  
}

#FIx any double _ that may exist due to missing depths
data.result$Activity_ID<-sub("__", "_", data.result$Activity_ID)


#Check that all IDs match between Activity and Result sheets
which(data.result$Activity_ID %in% activity$Activity_ID==FALSE)
which(activity$Activity_ID %in% data.result$Activity_ID==FALSE) #the 1 is okay, this is text column at top

#Remove unnecessary columns ####
#THIS IS DELEATING ACTIVITY ID IS THAT RIGHT???? i CHANGED IT####
data.result <- data.result[, -c(2, 39:42)]

#Compare column names
names1 <- colnames(data.result)
names2 <- colnames(result.template)

which(names1 %in% names2==FALSE) #all good

#Bind data to template column names
result.wsheet <- rbind.fill(result.template, data.result)
result.wsheet$Result_Value <- as.numeric(result.wsheet$Result_Value)

result.wsheet$Method_Detection_Limit_Value <- format(as.numeric(result.wsheet$Method_Detection_Limit_Value),
                                                     scientific = FALSE)
result.wsheet$Lower_Reporting_Limit_Value <- format(as.numeric(result.wsheet$Lower_Reporting_Limit_Value),
                                                    scientific = FALSE)


#Clear NAs
result.wsheet <- clear.na(result.wsheet) 

#Add result sheet to workbook
addWorksheet(wb= wwqmp.lab, "Result")

#Fill result worksheet with data
writeData(wb= wwqmp.lab, sheet= "Result", x= result.wsheet)

#Write out final excel worksheet for Baseline EDDs
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2024")
saveWorkbook(wb= wwqmp.lab, "HydroShareIntermediateData2024/WWQMP_Lab_EDD_24-04-01_cr.xlsx",
             overwrite = TRUE)

