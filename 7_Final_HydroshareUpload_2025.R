rm(list=ls())
library(tidyverse)
library(readxl)
library(openxlsx)
library(plyr)
library(stringr)

#library(dplyr)
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025")
#Load Data ####

#List all files uploaded to WQX
#should have four files two lab and two field
files <- list.files("PreFinalData2025", full.names = TRUE)

#THIS MESSES UP THE CODE SO IGNORE FOR NOW
#Remove raw data files, will be handled separately
# files <- files[!which(str_detect(files, "Raw")
#                       | str_detect(files, "HydroShare"))]

#Create empty dataframes for each sheet to be filled
project <- NULL
station <- NULL

activity <- NULL

result <- NULL

#Read in and merge Project information for each file
for(i in 1:length(files)){
  
  #Read in excel file
  file <- data.frame(read_excel(path= files[i],
                                sheet= "Project"))
  
  #Remove text for data type and character limit
  file <- file[-which(str_detect(file$Project_Name, "Text")) ,]
  
  #Bind all project information
  project <- rbind.fill(file, project)
  
}



#Read in and merge Station information for each file
for(i in 1:length(files)){
  
  #Read in excel file
  file <- data.frame(read_excel(path= files[i],
                                sheet= "Stations"))
  
  #Remove text for data type and character limit
  file <- file[-which(str_detect(file$Station_Name, "Text")) ,]
  
  #Bind all project information
  station <- rbind.fill(file, station)
  
}


#Read in and merge Activity information for each file
for(i in 1:length(files)){
  
  #Read in excel file
  file <- data.frame(read_excel(path= files[i],
                                sheet= "Activity"))
  
  #Remove text for data type and character limit
  ifelse(str_detect(file$Activity_ID, "Text") == TRUE,
         yes= file <- file[-which(str_detect(file$Activity_ID, "Text")) ,],
         no= file <- file)
  
  
  #Format activity start dates and times if necessary
  for(j in 1:length(file$Activity_Start_Date)){
    
    if(str_detect(file$Activity_Start_Date[j], "-")== FALSE){
      file$Activity_Start_Date[j] <- format(as.POSIXct((as.numeric(file$Activity_Start_Date[j])*60*60*24),
                                                       origin= "1899-12-31"), format= "%Y-%m-%d")
      file$Activity_Start_Time[j] <- format(as.POSIXct(Sys.Date()+(as.numeric(file$Activity_Start_Time[j])),
      ),format= "%H:%M:%S", tz= "UTC")
    }
    
  }
  
  #Bind all project information
  activity <- rbind.fill(file, activity)
  
}


#Read in and merge Result information for each file
for(i in 1:length(files)){
  
  #Read in excel file
  file <- data.frame(read_excel(path= files[i],
                                sheet= "Result"))
  
  #Remove text for data type and character limit
  ifelse(str_detect(file$Activity_ID, "Text") == TRUE,
         yes= file <- file[-which(str_detect(file$Activity_ID, "Text")) ,],
         no= file <- file)
  
  
  #Format analysis start dates and times if necessary
  for(j in 1:length(file$Analysis_Start_Date)){
    
    if(str_detect(file$Analysis_Start_Date[j], "-")== FALSE
       & is.na(file$Analysis_Start_Date[j]) == FALSE){
      file$Analysis_Start_Date[j] <- format(as.POSIXct(file$Analysis_Start_Date[j],format= "%Y/%m/%d"), 
                                            format= "%Y-%m-%d")
      
    }
    
    if(str_detect(file$Analysis_Start_Time[j], "1899")
       & is.na(file$Analysis_Start_Time[j]) == FALSE){
      file$Analysis_Start_Time <- substr(file$Analysis_Start_Time, 12, 19)
    }
    
  }
  
  
  #Bind all project information
  result <- rbind.fill(file, result)
  
}

#edit site info #####

#Standardize Logan Creek station ID and lower still AND LOST LOON
activity$Station_ID <- sub("LOGAN-CRK-TC", "LOGAN", activity$Station_ID)
activity$Station_ID<- sub("LOST-COO", "LOSTLOON", activity$Station_ID)
activity$Activity_ID<- sub("LOST-COO", "LOSTLOON", activity$Activity_ID)

#fix in result sheet as well
result$Activity_ID<- sub("LOST-COO", "LOSTLOON", result$Activity_ID)
result$Site_ID<- sub("LOST-COO", "LOSTLOON", result$Site_ID)
result$my.activity<- sub("LOST-COO", "LOSTLOON", result$my.activity)

result<- result %>% mutate(Station_ID = case_when(Site_ID == "LOSTLOON" ~ "LOSTLOON",
                                                  .default = result$Station_ID))

#fix in station as well
station$X.Station_ID<- sub("LOSTCOO", "LOSTLOON", station$X.Station_ID)
station$Station_Name<- sub("Lost Coon", "Lost Loon", station$Station_Name)

#Keep only unique project and station information
project <- unique(project[c(colnames(project))])
station <- unique(station[c(colnames(station))])


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


#Sites Files for HydroShare Upload ####



#START HERE #################################################################################

#Remove Swan Lake Six and State, no data uploaded for these sites
#station <- station [-c(74,75) ,]

#Create site data upload file
site <- station

#Remove columns that are empty
site <- site[, nrow(site) != colSums(is.na(site))]

#Rename first column
colnames(site)[1] <- "Station_ID"

#Create dataframe to connect project and site IDs
project2 <- activity[, c("Project_ID", "Station_ID")]
project2 <- unique(project2[c(colnames(project2))])

#Merge project information with station IDs
colnames(project)[1] <- "Project_ID"
colnames(project2)[1] <- "Project_ID"
project2 <- merge(project, project2, by= "Project_ID")


#Remove unneeded columns
project2 <- project2[, -c(4,5)]


#Merge project information with site information
site <- merge(site, project2, by= "Station_ID")

#Remove NAs
site <- clear.na(site)


#Write csv
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare")
write.csv(site, "HydroShare2025/Sites25.csv", row.names=FALSE)

#Results with QA File for HydroShare Upload ####
#Remove all columns that are completely empty
data <- result[, nrow(result) != colSums(is.na(result))]




data<-result[,c(1,7:19,21:26,29:30,35,36)]
#data<-data %>% mutate(Sample_Collection_Method_ID = "", Sample_Collection_Equipment_Name = "")


# lakes2011 <- lakes2011[, nrow(lakes2011) != colSums(is.na(lakes2011))]

#Remove NAs, keep depth columns but replace numbers with NA
#because depth was never calibrated for streams
# cow <- cow[, nrow(cow) != colSums(is.na(cow))]
# cow$Result_Depth_Height_Measure <- NA

#Remove other columns with data we don't plan to upload
#Analysis start time, time zone
data <- data[, -c(17:18)]

# cow <- cow[, -c(17:18)]
# lakes2011 <- lakes2011[, -c(24:25)]
glimpse(data)
#Remove empty columns from Activity file
act <- activity[, nrow(activity) != colSums(is.na(activity))]




#Remove filled columns not being uploaded
#Medium, medium subdivision, organization collecting, personnel
act <- act[, -c(5,6, 12:13)]

#Merge activity information and results information 
colnames(act)[1] <- "Project_ID"
colnames(data)[1] <- "Activity_ID"
data <- merge(data, act, by= ("Activity_ID"))

#Additional station data to include
site2 <- site[, c("Station_ID", "Station_Name", "Station_Type", "HUC_8_digit")]

#make sure all stations match before r binding here ####
unique(data$Station_ID)
unique(site2$Station_ID)
#run just to fix maryron and ashley in 2023




# #hopefully don't need to run this
data<-data %>% mutate(Station_ID2 = case_when(Station_ID == "ASHLEY_E" ~ "ASHLEY-E",
                                              Station_ID == "ASHLEY_W" ~ "ASHLEY-W",
                                              Station_ID == "BEAV.CRK.RR" ~ "BEAV-CRK-RR",
                                              Station_ID == "BEV.CRK.RR.7.5.22" ~ "BEAV-CRK-RR",
                                              Station_ID == "BIG_THERR" ~ "BIG-THERR",
                                              Station_ID == "FH.SOMERS" ~ "FH-SOMERS",
                                              Station_ID == "COW_CRK_PA" ~ "COW-CRK-PA",
                                              Station_ID == "FH.CONRAD" ~ "FH-CONRAD",
                                              Station_ID == "FH.INDIAN" ~ "FH-INDIAN",
                                              Station_ID == "FH.MACK" ~ "FH-MACK",
                                              Station_ID == "FH.SKIDOO" ~ "FH-SKIDOO",
                                              Station_ID == "FH_SKIDOO" ~ "FH-SKIDOO",
                                              Station_ID == "FH.WOODS" ~ "FH-WOODS",
                                              Station_ID == "FH.YELLOW" ~ "FH-YELLOW",
                                              Station_ID == "FH_YELLOW" ~ "FH-YELLOW",
                                              Station_ID == "LITT.BITT" ~ "LITT-BITT",
                                              Station_ID == "LITT_BITT" ~ "LITT-BITT",
                                              Station_ID == "LOWER.STILL" ~ "LOWER-STILL",
                                              Station_ID == "LOWER_STILL" ~ "LOWER-STILL",
                                              Station_ID == "MARYRON.E" ~ "MARYRON-E",
                                              Station_ID == "MARYRON.W" ~ "MARYRON-W",
                                              Station_ID == "MARYRON_W" ~ "MARYRON-W",
                                              Station_ID == "MARYRON_E" ~ "MARYRON-E",
                                              Station_ID == "HANS.DOY" ~ "HANS-DOY",
                                              Station_ID == "HANS_DOY" ~ "HANS-DOY",
                                              Station_ID == "LAZY.CRK" ~ "LAZY-CRK-S",
                                              Station_ID == "SWAN.N" ~ "SWAN-N",
                                              Station_ID == "SWAN.S" ~ "SWAN-S",
                                              Station_ID == "UPP.STILL" ~ "UPP-STILL",
                                              Station_ID == "UPP.WF" ~"UPP-WF",
                                              Station_ID == "BEAV.CRK.RR.4.20.22" ~"BEAV-CRK-RR",
                                              Station_ID == "BEAV.CRK.RR.5.10.22" ~"BEAV-CRK-RR",
                                              Station_ID == "BEAV.CRK.RR.5.26.22" ~"BEAV-CRK-RR",
                                              Station_ID == "BEAV.CRK.RR.6.14.22" ~"BEAV-CRK-RR",
                                              Station_ID == "BEAV.CRK.RR.7.5.22" ~"BEAV-CRK-RR",
                                              Station_ID == "BEAV.CRK.RR" ~"BEAV-CRK-RR",
                                              Station_ID == "WALKER-CRK-WA" ~ "WALKER-CRK-MR",
                                              Station_ID == "TALLY01.5.5.22" ~ "TALLY",
                                              Station_ID == "TALLY01.6.7.22" ~ "TALLY",
                                              Station_ID == "TALLY01.7.11.22" ~ "TALLY",
                                              Station_ID == "TALLY01" ~ "TALLY",
                                              Station_ID == "WF.LK.IP1.4.19.22" ~ "WF-LK-IP1",
                                              Station_ID == "WF.LK.IP1.5.10.22" ~ "WF-LK-IP1",
                                              Station_ID == "WF.LK.IP1.5.25.22" ~ "WF-LK-IP1",
                                              Station_ID == "WF.LK.IP1.6.14.22" ~ "WF-LK-IP1",
                                              Station_ID == "WF.LK.IP1.7.5.22" ~ "WF-LK-IP1",
                                              Station_ID == "WF.LK.IP2.4.19.22" ~ "WF-LK-IP2",
                                              Station_ID == "WF.LK.IP1" ~ "WF-LK-IP1",
                                              Station_ID == "WF.LK.IP2.4.19.22" ~ "WF-LK-IP2",
                                              Station_ID == "WF.LK.IP2.5.10.22" ~ "WF-LK-IP2",
                                              Station_ID == "WF.LK.IP2.5.25.22" ~ "WF-LK-IP2",
                                              Station_ID == "WF.LK.IP2.6.14.22" ~ "WF-LK-IP2",
                                              Station_ID == "WF.LK.IP2.7.5.22" ~ "WF-LK-IP2",
                                              Station_ID == "WF.LK.IP2" ~ "WF-LK-IP2",
                                              Station_ID == "LOW-STILL" ~ "LOWER-STILL",
                                              Station_ID == "VIKING CREEK" ~ "VIKING-CRK-WA",
                                              .default = Station_ID))


#check
unique(data$Station_ID2)
unique(site2$Station_ID)

data<- data %>% mutate(Station_ID = Station_ID2)

#Merge site data with results dataframe
data <- merge(data, site2, by= "Station_ID")



#Reorder columns
data <- data[, c("Station_ID", "Characteristic_Name", "Characteristic_ID",
                 "Activity_Start_Date", "Activity_Start_Time", "Activity_Start_Time_Zone",
                 "Result_Value", "Result_Value_Unit", "Result_Detection_Condition",
                 "Result_Qualifier", "Method_Detection_Limit_Value", "Lower_Reporting_Limit_Value",
                 "Detection_Limit_Unit", "Result_Depth_Height_Measure", "Result_Depth_Height_Measure_Unit",
                 "Sample_Collection_Method_ID", "Sample_Collection_Equipment_Name", "Analytical_Method_ID", "Sample_Fraction",
                 "Method_Speciation_Name", "Laboratory_Name", "Analysis_Start_Date",
                 "Laboratory_Batch_ID", "Laboratory_Sample_ID", "Dilution_Factor",
                 "Station_Name", "Station_Type", "HUC_8_digit",
                 "Project_ID", "Activity_Type",
                 "Value_Type", "Activity_ID", "Result_Comment")]



#Subset By Routine or Quality Assurance Sample  
#there was nothing but routine so this should be empty####
qa <- data[data$Activity_Type == "QC-FB" | data$Activity_Type == "QC-FD" ,]
# 
# data <- data[data$Activity_Type == "F-MSR/OBS" | data$Activity_Type == "S-ROUTINE" ,]

#Remove NAs
#qa <- clear.na(qa)
data <- clear.na(data)

#first some quick QA/QC on the NMLN data ##### SPECIFIC TO 2024
#remove blanchard bad depth points
data<- data %>% filter(!(Station_ID == "BLANCH" & Result_Depth_Height_Measure == 0))
#remove peterson bad depth points
data<-data %>% filter(!(Station_ID == "PETERS" & Result_Value == 0))




#Write csvs
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/FinalResults2025")
write.csv(data, "HydroData2025.csv",
          row.names = FALSE)
#write.csv(qa, "DuplicateAndBlankData2023.csv",
# row.names = FALSE)

#Raw Data ####

#Load files
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
wwqmp <- read.csv("WWQMP_RawDataFile_25_cr.csv")

nmln <- read.csv("NMLN_RawFileOnlyData_25_cr.csv")


# #Add project information
wwqmp$Project_ID <- "WWQMP"
nmln$Project_ID <- "NMLN"

#Merge raw data for projects
raw <- rbind.fill(wwqmp, nmln)

#Extract site from activity ID 
for(i in 1:length(raw$Data_Logger_Line_ID)){
  end <- unlist(gregexpr("_\\d+-\\d+", raw$Activity_ID[i]))
  raw$Station_ID[i] <- substr(raw$Activity_ID[i], 1, end-1)
}

#Extract time from activity ID
for(i in 1:length(raw$Activity_ID)){
  start <- unlist(gregexpr("_\\d+:\\d+", raw$Activity_ID[i]))
  raw$Activity_Start_Time[i] <- substr(raw$Activity_ID[i],
                                       start+1, nchar(raw$Activity_ID)-2)
}

#Remove empty columns
raw <- raw[, nrow(raw) != colSums(is.na(raw))]

#Additional information for raw file from activity sheet
raw.all <- merge(raw, act, by= "Activity_ID")

#Remove analysis start date (doesn't provide unique info for field data)
raw.all <- raw.all[, -7]

#not needed for 2022 ####
# #Remove empty rows from cow and lakes raw files 
# lakes.raw <- lakes.raw[, nrow(lakes.raw) != colSums(is.na(lakes.raw))]
# cow.raw <- cow.raw[, nrow(cow.raw) != colSums(is.na(cow.raw))]
# 
# #add time zones
# for(i in 1:length(cow.raw$Activity_Start_Date)){
#   ifelse(cow.raw$Activity_Start_Date[i] > as.POSIXct("2014-03-09") & cow.raw$Activity_Start_Date[i] < as.POSIXct("2014-11-02")
#          |cow.raw$Activity_Start_Date[i] > as.POSIXct("2015-03-08") & cow.raw$Activity_Start_Date[i] < as.POSIXct("2015-11-01")
#          |cow.raw$Activity_Start_Date[i] > as.POSIXct("2016-03-13") & cow.raw$Activity_Start_Date[i] < as.POSIXct("2016-11-06")
#          |cow.raw$Activity_Start_Date[i] > as.POSIXct("2017-03-12") & cow.raw$Activity_Start_Date[i] < as.POSIXct("2017-11-05")
#          |cow.raw$Activity_Start_Date[i] > as.POSIXct("2018-03-11") & cow.raw$Activity_Start_Date[i] < as.POSIXct("2018-11-04")
#          |cow.raw$Activity_Start_Date[i] > as.POSIXct("2019-03-10") & cow.raw$Activity_Start_Date[i] < as.POSIXct("2019-11-03")
#          |cow.raw$Activity_Start_Date[i] > as.POSIXct("2020-03-08") & cow.raw$Activity_Start_Date[i] < as.POSIXct("2020-11-01"),
#          yes= cow.raw$Activity_Start_Time_Zone[i] <- "MDT",
#          no= cow.raw$Activity_Start_Time_Zone[i] <- "MST")
#   
# }
# 
# for(i in 1:length(lakes.raw$Activity_Start_Date)){
#   ifelse(lakes.raw$Activity_Start_Date[i] > as.POSIXct("2011-03-13") 
#          & lakes.raw$Activity_Start_Date[i] < as.POSIXct("2011-11-06"),
#          yes= lakes.raw$Activity_Start_Time_Zone[i] <- "MDT",
#          no= lakes.raw$Activity_Start_Time_Zone[i] <- "MST")
# }  
# 
# #Create empty columns for sample collection information in cow data
# cow.raw$Sample_Collection_Method_ID <- ""
# cow.raw$Sample_Collection_Equipment_Name <- ""
# 
# #Add sample collection information for lab samples
# for(i in 1:length(cow.raw$Activity_ID)){
#   if(substr(cow.raw$Activity_ID[i], 
#             nchar(cow.raw$Activity_ID[i]), nchar(cow.raw$Activity_ID[i])) == "R"){
#     cow.raw$Sample_Collection_Method_ID[i] <- "EWI"
#     cow.raw$Sample_Collection_Equipment_Name[i] <- "Water Bottle"
#   }
# }
# 
# #Create empty columns for sample collection information in lakes data
# lakes.raw$Sample_Collection_Method_ID <- ""
# lakes.raw$Sample_Collection_Equipment_Name <- ""
# 
# #Add sample collection information for lab samples
# for(i in 1:length(lakes.raw$Activity_ID)){
#   if(substr(lakes.raw$Activity_ID[i], 
#             nchar(lakes.raw$Activity_ID[i]), nchar(lakes.raw$Activity_ID[i])) != "F"){
#     lakes.raw$Sample_Collection_Method_ID[i] <- "IWS"
#     lakes.raw$Sample_Collection_Equipment_Name[i] <- "Van Dorn Bottle"
#   }
# }

#Merge cow and 2011 lakes raw data with all raw data
#raw.all <- rbind.fill(raw.all, lakes.raw, cow.raw)

#Additional information on sites
detach(package:plyr)
raw.all<- raw.all %>% rename(Station_ID  = Station_ID.x,
                             Activity_Start_Time = Activity_Start_Time.y,
                             Project_ID = Project_ID.x)
raw.all <- merge(raw.all, site2, by= "Station_ID")

#Remove empty columns
raw.all <- raw.all[, nrow(raw.all) != colSums(is.na(raw.all))]

#Reorder
raw.all <- raw.all[, c("Station_ID", "Characteristic_Name", "Characteristic_ID",
                       "Activity_Start_Date", "Activity_Start_Time", "Activity_Start_Time_Zone",
                       "Result_Value", "Result_Value_Unit", 
                       "Result_Depth_Height_Measure", "Result_Depth_Height_Measure_Unit",
                       "Station_Name", "Station_Type", "HUC_8_digit",
                       "Project_ID", "Activity_Type",
                       "Value_Type", "Activity_ID")]



#Remove NAs
raw <- clear.na(raw)

#Write csv
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/FinalResults2025")
write.csv(raw, "DataNeedingQa25.csv",
          row.names= FALSE)

