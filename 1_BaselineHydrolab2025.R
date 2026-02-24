rm(list=ls())
library(readxl)
library(plyr)
library(stringr)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(anytime)
#set wd
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/HYDROLAB/2025_Hydrolab")

#Create a list of all files being run through the script.

hlfiles <- list.files("./SITE VISIT", recursive=TRUE, full.names=TRUE)


print(hlfiles)


#Loop for formatting hydrolabs into WQX uploadable format ####
#this is updated code that uses pivot longer instead of a for loop to 
#transpose the data
#script is marked where change was made

#re-empty data frames
data <- NULL
failed <- NULL

for (i in 1:length(hlfiles)) {
  
  #read in csv
  file.i <- read.csv(hlfiles[i], header= FALSE, na.strings=c("","NA"))
  
  
  #remove empty rows and columns
  file.i <- file.i[rowSums(is.na(file.i)) != ncol(file.i) ,]
  file.i <- file.i[, colSums(is.na(file.i)) !=nrow(file.i) & colSums(is.na(file.i)) 
                   != nrow(file.i)-1 ]
  
  #check that file name is included
  if (str_detect(file.i[1,1],"Log File Name")==FALSE){
    failed$file[i]<-hlfiles[i];
    failed$error[i]<-"logfile"; 
    next
  }
  
  #store site ID
  logger <- file.i[1,1]
  
  #make it print 
  print(logger)
  
  #Rename columns, remove setup date and setup time
  set <- which((str_detect(file.i[,1],"^Date$")))
  
  file.i <- setNames(file.i, file.i[set,])
  
  file.i <- file.i[-c(1:set),]
  
  
  #Remove recovery information
  recovery <- str_detect(file.i[,1],"Recovery")
  delete <- which(recovery)
  if(length(delete)==1){file.i <- file.i[-delete ,]} 
  
  #Remove power loss information
  power <- which(str_detect(file.i[, 1], "Power"))
  if(length(power)>=1){file.i <- file.i[-power ,]} 
  
  
  #remove time change information #### NEW CODE 2024
  timechange<- (str_detect(file.i[,1],"Time changed"))
  delete2<- which(timechange)
  if(length(delete2)>=1){file.i <- file.i[-delete2 ,]}
  
  
  #remove battery charge
  battery <- which(str_detect(colnames(file.i),"IB"))
  if(length(battery >=1)){
    file.i <- file.i[, -battery]
  }
  
  
  
  #Remove BPSvr4
  bps <- which(str_detect(colnames(file.i),"BPSvr4"))
  if(length(bps >=1)){
    file.i <- file.i[, -bps]
  }
  
  #Remove blanks column names (associated with columns of Xs)
  file.i <- file.i[ , is.na(colnames(file.i)) == FALSE]
  
  
  
  #remove top row which contains units
  file.i <- file.i[-1,] 
  
  
  
  #using pivot longer instead of old transpose code ###########
  #some of the 2024 files have two dates in them and this works for that format
  #this new code replaces a bunch of for loop transpose code 
  #to see old code look at this script from 2023
  
  #transpose file.i with pivot longer
  file2<-file.i %>% pivot_longer(cols = c("Temp", "SpCond", "Res", "Sal", "TDS", 
                                          "pH", "ORP", "CHL", "PAR", "refPAR", "LDO%",
                                          "LDO", "BP"), names_to = "PARAMETER",
                                 values_to = "RESULT")
  
  #ADD SITE
  file2$SITE_ID <- logger
  #Remove Log File Name:
  file2$SITE_ID <- sub("Log File Name : ", "", file2$SITE_ID)
  
  
  #add depth units
  file2$DEPTH_UNITS<- "m"
  
  # #add units ########
  file2<- file2 %>% mutate(UNITS = case_when(PARAMETER == "Temp" ~ "deg",
                                             PARAMETER == "SpCond" ~ "uS/cm",
                                             PARAMETER == "Res" ~ "k�-cm",
                                             PARAMETER == "Sal" ~ "ppt",
                                             PARAMETER == "TDS" ~ "g/l",
                                             PARAMETER == "pH" ~ "Units",
                                             PARAMETER == "ORP" ~ "mV",
                                             PARAMETER == "CHL" ~ "ug/l",
                                             PARAMETER == "PAR" ~ "umol/s/m2",
                                             PARAMETER == "refPAR" ~ "umol/s/m2",
                                             PARAMETER == "LDO%" ~ "Sat",
                                             PARAMETER == "LDO" ~ "mg/l",
                                             PARAMETER == "BP" ~ "mmHg",
                                             .default = "uhoh"))

  
  #rename cols 
  file2<- file2 %>% rename(DATE = Date, TIME = Time, DEPTH = Dep200)
  
  
  
  
  #Compile
  data <- rbind.fill(data,file2)
  
  data <- data[, c("SITE_ID", "DATE", "TIME", "PARAMETER", 
                   "RESULT", "UNITS", "DEPTH", "DEPTH_UNITS")]
  
}

#there are 50 warnings lets see them I think these have to do with the csv being a UTC file ####
#the data seems fine so I think it's okay
warnings()
























#for some reason swift creek had the temp data in c so I'm gonna run that seperately ####
#Create a list of all files being run through the script.


other <- list.files("./CelciusFiles", recursive=TRUE, full.names=TRUE)

print(other)


#Loop for formatting hydrolabs into WQX uploadable format ####
#this is updated code that uses pivot longer instead of a for loop to 
#transpose the data
#script is marked where change was made

#re-empty data frames
dataother <- NULL
failedother <- NULL

for (i in 1:length(other)) {
  
  #read in csv
  file.i <- read.csv(other[i], header= FALSE, na.strings=c("","NA"))
  
  
  #remove empty rows and columns
  file.i <- file.i[rowSums(is.na(file.i)) != ncol(file.i) ,]
  file.i <- file.i[, colSums(is.na(file.i)) !=nrow(file.i) & colSums(is.na(file.i)) 
                   != nrow(file.i)-1 ]
  
  #check that file name is included
  if (str_detect(file.i[1,1],"Log File Name")==FALSE){
    failed$file[i]<-other[i];
    failed$error[i]<-"logfile"; 
    next
  }
  
  #store site ID
  logger <- file.i[1,1]
  
  #make it print 
  print(logger)
  
  #Rename columns, remove setup date and setup time
  set <- which((str_detect(file.i[,1],"^Date$")))
  
  file.i <- setNames(file.i, file.i[set,])
  
  file.i <- file.i[-c(1:set),]
  
  
  #Remove recovery information
  recovery <- str_detect(file.i[,1],"Recovery")
  delete <- which(recovery)
  if(length(delete)==1){file.i <- file.i[-delete ,]} 
  
  #Remove power loss information
  power <- which(str_detect(file.i[, 1], "Power"))
  if(length(power)>=1){file.i <- file.i[-power ,]} 
  
  
  #remove time change information #### NEW CODE 2024
  timechange<- (str_detect(file.i[,1],"Time changed"))
  delete2<- which(timechange)
  if(length(delete2)>=1){file.i <- file.i[-delete2 ,]}
  
  
  #remove battery charge
  battery <- which(str_detect(colnames(file.i),"IB"))
  if(length(battery >=1)){
    file.i <- file.i[, -battery]
  }
  
  
  
  #Remove BPSvr4
  bps <- which(str_detect(colnames(file.i),"BPSvr4"))
  if(length(bps >=1)){
    file.i <- file.i[, -bps]
  }
  
  #Remove blanks column names (associated with columns of Xs)
  file.i <- file.i[ , is.na(colnames(file.i)) == FALSE]
  
  
  
  #remove top row which contains units
  file.i <- file.i[-1,] 
  
  
  
  #using pivot longer instead of old transpose code ###########
  #some of the 2024 files have two dates in them and this works for that format
  #this new code replaces a bunch of for loop transpose code 
  #to see old code look at this script from 2023
  
  #transpose file.i with pivot longer
  file2<-file.i %>% pivot_longer(cols = c("Temp", "SpCond", "Res", "Sal", "TDS", 
                                          "pH", "ORP", "CHL", "PAR", "refPAR", "LDO%",
                                          "LDO", "BP"), names_to = "PARAMETER",
                                 values_to = "RESULT")
  
  #ADD SITE
  file2$SITE_ID <- logger
  #Remove Log File Name:
  file2$SITE_ID <- sub("Log File Name : ", "", file2$SITE_ID)
  
  
  #add depth units
  file2$DEPTH_UNITS<- "m"
  
  # #add units ########
  file2<- file2 %>% mutate(UNITS = case_when(PARAMETER == "Temp" ~ "degC",
                                             PARAMETER == "SpCond" ~ "uS/cm",
                                             PARAMETER == "Res" ~ "k�-cm",
                                             PARAMETER == "Sal" ~ "ppt",
                                             PARAMETER == "TDS" ~ "g/l",
                                             PARAMETER == "pH" ~ "Units",
                                             PARAMETER == "ORP" ~ "mV",
                                             PARAMETER == "CHL" ~ "ug/l",
                                             PARAMETER == "PAR" ~ "umol/s/m2",
                                             PARAMETER == "refPAR" ~ "umol/s/m2",
                                             PARAMETER == "LDO%" ~ "Sat",
                                             PARAMETER == "LDO" ~ "mg/l",
                                             PARAMETER == "BP" ~ "mmHg",
                                             .default = "uhoh"))
  
  
  #rename cols 
  file2<- file2 %>% rename(DATE = Date, TIME = Time, DEPTH = Dep200)
  
  
  
  
  #Compile
  dataother <- rbind.fill(dataother,file2)
  
  dataother <- dataother[, c("SITE_ID", "DATE", "TIME", "PARAMETER", 
                   "RESULT", "UNITS", "DEPTH", "DEPTH_UNITS")]
  
}

#there are 50 warnings lets see them I think these have to do with the csv being a UTC file ####
#the data seems fine so I think it's okay
warnings()


#now convert temp degC to degF
glimpse(dataother)

dataother$RESULT<- as.numeric(dataother$RESULT)

dataother1<- dataother %>% mutate(Result2 = case_when(PARAMETER == "Temp" ~ (RESULT*9/5)+32,
                                            .default = RESULT))

dataother2<- dataother1 %>% select(-RESULT) %>% rename(RESULT = Result2)


#now bind into larger dataframe
data1<- rbind(data, dataother2)

#make date a date 
data1$DATE<- mdy(data1$DATE)

#data1$TIME<- hms(data1$TIME)



#now fix time and date in august where it starts to be off ######
#divide data
baddate<- data1 %>% filter(DATE > "2025-07-01")

glimpse(baddate)

#re-format hours col
baddate$TIME<-as.POSIXct(baddate$TIME,
                         
                         format= "%H:%M:%S")



#change the date and time
#add a day to fix the date --- THIS WAS CHECKED USING FIELD NOTES
baddate1<- baddate %>% mutate(newdate = (DATE + days(1)))
  


#try to add minutes to this 
baddate2<- baddate1 %>% mutate(newtime = (TIME + minutes(40))) 
  
#remove dat from time, remove uneeded cols and rename corrected cols to match waht is needed
baddate3<- baddate2 %>% 
  separate(newtime,sep =' ',into=c('date1','timecorrected')) %>% glimpse() %>% 
  select(-DATE, -TIME, -date1) %>% rename(TIME = timecorrected, DATE = newdate)


  
#now rbind in with the data that did not need time date corrections
gooddate<-data1 %>% filter(DATE <= "2025-07-01") #check that gooddate obs + baddate obs = data1

#rbind back together
dataall<-rbind(gooddate, baddate3)

glimpse(dataall)



#fix some of the site ids this may not need to be done every time but should always be checked####
unique(dataall$SITE_ID)
#swift-oln needed to be corrected
dataall$SITE_ID<-sub("SWIFT-OLN", "SWIFT-CRK-OLN", dataall$SITE_ID)

#check that it worked
unique(dataall$SITE_ID)


#Failed will probably be empty, but if not run this code
failed <- data.frame(failed)

#Add in turbidity from spreadsheet #### 
#GOING TO ADD IN ALL YEARS EXCEPT 2022 AND 2023 SO THAT TURB DEPTHS CAN BE 
#FIXED IN FINAL DATAFRAME
turb <- read_csv("C:/Users/User/Dropbox/database/WF WQ DATA/TURBIDITY/WLITurb07-25.csv")
glimpse(turb)
glimpse(data)
#Format to match the rest of the data
colnames(turb)[3] <- "RESULT"

turb$PARAMETER <- "Turbidity"
turb$DATE<- mdy(turb$DATE)
#turb$TIME<- hm(turb$TIME)
#turb$DATE <- format(turb$DATE, "%Y-%m-%d")
#turb$TIME <- format(turb$TIME, "%H:%M:%I")
turb$UNITS <- "NTU"
turb$DEPTH_UNITS <- "m"

#remove counting col
turb<-turb[,-1]

#add time col
turb$TIME<- ""

#add only the 2025 data
turb25<- turb %>% filter(DATE > "2025-01-01")


dataallturb <- rbind.fill(dataall, turb25)


#add secchi from spreadsheet #####
secchi <- read_csv("C:/Users/User/Dropbox/database/WF WQ DATA/SECCHI/Whitefish_Secchi_Data_ALL_Clean.csv")

#Format to match the rest of the data
secchi$DEPTH<- secchi$RESULT
secchi$DATE<- mdy(secchi$DATE)
glimpse(secchi)
#cut down to only 2025
secchi<- filter(secchi, DATE > "2025-01-01")


#combine
data.1 <- rbind.fill(dataallturb, secchi)

#check that data doesn't have any weird site_ids
unique(dataallts$SITE_ID)


#YOU COULD AT SOME POINT PUT STREAM TURB IN HERE ############

#separate into streams vs. lakes
#remove all streams as these will be processed seperatly #####
tribs <- data.1 %>%  filter(str_detect(data.1$SITE_ID, "SWIFT-CRK-OLN")==TRUE
                            | str_detect(data.1$SITE_ID, "SWIFT-CRK-DEL")==TRUE
                            | str_detect(data.1$SITE_ID, "SMITH-CRK-ELD")==TRUE
                            | str_detect(data.1$SITE_ID, "LAZY-CRK-S")==TRUE
                            | str_detect(data.1$SITE_ID, "WF-R-SPB")==TRUE
                            | str_detect(data.1$SITE_ID, "WALKER-CRK-MR")==TRUE
                            | str_detect(data.1$SITE_ID, "VIKING-CRK-WA")==TRUE
                            | str_detect(data.1$SITE_ID, "LOGAN")==TRUE
                            | str_detect(data.1$SITE_ID, "BEAV-CRK-RR")==TRUE
                            | str_detect(data.1$SITE_ID, "WF-R-2S")==TRUE
                            | str_detect(data.1$SITE_ID, "WF-R-H40")==TRUE
                            | str_detect(data.1$SITE_ID, "WF-R-JPR")==TRUE
                            | str_detect(data.1$SITE_ID, "COW-CRK-PA")==TRUE
                            | str_detect(data.1$SITE_ID, "WF-R-JPR")==TRUE
                            | str_detect(data.1$SITE_ID, "COW-CRK-RR")==TRUE
                            | str_detect(data.1$SITE_ID, "WF-R-SPB") == TRUE
                            | str_detect(data.1$SITE_ID, "HELLR-CRK-T") == TRUE
                            | str_detect(data.1$SITE_ID, "HASK-CRK-MR") == TRUE
                            | str_detect(data.1$SITE_ID, "WALKER-CRK-WA") == TRUE)

lakes<-  data.1 %>%  filter(str_detect(data.1$SITE_ID, "WF-LK-IP")==TRUE
                            | str_detect(data.1$SITE_ID, "TALLY")==TRUE
                            | str_detect(data.1$SITE_ID, "WF.LK.IP")==TRUE)


#ADD UP LAKES AND TRIBS AND SEE IF THEY HAVE THE SAME NUMBER OF OBVS AS DATA.1 !!!!!!!! #####


#check the site ids for lakes and tribs
unique(lakes$SITE_ID)
unique(tribs$SITE_ID)
#save final 2025 baseline csv (make sure format matches info doc) ####
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")

write.csv(lakes, "HydrolabCompilationLakesSecchiTurb2025.csv")

write.csv(tribs, "HydrolabCompilationTribsTurb2025.csv")



