rm(list=ls())
library(readxl)
library(plyr)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
#set wd
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/HYDROLAB/2025_Hydrolab")

#Create a list of all files being run through the script.

hlfiles <- list.files("./Lakes_Hydrolab_2025", recursive=TRUE, full.names=TRUE)

data <- NULL
failed <- NULL

#Loop for formatting hydrolabs into WQX uploadable format
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
  
  #make it print ####
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
  if(length(power)==1){file.i <- file.i[-power ,]} 
  
  
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
  
  
  #store parameter names
  parameters <- colnames(file.i[3:length(colnames(file.i))])
  
  #store units
  units <- file.i[1, c(3:length(colnames(file.i)))]
  units <- setNames(units, units[1,])
  units <- colnames(units)
  
  #store depths
  dep <- str_detect(colnames(file.i),"Dep")
  depthcol <- which(dep)
  depth <- file.i[c(2:length(file.i[,depthcol])),depthcol]
  
  file.i <- file.i[-1,]
  
  #i think this is sometimes remaining empty and throwing the date off???? #####
  #store date
  date <- file.i[2,1]
  
  
  
  #this is not working ####
  #loop to transpose data
  for (i in 1:length(file.i$Temp)){
    
    file.i[i,3] <- paste0(file.i[i,3],"_",file.i[i,depthcol],"/",file.i[i,2])
    file.i[i,4] <- paste0(file.i[i,4],"_",file.i[i,depthcol],"/",file.i[i,2])
    file.i[i,5] <- paste0(file.i[i,5],"_",file.i[i,depthcol],"/",file.i[i,2])
    file.i[i,6] <- paste0(file.i[i,6],"_",file.i[i,depthcol],"/",file.i[i,2])
    file.i[i,7] <- paste0(file.i[i,7],"_",file.i[i,depthcol],"/",file.i[i,2])
    if(length(colnames(file.i))>7) {
      if(str_detect(colnames(file.i[8]), "Dep") == FALSE) {
        file.i[i,8] <- paste0(file.i[i,8],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>8) {
      if(str_detect(colnames(file.i[9]), "Dep") == FALSE) {
        file.i[i,9] <- paste0(file.i[i,9],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>9) {
      if(str_detect(colnames(file.i[10]), "Dep") == FALSE) {
        file.i[i,10] <- paste0(file.i[i,10],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>10) {
      if(str_detect(colnames(file.i[11]), "Dep") == FALSE) {
        file.i[i,11] <- paste0(file.i[i,11],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>11) {
      if(str_detect(colnames(file.i[12]), "Dep") == FALSE) {
        file.i[i,12] <- paste0(file.i[i,12],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>12) {
      if(str_detect(colnames(file.i[13]), "Dep") == FALSE) {
        file.i[i,13] <- paste0(file.i[i,13],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>13) {
      if(str_detect(colnames(file.i[14]), "Dep") == FALSE) {
        file.i[i,14] <- paste0(file.i[i,14],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>14) {
      if(str_detect(colnames(file.i[15]), "Dep") == FALSE) {
        file.i[i,15] <- paste0(file.i[i,15],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>15) {
      if(str_detect(colnames(file.i[16]), "Dep") == FALSE) {
        file.i[i,16] <- paste0(file.i[i,16],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    if(length(colnames(file.i))>16){
      if(str_detect(colnames(file.i[17]), "Dep") == FALSE) {
        file.i[i,17] <- paste0(file.i[i,17],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    
    if(length(colnames(file.i))>17){
      if(str_detect(colnames(file.i[18]), "Dep") == FALSE) {
        file.i[i,18] <- paste0(file.i[i,18],"_",file.i[i,depthcol],"/",file.i[i,2])
      }
    }
    
    
    
    
  }
  
  file.i <- file.i[, -c(depthcol)]
  file.i <- file.i[, -c(1:2)]
  
  
  #formatting data
  file.i <- t(file.i)
  
  test <- data.frame(result=unlist(file.i, use.names=FALSE))
  test <- data.frame(result=unlist(test, use.names=FALSE))
  
  takeout <- which(str_detect(parameters, "Dep"))
  parameters <- parameters[-takeout]
  
  takeout2 <- which(str_detect(units, "meters"))
  units <- units[-takeout2]
  
  test$PARAMETER <- parameters
  test$UNITS <- units
  
  #####
  for (i in 1:length(test$result)){
    
    start <- unlist(gregexpr("_",test[i,1]))
    switch <- unlist(gregexpr("/",test[i,1]))
    test$DEPTH[i] <- substr(test[i,1],start+1,switch-1)
    test$TIME[i] <- substr(test[i,1],switch+1,nchar(test[i,1]))
    test$RESULT[i] <- substr(test[i,1],1,start-1)
    
    #edit this so it works for all sites ####
    test$DEPTH[i]<-gsub( "_.*", "", test$DEPTH[i])
    test$TIME[i]<-gsub("/.*", "", test$TIME[i])
    
  }
  
  
  #add back in date, site
  test$DEPTH_UNITS <- "m"
  test$DATE <- date
  test$SITE_ID <- logger
  
  #Remove Log File Name:
  test$SITE_ID <- sub("Log File Name : ", "", test$SITE_ID)
  
  #Clean Units
  # test$UNITS <- sub("�F", "deg ", test$UNITS)
  # test$UNITS <- sub("?S/cm", "uS/cm ", test$UNITS)
  # test$UNITS <- sub("?g/l", "ug/l", test$UNITS)
  # test$UNITS <- sub("?E/s/m?", "umol/s/m2", test$UNITS)
  
  #Clean Unites
  #rewrite this code so it works
  #some units are still messed up ####
  test$UNITS <- str_replace(test$UNITS, "�F", "deg") 
  test$UNITS <- str_replace(test$UNITS, "�S/cm", "uS/cm")
  test$UNITS <- str_replace(test$UNITS,  "�g/l",  "ug/l")
  test$UNITS <- str_replace(test$UNITS, "�E/s/m�", "umol/s/m2")
  
  #Compile
  data <- rbind.fill(data,test)
  
  data <- data[, c("SITE_ID", "DATE", "TIME", "PARAMETER", 
                   "RESULT", "UNITS", "DEPTH", "DEPTH_UNITS")]
  
}
#### 18 warnings think they might be related to UTF encoding but time is righ so carry on ####
warnings()
glimpse(data)

#make date a date
data$DATE<- mdy(data$DATE)


#Failed will probably be null, but if not run this code
failed <- data.frame(failed)

#check site ids
unique(data$SITE_ID)

#fix site ids ######
#FH-MACH FH-MACK is wrong, FH-YELLOW, HALFM, HANS-DOY, LOST-COO TO LOSTLOON
#TETRAULT TO TETRAU
#JETTE is missing volunteer could not be contacted so it never got sampled
data$SITE_ID<- sub("FH-MACH", "FH-MACK", data$SITE_ID)
data$SITE_ID<- sub("FHYELLOW", "FH-YELLOW", data$SITE_ID)
data$SITE_ID<- sub("HALFMOON", "HALFM", data$SITE_ID)
data$SITE_ID<- sub("HANS_DOY", "HANS-DOY", data$SITE_ID)
data$SITE_ID<- sub("LOST-COO", "LOSTLOON", data$SITE_ID)
data$SITE_ID<- sub("TETRAULT", "TETRAU", data$SITE_ID)



#check should be same amount in each
unique(data$SITE_ID)


#add time to others so that they upload properly later ####
for(i in 1:length(data$TIME)){
  if(data$TIME[i] == "NA"){
    data$TIME[i] <- "01:00:00"
  }
}


#the 2025 secchi data will not be included it has not been processed ########
#add secchi from spreadsheet #####
#ONLY coordinator secchi data goes into the hydroshare Volunteer secchi data DOES NOT
#secchi should be in decimal feet---> pre-2025 I have no idea if it actual is I think the data 
#may be a mix of both I left it as is

#raw data
secchi<-read.csv("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/DATA/Secchi/Secchi_2018_2024_NMLN_coordinator data.csv")

glimpse(secchi)

#Format to match the rest of the data

#rename cols
secchi<- secchi %>% dplyr::rename(DATE = Date, SITE_ID = Name, UNITS = Units, 
                                  RESULT = Secchi.Value )


#remove columns
secchi2<-secchi[,c(-2, -8, -7, -6)]

#add needed cols and edit units
secchi2$UNITS <- "ft"
secchi2$DEPTH_UNITS <- "ft"
secchi2$TIME <- ""
secchi2$PARAMETER <- "DEPTH-SECCHI"
secchi2$DEPTH<- secchi2$RESULT




#match site ids 
secchi3<- secchi2 %>% mutate(SITE_ID = case_when(grepl("Abbot", SITE_ID) ~ "ABBOT",
                                                 grepl("Ashley Lake East", SITE_ID) ~ "ASHLEY-E",
                                                 grepl("Ashley Lake West", SITE_ID) ~ "ASHLEY-W",
                                                 grepl("Ashley   East", SITE_ID) ~ "ASHLEY-E",
                                                 grepl("Ashley  West", SITE_ID) ~ "ASHLEY-W",
                                                 grepl("Bailey", SITE_ID) ~ "BAILEY",
                                                 grepl("Beaver", SITE_ID) ~ "BEAVER",
                                                 grepl("Big Therr", SITE_ID) ~ "BIG-THERR",
                                                 grepl("Lake Blaine", SITE_ID) ~ "BLAINE",
                                                 grepl("Blaine", SITE_ID) ~ "BLAINE",
                                                 grepl("Blanchard", SITE_ID) ~ "BLANCH",
                                                 grepl("Bootjack", SITE_ID) ~ "BOOTJACK",
                                                 grepl("Dickey", SITE_ID) ~ "DICKEY",
                                                 grepl("Dollar", SITE_ID) ~ "DOLLAR",
                                                 grepl("Echo", SITE_ID) ~ "ECHO",
                                                 grepl("Fish", SITE_ID) ~ "FISH",
                                                 grepl("Skidoo", SITE_ID) ~ "FH-SKIDOO",
                                                 grepl("Indian", SITE_ID) ~ "FH-INDIAN",
                                                 grepl("Woods", SITE_ID) ~ "FH-WOODS",
                                                 grepl("Conrad", SITE_ID) ~ "FH-CONRAD",
                                                 grepl("Mack", SITE_ID) ~ "FH-MACK",
                                                 grepl("Yellow", SITE_ID) ~ "FH-YELLOW",
                                                 grepl("Somers", SITE_ID) ~ "FH-SOMERS",
                                                 grepl("Foy", SITE_ID) ~ "FOYS",
                                                 grepl("Glen", SITE_ID) ~ "GLEN",
                                                 grepl("Halfmoon", SITE_ID) ~ "HALFM",
                                                 grepl("Hanson", SITE_ID) ~ "HANS-DOY",
                                                 grepl("Holland", SITE_ID) ~ "HOLLAND",
                                                 grepl("Jette", SITE_ID) ~ "JETTE", 
                                                 grepl("Five", SITE_ID) ~ "LAKEFI",
                                                 grepl("Lindbergh", SITE_ID) ~ "LINDBERGH",
                                                 grepl("Little Bitt", SITE_ID) ~ "LITT-BITT",
                                                 grepl("Loon", SITE_ID) ~ "LOON", 
                                                 grepl("Lost Coon", SITE_ID) ~ "LOSTCOO",
                                                 grepl("Lower Still", SITE_ID) ~ "LOWER-STILL",
                                                 grepl("Mary Ronan East", SITE_ID) ~ "MARYRON-E",
                                                 grepl("Mary Ronan West", SITE_ID) ~ "MARYRON-W",
                                                 grepl("McGilvray", SITE_ID) ~ "MCGILV",
                                                 grepl("Murphy", SITE_ID) ~ "MURPHY",
                                                 grepl("Murray", SITE_ID) ~ "MURRAY",
                                                 grepl("Peterson", SITE_ID) ~ "PETERS",
                                                 grepl("Rogers", SITE_ID) ~ "ROGERS",
                                                 grepl("Skyles", SITE_ID) ~ "SKYLES",
                                                 grepl("Smith", SITE_ID) ~ "SMITH",
                                                 grepl("Sophie", SITE_ID) ~ "SOPHIE",
                                                 grepl("Spencer", SITE_ID) ~ "SPENCER",
                                                 grepl("Swan Lake South", SITE_ID) ~ "SWAN-S",
                                                 grepl("Swan Lake North", SITE_ID) ~ "SWAN-N",
                                                 grepl("Swan South", SITE_ID) ~ "SWAN-S",
                                                 grepl("Swan North", SITE_ID) ~ "SWAN-N",
                                                 grepl("Tally", SITE_ID) ~ "TALLY",
                                                 grepl("Tetrault", SITE_ID) ~ "TETRAU",
                                                 grepl("Upper Stillwater", SITE_ID) ~ "UPP-STILL",
                                                 grepl("Whitefish", SITE_ID) ~ "WF-LK-IP1",
                                                 .default = "uhoh"
))


#see if this worked
unique(secchi3$SITE_ID)

unique(data$SITE_ID)

glimpse(secchi3)

#remove blaine years with bad date format and is old data anyways
secchi3<- secchi3 %>% filter(!(DATE == "2011" | DATE == "2012" | DATE == "2013" | DATE == "2014" |
                                 DATE == "2105" | DATE == "2016"))

#remove lakes that don't belong in NMLN
secchi3<- secchi3 %>% filter(!(SITE_ID == "WF-LK-IP1" | SITE_ID == "TALLY"))


#cut to only most recent year
secchi3$DATE<- mdy(secchi3$DATE)
glimpse(secchi3)
secchi3<- secchi3 %>% filter(DATE > "2023-12-31")


#combine
data.1 <- rbind.fill(data, secchi3)
#check that it combined properly add up observations and both and make sure they equal data.1
unique(data.1$SITE_ID)
unique(data$SITE_ID)

#save csv
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
write.csv(data, "HydrolabCompilationNMLN2025.csv", row.names= FALSE)

