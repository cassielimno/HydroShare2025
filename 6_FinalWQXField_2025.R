rm(list=ls())

library(readxl)
library(openxlsx)
#set working directory
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")

#Load Data ####
#these get moved into PreFinaldata folder so check there as well
wwqmp <- data.frame(read_excel("WWQMP_Field_EDD_25_cr.xlsx",
                               sheet= "Result"))
nmln <- data.frame(read_excel("NMLN_Field_EDD_25_cr.xlsx",
                              sheet= "Result"))

nmln.station <- data.frame(read_excel("NMLN_Field_EDD_25_cr.xlsx",
                                      sheet= "Activity"))

#Extract site from activity ID (WWQMP) (takes ~2 minutes)
for(i in 1:length(wwqmp$Data_Logger_Line_ID)){
  end <- unlist(gregexpr("_\\d+-\\d+", wwqmp$Activity_ID[i]))
  wwqmp$site[i] <- substr(wwqmp$Activity_ID[i], 1, end-1)
}

#Extract date from activity ID (WWQMP) (takes ~2 minutes)
for(i in 1:length(wwqmp$Activity_ID)){
  start <- unlist(gregexpr("_\\d+-\\d+", wwqmp$Activity_ID[i]))
  end2 <- unlist(gregexpr("_\\d+:\\d+", wwqmp$Activity_ID[i]))
  wwqmp$date[i] <- substr(wwqmp$Activity_ID[i], start+1, end2-1)
}

#Extract site from activity ID (NMLN) (takes ~2 minutes)
for(i in 1:length(nmln$Data_Logger_Line_ID)){
  end <- unlist(gregexpr("_\\d+-\\d+", nmln$Activity_ID[i]))
  nmln$site[i] <- substr(nmln$Activity_ID[i], 1, end-1)
}

#Extract date from activity ID (NMLN)(takes ~2 minutes)
for(i in 1:length(nmln$Activity_ID)){
  start <- unlist(gregexpr("_\\d+-\\d+", nmln$Activity_ID[i]))
  end2 <- unlist(gregexpr("_\\d+:\\d+", nmln$Activity_ID[i]))
  nmln$date[i] <- substr(nmln$Activity_ID[i], start+1, end2-1)
}

#Function for removing NAs to leave empty cells
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


#Separate lakes out from other baseline sites
#QA decisions were different between streams and lakes
#make sure you are matching correct site names ####
unique(wwqmp$Activity_ID)
lakes<- which(str_detect(wwqmp$Activity_ID, "WF-LK-IP") == TRUE 
              |str_detect(wwqmp$Activity_ID, "TALLY") == TRUE
              |str_detect(wwqmp$Activity_ID, "WF.LK.IP") == TRUE)

lake <- wwqmp[lakes ,]
stream <- wwqmp[-lakes ,]


#check
unique(lake$site)
unique(stream$site)

#Convert results to numerbers
stream$Result_Value <- as.numeric(stream$Result_Value)

# WWQMP Streams Quality Assurance####
#See Quality Assurance document for justifications on data removals

#Barometric Pressure

#Set removal column default to 'keep'
stream$remove <- "keep"

#Remove data points for barometric pressure outside 666 and 707 mmHg range
for(i in 1:length(stream$Result_Value)){
  if(stream$Characteristic_Name[i] == "Barometric pressure" & stream$Result_Value[i] > 707
     | stream$Characteristic_Name[i] == "Barometric pressure" & stream$Result_Value[i] < 666){
    
    stream$remove[i] <- "BP"
  }
  
}

#28 total observations
bp <- stream[stream$remove == "BP" ,]

#Store IDs where BP was unreliable; BP is used for dissolved oxygen calculations
bp.ids <- unique(bp$Activity_ID)

#Flag dissolved oxygen saturation data where BP was unreliable
for(i in 1:length(bp.ids)){
  id <- bp.ids[i]
  
  for(j in 1:length(stream$Result_Value)){
    if(stream$Activity_ID[j] == id
       & stream$Characteristic_Name[j] == "Dissolved oxygen saturation"){
      
      stream$remove[j] <- "bp-dosat"
    }
    
  }
  
}

#28 total points
sat <- stream[stream$remove == "bp-dosat" ,]

#Chlorophyll a --- from the probe

#Flag chlorophyll a data above 8.5 ug/l
stream$remove[stream$Characteristic_Name == "Chlorophyll a (probe)" & stream$Result_Value > 8.5] <- "chla"
#6 data point
chla <- stream[stream$remove == "chla" ,]

#DO Concentration

#Remove dissolved oxygen concentrations less than 3 mg/l
stream$remove[stream$Characteristic_Name == "Dissolved oxygen (DO)" & stream$Result_Value < 3] <- "do"
#17 total data points
do <- stream[stream$remove == "do" ,]

#If dissolved oxygen concentration was unreliable, flag dissolved oxygen saturation as well
do.id <- unique(do$Activity_ID)

#add second remove col so they don't write over other do issues
stream$remove2<- "keep"

for(i in 1:length(do.id)){
  id <- do.id[i]
  
  for(j in 1:length(stream$Activity_ID)){
    if(stream$Activity_ID[j] == id & stream$Characteristic_Name[j] == "Dissolved oxygen saturation"){
      
      stream$remove2[j] <- "conc-dosat"
    }
  }
  
}

#DO Saturation
#17 + 28 total points removed for DO saturation (DO concentration + BP concerns)
#8 of the bp ones are also conc-dosat ones so there will only be 37 in dosat
dosat <- stream[stream$remove2 == "conc-dosat" | stream$remove == "bp-dosat" ,]

#PAR
#PAR cable not used in stream measurements, so values not reliable
stream$remove[stream$Characteristic_Name == "Light, photosynthetic active radiation (PAR)"] <- "remove"

#Raw file data
#Flag parameters that will be uploaded as no-QA data attachment
stream$remove[stream$Characteristic_Name == "pH" 
              | stream$Characteristic_Name == "Oxidation reduction potential (ORP)"
              | (stream$Characteristic_Name == "Chlorophyll a (probe)" 
                 & stream$Result_Value < 30)] <- "rawfile"

#270 data points for raw data file
stream.rawfile <- stream[stream$remove == "rawfile" ,]
#check
unique(stream.rawfile$Characteristic_Name)

#Specific Conductance

#Remove Smith Creek specific conductance values below 100 uS/cm
stream$remove[stream$Characteristic_Name == "Specific conductance" 
              & stream$site == "SMITH-CRK-ELD" & stream$Result_Value < 100] <- "smithsc"

#Remove all specific conductance values below 40 uS/cm
stream$remove[stream$Characteristic_Name == "Specific conductance" 
              & stream$Result_Value < 40] <- "sc"

#4 total data points
sc <- stream[stream$remove == "smithsc" | stream$remove == "sc" ,]

#Temperature

#Remove temperatures greater than 100 degrees F
stream$remove[stream$Characteristic_Name == "Temperature, water" & stream$Result_Value > 100] <- "temp"
#0 data points
temp <- stream[stream$remove == "temp" ,]

#make a quick edit here
#fix units there should be no degC ---- and fix result value unit
stream$Result_Value_Unit<-sub("degC", "deg", stream$Result_Value_Unit)


#Keep just points that haven't been flagged for removal
stream.upload <- stream[stream$remove == "keep" & stream$remove2 == "keep",]

#check this ------ still has rawfile parameters in it???? ######
unique(stream.upload$Characteristic_Name)

#Convert dates to character strings
stream.upload$Analysis_Start_Date <- as.character(stream.upload$Analysis_Start_Date)

#Remove NAs from data frame
stream.upload <- clear.na(stream.upload)




#WWQMP Lakes Quality Assurance ####
#See quality assurance document for justifications on data removals

#Set default for removal column to 'keep'
lake$remove <- "keep"

#in case I need a second col
lake$remove2<- "keep"

#Convert results to numbers
lake$Result_Value <- as.numeric(lake$Result_Value)

#Barometric Pressure

#Remove barometric pressures outside 650-710 mmHg range
for(i in 1:length(lake$Result_Value)){
  if(lake$Characteristic_Name[i] == "Barometric pressure" & lake$Result_Value[i] > 710
     | lake$Characteristic_Name[i] == "Barometric pressure" & lake$Result_Value[i] < 650){
    
    lake$remove[i] <- "BP"
  }
  
}

#98 data points
bp <- lake[lake$remove == "BP" ,]

#Store activity IDs where barometric pressure was not reliable
bp.ids <- unique(bp$Activity_ID)

#Flag DO saturation where BP was unreliable; BP used in DO saturation calculations
for(i in 1:length(bp.ids)){
  id <- bp.ids[i]
  
  for(j in 1:length(lake$Result_Value)){
    if(lake$Activity_ID[j] == id
       & lake$Characteristic_Name[j] == "Dissolved oxygen saturation"){
      
      lake$remove[j] <- "bp-dosat"
    }
    
  }
  
}

#98 data points
sat <- lake[lake$remove == "bp-dosat" ,]

#Chlorophyll a

#Remove chlorophyll a values above 8.5 ug/l
lake$remove[lake$Characteristic_Name == "Chlorophyll a (probe)" &lake$Result_Value > 8.5] <- "chla"

#5 data points
chla <- lake[lake$remove == "chla" ,]

#DO Concentration
#Remove DO concentrations above 15 mg/l
#Or below 5 mg/l in Whitefish Lake, because it does not get anaerobic
lake$remove[lake$Characteristic_Name == "Dissolved oxygen (DO)" & lake$Result_Value > 15] <- "do.high"
lake$remove[lake$Characteristic_Name == "Dissolved oxygen (DO)"&lake$site == "WF-LK-IP2" &lake$Result_Value < 5] <- "do.low"

#3 data points THIS NEEDS TO BE ACTUALLY CHECKED EACH TIME
#TO MAKE SURE IT IS JUST A RANDOM LOW VALUE AND NOT ANOXIA
#CHECK THE PROFILE FOR ANY SUS VALUES
do <- lake[lake$remove == "do.high" | lake$remove == "do.low" ,]

#Flag unreliable DO concentration points for removal from DO saturation as well
do.id <- unique(do$Activity_ID)

for(i in 1:length(do.id)){
  id <- do.id[i]
  
  for(j in 1:length(lake$Activity_ID)){
    if(lake$Activity_ID[j] == id & lake$Characteristic_Name[j] == "Dissolved oxygen saturation"){
      
      lake$remove[j] <- "conc-dosat"
    }
  }
  
}

#DO Saturation
#101 data points removed, BP + DO concentration concerns
dosat <- lake[lake$remove == "conc-dosat" | lake$remove == "bp-dosat" ,]

#Data to be uploaded as a no-QA raw data attachment
lake$remove[lake$Characteristic_Name == "pH" 
            | lake$Characteristic_Name == "Oxidation reduction potential (ORP)"
            | (lake$Characteristic_Name == "Chlorophyll a (probe)" & lake$Result_Value < 8.5)
            |lake$Characteristic_Name == "Light, photosynthetic active radiation (PAR)"] <- "rawfile"

#2262 data points for raw data attachment
lake.rawfile <- lake[lake$remove == "rawfile" ,]

#check
unique(lake.rawfile$Characteristic_ID)

#Specific Conductance

#Remove specific conductance below 50 uS/cmn
lake$remove[lake$Characteristic_Name == "Specific conductance" & lake$Result_Value < 50] <- "sclow"

#Remove specific conductance above 180 not in Tally lake (Tally was regularly higher)
lake$remove[lake$Characteristic_Name == "Specific conductance" & lake$Result_Value > 180 & lake$site != "TALLY"] <- "sciphigh"

#Remove high specific conductance data by depth from Tally lake
lake$remove[lake$Characteristic_Name == "Specific conductance" & 
              lake$Result_Value > 200 & lake$site == "TALLY" |
              lake$Characteristic_Name == "Specific conductance" &
              lake$Result_Value > 190 & lake$Result_Depth_Height_Measure > 129] <- "sctallyhigh"

#3 points
sc <- lake[lake$remove == "sclow" | lake$remove == "sciphigh" | lake$remove == "sctallyhigh" ,]

#Turbidity
#Remove turbidity above 50 NTU
lake$remove[lake$Characteristic_Name == "Turbidity" & lake$Result_Value > 50] <- "turb"

#0 data points
turb <- lake[lake$remove == "turb" ,]

#Remove all data not being uploaded
lake.upload <- lake[lake$remove == "keep" ,]

#Add NA as sample fraction
lake.upload$Sample_Fraction[2:length(lake.upload$Sample_Fraction)] <- "NA"

#Convert dates to character strings
lake.upload$Analysis_Start_Date <- as.character(lake.upload$Analysis_Start_Date)
lake.upload$date <- as.character(lake.upload$date)

#Remove NAs
lake.upload <- clear.na(lake.upload)


#NMLN Quality Assurance ####
#See quality assurance document for justification on data removals

#Set default for removal column to keep
nmln$remove <- "keep"

#Convert results to numbers
nmln$Result_Value <- as.numeric(nmln$Result_Value)


#Barometric pressure

#Remove BP lower than 640 mmHg
nmln$remove[nmln$Characteristic_Name == "Barometric pressure" & nmln$Result_Value < 640] <- "BP"

#26 total data points
bp <- nmln[nmln$remove == "BP" ,]

#Store IDs where BP was unreliable
bp.ids <- unique(bp$Activity_ID)

#Dissolved oxygen saturation

#Remove dissolved oxygen saturation where BP was unreliable
for(i in 1:length(bp.ids)){
  id <- bp.ids[i]
  
  for(j in 1:length(nmln$Activity_ID)){
    if(nmln$Activity_ID[j] == id & nmln$Characteristic_Name[j] == "Dissolved oxygen saturation"){
      
      nmln$remove[j] <- "bp-dosat"
    }
    
  }
}

#26 total data points
sat <- nmln[nmln$remove == "bp-dosat" ,]

#Specific conductance

#If specific conductance units are mS/cm, convert to uS/cm
for(i in 1:length(nmln$Result_Value_Unit)){
  
  if(nmln$Result_Value_Unit[i] == "mS/cm"){
    nmln$Result_Value_Unit[i] <- "uS/cm"
    nmln$Result_Value[i] <- nmln$Result_Value[i] * 1000
  }
  
}

#Remove specific conductance less than 32 uS/cm not at Lindbergh (Lindbergh consistantly low)
nmln$remove[nmln$Characteristic_Name == "Specific conductance" 
            & nmln$Result_Value < 32 & nmln$site != "LINDBERGH"] <- "sc"

#2 total data points
sc <- nmln[nmln$remove == "sc" ,]

#Water temp

#Remove temperature above 82 degrees F
nmln$remove[nmln$Characteristic_Name == "Temperature, water" & nmln$Result_Value > 82] <- "temp"

#0 data points
temp <- nmln[nmln$remove == "temp" ,]

#Concern that these readings may have been just above lake surface,
#Remove all data associated with high temperature readings
temp.ids <- unique(temp$Activity_ID)

#Remove data from results worksheet
for(i in 1:length(temp.ids)){
  id <- temp.ids[i]
  
  for(j in 1:length(nmln$Activity_ID)){
    if(nmln$Activity_ID[j] == id){
      
      nmln$remove[j] <- "hightemp"
    }
  }
  
}

#0 total datapoints
hightemp <- nmln[nmln$remove == "hightemp" ,]

#Remove activities associated with high temperatures

nmln.station$remove <- "keep"

for(i in 1:length(temp.ids)){
  id <- temp.ids[i]
  
  for(j in 1:length(nmln.station$Activity_ID)){
    if(nmln.station$Activity_ID[j] == id){
      
      nmln.station$remove[j] <- "hightemp"
    }
  }
  
}

#0 activities removed
nmln.station <- nmln.station[nmln.station$remove = "hightemp" ,]

#remove rows where depth is blank (one is the text row) #####
nmln<- nmln %>% filter(!if_any(Result_Depth_Height_Measure, is.na))

#remove lakes where all depth is taken at 0 ####
nmln$Result_Depth_Height_Measure<-as.numeric(nmln$Result_Depth_Height_Measure)
for( i in unique(nmln$site)){
  
  onelake<-nmln %>% filter(site == i)
  
  print(unique(onelake$site))
  
  print(range(onelake$Result_Depth_Height_Measure))
  
  
}
#remove any lakes with range of 0 ####


#Data not to be uploaded at all
#NOTE THIS IS WHERE SECCHI, TURB AND PAR ARE REMOVED #####
#leaving 2024 secchi in -- it is only the non-volunter values
#for now for 2023 im gonna keep secchi in
nmln$remove[nmln$Characteristic_Name == "Turbidity"
            #| nmln$Characteristic_Name == "Depth, Secchi disk depth"
            | nmln$Characteristic_Name == "Light, photosynthetic active radiation (PAR)"] <- "remove"

#Flag data to be uploaded as part of no-QA raw data attachment
nmln$remove[nmln$Characteristic_Name == "Chlorophyll a (probe)"
            | nmln$Characteristic_Name == "Oxidation reduction potential (ORP)"
            | nmln$Characteristic_Name == "pH"
            &is.na(nmln$Characteristic_Name) == FALSE] <- "rawfile"

#2238 data points for raw data file
rawfile <- nmln[nmln$remove == "rawfile" ,]
#check
unique(rawfile$Characteristic_Name)
#Data to keep for upload
nmln.upload <- nmln[nmln$remove == "keep" ,]


result.ids <- unique(nmln.upload$Activity_ID)
#Make sure all activity IDs still match between activity and result sheet
which(result.ids %in% nmln.station$Activity_ID == FALSE)

#Remove NAs
nmln.upload <- clear.na(nmln.upload)
nmln.station <- clear.na(nmln.station)




#Update Excel Files ####

#Load WQX format workbook
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
wwqmp.field <- loadWorkbook("WWQMP_Field_EDD_25_cr.xlsx")
nmln.field <- loadWorkbook("NMLN_Field_EDD_25_cr.xlsx")

#Merge stream and lake data for WWQMP
wwqmp.upload <- rbind.fill(stream.upload, lake.upload)

#Remove columns not needed for upload -- remove col site and date
#check these match up ####
nmln.station <- nmln.station[, -28]
nmln.upload <- nmln.upload[, -c(40:43)]
wwqmp.upload <- wwqmp.upload[, -c(40:43)]

#Clear existing data from worksheets
deleteData(wb= nmln.field, sheet= "Activity", cols = 1:100, rows = 1:100000, 
           gridExpand = TRUE)
deleteData(wb= nmln.field, sheet= "Result", cols = 1:100, rows = 1:100000, 
           gridExpand = TRUE)
deleteData(wb= wwqmp.field, sheet= "Result", cols = 1:100, rows = 1:100000, 
           gridExpand = TRUE)
#new addition ####
deleteData(wb= nmln.field, sheet= "Activity", cols = 1:100, rows = 1:100000, 
           gridExpand = TRUE)

#Update Activity for NMLN
writeData(nmln.field, sheet= "Activity", x= nmln.station)


#Update Results for NMLN
writeData(nmln.field, sheet= "Result", x= nmln.upload)

#Update Results for WWQMP
writeData(wwqmp.field, sheet= "Result", x= wwqmp.upload)

#Write updated excel files
saveWorkbook(wwqmp.field, file= "WWQMP_Field_EDD_25_cr.xlsx",
             overwrite = TRUE)

saveWorkbook(nmln.field, file= "NMLN_Field_EDD_25_cr.xlsx",
             overwrite = TRUE)

#Write csvs for data rawfiles
rawfile <- clear.na(rawfile)
rawfile <- rawfile[, -c(38:40)]

write.csv(rawfile, "NMLN_RawFileOnlyData_25_cr.csv",
          row.names = FALSE)

wwqmp.rawfile <- rbind.fill(stream.rawfile, lake.rawfile)
wwqmp.rawfile <- clear.na(wwqmp.rawfile)
wwqmp.rawfile <- wwqmp.rawfile[, -c(38:40)]

write.csv(wwqmp.rawfile, "WWQMP_RawDataFile_25_cr.csv",
          row.names= FALSE)

