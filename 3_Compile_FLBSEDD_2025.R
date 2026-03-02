#for baseline
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/WATER CHEMISTRY/Water_Chemistry_25")
#for nmln
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/DATA/Chemistries")
rm(list=ls())


library(readxl)
library(plyr)
library(stringr)
library(dplyr)

#Compile and Clean EDDs ####


#Merge EDDs by program and add program details
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/DATA/Chemistries")
setwd("C:/Users/User/Dropbox/WLI (2)/PROJECTS/NMLN/REPORTING/DATA/Chemistries/Master Chemistry Data")
nmln.edds <- read.csv("VMP_EDD (2025).csv")
setwd("C:/Users/User/Dropbox/database/WF WQ DATA/WATER CHEMISTRY/Water_Chemistry_25")
base.edds <- read.csv("WLI_Baseline_EDD_2025.csv")
nmln.edds$Program <- "NMLN"
base.edds$Program <- "Baseline"

#turns things uppercase in case they aren't
nmln.edds$Activity_ID<- toupper(nmln.edds$Activity_ID)

#Tally will be uploaded as Baseline program
for(i in 1:length(nmln.edds$Activity_ID)){
  
  if(str_detect(nmln.edds$Activity_ID[i], "TALLY") == TRUE){
    
    nmln.edds$Program[i] <- "Baseline"
    
  }
  
}

#check the station ids just for NMLN do baseline at the end #####
#baseline has to wait because the depths are stored in the station ID
#change all underscores, lost loon, tetrau, fh-mack, 
glimpse(nmln.edds)
unique(nmln.edds$Activity_ID)


#fix activity ids
nmln.edds$Activity_ID<-sub("_", "-", nmln.edds$Activity_ID)
nmln.edds$Activity_ID<-sub("LOSTCOO", "LOSTLOON", nmln.edds$Activity_ID)
nmln.edds$Activity_ID<-sub("TETRAULT", "TETRAU", nmln.edds$Activity_ID)
nmln.edds$Activity_ID<-sub("FH-MACH", "FH-MACK", nmln.edds$Activity_ID)

#CHECK
unique(nmln.edds$Activity_ID)

#Merge both program EDDs
edds <- rbind(nmln.edds, base.edds)



#Extract date and time from the comment text

for (i in 1:length(edds$Result_Comment)){
  start <- unlist(gregexpr(pattern="Date: \\d+/\\d+/\\d+",edds$Result_Comment[i]))
  end <- nchar(edds$Result_Comment[i])
  edds$Activity_Date[i] <- paste(substr(edds$Result_Comment[i], start+6, end))
}


#Fill date to Activity Date column
for (i in 1:length(edds$Activity_Date)){
  start <- unlist(gregexpr(pattern=" \\d+",edds$Activity_Date[i]))
  end <- nchar(edds$Activity_Date[i])
  edds$Activity_Time[i] <- paste(substr(edds$Activity_Date[i], start+1, end))
}

for (i in 1:length(edds$Activity_Date)){
  ifelse(str_detect(edds$Activity_Time[i], "M")==TRUE, 
         yes= edds$Activity_Date[i] <- sub(edds$Activity_Time[i], "", edds$Activity_Date[i]),
         no= edds$Activity_Time[i] <- "")
  
}

#Format date as POSIX
edds$Activity_Date <- as.POSIXct(edds$Activity_Date, format="%m/%d/%Y")

#Format times as POSIX
edds$Activity_Time <- as.POSIXct(edds$Activity_Time, format= "%I:%M:%S %p")
edds$Activity_Time <- sub(Sys.Date(), "", edds$Activity_Time)



base.edds <- edds[edds$Program == "Baseline" ,]
nmln.edds <- edds[edds$Program == "NMLN" ,]



#Write csv for initial compilation
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2024/HydroShareIntermediateData2024")
write.csv(edds, "FLBSEDDCompilation_25-03-26_cr.csv")


#SiteID merge ####

#Create unique site list
sites <- unique(edds[c("Activity_ID", "Program")])
nmln.sites <- sites[sites$Program == "NMLN" ,]
base.sites <- sites[sites$Program == "Baseline" ,]

#Write csv of site lists for each program
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2024/HydroShareIntermediateData2024/Sites")
write.csv(nmln.sites, "FLBSNMLN_Sites_25-03-26_cr.csv",
          row.names= FALSE)
write.csv(base.sites, "FLBSBaseline_Sites_25-03-26_cr.csv",
          row.names = FALSE)


#Read in updated site list
#Manual updates in excel included:
#Matching site names to site IDs --> this should be good already
#Adding activity type (routine, duplicate, blank) ---> search for dup or duplicate 
#if there are no dup then all are routine
#and this col can easily be added in R

# sites1 <- read.csv("../3_CompilationResults/Sites/FLBSBaseline_Sites_Edited_22-01-14_mr.csv")
# colnames(sites1)[1] <- "Activity_ID"
# 
# sites2 <- read.csv("../3_CompilationResults/Sites/FLBSNMLN_Sites_Edited_22-01-14_mr.csv")

#if you read lines 91-96 and are doing thing in R rather than manually use the 
#below code #####
#if not un comment lines 98-101 and use that code for it
#forbaseline
sites1<-base.sites

#make activity type col and a correct site id col
#fix tribs here as well ####
unique(sites1$Activity_ID)

# #turns things uppercase in case they aren't #####
#apply this to a temporary column
sites1$Activity_ID2<-sites1$Activity_ID

sites1$Activity_ID2<- toupper(sites1$Activity_ID2)

#THIS IS WHERE ONE OF THEM NEEDS TO BE ADDED AT ACTIVITY TYPE DUPLICATE FIX THE CODE ####
sites1<-sites1 %>% mutate(Activity_Type = case_when(str_detect(Activity_ID2, "DUPLICATE") ~ "Duplicate",
                                                    str_detect(Activity_ID2, "BLANK") ~ "Blank",
                                                    .default = "Routine" ), 
                          Site_ID =  case_when(str_detect(Activity_ID2, "WF-LK-IP1") ~ "WF-LK-IP1",
                                               str_detect(Activity_ID2, "WF-LK-IP2") ~ "WF-LK-IP2",
                                               str_detect(Activity_ID2, "WF_LK_IP1") ~ "WF-LK-IP1",
                                               str_detect(Activity_ID2, "WF_LK_IP2") ~ "WF-LK-IP2",
                                               str_detect(Activity_ID2, "BEAV_CRK_RR") ~ "BEAV-CRK-RR",
                                               str_detect(Activity_ID2, "HELLR_CRK_T") ~ "HELLR-CRK-T",
                                               str_detect(Activity_ID2, "LAZY_CRK_S") ~ "LAZY-CRK-S",
                                               str_detect(Activity_ID2, "SMITH_CRK_ELD" ) ~ "SMITH-CRK-ELD" ,
                                               str_detect(Activity_ID2, "SWIFT_CRK_DEL") ~ "SWIFT-CRK-DEL",
                                               str_detect(Activity_ID2, "VIKING_CRK_WA") ~ "VIKING-CRK-WA",
                                               str_detect(Activity_ID2, "WF_R_SPB") ~ "WF-R-SPB",
                                               str_detect(Activity_ID2, "COW_CRK_PA") ~ "COW-CRK-PA",
                                               str_detect(Activity_ID2, "HASK_CRK_MR") ~ "HASK-CRK-MR",
                                               str_detect(Activity_ID2, "WALKER_CRK_MR" ) ~ "WALKER-CRK-MR" ,
                                               str_detect(Activity_ID2, "COW_CR_PA") ~ "COW-CRK-PA",
                                               str_detect(Activity_ID2, "COW_CRK_RR") ~ "COW-CRK-PA",
                                               str_detect(Activity_ID2, "SWIFT_CRK_OLN") ~ "SWIFT-CRK-OLN",
                                               str_detect(Activity_ID2, "HELLR_CRK_WA" ) ~ "HELLR-CRK-T" ,
                                               str_detect(Activity_ID2, "WLS") ~ "MLS",
                                               str_detect(Activity_ID2, "WF_LL_IP1") ~ "WF-LK-IP1",
                                               str_detect(Activity_ID2, "HELL_CRK_T") ~ "HELLR-CRK-T",
                                               .default = Activity_ID2))

    
#additional fixes for 2024 ####
#fix hellroaring and wf ll both added in the code above

#check this
unique(sites1$Site_ID)

unique(sites1$Activity_ID)

#remove temp column -------------------------------- select all but activity id 2 here
head(sites1)
sites1<-sites1 %>% select(Activity_ID, Program, Activity_Type, Site_ID)

#again for nmln
sites2<-nmln.sites

#FIX THIS RIGHT HERE ######
sites2<-sites2 %>% mutate(Activity_Type = case_when(str_detect(Activity_ID, "DUPLICATE") ~ "Duplicate",
                                                    str_detect(Activity_ID, "BLANK") ~ "Blank",
                                                    .default = "Routine" ),
                          Site_ID = Activity_ID)




#Merge Baseline sites and EDDs (sites not included in site list were not long term monitoring sites)
#stopped here #####
edd2 <- merge(base.edds, sites1, by="Activity_ID")

#Take out pheophytin data these all have blank characterisit ID's and are not needed ####
edd2<- edd2 %>% filter(!(Characteristic_ID == ""))

#take out mls samples ---these don't need to be processed for hydroshare
edd2<-edd2 %>% filter(!(Site_ID == "MLS"))



#Extract Depths for Whitefish lake site
edd2$tempdepth <- ""

for(i in 1:length(edd2$Activity_ID)){
  
  if(edd2$Site_ID[i] == "WF-LK-IP1"
     | edd2$Site_ID[i] == "WF-LK-IP2"){
    
    edd2$tempdepth[i] <- edd2$Activity_ID[i]
  }
  
}

unique(edd2$tempdepth)

#Remove everything related to lake names
edd2$tempdepth <- sub("WF Lake", "", edd2$tempdepth)
edd2$tempdepth <- sub("WF lake", "", edd2$tempdepth)
edd2$tempdepth <- sub("WF_LK_IP1", "", edd2$tempdepth)
edd2$tempdepth <- sub("WF_LK_IP1_", "", edd2$tempdepth)
edd2$tempdepth <- sub("WF_LK_IP2", "", edd2$tempdepth)
edd2$tempdepth <- sub("WF_LK_IP2_", "", edd2$tempdepth)
edd2$tempdepth <- sub("WFLake", "", edd2$tempdepth)
edd2$tempdepth <- sub("WfLake", "", edd2$tempdepth)
edd2$tempdepth <- sub("Whitefish Lake", "", edd2$tempdepth)
edd2$tempdepth <- sub("Whitefish lake", "", edd2$tempdepth)
edd2$tempdepth <- sub("WF-LK-IP1", "", edd2$tempdepth)
edd2$tempdepth <- sub("WF-LK-IP2", "", edd2$tempdepth)

unique(edd2$tempdepth)

#Remove everything IP related
edd2$tempdepth <- sub("IP-1", "", edd2$tempdepth)
edd2$tempdepth <- sub("IP1", "", edd2$tempdepth)
edd2$tempdepth <- sub("IP-2", "", edd2$tempdepth)
edd2$tempdepth <- sub("IP2", "", edd2$tempdepth)
edd2$tempdepth <- sub("Ip-2", "", edd2$tempdepth)
edd2$tempdepth <- sub("IP 1", "", edd2$tempdepth)

unique(edd2$tempdepth)

#Remove chla indicators
edd2$tempdepth <- sub("Chla", "", edd2$tempdepth)
edd2$tempdepth <- sub("Chl", "", edd2$tempdepth)
edd2$tempdepth <- sub("chl", "", edd2$tempdepth)
edd2$tempdepth <- sub("CHLA", "", edd2$tempdepth)
edd2$tempdepth <- sub("\\(a\\)", "", edd2$tempdepth)

unique(edd2$tempdepth)

#Remove any remaining words or letters
edd2$tempdepth <- sub("m", "", edd2$tempdepth)
edd2$tempdepth <- sub("M", "", edd2$tempdepth)
edd2$tempdepth <- sub("-a", "", edd2$tempdepth)
edd2$tempdepth <- sub("Calcium", "", edd2$tempdepth)

unique(edd2$tempdepth)

#Remove any additional symbols
edd2$tempdepth <- sub("\\(", "", edd2$tempdepth)
edd2$tempdepth <- sub("\\)", "", edd2$tempdepth)
edd2$tempdepth <- sub("_", "", edd2$tempdepth)
edd2$tempdepth <- sub("\\,", "", edd2$tempdepth)
edd2$tempdepth <- sub("-", "", edd2$tempdepth)

unique(edd2$tempdepth)

#Add tempdepth to actual depth column
edd2$Result_Depth_Height_Measure <- as.numeric(edd2$tempdepth)

#Add units where needed
for(i in 1:length(edd2$Activity_ID)){
  if(is.na(edd2$Result_Depth_Height_Measure[i]) == FALSE){
    edd2$Result_Depth_Height_Measure_Unit[i] <- "m"
  }
}

#Remove temporary column
#make sure this is correct col ####
edd2 <- edd2[, -44]

#Clear NAs from empty cells
for(i in 1:length(colnames(edd2))){
  
  list1 <- edd2[, i]
  
  for(j in 1:length(list1)){
    
    if(is.na(list1[j])==TRUE){
      list1[j] <- ""
    }
  }
  
  edd2[, i] <- list1
}

#check baseline activity ids and station ids #####
unique(edd2$Activity_ID)
unique(edd2$Site_ID)
edd2$Activity_ID<- toupper(edd2$Activity_ID)

edd2$Site_ID<-sub("WF_RVR_SPB|WF_RVR_SRB", "WF-R-SPB", edd2$Site_ID)

#Removes some stuff that is not compatable with WQX otherwise these 
#and fixes a mistake the lab made
#fix labeling of lines 
edd3<- edd2 %>% mutate(Sample_Fraction = case_when(Characteristic_ID == "14265-44-2" ~ "Dissolved",
                                                               Characteristic_ID == "TP" ~ "Unfiltered",
                                                               Characteristic_ID == "TN" ~ "Unfiltered",
                                                               Characteristic_ID == "7631-86-9" ~ "Dissolved",
                                                               Characteristic_ID == "NN" ~ "Dissolved",
                                                               Characteristic_ID == "ORGANIC-C" ~ "Unfiltered",
                                                               Characteristic_ID == "7664-41-7" ~ "Dissolved",
                                                               .default = "NA"),
                                   Lower_Reporting_Limit_Value = case_when(Characteristic_ID == "14265-44-2" ~ .800,
                                                                           Characteristic_ID == "TP" ~ 1.50,
                                                                           Characteristic_ID == "TN" ~ 25.00,
                                                                           Characteristic_ID == "7664-41-7" ~ 1.50,
                                                                           Characteristic_ID == "NN" ~ 1.50,
                                                                           Characteristic_ID == "ORGANIC-C" ~ .100 ,
                                                                           Characteristic_ID == "7631-86-9" ~ .100,
                                                                           Characteristic_ID == "CHL-A-CP" ~ .200,
                                                                           TRUE ~ as.numeric(Lower_Reporting_Limit_Value)),
                                   Detection_Limit_Unit = case_when(Characteristic_ID == "14265-44-2" ~ "ug/L",
                                                                    Characteristic_ID == "TP" ~ "ug/L",
                                                                    Characteristic_ID == "TN" ~ "ug/L",
                                                                    Characteristic_ID == "7664-41-7" ~ "ug/L",
                                                                    Characteristic_ID == "NN" ~ "ug/L",
                                                                    Characteristic_ID == "ORGANIC-C" ~ "mg/L" ,
                                                                    Characteristic_ID == "7631-86-9" ~ "mg/L",
                                                                    Characteristic_ID == "CHL-A-CP" ~ "ug/L",
                                                                    TRUE ~ as.character(Detection_Limit_Unit)),
                                   Activity_Type = case_when(Characteristic_ID == "14265-44-2" ~ "S-ROUTINE",
                                                             Characteristic_ID == "TP" ~ "S-ROUTINE",
                                                             Characteristic_ID == "TN" ~ "S-ROUTINE",
                                                             Characteristic_ID == "7664-41-7" ~ "S-ROUTINE",
                                                             Characteristic_ID == "NN" ~ "S-ROUTINE",
                                                             Characteristic_ID == "ORGANIC-C" ~ "S-ROUTINE" ,
                                                             Characteristic_ID == "7631-86-9" ~ "S-ROUTINE",
                                                             Characteristic_ID == "CHL-A-CP" ~ "S-ROUTINE",
                                                             Characteristic_ID == "TSS" ~ "S-ROUTINE",
                                                             TRUE ~ "F-MSR/OBS"),
                                   Laboratory_Name = case_when(Activity_Type == "S-ROUTINE" ~ "FLBS",
                                                               TRUE ~ as.character(Laboratory_Name)),
                                   Result_Detection_Condition = case_when(Result_Value = is.na(Result_Value) ~ "Not Detected",
                                                                          TRUE ~ as.character(Result_Detection_Condition)),
                                   Method_Speciation_Name = case_when(Characteristic_ID == "ORGANIC-C" ~ "",
                                                                      TRUE ~ as.character(Method_Speciation_Name)),
                                   
                                   Analytical_Method_ID = case_when(Characteristic_ID == "14265-44-2" ~ "365.1",
                                                                    Characteristic_ID == "TP" ~ "365.1",
                                                                    Characteristic_ID == "TN" ~ "4500-N-C",
                                                                    Characteristic_ID == "7664-41-7" ~ "350.1",
                                                                    Characteristic_ID == "NN" ~ "353.2",
                                                                    Characteristic_ID == "ORGANIC-C" ~ "415.1" ,
                                                                    Characteristic_ID == "7631-86-9" ~ "370.1",
                                                                    Characteristic_ID == "CHL-A-CP" ~ "10200-H",
                                                                    Characteristic_ID == "TSS" ~ "2540-D",
                                                                    TRUE ~ as.character(Analytical_Method_ID)))
#change ids
edd3$Activity_ID<-sub("WF_RVR_SPB|WF_RVR_SRB", "WF-R-SPB", edd3$Activity_ID)
#fix _
edd3$Activity_ID<-sub("_", "-", edd3$Activity_ID)

unique(edd3$Activity_ID)
unique(edd3$Site_ID)
#Write csv of compiled results
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/HydroShareIntermediateData2025")
write.csv(edd3, "FLBS_BaselineEDD_26-02-24_cr.csv",
          row.names= FALSE)

#Merge NMLN Sites and EDDs
edd4 <- merge(nmln.edds, sites2, by="Activity_ID")



edd4<- edd4 %>% mutate(Sample_Fraction = case_when(Characteristic_ID == "14265-44-2" ~ "Dissolved",
                                                   Characteristic_ID == "TP" ~ "Unfiltered",
                                                   Characteristic_ID == "TN" ~ "Unfiltered",
                                                   Characteristic_ID == "7631-86-9" ~ "Dissolved",
                                                   Characteristic_ID == "NN" ~ "Dissolved",
                                                   Characteristic_ID == "ORGANIC-C" ~ "Unfiltered",
                                                   Characteristic_ID == "7664-41-7" ~ "Dissolved",
                                                   .default = "NA"),
                       Lower_Reporting_Limit_Value = case_when(Characteristic_ID == "14265-44-2" ~ .800,
                                                               Characteristic_ID == "TP" ~ 1.50,
                                                               Characteristic_ID == "TN" ~ 25.00,
                                                               Characteristic_ID == "7664-41-7" ~ 1.50,
                                                               Characteristic_ID == "NN" ~ 1.50,
                                                               Characteristic_ID == "ORGANIC-C" ~ .100 ,
                                                               Characteristic_ID == "7631-86-9" ~ .100,
                                                               Characteristic_ID == "CHL-A-CP" ~ .200,
                                                               TRUE ~ as.numeric(Lower_Reporting_Limit_Value)),
                       Detection_Limit_Unit = case_when(Characteristic_ID == "14265-44-2" ~ "ug/L",
                                                        Characteristic_ID == "TP" ~ "ug/L",
                                                        Characteristic_ID == "TN" ~ "ug/L",
                                                        Characteristic_ID == "7664-41-7" ~ "ug/L",
                                                        Characteristic_ID == "NN" ~ "ug/L",
                                                        Characteristic_ID == "ORGANIC-C" ~ "mg/L" ,
                                                        Characteristic_ID == "7631-86-9" ~ "mg/L",
                                                        Characteristic_ID == "CHL-A-CP" ~ "ug/L",
                                                        TRUE ~ as.character(Detection_Limit_Unit)),
                       Activity_Type = case_when(Characteristic_ID == "14265-44-2" ~ "S-ROUTINE",
                                                 Characteristic_ID == "TP" ~ "S-ROUTINE",
                                                 Characteristic_ID == "TN" ~ "S-ROUTINE",
                                                 Characteristic_ID == "7664-41-7" ~ "S-ROUTINE",
                                                 Characteristic_ID == "NN" ~ "S-ROUTINE",
                                                 Characteristic_ID == "ORGANIC-C" ~ "S-ROUTINE" ,
                                                 Characteristic_ID == "7631-86-9" ~ "S-ROUTINE",
                                                 Characteristic_ID == "CHL-A-CP" ~ "S-ROUTINE",
                                                 Characteristic_ID == "TSS" ~ "S-ROUTINE",
                                                 TRUE ~ "F-MSR/OBS"),
                       Laboratory_Name = case_when(Activity_Type == "S-ROUTINE" ~ "FLBS",
                                                   TRUE ~ as.character(Laboratory_Name)),
                       Result_Detection_Condition = case_when(Result_Value = is.na(Result_Value) ~ "Not Detected",
                                                              TRUE ~ as.character(Result_Detection_Condition)),
                       Method_Speciation_Name = case_when(Characteristic_ID == "ORGANIC-C" ~ "",
                                                          TRUE ~ as.character(Method_Speciation_Name)),
                       
                       Analytical_Method_ID = case_when(Characteristic_ID == "14265-44-2" ~ "365.1",
                                                        Characteristic_ID == "TP" ~ "365.1",
                                                        Characteristic_ID == "TN" ~ "4500-N-C",
                                                        Characteristic_ID == "7664-41-7" ~ "350.1",
                                                        Characteristic_ID == "NN" ~ "353.2",
                                                        Characteristic_ID == "ORGANIC-C" ~ "415.1" ,
                                                        Characteristic_ID == "7631-86-9" ~ "370.1",
                                                        Characteristic_ID == "CHL-A-CP" ~ "10200-H",
                                                        Characteristic_ID == "TSS" ~ "2540-D",
                                                        TRUE ~ as.character(Analytical_Method_ID)))



for(i in 1:length(colnames(edd4))){
  
  list1 <- edd4[, i]
  
  for(j in 1:length(list1)){
    
    if(is.na(list1[j])==TRUE){
      list1[j] <- ""
    }
  }
  
  edd4[, i] <- list1
}


#check ids
unique(edd4$Activity_ID)
unique(edd4$Site_ID)


#Write csv with FLBS NMLN Results
write.csv(edd4, "FLBS_NMLNEDD_26-02-26_cr.csv")






