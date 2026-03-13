rm(list=ls())
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025")
library(readxl)
library(openxlsx)
library(ggplot2)

#before this make sure to check all graphs in the QA/QC script 
#against the QA/QC document to see if any need to be removed

#for 2023 nothing needed to be removed

#Load Data ####
wwqmp <- data.frame(read_excel("HydroShareIntermediateData2025/WWQMP_Lab_EDD_25-03-03_cr.xlsx",
                               sheet= "Result"))
nmln <- data.frame(read_excel("HydroShareIntermediateData2025/NMLN_Lab_EDD_26-02-27_cr.xlsx",
                              sheet= "Result"))



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



#WWQMP QA ####
#See Quality Assurance document for justifications on data removal

# specific QAQC not needed for this ####
# #Remove 1990000 ug/l orthophosphate (only data point removed for WWQMP)
# wwqmp <- wwqmp[-which(wwqmp$Result_Value == "1990000") ,]

#NMLN QA ####

#Remove Data points not collected for NMLN program
remove <- which(nmln$Characteristic_Name == "Iron"
                | nmln$Characteristic_Name == "Manganese"
                | nmln$Characteristic_Name == "Inorganic nitrogen (nitrate and nitrite)"
                | nmln$Characteristic_Name == "Organic carbon"
                | nmln$Characteristic_Name == "Orthophosphate"
                | nmln$Characteristic_Name == "Sulfate"
                | nmln$Characteristic_Name == "Total suspended solids"
                | nmln$Characteristic_Name == "Ammonia-nitrogen")

check <- nmln[remove ,]
#this doesnt run properly ####
#nmln <- nmln[-remove ,]

#specific QAQC not needed #####
# #Remove Murray Lake high nitrogen data point
# nmln <- nmln[-which(nmln$Characteristic_Name == "Total nitrogen, mixed forms"
#                     & nmln$Result_Value == "4580") ,]





#QA/QC for baseline ####
#look at range for each characterisitc to see if it meets standards
wwqmp<- clear.na(wwqmp)
#make a dataframe for lakes
lakes<-wwqmp %>% filter(Site_ID == "WF-LK-IP1" | Site_ID == "WF-LK-IP2" | Site_ID == "TALLY")
lakes$Result_Value<-as.numeric(lakes$Result_Value)
glimpse(lakes)
for( i in unique(lakes$Characteristic_Name)){
  
  onelake<-lakes %>% filter(Characteristic_Name == i)
  
  print(unique(onelake$Characteristic_Name))
  
  print(range(onelake$Result_Value, na.rm = TRUE))
  
  #print(unique(onelake$Characterisitc_Name))
  
  print(median(onelake$Result_Value, na.rm = TRUE))
  
  
}



#all values are within acceptable range and high TN value is from tally so seems fine
#keeping all for 2025 ####


#do again for streams
streams<-wwqmp %>% filter(!(Site_ID == "WF-LK-IP1" | Site_ID == "WF-LK-IP2" | Site_ID == "TALLY"))
streams$Result_Value<-as.numeric(streams$Result_Value)
glimpse(streams)
for( i in unique(streams$Characteristic_Name)){
  
  onestream<-streams %>% filter(Characteristic_Name == i)
  
  print(unique(onestream$Characteristic_Name))
  
  print(range(onestream$Result_Value, na.rm = TRUE))
  
  
}

#all values are within previous ranges found in QA/QC document



#and then for NMLN
nmln$Result_Value<-as.numeric(nmln$Result_Value)
glimpse(nmln)
for( i in unique(nmln$Characteristic_Name)){
  
  onenmln<-nmln %>% filter(Characteristic_Name == i)
  
  print(unique(onenmln$Characteristic_Name))
  
  print(range(onenmln$Result_Value, na.rm = TRUE))
  
  
}



#reporting limit for FLBS chl-a data is .02 FH-somers is 0.03 so keep it#####
#keep all data

#Update Excel Workbook ####

#Load workbooks
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025")
nmln.lab <- loadWorkbook("HydroShareIntermediateData2025/NMLN_Lab_EDD_26-02-27_cr.xlsx")
wwqmp.lab <- loadWorkbook("HydroShareIntermediateData2025/WWQMP_Lab_EDD_25-03-03_cr.xlsx")


#Remove old worksheets
deleteData(wb= nmln.lab, sheet= "Result", cols = 1:100, rows = 1:100000, 
           gridExpand = TRUE)
deleteData(wb= wwqmp.lab, sheet= "Result", cols = 1:100, rows = 1:100000, 
           gridExpand = TRUE)

#Update Results sheets
writeData(wb= nmln.lab, sheet= "Result", x= nmln)
writeData(wb= wwqmp.lab, sheet= "Result", x= wwqmp)

#Write excel workbooks for lab results
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025")
saveWorkbook(nmln.lab, "FinalResults2025/NMLN_Lab_EDD_26-02-27_cr.xlsx",
             overwrite = TRUE)
saveWorkbook(wwqmp.lab, "FinalResults2025/WWQMP_Lab_EDD_25-03-03_cr.xlsx",
             overwrite = TRUE)


