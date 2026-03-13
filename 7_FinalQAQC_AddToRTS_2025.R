#more detailed QA/QC for all data for 2025---- gets done before final hydroshare step
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)          
library(readr)
library(janitor)
library(ggiraph)
library(plotly)
library(readxl)
library(openxlsx)
library(tidyverse)
library(readxl)
library(openxlsx)
library(plyr)
library(stringr)
#library(dplyr)
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/FinalResults2025")
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

#Load Data ####



#hydroshare full data
data<- read.csv("HydroData2025.csv")

data<-clear.na(data)


#turn date to date
data$Activity_Start_Date<- ymd(data$Activity_Start_Date)


#make month year day cols will have to take these out later
data<-data %>% mutate(year = year(Activity_Start_Date), 
                      month = month(Activity_Start_Date), day = yday(Activity_Start_Date))



#test all the varibles ####
unique(data$Characteristic_Name)

glimpse(data)
#four loop to display chem varibles 
#filterfor just chem varibles
target<- c("TN", "TP", "CHL-A-CP",
           "TSS", "ORGANIC-C", "14265-44-2",
           "NN", "7664-41-7", "7631-86-9")

chem<- data %>% filter(Characteristic_ID %in% target, Station_ID == "WF-LK-IP1" | 
                          Station_ID == "WF-LK-IP2")

#make graphs to check data for weird outliers

for(i in unique(chem$Characteristic_Name)){
  
  onechem<- chem %>% filter(Characteristic_Name == i)
  
  hey<- (ggplot(data = onechem)+
           geom_point(aes(y = Result_Value, x = Activity_Start_Date, color = Station_ID))+
           ggtitle(i))
  
  print(ggplotly(hey))
  
}


#test physical values in the same way
#filter for just those
unique(data$Characteristic_ID)


target2<- c("BAR-PRESSURE", "TEMP-W", "DO-SAT", "DO", "SC")

phys<- data %>% filter(Characteristic_ID %in% target2, Station_ID == "WF-LK-IP1" | 
                          Station_ID == "WF-LK-IP2")
phys$year<-as.character(phys$year)

for(i in unique(phys$Characteristic_Name)){
  
  onephys<- phys %>% filter(Characteristic_Name == i)
  
  hey<- (ggplot(data = onephys)+
           geom_point(aes(y = Result_Depth_Height_Measure*-1, 
                          x = Result_Value, color = Station_ID, shape = year))+
           facet_wrap(~Activity_Start_Date)+
           ggtitle(i))
  
  print(ggplotly(hey))
  
}

#pull out parameters that are visual outliers/seem to be off
#some of these are maybe in the air or after the sensor hit the bottom
#or just clear wacky data
#if the sensor was out of the water I just pulled all of the data
#I also pulled data near the bottom where it only moved 1m but was more than 1mg/l off

#yank 'em ####
data2 <- data %>% filter(!(Characteristic_ID == "DO" & Result_Value == 14.14 &
                          Station_ID == "WF-LK-IP1")) %>% 
  filter(!(Characteristic_ID == "DO" & Result_Value == 7.07 &
           Station_ID == "WF-LK-IP2"))
  



#filter out NMLN data that is not good ####
#I found all these points to pull by looking at the NMLN dashboard graphs
nmln<- data2 %>% filter(Project_ID == "NMLN")



target<- c("TN", "TP", "CHL-A-CP",
           "TSS", "ORGANIC-C", "14265-44-2",
           "NN", "7664-41-7", "7631-86-9")

chem.n<- nmln %>% filter(Characteristic_ID %in% target)

#make graphs to check data for weird outliers

for(i in unique(chem.n$Characteristic_Name)){
  
  onechem<- chem.n   %>% filter(Characteristic_Name == i)
  

  hey<- (ggplot(data = onechem)+
           geom_point(aes(y = Result_Value, x = Activity_Start_Date, color = Station_ID))+
           ggtitle(i))
  
  print(ggplotly(hey))
  
}

#test physical values in the same way
#filter for just those
unique(data$Characteristic_ID)


target2<- c("BAR-PRESSURE", "TEMP-W", "DO-SAT", "DO", "SC")



phys.n<- nmln %>% filter(Characteristic_ID %in% target2)
phys.n$year<-as.character(phys.n$year)



  
  for(j in unique(phys.n$Station_ID)) {
    
    onelake<- phys.n %>% filter(Station_ID == j)
    
    hey<- (ggplot(data = onelake)+
             geom_point(aes(y = Result_Depth_Height_Measure*-1, 
                            x = Result_Value, color = Characteristic_Name))+
             facet_wrap(~Characteristic_Name, scales = "free")+
             ggtitle(j))
    
    print(ggplotly(hey))
    
  }

#split data so all the graphs show up

nmln.a<- nmln %>% filter(str_detect(Station_ID, "^A|^B|^C|^D|^E|^F|^G|^H|^I|^J|^K|^L"))


phys.a<- nmln.a %>% filter(Characteristic_ID %in% target2)
phys.a$year<-as.character(phys.a$year)




for(j in unique(phys.a$Station_ID)) {
  
  onelake<- phys.a %>% filter(Station_ID == j)
  
  hey<- (ggplot(data = onelake)+
           geom_point(aes(y = Result_Depth_Height_Measure*-1, 
                          x = Result_Value, color = Characteristic_Name))+
           facet_wrap(~Characteristic_Name, scales = "free")+
           ggtitle(j))
  
  print(ggplotly(hey))
  
}


#same thing for other letters
nmln.a<- nmln %>% filter(str_detect(Station_ID, "^A|^B|^C|^D|^E"))


phys.a<- nmln.a %>% filter(Characteristic_ID %in% target2)
phys.a$year<-as.character(phys.a$year)




for(j in unique(phys.a$Station_ID)) {
  
  onelake<- phys.a %>% filter(Station_ID == j)
  
  hey<- (ggplot(data = onelake)+
           geom_point(aes(y = Result_Depth_Height_Measure*-1, 
                          x = Result_Value, color = Characteristic_Name))+
           facet_wrap(~Characteristic_Name, scales = "free")+
           ggtitle(j))
  
  print(ggplotly(hey))
  
}



data2<-clear.na(data2) #if you don't run this it takes out chem data which has na as a depth
#should take out 22
data3<-data2 %>% filter(!(Station_ID == "ROGERS" & Result_Depth_Height_Measure == 4.69)) %>%
        filter(!(Station_ID == "MURPHY" & Result_Depth_Height_Measure == 8.01)) %>%
        filter(!(Station_ID == "MURRAY" & Result_Depth_Height_Measure == 0)) %>%
        filter(!(Station_ID == "GLEN" & Result_Depth_Height_Measure == 10.79)) %>%
         filter(!(Station_ID == "ASHLEY-E" & Result_Depth_Height_Measure == 44.01))



#check tally lake by itself ####

#four loop to display chem varibles 
#filterfor just chem varibles
target<- c("TN", "TP", "CHL-A-CP",
           "TSS", "ORGANIC-C", "14265-44-2",
           "NN", "7664-41-7", "7631-86-9")

chem<- data %>% filter(Characteristic_ID %in% target, Station_ID == "TALLY")

#make graphs to check data for weird outliers

for(i in unique(chem$Characteristic_Name)){
  
  onechem<- chem %>% filter(Characteristic_Name == i)
  
  hey<- (ggplot(data = onechem)+
           geom_point(aes(y = Result_Value, x = Activity_Start_Date, color = Station_ID))+
           ggtitle(i))
  
  print(ggplotly(hey))
  
}


#test physical values in the same way
#filter for just those
unique(data1$Characteristic_ID)

target2<- c("BAR-PRESSURE", "TEMP-W", "DO-SAT", "DO", "SC")

phys<- data %>% filter(Characteristic_ID %in% target2, Station_ID == "TALLY")
phys$year<-as.character(phys$year)

for(i in unique(phys$Characteristic_Name)){
  
  onephys<- phys %>% filter(Characteristic_Name == i)
  
  hey<- (ggplot(data = onephys)+
           geom_point(aes(y = Result_Depth_Height_Measure*-1, 
                          x = Result_Value, color = Station_ID, shape = year))+
           facet_wrap(~Activity_Start_Date)+
           ggtitle(i))
  
  print(ggplotly(hey))
  
}


#yank bad data from tally lake

#take out of main dataframe

#should take out 10
data4<- data3 %>% filter(!(Station_ID == "TALLY" & Activity_Start_Date == "2025-05-07"
                         & Result_Depth_Height_Measure == 0.26)) %>% #in air i think
                filter(!(Station_ID == "TALLY" & Activity_Start_Date == "2025-06-10" &
                         Result_Depth_Height_Measure == 0.99)) #in air i think


#Tributaries QAQC ####
#four loop to display chem varibles 
#filterfor just chem varibles
target<- c("TN", "TP", "CHL-A-CP",
           "TSS", "ORGANIC-C", "14265-44-2",
           "NN", "7664-41-7", "7631-86-9")

chem<- data %>% filter(Characteristic_ID %in% target, Station_Type == "River/Stream")

#make graphs to check data for weird outliers

for(i in unique(chem$Characteristic_Name)){
  
  onechem<- chem %>% filter(Characteristic_Name == i)
  
  hey<- (ggplot(data = onechem)+
           geom_point(aes(y = Result_Value, x = Activity_Start_Date, color = Station_ID))+
           facet_wrap(~Station_ID, scales = "free")+
           ggtitle(i))
  
  print(ggplotly(hey))
  
}


#test physical values in the same way
#filter for just those
unique(data1$Characteristic_ID)

target2<- c("BAR-PRESSURE", "TEMP-W", "DO-SAT", "DO", "SC")

phys<- data %>% filter(Characteristic_ID %in% target2, Station_Type == "River/Stream")
phys$year<-as.character(phys$year)

for(i in unique(phys$Characteristic_Name)){
  
  onephys<- phys %>% filter(Characteristic_Name == i)
  
  hey<- (ggplot(data = onephys)+
           geom_point(aes(y = Result_Value, 
                          x = Activity_Start_Date, color = Station_ID))+
           facet_wrap(~Station_ID, scales = "free")+
           ggtitle(i))
  
  print(ggplotly(hey))
  
}

#yank any bad data
# I found some high values but there was nothing in the field notes about
#sampling issues and some were associated with rain
#they also passed the first QAQC which means they are realistic values 
#so I am leaving everything in

data4$Result_Value<- as.numeric(data4$Result_Value)

data5<- data4 %>% mutate(Result_Comment = case_when(str_detect(Activity_ID, "WALKER-CRK-MR_2025-09-22") ~ "Water was stirred up sediment in sample",
                                                   str_detect(Activity_ID, "COW-CRK-PA_2025-05-07") ~ "Ducks stirred up water",
                                                   .default = as.character(Result_Comment))) %>% #add notes on data
  filter(!(Station_ID == "COW-CRK-PA" & Activity_Start_Date == "2025-09-22" 
                              & Characteristic_ID == "BAR-PRESSURE")) %>% 
  filter(!(Station_ID == "COW-CRK-PA" & Activity_Start_Date == "2025-09-22" #pull these bar pressure was messed up
         & Characteristic_ID == "DO-SAT")) %>% 
  filter(!(Station_ID == "COW-CRK-PA" & Activity_Start_Date == "2025-09-22"
           & Characteristic_ID == "DO")) %>% 
  filter(!(Station_ID == "SWIFT-CRK-OLN" & Activity_Start_Date == "2025-09-22" &
         Characteristic_ID == "TEMP-W")) #take out temp that is c


#convert temp to f
test4<- data4 %>% filter(Station_ID == "SWIFT-CRK-OLN", Activity_Start_Date == "2025-09-22",
                         Characteristic_ID == "TEMP-W") %>% 
  mutate(Result_Value = (Result_Value*9/5)+32)

#rbind back together
data6<- rbind(data5, test4)



#now export for DEQ data base
datafinal<-data6 %>% select(-year, -day, -month)
#so you know where it goes
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")

#export data
write.csv(datafinal, "HydroshareFinal2025.csv")


#RBIND ON THE RTS DATAFRAME ####
longdata<- read.csv("HydroshareFinalData2007-2024_cr12726.csv")

longdata<- longdata %>% select(-X.1, -X)
glimpse(longdata)
glimpse(datafinal)

datafinal<-datafinal %>% select(-Activity_ID)
#rbind them
alldata<- rbind(longdata, datafinal)

#EXPORT
write.csv(alldata, "HydroshareFinal2007-2025.csv")

#NEXT STEP
#make an EDD worksheet from this results csv
#OR copy and past ones made previously into one big EDD
#also get key then check data 
#then submit through email with SAP for NMLN and baseline and pdf filled out


#DOING SAME THING WITH EDD ######
#upload data
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")


data <- data.frame(read_excel("WQXFinal2022-2024_EDD.xlsx",
                              sheet= "Result"))

glimpse(data)



#MAKE A JUMBO 2025 EDD 
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/PreFinalData2025")

field.flbs <- data.frame(read_excel("WWQMP_Field_EDD_25_cr.xlsx",
                              sheet= "Result"))

field.flbs<-field.flbs[,1:36]

glimpse(field.flbs)

field.flbs$Analysis_Start_Date<- ymd(field.flbs$Analysis_Start_Date)

lab.flbs<-  data.frame(read_excel("WWQMP_Lab_EDD_25-03-03_cr.xlsx",
                                  sheet= "Result"))
glimpse(lab.flbs)
lab.flbs<- lab.flbs[,1:36]

lab.flbs$Analysis_Start_Date<- mdy(lab.flbs$Analysis_Start_Date)

field.nmln<- data.frame(read_excel("NMLN_Field_EDD_25_cr.xlsx",
                                   sheet= "Result"))

field.nmln<- field.nmln[,1:36]

field.nmln$Analysis_Start_Date<- ymd(field.nmln$Analysis_Start_Date)

lab.nmln <- data.frame(read_excel("NMLN_Lab_EDD_26-02-27_cr.xlsx",
                                  sheet= "Result")) 

lab.nmln$Analysis_Start_Date<- mdy(lab.nmln$Analysis_Start_Date)


lab.nmln<- lab.nmln[,1:36]

#pull acivity as well
field.flbs.a <- data.frame(read_excel("WWQMP_Field_EDD_25_cr.xlsx",
                                    sheet= "Activity"))

lab.flbs.a<-  data.frame(read_excel("WWQMP_Lab_EDD_25-03-03_cr.xlsx",
                                  sheet= "Activity"))


field.nmln.a<- data.frame(read_excel("NMLN_Field_EDD_25_cr.xlsx",
                                   sheet= "Activity"))

lab.nmln.a <- data.frame(read_excel("NMLN_Lab_EDD_26-02-27_cr.xlsx",
                                  sheet= "Activity"))

#rbind both

activity<- rbind(field.flbs.a, lab.flbs.a, field.nmln.a, lab.nmln.a)

result<- rbind(field.flbs, lab.flbs, lab.nmln, field.nmln)

glimpse(result)
#UPLOAD RESULTS AND ACTIVITY OF ALLL EDDS THEN RBIND THEN MAKE A FULL EDD 
#PULL DATA THEN EXPORT ONE BACK INTO A FULL EDD THEN STORE.


#pull out data points that need to be removed -- were already removed in other dataframe

#pull wf
result2 <- result %>% filter(!(Characteristic_ID == "DO" & Result_Value == 14.14   )) %>% #wf-lk-ip1
  filter(!(Characteristic_ID == "DO" & Result_Value == 7.07 & Analysis_Start_Date == "2025-06-17")) #wf-lk-ip2


glimpse(result2)

result2$Result_Depth_Height_Measure<- as.numeric(result2$Result_Depth_Height_Measure)

#pull out nmln should take out 22
result3<-result2 %>% filter(!(Activity_ID == "ROGERS_2025-07-09_11:12:45_F")) %>% #rogers depth = 4.69
  filter(!(Result_Depth_Height_Measure == 8.01 & Analysis_Start_Date == "2025-07-21")) %>% #murphy
  filter(!(Activity_ID == "MURRAY_2025-07-31_13:07:00_F")) %>% #murray #temp and do 07/31 depth 0
  filter(!(Activity_ID == "GLEN_2025-07-21_10:57:19_F")) %>% #glen depth 10.79
  filter(!(Activity_ID == "ASHLEY-E_2025-08-06_10:56:35_F")) #ashley-e depth 44.01




#pulll out tally ####
#should take out 10
result4<- result3 %>% filter(!(Activity_ID == "TALLY_2025-05-07_11:35:08_F" #2025-05-07 depth 0.26
                           )) %>% #in air i think tally lake
  filter(!( Activity_ID == "TALLY_2025-06-10_11:09:00_F")) #in air i think tally lake, depth 0.99, 6-10 date




#stream pulls ####
data4$Result_Value<- as.numeric(data4$Result_Value)

result5<- result4 %>% mutate(Result_Comment = case_when(str_detect(Activity_ID, "WALKER-CRK-MR_2025-09-22") ~ "Water was stirred up sediment in sample",
                                                    str_detect(Activity_ID, "COW-CRK-PA_2025-05-07") ~ "Ducks stirred up water",
                                                    .default = as.character(Result_Comment))) %>% #add notes on data
  #filter(( Analysis_Start_Date == "2025-09-22" 
           #& Characteristic_ID == "BAR-PRESSURE")) #%>% #cow
  #filter((Analysis_Start_Date == "2025-09-22" #pull these bar pressure was messed up
          # & Characteristic_ID == "DO-SAT")) #%>% #cow
  #filter((Analysis_Start_Date == "2025-09-22"
          # & Characteristic_ID == "DO")) #%>% #cow
  filter((Analysis_Start_Date == "2025-09-22" &
             Characteristic_ID == "TEMP-W")) #take out temp that is c swift

#takes out 4
result5<- result4 %>% mutate(Result_Comment = case_when(str_detect(Activity_ID, "WALKER-CRK-MR_2025-09-22") ~ "Water was stirred up sediment in sample",
                                                        str_detect(Activity_ID, "COW-CRK-PA_2025-05-07") ~ "Ducks stirred up water",
                                                        .default = as.character(Result_Comment))) %>% #add notes on data
  filter(!(Activity_ID == "COW-CRK-PA_2025-09-22_15:39:50_F" 
           & Characteristic_ID == "BAR-PRESSURE")) %>% #cow creek 9/22
  filter(!(Activity_ID == "COW-CRK-PA_2025-09-22_15:39:50_F"  #pull these bar pressure was messed up
           & Characteristic_ID == "DO-SAT")) %>% 
  filter(!(Activity_ID == "COW-CRK-PA_2025-09-22_15:39:50_F" 
           & Characteristic_ID == "DO")) %>% 
  filter(!(Activity_ID == "SWIFT-CRK-OLN_2025-09-22_10:10:32_F" & 
             Characteristic_ID == "TEMP-W")) #take out temp that is c


#convert temp to f
result4$Result_Value<-as.numeric(result4$Result_Value)


test4<- result4 %>% filter(Activity_ID == "SWIFT-CRK-OLN_2025-09-22_10:10:32_F", 
                         Characteristic_ID == "TEMP-W") %>% 
  mutate(Result_Value = (Result_Value*9/5)+32)

#rbind back together
result6<- rbind(result5, test4)

#take out turb and secchi
#takes out 98 total
result7<- result6 %>% filter(!(Characteristic_ID == "TURB")) %>% #SHOULDke out 84
                filter(!(Characteristic_ID == "DEPTH-SECCHI")) #TAKES OUT 14

#NEXT STEP ####
#NOW PUT IT ALL BACK INTO EDD/MAKE A JUMMBO EDD TO PUT IT INTO


#offload deannas with new edits ####\
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025/FinalResults2025")
edd.d <- loadWorkbook("WQX_2025_EDD.xlsx")


#Fill results worksheet with results
writeData(wb= edd.d, sheet= "Result", x= result7,   keepNA = openxlsx_getOp("keepNA", FALSE))

#acticvity
writeData(wb= edd.d, sheet= "Activity", x= activity,   keepNA = openxlsx_getOp("keepNA", FALSE))

#Write final excel spreadsheet for NMLN field data
saveWorkbook(wb= edd.d, "WQX_2025_EDD.xlsx",
             overwrite = TRUE)















