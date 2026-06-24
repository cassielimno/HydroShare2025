 #fixing an issue from 2024 
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
#testing git
#trend graphs for haskill
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(readr)
library(readxl)
library(openxlsx)

#load data
data <- read_csv("HydroShareFinal2007-2025.csv")


#LDO and temp issue
datafix<- data %>% mutate(Characteristic_ID = case_when((Characteristic_Name == "LDO"   & Characteristic_ID == "TEMP-W")~ "DO",
                                                        .default = data$Characteristic_ID),
                          Characteristic_Name = case_when(Characteristic_Name == "LDO" ~ "Dissolved oxygen (DO)",
                                                          Characteristic_ID == "TEMP-W" ~ "Temperature, water",
                          .default = data$Characteristic_Name))
                      
#test if this worked
unique(datafix$Characteristic_Name)
unique(data$Characteristic_Name)


#see if there are any other issues
unique(data$Characteristic_ID)


#SAVE NEW DATA FRAME
#EXPORT
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
write.csv(datafix, "HydroshareFinal2007-2025.csv")


#other fixes needed NOW FIX THE EDDS
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/WhitefishFinalData")
edd<- data.frame(read_excel("WQX_2024_EDD.xlsx",
                                          sheet= "Result"))


#test if this needs fixing too or if it got fixed in teh WQX upload process
unique(edd$Characteristic_Name)
#looks good its fixed













                          