#script for basline lab QA 2023 CR
rm(list=ls())


library(readxl)
library(stringr)
library(tidyverse)
library(dplyr)
#Load data ####
setwd("C:/Users/User/Dropbox/WLI (2)/CASSIE/ProcessingForHydroShare/HydroShare2025")
data <- data.frame(read_excel("HydroShareIntermediateData2025/WWQMP_Lab_EDD_25-03-03_cr.xlsx", 
                              sheet="Result"))

#Remove first row (column heading information)
data <- data[-1 ,]


#Extract site from Activity ID
for(i in 1:length(data$Data_Logger_Line_ID)){
  end <- unlist(gregexpr("_\\d+-\\d+", data$Activity_ID[i]))
  data$site[i] <- substr(data$Activity_ID[i], 1, end-1)
}

#Extract date from Activity ID
for(i in 1:length(data$Activity_ID)){
  start <- unlist(gregexpr("_\\d+-\\d+", data$Activity_ID[i]))
  end2 <- unlist(gregexpr("_\\d+:\\d+", data$Activity_ID[i]))
  data$date[i] <- substr(data$Activity_ID[i], start+1, end2-1)
}



#Extract date from Activity ID
for(i in 1:length(data$Activity_ID)){
  start <- unlist(gregexpr("_\\d+-\\d+", data$Activity_ID[i]))
  end2 <- unlist(gregexpr("_\NA", data$Activity_ID[i]))
  data$date[i] <- substr(data$Activity_ID[i], start+1, end2-1)
}





#Format dates as POSIX and results as numbers
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")

data$Result_Value <- as.numeric(data$Result_Value)

#splitting lakes and tribs in WWQMP
#changed site to site ID in this code below ####
#AND changed to str_detect so it can do a more general search
unique(data$site)

lakes <- data[which(str_detect(data$Site_ID, "TALLY" )==TRUE | str_detect(data$Site_ID, "WF-LK-IP1")==TRUE | str_detect(data$Site_ID, "WF-LK-IP2") == TRUE) ,]

#edit these names here ####
tribs <-  data[which(str_detect(data$Site_ID, "BEAV" )==TRUE| str_detect(data$Site_ID, "HASK")==TRUE | str_detect(data$Site_ID, "HELLR") == TRUE | str_detect(data$Site_ID, "LAZY") == TRUE
                     | str_detect(data$Site_ID, "SMITH") == TRUE | str_detect(data$Site_ID, "VIKING") == TRUE | str_detect(data$Site_ID, "WALKER") == TRUE | str_detect(data$Site_ID, "COW") == TRUE 
                     | str_detect(data$Site_ID, "SWIFT") == TRUE | str_detect(data$Site_ID, "WF-R") == TRUE), ]


#Streams QA Plots ####

#Create dataframe of symbols for each site for plotting (streams)
#make sure you fit the number of unique sites you have when making dataframe ####
sites <- data.frame(unique(tribs$Site_ID), c(1:12))
colnames(sites)[1] <- "Site_ID"
colnames(sites)[2] <- "pch"

data <- merge(tribs, sites, by="Site_ID")

#Set non-detects equal to zero
data$Result_Value[is.na(data$Result_Value)] <- 0 

#Fix discrepancies in units
data$Result_Value_Unit <- sub("/L", "/l", data$Result_Value_Unit)
data$Result_Value_Unit <- sub("mg/m3", "ug/l", data$Result_Value_Unit) #all ppm
data$Result_Value_Unit <- sub("mg/m2", "ug/l", data$Result_Value_Unit) 

data$Result_Value_Unit <- sub("mg/cu. m", "ug/l", data$Result_Value_Unit)

#Create list of parameters and units
param <- unique(data[c("Characteristic_Name","Result_Value_Unit")])

#Remove NAs (from non-detects)
param <- param[is.na(param$Result_Value_Unit) == FALSE ,]

#Loop for making initial time series plots for streams
for(i in 1:length(param$Characteristic_Name)){
  
  #Write pngs to streams QA folder
  # png(filename= paste0("QAGraphs2022", param$Characteristic_Name[i], ".png"),
  #     width= 500, height= 500)
  
  #Subset data to specific parameter
  data.temp <- data[data$Characteristic_Name == param$Characteristic_Name[i] ,]
  
  #Plot time series
  plot(data.temp$Result_Value ~ data.temp$date,
       main= paste("WWQMP Tributaries", param$Characteristic_Name[i]),
       xlab= "Date",
       ylab= param$Result_Value_Unit[i],
       pch= data.temp$pch)
  
  #Add legend
  legend(bty= "n", "topleft", legend= sites$Site_ID, pch= sites$pch, cex= 0.8)
  
  #Close plot
  #dev.off()
}

#Specific parameter plots fixes

#Make combined plot total phosphorus (names from both labs)
tp1 <- data[data$Characteristic_Name == "Total Phosphorus, mixed forms"
            | data$Characteristic_Name == "Phosphate-phosphorus",]


#Write png to stream QA folder
#png(filename= "QAGraphs2022/LabGraphs/TP_All.png", width= 500, height= 500)

#Plot as time series
plot(tp1$Result_Value ~ tp1$date,
     main="WWQMP Tributaries Total Phosphorus",
     xlab= "Date",
     ylab= "Total P (ug/l)",
     pch= tp1$pch)

#Add legend
legend(bty="n", "topleft", legend=sites$Site_ID, pch=sites$pch, cex=0.8)

#Close plot
#dev.off()

#Make combined plot for total nitrogen (names from both labs)
tn1 <- data[data$Characteristic_Name == "Total nitrogen, mixed forms" 
            | data$Characteristic_Name == "Nutrient-nitrogen" ,]

#Write png to streams QA folder
#png(filename= "QAGraphs2022/LabGraphs//TN_All.png", width= 500, height= 500)

#Plot time series
plot(tn1$Result_Value ~ tn1$date,
     main="WWQMP Tributaries Total Nitrogen",
     xlab= "Date",
     ylab= "TN (ug/l)",
     pch= tn1$pch,
     #ylim=c(0,2000)
)

#Add legend
legend(bty="n", "topleft", legend=sites$Site_ID, pch=sites$pch, cex=0.8)

#Turn off plot
#dev.off()

#Add y limits to nitrate plot to see data more clearly
ni <- data[data$Characteristic_Name == "Inorganic nitrogen (nitrate and nitrite)" ,]

#Write png to streams QA folder
#png(filename= "QAGraphs2022/LabGraphs//Nitrate_Ylimit.png", width= 500, height= 500)

#plot as time series
plot(ni$Result_Value ~ ni$date,
     main="WWQMP Tribuatries Nitrate and Nitrite",
     xlab= "Date",
     ylab= "Nitrate Nitrite (ug/l)",
     pch= ni$pch,
     ylim=c(0,600)
)

#Add legend
legend(bty="n", "topleft", legend=sites$Site_ID, pch=sites$pch, cex=0.8)

#Close plot
#dev.off()

#Lakes QA Plots ####

#Create site list and add symbols for plotting
#again match for unique number you have for this years data ####
sites <- data.frame(unique(lakes$Site_ID), c(1:3))
colnames(sites)[1] <- "Site_ID"
colnames(sites)[2] <- "pch"

#Merge site symbol data with results
data <- merge(lakes, sites, by="Site_ID")

#Set non-detects equal to zero
data$Result_Value[is.na(data$Result_Value)] <- 0 

#Fix discrepancies in units
data$Result_Value_Unit <- sub("/L", "/l", data$Result_Value_Unit)
data$Result_Value_Unit <- sub("mg/m3", "ug/l", data$Result_Value_Unit) #all ppm
data$Result_Value_Unit <- sub("mg/m2", "ug/l", data$Result_Value_Unit) 
data$Result_Value_Unit <- sub("mg/cu. m", "ug/l", data$Result_Value_Unit)

#Create list of parameters and units
param <- unique(data[c("Characteristic_Name","Result_Value_Unit")])

#Remove NAs (from non-detects)
param <- param[is.na(param$Result_Value_Unit) == FALSE ,]

#Loop for making initial time series plots for Whitefish and Tally lakes
for(i in 1:length(param$Characteristic_Name)){
  
  #Write pngs to streams QA folder
  #png(filename= paste0("QAGraphs2022/LabGraphs/TallyWF", param$Characteristic_Name[i], ".png"),
  #width= 500, height= 500)
  
  #Subset data to specific parameter
  data.temp <- data[data$Characteristic_Name == param$Characteristic_Name[i] ,]
  
  #Plot time series
  plot(data.temp$Result_Value ~ data.temp$date,
       main= paste("WWQMP Lakes", param$Characteristic_Name[i]),
       xlab= "Date",
       ylab= param$Result_Value_Unit[i],
       pch= data.temp$pch)
  
  #Add legend
  ifelse(param$Characteristic_Name[i] == "Organic carbon",
         yes= legend(bty= "n", "topright", legend= sites$Site_ID, pch= sites$pch, cex= 0.8),
         no= legend(bty= "n", "topleft", legend= sites$Site_ID, pch= sites$pch, cex= 0.8))
  
  
  #Close plot
  #dev.off()
}

#Specific parameter plot fixes

#Make combined plot total phosphorus (names from both labs)
tp1 <- data[data$Characteristic_Name == "Total Phosphorus, mixed forms"
            | data$Characteristic_Name == "Phosphate-phosphorus",]


#Write png to Tally Whitefish QA folder
#png(filename= "QAGraphs2022/LabGraphs/TP_All.png", width= 500, height= 500)

#Plot as time series
plot(tp1$Result_Value ~ tp1$date,
     main="WWQMP Lakes Total Phosphorus",
     xlab= "Date",
     ylab= "Total P (ug/l)",
     pch= tp1$pch)

#Add legend
legend(bty="n", "topleft", legend=sites$Site_ID, pch=sites$pch, cex=0.8)

#Close plot
#dev.off()


#Make combined plot for total nitrogen (names from both labs)
tn1 <- data[data$Characteristic_Name == "Total nitrogen, mixed forms" 
            | data$Characteristic_Name == "Nutrient-nitrogen" ,]

#Write png to Tally Whitefish QA folder
#png(filename= "QAGraphs2022/LabGraphs/TN_All.png", width= 500, height= 500)

#Plot time series
plot(tn1$Result_Value ~ tn1$date,
     main="WWQMP Lakes Total Nitrogen",
     xlab= "Date",
     ylab= "TN (ug/l)",
     pch= tn1$pch,
     #ylim=c(0,2000)
)

#Add legend
legend(bty="n", "topleft", legend=sites$Site_ID, pch=sites$pch, cex=0.8)

#Turn off plot
#dev.off()

#Adjust Y limits on orthophosphate to see data
op <- data[data$Characteristic_Name == "Orthophosphate"
           & data$Result_Value < 500000 ,]

#Write png to Tally Whitefish QA folder
#png(filename= "QAGraphs2022/LabGraphs//Orthophos_Ylim.png", width= 500, height= 500)

#Plot time series
plot(op$Result_Value ~ op$date,
     main="WWQMP Lakes Orthophospate",
     xlab= "Date",
     ylab= "Orthophosphate (ug/l)",
     pch= op$pch
)

#Add legend
legend(bty="n", "topright", legend=sites$Site_ID, pch=sites$pch, cex=0.8)


#Close plot
#dev.off()






#this is not needed since I commented out the png part of the above code ####
#plots if you want to plot them in R ####
#re run some code from above 
#Load data ####
data <- data.frame(read_excel("HydroShareIntermediateData2022/WWQMP_Lab_EDD_22-02-16_cr.xlsx", 
                              sheet="Result"))

#Remove first row (column heading information)
data <- data[-1 ,]

#Extract site from Activity ID
for(i in 1:length(data$Data_Logger_Line_ID)){
  end <- unlist(gregexpr("_\\d+-\\d+", data$Activity_ID[i]))
  data$site[i] <- substr(data$Activity_ID[i], 1, end-1)
}

#Extract date from Activity ID
for(i in 1:length(data$Activity_ID)){
  start <- unlist(gregexpr("_\\d+-\\d+", data$Activity_ID[i]))
  end2 <- unlist(gregexpr("_\\d+:\\d+", data$Activity_ID[i]))
  data$date[i] <- substr(data$Activity_ID[i], start+1, end2-1)
}

#Format dates as POSIX and results as numbers
data$date <- as.POSIXct(data$date, format="%Y-%m-%d")

data$Result_Value <- as.numeric(data$Result_Value)



#streams first ####
#Create list of parameters and units
param <- unique(data[c("Characteristic_Name","Result_Value_Unit")])

#Remove NAs (from non-detects)
param <- param[is.na(param$Result_Value_Unit) == FALSE ,]
#make a param just with trib parameters
#might have to re-run param from above
param.tribs<- param %>% filter(!str_detect(Characteristic_Name, "Silica")) %>%  filter(!str_detect(Characteristic_Name, "Chlorophyll a, corrected for pheophytin"))
#sites for tribs
sites.tribs <- data.frame(unique(tribs$site), c(1:11))
colnames(sites.tribs)[1] <- "site"
colnames(sites.tribs)[2] <- "pch"
#NOTE GRAPH PCH LABELS AND LEGENDS MAY BE WRONG?? IDK WHY
for(i in 1:length(param.tribs$Characteristic_Name)){
  
  
  #Subset data to specific parameter
  data.temp <- tribs[tribs$Characteristic_Name == param.tribs$Characteristic_Name[i] ,]
  
  #Plot time series
  plot(data.temp$Result_Value ~ data.temp$date,
       main= paste("WWQMP Tributaries", param.tribs$Characteristic_Name[i]),
       xlab= "Date",
       ylab= param.tribs$Result_Value_Unit[i],
       pch= sites.tribs$pch)
  
  ifelse(param.tribs$Characteristic_Name[i] == "Organic carbon",
         yes= legend(bty= "n", "topright", legend= sites.tribs$site, pch= sites.tribs$pch, cex= 0.8),
         no= legend(bty= "n", "topleft", legend= sites.tribs$site, pch= sites.tribs$pch, cex= 0.8))
  
  
  
}


#now for lakes ####
#Create site list and add symbols for plotting
#again match for unique number you have for this years data ####
sites <- data.frame(unique(lakes$site), c(1:3))
colnames(sites)[1] <- "site"
colnames(sites)[2] <- "pch"


for(i in 1:length(param$Characteristic_Name)){
  
  
  #Subset data to specific parameter
  data.temp <- lakes[lakes$Characteristic_Name == param$Characteristic_Name[i] ,]
  
  #Plot time series
  plot(data.temp$Result_Value ~ data.temp$date,
       main= paste("WWQMP Lakes", param$Characteristic_Name[i]),
       xlab= "Date",
       ylab= param$Result_Value_Unit[i],
       pch= sites$pch)
  
  #Add legend
  ifelse(param$Characteristic_Name[i] == "Organic carbon",
         yes= legend(bty= "n", "topright", legend= sites$site, pch= sites$pch, cex= 0.8),
         no= legend(bty= "n", "topleft", legend= sites$site, pch= sites$pch, cex= 0.8))
  
  
  
}
