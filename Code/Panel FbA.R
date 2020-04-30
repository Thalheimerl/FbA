# # #         Forecast-based Action and Disaster Displacement                 # # # 
# # #                    (Thalheimer et al., 2012)                            # # # 
# # #                                                                         # # # 
# # #                           Panel Data                                    # # #
# # #           This code merges displacement, priority needs,                # # # 
# # #                      and climate variables                              # # # 


library(dplyr)
library(readxl)
library(lubridate)
library(reshape2)
library(stringr)

rm(list = ls())
select <- dplyr::select


user <- "Lisa" 
if (user %in% c("Lisa")){setwd("/Users/lisathalheimer/OneDrive - Nexus365/RCCC/FbA")}


# UNHCR displacement and priority needs data -------------------------------

# UNHCR Data --------------------------------------------------------------

df <- read_excel("Data/UNHCR-PRMN-Displacement-Dataset.xlsx") %>% janitor::clean_names()

# df %>% 
#   group_by(month_end,current_arrival_region,previous_departure_region,reason) %>% 
#   summarise(people = sum(number_of_individuals)) %>% 
#   ungroup() %>% 
#   group_by(month_end,current_arrival_region,previous_departure_region) %>% 
#   summarise(people = sum(people)) %>% 
#   ungroup() %>% 
#   mutate(date = as.Date(month_end,format="%Y-%m-%d")) -> df

df <- read_excel("Data/UNHCR-PRMN-Displacement-Dataset.xlsx")
df$`Current (Arrival) District` <- NULL
df$`Previous (Departure) District` <- NULL
df$`Year Week` <- NULL


# Now we sum over the priority needs 
df <- aggregate(df$`Number of Individuals`,by=list(df$`Month End`,df$`Current (Arrival) Region`,df$`Previous (Departure) Region`,df$Reason,df$`Current (Arrival) Priority Need`),sum)
names(df) <- c("date","arrival_region","departure_region","reason","needs","people")

df <- df[order(df$date,df$arrival_region,df$departure_region),] # just sorting

df$date <- as.Date(df$date, format="%Y-%m-%d") # get rid of the time zone
df$date <- str_sub(df$date, start = 1L, end = 8L)
df$date <- paste0(df$date,"01")
df$date <- as.Date(df$date,format="%Y-%m-%d") 



##########################
# CLIMATE data

climate <- read_excel("Data/Adm1_climate_data.xlsx") #%>% janitor::clean_names()
climate$date <- as.Date(paste0(climate$year,"-",climate$month,"-01"),format = "%Y-%m-%d")
climate$year <- NULL
climate$month <- NULL

climate %>% 
  rename(arrival_region = admin1Name) %>% 
  full_join(df, climate, by=c("arrival_region","date")) -> df


# write it out ------------------------------------------------------------

write.csv(df, file = "Data/FbA_panel_dataset_disaggregated2.csv",row.names = F)


# # Conflict Data -----------------------------------------------------------
# # We first add conflict to the arrival region ------------------------
# conflict <- read.csv("Data/acled_fatalities_year_month_region_actor1_actor2.csv",stringsAsFactors = F)
# 
# ##### Rename some regions
# # #Banaadir should be Banadir
# # #Galguduud should be Galgaduud
# # #Hiiraan should be Hiraan
# # #Jubbada Dhexe should be Middle Juba
# # #Jubbada Hoose should be Lower Juba
# # #Shabeellaha Dhexe should be Middle Shabelle
# # #Shabeellaha Hoose should be Lower Shabelle
# 
# conflict$region[conflict$region=="Banaadir"] <- "Banadir"
# conflict$region[conflict$region=="Galguduud"] <- "Galgaduud"
# conflict$region[conflict$region=="Hiiraan"] <- "Hiraan"
# conflict$region[conflict$region=="Jubbada Dhexe"] <- "Middle Juba"
# conflict$region[conflict$region=="Jubbada Hoose"] <- "Lower Juba"
# conflict$region[conflict$region=="Shabeellaha Dhexe"] <- "Middle Shabelle"
# conflict$region[conflict$region=="Shabeellaha Hoose"] <- "Lower Shabelle"
# 
# conflict <- conflict[,c("year","month","region","fatalities_count")]
# conflict <- aggregate(conflict$fatalities_count,by=list(conflict$year,conflict$month,conflict$region),sum)
# names(conflict) <- c("year","month","region","conflictevents_total")
# 
# 
# # # Recode fatalities and drop duplicates! # doesn't look like we have any duplicates
# # conflict <- conflict2 %>% 
# #   group_by(region, year, month) %>% # Group these many rows into 1 coherent group
# #   mutate(conflictevents_total = sum(fatalities_count, na.rm=T)) %>% # Count conflict events within region_year_mnths
# #   select(-fatalities_count)%>% # remove old variable
# #   distinct() # drop duplicates
# 
# conflict$date <- as.Date(paste0(conflict$year,"-",conflict$month,"-01"),format = "%Y-%m-%d")
# conflict$year <- NULL
# conflict$month <- NULL
# 
# names(conflict)[1:2] <- paste0("arrival_",names(conflict)[1:2])
# df <- merge(df,conflict,by = c("arrival_region","date"),all = T)
# 
# # Now we add the data to the origin region ------------------------
# conflict <- read.csv("Data/acled_fatalities_year_month_region_actor1_actor2.csv",stringsAsFactors = F)
# 
# ##### Rename some regions
# # Banaadir should be Banadir
# # Galguduud should be Galgaduud
# # Hiiraan should be Hiraan
# # Jubbada Dhexe should be Middle Juba
# # Jubbada Hoose should be Lower Juba
# # Shabeellaha Dhexe should be Middle Shabelle
# # Shabeellaha Hoose should be Lower Shabelle
# 
# conflict$region[conflict$region=="Banaadir"] <- "Banadir"
# conflict$region[conflict$region=="Galguduud"] <- "Galgaduud"
# conflict$region[conflict$region=="Hiiraan"] <- "Hiraan"
# conflict$region[conflict$region=="Jubbada Dhexe"] <- "Middle Juba"
# conflict$region[conflict$region=="Jubbada Hoose"] <- "Lower Juba"
# conflict$region[conflict$region=="Shabeellaha Dhexe"] <- "Middle Shabelle"
# conflict$region[conflict$region=="Shabeellaha Hoose"] <- "Lower Shabelle"
# 
# 
# conflict <- conflict[,c("year","month","region","fatalities_count")]
# conflict <- aggregate(conflict$fatalities_count,by=list(conflict$year,conflict$month,conflict$region),sum)
# names(conflict) <- c("year","month","region","conflictevents_total")
# 
# conflict$date <- as.Date(paste0(conflict$year,"-",conflict$month,"-01"),format = "%Y-%m-%d")
# conflict$year <- NULL
# conflict$month <- NULL
# 
# names(conflict)[1:2] <- paste0("departure_",names(conflict)[1:2])
# df <- merge(df,conflict,by = c("departure_region","date"),all = T)


