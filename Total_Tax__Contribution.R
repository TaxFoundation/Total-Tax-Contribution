###Total Tax Contribution###

#Clean up working environment####
rm(list=ls())
gc()

#Directory Variables####
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#source_data<-"C:/Users/acer/Documents/GitHub/Total Tax Contribution/source_data/"
#final_outputs<-"C:/Users/acer/Documents/GitHub/Total Tax Contribution/final_outputs/"
setwd("C:/Users/acer/Documents/GitHub/Total Tax Contribution/")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Define Using function####
using<-function(...,prompt=TRUE){
  libs<-sapply(substitute(list(...))[-1],deparse)
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  installAndRequire<-function(){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages count not be found: ",libsmsg,"n\r\n\rInstall missing packages?",collapse="")
    if(prompt==FALSE){
      installAndRequire()
    }else if(winDialog(type=c("yesno"),libsmsg)=="YES"){
      installAndRequire()
    }
  }
}


using(gtools)
using(readxl)
using(rvest)
using(htmltab)
using(stringr)
using(naniar)
using(OECD)
using(plyr)
using(dplyr)
using(reshape2)
using(countrycode)
using(tidyverse)
using(stringr)
#using(IMFData)
using(readr)
using(scales)


#ISO-Codes####

#Read in iso-codes
country_iso_cont <- read.csv(paste("country_codes.csv",sep=""))

#Keep and rename selected columns
country_iso_cont <- subset(country_iso_cont, select = c(official_name_en, ISO3166.1.Alpha.2, ISO3166.1.Alpha.3, Continent))

colnames(country_iso_cont)[colnames(country_iso_cont)=="official_name_en"] <- "country"
colnames(country_iso_cont)[colnames(country_iso_cont)=="ISO3166.1.Alpha.2"] <- "iso_2"
colnames(country_iso_cont)[colnames(country_iso_cont)=="ISO3166.1.Alpha.3"] <- "iso_3"
colnames(country_iso_cont)[colnames(country_iso_cont)=="Continent"] <- "continent"


#Replace continent abbreviation 'NA' (North America) to 'NO' (R does not recognize 'NA' as a character)
country_iso_cont$continent <- as.character(country_iso_cont$continent)
country_iso_cont$continent <- if_else(is.na(country_iso_cont$continent),"NO", country_iso_cont$continent)
country_iso_cont$continent <- ifelse(country_iso_cont$iso_3 == "TUR","EU",country_iso_cont$continent)


#Add the jurisdictions Netherland Antilles (was split into different jurisdictions in 2010) and Kosovo (has not yet officially been assigned a country code)
country_iso_cont$country <- as.character(country_iso_cont$country)
country_iso_cont$iso_2 <- as.character(country_iso_cont$iso_2)
country_iso_cont$iso_3 <- as.character(country_iso_cont$iso_3)

country_iso_cont[nrow(country_iso_cont) + 1 ,] = list("Kosovo, Republic of", "XK", "XKX", "EU")
country_iso_cont[nrow(country_iso_cont) + 1 ,] = list("Netherlands Antilles", "AN", "ANT", "NO")

#Drop the jurisdiction "Sark" (the island is fiscally autonomous but has no company registry, no company law, and also no ISO-code)
country_iso_cont <- subset(country_iso_cont, country_iso_cont$country != "Sark")

#Correct country names that were read in incorrectly (mostly because R does not recognize accents)

country_iso_cont$country <- as.character(country_iso_cont$country)

country_iso_cont$country[country_iso_cont$iso_3 == "ALA"] <- "Aland Islands"

country_iso_cont$country[country_iso_cont$iso_3 == "CIV"] <- "Cote d'Ivoire"

country_iso_cont$country[country_iso_cont$iso_3 == "CUW"] <- "Curacao"

country_iso_cont$country[country_iso_cont$iso_3 == "REU"] <- "Reunion" 

country_iso_cont$country[country_iso_cont$iso_3 == "BLM"] <- "Saint Barthelemy"

country_iso_cont$country[country_iso_cont$iso_3 == "TWN"] <- "Taiwan"

#New dataframe that includes columns that define if a country belongs to a certain country group

country_iso_cont_groups <- country_iso_cont

country_iso_cont_groups$oecd <- ifelse(country_iso_cont$iso_3 == "AUS"
                                       | country_iso_cont$iso_3 == "AUT"
                                       | country_iso_cont$iso_3 == "BEL"
                                       | country_iso_cont$iso_3 == "CAN"
                                       | country_iso_cont$iso_3 == "CHL"
                                       | country_iso_cont$iso_3 == "COL"
                                       | country_iso_cont$iso_3 == "CRI"
                                       | country_iso_cont$iso_3 == "CZE"
                                       | country_iso_cont$iso_3 == "DNK"
                                       | country_iso_cont$iso_3 == "EST"
                                       | country_iso_cont$iso_3 == "FIN"
                                       | country_iso_cont$iso_3 == "FRA"
                                       | country_iso_cont$iso_3 == "DEU"
                                       | country_iso_cont$iso_3 == "GRC"
                                       | country_iso_cont$iso_3 == "HUN"
                                       | country_iso_cont$iso_3 == "ISL"
                                       | country_iso_cont$iso_3 == "IRL"
                                       | country_iso_cont$iso_3 == "ISR"
                                       | country_iso_cont$iso_3 == "ITA"
                                       | country_iso_cont$iso_3 == "JPN"
                                       | country_iso_cont$iso_3 == "KOR"
                                       | country_iso_cont$iso_3 == "LTU"
                                       | country_iso_cont$iso_3 == "LUX"
                                       | country_iso_cont$iso_3 == "LVA"
                                       | country_iso_cont$iso_3 == "MEX"
                                       | country_iso_cont$iso_3 == "NLD"
                                       | country_iso_cont$iso_3 == "NZL"
                                       | country_iso_cont$iso_3 == "NOR"
                                       | country_iso_cont$iso_3 == "POL"
                                       | country_iso_cont$iso_3 == "PRT"
                                       | country_iso_cont$iso_3 == "SVK"
                                       | country_iso_cont$iso_3 == "SVN"
                                       | country_iso_cont$iso_3 == "ESP"
                                       | country_iso_cont$iso_3 == "SWE"
                                       | country_iso_cont$iso_3 == "CHE"
                                       | country_iso_cont$iso_3 == "TUR"
                                       | country_iso_cont$iso_3 == "GBR"
                                       | country_iso_cont$iso_3 == "USA"
                                       ,1,0)

country_iso_cont_groups$eu27 <- ifelse(country_iso_cont$iso_3 == "AUT"
                                       | country_iso_cont$iso_3 == "BEL"
                                       | country_iso_cont$iso_3 == "BGR"
                                       | country_iso_cont$iso_3 == "CZE"
                                       | country_iso_cont$iso_3 == "CYP"
                                       | country_iso_cont$iso_3 == "DNK"
                                       | country_iso_cont$iso_3 == "EST"
                                       | country_iso_cont$iso_3 == "FIN"
                                       | country_iso_cont$iso_3 == "FRA"
                                       | country_iso_cont$iso_3 == "DEU"
                                       | country_iso_cont$iso_3 == "GRC"
                                       | country_iso_cont$iso_3 == "HUN"
                                       | country_iso_cont$iso_3 == "HRV"
                                       | country_iso_cont$iso_3 == "IRL"
                                       | country_iso_cont$iso_3 == "ITA"
                                       | country_iso_cont$iso_3 == "LTU"
                                       | country_iso_cont$iso_3 == "LUX"
                                       | country_iso_cont$iso_3 == "LVA"
                                       | country_iso_cont$iso_3 == "MLT"
                                       | country_iso_cont$iso_3 == "NLD"
                                       | country_iso_cont$iso_3 == "POL"
                                       | country_iso_cont$iso_3 == "PRT"
                                       | country_iso_cont$iso_3 == "ROU"
                                       | country_iso_cont$iso_3 == "SVK"
                                       | country_iso_cont$iso_3 == "SVN"
                                       | country_iso_cont$iso_3 == "ESP"
                                       | country_iso_cont$iso_3 == "SWE"
                                       ,1,0)
country_iso_cont_groups$oecd_Europe <- ifelse(country_iso_cont$iso_3 == "AUS"
                                       | country_iso_cont$iso_3 == "AUT"
                                       | country_iso_cont$iso_3 == "BEL"
                                       | country_iso_cont$iso_3 == "BGR"
                                       | country_iso_cont$iso_3 == "CAN"
                                       | country_iso_cont$iso_3 == "CHL"
                                       | country_iso_cont$iso_3 == "COL"
                                       | country_iso_cont$iso_3 == "CRI"
                                       | country_iso_cont$iso_3 == "CYP"
                                       | country_iso_cont$iso_3 == "CZE"
                                       | country_iso_cont$iso_3 == "DNK"
                                       | country_iso_cont$iso_3 == "EST"
                                       | country_iso_cont$iso_3 == "FIN"
                                       | country_iso_cont$iso_3 == "FRA"
                                       | country_iso_cont$iso_3 == "DEU"
                                       | country_iso_cont$iso_3 == "GRC"
                                       | country_iso_cont$iso_3 == "HUN"
                                       | country_iso_cont$iso_3 == "HRV"
                                       | country_iso_cont$iso_3 == "ISL"
                                       | country_iso_cont$iso_3 == "IRL"
                                       | country_iso_cont$iso_3 == "ISR"
                                       | country_iso_cont$iso_3 == "ITA"
                                       | country_iso_cont$iso_3 == "JPN"
                                       | country_iso_cont$iso_3 == "KOR"
                                       | country_iso_cont$iso_3 == "LTU"
                                       | country_iso_cont$iso_3 == "LUX"
                                       | country_iso_cont$iso_3 == "LVA"
                                       | country_iso_cont$iso_3 == "MLT"
                                       | country_iso_cont$iso_3 == "MEX"
                                       | country_iso_cont$iso_3 == "NLD"
                                       | country_iso_cont$iso_3 == "NZL"
                                       | country_iso_cont$iso_3 == "NOR"
                                       | country_iso_cont$iso_3 == "POL"
                                       | country_iso_cont$iso_3 == "PRT"
                                       | country_iso_cont$iso_3 == "ROU"
                                       | country_iso_cont$iso_3 == "SVK"
                                       | country_iso_cont$iso_3 == "SVN"
                                       | country_iso_cont$iso_3 == "ESP"
                                       | country_iso_cont$iso_3 == "SWE"
                                       | country_iso_cont$iso_3 == "CHE"
                                       | country_iso_cont$iso_3 == "TUR"
                                       | country_iso_cont$iso_3 == "GBR"
                                       | country_iso_cont$iso_3 == "USA"
                                       ,1,0)

country_iso_cont_groups$Europe <- ifelse(country_iso_cont$continent == "EU",1,0)
country_iso_cont_groups$NorthAmerica <- ifelse(country_iso_cont$continent == "NO",1,0)
country_iso_cont_groups$SouthAmerica <- ifelse(country_iso_cont$continent == "SA",1,0)

country_iso_cont_groups$Africa <- ifelse(country_iso_cont$continent == "AF",1,0)

country_iso_cont_groups$Asia <- ifelse(country_iso_cont$continent == "AS",1,0)
country_iso_cont_groups$Oceania <- ifelse(country_iso_cont$continent == "OC",1,0)


#OECD Data: OECD Countries national currency####

#Read 2023 OECD countries data expect for AUS, GRE, POL and JAP (missing or incomplete). 

url = "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_OECD@DF_RSOECD,/BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+HUN+ISL+IRL+ISR+ITA+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+OECD_REP+FEDOECD+UNIOECD+AUT..S13.T_1000+T_1100+T_1110+T_1120+T_1200+T_1210+T_1220+T_1300+T_2000+T_2100+T_2110+T_2120+T_2200+T_2210+T_2220+T_2300+T_2310+T_2320+T_2400+T_2410+T_2420+T_3000+T_4000+T_4100+T_4110+T_4120+T_4200+T_4210+T_4220+T_4300+T_4310+T_4320+T_4400+T_4500+T_4510+T_4520+T_4600+T_5000+T_5100+T_5110+T_5111+T_5112+T_5113+T_5120+T_5121+T_5122+T_5123+T_5124+T_5125+T_5126+T_5127+T_5128+T_5130+T_5200+T_5210+T_5211+T_5212+T_5213+T_5220+T_5300+T_6000+T_6100+T_6200+T_AA+T_AB+T_AC+T_AD+T_AE+T_AF+T_AG+T_AH+T_AI+T_AJ+T_AK+T_CUS+T_SRF+T_NWTOT+T_NWEXP+T_NWTRAN+T_SPLIT+T_GROSS+T_NET+_T..XDC.A?startPeriod=2023&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"

oecd_data_national_currency<-read.csv(url)

#Keep selected columns
oecd_data_national_currency<-oecd_data_national_currency[c(5,6,11,21,25,26,27,31)]

#Calculate value according to UNIT_MULT
oecd_data_national_currency$value<- oecd_data_national_currency$OBS_VALUE*10^oecd_data_national_currency$UNIT_MULT

#Keep selected columns and rename columns
oecd_data_national_currency<-oecd_data_national_currency[c(1,2,7,9)]

#Convert from long to wide format
oecd_data_national_currency_wide <- spread (oecd_data_national_currency, key = REVENUE_CODE, value = value)

#Rename columns
colnames(oecd_data_national_currency_wide)[colnames(oecd_data_national_currency_wide)=="Reference.area"] <- "country"
colnames(oecd_data_national_currency_wide)[colnames(oecd_data_national_currency_wide)=="REF_AREA"] <- "iso_3"

#Merge 'oecd_data_national_currency' dataset with country groups
oecd_data_national_currency_wide <- merge(oecd_data_national_currency_wide, country_iso_cont_groups, by= "iso_3")

#Delete redundant columns
oecd_data_national_currency_wide <- oecd_data_national_currency_wide[,-c(66,67,68,69,70,71,72,73,74,75,76,78,79,80,81,83,84,85,86)]
colnames(oecd_data_national_currency_wide)[colnames(oecd_data_national_currency_wide)=="country.x"] <- "country"


#Read 2022 data for Non-OECD ( BGR,HRV, MLT, ROU) countries and AUS + GRC + POL+ JPN for which 2023 data is incomplete or missing.

url = "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,DSD_REV_COMP_GLOBAL@DF_RSGLOBAL,1.1/AUS+GRC+POL+JPN+BGR+HRV+MLT+ROU..S13.T_1000+T_1100+T_1110+T_1120+T_1200+T_1210+T_1220+T_1300+T_2000+T_2100+T_2110+T_2120+T_2200+T_2210+T_2220+T_2300+T_2310+T_2320+T_2400+T_2410+T_2420+T_3000+T_4000+T_4100+T_4110+T_4120+T_4200+T_4210+T_4220+T_4300+T_4310+T_4320+T_4400+T_4500+T_4510+T_4520+T_4600+T_5000+T_5100+T_5110+T_5111+T_5112+T_5113+T_5120+T_5121+T_5122+T_5123+T_5124+T_5125+T_5126+T_5127+T_5128+T_5130+T_5200+T_5210+T_5211+T_5212+T_5213+T_5220+T_5300+T_6000+T_6100+T_6200+T_CUS+_T..XDC.A?startPeriod=2022&endPeriod=2022&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
non_oecd_national_currency<-read.csv(url)

#Keep selected columns
non_oecd_national_currency<-non_oecd_national_currency[c(5,6,21,25,26,27)]

#Calculate value according to UNIT_MULT
non_oecd_national_currency$value<- non_oecd_national_currency$OBS_VALUE*10^non_oecd_national_currency$UNIT_MULT

#Keep selected columns and rename columns
non_oecd_national_currency<-non_oecd_national_currency[c(1,2,6,7)]

#Convert from long to wide format
non_oecd_national_currency_wide <- spread (non_oecd_national_currency, key = REVENUE_CODE, value = value)

#Rename columns
colnames(non_oecd_national_currency_wide)[colnames(non_oecd_national_currency_wide)=="Reference.area"] <- "country"
colnames(non_oecd_national_currency_wide)[colnames(non_oecd_national_currency_wide)=="REF_AREA"] <- "iso_3"


#Merge 'non_oecd_national_currency' dataset with country groups
non_oecd_national_currency_wide <- merge(non_oecd_national_currency_wide, country_iso_cont_groups, by= "iso_3")


#Delete redundant columns
non_oecd_national_currency_wide <- non_oecd_national_currency_wide[,-c(68)]

colnames(non_oecd_national_currency_wide)[colnames(non_oecd_national_currency_wide)=="country.x"] <- "country"

#Merge oecd_data with non_oecd
oecd_europe_national_currency_wide <-rbind(non_oecd_national_currency_wide, oecd_data_national_currency_wide)


#Correct country names
oecd_europe_national_currency_wide$country[oecd_europe_national_currency_wide$iso_3 == "TUR"] <- "Turkey"


#Substitute NA values with 0
oecd_europe_national_currency_wide[is.na(oecd_europe_national_currency_wide)] <- 0

write.csv(oecd_europe_national_currency_wide, "oecd_europe_wide.csv", row.names = FALSE)


#Correcting wealth tax data '4200' for Colombia, Greece and Hungary

#In 2023 wealth tax in Colombia only applied to individuals (https://www.taxathand.com/article/35899/Colombia/2024/Tax-reform-bill-presented-to-Congress-intended-to-increase-tax-revenue)
oecd_europe_national_currency_wide$"4210"<-ifelse(oecd_europe_national_currency_wide$iso_3== "COL", oecd_europe_national_currency_wide$"4200", oecd_europe_national_currency_wide$"4210")

oecd_europe_national_currency_wide$"4210"<-ifelse(oecd_europe_national_currency_wide$iso_3== "GRC", oecd_europe_national_currency_wide$"4200"/2, oecd_europe_national_currency_wide$"4210")
oecd_europe_national_currency_wide$"4220"<-ifelse(oecd_europe_national_currency_wide$iso_3== "GRC", oecd_europe_national_currency_wide$"4200"/2, oecd_europe_national_currency_wide$"4220")

oecd_europe_national_currency_wide$"4210"<-ifelse(oecd_europe_national_currency_wide$iso_3== "HUN", oecd_europe_national_currency_wide$"4200"/2, oecd_europe_national_currency_wide$"4210")
oecd_europe_national_currency_wide$"4220"<-ifelse(oecd_europe_national_currency_wide$iso_3== "HUN", oecd_europe_national_currency_wide$"4200"/2, oecd_europe_national_currency_wide$"4220")

#Correcting  recurrent taxes on immovable property '4100' for CHL, ESP,ISR, ITA, NZL, PRT (using distribution in tables A from OECD 2017 paper )
oecd_europe_national_currency_wide$"4120"<-ifelse(oecd_europe_national_currency_wide$iso_3== "CHL", oecd_europe_national_currency_wide$"4100"*542478/924062, oecd_europe_national_currency_wide$"4120")
oecd_europe_national_currency_wide$"4110"<-ifelse(oecd_europe_national_currency_wide$iso_3== "CHL", oecd_europe_national_currency_wide$"4100"-oecd_europe_national_currency_wide$"4120", oecd_europe_national_currency_wide$"4110")

oecd_europe_national_currency_wide$"4110"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ESP", oecd_europe_national_currency_wide$"4100"-oecd_europe_national_currency_wide$"4120", oecd_europe_national_currency_wide$"4110")
oecd_europe_national_currency_wide$"4110"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ISR", oecd_europe_national_currency_wide$"4100"-oecd_europe_national_currency_wide$"4120", oecd_europe_national_currency_wide$"4110")

oecd_europe_national_currency_wide$"4120"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ITA", oecd_europe_national_currency_wide$"4100"*9472/25199, oecd_europe_national_currency_wide$"4120")
oecd_europe_national_currency_wide$"4110"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ITA", oecd_europe_national_currency_wide$"4100"-oecd_europe_national_currency_wide$"4120", oecd_europe_national_currency_wide$"4110")

oecd_europe_national_currency_wide$"4120"<-ifelse(oecd_europe_national_currency_wide$iso_3== "NZL", oecd_europe_national_currency_wide$"4100"/2, oecd_europe_national_currency_wide$"4120")
oecd_europe_national_currency_wide$"4110"<-ifelse(oecd_europe_national_currency_wide$iso_3== "NZL", oecd_europe_national_currency_wide$"4100"/2, oecd_europe_national_currency_wide$"4110")

oecd_europe_national_currency_wide$"4120"<-ifelse(oecd_europe_national_currency_wide$iso_3== "PRT", oecd_europe_national_currency_wide$"4100"*20/1455, oecd_europe_national_currency_wide$"4120")
oecd_europe_national_currency_wide$"4110"<-ifelse(oecd_europe_national_currency_wide$iso_3== "PRT", oecd_europe_national_currency_wide$"4100"-oecd_europe_national_currency_wide$"4120", oecd_europe_national_currency_wide$"4110")

#Upload employment data % private/total employment
employment <- read_excel("Employment_Private_total.xlsx")

#Upload 2022 Public sector SSC as % of total SSC contribution for 2000 and 3000
Public_SSC <- read_excel("SSC_government.xlsx")

#Upload Pass-through data (non corporate business tax revenue as % total taxes on Income, Profits and Capital gains (1000))
Pass_through <- read_excel("Pass_through.xlsx")

#Merge dataset with private employment data
oecd_europe_national_currency_wide <- merge(oecd_europe_national_currency_wide, employment, by= "iso_3")

#Merge dataset with Public sector SSC
oecd_europe_national_currency_wide <- merge(oecd_europe_national_currency_wide, Public_SSC, by= "iso_3")

#Merge dataset with Pass_through data (non corporate business tax revenue as % total taxes on Income, Profits and Capital gains (1000))
oecd_europe_national_currency_wide <- merge(oecd_europe_national_currency_wide, Pass_through, by= "iso_3")

#Delete redundant columns
oecd_europe_national_currency_wide <- oecd_europe_national_currency_wide[,-c(79)]
colnames(oecd_europe_national_currency_wide)[colnames(oecd_europe_national_currency_wide)=="country.x"] <- "country"

#For BGR, DNK, HRV, ISL, KOR, MLT, NLD, NZL, PRT, ROU that don't have public sector SSC % substitute it with % Public employment/ total employment

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "BGR", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "BGR", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "DNK", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "DNK", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "HRV", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "HRV", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ISL", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ISL", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "KOR", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "KOR", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "MLT", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "MLT", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "NLD", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "NLD", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "NZL", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "NZL", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "PRT", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "PRT", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide$"2000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ROU", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"3000_gov"<-ifelse(oecd_europe_national_currency_wide$iso_3== "ROU", 1-oecd_europe_national_currency_wide$"employment", oecd_europe_national_currency_wide$"3000_gov")

oecd_europe_national_currency_wide[is.na(oecd_europe_national_currency_wide)] <- 0

#Pass-through income as tax liability 
oecd_europe_national_currency_wide$"1100_TL"<- (oecd_europe_national_currency_wide$"1000" * oecd_europe_national_currency_wide$"Pass_through_1000")

#Calculate Income tax (non business remittance) for public employees
oecd_europe_national_currency_wide$"1110_NONB"<- (oecd_europe_national_currency_wide$"1110" * oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"1110_REM"<- (oecd_europe_national_currency_wide$"1110" - oecd_europe_national_currency_wide$"1110_NONB")

#Correct 2100 and 2200 Social security contributions by employees/ employers to account only for private employment (remittance/liability) the rest is non-business (public sector employees)
oecd_europe_national_currency_wide$"2100_NONB"<- (oecd_europe_national_currency_wide$"2100" * oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"2100_REM"<- (oecd_europe_national_currency_wide$"2100"- oecd_europe_national_currency_wide$"2100_NONB")

oecd_europe_national_currency_wide$"2200_NONB"<- (oecd_europe_national_currency_wide$"2200" * oecd_europe_national_currency_wide$"2000_gov")
oecd_europe_national_currency_wide$"2200_TL"<- (oecd_europe_national_currency_wide$"2200" - oecd_europe_national_currency_wide$"2200_NONB")

#Correct 3000 taxes on payroll and workforce to account for private employment only (remittance/liability) the rest is non-business (public sector employees) 
oecd_europe_national_currency_wide$"3000_NONB"<- (oecd_europe_national_currency_wide$"3000" * oecd_europe_national_currency_wide$"3000_gov")
oecd_europe_national_currency_wide$"3000_TL"<- (oecd_europe_national_currency_wide$"3000" - oecd_europe_national_currency_wide$"3000_NONB")

#Calculate business tax liability, remittance, non-business and unallocable####
oecd_europe_national_currency_wide$TL<- oecd_europe_national_currency_wide$"1100_TL"+oecd_europe_national_currency_wide$"1200"+oecd_europe_national_currency_wide$"2200_TL"+ oecd_europe_national_currency_wide$"2300"+ oecd_europe_national_currency_wide$"3000_TL"+ oecd_europe_national_currency_wide$"4120"+ oecd_europe_national_currency_wide$"4220"+ oecd_europe_national_currency_wide$"5121"+ oecd_europe_national_currency_wide$"5122"+ oecd_europe_national_currency_wide$"5125"+ oecd_europe_national_currency_wide$"5126"+ oecd_europe_national_currency_wide$"5212"+ oecd_europe_national_currency_wide$"5213"+ oecd_europe_national_currency_wide$"5220"+ oecd_europe_national_currency_wide$"6100"
oecd_europe_national_currency_wide$REM<- oecd_europe_national_currency_wide$"1110_REM"+oecd_europe_national_currency_wide$"1120"-oecd_europe_national_currency_wide$"1100_TL"+ oecd_europe_national_currency_wide$"2100_REM"+oecd_europe_national_currency_wide$"5111"+oecd_europe_national_currency_wide$"5112"+oecd_europe_national_currency_wide$"5113"
oecd_europe_national_currency_wide$NONB<- oecd_europe_national_currency_wide$"1110_NONB"+oecd_europe_national_currency_wide$"2100_NONB"+ oecd_europe_national_currency_wide$"2200_NONB"+ oecd_europe_national_currency_wide$"3000_NONB"+oecd_europe_national_currency_wide$"4110"+oecd_europe_national_currency_wide$"4210"+oecd_europe_national_currency_wide$"4300"+oecd_europe_national_currency_wide$"5211"
oecd_europe_national_currency_wide$UNALLOC<- oecd_europe_national_currency_wide$"1300"+oecd_europe_national_currency_wide$"2400"+oecd_europe_national_currency_wide$"4400"+oecd_europe_national_currency_wide$"4500"+oecd_europe_national_currency_wide$"4600"+oecd_europe_national_currency_wide$"5123"+oecd_europe_national_currency_wide$"5124"+oecd_europe_national_currency_wide$"5127"+oecd_europe_national_currency_wide$"5128"+oecd_europe_national_currency_wide$"5130"+oecd_europe_national_currency_wide$"5300"+oecd_europe_national_currency_wide$"6200"

#Correct data for 1100 (Taxes on income, profits and capital gains of individuals) for Chile, Spain, Mexico, Poland and Portugal (they only have data for 1100, no distribution between 1110 and 1120). OECD 2017 report was used to distributed the 1100 revenue between TL, REM and UNALLOC)
oecd_europe_national_currency_wide$REM<-ifelse(oecd_europe_national_currency_wide$iso_3== "CHL", oecd_europe_national_currency_wide$REM+oecd_europe_national_currency_wide$"1100", oecd_europe_national_currency_wide$REM)

oecd_europe_national_currency_wide$TL<-ifelse(oecd_europe_national_currency_wide$iso_3== "ESP", oecd_europe_national_currency_wide$TL+oecd_europe_national_currency_wide$"1100"*10489/ 79655, oecd_europe_national_currency_wide$TL)
oecd_europe_national_currency_wide$REM<-ifelse(oecd_europe_national_currency_wide$iso_3== "ESP", oecd_europe_national_currency_wide$REM+oecd_europe_national_currency_wide$"1100"*68434/ 79655, oecd_europe_national_currency_wide$REM)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "ESP", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"1100"*732/ 79655, oecd_europe_national_currency_wide$UNALLOC)

oecd_europe_national_currency_wide$TL<-ifelse(oecd_europe_national_currency_wide$iso_3== "MEX", oecd_europe_national_currency_wide$TL+oecd_europe_national_currency_wide$"1100"*32052/ 514208, oecd_europe_national_currency_wide$TL)
oecd_europe_national_currency_wide$REM<-ifelse(oecd_europe_national_currency_wide$iso_3== "MEX", oecd_europe_national_currency_wide$REM+oecd_europe_national_currency_wide$"1100"*473233/ 514208, oecd_europe_national_currency_wide$REM)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "MEX", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"1100"*8924/ 514208, oecd_europe_national_currency_wide$UNALLOC)

oecd_europe_national_currency_wide$TL<-ifelse(oecd_europe_national_currency_wide$iso_3== "POL", oecd_europe_national_currency_wide$TL+oecd_europe_national_currency_wide$"1100"*19939/ 74216, oecd_europe_national_currency_wide$TL)
oecd_europe_national_currency_wide$REM<-ifelse(oecd_europe_national_currency_wide$iso_3== "POL", oecd_europe_national_currency_wide$REM+oecd_europe_national_currency_wide$"1100"*35085/ 74216, oecd_europe_national_currency_wide$REM)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "POL", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"1100"*19192/ 74216, oecd_europe_national_currency_wide$UNALLOC)

oecd_europe_national_currency_wide$TL<-ifelse(oecd_europe_national_currency_wide$iso_3== "PRT", oecd_europe_national_currency_wide$TL+oecd_europe_national_currency_wide$"1100"*674/ 13318, oecd_europe_national_currency_wide$TL)
oecd_europe_national_currency_wide$REM<-ifelse(oecd_europe_national_currency_wide$iso_3== "PRT", oecd_europe_national_currency_wide$REM+oecd_europe_national_currency_wide$"1100"*2179/ 13318, oecd_europe_national_currency_wide$REM)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "PRT", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"1100"*10455/ 13318, oecd_europe_national_currency_wide$UNALLOC)

#Correct 1120 for Denmark (not withholding taxes).1120 for Denmark is non business remittance.
oecd_europe_national_currency_wide$REM<-ifelse(oecd_europe_national_currency_wide$iso_3== "DNK", oecd_europe_national_currency_wide$REM-oecd_europe_national_currency_wide$"1120", oecd_europe_national_currency_wide$REM)
oecd_europe_national_currency_wide$NONB<-ifelse(oecd_europe_national_currency_wide$iso_3== "DNK", oecd_europe_national_currency_wide$NONB+oecd_europe_national_currency_wide$"1120", oecd_europe_national_currency_wide$NONB)

#Correct  recurrent taxes on immovable property '4100' for Australia, Bulgaria, Canada, Costa Rica,Denmark, Estonia, Greece,Hungary, Ireland, Iceland, Japan, Korea, Luxembourg, Turkey and the United States to add them to Unallocable
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "AUS", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "BGR", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "CAN", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "CRI", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "DNK", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "EST", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "GRC", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "HUN", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "IRL", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "ISL", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "JPN", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "KOR", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "LUX", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "TUR", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "USA", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"4100", oecd_europe_national_currency_wide$UNALLOC)

#Correct 5200 and 5210 taxes on use of goods, or on permission to use goods or perform activities for Chile, Czechia, Estonia,Hungary, Ireland,Iceland, Japan, Korea, New Zealand to Unallocable. For these countries 5210 is not distrusted correctly between 5211,5212, and 5213)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "CHL", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5210"-oecd_europe_national_currency_wide$"5213", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "CZE", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5210"-oecd_europe_national_currency_wide$"5212"-oecd_europe_national_currency_wide$"5213", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "EST", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5210"-oecd_europe_national_currency_wide$"5213", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "HUN", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5210"-oecd_europe_national_currency_wide$"5213", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "ISL", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5200", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "JPN", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5210", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "KOR", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5210", oecd_europe_national_currency_wide$UNALLOC)
oecd_europe_national_currency_wide$UNALLOC<-ifelse(oecd_europe_national_currency_wide$iso_3== "NZL", oecd_europe_national_currency_wide$UNALLOC+oecd_europe_national_currency_wide$"5210"-oecd_europe_national_currency_wide$"5213", oecd_europe_national_currency_wide$UNALLOC)

#Table for the report####

#calculate %
oecd_europe_national_currency_wide$TL_per<-oecd_europe_national_currency_wide$TL/oecd_europe_national_currency_wide$TOTALTAX
oecd_europe_national_currency_wide$REM_per<-oecd_europe_national_currency_wide$REM/oecd_europe_national_currency_wide$TOTALTAX
oecd_europe_national_currency_wide$NONB_per<-oecd_europe_national_currency_wide$NONB/oecd_europe_national_currency_wide$TOTALTAX
oecd_europe_national_currency_wide$UNALLOC_per<-oecd_europe_national_currency_wide$UNALLOC/oecd_europe_national_currency_wide$TOTALTAX

#Calculate different categories for tax liability
oecd_europe_national_currency_wide$TL_SS<-(oecd_europe_national_currency_wide$"2200_TL"+-oecd_europe_national_currency_wide$"2300"+oecd_europe_national_currency_wide$"3000_TL")/oecd_europe_national_currency_wide$TOTALTAX
oecd_europe_national_currency_wide$TL_excise<-oecd_europe_national_currency_wide$"5121"/oecd_europe_national_currency_wide$TOTALTAX
oecd_europe_national_currency_wide$TL_income<- (oecd_europe_national_currency_wide$TL-(oecd_europe_national_currency_wide$"2200_TL"+ oecd_europe_national_currency_wide$"2300"+ oecd_europe_national_currency_wide$"3000_TL"+ oecd_europe_national_currency_wide$"4120"+ oecd_europe_national_currency_wide$"4220"+ oecd_europe_national_currency_wide$"5121"+ oecd_europe_national_currency_wide$"5122"+ oecd_europe_national_currency_wide$"5125"+ oecd_europe_national_currency_wide$"5126"+ oecd_europe_national_currency_wide$"5212"+ oecd_europe_national_currency_wide$"5213"+ oecd_europe_national_currency_wide$"5220"+ oecd_europe_national_currency_wide$"6100"))/oecd_europe_national_currency_wide$TOTALTAX

#Create regional sets 
OECD<- subset(oecd_europe_national_currency_wide, oecd== 1)
Europe <- subset(oecd_europe_national_currency_wide, continent== "EU")
EU27 <- subset(oecd_europe_national_currency_wide, eu27== 1)

#Simple Means
Alldata_mean_TL<- mean(oecd_europe_national_currency_wide$TL_per, na.rm=TRUE)
Alldata_mean_REM<- mean(oecd_europe_national_currency_wide$REM_per, na.rm=TRUE)
Alldata_mean_UNALLOC<- mean(oecd_europe_national_currency_wide$UNALLOC_per, na.rm=TRUE)
Alldata_mean_NONB<- mean(oecd_europe_national_currency_wide$NONB_per, na.rm=TRUE)
Alldata_mean_TL_SS<- mean(oecd_europe_national_currency_wide$TL_SS, na.rm=TRUE)
Alldata_mean_TL_income<- mean(oecd_europe_national_currency_wide$TL_income, na.rm=TRUE)
Alldata_mean_TL_excise<- mean(oecd_europe_national_currency_wide$TL_excise, na.rm=TRUE)

OECD_mean_TL <- mean(OECD$TL_per, na.rm=TRUE)
OECD_mean_REM <- mean(OECD$REM_per, na.rm=TRUE)
OECD_mean_UNALLOC <- mean(OECD$UNALLOC_per, na.rm=TRUE)
OECD_mean_NONB <- mean(OECD$NONB_per, na.rm=TRUE)
OECD_mean_TL_SS <- mean(OECD$TL_SS, na.rm=TRUE)
OECD_mean_TL_income <- mean(OECD$TL_income, na.rm=TRUE)
OECD_mean_TL_excise <- mean(OECD$TL_excise, na.rm=TRUE)

europe_mean_TL <- mean(Europe$TL_per, na.rm=TRUE)
europe_mean_REM <- mean(Europe$REM_per, na.rm=TRUE)
europe_mean_UNALLOC <- mean(Europe$UNALLOC_per, na.rm=TRUE)
europe_mean_NONB <- mean(Europe$NONB_per, na.rm=TRUE)
europe_mean_TL_SS <- mean(Europe$TL_SS, na.rm=TRUE)
europe_mean_TL_income <- mean(Europe$TL_income, na.rm=TRUE)
europe_mean_TL_excise <- mean(Europe$TL_excise, na.rm=TRUE)


eu27_mean_TL <- mean(EU27$TL_per, na.rm=TRUE)
eu27_mean_REM <- mean(EU27$REM_per, na.rm=TRUE)
eu27_mean_UNALLOC <- mean(EU27$UNALLOC_per, na.rm=TRUE)
eu27_mean_NONB <- mean(EU27$NONB_per, na.rm=TRUE)
eu27_mean_TL_SS <- mean(EU27$TL_SS, na.rm=TRUE)
eu27_mean_TL_income <- mean(EU27$TL_income, na.rm=TRUE)
eu27_mean_TL_excise <- mean(EU27$TL_excise, na.rm=TRUE)

#add the regional means to the table
Alldata_mean<-c("NA","All Countries Average",round(Alldata_mean_TL, digits = 4), round(Alldata_mean_REM, digits = 4), round(Alldata_mean_UNALLOC, digits = 4), round(Alldata_mean_NONB, digits = 4), round(Alldata_mean_TL_SS, digits = 4), round(Alldata_mean_TL_income, digits = 4), round(Alldata_mean_TL_excise, digits = 4))
OECD_mean<-c("NA","OECD Average",round(OECD_mean_TL, digits = 4), round(OECD_mean_REM, digits = 4), round(OECD_mean_UNALLOC, digits = 4), round(OECD_mean_NONB, digits = 4), round(OECD_mean_TL_SS, digits = 4), round(OECD_mean_TL_income, digits = 4), round(OECD_mean_TL_excise, digits = 4))
eu27_mean<-c("NA","EU 27 Average",round(eu27_mean_TL, digits = 4), round(eu27_mean_REM, digits = 4), round(eu27_mean_UNALLOC, digits = 4), round(eu27_mean_NONB, digits = 4), round(eu27_mean_TL_SS, digits = 4), round(eu27_mean_TL_income, digits = 4),round(eu27_mean_TL_excise, digits = 4))
europe_mean<-c("NA", "Europe Average",round(europe_mean_TL, digits = 4), round(europe_mean_REM, digits = 4), round(europe_mean_UNALLOC, digits = 4), round(europe_mean_NONB, digits = 4), round(europe_mean_TL_SS, digits = 4), round(europe_mean_TL_income, digits = 4), round(europe_mean_TL_excise, digits = 4))

oecd_europe_national_currency_Final<- subset (oecd_europe_national_currency_wide, select = c("iso_3","country","TL_per","REM_per","NONB_per","UNALLOC_per","TL_SS","TL_income","TL_excise" ))

data_Final<-rbind(oecd_europe_national_currency_Final, Alldata_mean, OECD_mean, eu27_mean, europe_mean)

colnames(data_Final)[colnames(data_Final)=="country"] <- "Country"
colnames(data_Final)[colnames(data_Final)=="TL_per"] <- "Business Legal Tax Liability"
colnames(data_Final)[colnames(data_Final)=="REM_per"] <- "Taxes Remitted by Business"
colnames(data_Final)[colnames(data_Final)=="UNALLOC_per"] <- "Unallocated"
colnames(data_Final)[colnames(data_Final)=="NONB_per"] <- "Non-Business Tax Collections"


write.csv(data_Final, "oecd_europe_table.csv", row.names = FALSE)