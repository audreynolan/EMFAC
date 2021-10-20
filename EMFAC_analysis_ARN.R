## 
## This code analyzes data output from CARB's EMFAC2021 model found at https://arb.ca.gov/emfac/emissions-inventory/d7e33b22a7ef163d2dc9fd91182391d41cb025f9 and outputs variables used to help quantify on-road emissions for GHG emissions inventory and forecasting. Data from the EMFAC2021 model was gathered using the following inputs:
## Output = Emissions
## Model Version = EMFAC2021 v1.0.1
## Region Type = County
## Region = all
## Calendar year = all
## Season = annual
## Vehicle Category = EMFAC2007
## Model Year = Aggregate
## Speed = Aggregate
## Fuel = All
## Output Unit = tons/year
##
## data is saved on the server at S:\ajared\09_EMFAC analysis\EMFAC2021_2007categories
## The results of this code are summary statistics of EMFAC2021 data, including VMT, emissions factors (CO2e/mile), and energy usage (kWh) for passenger, commercial, buses, and commercial + buses for all years between 2000-2050 and all counties in California.

## load packages ####
library(openxlsx)
library(plyr)

## read data ####
## get filepaths for all EMFAC files
filepath <- "R:/SUS/Climate Action Planning/EMFAC2021 Data/EMFAC2021_2007categories"
filepaths <- list.files(filepath, full.names = TRUE)

## load all data into R as a list; each list element corresponds to a county and each table contains all emfac output data for 2000-2050
data <- lapply(filepaths, function(x) read.csv(x,skip=8))

## subset data
subcols <- c("Region","Calendar.Year","Vehicle.Category","Fuel","Population","Total.VMT","CVMT","EVMT","Trips","Energy.Consumption","CO2_TOTEX","CH4_TOTEX","N2O_TOTEX","Fuel.Consumption")
data2 <- lapply(data,function(x) x[,subcols])
data3 <- dplyr::bind_rows(data2)

## define GWPs for analysis ####
CO2_gwp <- 1
CH4_gwp <- 28
N2O_gwp <-265

## analyze data ####
## assign vehicle designations to each vehicle type
passenger <- c("LDA","LDT1","LDT2","MCY","MDV","MH")
commercial <- c("LHDT1","LHDT2","HHDT","MHDT")
buses <- c("OBUS","SBUS","UBUS")
commercial_buses <- c("LHDT1","LHDT2","HHDT","MHDT","OBUS","SBUS","UBUS")
data3$Vehicle.Designation <- rep(NA,nrow(data3))
data3$Vehicle.Designation[data3$Vehicle.Category%in%passenger] <- "Passenger"
data3$Vehicle.Designation[data3$Vehicle.Category%in%commercial] <- "Commercial"
data3$Emissions <- data3$CO2_TOTEX*CO2_gwp + data3$CH4_TOTEX*CH4_gwp + data3$N2O_TOTEX*N2O_gwp

## calculate average TPM for gasoline and diesel passenger vehicles separately for each region and year
## add explanation of ddply, how it works
Passenger.CTPM <- ddply(data3[data3$Fuel%in%c("Diesel","Gasoline")&data3$Vehicle.Designation=="Passenger",], 
                        .(Region, Calendar.Year), summarize,
                        Passenger.CTPM = sum(Trips)/sum(Total.VMT))

## calculate total VMT, CVMT, EVMT, EV share and EF for passenger vehicles separately for each region and year
Passenger <- ddply(data3[data3$Vehicle.Designation=="Passenger", ],
                   .(Region, Calendar.Year), summarize,
                   Passenger.CVMT_mpy = sum(CVMT),
                   Passenger.EVMT_mpy = sum(EVMT),
                   Passenger.VMT_mpy = sum(Total.VMT),
                   Passenger.Energy.Consumption_kwhpy = sum(Energy.Consumption),
                   Passenger.EV.Share = sum(EVMT) / sum(Total.VMT),
                   Passenger.EPM_kwhpevm = sum(Energy.Consumption) / sum(EVMT),
                   Passenger.EF_kwhpm = sum(Emissions) * 0.907185 / sum(Total.VMT),
                   Passenger.CEF_kwhpcm = sum(Emissions) * 0.907185 / sum(CVMT))

## calculate total VMT, CVMT, EVMT, EV share and EF for commercial vehicles separately for each region and year
Commercial <- ddply(data3[data3$Vehicle.Designation=="Commercial",],
                    .(Region, Calendar.Year), summarize,
                    Commercial.CVMT_mpy = sum(CVMT),
                    Commercial.EVMT_mpy = sum(EVMT),
                    Commercial.VMT_mpy = sum(Total.VMT),
                    Commercial.Energy.Consumption_kwhpy = sum(Energy.Consumption),
                    Commercial.EV.Share = sum(EVMT)/sum(Total.VMT),
                    Commercial.EPM_kwhpevm = sum(Energy.Consumption)/sum(EVMT),
                    Commercial.EF_kwhpm = sum(Emissions)*0.907185/sum(Total.VMT),
                    Commercial.CEF_kwhpcm = sum(Emissions)*0.907185/sum(CVMT))

## combine data into single data frame ####
outdata <- full_join(Passenger,Passenger.CTPM,by=c("Region","Calendar.Year"))
outdata2 <- full_join(outdata,Commercial,by=c("Region","Calendar.Year"))

## write data to csv file ####
## Uncomment the lines below to write the data to the server
# writefile <- "S:/ajared/09_EMFAC analysis/EMFAC2021_data.csv"
# write.csv(outdata2,file=writefile,row.names=FALSE)
