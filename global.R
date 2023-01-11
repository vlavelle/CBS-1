library(dplyr)
library(shiny)
library(shinydashboard)
library(sf)
library(cbsodataR)
library(ggplot2)
library(forcats)
library(ggiraph)
library(plotly)
library(readxl)

# set wd to the file location where you have downloaded the global, server and UI files
setwd("~/R/CBS Project/Dashboard1/")

################################################################################
#Overview by Tab

####Mobility Indicators
### Data 84710 
# Motives & Modes per Region
# Modes per Region (Both Plots)

### Data 85055
# Timeframe Data: Travel Purpose

### Data 85056
# Timeframe Data: Travel Mode

### Data 84709
#Personal Characteristics


##### Green Mobility

##### Traffic Intensity


##### Proximity to Amenities
### Data 80305
# Map and Plot



##### data84710 ################################################################
# Motives & Modes per Region
# Modes per Region (Both Plots)
metadata84710 <- cbs_get_meta("84710ENG") 

data84710 <- cbs_get_data(
  id = "84710ENG",
  Periods = has_substring("JJ"),
  RegionCharacteristics = c("NL01    ", "LD01    ", "PV20    ",
                            "PV21    ", "PV22    "),
  Population = "A048710", #6 and older
  Margins = "MW00000", #pure value, no confidence interval values
  select = c(
    "TravelMotives",
    "TravelModes",
    "RegionCharacteristics",
    "Periods",
    "Trips_1", #daily data
    "DistanceTravelled_2", #daily data
    "Trips_4", #yearly data
    "DistanceTravelled_5" #yearly data
  )
)


tempPeriods84710 <- metadata84710$Periods
tempMotives84710 <- metadata84710$TravelMotives
tempModes84710 <- metadata84710$TravelModes
tempRegion84710 <- metadata84710$RegionCharacteristics


data84710$Periods <- tempPeriods84710$Title[match(data84710$Periods, tempPeriods84710$Key)]
data84710$TravelMotives <- tempMotives84710$Title[match(data84710$TravelMotives, tempMotives84710$Key)]
data84710$TravelModes <- tempModes84710$Title[match(data84710$TravelModes, tempModes84710$Key)]

#This is the fixed one, for testing purposes it is 2 right now
data84710$RegionCharacteristics <- recode(data84710$RegionCharacteristics, 
                                          "NL01    " = "The Netherlands",
                                          "LD01    " = "Northern Netherlands",
                                          "PV20    " = "Groningen",
                                          "PV21    " = "Friesland",
                                          "PV22    " = "Drenthe")


##### Data 85055ENG ############################################################
# Timeframe Data: Travel Purpose
# IDEA 6

metadata85055 <- cbs_get_meta("85055ENG")

data85055 <- cbs_get_data(
  id = "85055ENG", 
  TripCharacteristics = c(
    "2030851", "2030871", "2030891", "2030911", "2030931", "2030950", "2031000", 
    "2031090", "2031100", "2031110", "2031120", "2031130", "2031140", "2031150", 
    "2031160", "2031170", "2031180", "2031190", "2031200", "2031210", "2031220", 
    "2031230", "2031240", "2031250", "2031260", "2031270", "2820701", "2820702", 
    "2820704", "2820705", "2820706","A025261", "A025262", "A025263", "A025264", 
    "A025265", "A025266", "A025267", "A025268"
  ), 
  Population = "A048710", 
  TravelPurposes = c(
    "2030170","2030190","2030200","2030210","2030220","2030230","2030240",
    "2030250","2820740","T001080"
  ), 
  Margins = "MW00000", 
  RegionCharacteristics = c(
    "PV20    ", "PV21    ", "PV22    ", "LD01    "
  )
)


##Data Prep Idea 6##
#temp tables
temp_Region85055 <- metadata85055$RegionCharacteristics
temp_Periods85055 <- metadata85055$Periods
temp_TripCharacteristics85055 <- metadata85055$TripCharacteristics
temp_TravelPurposes85055 <- metadata85055$TravelPurposes
temp_Population85055 <- metadata85055$Population

#Fixing Region names manually
data85055$RegionCharacteristics <- recode(data85055$RegionCharacteristics,
                                          "LD01    " = "Northern Netherlands",
                                          "PV20    " = "Groningen",
                                          "PV21    " = "Friesland",
                                          "PV22    " = "Drenthe")
#Matching and replacing Keys for Keys
data85055$Periods <- temp_Periods85055$Title[match(data85055$Periods, temp_Periods85055$Key)]
data85055$TripCharacteristics <- temp_TripCharacteristics85055$Title[match(data85055$TripCharacteristics, temp_TripCharacteristics85055$Key)]
data85055$TravelPurposes <- temp_TravelPurposes85055$Title[match(data85055$TravelPurposes, temp_TravelPurposes85055$Key)]
data85055$Population <- temp_Population85055$Title[match(data85055$Population, temp_Population85055$Key)]
data85055 <- data85055 %>% mutate(
  Timeframe = case_when(
    grepl("Distance:", data85055$TripCharacteristics) ~ "Distance",
    grepl("Time travelled:", data85055$TripCharacteristics) ~ "Time travelled",
    grepl("Trip in", data85055$TripCharacteristics) ~ "Month",
    grepl("Departure time:", data85055$TripCharacteristics) ~ "Departure time",
    grepl("day", data85055$TripCharacteristics) ~ "Day of the week"
  )
) %>% select(TripCharacteristics, Timeframe, TravelPurposes, RegionCharacteristics, Periods, AverageDistanceTravelledPerTrip_1, AverageTravelTimePerTrip_2)  

data85055$TripCharacteristics <- sub('Departure time: ', '', data85055$TripCharacteristics)
data85055$TripCharacteristics <- sub('Trip in ', '', data85055$TripCharacteristics)

##### Data 85056ENG ############################################################
# Timeframe Data: Travel Mode

# want to do similar to above for dataset 85056 for modes of travel at different times
metadata85056 <- cbs_get_meta("85056ENG")
data85056 <- cbs_get_data(id = "85056ENG", 
                          TripCharacteristics = c("2030851", "2030871", 
                                                  "2030891", "2030911", "2030931", "2030950", "2031000",
                                                  "2031090", "2031100", "2031110", "2031120", "2031130", 
                                                  "2031140", "2031150", "2031160", "2031170", "2031180", 
                                                  "2031190", "2031200", "2031210", "2031220", "2031230", 
                                                  "2031240", "2031250", "2031260", "2031270", "2820701", 
                                                  "2820702", "2820704", "2820705", "2820706","A025261", 
                                                  "A025262", "A025263", "A025264", "A025265", "A025266", 
                                                  "A025267", "A025268"), 
                          Population = "A048710", 
                          ModesOfTravel = c("T001093","A048583","A048584","A018981","A018982",               
                                            "A018984","A018985","A018986"),
                          Margins = "MW00000", 
                          RegionCharacteristics = c("PV20    ", "PV21    ", 
                                                    "PV22    ", "LD01    "))

temp_Region85056 <- metadata85056$RegionCharacteristics
temp_Periods85056 <- metadata85056$Periods
temp_TripCharacteristics85056 <- metadata85056$TripCharacteristics
temp_ModesOfTravel85056 <- metadata85056$ModesOfTravel
temp_Population85056 <- metadata85056$Population

#Fixing Region names manually
data85056$RegionCharacteristics <- recode(data85056$RegionCharacteristics,
                                          "LD01    " = "Northern Netherlands",
                                          "PV20    " = "Groningen",
                                          "PV21    " = "Friesland",
                                          "PV22    " = "Drenthe")
#Matching and replacing Keys for Keys
data85056$Periods <- temp_Periods85056$Title[match(data85056$Periods, temp_Periods85056$Key)]
data85056$TripCharacteristics <- temp_TripCharacteristics85056$Title[match(data85056$TripCharacteristics, temp_TripCharacteristics85056$Key)]
data85056$ModesOfTravel <- temp_ModesOfTravel85056$Title[match(data85056$ModesOfTravel, temp_ModesOfTravel85056$Key)]
data85056$Population <- temp_Population85056$Title[match(data85056$Population, temp_Population85056$Key)]

#mutation for aliasing
data85056 <- data85056 %>% mutate(
  Timeframe = case_when(
    grepl("Distance:", data85056$TripCharacteristics) ~ "Distance",
    grepl("Time travelled:", data85056$TripCharacteristics) ~ "Time travelled",
    grepl("Trip in", data85056$TripCharacteristics) ~ "Month",
    grepl("Departure time:", data85056$TripCharacteristics) ~ "Departure time",
    grepl("day", data85056$TripCharacteristics) ~ "Day of the week"
  )
) %>% select(TripCharacteristics, Timeframe, ModesOfTravel, RegionCharacteristics, Periods, AverageDistanceTravelledPerTrip_1, AverageTravelTimePerTrip_2)  

data85056$TripCharacteristics <- sub('Departure time: ', '', data85056$TripCharacteristics)
data85056$TripCharacteristics <- sub('Trip in ', '', data85056$TripCharacteristics)


##### Data 84709NED ############################################################
# Personal Characteristics
# Idea 2 and 8

metadata84709 <- cbs_get_meta("84709NED")
datatemp <- cbs_get_data('84709NED', Persoonskenmerken = c( "T009002", "51511  ", "52020  ", "53105  ", "53500  ", "53705  ", "53850  ", "53925  ", "21600  ", "1012600", "2012655", "2012657","1014752", "1014753", "1014754", "1014755","1014756", "2030530", "2030540", "2030550","2018700", "2018740", "2018790", "2017560","2017565", "2012751", "2017572", "2820713", "2017576", "2017500", "A048766", "A048767","A048768", "A048769", "A048770"), Vervoerwijzen = c("T001093", "A048583", "A048584", "A018981", "A018982", "A018984", "A018985", "A018986"), RegioS = c("NL01    ","LD01    ","PV20    ","PV21    ", "PV22    "), Geslacht = "T001038", Marges = c("MW00000"), Populatie = c("A048710"), select = c("Persoonskenmerken", "RegioS", "Perioden", "Vervoerwijzen", "Verplaatsingen_1", "Afstand_2", "Reisduur_3"))

data84709 <- datatemp
temp_RegioS84709 <- metadata84709$RegioS # Perioden
temp_Perioden84709 <- metadata84709$Perioden # RegioS
temp_Persoonskenmerken84709 <- metadata84709$Persoonskenmerken # Persoonskenmerken
temp_Vervoerwijzen84709 <- metadata84709$Vervoerwijzen


#Matching and replacing Keys for Keys
data84709$RegioS <- temp_RegioS84709$Title[match(data84709$RegioS, temp_RegioS84709$Key)]
data84709$Perioden <- temp_Perioden84709$Title[match(data84709$Perioden, temp_Perioden84709$Key)]
data84709$Persoonskenmerken <- temp_Persoonskenmerken84709$Title[match(data84709$Persoonskenmerken, temp_Persoonskenmerken84709$Key)]
data84709$Vervoerwijzen <- temp_Vervoerwijzen84709$Title[match(data84709$Vervoerwijzen, temp_Vervoerwijzen84709$Key)]


data84709 <- data84709 %>% mutate(Feature = case_when(
  grepl("Totaal personen", data84709$Persoonskenmerken) ~ "All",
  grepl("Leeftijd:", data84709$Persoonskenmerken) ~ "Age",
  grepl("Migratieachtergrond:", data84709$Persoonskenmerken) ~ "Background",
  grepl("Gestandaardiseerd inkomen:", data84709$Persoonskenmerken) ~ "Income",
  grepl("OV-Studentenkaart:", data84709$Persoonskenmerken) ~ "Travel Product",
  grepl("Onderwijsniveau:", data84709$Persoonskenmerken) ~ "Education",
  grepl("Participatie:", data84709$Persoonskenmerken) ~ "Employment(??)",
  grepl("Rijbewijs,", data84709$Persoonskenmerken) ~ "Driver's License",
  grepl("Geen rijbewijs", data84709$Persoonskenmerken) ~ "Driver's License"
) )
# now it can be filtered on the column "Feature"
unique(data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Leeftijd:', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Migratieachtergrond:', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Gestandaardiseerd inkomen:', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('OV-Studentenkaart: ', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Onderwijsniveau: ', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Participatie: ', '', data84709$Persoonskenmerken)


##### Data 80305 and Shapefile Import ##########################################
#For Proximity to Amenties Tab

# metadata
metadata80305 <- cbs_get_meta("80305ENG") 
# data from CBS
data80305 <- cbs_get_data(
  id = "80305ENG",
  Periods = "2020JJ00", # Only choosing 2020 for now
  Regions = c("PV20  ", "PV21  ", "PV22  ", "GM1680", "GM0059", "GM0060", 
              "GM0003", "GM0106", "GM0005", "GM0007","GM0063", "GM0055",
              "GM0009", "GM0064", "GM1681", "GM0109", "GM0065", "GM1891", 
              "GM0010", "GM0058", "GM1979", "GM1651", "GM0114", "GM1722", 
              "GM0070", "GM1921", "GM1940", "GM0653", "GM0014", "GM0015",
              "GM0017", "GM0072", "GM0074", "GM1966", "GM0118", "GM0018", 
              "GM0079", "GM0022", "GM0080", "GM0081", "GM0082", "GM0140", 
              "GM0024", "GM1663", "GM0025", "GM0083", "GM1908", "GM1987",
              "GM0119", "GM1731", "GM1952", "GM0104", "GM1970", "GM1699",
              "GM1895", "GM0085", "GM0086", "GM0765", "GM1661", "GM0039",
              "GM0088", "GM0051", "GM0040", "GM0090", "GM0091", "GM0037",
              "GM1900", "GM0093", "GM1730", "GM0737", "GM0047", "GM0048",
              "GM0096", "GM1949", "GM1969", "GM1701", "GM1950", "GM0098", 
              "GM0052", "GM0053", "GM1690", "GM0710", "GM0683", "GM0056"),
  select = c("Periods", "Regions", "DistanceToGPPractice_1", "DistanceToGPPost_5", 
             "DistanceToPharmacy_6", "DistanceToHospital_11", "DistanceToLargeSupermarket_20", 
             "DistanceToShopForOtherDailyFood_24", "DistanceToDepartmentStore_28", "DistanceToCafeEtc_32", 
             "DistanceToRestaurant_40", "DistanceToDaycareCentres_48", "DistanceToOutOfSchoolCare_52", 
             "DistanceToSchool_60", "DistanceToSchool_64", "DistanceToTrainStationsAllTypes_101", "DistanceToLibrary_103")
) #the recreational, (semi)public green and sports one were NA for all regions

# Data Prep
tempPeriods <- metadata80305$Periods
tempRegion <- metadata80305$Regions
data80305$Periods <- tempPeriods$Title[match(data80305$Periods, tempPeriods$Key)]

#New Column "Municipality" made, "Region" is kept to match with shapefile
#fixing region names
data80305$Municipality <- tempRegion$Title[match(data80305$Regions, tempRegion$Key)]
data80305$Municipality<- recode(data80305$Municipality,
                                "Groningen (PV)" = "Groningen ", #extra space because city/province issue
                                "Fryslân (PV)" = "Friesland",
                                "Drenthe (PV)" = "Drenthe")

data80305%>%
  mutate(Avg15 = (
    DistanceToGPPractice_1 + DistanceToGPPost_5 +
      DistanceToPharmacy_6 + DistanceToHospital_11 +
      DistanceToLargeSupermarket_20 + DistanceToShopForOtherDailyFood_24 + 
      DistanceToDepartmentStore_28 + DistanceToCafeEtc_32 +
      DistanceToRestaurant_40 + DistanceToDaycareCentres_48 + 
      DistanceToOutOfSchoolCare_52 + DistanceToSchool_60 + 
      DistanceToSchool_64 + DistanceToTrainStationsAllTypes_101
  ) / 15) 

#Shapefile Import through API call from PDOK
municipalBoundaries_unfiltered <- st_read(
  "https://service.pdok.nl/kadaster/bestuurlijkegebieden/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson%3B%20subtype%3Dgeojson&typeName=bestuurlijkegebieden:Gemeentegebied"
)

#Reducing and filtering the imported data for efficiency 
municipalBoundaries <- municipalBoundaries_unfiltered %>%
  filter(ligtInProvincieCode %in% c("20", "21", "22")) %>%
  select(-id, -code)

####Provincial Boundaries Calculation####
#This is to combine the polygons per province into a small table
#First have to create a subset into the different provinces
provinces <- municipalBoundaries %>%
  group_split(ligtInProvincieNaam)

#combining the municipality polygons by the province and renaming the column
provincialBoundaries <- data.frame(c("Drenthe","Friesland","Groningen"), 
                                   c(st_union(provinces[[1]]),
                                     st_union(provinces[[2]]),
                                     st_union(provinces[[3]]))) %>%
  rename(provinces = c..Drenthe....Friesland....Groningen..)
# Joining Data
# Joining Municipal Shapefile by Municipality  and calculating a new column
mapData <- municipalBoundaries %>%
  left_join(data80305, by = c(identificatie="Regions")) 


## Drivers License:
data83488 <- cbs_get_data('83488ENG', Region = c("PV20  ","PV21  ", "PV22  "))
metadata83488 <- cbs_get_meta("83488ENG")


# Dataprep drivers license  
tempCategoryDrivingLicense83488 <- metadata83488$CategoryDrivingLicence
tempAgeDrivingLicenseHolder83488 <- metadata83488$AgeDrivingLicenseHolder
tempRegion83488 <- metadata83488$Region
tempPeriods83488 <- metadata83488$Periods

data83488$CategoryDrivingLicence <- tempCategoryDrivingLicense83488$Title[match(data83488$CategoryDrivingLicence, tempCategoryDrivingLicense83488$Key)]

data83488$AgeDrivingLicenseHolder <- tempAgeDrivingLicenseHolder83488$Title[match(data83488$AgeDrivingLicenseHolder, tempAgeDrivingLicenseHolder83488$Key)]
data83488$Region <- tempRegion83488$Title[match(data83488$Region, tempRegion83488$Key)]
data83488$Periods <- tempPeriods83488$Title[match(data83488$Periods, tempPeriods83488$Key)]

# Traffic Intensity
data83712 <- cbs_get_data("83712NED", RegioS = c("PV20", "PV21", "PV22"), Perioden = has_substring("JJ")) 

metadata83712 <- cbs_get_meta("83712NED") 

tempRegioS83712 <- metadata83712$RegioS
tempPerioden83712 <- metadata83712$Perioden

data83712$RegioS <- tempRegioS83712$Title[match(data83712$RegioS, tempRegioS83712$Key)]
data83712$Perioden <- tempPerioden83712$Title[match(data83712$Perioden, tempPerioden83712$Key)]

colnames(data83712)[1] = "provinces"
colnames(data83712)[2] = "Years"

data83712$provinces <- gsub(" (PV)", "", data83712$provinces, fixed = TRUE)
data83712$provinces <- gsub("Fryslân", "Friesland", data83712$provinces, fixed = TRUE)


# Lengte van rijkswegen

data70806 <- cbs_get_data("70806NED", SoortRijbanen = c("T001491", "A047342", "A047344", "A047345", "A047346", "A047348", "A047349", "A047350", "A047352", "A047354", "A047355", "A047356", "A047357", "A047358", "A047360", "A047362", "A047363", "A047364", "A047365", "A047366"), Perioden = c("2021JJ00"), RegioS = c("GM1680", "GM0059", "GM0060","GM0003", "GM0106", "GM0005", "GM0007","GM0063", "GM0055", "GM0009", "GM0064", "GM1681", "GM0109", "GM0065", "GM1891", "GM0010", "GM0058", "GM1979", "GM1651", "GM0114", "GM1722","GM0070", "GM1921", "GM1940", "GM0653", "GM0014", "GM0015", "GM0017", "GM0072", "GM0074", "GM1966", "GM0118", "GM0018",  "GM0079", "GM0022", "GM0080", "GM0081", "GM0082", "GM0140", "GM0024", "GM1663", "GM0025", "GM0083", "GM1908", "GM1987", "GM0119", "GM1731", "GM1952", "GM0104", "GM1970", "GM1699","GM1895", "GM0085", "GM0086", "GM0765", "GM1661", "GM0039","GM0088", "GM0051", "GM0040", "GM0090", "GM0091", "GM0037","GM1900", "GM0093", "GM1730", "GM0737", "GM0047", "GM0048","GM0096", "GM1949", "GM1969", "GM1701", "GM1950", "GM0098","GM0052", "GM0053", "GM1690", "GM0710", "GM0683", "GM0056"))
metadata70806 <- cbs_get_meta("70806NED")


tempPerioden70806<- metadata70806$Perioden
tempSoortrijbanen70806 <- metadata70806$SoortRijbanen

data70806$Perioden <- tempPerioden70806$Title[match(data70806$Perioden, tempPerioden70806$Key)]
data70806$SoortRijbanen <- tempSoortrijbanen70806$Title[match(data70806$SoortRijbanen, tempSoortrijbanen70806$Key)]

colnames(data70806)[1] <- "identificatie"


mapDatarijbanen <- municipalBoundaries %>%
  left_join(data70806, municipalBoundaries, by = "identificatie") 


## GREEN MOBILITY
elektrische_personenauto_provincie_2_ <- read_excel("elektrische_personenauto_provincie (2).xlsx", 
                                                    sheet = "Tabel 1")
data_elek_2 <- elektrische_personenauto_provincie_2_ %>%
  filter(Regio== "Friesland"|Regio== "Groningen"|Regio== "Drenthe")
colnames(data_elek_2)[2] = "Years"
colnames(data_elek_2)[3] = "Count"
colnames(data_elek_2)[1] = "Region"

data_elek_2 <- data_elek_2 %>%
  mutate(Vehicles = "Electric Vehicle")

data_elek_2 <- data_elek_2[c("Region", "Years", "Vehicles", "Count")]
data_elek_2 <- mutate(data_elek_2, across(everything(), as.factor))
data_elek_2$Count <- as.numeric(as.character(data_elek_2$Count))



## Voertuigen 1 
data85239 <- cbs_get_data("85239NED") 

#####Data Prep Idea 1#####

metadata85239 <- cbs_get_meta("85239NED")

tempVoertuigtype85239 <- metadata85239$Voertuigtype
tempBouwjaren85239 <- metadata85239$Bouwjaren
tempPerioden85239 <- metadata85239$Perioden

#Then, Replace Keys, by matching keys of temp table and imported table

data85239$Voertuigtype <- tempVoertuigtype85239$Title[match(data85239$Voertuigtype, tempVoertuigtype85239$Key)]
data85239$Bouwjaren <- tempBouwjaren85239$Title[match(data85239$Bouwjaren, tempBouwjaren85239$Key)]
data85239$Perioden <- tempPerioden85239$Title[match(data85239$Perioden, tempPerioden85239$Key)]
data85239 <- data85239 %>%  filter(Bouwjaren == "Totaal alle bouwjaren")

dataGroningen <- data85239 %>%
  select(Perioden, Bouwjaren, Voertuigtype, Groningen_2) %>% 
  mutate(Region = "Groningen") %>% 
  rename(Count = Groningen_2)

dataFriesland <- data85239 %>%
  select(Perioden, Bouwjaren, Voertuigtype, Fryslan_3) %>% mutate(Region = "Friesland") %>% rename(Count = Fryslan_3)
dataDrenthe <- data85239 %>% select(Perioden, Bouwjaren, Voertuigtype,Drenthe_4) %>% mutate(Region = "Drenthe") %>% rename(Count = Drenthe_4)

#Dataprep for combined lineplot
data85239new <- bind_rows(dataGroningen, dataFriesland, dataDrenthe)
data85239new <- data85239new %>% group_by(Perioden, Voertuigtype) %>% select(Perioden, Voertuigtype, Count, Region)

datafiltervoertuig <- data85239new %>%
  filter(Voertuigtype == "Totaal bedrijfsvoertuigen")
colnames(data85239new)[1] = "Years"
colnames(data85239new)[2] = "Vehicles"

data85239new <- data85239new[c("Region", "Years", "Vehicles", "Count")]



# VOERTUIGEN 2 SHINY
metadata85240 <- cbs_get_meta("85240NED")
data85240 <- cbs_get_data("85240NED") 
data85240 <- data85240 %>%
  filter(Provincie == "PV20  "| Provincie == "PV21  "| Provincie == "PV22  ") %>%
  filter(TenaamstellingEnLeeftijdParticulier == "T001191") %>%
  filter(Bouwjaar == "T001378")

tempPerioden85240 <- metadata85240$Perioden
tempProvincie85240 <- metadata85240$Provincie
data85240$Perioden <- tempPerioden85240$Title[match(data85240$Perioden, tempPerioden85240$Key)]
data85240$Provincie <- tempProvincie85240$Title[match(data85240$Provincie, tempProvincie85240$Key)]

dataVoertuigenmetbromfietskenteken <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, VoertuigenMetBromfietskenteken_1) %>% 
  mutate(Voertuigtype = "Voertuig met bromfietskenteken") %>% 
  rename(Count = VoertuigenMetBromfietskenteken_1)

dataSnorfiets <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, Snorfiets_2) %>% 
  mutate(Voertuigtype = "Snorfiets") %>% 
  rename(Count = Snorfiets_2)

dataBrommobiel <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, Bromfiets_3) %>% 
  mutate(Voertuigtype = "Bromfiets") %>% 
  rename(Count = Bromfiets_3)

dataElektrischebrommobiel <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, OverigeVoertuigenMetBromfietskenteken_5) %>% 
  mutate(Voertuigtype = "Overige voertuigen met bromfietskenteken") %>% 
  rename(Count = OverigeVoertuigenMetBromfietskenteken_5)


#Dataprep for combined lineplot
data85240new <- bind_rows(dataVoertuigenmetbromfietskenteken, dataSnorfiets, dataBrommobiel, dataElektrischebrommobiel)
data85240new <- data85240new %>% group_by(Perioden, Voertuigtype) %>% select(Perioden, Provincie, Count, Voertuigtype)

colnames(data85240new)[1] = "Years"
colnames(data85240new)[2] = "Region"
colnames(data85240new)[4] = "Vehicles"

data85240new <- data85240new[c("Region", "Years", "Vehicles", "Count")]
data85240new <- data85240new %>%
  mutate(Region = case_when(
    Region == "Groningen (PV)"~ "Groningen",
    Region == "Drenthe (PV)"~ "Drenthe",
    Region == "Fryslân (PV)"~ "Friesland"))



# Voertuigen 3
data85237 <- cbs_get_data("85237NED") 


metadata85237 <- cbs_get_meta("85237NED")

tempPerioden85237 <- metadata85237$Perioden
tempBouwjaar85237 <- metadata85237$Bouwjaar
tempBrandstofsoort85237 <- metadata85237$Brandstofsoort

#Then, Replace Keys, by matching keys of temp table and imported table
data85237$Perioden <- tempPerioden85237$Title[match(data85237$Perioden, tempPerioden85237$Key)]
data85237$Bouwjaar <- tempBouwjaar85237$Title[match(data85237$Bouwjaar, tempBouwjaar85237$Key)]


data85237 <- data85237 %>%
  filter(Bouwjaar == "Totaal alle bouwjaren") %>% 
  select(Perioden, TotaalNederland_1, Groningen_2, Fryslan_3, Drenthe_4)

dataGroningen1 <- data85237 %>%
  select(Perioden, Groningen_2) %>%
  mutate(Region = "Groningen") %>%
  rename(Count = Groningen_2)

dataFriesland1 <- data85237 %>%
  select(Perioden, Fryslan_3) %>%
  mutate(Region = "Friesland") %>%
  rename(Count = Fryslan_3)

dataDrenthe1 <- data85237 %>%
  select(Perioden, Drenthe_4) %>%
  mutate(Region = "Drenthe") %>%
  rename(Count = Drenthe_4)

data85237new <- bind_rows(dataGroningen1, dataFriesland1, dataDrenthe1)
data85237new <- data85237new %>% 
  group_by(Perioden) %>% 
  select(Perioden, Count, Region)

data85237new <- data85237new %>%
  mutate(Vehicles = "Normal car")

colnames(data85237new)[1] = "Years"

#Dataprep for combined lineplot
datacombined <- bind_rows(data_elek_2 , data85240new, data85239new, data85237new)


datacombined <- datacombined %>%
  mutate(Vehicles = case_when(
    Vehicles == "Voertuig met bromfietskenteken" ~ "All mopeds",
    Vehicles == "Snorfiets" ~ "Moped(25km/h)",
    Vehicles == "Bromfiets" ~ "Moped(45km/h)",
    Vehicles == "Overige voertuigen met bromfietskenteken" ~ "Remaining mopeds",
    Vehicles == "Totaal bedrijfsvoertuigen" ~ "All commercial vehicles",
    Vehicles == "Bestelauto" ~ "Van",
    Vehicles == "Vrachtauto (excl. trekker voor oplegger)" ~ "Truck",
    Vehicles == "Trekker voor oplegger" ~ "Tractor",
    Vehicles == "Speciaal voertuig" ~ "Special vehicle",
    Vehicles == "Bus" ~ "Bus",
    Vehicles == "Aanhangwagen" ~ "Trailer",
    Vehicles == "Oplegger" ~ "Semi trailer", 
    Vehicles == "Electric Vehicle" ~ "Electric vehicle",
    Vehicles == "Normal car" ~ "Normal car")) %>% 
  group_by(Region)


## FUEL TYPES
data85239 <- cbs_get_data("85239NED") 


#####Data Prep Idea 1#####

metadata85239 <- cbs_get_meta("85239NED")

tempVoertuigtype85239 <- metadata85239$Voertuigtype
tempBouwjaren85239 <- metadata85239$Bouwjaren
tempPerioden85239 <- metadata85239$Perioden

#Then, Replace Keys, by matching keys of temp table and imported table
data85239$Voertuigtype <- tempVoertuigtype85239$Title[match(data85239$Voertuigtype, tempVoertuigtype85239$Key)]
data85239$Bouwjaren <- tempBouwjaren85239$Title[match(data85239$Bouwjaren, tempBouwjaren85239$Key)]
data85239$Perioden <- tempPerioden85239$Title[match(data85239$Perioden, tempPerioden85239$Key)]

data85239 <- data85239 %>%  filter(Bouwjaren == "Totaal alle bouwjaren")



dataFueltypeCommercial <- data85239 %>%
  select(Voertuigtype, Perioden, Benzine_15, Diesel_16, LPG_17, Elektriciteit_18, CNG_19, GeenOverigeOnbekendeBrandstof_20, Totaal_21)

colnames(dataFueltypeCommercial)[8] = "OverigOnbekend_20"

# All normal Vehicles
data85237 <- cbs_get_data("85237NED") 

#####Data Prep Idea 1#####

metadata85237 <- cbs_get_meta("85237NED")

tempPerioden85237 <- metadata85237$Perioden
tempBouwjaar85237 <- metadata85237$Bouwjaar


#Then, Replace Keys, by matching keys of temp table and imported table
data85237$Perioden <- tempPerioden85237$Title[match(data85237$Perioden, tempPerioden85237$Key)]
data85237$Bouwjaar <- tempBouwjaar85237$Title[match(data85237$Bouwjaar, tempBouwjaar85237$Key)]

data85237 <- data85237 %>%
  filter(Bouwjaar == "Totaal alle bouwjaren")

dataFueltypeNormal <- data85237 %>%
  select(Bouwjaar, Perioden, Benzine_15, Diesel_16, LPG_17, Elektriciteit_18, CNG_19, OverigOnbekend_20, Totaal_21)

dataFueltypeNormal <- dataFueltypeNormal %>%
  mutate(Voertuigtype = "Personal vehicle")

dataFueltypeNormal <- dataFueltypeNormal[c("Voertuigtype", "Perioden", "Benzine_15", "Diesel_16", "LPG_17", "Elektriciteit_18", "CNG_19", "OverigOnbekend_20", "Totaal_21")]


dataFueltypes <- bind_rows(dataFueltypeNormal, dataFueltypeCommercial)
colnames(dataFueltypes)[1] = "Vehicletype"
colnames(dataFueltypes)[2] = "Years"



dataBenzine <- dataFueltypes %>%
  select(Vehicletype, Years, Benzine_15) %>% 
  mutate(Fueltype = "Benzine") %>% 
  rename(Count = Benzine_15)

dataDiesel <- dataFueltypes %>%
  select(Vehicletype, Years, Diesel_16) %>% 
  mutate(Fueltype = "Diesel") %>% 
  rename(Count = Diesel_16)

dataLPG <- dataFueltypes %>%
  select(Vehicletype, Years, LPG_17) %>% 
  mutate(Fueltype = "LPG") %>% 
  rename(Count = LPG_17)

dataElektriciteit <- dataFueltypes %>%
  select(Vehicletype, Years, Elektriciteit_18) %>% 
  mutate(Fueltype = "Elektriciteit") %>% 
  rename(Count = Elektriciteit_18)

dataCNG <- dataFueltypes %>%
  select(Vehicletype, Years, CNG_19) %>% 
  mutate(Fueltype = "CNG") %>% 
  rename(Count = CNG_19)

dataOnbekend <- dataFueltypes %>%
  select(Vehicletype, Years, OverigOnbekend_20) %>% 
  mutate(Fueltype = "Unknown") %>% 
  rename(Count = OverigOnbekend_20)

datafueltypes1 <- bind_rows(dataBenzine, dataDiesel, dataLPG, dataElektriciteit, dataCNG, dataOnbekend)

datafueltypes1 <- datafueltypes1 %>%
  mutate(Vehicletype = case_when(
    Vehicletype == "Totaal bedrijfsvoertuigen" ~ "All commercial vehicles",
    Vehicletype == "Bestelauto" ~ "Van",
    Vehicletype == "Vrachtauto (excl. trekker voor oplegger)" ~ "Truck",
    Vehicletype == "Trekker voor oplegger" ~ "Tractor",
    Vehicletype == "Speciaal voertuig" ~ "Special vehicle",
    Vehicletype == "Bus" ~ "Bus",
    Vehicletype == "Aanhangwagen" ~ "Trailer",
    Vehicletype == "Oplegger" ~ "Semi trailer", 
    Vehicletype == "Personal vehicle" ~ "Personal vehicle"))
