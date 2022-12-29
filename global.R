library(dplyr)
library(shiny)
library(shinydashboard)
library(sf)
library(cbsodataR)
library(ggplot2)
library(forcats)
library(ggiraph)
library(plotly)

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
  select = c(
    "TravelMotives",
    "Population",
    "TravelModes",
    "RegionCharacteristics",
    "Periods",
    "Trips_4",
    "DistanceTravelled_5"
  )
)


tempPeriods84710 <- metadata84710$Periods
tempMotives84710 <- metadata84710$TravelMotives
tempModes84710 <- metadata84710$TravelModes
tempRegion84710 <- metadata84710$RegionCharacteristics


data84710$Periods <- tempPeriods84710$Title[match(data84710$Periods, tempPeriods84710$Key)]
data84710$TravelMotives <- tempMotives84710$Title[match(data84710$TravelMotives, tempMotives84710$Key)]
data84710$TravelModes <- tempModes84710$Title[match(data84710$TravelModes, tempModes84710$Key)]
data84710$RegionCharacteristics <- tempRegion84710$Title[match(data84710$RegionCharacteristics, tempRegion84710$Key)]



##### Data 85055ENG ############################################################
# Timeframe Data: Travel Purpose
# IDEA 6

metadata85055 <- cbs_get_meta("85055ENG")

data85055 <- cbs_get_data(id = "85055ENG", TripCharacteristics = c("2030851", "2030871", "2030891", "2030911", "2030931", "2030950", "2031000", "2031090", "2031100", "2031110", "2031120", "2031130", "2031140", "2031150", "2031160", "2031170", "2031180", "2031190", "2031200", "2031210", "2031220", "2031230", "2031240", "2031250", "2031260", "2031270", "2820701", "2820702", "2820704", "2820705", "2820706","A025261", "A025262", "A025263", "A025264", "A025265", "A025266", "A025267", "A025268"), Population = "A048710", TravelPurposes = c("2030170","2030190","2030200","2030210","2030220","2030230","2030240","2030250","2820740","T001080"), Margins = "MW00000", RegionCharacteristics = c("PV20    ", "PV21    ", "PV22    ", "LD01    "))


##Data Prep Idea 6##
#temp tables
temp_Region85055 <- metadata85055$RegionCharacteristics
temp_Periods85055 <- metadata85055$Periods
temp_TripCharacteristics85055 <- metadata85055$TripCharacteristics
temp_TravelPurposes85055 <- metadata85055$TravelPurposes
temp_Population85055 <- metadata85055$Population

#Matching and replacing Keys for Keys
data85055$RegionCharacteristics <- temp_Region85055$Title[match(data85055$RegionCharacteristics, temp_Region85055$Key)]
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

data85056$RegionCharacteristics <- temp_Region85056$Title[match(data85056$RegionCharacteristics, temp_Region85056$Key)]
data85056$Periods <- temp_Periods85056$Title[match(data85056$Periods, temp_Periods85056$Key)]
data85056$TripCharacteristics <-
  temp_TripCharacteristics85056$Title[match(data85056$TripCharacteristics,
                                            temp_TripCharacteristics85056$Key)]
data85056$ModesOfTravel <- temp_ModesOfTravel85056$Title[match(data85056$ModesOfTravel, temp_ModesOfTravel85056$Key)]
data85056$Population <- temp_Population85056$Title[match(data85056$Population, temp_Population85056$Key)]

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
data80305$Periods <- tempPeriods$Title[
  match(data80305$Periods, tempPeriods$Key)]

#New Column "Municipality" made, "Region" is kept to match with shapefile
data80305$Municipality <- tempRegion$Title[
  match(data80305$Regions, tempRegion$Key)]

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
                                   c(st_union(provinces), 
                                     renmae(provinces = c..Drenthe....Friesland....Groningen..)))


# Joining Data
# Joining Municipal Shapefile by Municipality  and calculating a new column
mapData <- municipalBoundaries %>%
  left_join(data80305, by = c(identificatie="Regions")) %>%
  mutate(Avg15 = (
    DistanceToGPPractice_1 + DistanceToGPPost_5 +
      DistanceToPharmacy_6 + DistanceToHospital_11 +
      DistanceToLargeSupermarket_20 + DistanceToShopForOtherDailyFood_24 + 
      DistanceToDepartmentStore_28 + DistanceToCafeEtc_32 +
      DistanceToRestaurant_40 + DistanceToDaycareCentres_48 + 
      DistanceToOutOfSchoolCare_52 + DistanceToSchool_60 + 
      DistanceToSchool_64 + DistanceToTrainStationsAllTypes_101
  ) / 15
  ) 
