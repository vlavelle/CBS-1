library(dplyr)
library(shiny)
library(shinydashboard)
library(sf)
library(cbsodataR)
library(ggplot2)

# set wd (at top of screen click session and choose how you set your wd)
# shape file must be in same location as where you have your app saved
setwd("~/R/CBS Project/Dashboard1/")

### IDEA 2 ##

# this data is currently used to make a bar graph
# the bar graph is not informative 
# we will fix this later

data709 <- cbs_get_data(id = "84709NED", Populatie = "A048710", Geslacht = "T001038", 
Vervoerwijzen = c("T001093", "A048583", "A048584", "A018981", "A018984"), 
    Marges = "MW00000", RegioS = "NL01    ", 
select = c("Populatie", "Geslacht", "Persoonskenmerken", "Vervoerwijzen", "Marges", 
         "Perioden", "RegioS", "Verplaatsingen_1", "Afstand_2", "Reisduur_3"))
data84709 <- data709
metadata84709 <- cbs_get_meta("84709NED")
tempPerioden84709 <- metadata84709$Perioden
tempVervoerwijzen84709 <- metadata84709$Vervoerwijzen
tempRegioS84709 <- metadata84709$RegioS
tempPersoonskenmerken84709 <- metadata84709$Persoonskenmerken

#Then, Replace Keys, by matching keys of temp table and imported table
data84709$Perioden <- tempPerioden84709$Title[match(data84709$Perioden, tempPerioden84709$Key)]
data84709$Vervoerwijzen <- tempVervoerwijzen84709$Title[match(data84709$Vervoerwijzen, tempVervoerwijzen84709$Key)]
data84709$RegioS <- tempRegioS84709$Title[match(data84709$RegioS, tempRegioS84709$Key)]
data84709$Persoonskenmerken <- tempPersoonskenmerken84709$Title[match(data84709$Persoonskenmerken, tempPersoonskenmerken84709$Key)]

# filtering to only have info for North Netherlands
data84709 <- data84709 %>% filter(RegioS %in% c("Groningen (PV)", "Fryslân (PV)", "Drenthe (PV)", "Noord-Nederland (LD)"))



### Dataset 80305ENG ###
# this dataset is combined with the map shape file to colour the interactive map
metadata80305 <- cbs_get_meta("80305ENG") 

dataImport <- cbs_get_data(
  id = "80305ENG",
  Periods = has_substring("JJ"),
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
             "DistanceToPharmacy_6", "DistanceToHospital_7", "DistanceToHospital_11", 
             "DistanceToHealthCentre_15", "DistanceToLargeSupermarket_20", 
             "DistanceToShopForOtherDailyFood_24", "DistanceToDepartmentStore_28", 
             "DistanceToCafeEtc_32", "DistanceToCafeteriaEtc_36", 
             "DistanceToRestaurant_40", "DistanceToHotelEtc_44", 
             "DistanceToDaycareCentres_48", "DistanceToOutOfSchoolCare_52", 
             "DistanceToSchool_56", "DistanceToSchool_60", "DistanceToSchool_64", 
             "DistanceToSchool_68", "DistanceToPublicGreenTotal_87", 
             "DistanceToPark_88", "DistanceToRecreationalTerrain_89", 
             "DistanceToForest_90", "DistanceToOpenNatTerrainTotal_91", 
             "DistanceToOpenDryNaturalTerrain_92", "DistanceToOpenWetNaturalTerrain_93", 
             "DistanceToSemiPublicGreenTotal_94", "DistanceToSportsArea_95", 
             "DistanceToRecreationalArea_97", "DistanceToCemetery_98", 
             "DistanceToRecreationalInlandWaters_99", "DistanceToTrainStationsAllTypes_101", 
             "DistanceToImportantTransferStation_102", "DistanceToLibrary_103", 
             "DistanceToSwimmingPool_104", "DistanceToMuseum_106", 
             "TotalDistanceToPerformingArts_110", "DistanceToPopMusicVenue_114", 
             "DistanceToCinema_115")
)

data80305 <- dataImport
#####Data Prep#####

tempPeriods80305 <- metadata80305$Periods
tempRegion80305 <- metadata80305$Regions


data80305$Periods <- tempPeriods80305$Title[match(data80305$Periods, tempPeriods80305$Key)]
data80305$Municipality <- tempRegion80305$Title[match(data80305$Regions, tempRegion80305$Key)]

#####Shapefile Import######
#Shapefile Import, now new and improved!
#Aestetic issue: The island boundaries are gone. very sad.
municipalBoundaries <- st_read("https://service.pdok.nl/kadaster/bestuurlijkegebieden/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson%3B%20subtype%3Dgeojson&typeName=bestuurlijkegebieden:Gemeentegebied")

#####Joining Data#########
#Joining Shapefile by Municipality  
data80305 <- municipalBoundaries %>%
  left_join(data80305, by=c(identificatie="Regions"))




###DataIdea1##########################################################
#Data in the same form as the data above, for Idea 1

#It gives an error, but the data seems to be complete, so no clue why.
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


#####Data Prep Idea 1#####

tempPeriods84710 <- metadata84710$Periods
tempMotives84710 <- metadata84710$TravelMotives
tempModes84710 <- metadata84710$TravelModes
tempRegion84710 <- metadata84710$RegionCharacteristics

##These bits do the matching and replacing!
data84710$Periods <- tempPeriods84710$Title[match(data84710$Periods, tempPeriods84710$Key)]
data84710$TravelMotives <- tempMotives84710$Title[match(data84710$TravelMotives, tempMotives84710$Key)]
data84710$TravelModes <- tempModes84710$Title[match(data84710$TravelModes, tempModes84710$Key)]
data84710$RegionCharacteristics <- tempRegion84710$Title[match(data84710$RegionCharacteristics, tempRegion84710$Key)]



# IDEA 6 #
metadata85055 <- cbs_get_meta("85055ENG")

data85055 <- cbs_get_data(
  id = "85055ENG", 
  TripCharacteristics = c(
    "2031090", "2031100", "2031110", "2031120", "2031130", "2031140", "2031150", 
    "2031160", "2031170", "2031180", "2031190", "2031200", "2031210", "2031220", 
    "2031230", "2031240", "2031250", "2031260", "2031270"
  ), 
  Population = "A048710", 
  TravelPurposes = "2030170", 
  Margins = "MW00000",     
  RegionCharacteristics = has_substring("NL") | 
    c("LD01    ",     "PV20    ", "PV21    ", "PV22    ")
)

#####Data Prep Idea 6#####
#temp tables
temp_Region85055 <- metadata85055$RegionCharacteristics
temp_Periods85055 <- metadata85055$Periods
temp_TripCharacteristics85055 <- metadata85055$TripCharacteristics
temp_TravelPurposes85055 <- metadata85055$TravelPurposes
temp_Population85055 <- metadata85055$Population

#Matching and replacing Keys for Keys
data85055$RegionCharacteristics <- temp_Region85055$Title[match(data85055$RegionCharacteristics, temp_Region85055$Key)]
data85055$Periods <- temp_Periods85055$Title[match(data85055$Periods, temp_Periods85055$Key)]
data85055$TripCharacteristics <-
  temp_TripCharacteristics85055$Title[match(data85055$TripCharacteristics,
                                            temp_TripCharacteristics85055$Key)]
data85055$TravelPurposes <- temp_TravelPurposes85055$Title[match(data85055$TravelPurposes, temp_TravelPurposes85055$Key)]
data85055$Population <- temp_Population85055$Title[match(data85055$Population, temp_Population85055$Key)]

