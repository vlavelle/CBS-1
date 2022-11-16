library(shiny)
library(cbsodataR)
library(ggplot2)
library(tidyverse)
library(sf)

###Data#for#Map#################################################################
#These should probably be in a global file!

metadata <- cbs_get_meta("80305ENG") 

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


#####Data Prep#####
#First, we have to make temp tables from the Metadata, these include the
#Key and Title column. We will use those to "match" and replace the Keys
#with Titles in our Dataset.
tempPeriods <- metadata$Periods
tempRegion <- metadata$Regions

#These bits do the matching and replacing!
#Periods is replaced
dataImport$Periods <- tempPeriods$Title[
  match(dataImport$Periods, tempPeriods$Key)
]
#New Column "Municipality"made, "Region" is kept to match with shapefile
dataImport$Municipality <- tempRegion$Title[
  match(dataImport$Regions, tempRegion$Key)
]
#####Shapefile Import######
#Shapefile Import, now new and improved!
#Aestetic issue: The island boundaries are gone. very sad.
municipalBoundaries <- st_read("https://service.pdok.nl/kadaster/bestuurlijkegebieden/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson%3B%20subtype%3Dgeojson&typeName=bestuurlijkegebieden:Gemeentegebied")

#####Joining Data#########
#Joining Shapefile by Municipality  
dataImport <- municipalBoundaries %>%
  left_join(dataImport, by=c(identificatie="Regions"))

###DataIdea1##########################################################
#Data in the same form as the data above, for Idea 1

#It gives an error, but the data seems to be complete, so no clue why.
metadata1 <- cbs_get_meta("84710ENG") 

dataImport1 <- cbs_get_data(
  id = "84710ENG",
  Periods = has_substring("JJ"),
  RegionCharacteristics = c("NL01    ", "LD01    ", "PV20    ",
                            "PV21    ", "PV22    "),
  select = c("TravelMotives", "Population", "TravelModes",
             "RegionCharacteristics", "Periods", "Trips_4", 
             "DistanceTravelled_5")
)


#####Data Prep1#####
#First, we have to make temp tables from the Metadata, these include the
#Key and Title column. We will use those to "match" and replace the Keys
#with Titles in our Dataset.
tempPeriods1 <- metadata1$Periods
tempMotives1 <- metadata1$TravelMotives
tempModes1 <- metadata1$TravelModes
tempRegion1 <- metadata1$RegionCharacteristics

##These bits do the matching and replacing!
#Periods
dataImport1$Periods <- tempPeriods1$Title[
  match(dataImport1$Periods, tempPeriods1$Key)
]
#TravelMotives
dataImport1$TravelMotives <- tempMotives1$Title[
  match(dataImport1$TravelMotives, tempMotives1$Key)
]
#TravelModes
dataImport1$TravelModes <- tempModes1$Title[
  match(dataImport1$TravelModes, tempModes1$Key)
]
#RegionCharacteristics
dataImport1$RegionCharacteristics <- tempRegion1$Title[
  match(dataImport1$RegionCharacteristics, tempRegion1$Key)
]



