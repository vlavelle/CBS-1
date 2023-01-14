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

################ Mobility Indicators ###########################################
##### Data 84710 - Regions in years Tabs #######################################
# Regions in years, Both Purpose of Travel and Mode of Travel first tab
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

##Data Prep##
#temp tables
tempPeriods84710 <- metadata84710$Periods
tempMotives84710 <- metadata84710$TravelMotives
tempModes84710 <- metadata84710$TravelModes

#Matching and replacing Keys for Keys
data84710$Periods <- tempPeriods84710$Title[match(data84710$Periods, tempPeriods84710$Key)]
data84710$TravelMotives <- tempMotives84710$Title[match(data84710$TravelMotives, tempMotives84710$Key)]
data84710$TravelModes <- tempModes84710$Title[match(data84710$TravelModes, tempModes84710$Key)]
#Fixing Region names manually
data84710$RegionCharacteristics <- recode(data84710$RegionCharacteristics, 
                                          "NL01    " = "The Netherlands",
                                          "LD01    " = "Northern Netherlands",
                                          "PV20    " = "Groningen",
                                          "PV21    " = "Friesland",
                                          "PV22    " = "Drenthe")

##### Data 85055 - Timeframe: Purpose of Travel ################################
# Purpose of Travel > Regions in years: for a period, in a timeframe
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

##Data Prep##
#temp tables
temp_Region85055 <- metadata85055$RegionCharacteristics
temp_Periods85055 <- metadata85055$Periods
temp_TripCharacteristics85055 <- metadata85055$TripCharacteristics
temp_TravelPurposes85055 <- metadata85055$TravelPurposes
temp_Population85055 <- metadata85055$Population

#Matching and replacing Keys for Keys
data85055$Periods <- temp_Periods85055$Title[match(data85055$Periods, temp_Periods85055$Key)]
data85055$TripCharacteristics <- temp_TripCharacteristics85055$Title[match(data85055$TripCharacteristics, temp_TripCharacteristics85055$Key)]
data85055$TravelPurposes <- temp_TravelPurposes85055$Title[match(data85055$TravelPurposes, temp_TravelPurposes85055$Key)]
data85055$Population <- temp_Population85055$Title[match(data85055$Population, temp_Population85055$Key)]
#Fixing Region names manually
data85055$RegionCharacteristics <- recode(data85055$RegionCharacteristics,
                                          "LD01    " = "Northern Netherlands",
                                          "PV20    " = "Groningen",
                                          "PV21    " = "Friesland",
                                          "PV22    " = "Drenthe")
#Mutations
data85055 <- data85055 %>% mutate(
  Timeframe = case_when(
    grepl("Distance:", data85055$TripCharacteristics) ~ "Distance",
    grepl("Time travelled:", data85055$TripCharacteristics) ~ "Time travelled",
    grepl("Trip in", data85055$TripCharacteristics) ~ "Month",
    grepl("Departure time:", data85055$TripCharacteristics) ~ "Departure time",
    grepl("day", data85055$TripCharacteristics) ~ "Day of the week"
  )
) %>% 
  select(TripCharacteristics, Timeframe, TravelPurposes, RegionCharacteristics, 
         Periods, AverageDistanceTravelledPerTrip_1, AverageTravelTimePerTrip_2)  

data85055$TripCharacteristics <- sub('Departure time: ', '', data85055$TripCharacteristics)
data85055$TripCharacteristics <- sub('Trip in ', '', data85055$TripCharacteristics)

##### Data 85056 - Timeframe: Mode of Travel ###################################
# Mode of Travel > Regions in years: for a period, in a timeframe
metadata85056 <- cbs_get_meta("85056ENG")
data85056 <- cbs_get_data(id = "85056ENG", 
                          TripCharacteristics = c(
                            "2030851", "2030871", "2030891", "2030911", "2030931", 
                            "2030950", "2031000", "2031090", "2031100", "2031110", 
                            "2031120", "2031130", "2031140", "2031150", "2031160", 
                            "2031170", "2031180", "2031190", "2031200", "2031210", 
                            "2031220", "2031230", "2031240", "2031250", "2031260", 
                            "2031270", "2820701", "2820702", "2820704", "2820705", 
                            "2820706", "A025261", "A025262", "A025263", "A025264", 
                            "A025265", "A025266", "A025267", "A025268"
                          ), 
                          Population = "A048710", 
                          ModesOfTravel = c(
                            "T001093","A048583","A048584","A018981","A018982",
                            "A018984","A018985","A018986"
                          ),
                          Margins = "MW00000", 
                          RegionCharacteristics = c(
                            "PV20    ", "PV21    ", "PV22    ", "LD01    "
                          )
)

##Data Prep##
#temp tables
temp_Region85056 <- metadata85056$RegionCharacteristics
temp_Periods85056 <- metadata85056$Periods
temp_TripCharacteristics85056 <- metadata85056$TripCharacteristics
temp_ModesOfTravel85056 <- metadata85056$ModesOfTravel
temp_Population85056 <- metadata85056$Population

#Matching and replacing Keys for Keys
data85056$Periods <- temp_Periods85056$Title[match(data85056$Periods, temp_Periods85056$Key)]
data85056$TripCharacteristics <- temp_TripCharacteristics85056$Title[match(data85056$TripCharacteristics, temp_TripCharacteristics85056$Key)]
data85056$ModesOfTravel <- temp_ModesOfTravel85056$Title[match(data85056$ModesOfTravel, temp_ModesOfTravel85056$Key)]
data85056$Population <- temp_Population85056$Title[match(data85056$Population, temp_Population85056$Key)]
#Fixing Region names manually
data85056$RegionCharacteristics <- recode(data85056$RegionCharacteristics,
                                          "LD01    " = "Northern Netherlands",
                                          "PV20    " = "Groningen",
                                          "PV21    " = "Friesland",
                                          "PV22    " = "Drenthe")
#mutation for aliasing
data85056 <- data85056 %>% 
  mutate(Timeframe = case_when(
    grepl("Distance:", data85056$TripCharacteristics) ~ "Distance",
    grepl("Time travelled:", data85056$TripCharacteristics) ~ "Time travelled",
    grepl("Trip in", data85056$TripCharacteristics) ~ "Month",
    grepl("Departure time:", data85056$TripCharacteristics) ~ "Departure time",
    grepl("day", data85056$TripCharacteristics) ~ "Day of the week"
  )) %>% 
  select(
    TripCharacteristics, Timeframe, ModesOfTravel, RegionCharacteristics, 
    Periods, AverageDistanceTravelledPerTrip_1, AverageTravelTimePerTrip_2
  )  

data85056$TripCharacteristics <- sub('Departure time: ', '', data85056$TripCharacteristics)
data85056$TripCharacteristics <- sub('Trip in ', '', data85056$TripCharacteristics)

##### Data 84709 - Personal Characteristics ####################################
# Personal Characteristics
metadata84709 <- cbs_get_meta("84709NED")
datatemp <- cbs_get_data(
  '84709NED', 
  Persoonskenmerken = c( 
    "T009002", "51511  ", "52020  ", "53105  ", "53500  ", "53705  ", "53850  ", 
    "53925  ", "21600  ", "1012600", "2012655", "2012657","1014752", "1014753", 
    "1014754", "1014755","1014756", "2030530", "2030540", "2030550","2018700", 
    "2018740", "2018790", "2017560","2017565", "2012751", "2017572", "2820713", 
    "2017576", "2017500", "A048766", "A048767","A048768", "A048769", "A048770"
  ), 
  Vervoerwijzen = c(
    "T001093", "A048583", "A048584", "A018981", "A018982", "A018984", "A018985", "A018986"), 
  RegioS = c("NL01    ","LD01    ","PV20    ","PV21    ", "PV22    "), 
  Geslacht = "T001038", 
  Marges = c("MW00000"), 
  Populatie = c("A048710"), 
  select = c(
    "Persoonskenmerken", "RegioS", "Perioden", "Vervoerwijzen", 
    "Verplaatsingen_1", "Afstand_2", "Reisduur_3"
  )
)

##Data Prep##
#temp tables
data84709 <- datatemp
temp_Perioden84709 <- metadata84709$Perioden #Perioden
temp_Persoonskenmerken84709 <- metadata84709$Persoonskenmerken # Persoonskenmerken
temp_Vervoerwijzen84709 <- metadata84709$Vervoerwijzen

#Matching and replacing Keys for Keys
data84709$Perioden <- temp_Perioden84709$Title[match(data84709$Perioden, temp_Perioden84709$Key)]
data84709$Persoonskenmerken <- temp_Persoonskenmerken84709$Title[match(data84709$Persoonskenmerken, temp_Persoonskenmerken84709$Key)]
data84709$Vervoerwijzen <- temp_Vervoerwijzen84709$Title[match(data84709$Vervoerwijzen, temp_Vervoerwijzen84709$Key)]
#Fixing Region names manually
data84709$RegioS <- recode(data84709$RegioS, 
                           "NL01    " = "The Netherlands",
                           "LD01    " = "Northern Netherlands",
                           "PV20    " = "Groningen",
                           "PV21    " = "Friesland",
                           "PV22    " = "Drenthe")

#mutation and translating from Dutch
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
data84709$Persoonskenmerken <- sub('Leeftijd:', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Migratieachtergrond:', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Gestandaardiseerd inkomen:', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('OV-Studentenkaart: ', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Onderwijsniveau: ', '', data84709$Persoonskenmerken)
data84709$Persoonskenmerken <- sub('Participatie: ', '', data84709$Persoonskenmerken)

##### Data 83488 - Drivers License #############################################
## Drivers License
data83488 <- cbs_get_data('83488ENG', 
                          Region = c("PV20  ","PV21  ", "PV22  "))
metadata83488 <- cbs_get_meta("83488ENG")

## Data Prep##
# temp tables
tempCategoryDrivingLicense83488 <- metadata83488$CategoryDrivingLicence
tempAgeDrivingLicenseHolder83488 <- metadata83488$AgeDrivingLicenseHolder
tempPeriods83488 <- metadata83488$Periods

# Matching and replacing Keys for Keys
data83488$CategoryDrivingLicence <- tempCategoryDrivingLicense83488$Title[match(data83488$CategoryDrivingLicence, tempCategoryDrivingLicense83488$Key)]
data83488$AgeDrivingLicenseHolder <- tempAgeDrivingLicenseHolder83488$Title[match(data83488$AgeDrivingLicenseHolder, tempAgeDrivingLicenseHolder83488$Key)]
data83488$Periods <- tempPeriods83488$Title[match(data83488$Periods, tempPeriods83488$Key)]
# Fixing Region names manually
data83488$Region <- recode(data83488$Region,
                           "PV20  " = "Groningen",
                           "PV21  " = "Friesland",
                           "PV22  " = "Drenthe")




################ Green Mobility ################################################
##### Data from Excel ############################################################
# data from excel
elektrische_personenauto_provincie_2_ <- read_excel("elektrische_personenauto_provincie (2).xlsx", 
                                                    sheet = "Tabel 1")
data_elek_2 <- elektrische_personenauto_provincie_2_ %>%
  filter(Regio== "Friesland"|Regio== "Groningen"|Regio== "Drenthe")
colnames(data_elek_2)[2] = "Years" #Changing colnames
colnames(data_elek_2)[3] = "Count"
colnames(data_elek_2)[1] = "Region"

data_elek_2 <- data_elek_2 %>% #Add extra column to dataframe
  mutate(Vehicles = "Electric Vehicle")

data_elek_2 <- data_elek_2[c("Region", "Years", "Vehicles", "Count")]
data_elek_2 <- mutate(data_elek_2, across(everything(), as.factor))
data_elek_2$Count <- as.numeric(as.character(data_elek_2$Count))

##### Data 85239 - Vehicles 1 ##################################################
## VOERTUIGEN/ Vehicles 1 
metadata85239 <- cbs_get_meta("85239NED")
data85239 <- cbs_get_data("85239NED") #Please filter the import!

##Data Prep##
# temp tables
tempVoertuigtype85239 <- metadata85239$Voertuigtype
tempBouwjaren85239 <- metadata85239$Bouwjaren
tempPerioden85239 <- metadata85239$Perioden

# Matching and replacing Keys for Keys
data85239$Voertuigtype <- tempVoertuigtype85239$Title[match(data85239$Voertuigtype, tempVoertuigtype85239$Key)]
data85239$Bouwjaren <- tempBouwjaren85239$Title[match(data85239$Bouwjaren, tempBouwjaren85239$Key)]
data85239$Perioden <- tempPerioden85239$Title[match(data85239$Perioden, tempPerioden85239$Key)]
data85239 <- data85239 %>%  filter(Bouwjaren == "Totaal alle bouwjaren")

# Mutations
#Making different datasets to change regions from columns to rows

dataGroningen <- data85239 %>%
  select(Perioden, Bouwjaren, Voertuigtype, Groningen_2) %>% 
  mutate(Region = "Groningen") %>% 
  rename(Count = Groningen_2)

dataFriesland <- data85239 %>%
  select(Perioden, Bouwjaren, Voertuigtype, Fryslan_3) %>% 
  mutate(Region = "Friesland") %>% 
  rename(Count = Fryslan_3)

dataDrenthe <- data85239 %>% 
  select(Perioden, Bouwjaren, Voertuigtype,Drenthe_4) %>% 
  mutate(Region = "Drenthe") %>% 
  rename(Count = Drenthe_4)

#Dataprep for combined lineplot
data85239new <- bind_rows(dataGroningen, dataFriesland, dataDrenthe)
data85239new <- data85239new %>% 
  group_by(Perioden, Voertuigtype) %>% 
  select(Perioden, Voertuigtype, Count, Region)

datafiltervoertuig <- data85239new %>% # Change colnames
  filter(Voertuigtype == "Totaal bedrijfsvoertuigen")
colnames(data85239new)[1] = "Years"
colnames(data85239new)[2] = "Vehicles"

data85239new <- data85239new[c("Region", "Years", "Vehicles", "Count")]

##### Data 85240 - Vehicles 2###################################################
# VOERTUIGEN/Vehicles 2 
metadata85240 <- cbs_get_meta("85240NED")
data85240 <- cbs_get_data("85240NED") #Please filter the import!

# Please filter the import!
data85240 <- data85240 %>%
  filter(Provincie == "PV20  "| Provincie == "PV21  "| Provincie == "PV22  ") %>%
  filter(TenaamstellingEnLeeftijdParticulier == "T001191") %>%
  filter(Bouwjaar == "T001378")

# Temp table
tempPerioden85240 <- metadata85240$Perioden
# key matching
data85240$Perioden <- tempPerioden85240$Title[match(data85240$Perioden, tempPerioden85240$Key)]
# fixing Region names manually
data85240$Provincie <- recode(data85240$Provincie,
                              "PV20  " = "Groningen",
                              "PV21  " = "Friesland",
                              "PV22  " = "Drenthe")

#Making different datasets to change regions from columns to rows

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
data85240new <- data85240new %>% 
  group_by(Perioden, Voertuigtype) %>% 
  select(Perioden, Provincie, Count, Voertuigtype)

colnames(data85240new)[1] = "Years"  #Changing colnames
colnames(data85240new)[2] = "Region"
colnames(data85240new)[4] = "Vehicles"

##### Data 85237 - Vehicles 3 ##################################################
# VOERTUIGEN/Vehicles 3
data85237 <- cbs_get_data("85237NED") #Please filter the import!
metadata85237 <- cbs_get_meta("85237NED")

## Data Prep##
# temp tables
tempPerioden85237 <- metadata85237$Perioden
tempBouwjaar85237 <- metadata85237$Bouwjaar
tempBrandstofsoort85237 <- metadata85237$Brandstofsoort

# Then, Replace Keys, by matching keys of temp table and imported table
data85237$Perioden <- tempPerioden85237$Title[match(data85237$Perioden, tempPerioden85237$Key)]
data85237$Bouwjaar <- tempBouwjaar85237$Title[match(data85237$Bouwjaar, tempBouwjaar85237$Key)]

data85237 <- data85237 %>%
  filter(Bouwjaar == "Totaal alle bouwjaren") %>% 
  select(Perioden, TotaalNederland_1, Groningen_2, Fryslan_3, Drenthe_4)

#Making different datasets to change regions from columns to rows
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

# Dataprep for combined lineplot
datacombined <- bind_rows(data_elek_2 , data85240new, data85239new, data85237new)

# Renaming Dutch to English

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

##### Data 85239 - Fuel Types ##################################################
## FUEL TYPES
data85239 <- cbs_get_data("85239NED")
# Please filter the import!
metadata85239 <- cbs_get_meta("85239NED")

tempVoertuigtype85239 <- metadata85239$Voertuigtype
tempBouwjaren85239 <- metadata85239$Bouwjaren
tempPerioden85239 <- metadata85239$Perioden

# Then, Replace Keys, by matching keys of temp table and imported table
data85239$Voertuigtype <- tempVoertuigtype85239$Title[match(data85239$Voertuigtype, tempVoertuigtype85239$Key)]
data85239$Bouwjaren <- tempBouwjaren85239$Title[match(data85239$Bouwjaren, tempBouwjaren85239$Key)]
data85239$Perioden <- tempPerioden85239$Title[match(data85239$Perioden, tempPerioden85239$Key)]

data85239 <- data85239 %>%  filter(Bouwjaren == "Totaal alle bouwjaren")

dataFueltypeCommercial <- data85239 %>%
  select(Voertuigtype, Perioden, Benzine_15, Diesel_16, LPG_17, Elektriciteit_18, CNG_19, GeenOverigeOnbekendeBrandstof_20, Totaal_21)

colnames(dataFueltypeCommercial)[8] = "OverigOnbekend_20"

##### Data 85237 - All Normal Vehicles #########################################
# All normal Vehicles
data85237 <- cbs_get_data("85237NED") 
metadata85237 <- cbs_get_meta("85237NED")

## Data Prep##
# temp tables
tempPerioden85237 <- metadata85237$Perioden
tempBouwjaar85237 <- metadata85237$Bouwjaar

# Then, Replace Keys, by matching keys of temp table and imported table
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



################ Proximity to Amenities ########################################
##### Data 80305 - Proximity to Amenities ######################################
# Proximity to Amenities
metadata80305 <- cbs_get_meta("80305ENG") 
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
  select = c("Periods", "Regions", "DistanceToGPPractice_1", "DistanceToGPPost_5","DistanceToPharmacy_6", "DistanceToHospital_11", "DistanceToLargeSupermarket_20", "DistanceToShopForOtherDailyFood_24", "DistanceToDepartmentStore_28", "DistanceToCafeEtc_32", "DistanceToRestaurant_40", "DistanceToDaycareCentres_48", "DistanceToOutOfSchoolCare_52", "DistanceToSchool_60", "DistanceToSchool_64", "DistanceToTrainStationsAllTypes_101", "DistanceToLibrary_103")
) # the recreational, (semi)public green and sports one were NA for all regions

data80305 <- data80305 %>%
  mutate(
    Avg15 = (
      DistanceToGPPractice_1 + DistanceToGPPost_5 +
        DistanceToPharmacy_6 + DistanceToHospital_11 +
        DistanceToLargeSupermarket_20 + DistanceToShopForOtherDailyFood_24 +
        DistanceToDepartmentStore_28 + DistanceToCafeEtc_32 +
        DistanceToRestaurant_40 + DistanceToDaycareCentres_48 +
        DistanceToOutOfSchoolCare_52 + DistanceToSchool_60 +
        DistanceToSchool_64 + DistanceToTrainStationsAllTypes_101 + DistanceToLibrary_103
    ) / 15
  ) %>%
  mutate(Avg15 = round(Avg15, 2))

data80305 <-
  data80305 %>% rename(
    "GP Practice" = "DistanceToGPPractice_1",
    "GP Post" = "DistanceToGPPost_5",
    "Pharmacy" = "DistanceToPharmacy_6",
    "Hospital" = "DistanceToHospital_11",
    "Large Supermarket" = "DistanceToLargeSupermarket_20",
    "Shop For Other Daily Food" = "DistanceToShopForOtherDailyFood_24",
    "Department Store" = "DistanceToDepartmentStore_28",
    "Cafe" = "DistanceToCafeEtc_32",
    "Restaurant" = "DistanceToRestaurant_40",
    "Daycare Centres" = "DistanceToDaycareCentres_48",
    "Out Of School Care" = "DistanceToOutOfSchoolCare_52",
    "School Type 1" = "DistanceToSchool_60",
    "School Type 2" = "DistanceToSchool_64",
    "Train Station" = "DistanceToTrainStationsAllTypes_101",
    "Library" = "DistanceToLibrary_103",
    "Average of 15 indicators" = "Avg15"
  )

## Data Prep##
# temp tables
tempPeriods <- metadata80305$Periods
tempRegion <- metadata80305$Regions

data80305$Periods <-
  tempPeriods$Title[match(data80305$Periods, tempPeriods$Key)]

# New Column "Municipality" made, "Region" is kept to match with shapefile
# fixing region names
data80305$Municipality <-
  tempRegion$Title[match(data80305$Regions, tempRegion$Key)]
data80305$Municipality <- recode(
  data80305$Municipality,
  "Groningen (PV)" = "Groningen",
  #extra space because city/province issue
  "Fryslân (PV)" = "Friesland",
  "Drenthe (PV)" = "Drenthe"
)




longformdata80305 <-
  pivot_longer(
    data80305,
    cols = c(
      "GP Practice",
      "GP Post",
      "Pharmacy",
      "Hospital",
      "Large Supermarket",
      "Shop For Other Daily Food",
      "Department Store",
      "Cafe",
      "Restaurant",
      "Daycare Centres",
      "Out Of School Care",
      "School Type 1",
      "School Type 2",
      "Train Station",
      "Library",
      "Average of 15 indicators"
    ), 
    values_to = "distances",
    values_drop_na = FALSE
  )


##### Shapefile Import #########################################################
# Shapefile Import through API call from PDOK
municipalBoundaries_unfiltered <- st_read(
  "https://service.pdok.nl/kadaster/bestuurlijkegebieden/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson%3B%20subtype%3Dgeojson&typeName=bestuurlijkegebieden:Gemeentegebied"
)
municipalBoundaries_unfiltered$naam[municipalBoundaries_unfiltered$naam  == "Groningen"] <- "Groningen Municipality"

# Reducing and filtering the imported data for efficiency 
municipalBoundaries <- municipalBoundaries_unfiltered %>%
  filter(ligtInProvincieCode %in% c("20", "21", "22")) %>%
  select(-id, -code) 


mapDataproximity <- municipalBoundaries %>%
  left_join(longformdata80305, by = c(identificatie = "Regions"))



################ Traffic/Infrastructure ########################################
##### Data 70806 - Length of Highways ##########################################
# Length of Highways
datatemporary <- cbs_get_data(
  "70806NED",
  SoortRijbanen = c(
    "T001491",
    "A047342",
    "A047344",
    "A047345",
    "A047346",
    "A047348",
    "A047349",
    "A047350",
    "A047352",
    "A047354",
    "A047355",
    "A047356",
    "A047357",
    "A047358",
    "A047360",
    "A047362",
    "A047363",
    "A047364",
    "A047365",
    "A047366"
  ),
  Perioden = has_substring("JJ")
)
metadata70806 <- cbs_get_meta("70806NED")
data70806 <- datatemporary

data70806 <- data70806 %>% 
  filter(RegioS %in% c(
                       "GM1680", "GM0059", "GM0060",
                       "GM0003", "GM0106", "GM0005", 
                       "GM0007","GM0063", "GM0055", 
                       "GM0009", "GM0064", "GM1681", 
                       "GM0109", "GM0065", "GM1891", 
                       "GM0010", "GM0058", "GM1979", 
                       "GM1651", "GM0114", "GM1722",
                       "GM0070", "GM1921", "GM1940", 
                       "GM0653", "GM0014", "GM0015", 
                       "GM0017", "GM0072", "GM0074", 
                       "GM1966", "GM0118", "GM0018",  
                       "GM0079", "GM0022", "GM0080", 
                       "GM0081", "GM0082", "GM0140", 
                       "GM0024", "GM1663", "GM0025", 
                       "GM0083", "GM1908", "GM1987", 
                       "GM0119", "GM1731", "GM1952", 
                       "GM0104", "GM1970", "GM1699",
                       "GM1895", "GM0085", "GM0086", 
                       "GM0765", "GM1661", "GM0039",
                       "GM0088", "GM0051", "GM0040", 
                       "GM0090", "GM0091", "GM0037",
                       "GM1900", "GM0093", "GM1730", 
                       "GM0737", "GM0047", "GM0048",
                       "GM0096", "GM1949", "GM1969", 
                       "GM1701", "GM1950", "GM0098",
                       "GM0052", "GM0053", "GM1690", 
                       "GM0710", "GM0683", "GM0056")) 
# This filtering will be done in import
# Years may be added as interactivity

# data70806_2 <- data70806 %>% filter(RegioS %in% c("PV20  ", "PV21  ", "PV22  "))
tempPerioden70806<- metadata70806$Perioden
tempSoortrijbanen70806 <- metadata70806$SoortRijbanen
tempRegioS70806 <- metadata70806$RegioS

data70806$Perioden <- tempPerioden70806$Title[match(data70806$Perioden, tempPerioden70806$Key)]
data70806$SoortRijbanen <- tempSoortrijbanen70806$Title[match(data70806$SoortRijbanen, tempSoortrijbanen70806$Key)]
colnames(data70806)[1] <- "identificatie"
# data70806_2$Perioden <- tempPerioden70806$Title[match(data70806_2$Perioden, tempPerioden70806$Key)]
# data70806_2$SoortRijbanen <- tempSoortrijbanen70806$Title[match(data70806_2$SoortRijbanen, tempSoortrijbanen70806$Key)]
# data70806_2$RegioS <- tempRegioS70806$Title[match(data70806_2$RegioS, tempRegioS70806$Key)]
# data70806_2$RegioS <- gsub(" (PV)", "", data70806_2$RegioS, fixed = TRUE)


mapDatarijbanen <- municipalBoundaries %>%
  left_join(data70806, municipalBoundaries, by = "identificatie") 



##### Data 83712 - Traffic Intensity ###########################################
# Traffic Intensity
metadata83712 <- cbs_get_meta("83712NED") 
data83712 <- cbs_get_data(
  "83712NED", 
  RegioS = c("PV20", "PV21", "PV22"), 
  Perioden = has_substring("JJ")
) 

## Data Prep##
# temp tables
tempRegioS83712 <- metadata83712$RegioS
tempPerioden83712 <- metadata83712$Perioden

# Matching and replacing Keys for Keys
data83712$RegioS <- tempRegioS83712$Title[match(data83712$RegioS, tempRegioS83712$Key)]
data83712$Perioden <- tempPerioden83712$Title[match(data83712$Perioden, tempPerioden83712$Key)]

# mutation and aliasing
colnames(data83712)[1] = "provinces"
colnames(data83712)[2] = "Years"

data83712$provinces <- gsub(" (PV)", "", data83712$provinces, fixed = TRUE)
data83712$provinces <- gsub("Fryslân", "Friesland", data83712$provinces, fixed = TRUE)