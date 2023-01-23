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
library(tidyr)


# set wd to the file location where you have downloaded the global, server and UI files
setwd("/Users/macbookair/Desktop/CBS/dashboard")

################ Mobility Indicators ###########################################
##### Data 84710 - Regions in years Tabs #######################################
# Regions in years, Both Purpose of Travel and Mode of Travel first tab
metadata84710 <- cbs_get_meta("84710ENG")
data84710 <- cbs_get_data(
  id = "84710ENG",
  Periods = has_substring("JJ"),
  RegionCharacteristics = c(
    "NL01    ", "LD01    ", "PV20    ",
    "PV21    ", "PV22    "
  ),
  Population = "A048710",
  # 6 and older
  Margins = "MW00000",
  # pure value, no confidence interval values
  select = c(
    "TravelMotives",
    "TravelModes",
    "RegionCharacteristics",
    "Periods",
    "Trips_1",
    # daily data
    "DistanceTravelled_2",
    # daily data
    "Trips_4",
    # yearly data
    "DistanceTravelled_5" # yearly data
  )
)

## Data Prep ##
# temp tables
tempPeriods84710 <- metadata84710$Periods
tempMotives84710 <- metadata84710$TravelMotives
tempModes84710 <- metadata84710$TravelModes

# Matching and replacing Keys for Keys
data84710$Periods <-
  tempPeriods84710$Title[match(data84710$Periods, tempPeriods84710$Key)]
data84710$TravelMotives <-
  tempMotives84710$Title[match(data84710$TravelMotives, tempMotives84710$Key)]
data84710$TravelModes <-
  tempModes84710$Title[match(data84710$TravelModes, tempModes84710$Key)]

# Fixing Region names manually
data84710$RegionCharacteristics <-
  recode(
    data84710$RegionCharacteristics,
    "NL01    " = "The Netherlands",
    "LD01    " = "Northern Netherlands",
    "PV20    " = "Groningen",
    "PV21    " = "Friesland",
    "PV22    " = "Drenthe"
  )

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
    "2820704", "2820705", "2820706", "A025261", "A025262", "A025263", "A025264",
    "A025265", "A025266", "A025267", "A025268"
  ),
  Population = "A048710",
  TravelPurposes = c(
    "2030170", "2030190", "2030200", "2030210", "2030220", "2030230", "2030240",
    "2030250", "2820740", "T001080"
  ),
  Margins = "MW00000",
  RegionCharacteristics = c(
    "PV20    ", "PV21    ", "PV22    ", "LD01    "
  )
)

## Data Prep##
# temp tables
temp_Region85055 <- metadata85055$RegionCharacteristics
temp_Periods85055 <- metadata85055$Periods
temp_TripCharacteristics85055 <- metadata85055$TripCharacteristics
temp_TravelPurposes85055 <- metadata85055$TravelPurposes


# Matching and replacing Keys for Keys
data85055$Periods <-
  temp_Periods85055$Title[match(data85055$Periods, temp_Periods85055$Key)]

data85055$TripCharacteristics <-
  temp_TripCharacteristics85055$Title[match(
    data85055$TripCharacteristics,
    temp_TripCharacteristics85055$Key
  )]

data85055$TravelPurposes <-
  temp_TravelPurposes85055$Title[match(data85055$TravelPurposes, temp_TravelPurposes85055$Key)]

# Fixing Region names manually
data85055$RegionCharacteristics <-
  recode(
    data85055$RegionCharacteristics,
    "LD01    " = "Northern Netherlands",
    "PV20    " = "Groningen",
    "PV21    " = "Friesland",
    "PV22    " = "Drenthe"
  )
# Mutations
data85055 <- data85055 %>%
  mutate(
    Timeframe = case_when(
      grepl("Distance:", data85055$TripCharacteristics) ~ "Distance",
      grepl("Time travelled:", data85055$TripCharacteristics) ~ "Time travelled",
      grepl("Trip in", data85055$TripCharacteristics) ~ "Month",
      grepl("Departure time:", data85055$TripCharacteristics) ~ "Departure time",
      grepl("day", data85055$TripCharacteristics) ~ "Day of the week"
    )
  ) %>%
  select(
    TripCharacteristics,
    Timeframe,
    TravelPurposes,
    RegionCharacteristics,
    Periods,
    AverageDistanceTravelledPerTrip_1,
    AverageTravelTimePerTrip_2
  )

data85055$TripCharacteristics <-
  sub("Departure time: ", "", data85055$TripCharacteristics)
data85055$TripCharacteristics <-
  sub("Trip in ", "", data85055$TripCharacteristics)

##### Data 85056 - Timeframe: Mode of Travel ###################################
# Mode of Travel > Regions in years: for a period, in a timeframe
metadata85056 <- cbs_get_meta("85056ENG")
data85056 <- cbs_get_data(
  id = "85056ENG",
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
    "T001093", "A048583", "A048584", "A018981", "A018982",
    "A018984", "A018985", "A018986"
  ),
  Margins = "MW00000",
  RegionCharacteristics = c(
    "PV20    ", "PV21    ", "PV22    ", "LD01    "
  )
)

## Data Prep##
# temp tables
temp_Region85056 <- metadata85056$RegionCharacteristics
temp_Periods85056 <- metadata85056$Periods
temp_TripCharacteristics85056 <- metadata85056$TripCharacteristics
temp_ModesOfTravel85056 <- metadata85056$ModesOfTravel


# Matching and replacing Keys for Keys
data85056$Periods <-
  temp_Periods85056$Title[match(data85056$Periods, temp_Periods85056$Key)]

data85056$TripCharacteristics <-
  temp_TripCharacteristics85056$Title[match(
    data85056$TripCharacteristics,
    temp_TripCharacteristics85056$Key
  )]

data85056$ModesOfTravel <-
  temp_ModesOfTravel85056$Title[match(data85056$ModesOfTravel, temp_ModesOfTravel85056$Key)]


# Fixing Region names manually
data85056$RegionCharacteristics <-
  recode(
    data85056$RegionCharacteristics,
    "LD01    " = "Northern Netherlands",
    "PV20    " = "Groningen",
    "PV21    " = "Friesland",
    "PV22    " = "Drenthe"
  )
# mutation for aliasing
data85056 <- data85056 %>%
  mutate(
    Timeframe = case_when(
      grepl("Distance:", data85056$TripCharacteristics) ~ "Distance",
      grepl("Time travelled:", data85056$TripCharacteristics) ~ "Time travelled",
      grepl("Trip in", data85056$TripCharacteristics) ~ "Month",
      grepl("Departure time:", data85056$TripCharacteristics) ~ "Departure time",
      grepl("day", data85056$TripCharacteristics) ~ "Day of the week"
    )
  ) %>%
  select(
    TripCharacteristics,
    Timeframe,
    ModesOfTravel,
    RegionCharacteristics,
    Periods,
    AverageDistanceTravelledPerTrip_1,
    AverageTravelTimePerTrip_2
  )

data85056$TripCharacteristics <-
  sub("Departure time: ", "", data85056$TripCharacteristics)

data85056$TripCharacteristics <-
  sub("Trip in ", "", data85056$TripCharacteristics)

##### Data 84709 - Personal Characteristics ####################################
# Personal Characteristics
metadata84709 <- cbs_get_meta("84709NED")
data84709 <- cbs_get_data(
  "84709NED",
  Persoonskenmerken = c(
    "T009002", "51511  ", "52020  ", "53105  ", "53500  ", "53705  ", "53850  ",
    "53925  ", "21600  ", "1012600", "2012655", "2012657", "1014752", "1014753",
    "1014754", "1014755", "1014756", "2030530", "2030540", "2030550", "2018700",
    "2018740", "2018790", "2017560", "2017565", "2012751", "2017572", "2820713",
    "2017576", "2017500", "A048766", "A048767", "A048768", "A048769", "A048770"
  ),
  Vervoerwijzen = c(
    "T001093",
    "A048583",
    "A048584",
    "A018981",
    "A018982",
    "A018984",
    "A018985",
    "A018986"
  ),
  RegioS = c("NL01    ", "LD01    ", "PV20    ", "PV21    ", "PV22    "),
  Geslacht = "T001038", # selecting total of genders
  Marges = c("MW00000"),
  Populatie = c("A048710"),
  select = c(
    "Persoonskenmerken", "RegioS", "Perioden", "Vervoerwijzen",
    "Verplaatsingen_1", "Afstand_2", "Reisduur_3"
  )
)

## Data Prep##
# temp tables

temp_Perioden84709 <- metadata84709$Perioden # Perioden
temp_Persoonskenmerken84709 <- metadata84709$Persoonskenmerken # Persoonskenmerken
temp_Vervoerwijzen84709 <- metadata84709$Vervoerwijzen

# Matching and replacing Keys for Keys
data84709$Perioden <-
  temp_Perioden84709$Title[match(data84709$Perioden, temp_Perioden84709$Key)]

data84709$Persoonskenmerken <-
  temp_Persoonskenmerken84709$Title[match(
    data84709$Persoonskenmerken,
    temp_Persoonskenmerken84709$Key
  )]
data84709$Vervoerwijzen <-
  temp_Vervoerwijzen84709$Title[match(data84709$Vervoerwijzen, temp_Vervoerwijzen84709$Key)]

# Fixing Region names manually
data84709$RegioS <- recode(
  data84709$RegioS,
  "NL01    " = "The Netherlands",
  "LD01    " = "Northern Netherlands",
  "PV20    " = "Groningen",
  "PV21    " = "Friesland",
  "PV22    " = "Drenthe"
)

# mutation and translating from Dutch
# finds all entries of column persoonskenmerken with the given words
# renames these entries
data84709 <- data84709 %>% mutate(
  Feature = case_when(
    grepl("Totaal personen", data84709$Persoonskenmerken) ~ "All",
    grepl("Leeftijd:", data84709$Persoonskenmerken) ~ "Age",
    grepl("Migratieachtergrond:", data84709$Persoonskenmerken) ~ "Background",
    grepl("Gestandaardiseerd inkomen:", data84709$Persoonskenmerken) ~ "Income",
    grepl("OV-Studentenkaart:", data84709$Persoonskenmerken) ~ "Travel Product",
    grepl("Onderwijsniveau:", data84709$Persoonskenmerken) ~ "Education",
    grepl("Participatie:", data84709$Persoonskenmerken) ~ "Employment Status",
    grepl("Rijbewijs,", data84709$Persoonskenmerken) ~ "Driver's License",
    grepl("Geen rijbewijs", data84709$Persoonskenmerken) ~ "Driver's License"
  )
)


# now it can be filtered on the column "Feature"
data84709$Persoonskenmerken <-
  sub("Leeftijd:", "", data84709$Persoonskenmerken)

data84709$Persoonskenmerken <-
  sub("Migratieachtergrond:", "", data84709$Persoonskenmerken)

data84709$Persoonskenmerken <-
  sub("Gestandaardiseerd inkomen:", "", data84709$Persoonskenmerken)

data84709$Persoonskenmerken <-
  sub("OV-Studentenkaart: ", "", data84709$Persoonskenmerken)

data84709$Persoonskenmerken <-
  sub("Onderwijsniveau: ", "", data84709$Persoonskenmerken)

data84709$Persoonskenmerken <-
  sub("Participatie: ", "", data84709$Persoonskenmerken)

data84709 <- data84709 %>%
  mutate(Personal_Characteristics = case_when(
    Persoonskenmerken == "Totaal personen" ~ "All people",
    Persoonskenmerken == " arbeidsongeschikt" ~ "Incapacitated",
    Persoonskenmerken == "anders/overig" ~ "Other",
    Persoonskenmerken == "gepensioneerd/VUT" ~ "Retired",
    Persoonskenmerken == "Participatie:student/scholier" ~ "Student/Scholar",
    Persoonskenmerken == "werkloos" ~ "Unemployed",
    Persoonskenmerken == "werkzaam 30 uur pw of meer" ~ "Employed, working 30+ hours weekly",
    Persoonskenmerken == "werkzaam: 12 tot 30 uur pw" ~ "Employed, working 12 to 30 hours weekly",
    Persoonskenmerken == "6 tot 12 jaar" ~ "6 to 12 years",
    Persoonskenmerken == " 12 tot 18 jaar" ~ "12 to 18 years",
    Persoonskenmerken == " 18 tot 25 jaar" ~ "18 to 25 years",
    Persoonskenmerken == " 25 tot 35 jaar" ~ "25 to 35 years",
    Persoonskenmerken == " 35 tot 50 jaar" ~ "35 to 50 years",
    Persoonskenmerken == " 50 tot 65 jaar" ~ "50 to 65 years",
    Persoonskenmerken == " 65 tot 75 jaar" ~ "75 to 75 years",
    Persoonskenmerken == " 75 jaar of ouder" ~ "75 years +",
    Persoonskenmerken == " Nederland" ~ "Dutch",
    Persoonskenmerken == " niet-westers" ~ "Non-western",
    Persoonskenmerken == " westers" ~ "Western",
    Persoonskenmerken == " 1e 20%-groep" ~ "0-20%-group",
    Persoonskenmerken == " 2e 20%-groep" ~ "20-40%-group",
    Persoonskenmerken == " 3e 20%-groep" ~ "40-60%-group",
    Persoonskenmerken == " 4e 20%-groep" ~ "60-80%-group",
    Persoonskenmerken == " 5e 20%-groep" ~ "80-100%-group",
    Persoonskenmerken == "geen" ~ "No subscription",
    Persoonskenmerken == "weekabonnement" ~ "Weekly-subscription",
    Persoonskenmerken == "weekendabonnement" ~ "Weekend-subscription",
    Persoonskenmerken == "1 Laag" ~ "Low",
    Persoonskenmerken == "2 Middelbaar" ~ "Middle",
    Persoonskenmerken == "3 Hoog" ~ "High",
    Persoonskenmerken == "Geen rijbewijs; jonger dan 17 jaar" ~ "No drivers license: Younger than 17 years",
    Persoonskenmerken == "Geen rijbewijs; wel 17 jaar of ouder" ~ "No drivers license: 17 years or older",
    Persoonskenmerken == "Rijbewijs, geen personenauto in hh" ~ "Drivers license: No personal car in owns",
    Persoonskenmerken == "Rijbewijs, personenauto in hh" ~ "Drivers license: Personal car in owns",
    Persoonskenmerken == "Rijbewijs, personenauto op eigen naam" ~ "Personal car in own name"
  ))

data84709 <- data84709 %>%
  mutate(Transport = case_when(
    Vervoerwijzen == "Totaal" ~ "Total",
    Vervoerwijzen == "Personenauto  (passagier)" ~ "Personal car(Passenger)",
    Vervoerwijzen == "Personenauto (bestuurder)" ~ "Personal car(Driver)",
    Vervoerwijzen == "Trein" ~ "Train",
    Vervoerwijzen == "Overige vervoerwijze" ~ "Other",
    Vervoerwijzen == "Bus/tram/metro" ~ "Bus/tram/metro",
    Vervoerwijzen == "Fiets" ~ "Bicycle",
    Vervoerwijzen == "Lopen" ~ "Walking"
  ))

data84709 <-
  data84709 %>% select(
    Personal_Characteristics,
    Feature,
    Verplaatsingen_1,
    Perioden,
    RegioS,
    Transport,
    Reisduur_3,
    Afstand_2
  )

##### Data 83488 - Drivers License #############################################
## Drivers License
data83488 <- cbs_get_data("83488ENG",
  Region = c("PV20  ", "PV21  ", "PV22  ")
)
metadata83488 <- cbs_get_meta("83488ENG")

## Data Prep##
# temp tables
tempCategoryDrivingLicense83488 <- metadata83488$CategoryDrivingLicence
tempAgeDrivingLicenseHolder83488 <- metadata83488$AgeDrivingLicenseHolder
tempPeriods83488 <- metadata83488$Periods

# Matching and replacing Keys for Keys
data83488$CategoryDrivingLicence <-
  tempCategoryDrivingLicense83488$Title[match(
    data83488$CategoryDrivingLicence,
    tempCategoryDrivingLicense83488$Key
  )]

data83488$AgeDrivingLicenseHolder <-
  tempAgeDrivingLicenseHolder83488$Title[match(
    data83488$AgeDrivingLicenseHolder,
    tempAgeDrivingLicenseHolder83488$Key
  )]

data83488$Periods <-
  tempPeriods83488$Title[match(data83488$Periods, tempPeriods83488$Key)]
# Fixing Region names manually
data83488$Region <- recode(
  data83488$Region,
  "PV20  " = "Groningen",
  "PV21  " = "Friesland",
  "PV22  " = "Drenthe"
)




################ Green Mobility ################################################
##### Data from Excel ############################################################
# data from excel
elektrische_personenauto_provincie_2_ <-
  read_excel("elektrische_personenauto_provincie (2).xlsx",
    sheet = "Tabel 1"
  )

data_elek_2 <- elektrische_personenauto_provincie_2_ %>%
  filter(Regio == "Friesland" |
    Regio == "Groningen" | Regio == "Drenthe")

# renaming columns to English
data_elek_2 <- data_elek_2 %>% rename(
  "Years" = "Jaar",
  "Count" = "Aantal",
  "Region" = "Regio"
)


# Add extra column to data frame to specify this is for electric vehicles
data_elek_2 <- data_elek_2 %>%
  mutate(Vehicles = "Electric Vehicle")

# make all variables factors instead of characters or doubles
# must be done so it has variables of same type as CBS data tables
data_elek_2 <- mutate(data_elek_2, across(everything(), as.factor))

# make the count numeric again
# going straight from factor to numeric changes the values
# hence we make it character first
data_elek_2$Count <- as.numeric(as.character(data_elek_2$Count))

##### Data 85239 - Vehicles 1 ##################################################
## VOERTUIGEN/ Vehicles 1
metadata85239 <- cbs_get_meta("85239NED")
data85239 <- cbs_get_data(
  "85239NED",
  Perioden = has_substring("JJ"),
  Bouwjaren = "T001378",
  select = c(
    "Voertuigtype",
    "Perioden",
    "Groningen_2",
    "Fryslan_3",
    "Drenthe_4"
  )
)

## Data Prep##
# temp tables
tempVoertuigtype85239 <- metadata85239$Voertuigtype
tempPerioden85239 <- metadata85239$Perioden

# Matching and replacing Keys for Keys
data85239$Voertuigtype <- tempVoertuigtype85239$Title[match(data85239$Voertuigtype, tempVoertuigtype85239$Key)]
data85239$Perioden <- tempPerioden85239$Title[match(data85239$Perioden, tempPerioden85239$Key)]
# data85239 <- data85239 %>% filter(Bouwjaren == "Totaal alle bouwjaren")

# Mutations
data85239 <- data85239 %>% rename(
  "Years" = "Perioden",
  "Groningen" = "Groningen_2",
  "Friesland" = "Fryslan_3",
  "Drenthe" = "Drenthe_4",
  "Vehicles" = "Voertuigtype"
)
# Change regions from columns to rows by changing from wide to long dataset
data85239new <- pivot_longer(data85239,
  cols = c(
    Groningen,
    Friesland,
    Drenthe
  ),
  names_to = "Region",
  values_to = "Count",
  values_drop_na = FALSE
)

data85239new <- data85239new %>%
  group_by(Years, Vehicles) %>%
  select(Years, Vehicles, Count, Region)


# why? @Thijs
datafiltervoertuig <- data85239new %>%
  filter(Vehicles == "Totaal bedrijfsvoertuigen")



##### Data 85240 - Vehicles 2 ###################################################
# VOERTUIGEN/Vehicles 2
metadata85240 <- cbs_get_meta("85240NED")
data85240 <- cbs_get_data(
  "85240NED",
  Perioden = has_substring("JJ"),
  Provincie = c("PV20  ", "PV21  ", "PV22  "),
  TenaamstellingEnLeeftijdParticulier = "T001191",
  Bouwjaar = "T001378",
  select = c(
    "Provincie",
    "Perioden",
    "VoertuigenMetBromfietskenteken_1",
    "Snorfiets_2",
    "Bromfiets_3",
    "Brommobiel_4",
    "OverigeVoertuigenMetBromfietskenteken_5"
  )
)


# Temp table
tempPerioden85240 <- metadata85240$Perioden
# key matching
data85240$Perioden <-
  tempPerioden85240$Title[match(data85240$Perioden, tempPerioden85240$Key)]

# fixing Region names
data85240$Provincie <- recode(
  data85240$Provincie,
  "PV20  " = "Groningen",
  "PV21  " = "Friesland",
  "PV22  " = "Drenthe"
)


data85240 <- data85240 %>% rename(
  "Years" = "Perioden",
  "Region" = "Provincie"
)

# Change vehicle types from columns to rows by changing wide data to long
data85240new <- pivot_longer(data85240,
  cols = c(
    VoertuigenMetBromfietskenteken_1,
    Snorfiets_2,
    Bromfiets_3,
    Brommobiel_4,
    OverigeVoertuigenMetBromfietskenteken_5
  ),
  names_to = "Vehicles",
  values_to = "Count",
  values_drop_na = FALSE
)
data85240new <- data85240new %>% mutate(Vehicles = case_when(
  Vehicles == "VoertuigenMetBromfietskenteken_1" ~ "Voertuigen Met Bromfietskenteken",
  Vehicles == "Snorfiets_2" ~ "Snorfiets",
  Vehicles == "Bromfiets_3" ~ "Bromfiets",
  Vehicles == "Brommobiel_4" ~ "Brommobiel",
  Vehicles == "OverigeVoertuigenMetBromfietskenteken_5" ~ "Overige voertuigen met bromfietskenteken"
))

data85240new <-
  data85240new %>%
  group_by(Years, Vehicles)



##### Data 85237 - Vehicles 3 ##################################################
# VOERTUIGEN/Vehicles 3
data85237 <- cbs_get_data(
  "85237NED",
  Perioden = has_substring("JJ"),
  Bouwjaar = "T001378",
  select = c(
    "Perioden",
    "Groningen_2",
    "Fryslan_3",
    "Drenthe_4"
  )
)
metadata85237 <- cbs_get_meta("85237NED")

## Data Prep##
# temp tables
tempPerioden85237 <- metadata85237$Perioden
tempBrandstofsoort85237 <- metadata85237$Brandstofsoort

# Then, Replace Keys, by matching keys of temp table and imported table
data85237$Perioden <-
  tempPerioden85237$Title[match(data85237$Perioden, tempPerioden85237$Key)]


data85237 <- data85237 %>% rename(
  "Years" = "Perioden",
  "Groningen" = "Groningen_2",
  "Friesland" = "Fryslan_3",
  "Drenthe" = "Drenthe_4"
)
data85237new <- pivot_longer(data85237,
  cols = c(
    Groningen,
    Friesland,
    Drenthe
  ),
  names_to = "Region",
  values_to = "Count",
  values_drop_na = FALSE
)

# Adds column showing vehicle type as "Normal car"
data85237new <- data85237new %>%
  mutate(Vehicles = "Normal car")



# Dataprep for combined lineplot
datacombined <-
  bind_rows(data_elek_2, data85240new, data85239new, data85237new)

# Renaming Dutch to English
datacombined <- datacombined %>%
  mutate(Vehicles = case_when(
    Vehicles == "Voertuigen Met Bromfietskenteken" ~ "All mopeds",
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
    Vehicles == "Normal car" ~ "Normal car",
    Vehicles == "Brommobiel" ~ "Moped (45km/h and <350kg)"
  )) %>%
  group_by(Region)

##### Data 85239 - Fuel Types ##################################################
## FUEL TYPES
data85239 <- cbs_get_data(
  "85239NED",
  Bouwjaren = "T001378",
  select = c(
    "Voertuigtype",
    "Perioden",
    "Benzine_15",
    "Diesel_16",
    "LPG_17",
    "Elektriciteit_18",
    "CNG_19",
    "GeenOverigeOnbekendeBrandstof_20",
    "Totaal_21"
  )
)


tempVoertuigtype85239 <- metadata85239$Voertuigtype
tempPerioden85239 <- metadata85239$Perioden

# Then, Replace Keys, by matching keys of temp table and imported table
data85239$Voertuigtype <-
  tempVoertuigtype85239$Title[match(data85239$Voertuigtype, tempVoertuigtype85239$Key)]

data85239$Perioden <-
  tempPerioden85239$Title[match(data85239$Perioden, tempPerioden85239$Key)]


dataFueltypeCommercial <- data85239

dataFueltypeCommercial <- dataFueltypeCommercial %>%
  rename("OverigOnbekend_20" = "GeenOverigeOnbekendeBrandstof_20")


##### Data 85237 - All Normal Vehicles #########################################
# All normal Vehicles
data85237 <-
  cbs_get_data(
    "85237NED",
    Perioden = has_substring("JJ"),
    Bouwjaar = c("T001378"),
    select = c(
      "Perioden",
      "Benzine_15",
      "Diesel_16",
      "LPG_17",
      "Elektriciteit_18",
      "CNG_19",
      "OverigOnbekend_20",
      "Totaal_21"
    )
  )
metadata85237 <- cbs_get_meta("85237NED")

## Data Prep##
# temp tables
tempPerioden85237 <- metadata85237$Perioden

# Then, Replace Keys, by matching keys of temp table and imported table
data85237$Perioden <-
  tempPerioden85237$Title[match(data85237$Perioden, tempPerioden85237$Key)]

# adding column showing the vehicle type is "Personal vehicles"
dataFueltypeNormal <- data85237 %>%
  mutate(Voertuigtype = "Personal vehicle")

# combining normal fuel types and commercial vehicles
dataFueltypes <- bind_rows(dataFueltypeNormal, dataFueltypeCommercial)

# renaming the columns
dataFueltypes <- dataFueltypes %>% rename(
  "Years" = "Perioden",
  "Vehicletype" = "Voertuigtype",
  "Benzine" = "Benzine_15",
  "Diesel" = "Diesel_16",
  "LPG" = "LPG_17",
  "Electricity" = "Elektriciteit_18",
  "CNG" = "CNG_19",
  "Unknown" = "OverigOnbekend_20",
  "Total" = "Totaal_21"
)

# converting from a wide dataset to a long dataset
datafueltypes1 <- pivot_longer(
  dataFueltypes,
  cols = c(
    Benzine,
    Diesel,
    LPG,
    Electricity,
    CNG,
    Unknown,
    Total
  ),
  names_to = "Fueltype",
  values_to = "Count",
  values_drop_na = FALSE
)


# renaming Dutch variables to English
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
    Vehicletype == "Personal vehicle" ~ "Personal vehicle"
  ))





################ Proximity to Amenities ########################################
##### Data 80305 - Proximity to Amenities ######################################
# Proximity to Amenities

metadata80305 <- cbs_get_meta("80305ENG")
data80305 <- cbs_get_data(
  id = "80305ENG",
  Periods = "2020JJ00", # Only choosing 2020
  Regions = c(
    "PV20  ", "PV21  ", "PV22  ", "GM1680", "GM0059", "GM0060",
    "GM0003", "GM0106", "GM0005", "GM0007", "GM0063", "GM0055",
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
    "GM0052", "GM0053", "GM1690", "GM0710", "GM0683", "GM0056"
  ),
  select = c(
    "Periods",
    "Regions",
    "DistanceToGPPractice_1",
    "DistanceToGPPost_5",
    "DistanceToPharmacy_6",
    "DistanceToHospital_11",
    "DistanceToLargeSupermarket_20",
    "DistanceToShopForOtherDailyFood_24",
    "DistanceToDepartmentStore_28",
    "DistanceToCafeEtc_32",
    "DistanceToRestaurant_40",
    "DistanceToDaycareCentres_48",
    "DistanceToOutOfSchoolCare_52",
    "DistanceToSchool_56",
    "DistanceToSchool_60",
    "DistanceToTrainStationsAllTypes_101",
    "DistanceToLibrary_103"
  )
)

# creating the variable 'Avg15', the average of all 15 indicators by region
data80305 <- data80305 %>%
  mutate(
    Avg15 = (
      DistanceToGPPractice_1 + DistanceToGPPost_5 +
        DistanceToPharmacy_6 + DistanceToHospital_11 +
        DistanceToLargeSupermarket_20 + DistanceToShopForOtherDailyFood_24 +
        DistanceToDepartmentStore_28 + DistanceToCafeEtc_32 +
        DistanceToRestaurant_40 + DistanceToDaycareCentres_48 +
        DistanceToOutOfSchoolCare_52 + DistanceToSchool_56 +
        DistanceToSchool_60 + DistanceToTrainStationsAllTypes_101 + DistanceToLibrary_103
    ) / 15
  ) %>%
  mutate(Avg15 = round(Avg15, 2))

# renaming the columns
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
    "Secondary schools" = "DistanceToSchool_56",
    "Primary schools" = "DistanceToSchool_60",
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
  "Groningen (PV)" = "Groningen ",
  # extra space because city/province issue
  "Fryslân (PV)" = "Friesland",
  "Drenthe (PV)" = "Drenthe"
)


# converting the data from wide fromat to long format
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
      "Secondary schools",
      "Primary schools",
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
municipalBoundaries_unfiltered$naam[municipalBoundaries_unfiltered$naam == "Groningen"] <- "Groningen Municipality"

# Reducing and filtering the imported data for efficiency
municipalBoundaries <- municipalBoundaries_unfiltered %>%
  filter(ligtInProvincieCode %in% c("20", "21", "22")) %>%
  select(-id, -code)


mapDataproximity <- municipalBoundaries %>%
  left_join(longformdata80305, by = c(identificatie = "Regions"))
mapDataproximity$ligtInProvincieNaam <- recode(mapDataproximity$ligtInProvincieNaam,
  "Fryslân" = "Friesland"
)



################ Traffic/Infrastructure ########################################
##### Data 70806 - Length of Highways ##########################################
# Length of Highways
metadata70806 <- cbs_get_meta("70806NED")
data70806 <- cbs_get_data(
  "70806NED",
  SoortRijbanen = c("T001491", "A047344", "A047352", "A047360"),
  RegioS = c(
    "GM1680", "GM0059", "GM0060",
    "GM0003", "GM0106", "GM0005",
    "GM0007", "GM0063", "GM0055",
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
    "GM0710", "GM0683", "GM0056"
  ),
  Perioden = has_substring("JJ")
)

tempPerioden70806 <- metadata70806$Perioden
tempSoortrijbanen70806 <- metadata70806$SoortRijbanen
tempRegioS70806 <- metadata70806$RegioS

data70806$Perioden <-
  tempPerioden70806$Title[match(data70806$Perioden, tempPerioden70806$Key)]
data70806$SoortRijbanen <-
  tempSoortrijbanen70806$Title[match(data70806$SoortRijbanen, tempSoortrijbanen70806$Key)]
# renaming column to match shapefile naming for binding together later
data70806 <- data70806 %>% rename(
  "identificatie" = "RegioS"
)

# Translating to English
data70806 <- data70806 %>%
  mutate(SoortRijbanen = case_when(
    SoortRijbanen == "Totale weglengte" ~ "Total of all roads",
    SoortRijbanen == "Gemeentelijke wegen: totaal" ~ "Total Municipal Roads",
    SoortRijbanen == "Provinciale wegen: totaal" ~ "Total Provincial Roads",
    SoortRijbanen == "Rijkswegen: totaal" ~ "Total National Roads"
  ))
# joining the shapefile data with the CBS dataset by the column identificatie
mapDatarijbanen <- municipalBoundaries %>%
  left_join(data70806, municipalBoundaries, by = "identificatie")



##### Data 83712 - Traffic Intensity ###########################################
# Traffic Intensity
metadata83712 <- cbs_get_meta("83712NED")
data83712 <- cbs_get_data("83712NED",
  RegioS = c("PV20", "PV21", "PV22"),
  Perioden = has_substring("JJ")
)

## Data Prep##
# temp tables
tempRegioS83712 <- metadata83712$RegioS
tempPerioden83712 <- metadata83712$Perioden

# Matching and replacing Keys for Keys
data83712$RegioS <-
  tempRegioS83712$Title[match(data83712$RegioS, tempRegioS83712$Key)]

data83712$Perioden <-
  tempPerioden83712$Title[match(data83712$Perioden, tempPerioden83712$Key)]

# renaming columns
data83712 <- data83712 %>% rename(
  "provinces" = "RegioS",
  "Years" = "Perioden"
)

# removing (PV) from province names and changing Fryslan to Friesland
data83712$provinces <- gsub(" (PV)", "", data83712$provinces, fixed = TRUE)
data83712$provinces <- gsub("Fryslân", "Friesland", data83712$provinces, fixed = TRUE)

###tables####
##Packages##
#adding data
packages_used <- c("dplyr", "shiny", "shinydashboard", "SF", "cbsodataR", 
                   "ggplot2", "plotly", "readxl", "forcats", "tidyr")
used_for <- c("Mutating, filtering, and renaming datasets", 
              "Creating interactive dashboard", "Creating interactive dashboard", 
              "Importing shapefile", "Importing datasets and metadata from CBS website", 
              "Plotting all data", "Converting ggplots to plotly for extra interactivity", 
              "Importing excel document into R", "Reordering of axes", 
              "Converting wide datasets into long datasets")
#making table
packages_table <- data.frame(packages_used, used_for)
#renaming columns
names(packages_table) <- c("Packages Used", "Used For")

##Data##
#adding data
data_used <- c("85055ENG", "85056ENG", "84709NED", "83488ENG", "85239NED",
                   "85240NED", "85237NED", "80305ENG", "70806NED", "83712NED"
               )
data_name <- c("Mobility; per trip, purposes of travel, trip characteristics and regions",
               "Mobility; per trip, trip characteristics, modes of travel and regions",
               "Mobiliteit; per persoon, persoonskenmerken, vervoerwijzen en regio's",
               "People with a driving license; driving license type, age, region, 1 January",
               "Bedrijfsvoertuigen actief; voertuigkenmerken, regio's, 1 januari",
               "Mopeds active; vehicle type, year of manufacture, age, region, 1 January",
               "Personenauto's actief; voertuigkenmerken, regio's, 1 januari",
               "Proximity to facilities; distances by car, regional",
               "Lengte van wegen; wegkenmerken, regio",
               "Traffic intensity; national roads, regions"
               )
data_summary <- c("The dataset contains the distance, purpose, region, and period of trips done in the Netherlands.",
                  "The dataset contains the distance, the mode of transport, region, and period of trips done in the Netherlands.",
                  "This dataset contains the average distance traveled, the year, the reason of travel, and the mode of transport that was taken for the trip(s). This dataset is in Dutch.",
                  "This dataset contains the number of people that have different types of driving licenses (eg. bus, car, etc)",
                  "Types of commercial vehicles active per region. Includes delivery vehicles, trucks, tractors, special vehicles, buses, trailers, and semi trailer.",
                  "This dataset contains the total number mopeds active according to age and region.",
                  "This dataset contains the total passenger cars active in different regions.",
                  "This dataset contains the average distance to a large variety of different facilities such as General Practitioners, grocery stores, etc. by car.",
                  "This dataset contains the length of road networks in the Netherlands, broken down by different kinds of roads and regions.",
                  "This dataset contains the intensity of traffic outside built up areas. In other words, there are different measuring points determining the number of motor vehicles passing per hour."
                  )
#making table
data_table <- data.frame(data_used, data_name, data_summary)
names(data_table) <-c("Dataset #", "Name of Dataset", "Short Summary")
