library(ggplot2)
library(cbsodataR)
cbs_get_data('84709NED')
library(magrittr) # needs tofill be run every time you start R and want to use %>%
library(dplyr)

#Split on age

data7094 <- data709 %>%
  filter (Perioden == "2021JJ00") %>% 
  filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")%>% 
  filter(Persoonskenmerken %in% c("51511  ","52020  ","53105  ","53500  ","53705  ","53850  ","53925  ","21600  "))%>% 
  mutate(Persoonskenmerken = case_when(
    Persoonskenmerken =="51511  "~ "Age 6 to 12 years",
    Persoonskenmerken =="52020  "~"Age 12 to 18 years",
    Persoonskenmerken =="53105  "~"Age 18 to 25 years",
    Persoonskenmerken =="53500  "~"Age 25 to 35 years",
    Persoonskenmerken =="53705  "~"Age 35 to 50 years",
    Persoonskenmerken =="53850  "~"Age 50 to 65 years" ,
    Persoonskenmerken =="53925  "~"Age 65 to 75 years",
    Persoonskenmerken =="21600  "~"Age 75 yearsor older"))
ggplot(data7094, aes(x = Persoonskenmerken ,y = Reisduur_3, fill = Persoonskenmerken)) +
  geom_col() +
  labs(
    title = "Average Daily Mobility in the Northern Provinces",
    x = "Personal Characteristics",
    y = "Avg Trips per Person Per Year",
  )

#Split on migration

  
data7095<-data709 %>% filter (Perioden == "2021JJ00") 
data7096<-data7095 %>% filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")
data7097<- data7096 %>% filter(Persoonskenmerken %in% c("1012600", "2012655","2012657"))
data7098<- data7097 %>% mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="1012600","Netherlands"))%>%
mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2012655","Western"))%>% 
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2012657","Not Western"))
  
  
ggplot(data7098, aes(x = Persoonskenmerken ,y = Reisduur_3, fill = Persoonskenmerken)) +
  geom_col() +
  labs(
    title = "Average Daily Mobility in the Northern Provinces",
    x = "Personal Characteristics",
    y = "Avg Trips per Person Per Year")

#Split on income


data70910<-data709 %>% filter (Perioden == "2021JJ00") 
data70911<-data70910 %>% filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")
data70912<- data70911 %>% filter(Persoonskenmerken %in% c("1014752", "1014753","1014754","1014755","1014756"))
data70913<- data70912 %>% mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="1014752","0-20%"))%>%
mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="1014753","20-40%"))%>% 
mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="1014754","40-60%"))%>%
mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="1014755","60-80%"))%>%
mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="1014756","80-100%"))



ggplot(data70913, aes(x = Persoonskenmerken ,y = Reisduur_3, fill = Persoonskenmerken)) +
  geom_col() +
  labs(
    title = "Average Daily Mobility in the Northern Provinces",
    x = "Personal Characteristics",
    y = "Avg Trips per Person Per Year")

#Split on working status


data70915<-data709 %>% filter (Perioden == "2021JJ00") 
data70916<-data70915 %>% filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")
data70917<- data70916 %>% filter(Persoonskenmerken %in% c("2012751", "2017500","2017560","2017565","2017572","2017576"))
data70918<- data70917 %>% mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2012751","Student"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2017500","Other"))%>% 
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2017560","12-30 hours a week working"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2017565","30 hours a week or more working"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2017572","Unemployed"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2017576","Retired"))
ggplot(data70918, aes(x = Persoonskenmerken ,y = Reisduur_3, fill = Persoonskenmerken)) +
  geom_col() +
  labs(
    title = "Average Daily Mobility in the Northern Provinces",
    x = "Personal Characteristics",
    y = "Avg Trips per Person Per Year")

#Split on educational level

data70920<-data709 %>% filter (Perioden == "2021JJ00") 
data70921<-data70920 %>% filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")
data70922<- data70921 %>% filter(Persoonskenmerken %in% c("2018700", "2018740","2018790"))
data70923<- data70922 %>% mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2012751","Student"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2018700","Lower vocational education"))%>% 
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2018740","Middle vocational education"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2018790","Higher vocational education"))

ggplot(data70923, aes(x = Persoonskenmerken ,y = Reisduur_3, fill = Persoonskenmerken)) +
  geom_col() +
  labs(
    title = "Average Daily Mobility in the Northern Provinces",
    x = "Personal Characteristics",
    y = "Avg Trips per Person Per Year")

#Split on OV-cart status


data70930<-data709 %>% filter (Perioden == "2021JJ00") 
data70931<-data70930 %>% filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")
data70932<- data70931 %>% filter(Persoonskenmerken %in% c("2030530","2030540","2030550"))
data70933<- data70932 %>% mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2012751","Student"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2030530","OV-Studencart: weekly season ticket"))%>% 
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2030540","OV-Studencart: weekend season ticket"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2030550","No OV-Studencart"))
  
  ggplot(data70933, aes(x = Persoonskenmerken ,y = Reisduur_3, fill = Persoonskenmerken)) +
  geom_col() +
  labs(
    title = "Average Daily Mobility in the Northern Provinces",
    x = "Personal Characteristics",
    y = "Avg Trips per Person Per Year")

#Split on drivers license status
  
  
data70925<-data709 %>% filter (Perioden == "2021JJ00") 
data70926<-data70925 %>% filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")
data70927<- data70926 %>% filter(Persoonskenmerken %in% c("A048766", "A048767","A048768","A048769","A048770"))
data70928<- data70927 %>% mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="A048766","Drivers license B, min. one car on his own name"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="A048767","Drivers license B, one car in household"))%>% 
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="A048768","Drivers license B, no car in household or on name"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="A048769","No drivers license B, but old enough"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="A048770","No drivers license B, but not old enough"))
  
  ggplot(data70928, aes(x = Persoonskenmerken ,y = Reisduur_3, fill = Persoonskenmerken)) +
  geom_col() +
  labs(
    title = "Average Daily Mobility in the Northern Provinces",
    x = "Personal Characteristics",
    y = "Avg Trips per Person Per Year")





