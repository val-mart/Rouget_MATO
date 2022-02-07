#library
library(readxl)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("hablar")
library(hablar)
#install.packages("gridExtra")
library("gridExtra")
#install.packages("cowplot")
library("cowplot")

#Regroupement données brutes 

#-------------------------------------2006_2019---------------------------------
#data 2006-2019
ICES2006_2019<-read_excel("./Data/OfficialNominalCatches 2006 2019/ICESCatchDataset2006-2019.xlsx", 
                          sheet = 2)

#filter + correction 
ICES2006_2019_MUR <- ICES2006_2019 %>% 
  filter(Species %in% c("MUR"))%>% #select the species 
  convert(num(5:18))%>% 
  tidyr::pivot_longer(cols=starts_with("2"),names_to="year",values_to="TLW") 

ICES2006_2019_MUR<-ICES2006_2019_MUR[,-c(3)] #delete column "Units" 

colnames(ICES2006_2019_MUR) <- c("Species","FAO_Area","Country","Year","TLW") #rename columns 



#-------------------------------------1950-2010---------------------------------
#data 1950-2010
ICES1950_2010 <- read_excel("./Data/HistoricalLandings1950-2010/ICES_1950-2010.xls")

#filter + correction
ICES1950_2005 <-ICES1950_2010[,-c(60:64) ] #delete column Units "2006" to "2010" because duplicate 

ICES1950_2005_MUR<- ICES1950_2005 %>% 
  filter(Species %in% c("Striped mullet","Surmullets(=Red mullets) nei","Red mullet")) %>% #select the species 
  convert(num(4:59)) %>%
  tidyr::pivot_longer(cols=(4:59),names_to="year",values_to="TLW")%>%
  relocate(Species, .before = Country) %>% #reorganize the columns
  relocate(Division, .after = Species)  #reorganize the columns

colnames(ICES1950_2005_MUR) <- c("Species","FAO_Area","Country","Year","TLW") #rename columns 


####-----------------------------ICES 1903-1949--------------------------------------######

ICES1903_1949<-read_excel("./Data/ICES1903-49/1903-1949_Landings.xlsx")


ICES1903_1949_MUR <- ICES1903_1949 %>% 
  filter(FAO_Species_Name %in% c("Surmullet")) %>% #select the species 
  convert(num(7))%>%
  relocate(FAO_Species_Name, .before = Country) %>%  #reorganize the columns
  relocate(FAO_Area, .after = FAO_Species_Name) %>%  #reorganize the columns
  relocate(Landings, .after = Year)  #reorganize the columns

ICES1903_1949_MUR

ICES1903_1949_MUR <-ICES1903_1949_MUR[,-c(6:7) ] #delete columns not useful "FAO_Species_Code" and "Species scientific name"

colnames(ICES1903_1949_MUR) <- c("Species","FAO_Area","Country","Year","TLW") #rename columns 



####--------------------------ICES 1903-2019--------------------------------------------#####


ICES1903_2019_MUR <- rbind(ICES1903_1949_MUR, ICES1950_2005_MUR, ICES2006_2019_MUR) #combine the data.frame for the tree periods 
View(ICES1903_2019_MUR)

####--------------------------ICES 1905-2019 for all the countries----------------------#####
#sum landings all the countries 
ICES1903_2019_MUR_TLWbyYear<-ICES1903_2019_MUR%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>%
  ungroup() 
ICES1903_2019_MUR_TLWbyYear

#convert in data.frame 
Year1 <- c(1905:1924, 1927:1928, 1950:2019) #no information between 1903:1905, 1925,1926 and 1929:1949
TLWallcountries <- ICES1903_2019_MUR_TLWbyYear$TLW
df1<-data.frame(Year1, TLWallcountries) # neccesary if I want the plot with geom_line()
df1

#plot
MUR_TLWbyYear <- ggplot(df1, aes(x=Year1, y=TLWallcountries)) +
  geom_point() +
  geom_line()   +
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus in all the countries by year from 1905 to 2019") +
  geom_smooth() 

MUR_TLWbyYear

####--------------------------ICES 1905-2019 separating the countries ----------------------#####


MUR_TLWbyYearbyCountries <- ggplot(ICES1903_2019_MUR , aes(x=Year, y=TLW)) +
  geom_point(aes(color = factor(Country))) +
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus by countries by year from 1905 to 2019") 

MUR_TLWbyYearbyCountries #pas très beau


####--------------------------ICES 1905-2019 separating the FAO_Area----------------------#####


MUR_TLWbyYearbyFAO_Area <- ggplot(ICES1903_2019_MUR , aes(x=Year, y=TLW)) +
  geom_point(aes(color = factor(FAO_Area))) +
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus by countries by year from 1905 to 2019") 

MUR_TLWbyYearbyFAO_Area  #pas très beau

####--------------------------ICES 1905-2019 France ----------------------#####


ICES1903_2019_MUR_FR <- ICES1903_2019_MUR %>% 
  filter(Country %in% c("FR","France")) #Select France 
ICES1903_2019_MUR_FR

#sum landings just for France 
ICES1903_2019_MUR_FR_TLWbyYear<-ICES1903_2019_MUR_FR%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>% #sum Landings just for France 
  ungroup() 

ICES1903_2019_MUR_FR_TLWbyYear

#convert in data.frame 
Year2 <- c(1950:2019) #no information between 1903:1949
TLWFR <- ICES1903_2019_MUR_FR_TLWbyYear$TLW
df2<-data.frame(Year2, TLWFR) # neccesary if I want the plot with geom_line()
df2


MUR_TLWbyYearFrance <- ggplot(df2 , aes(x=Year2, y=TLWFR)) +
  geom_point()+
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus France by year from 1950 to 2019")+
  geom_line()

MUR_TLWbyYearFrance


####--------------------------ICES 1905-2019 All the countries except France------------------#####

ICES1903_2019_MUR_withoutFR <- ICES1903_2019_MUR %>% 
  filter(Country != "FR", Country !="France") #unselect just France 
ICES1903_2019_MUR_withoutFR

#sum landings All the countries except France
ICES1903_2019_MUR_withoutFR_TLWbyYear<-ICES1903_2019_MUR_withoutFR%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>% #sum Landings just for France 
  ungroup() 

ICES1903_2019_MUR_withoutFR_TLWbyYear

#convert in data.frame 
Year3 <- c(1905:1924, 1927:1928, 1950:2019) #no information between 1903:1905, 1925,1926 and 1929:1949
TLWwithoutFR<- ICES1903_2019_MUR_withoutFR_TLWbyYear$TLW
df3<-data.frame(Year3, TLWwithoutFR) # neccesary if I want the plot with geom_line()
df3


MUR_TLWbyYearwithoutFR<- ggplot(df3 , aes(x=Year3, y=TLWwithoutFR)) +
  geom_point()+
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus, all countries except France by year from 1905 to 2019")+
  geom_line()

MUR_TLWbyYearwithoutFR


#Graphs landings FR et Autres pays 
plot_grid(MUR_TLWbyYearFrance, MUR_TLWbyYearwithoutFR, labels=c("FR", "nFR"), ncol = 1, nrow = 2)

####-----------------ICES 1905-2019_FAO_Area_8.a.b.c.d(Golfe de Gascogne)--------------#####


ICES1903_2019_MUR_FAO8 <- ICES1903_2019_MUR %>% 
  filter(FAO_Area %in% c("27.8", "27.8.a","27.8.b","27.8.c","27.8.d","27.8.d.2", 
  "27.8.d_NK", "VIII a", "VIII b", "VIII c", "VIII d (not specified)", "27.8_NK", 
  "VIII (not specified)")) #Select Area 8.a.b.c.d  
ICES1903_2019_MUR_FAO8

#sum landings just for FAO_Area_8.a.b.c.d(Golfe de Gascogne)
ICES1903_2019_MUR_FAO8_TLWbyYear<-ICES1903_2019_MUR_FAO8%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>% #sum Landings just for France 
  ungroup() 

ICES1903_2019_MUR_FAO8_TLWbyYear

#convert in data.frame 
Year4 <- c(1950:2019) #no information between 1903:1905, 1925,1926 and 1929:1949
TLWFAO8 <- ICES1903_2019_MUR_FAO8_TLWbyYear$TLW
df4<-data.frame(Year4, TLWFAO8) # neccesary if I want the plot with geom_line()
df4


MUR_TLWbyYearFAO8<- ggplot(df4 , aes(x=Year4, y=TLWFAO8)) +
  geom_point()+
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus in FAO_area8 by year from 1905 to 2019")+
  geom_line()

MUR_TLWbyYearFAO8

#Graphs landings FR et Autres pays 
plot_grid(MUR_TLWbyYearFrance, MUR_TLWbyYearwithoutFR, MUR_TLWbyYearFAO8, labels=c("FR", "nFR", "FAO8"), ncol = 1, nrow = 3)



####---------------ICES 1905-2019_FAO_Area_7.d/4a.b.c(Manche/MerduNord) 3.a?--------------#####


ICES1903_2019_MUR_FAO7and4 <- ICES1903_2019_MUR %>% 
  filter(FAO_Area %in% c("27.7.d", "27.4","27.4.a", "27.4.b","27.4.c","27.4_NK", "IV (not specified)", 
                         "IV a", "IV b", "IV c", "VII d")) #Select Area 7.d/4a.b.c 
ICES1903_2019_MUR_FAO7and4

#sum landings just for FAO_Area_7.d/4a.b.c(Manche/MerduNord)
ICES1903_2019_MUR_FAO7and4_TLWbyYear<-ICES1903_2019_MUR_FAO7and4%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>% #sum Landings just for France 
  ungroup() 

ICES1903_2019_MUR_FAO7and4_TLWbyYear

#convert in data.frame 
Year5 <- c(1950:2019) #no information between 1903:1905, 1925,1926 and 1929:1949
TLWFAO7and4<- ICES1903_2019_MUR_FAO7and4_TLWbyYear$TLW
df5<-data.frame(Year5, TLWFAO7and4) # neccesary if I want the plot with geom_line()
df5


MUR_TLWbyYearFAO7and4<- ggplot(df5 , aes(x=Year5, y=TLWFAO7and4)) +
  geom_point()+
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus in FAO_area7 and 4 by year from 1905 to 2019")+
  geom_line()

MUR_TLWbyYearFAO7and4




#Commande très utile pour afficher les valeurs d'une variable 
  
#table(ICES1903_2019_MUR$FAO_Area)
#ICES1903_2019_MUR%>%filter(grepl("27.8|VIII",FAO_Area))%>%pull(FAO_Area)%>%table
  # le + pas besoin de tout affcihé, affiche juste celle qui nous interresse 

#FAO <-table(ICES1903_2019_MUR_FR$FAO_Area)
#a<-print(FAO)
#View(a)
#a




df1
a <- full_join(df1,df2, by=c("Year1"="Year2"))
b <- full_join(a,df3, by=c("Year1"="Year3"))
c <- full_join(b,df4, by=c("Year1"="Year4"))
d<- full_join(c,df5, by=c("Year1"="Year5"))

View(d)

####--------------------------ICES 1905-2019 Espagne----------------------#####


ICES1903_2019_MUR_ES <- ICES1903_2019_MUR %>% 
  filter(Country %in% c("ES","Spain")) #Select Spain
ICES1903_2019_MUR_ES

#sum landings just for Spain 
ICES1903_2019_MUR_ES_TLWbyYear<-ICES1903_2019_MUR_ES%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>% #sum Landings just for Spain 
  ungroup() 

ICES1903_2019_MUR_ES_TLWbyYear

#convert in data.frame 
Year7 <- c(1950:2019) #no information between 1903:1949
TLWES <- ICES1903_2019_MUR_ES_TLWbyYear$TLW
df7<-data.frame(Year7, TLWES) # neccesary if I want the plot with geom_line()
df7


MUR_TLWbyYearSpain <- ggplot(df7 , aes(x=Year7, y=TLWES)) +
  geom_point()+
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus Spain by year from 1950 to 2019")+
  geom_line()

MUR_TLWbyYearSpain

#Graphs landings FR, ES et Autres pays 
plot_grid(MUR_TLWbyYearFrance, MUR_TLWbyYearwithoutFR, MUR_TLWbyYearFAO8, MUR_TLWbyYearFAO7and4, MUR_TLWbyYearSpain, labels=c("FR", "nFR", "FAO8", "FAO7and4", "ES"), ncol = 2, nrow = 3)


####--------------------------ICES 1905-2019 Netherlands----------------------#####


ICES1903_2019_MUR_NT <- ICES1903_2019_MUR %>% 
  filter(Country %in% c("NL","NLD", "Netherlands")) #Select Netherlands 
ICES1903_2019_MUR_NT

#sum landings just for Netherlands 
ICES1903_2019_MUR_NT_TLWbyYear<-ICES1903_2019_MUR_NT%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>% #sum Landings just for Netherlands  
  ungroup() 

ICES1903_2019_MUR_NT_TLWbyYear

#convert in data.frame 
Year8 <- c(1906:1920,1922, 1950:2019) #no information between 1903:1949
TLWNT <- ICES1903_2019_MUR_NT_TLWbyYear$TLW
df8<-data.frame(Year8, TLWNT) # neccesary if I want the plot with geom_line()
df8


MUR_TLWbyYearNetherlands  <- ggplot(df8 , aes(x=Year8, y=TLWNT)) +
  geom_point()+
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus  Netherlands by year from 1906 to 2019")+
  geom_line()

MUR_TLWbyYearNetherlands

####--------------------------ICES 1905-2019 Great Britain----------------------#####


ICES1903_2019_MUR_GB <- ICES1903_2019_MUR %>% 
  filter(Country %in% c("GBE","Channel Is.- Guernsey", "Channel Is.- Jersey", "Channel Islands (ns)", 
  "Ireland", "Isle of Man", "UK - Eng+Wales+N.Irl.", "UK - Eng+Wales+N.Irl.", "UK - England & Wales", 
  "UK - Scotland", "GG", "GB", "IE", "JE")) #Select Great Britain 
ICES1903_2019_MUR_GB

#sum landings just for Great Britain 
ICES1903_2019_MUR_GB_TLWbyYear<-ICES1903_2019_MUR_GB%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>% #sum Landings just for Netherlands  
  ungroup() 

View(ICES1903_2019_MUR_GB_TLWbyYear)

#convert in data.frame 
Year9 <- c(1905:1924,1950:2019) 
TLWGB <- ICES1903_2019_MUR_GB_TLWbyYear$TLW
df9<-data.frame(Year9, TLWGB) # neccesary if I want the plot with geom_line()
df9


MUR_TLWbyYearGreatBritain  <- ggplot(df9 , aes(x=Year9, y=TLWGB)) +
  geom_point()+
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus Great Britain by year from 1905 to 2019")+
  geom_line()

MUR_TLWbyYearGreatBritain


#Graphs landings FR, ES et Autres pays 
plot_grid(MUR_TLWbyYearFrance, MUR_TLWbyYearwithoutFR, MUR_TLWbyYearFAO8, MUR_TLWbyYearFAO7and4, MUR_TLWbyYearSpain, MUR_TLWbyYearGreatBritain, MUR_TLWbyYearNetherlands, labels=c("FR", "nFR", "FAO8", "FAO7and4", "ES", "GB", "NT"), ncol = 3, nrow = 3)

