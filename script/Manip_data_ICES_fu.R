



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

View(ICES1903_1949_MUR)

ICES1903_1949_MUR <-ICES1903_1949_MUR[,-c(6:7) ] #delete columns not useful "FAO_Species_Code" and "Species scientific name"

colnames(ICES1903_1949_MUR) <- c("Species","FAO_Area","Country","Year","TLW") #rename columns 



####--------------------------ICES 1903-2019--------------------------------------------#####


ICES1903_2019_MUR <- rbind(ICES1903_1949_MUR, ICES1950_2005_MUR, ICES2006_2019_MUR) #combine the data.frame for the tree periods 
View(ICES1903_2019_MUR)

#sum landings 
ICES1903_2019_MUR_TLWbyYear<-ICES1903_2019_MUR%>%group_by(Year)%>% #do the sum of landings each year 
  summarise(TLW=sum(TLW,na.rm=T))%>%
  ungroup() 


####--------------------------ICES 1905-2019 for all the countries----------------------#####

#convert in data.frame 
Year <- c(1905:1924, 1927:1928, 1950:2019) #no information between 1903:1905, 1925,1926 and 1929:1949
TLW <- ICES1903_2019_MUR_TLWbyYear$TLW
df4<-data.frame(Year, TLW) # neccesary if I want the plot with geom_line()
df4

#plot
MUR_TLWbyYear <- ggplot(df4, aes(x=Year, y=TLW)) +
  geom_point() +
  geom_line()   +
  labs(x = "Year", y = "TLW (tons)", 
       title = "TLW for Mullus surmuletus in all the countries by year from 1905 to 2019") +
  geom_smooth() 

MUR_TLWbyYear

####--------------------------ICES 1905-2019 separating the countries ----------------------#####

#ICES2006_2019_MUR <- ICES2006_2019 %>% 
#filter(Species %in% c("MUR"))%>%


MUR_TLWbyYearbyCountries <- ggplot(ICES1903_2019_MUR , aes(x=Year, y=TLW)) +
geom_point(aes(color = factor(Country))) +
labs(x = "Year", y = "TLW (tons)", 
     title = "TLW for Mullus surmuletus by countries by year from 1905 to 2019") 
  
MUR_TLWbyYearbyCountries
