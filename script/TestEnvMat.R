# TRAITEMENT DE FICHIER netcdf (SST, CHLOROA) ET DONNEES NAO DANS L ETUDE DU ROUGET BARBERT 
#un exemple à l aide du package raster

#les packages utiles 
#install.packages("sp") ; install.packages("rgdal") ; install.packages("raster")
library(sp) ; library(rgdal) ; library(raster)
library(sf) : library(raster) # Fonctions de base
library(rasterVis) #Pour des representations graphiques plus elaborees
library(dplyr)

#-----------------------------RECUPERATION DES DONNEES----------------------------------------- 
#Lecture des fichiers netcdf

#Temperature de surface 
Data_SST_1982_1989 <- "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/cmems-IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_1982_1989.nc"
Data_SST_1982_1989 <- stack(Data_SST_1982_1989)
Data_SST_1990_2005 <- "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/cmems-IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_1990_2005.nc"
Data_SST_1990_2005 <- stack(Data_SST_1990_2005)
Data_SST_2006_2020 <- "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/cmems-IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_2006_2020.nc"
Data_SST_2006_2020 <- stack(Data_SST_2006_2020)
#Fusion des 3 fichiers 
Data_SST_1982_2020 <- stack(Data_SST_1982_1989, Data_SST_1990_2005, Data_SST_2006_2020)
Data_SST_1982_2020
#Conversion celcius 
#Data_SST_1982_2020_C <- Data_SST_1982_2020 - 273.15
Data_SST_1982_2020_C

#Concentration en Chlorophylle a 
Data_chl_1997_2007 <- "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/dataset-oc-atl-chl-multi_cci-l4-chl_1km_monthly-rep-v02_1997_2007.nc"
Data_chl_1997_2007 <- stack(Data_chl_1997_2007)
Data_chl_2008_2019 <- "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/dataset-oc-atl-chl-multi_cci-l4-chl_1km_monthly-rep-v02_2008_2019.nc"
Data_chl_2008_2019 <- stack(Data_chl_2008_2019)
Data_chl_2020_2021 <- "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/dataset-oc-atl-chl-multi_cci-l4-chl_1km_monthly-rep-v02_2020_2021.nc"
Data_chl_2020_2021 <- stack(Data_chl_2020_2021)
#Fusion des 3 fichiers 
Data_chl_1997_2021 <- stack(Data_chl_1997_2007, Data_chl_2008_2019, Data_chl_2020_2021)
Data_chl_1997_2021


#Lecture donnees spatiales zones CIEM 
load("C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/div.rdata")
plot(div)
plot(div["F_DIVISION"])
div8ab <- div%>%dplyr::filter(F_DIVISION%in%c("27.8.a" , "27.8.b"))
div4c7d <- div%>%dplyr::filter(F_DIVISION%in%c("27.4.c" , "27.7.d"))

#Extraction donnees
Data_NAO_1821_2021 <- read.table("C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/NAO/nao.dat.txt", quote="\"", comment.char="")

#Ajout de nom de colonnes 
colnames(Data_NAO_1821_2021) <- c("Year", "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december", "annual")
Data_NAO_1821_2021

#Suppression des valeurs non conformes du jeu de donnees NAO 
Data_NAO_1821_2021_2 <-  sapply(Data_NAO_1821_2021,as.character)
Data_NAO_1821_2021_2 <- ifelse(Data_NAO_1821_2021_2=="-99.99",NA, Data_NAO_1821_2021_2)
Data_NAO_1821_2021_2 <-  apply(Data_NAO_1821_2021_2,2, as.numeric)
Data_NAO_1821_2021_2 <- data.frame(Data_NAO_1821_2021_2)
Data_NAO_1821_2021_2 





#________________________________________________________________________________________________________________
#--------------------------------------------HIVER & PRINTEMPS---------------------------------------------------
#________________________________________________________________________________________________________________


#---------CALCUL D UNE VALEUR MOYENNE PAR REGROUPEMET DE CARTES : ICI PAR SAISON HIVER ET PRINTEMPS------------
#--------------------------------------------------SST---------------------------------------------------------
#-------------------------------------------------HIVER--------------------------------------------------------

#Creation un indice hivernal pour les donnees de SST
nom_map_SST_W<-names(Data_SST_1982_2020_C) 
idmois<-substr(nom_map_SST_W,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_SST_W,2,5)
#correspondant au caractère donnant l annee
idsaison_W<-rep("",length(nom_map_SST_W))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 


#Boucle pour affecter l indice W au mois de janvier, fevrier, mars de l annee et decembre de l annee precedente 
for(year in 1983:2020){
  #hiver
  winterencours<-(idyear==year) & (idmois %in%c("01","02","03"))
  winteravant<- (idyear == year - 1) & (idmois %in%c("12"))
  winter<-winterencours | winteravant
  idsaison_W[winter]<-paste0("W",year)  
}

#Filtrage : ne garde que les images ou idsaison W n est pas vide
imgtmp<-Data_SST_1982_2020_C[[which(idsaison_W!="")]]
imgtmp
map_W_Full_SST<-idsaison_W[idsaison_W!=""]
map_W_Full_SST

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
#Moy_saison_SST_1983_2020_W <- stackApply(Data_SST_1982_2020_C,map_W_Full_SST,mean)           
plot(Moy_saison_SST_1983_2020_W)
names(Moy_saison_SST_1983_2020_W)
Moy_saison_SST_1983_2020_W

#Sauvegarde 
#writeRaster(Moy_saison_SST_1983_2020_W, filename="Moy_saison_SST_1983_2020.tif", format="GTiff", overwrite=TRUE)
#Moy_saison_SST_1983_2020_W_2 <- system.file("Moy_saison_SST_1983_2020.tif", package="terra")

# SST : W 1983 2020  ---> 8ab
#result_SST_1983_2020_8ab_W <-data.frame(year=rep(NA,38), Mean_SST_W=rep(NA,38), Ecart_Type_SST_W=rep(NA,38))                      
#for(id2 in 1983:2020){                                                                                                       
  i <- id2 -1982                                                                                                           
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_SST_1983_2020_W), 8,11)
  idyear <- which(id == id2)
  map_mean_SST_W <- stackApply(Moy_saison_SST_1983_2020_W[[idyear]],substr(names(Moy_saison_SST_1983_2020_W)[idyear], 8,11),mean)               
  result_SST_1983_2020_8ab_W$Mean_SST_W[i] <- raster::extract(map_mean_SST_W,  div8ab, fun=mean,na.rm=T)                               
  result_SST_1983_2020_8ab_W$Ecart_Type_SST_W[i]  <- raster::extract(map_mean_SST_W, div8ab, fun=sd,na.rm=T)
  result_SST_1983_2020_8ab_W$year[i] <- id2
  result_SST_1983_2020_8ab_W$area[i] <- "8a et 8b"
#}
result_SST_1983_2020_8ab_W
#saveRDS(result_SST_1983_2020_8ab_W, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_SST_1983_2020_8ab_W.rds")
#readRDS(result_SST_1983_2020_8ab_W, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_SST_1983_2020_8ab_W.rds")
  

plot(result_SST_1983_2020_8ab_W$year, result_SST_1983_2020_8ab_W$Mean_SST_W)

# SST : W 1983 2020  ---> 4c7d
#result_SST_1983_2020_4c7d_W <-data.frame(year=rep(NA,38), Mean_SST_W=rep(NA,38), Ecart_Type_SST_W=rep(NA,38))                      
#for(id2 in 1983:2020){                                                                                                       
  i <- id2 -1982                                                                                                           
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_SST_1983_2020_W), 8,11)
  idyear <- which(id == id2)
  map_mean_SST_W <- stackApply(Moy_saison_SST_1983_2020_W[[idyear]],substr(names(Moy_saison_SST_1983_2020_W)[idyear], 8,11),mean)               
  result_SST_1983_2020_4c7d_W$Mean_SST_W[i] <- raster::extract(map_mean_SST_W,  div4c7d, fun=mean,na.rm=T)                               
  result_SST_1983_2020_4c7d_W$Ecart_Type_SST_W[i]  <- raster::extract(map_mean_SST_W, div4c7d, fun=sd,na.rm=T)
  result_SST_1983_2020_4c7d_W$year[i] <- id2
  result_SST_1983_2020_4c7d_W$area[i] <- "4c et 7d"
#}
result_SST_1983_2020_4c7d_W
#saveRDS(result_SST_1983_2020_4c7d_W, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_SST_1983_2020_4c7d_W.rds")

plot(result_SST_1983_2020_4c7d_W$year, result_SST_1983_2020_4c7d_W$Mean_SST_W)


#---------------------------------------GRAPH STT W 8ab-4c7d---------------------------------------------

result_SST_1983_2020_totalarea_W <- rbind(result_SST_1983_2020_8ab_W , result_SST_1983_2020_4c7d_W)
result_SST_1983_2020_totalarea_W 

ggplot(result_SST_1983_2020_totalarea_W , 
       mapping = aes(x = year, y = Mean_SST_W, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST_W - Ecart_Type_SST_W , ymax = Mean_SST_W + Ecart_Type_SST_W ), width = 0.5) + 
  ylim(10, 16) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des hivers 1983 à 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(1983, 2020), breaks = seq(1983, 2020, 5))

ggplot(result_SST_1983_2020_totalarea_W , 
       mapping = aes(x = year, y = Mean_SST_W, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST_W - Ecart_Type_SST_W , ymax = Mean_SST_W + Ecart_Type_SST_W ), width = 0.5) + 
  ylim(0, 18) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des hivers 2006 à 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(2006, 2020), breaks = seq(2006, 2020, 1))

#----------------------Test de correlation de Spearman : STT W 8ab-4c7d-----------------------------------------

#4c7d
result_SST_1983_2020_4c7d_W
corSST_1983_2020_4c7d_W <-cor.test(result_SST_1983_2020_4c7d_W$year, result_SST_1983_2020_4c7d_W$Mean_SST_W, method="spearman")
corSST_1983_2020_4c7d_W
#Positif

#8ab
result_SST_1983_2020_8ab_W
corSST_1983_2020_8ab_W <-cor.test(result_SST_1983_2020_8ab_W$year, result_SST_1983_2020_8ab_W$Mean_SST_W, method="spearman")
corSST_1983_2020_8ab_W
#Positif




#--------------------------------------------------SST---------------------------------------------------------

#-----------------------------------------------PRINTEMPS------------------------------------------------------

#Creation un indice printannier pour les donnees de SST
nom_map_SST_S<-names(Data_SST_1982_2020_C) 
idmois<-substr(nom_map_SST_S,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_SST_S,2,5)
#correspondant au caractère donnant l annee
idsaison_S<-rep("",length(nom_map_SST_S))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 

#Boucle pour affecter l indice S pour les mois de avril, mai, juin 
for(year in 1982:2020){
  #printemps
  spring<-(idyear==year) & (idmois %in%c("04","05","06"))
  idsaison_S[spring]<-paste0("S",year)  
}

#Filtrage : ne garde que les images ou idsaison S n est pas vide
imgtmp<-Data_SST_1982_2020_C[[which(idsaison_S!="")]]
imgtmp
map_S_Full_SST<-idsaison_S[idsaison_S!=""]
map_S_Full_SST

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
#Moy_saison_SST_1982_2020_S <- stackApply(Data_SST_1982_2020_C,map_S_Full_SST,mean)           
plot(Moy_saison_SST_1982_2020_S)
names(Moy_saison_SST_1982_2020_S)
Moy_saison_SST_1982_2020_S


# SST : S 1982 2020  ---> 8ab
#result_SST_1982_2020_8ab_S <-data.frame(year=rep(NA,39), Mean_SST_S=rep(NA,39), Ecart_Type_SST_S=rep(NA,39))                      
#for(id2 in 1982:2020){                                                                                                       
  i <- id2 -1981                                                                                                          
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_SST_1982_2020_S), 8,11)
  idyear <- which(id == id2)
  map_mean_SST_S <- stackApply(Moy_saison_SST_1982_2020_S[[idyear]],substr(names(Moy_saison_SST_1982_2020_S)[idyear], 8,11),mean)               
  result_SST_1982_2020_8ab_S$Mean_SST_S[i] <- raster::extract(map_mean_SST_S,  div8ab, fun=mean,na.rm=T)                               
  result_SST_1982_2020_8ab_S$Ecart_Type_SST_S[i]  <- raster::extract(map_mean_SST_S, div8ab, fun=sd,na.rm=T)
  result_SST_1982_2020_8ab_S$year[i] <- id2
  result_SST_1982_2020_8ab_S$area[i] <- "8a et 8b"
#}

result_SST_1982_2020_8ab_S
#saveRDS(result_SST_1982_2020_8ab_S, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_SST_1982_2020_8ab_S.rds")
#readRDS(result_SST_1982_2020_8ab_S, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_SST_1982_2020_8ab_S.rds")

plot(result_SST_1982_2020_8ab_S$year, result_SST_1982_2020_8ab_S$Mean_SST_S)

# SST : S 1982 2020  ---> 4c7d
#result_SST_1982_2020_4c7d_S <-data.frame(year=rep(NA,39), Mean_SST_S=rep(NA,39), Ecart_Type_SST_S=rep(NA,39))                      
#for(id2 in 1982:2020){                                                                                                       
  i <- id2 -1981                                                                                                           
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_SST_1982_2020_S), 8,11)
  idyear <- which(id == id2)
  map_mean_SST_S <- stackApply(Moy_saison_SST_1982_2020_S[[idyear]],substr(names(Moy_saison_SST_1982_2020_S)[idyear], 8,11),mean)               
  result_SST_1982_2020_4c7d_S$Mean_SST_S[i] <- raster::extract(map_mean_SST_S,  div4c7d, fun=mean,na.rm=T)                               
  result_SST_1982_2020_4c7d_S$Ecart_Type_SST_S[i]  <- raster::extract(map_mean_SST_S, div4c7d, fun=sd,na.rm=T)
  result_SST_1982_2020_4c7d_S$year[i] <- id2
  result_SST_1982_2020_4c7d_S$area[i] <- "4c et 7d"
#}
result_SST_1982_2020_4c7d_S
#saveRDS(result_SST_1982_2020_4c7d_S, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_SST_1982_2020_4c7d_S.rds")

plot(result_SST_1982_2020_4c7d_S$year, result_SST_1982_2020_4c7d_S$Mean_SST_S)

#---------------------------------------GRAPH STT S 8ab-4c7d---------------------------------------------

result_SST_1982_2020_totalarea_S <- rbind(result_SST_1982_2020_8ab_S , result_SST_1982_2020_4c7d_S)
result_SST_1982_2020_totalarea_S 

ggplot(result_SST_1982_2020_totalarea_S , 
       mapping = aes(x = year, y = Mean_SST_S, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST_S - Ecart_Type_SST_S , ymax = Mean_SST_S + Ecart_Type_SST_S ), width = 0.5) + 
  ylim(0, 18) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des printemps 1983 à 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(1983, 2020), breaks = seq(1983, 2020, 5)) +
  facet_grid(~area)

ggplot(result_SST_1982_2020_totalarea_S , 
       mapping = aes(x = year, y = Mean_SST_S, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST_S - Ecart_Type_SST_S , ymax = Mean_SST_S + Ecart_Type_SST_S ), width = 0.5) + 
  ylim(0, 18) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des printemps 2006 à 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(2006, 2020), breaks = seq(2006, 2020, 1))

#----------------------Test de correlation de Spearman : STT S 8ab-4c7d-----------------------------------------

#4c7d
result_SST_1982_2020_4c7d_S
corSST_1982_2020_4c7d_S <-cor.test(result_SST_1982_2020_4c7d_S$year, result_SST_1982_2020_4c7d_S$Mean_SST_S, method="spearman")
corSST_1982_2020_4c7d_S
#Positif


#8ab
result_SST_1982_2020_8ab_S
corSST_1982_2020_8ab_S <-cor.test(result_SST_1982_2020_8ab_S$year, result_SST_1982_2020_8ab_S$Mean_SST_S, method="spearman")
corSST_1982_2020_8ab_S
#Positif




#--------------------------------------------------Chl---------------------------------------------------------
#-------------------------------------------------HIVER--------------------------------------------------------
#Creation un indice hivernal pour les donnees de Chl
nom_map_chl_W<-names(Data_chl_1997_2021) 
idmois<-substr(nom_map_chl_W,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_chl_W,2,5)
#correspondant au caractère donnant l annee
idsaison_W<-rep("",length(nom_map_chl_W))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 


#Boucle pour affecter l indice W au mois de janvier, fevrier, mars de l annee et decembre de l annee precedente 
for(year in 1998:2021){
  #hiver
  winterencours<-(idyear==year) & (idmois %in%c("01","02","03"))
  winteravant<- (idyear == year - 1) & (idmois %in%c("12"))
  winter<-winterencours | winteravant
  idsaison_W[winter]<-paste0("W",year)  
}

#Filtrage : ne garde que les images ou idsaison W n est pas vide
imgtmp<-Data_chl_1997_2021[[which(idsaison_W!="")]]
imgtmp
map_W_Full_chl<-idsaison_W[idsaison_W!=""]
map_W_Full_chl

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
#Moy_saison_chl_1998_2021_W <- stackApply(Data_chl_1997_2021,map_W_Full_chl,mean)           
plot(Moy_saison_chl_1998_2021_W)
names(Moy_saison_chl_1998_2021_W)
Moy_saison_chl_1998_2021_W

#Sauvegarde 
#writeRaster(Moy_saison_SST_1983_2020_W, filename="Moy_saison_SST_1983_2020.tif", format="GTiff", overwrite=TRUE)
#Moy_saison_SST_1983_2020_W_2 <- system.file("Moy_saison_SST_1983_2020.tif", package="terra")

#chl : W 1998 2020  ---> 8ab
#result_chl_1998_2021_8ab_W <-data.frame(year=rep(NA,24), Mean_chl_W=rep(NA,24), Ecart_Type_chl_W=rep(NA,24), area=rep(NA,24))                      
#for(id2 in 1998:2021){                                                                                                       
  i <- id2 - 1997                                                                                                          
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_chl_1998_2021_W), 8,11)
  idyear <- which(id == id2)
  map_mean_chl_W <- stackApply(Moy_saison_chl_1998_2021_W[[idyear]],substr(names(Moy_saison_chl_1998_2021_W)[idyear], 8,11),mean)               
  result_chl_1998_2021_8ab_W$Mean_chl_W[i] <- raster::extract(map_mean_chl_W,  div8ab, fun=mean,na.rm=T)                               
  result_chl_1998_2021_8ab_W$Ecart_Type_chl_W[i]  <- raster::extract(map_mean_chl_W, div8ab, fun=sd,na.rm=T)
  result_chl_1998_2021_8ab_W$year[i] <- id2
  result_chl_1998_2021_8ab_W$area[i] <- "8a et 8b"
#}
result_chl_1998_2021_8ab_W
#saveRDS(result_chl_1998_2021_8ab_W, "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_chl_1998_2021_8ab_W.rds")

plot(result_chl_1998_2021_8ab_W$year, result_chl_1998_2021_8ab_W$Mean_chl_W)


# chl : W 1983 2020  ---> 4c7d
#result_chl_1998_2021_4c7d_W <-data.frame(year=rep(NA,24), Mean_chl_W=rep(NA,24), Ecart_Type_chl_W=rep(NA,24), area=rep(NA,24))                      
#for(id2 in 1998:2021){                                                                                                       
  i <- id2 - 1997                                                                                                          
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_chl_1998_2021_W), 8,11)
  idyear <- which(id == id2)
  map_mean_chl_W <- stackApply(Moy_saison_chl_1998_2021_W[[idyear]],substr(names(Moy_saison_chl_1998_2021_W)[idyear], 8,11),mean)               
  result_chl_1998_2021_4c7d_W$Mean_chl_W[i] <- raster::extract(map_mean_chl_W,  div4c7d, fun=mean,na.rm=T)                               
  result_chl_1998_2021_4c7d_W$Ecart_Type_chl_W[i]  <- raster::extract(map_mean_chl_W, div4c7d, fun=sd,na.rm=T)
  result_chl_1998_2021_4c7d_W$year[i] <- id2
  result_chl_1998_2021_4c7d_W$area[i] <- "4c et 7d"
#}
result_chl_1998_2021_4c7d_W
#saveRDS(result_chl_1998_2021_4c7d_W, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_chl_1998_2021_4c7d_W.rds")

plot(result_chl_1998_2021_4c7d_W$year, result_chl_1998_2021_4c7d_W$Mean_chl_W)


result_chl_1998_2021_totalarea_W <-rbind(result_chl_1998_2021_8ab_W , result_chl_1998_2021_4c7d_W)
result_chl_1998_2021_totalarea_W

#---------------------------------------------GRAPH Chl W 8ab-4c7d---------------------------------------------
#Graph avec passage en log10
ggplot(result_chl_1998_2021_totalarea_W , 
       mapping = aes(x = year, y = Mean_chl_W, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(-1,7) +
  labs(title = "Evolution de la moyenne de la Chl dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des hivers 1998 à 2021",
       x = "Temps (an)",
       y = "Concentration en Chlorphylle a (mg/m3)") + 
  scale_x_continuous(limits=c(1998 , 2021), breaks = seq(1998 , 2021, 5)) +
  scale_y_log10()

ggplot(result_chl_1998_2021_totalarea_W , 
       mapping = aes(x = year, y = Mean_chl_W, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = Mean_chl_W - Ecart_Type_chl_W , ymax = Mean_chl_W + Ecart_Type_chl_W ), width = 0.5) + 
  ylim(0, 13) +
  labs(title = "Evolution de la moyenne de la Chl dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des hivers 2006 à 2020",
       #caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Concentration en Chlorphylle a (mg/m3)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))+
  scale_y_log10()


#----------------------Test de correlation de Spearman : STT W 8ab-4c7d-----------------------------------------
#4c7d
result_chl_1998_2021_4c7d_W
corchl_1998_2021_4c7d_W <-cor.test(result_chl_1998_2021_4c7d_W$year, result_chl_1998_2021_4c7d_W$Mean_chl_W, method="spearman")
corchl_1998_2021_4c7d_W
#Négatif

#8ab
result_chl_1998_2021_8ab_W
corchl_1998_2021_8ab_W <-cor.test(result_chl_1998_2021_8ab_W$year, result_chl_1998_2021_8ab_W$Mean_chl_W, method="spearman")
corchl_1998_2021_8ab_W
#Négatif




#--------------------------------------------------Chl---------------------------------------------------------
#-------------------------------------------------PRINTEMPS----------------------------------------------------

#Creation un indice printannier pour les donnees de Chl
nom_map_chl_S<-names(Data_chl_1997_2021) 
idmois<-substr(nom_map_chl_S,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_chl_S,2,5)
#correspondant au caractère donnant l annee
idsaison_S<-rep("",length(nom_map_chl_S))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 

#Boucle pour affecter l indice S pour les mois de avril, mai, juin 
for(year in 1998:2021){
  #printemps
  spring<-(idyear==year) & (idmois %in%c("04","05","06"))
  idsaison_S[spring]<-paste0("S",year)  
}

#Filtrage : ne garde que les images ou idsaison S n est pas vide
imgtmp<-Data_chl_1997_2021[[which(idsaison_S!="")]]
imgtmp
map_S_Full_chl<-idsaison_S[idsaison_S!=""]
map_S_Full_chl

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
#Moy_saison_chl_1998_2021_S <- stackApply(Data_chl_1997_2021,map_S_Full_chl,mean)          
plot(Moy_saison_chl_1998_2021_S)
names(Moy_saison_chl_1998_2021_S)
Moy_saison_chl_1998_2021_S

#Chl: S 1983 2020  ---> 8ab
#result_chl_1998_2021_8ab_S <-data.frame(year=rep(NA,24), Mean_chl_S=rep(NA,24), Ecart_Type_chl_S=rep(NA,24))                      
#for(id2 in 1998 : 2021){                                                                                                       
  i <- id2 -1997                                                                                                           
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_chl_1998_2021_S), 8,11)
  idyear <- which(id == id2)
  map_mean_chl_S <- stackApply(Moy_saison_chl_1998_2021_S[[idyear]],substr(names(Moy_saison_chl_1998_2021_S)[idyear], 8,11),mean)               
  result_chl_1998_2021_8ab_S$Mean_chl_S[i] <- raster::extract(map_mean_chl_S,  div8ab, fun=mean,na.rm=T)                               
  result_chl_1998_2021_8ab_S$Ecart_Type_chl_S[i]  <- raster::extract(map_mean_chl_S, div8ab, fun=sd,na.rm=T)
  result_chl_1998_2021_8ab_S$year[i] <- id2
  result_chl_1998_2021_8ab_S$area[i] <- "8a et 8b"
#}
result_chl_1998_2021_8ab_S
#saveRDS(result_chl_1998_2021_8ab_S, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_chl_1998_2021_8ab_S.rds")
plot(result_chl_1998_2021_8ab_S$year, result_chl_1998_2021_8ab_S$Mean_chl_S)

#Chl : S 1983 2020  ---> 4c7d
#result_chl_1998_2021_4c7d_S <-data.frame(year=rep(NA,24), Mean_chl_S=rep(NA,24), Ecart_Type_chl_S=rep(NA,24))                      
#for(id2 in 1998 : 2021){                                                                                                       
  i <- id2 -1997                                                                                                           
  #id2<-2007                                                                                                                 
  id <- substr(names(Moy_saison_chl_1998_2021_S), 8,11)
  idyear <- which(id == id2)
  map_mean_chl_S <- stackApply(Moy_saison_chl_1998_2021_S[[idyear]],substr(names(Moy_saison_chl_1998_2021_S)[idyear], 8,11),mean)               
  result_chl_1998_2021_4c7d_S$Mean_chl_S[i] <- raster::extract(map_mean_chl_S,  div4c7d, fun=mean,na.rm=T)                               
  result_chl_1998_2021_4c7d_S$Ecart_Type_chl_S[i]  <- raster::extract(map_mean_chl_S, div4c7d, fun=sd,na.rm=T)
  result_chl_1998_2021_4c7d_S$year[i] <- id2
  result_chl_1998_2021_4c7d_S$area[i] <- "4c et 7d"
#}
result_chl_1998_2021_4c7d_S
#saveRDS(result_chl_1998_2021_4c7d_S, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_chl_1998_2021_4c7d_S.rds")

plot(result_chl_1998_2021_4c7d_S$year, result_chl_1998_2021_4c7d_S$Mean_chl_S)

#---------------------------------------------GRAPH Chl S 8ab-4c7d---------------------------------------------

result_chl_1998_2021_totalarea_S <-rbind(result_chl_1998_2021_8ab_S , result_chl_1998_2021_4c7d_S)
result_chl_1998_2021_totalarea_S 

#Graphs avec passage en log10
ggplot(result_chl_1998_2021_totalarea_S , 
       mapping = aes(x = year, y = Mean_chl_S, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = Mean_chl_S - Ecart_Type_chl_S , ymax = Mean_chl_S + Ecart_Type_chl_S ), width = 0.5) + 
  ylim(0, 13) +
  labs(title = "Evolution de la moyenne de la Chl dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des printemps 1998 à 2021",
       #caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Concentration en Chlorphylle a (mg/m3)") + 
  scale_x_continuous(limits=c(1998 , 2021), breaks = seq(1998 , 2021, 5)) +
  scale_y_log10()

ggplot(result_chl_1998_2021_totalarea_S , 
       mapping = aes(x = year, y = Mean_chl_S, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = Mean_chl_S - Ecart_Type_chl_S , ymax = Mean_chl_S + Ecart_Type_chl_S ), width = 0.5) + 
  ylim(0, 13) +
  labs(title = "Evolution de la moyenne de la Chl dans le Golf de Gascogne 
       et Manche/Mer du Nord lors des printemps 2006 à 2020",
       #caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Concentration en Chlorphylle a (mg/m3)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1)) +
  scale_y_log10()

#----------------------Test de correlation de Spearman : STT S 8ab-4c7d (log ou non)-----------------------------------------
#PAS LOG 
#4c7d
result_chl_1998_2021_4c7d_S
corchl_1998_2021_4c7d_S <-cor.test(result_chl_1998_2021_4c7d_S$year, result_chl_1998_2021_4c7d_S$Mean_chl_S, method="spearman")
corchl_1998_2021_4c7d_S
#Positif

#8ab
result_chl_1998_2021_8ab_S
corchl_1998_2021_8ab_S <-cor.test(result_chl_1998_2021_8ab_S$year, result_chl_1998_2021_8ab_S$Mean_chl_S, method="spearman")
corchl_1998_2021_8ab_S
#Positif

#--------------------------------------------------NAO---------------------------------------------------------
#-------------------------------------------------HIVER--------------------------------------------------------
Data_NAO_1821_2021_2 
nrow(Data_NAO_1821_2021_2)

#NAO : 1822_2021 W 
#result_NAO_1822_2021_W <-data.frame(year=rep(NA,200), Mean_NAO_W=rep(NA,200), Sum_NAO_neg_W=rep(NA,200), Sum_NAO_pos_W=rep(NA,200))                      
#for(id2 in 1822 : 2021){                                                                                                       
  i <- id2 - 1821                                                                                                           
  #id2 <-1825                                                                                                                 
  winterencours_NAO <- Data_NAO_1821_2021_2 %>% filter (Year == id2) %>% dplyr :: select(january, february, march) 
  winteravant_NAO <- Data_NAO_1821_2021_2 %>% filter (Year == id2 - 1 ) %>% dplyr ::select(december)
  winter_NAO <- cbind (winterencours_NAO, winteravant_NAO)
  winter_NAO <- as.numeric(winter_NAO)
  signe_W <- substr(winter_NAO,1,1)
  result_NAO_1822_2021_W$Sum_NAO_neg_W[i] <- sum(signe_W == "-")
  result_NAO_1822_2021_W$Sum_NAO_pos_W[i] <- 4 - (sum(signe_W == "-"))
  result_NAO_1822_2021_W$Mean_NAO_W[i] <- sum(winter_NAO, na.rm = TRUE)/4
  result_NAO_1822_2021_W$year[i] <- id2
#}
result_NAO_1822_2021_W 
#save(result_NAO_1822_2021_W, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_NAO_1822_2021_W.rds")

#---------------------------------------GRAPH NAO W : moyenne et somme indice + ---------------------------------------------
#result_NAO_1822_2021_W
#result_NAO_1822_2021_W <-data.frame(year=rep(NA,200), Mean_NAO_W=rep(NA,200), Sum_NAO_neg_W=rep(NA,200), Sum_NAO_pos_W=rep(NA,200))  
color_mean_W <- ifelse(result_NAO_1822_2021_W$Mean_NAO_W <= 0, "blue","red" )

#Moyenne 1822 a 2021
ggplot(result_NAO_1822_2021_W , 
       mapping = aes(x = year, y = Mean_NAO_W)) + 
  geom_point(color = color_mean_W) +
  geom_hline(yintercept=0) +
  geom_line() +   
  geom_smooth() + 
  ylim(-3, 3) +
  labs(title = "Evolution de la moyenne des indices NAO lors des hivers de 1822 à 2021",
       x = "Temps (an)",
       y = "NAO") + 
  scale_x_continuous(limits=c(1822 , 2021), breaks = seq(1820, 2021, 10))

#Moyenne 2006 a 2021
ggplot(result_NAO_1822_2021_W , 
       mapping = aes(x = year, y = Mean_NAO_W)) + 
  geom_point(color = color_mean_W) +    #ne veut pas fonctionner !!!
  geom_hline(yintercept=0) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(-3, 3) +
  labs(title = "Evolution de la moyenne des indices NAO lors des hivers de 2006 à 2021",
       x = "Temps (an)",
       y = "NAO") + 
  scale_x_continuous(limits=c(2006 , 2021), breaks = seq(2005, 2021, 5))


#Somme des NAO mensuel positif 1822 a 2021
ggplot(result_NAO_1822_2021_W , 
       mapping = aes(x = year, y = Sum_NAO_pos_W)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(0, 5) +
  labs(title = "Evolution du nombre d' indice NAO mensuel positif lors des hivers de 1822 à 2021",
       x = "Temps (an)",
       y = "Somme des indices NAO positifs") + 
  scale_x_continuous(limits=c(1822 , 2021), breaks = seq(1820, 2021, 10))

#Somme des NAO mensuel positif 2006 a 2021
ggplot(result_NAO_1822_2021_W , 
       mapping = aes(x = year, y = Sum_NAO_pos_W)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(0, 5) +
  labs(title = "Evolution du nombre d' indice NAO mensuel positif lors des hivers de 2006 à 2021",
       x = "Temps (an)",
       y = "Somme des indices NAO positifs") + 
  scale_x_continuous(limits=c(2006 , 2021), breaks = seq(2005, 2021, 5))

#----------------------Test de correlation de Spearman : NAO W : moyenne et somme indice +------------------------------------
#Moyenne 
corNAO_1822_2021_W_mean <-cor.test(result_NAO_1822_2021_W$year, result_NAO_1822_2021_W$Mean_NAO_W, method="spearman")
corNAO_1822_2021_W_mean
#Positif

#Somme indice + 
corNAO_1822_2021_W_sum_pos <-cor.test(result_NAO_1822_2021_W$year, result_NAO_1822_2021_W$Sum_NAO_pos_W, method="spearman")
corNAO_1822_2021_W_sum_pos
#Positif


#--------------------------------------------------NAO---------------------------------------------------------
#-------------------------------------------------PRINTEMPS----------------------------------------------------
#NAO : 1821_2021 S 
#result_NAO_1821_2021_S <-data.frame(year=rep(NA,201), Mean_NAO_S=rep(NA,201), Sum_NAO_neg_S=rep(NA,201), Sum_NAO_pos_S=rep(NA,201))                      
#for(id2 in 1821 : 2021){                                                                                                       
  i <- id2 - 1820                                                                                                           
  #id2 <-1825                                                                                                                 
  spring_NAO <- Data_NAO_1821_2021_2 %>% filter (Year == id2) %>% dplyr :: select (april, may, june) 
  spring_NAO <- as.numeric(spring_NAO)
  signe_S <- substr(spring_NAO,1,1)
  result_NAO_1821_2021_S$Sum_NAO_neg_S[i] <- sum(signe_S == "-")
  result_NAO_1821_2021_S$Sum_NAO_pos_S[i] <- 3 - (sum(signe_S == "-"))
  result_NAO_1821_2021_S$Mean_NAO_S[i] <- sum(spring_NAO, na.rm = TRUE)/3
  result_NAO_1821_2021_S$year[i] <- id2
#}
result_NAO_1821_2021_S
#save(result_NAO_1821_2021_S, file = "C:/Users/vmartin/Desktop/Stage/Données/Données environnementales/Results/result_NAO_1821_2021_S.rds")


#---------------------------------------GRAPH NAO S : moyenne et somme indice + ---------------------------------------------
#result_NAO_1821_2021_S
#result_NAO_1821_2021_S <-data.frame(year=rep(NA,201), Mean_NAO_S=rep(NA,201), Sum_NAO_neg_S=rep(NA,201), Sum_NAO_pos_S=rep(NA,201))
color_mean_S <- ifelse(result_NAO_1821_2021_S$Mean_NAO_S<= 0, "blue","red" )

#Moyenne 1821 a 2021
ggplot(result_NAO_1821_2021_S , 
       mapping = aes(x = year, y = Mean_NAO_S))  + 
  geom_point(color = color_mean_S) +
  geom_hline(yintercept=0) +
  geom_line() +   
  geom_smooth() + 
  ylim(-3, 3) +
  labs(title = "Evolution de la moyenne des indices NAO lors des printemps de 1821 à 2021",
       x = "Temps (an)",
       y = "NAO") + 
  scale_x_continuous(limits=c(1821 , 2021), breaks = seq(1820, 2021, 10))

#Moyenne 2006 a 2021
ggplot(result_NAO_1821_2021_S , 
       mapping = aes(x = year, y = Mean_NAO_S))  + 
  geom_point(color = color_mean_S) +
  geom_hline(yintercept=0) +
  geom_line() +   
  geom_smooth() + 
  ylim(-3, 3) +
  labs(title = "Evolution de la moyenne des indices NAO lors des printemps de 2006 à 2021",
       x = "Temps (an)",
       y = "NAO") + 
  scale_x_continuous(limits=c(2006 , 2021), breaks = seq(2005, 2021, 5))


#Somme des NAO mensuel positif 1822 a 2021
ggplot(result_NAO_1821_2021_S , 
       mapping = aes(x = year, y = Sum_NAO_pos_S)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(0, 3) +
  labs(title = "Evolution du nombre d' indice NAO mensuel positif lors des printemps de 1821 à 2021",
       x = "Temps (an)",
       y = "Somme des indices NAO positifs") + 
  scale_x_continuous(limits=c(1821 , 2021), breaks = seq(1820, 2021, 10))

#Somme des NAO mensuel positif 2006 a 2021
ggplot(result_NAO_1821_2021_S, 
       mapping = aes(x = year, y = Sum_NAO_pos_S)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(0, 3) +
  labs(title = "Evolution du nombre d' indice NAO mensuel positif lors des printemps de 2006 à 2021",
       x = "Temps (an)",
       y = "Somme des indices NAO positifs") + 
  scale_x_continuous(limits=c(2006 , 2021), breaks = seq(2005, 2021, 5))

#----------------------Test de correlation de Spearman : NAO S : moyenne et somme indice +------------------------------------
#Moyenne 
corNAO_1821_2021_S_mean <-cor.test(result_NAO_1821_2021_S$year, result_NAO_1821_2021_S$Mean_NAO_S, method="spearman")
corNAO_1821_2021_S_mean
#Negatif

#Somme indice + 
corNAO_1821_2021_S_sum_pos <-cor.test(result_NAO_1821_2021_S$year, result_NAO_1821_2021_S$Sum_NAO_pos_S, method="spearman")
corNAO_1821_2021_S_sum_pos
#Negatif







#



#________________________________________________________________________________________________________________
#------------------------------------------------ANNUEL----------------------------------------------------------
#________________________________________________________________________________________________________________
#-------------------CALCUL D UNE VALEUR MOYENNE PAR REGROUPEMET DE CARTES : ICI PAR ANNEE----------------------

#-------------------------------------------------SST---------------------------------------------------------
#---------------------Calcul de la moyenne/ecart-type T° zone 8ab de 1982 a 2020-----------------------------

result_SST_1982_2020_8ab <-data.frame(year=rep(NA,39),Mean_SST=rep(NA,39), Ecart_Type_SST=rep(NA,39))                     
for(id2 in 1982 : 2020){                                                                                                      
  i <- id2 - 1981                                                                                                           
  #id2<-2007                                                                                                               
  divnum <- div8ab                                                                                                        
  id <- substr(names(Data_SST_1982_2020_C), 2,5)                                                                            
  idyear <- which(id == id2)                                                                                               
  map_mean_SST <- stackApply(Data_SST_1982_2020_C[[idyear]],substr(names(Data_SST_1982_2020_C)[idyear], 2,5),mean)                  
  result_SST_1982_2020_8ab$Mean_SST[i] <- raster::extract(map_mean_SST,  divnum, fun=mean,na.rm=T)                              
  result_SST_1982_2020_8ab$Ecart_Type_SST[i]  <- raster::extract(map_mean_SST,  divnum, fun=sd,na.rm=T)                          
  result_SST_1982_2020_8ab$year[i] <- id2                                                                                         
  result_SST_1982_2020_8ab$area[i] <- "8a et 8b"
}
result_SST_1982_2020_8ab
plot(result_SST_1982_2020_8ab$year, result_SST_1982_2020_8ab$Mean_SST)

#-----------------------Graphiques series temporelles moyenne SST 8ab-----------------------------------
#1982 a 2020
ggplot(result_SST_1982_2020_8ab, 
       mapping = aes(x = year, y = Mean_SST)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST - Ecart_Type_SST, ymax = Mean_SST + Ecart_Type_SST), width = 0.5) + 
  ylim(12, 18) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne entre 1982 et 2020",
       subtitle = "Zones geographiques de pêche : 8a et 8b ",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(1982, 2020), breaks = seq(1980, 2020, 5))

#2006 a 2020
ggplot(result_SST_1982_2020_8ab, 
       mapping = aes(x = year, y = Mean_SST)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST - Ecart_Type_SST, ymax = Mean_SST + Ecart_Type_SST), width = 0.5) + 
  ylim(0, 17) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne entre 2006 et 2020",
       subtitle = "Zones geographiques de pêche : 8a et 8b ",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") +
  scale_x_continuous(limits=c(2005, 2020), breaks = seq(2005, 2020, 1))

#------------------------------------ZONE 4C ET 7D-------------------------------------------- 
#-------------Calcul de la moyenne/ecart-type T° zone 4c7d de 1982 a 2020---------------------

result_SST_1982_2020_4c7d <-data.frame(year=rep(NA,39),Mean_SST=rep(NA,39), Ecart_Type_SST=rep(NA,39))                     
for(id2 in 1982 : 2020){                                                                                                       
  i <- id2 - 1981                                                                                                             
  #id2<-2007                                                                                                                
  divnum <- div4c7d                                                                                                          
  id <- substr(names(Data_SST_1982_2020_C), 2,5)                                                                            
  idyear <- which(id == id2)                                                                                                
  map_mean_SST <- stackApply(Data_SST_1982_2020_C[[idyear]],substr(names(Data_SST_1982_2020_C)[idyear], 2,5),mean)                
  result_SST_1982_2020_4c7d$Mean_SST[i] <- raster::extract(map_mean_SST,  divnum, fun=mean,na.rm=T)                                
  result_SST_1982_2020_4c7d$Ecart_Type_SST[i]  <- raster::extract(map_mean_SST,  divnum, fun=sd,na.rm=T)                         
  result_SST_1982_2020_4c7d$year[i] <- id2
  result_SST_1982_2020_4c7d$area[i] <- "4c et 7d"
}
result_SST_1982_2020_4c7d
plot(result_SST_1982_2020_4c7d$year, result_SST_1982_2020_4c7d$Mean_SST)

#-----------------------Graphiques series temporelles moyenne SST 4c7d-----------------------------------
#1982 a 2020
ggplot(result_SST_1982_2020_4c7d, 
       mapping = aes(x = year, y = Mean_SST)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST - Ecart_Type_SST, ymax = Mean_SST + Ecart_Type_SST), width = 0.5) + 
  ylim(0, 18) +
  labs(title = "Evolution de la moyenne de la SST en Manche/Mer du Nord  entre 1982 et 2020",
       subtitle = "Zones geographiques de pêche : 4c et 7d ",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(1982, 2020), breaks = seq(1980, 2020, 5))

#2006 a 2020
ggplot(result_SST_1982_2020_4c7d, 
       mapping = aes(x = year, y = Mean_SST)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST - Ecart_Type_SST, ymax = Mean_SST + Ecart_Type_SST), width = 0.5) + 
  ylim(0, 17) +
  labs(title = "Evolution de la moyenne de la SST en Manche/Mer du Nord entre 2006 et 2020",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") +
  scale_x_continuous(limits=c(2005, 2020), breaks = seq(2005, 2020, 1))

#--------------------Graphiques series temporelles moyenne SST : 2 zones 8ab et 4c7d--------------------------------
#Fusion des deux tableau 8ab et 4c7d
result_SST_1982_2020_totalarea <- rbind(result_SST_1982_2020_4c7d, result_SST_1982_2020_8ab)
result_SST_1982_2020_totalarea

#1982 a 2020
ggplot(result_SST_1982_2020_totalarea, 
       mapping = aes(x = year, y = Mean_SST, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST - Ecart_Type_SST, ymax = Mean_SST + Ecart_Type_SST), width = 0.5) + 
  ylim(0, 18) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne et Manche/Mer du Nord  entre 1982 et 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(1982, 2020), breaks = seq(1980, 2020, 5))

#2006 a 2020
ggplot(result_SST_1982_2020_totalarea, 
       mapping = aes(x = year, y = Mean_SST, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_SST - Ecart_Type_SST, ymax = Mean_SST + Ecart_Type_SST), width = 0.5) + 
  ylim(0, 17) +
  labs(title = "Evolution de la moyenne de la SST dans le Golf de Gascogne et Manche/Mer du Nord entre 2006 et 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Temperature moyenne de surface (°C)") + 
  scale_x_continuous(limits=c(2005, 2020), breaks = seq(2005, 2020, 1))

#----------------------Test de correlation de Spearman : SST year 1982 2020-------------------------------------------------
#4c7d
corSST_1982_2020_4c7d <-cor.test(result_SST_1982_2020_4c7d$year, result_SST_1982_2020_4c7d$Mean_SST, method="spearman")
corSST_1982_2020_4c7d
#Positif

#8ab
corSST_1982_2020_8ab <-cor.test(result_SST_1982_2020_8ab$year, result_SST_1982_2020_8ab$Mean_SST, method="spearman")
corSST_1982_2020_8ab
#Positif

#-------------------------------------------------Chl---------------------------------------------------------

#-------------------Calcul de la moyenne/ecart-type [Chl a] zones 8ab de 1997 a 2021-------------------------

Data_chl_1997_2021
#1998_2020
result_chl_1998_2020_8ab <-data.frame(year=rep(NA,23),Mean_chl=rep(NA,23), Ecart_Type_chl=rep(NA,23))                    
for(id2 in 1998 : 2020){                                                                                                    
  i <- id2 -1997                                                                                                          
  #id2<-2020                                                                                                                
  divnum <- div8ab                                                                                                          
  id <- substr(names(Data_chl_1997_2021), 2,5)                                                                            
  idyear <- which(id == id2)                                                                                                
  map_mean_chl <- stackApply(Data_chl_1997_2021[[idyear]],substr(names(Data_chl_1997_2021)[idyear], 2,5),mean)                 
  result_chl_1998_2020_8ab$Mean_chl[i] <- raster::extract(map_mean_chl,  divnum, fun=mean,na.rm=T)                                
  result_chl_1998_2020_8ab$Ecart_Type_chl[i]  <- raster::extract(map_mean_chl,  divnum, fun=sd,na.rm=T)                          
  result_chl_1998_2020_8ab$year[i] <- id2
  result_chl_1998_2020_8ab$area[i] <- "8a et 8b"
}
result_chl_1998_2020_8ab
plot(result_chl_1998_2020_8ab$year, result_chl_1998_2020_8ab$Mean_chl)

#-------------Calcul de la moyenne/ecart-type [Chl a] zones 4c7d de 1998 a 2021---------------------

result_chl_1998_2020_4c7d <-data.frame(year=rep(NA,23),Mean_chl=rep(NA,23), Ecart_Type_chl=rep(NA,23))                     
for(id2 in 1998 : 2020){                                                                                                    
  i <- id2 -1997                                                                                                               
  #id2<-2007                                                                                                                
  divnum <- div4c7d                                                                                                          
  id <- substr(names(Data_chl_1997_2021), 2,5)                                                                            
  idyear <- which(id == id2)                                                                                                
  map_mean_SST <- stackApply(Data_chl_1997_2021[[idyear]],substr(names(Data_chl_1997_2021)[idyear], 2,5),mean)                
  result_chl_1998_2020_4c7d$Mean_chl[i] <- raster::extract(map_mean_SST,  divnum, fun=mean,na.rm=T)                                
  result_chl_1998_2020_4c7d$Ecart_Type_chl[i]  <- raster::extract(map_mean_SST,  divnum, fun=sd,na.rm=T)                         
  result_chl_1998_2020_4c7d$year[i] <- id2
  result_chl_1998_2020_4c7d$area[i] <- "4c et 7d"
}
result_chl_1998_2020_4c7d
plot(result_chl_1998_2020_4c7d$year, result_chl_1998_2020_4c7d$Mean_chl)

#Fusion des deux tableau 8ab et 4c7d
result_chl_1998_2020_totalarea <- rbind(result_chl_1998_2020_8ab, result_chl_1998_2020_4c7d)
result_chl_1998_2020_totalarea

#Rajout de deux colonnes : passage en log de mean et sd
#8ab + 4c7d 
result_chl_1998_2020_totalarea_log <- result_chl_1998_2020_totalarea %>% 
  mutate (Mean_chl_log = log(result_chl_1998_2020_totalarea$Mean_chl)) %>%
  mutate (Ecart_Type_chl_log = log(result_chl_1998_2020_totalarea$Ecart_Type_chl))
result_chl_1998_2020_totalarea_log

#8ab 
result_chl_1998_2020_8ab_log <- result_chl_1998_2020_8ab %>% 
  mutate (Mean_chl_log = log(result_chl_1998_2020_8ab$Mean_chl)) %>%
  mutate (Ecart_Type_chl_log = log(result_chl_1998_2020_8ab$Ecart_Type_chl))
result_chl_1998_2020_8ab_log

#4c7d
result_chl_1998_2020_4c7d_log <- result_chl_1998_2020_4c7d %>% 
  mutate (Mean_chl_log = log(result_chl_1998_2020_4c7d$Mean_chl)) %>%
  mutate (Ecart_Type_chl_log = log(result_chl_1998_2020_4c7d$Ecart_Type_chl))
result_chl_1998_2020_4c7d_log

#-----------------------Graphiques series temporelles moyenne Chl a 8ab/4c7d-----------------------------------

#PAS LOG 
#1998 a 2020
ggplot(result_chl_1998_2020_totalarea, 
       mapping = aes(x = year, y = Mean_chl, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_chl - Ecart_Type_chl, ymax = Mean_chl + Ecart_Type_chl), width = 0.5) + 
  ylim(-2.5, 15) +
  labs(title = "Evolution de la moyenne de la concentration en Chl a dans le Golf de Gascogne et Manche/Mer du Nord entre 1998 et 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Concentration en chlorophylle a (mg/m-3)") + 
  scale_x_continuous(limits=c(1998, 2020), breaks = seq(1995, 2020, 5))


#2006 a 2020
ggplot(result_chl_1998_2020_totalarea, 
       mapping = aes(x = year, y = Mean_chl, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = Mean_chl - Ecart_Type_chl, ymax = Mean_chl + Ecart_Type_chl), width = 0.5) + 
  ylim(-2.5, 15) +
  labs(title = "Evolution de la moyenne de la concentration en Chl a dans le Golf de Gascogne et Manche/Mer du Nord entre 2006 et 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Concentration en chlorophylle a (mg/m-3)") + 
  scale_x_continuous(limits=c(2006, 2020), breaks = seq(2005, 2020, 1))

#LOG
#1998 a 2020 
ggplot(result_chl_1998_2020_totalarea_log, 
       mapping = aes(x = year, y = Mean_chl_log, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = Mean_chl_log - Ecart_Type_chl_log, ymax = Mean_chl_log + Ecart_Type_chl_log), width = 0.5) + 
  ylim(-1, 2.5) +
  labs(title = "Evolution de la moyenne de la concentration en Chl (log10) a dans le Golf de Gascogne et Manche/Mer du Nord entre 1998 et 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Concentration en chlorophylle a (log10)") + 
  scale_x_continuous(limits=c(1998, 2020), breaks = seq(1995, 2020, 5))


#2006 a 2020 LOG
ggplot(result_chl_1998_2020_totalarea_log, 
       mapping = aes(x = year, y = Mean_chl_log, color=area)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = Mean_chl_log - Ecart_Type_chl_log, ymax = Mean_chl_log + Ecart_Type_chl_log), width = 0.5) + 
  ylim(-1, 2.5) +
  labs(title = "Evolution de la moyenne de la concentration en Chl (log10) a dans le Golf de Gascogne et Manche/Mer du Nord entre 2006 et 2020",
       caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "Concentration en chlorophylle a (log10)") + 
  scale_x_continuous(limits=c(2006, 2020), breaks = seq(2005, 2020, 1))

#----------------------Test de correlation de Spearman : Chl a 8ab/4c7d (LOG ou pas)----------------------------------------------- 
#PAS LOG 
#4c7d
corChl_1998_2020_4c7d <-cor.test(result_chl_1998_2020_4c7d$year, result_chl_1998_2020_4c7d$Mean_chl, method="spearman")
corChl_1998_2020_4c7d
#Negatif 

#8ab
corChl_1998_2020_8ab <-cor.test(result_chl_1998_2020_8ab$year, result_chl_1998_2020_8ab$Mean_chl, method="spearman")
corChl_1998_2020_8ab
#Positif 

#LOG 
#4c7d
corChl_1998_2020_4c7d_log <- cor.test(result_chl_1998_2020_4c7d_log$year, result_chl_1998_2020_4c7d_log$Mean_chl_log, method ="spearman")
corChl_1998_2020_4c7d_log
#Negatif 

#8ab
corChl_1998_2020_8ab_log <- cor.test(result_chl_1998_2020_8ab_log$year, result_chl_1998_2020_8ab_log$Mean_chl_log, method ="spearman")
corChl_1998_2020_8ab_log
#Positif

#---------------------------------------- NAO---------------------------------------------------------
#----------------------------NAO toutes zones annuel 1821 a 2021--------------------------------------
Data_NAO_1821_2021_2
#Il existe déjà une colonne avec les moyennes annuelles 
Data_NAO_1821_2021_2$annual

#Création d'un vecteur qui colore les points en bleu si le NAO est < 0 et rouge si > 0
color_mean_annual <- ifelse(Data_NAO_1821_2021_2$annual <= 0, "blue","red" )

result_NAO_1821_2021_annual <-data.frame(year=rep(NA,200), Sum_NAO_neg_annual=rep(NA,200), Sum_NAO_pos_annual=rep(NA,200))                      
for(id2 in 1821 : 2021){                                                                                                       
  i <- id2 - 1821                                                                                                           
  #id2 <-1825                                                                                                                 
  NAO_months_year <- Data_NAO_1821_2021_2 %>% filter (Year == id2) %>% select (january, february, march, april, may, june, july, august, 
                                                                                 september, october, november, december) 
  NAO_months_year <- as.numeric(NAO_months_year)
  signe_annual <- substr(NAO_months_year,1,1)
  result_NAO_1821_2021_annual$Sum_NAO_neg_annual[i] <- sum(signe_annual == "-")
  result_NAO_1821_2021_annual$Sum_NAO_pos_annual[i] <- 12 - (sum(signe_annual == "-"))
  result_NAO_1821_2021_annual$year[i] <- id2
}
result_NAO_1821_2021_annual  

#-----------------------Graphiques series temporelles moyenne + somme signe + : Chl a 8ab/4c7d-----------------------------------
#1821_2021
ggplot(Data_NAO_1821_2021_2, 
       mapping = aes(x = Year, y = annual)) +
  geom_point(color=color_mean_annual) +
  geom_hline(yintercept=0) +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = Mean_chl - Ecart_Type_chl, ymax = Mean_chl + Ecart_Type_chl), width = 0.5) + 
  ylim(-3, 3) +
  labs(title = "Evolution de l'indice NAO entre 1821 et 2021",
       #caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "") + 
  scale_x_continuous(limits=c(1821, 2021), breaks = seq(1820, 2020, 10))+ 
  theme_bw()

#2006_2021
ggplot(Data_NAO_1821_2021_2, 
       mapping = aes(x = Year, y = annual)) +
  geom_point(color=color_mean_annual) +
  geom_hline(yintercept=0) +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = Mean_chl - Ecart_Type_chl, ymax = Mean_chl + Ecart_Type_chl), width = 0.5) + 
  ylim(-3, 3) +
  labs(title = "Evolution de l'indice NAO entre 2006 et 2020",
       #caption = "Barres d erreur: ecart-type",
       x = "Temps (an)",
       y = "") + 
  scale_x_continuous(limits=c(2006, 2020), breaks = seq(2006, 2020))+ 
  theme_bw()

#1821_2021
ggplot(result_NAO_1821_2021_annual, 
       mapping = aes(x = year, y = Sum_NAO_pos_annual)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(0, 12) +
  labs(title = "Evolution de la somme des indices NAO positifs annuelle entre 1821 et 2021",
       x = "Temps (an)",
       y = "Somme indices NAO positifs") + 
  scale_x_continuous(limits=c(1821, 2021), breaks = seq(1820, 2020, 10))+ 
  theme_bw()

#2006_2021
ggplot(result_NAO_1821_2021_annual, 
       mapping = aes(x = year, y = Sum_NAO_pos_annual)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  ylim(0, 12) +
  labs(title = "Evolution de la somme des indices NAO positifs annuelle entre 1821 et 2021",
       x = "Temps (an)",
       y = "Somme indices NAO positifs") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2005, 2020, 1))+ 
  theme_bw()

#----------------------Test de correlation de Spearman : NAO year 1821 2021-------------------------------------------------
#annual
corData_NAO_1821_2021 <-cor.test(Data_NAO_1821_2021_2$Year, Data_NAO_1821_2021_2$annual, method="spearman")
corData_NAO_1821_2021
#Negatif 

#


#------------------------------CARTES MOYENNES HIVER ------------------------------------------------------

#---------------------------------------------SST--------------------------------------------------------

idyear_and_month_SST<- substr(names(Data_SST_1982_2020_C), 2,8)

#2020
idwinter2020 <- which(idyear_and_month_SST %in% c("2020.01" , "2020.02", "2020.03", "2019.12"))
idyear_and_month_SST[idwinter2020]
idwinter2020

mean_SST_winter_2020 <- calc(Data_SST_1982_2020_C[[idwinter2020]], fun = mean)
map_mean_SST_winter2020 <- levelplot(mean_SST_winter2020_2 , main = "Temperature moyenne de surface observée lors de l'hiver 2020 en Altantique Nord-Ouest")
map_mean_SST_winter2020

#1983
idwinter1983 <- which(idyear_and_month %in% c("1983.01" , "1983.02", "1983.03", "1982.12"))
idyear_and_month[idwinter1983]
idwinter1983

mean_SST_winter_1983 <- calc(Data_SST_1982_2020_C[[idwinter1983]], fun = mean)
map_mean_SST_winter_1983<- levelplot(mean_SST_winter_1983 , main = "Temperature moyenne de surface observée lors de l'hiver 1983 en Altantique Nord-Ouest")
map_mean_SST_winter_1983


#---------------------------------------------Chl--------------------------------------------------------

idyear_and_month_chl<- substr(names(Data_chl_1997_2021), 2,8)

#2020
idwinter2021 <- which(idyear_and_month_chl %in% c("2021.01" , "2021.02", "2021.03", "2020.12"))
idyear_and_month_chl[idwinter2021]
idwinter2021

mean_chl_winter_2021 <- calc(Data_chl_1997_2021[[idwinter2021]], fun = mean)
map_mean_chl_winter_2021 <- plot(mean_chl_winter_2021 , main = "Concentration en chlorophylle a observée lors de l'hiver 2021 en Altantique Nord-Ouest")


#1983
idwinter1998 <- which(idyear_and_month_chl %in% c("1998.01" , "1998.02", "1998.03", "1997.12"))
idyear_and_month_chl[idwinter1998]
idwinter1998

mean_chl_winter_1998 <- calc(Data_chl_1997_2021[[idwinter1998]], fun = mean)
map_mean_chl_winter_1998<- plot(mean_chl_winter_1998, main = "Concentration en chlorophylle a observée lors de l'hiver 1998 en Altantique Nord-Ouest")


#






#------------------------------CARTES MOYENNES ANNUEL ------------------------------------------------------

#---------------------------------------------SST--------------------------------------------------------

#2020
idyear <- substr(names(Data_SST_2006_2020_C), 2,5)
id2020 <- which(idyear == "2020")
idyear[id2020]
id2020

Data_SST_2020_C_mean<-stackApply(Data_SST_2006_2020_C[[id2020]],substr(names(Data_SST_2006_2020_C)[id2020],2,5),mean)
levelplot(Data_SST_2020_C_mean, main = "Temperature moyenne observée en 2020 en Altantique Nord-Ouest")

#1982
idyear2 <- substr(names(Data_SST_1982_1989_C), 2,5)
id1982 <- which(idyear2 == "1982")
idyear2[id1982]
id1982

Data_SST_1982_C_mean <-stackApply(Data_SST_1982_1989_C[[id1982]],substr(names(Data_SST_1982_1989_C)[id1982],2,5),mean)
levelplot(Data_SST_1982_C_mean, main = "Temperature moyenne observée en 1982 en Altantique Nord-Ouest")

























































































#----------------------------PAS D'IMPORTANCE + BONUS------------------------------------- 
#----GRAPHIQUES MOYENNE ANNUEL SST (°C & °K) SUR L ENSEMBLE DE LA ZONE en 2006-2020-----
kelv_to_celsius <- function(temp) { celsius <- (temp - 273.15) }
id <- substr(names(DataTemp), 2,5)

id2006 <- which(id == "2006") ; id2007 <- which(id == "2007") ; id2008 <- which(id == "2008") ; 
id2009 <- which(id == "2009") ; id2010 <- which(id == "2010") ; id2011 <- which(id == "2011") ; 
id2012 <- which(id == "2012") ; id2013 <- which(id == "2013") ; id2014 <- which(id == "2014") ; 
id2015 <- which(id == "2015") ; id2016 <- which(id == "2016") ; id2017 <- which(id == "2017") ; 
id2018 <- which(id == "2018") ; id2019 <- which(id == "2019") ; id2020 <- which(id == "2020")

#2006
DataTemp2006<-stackApply(DataTemp[[id2006]],substr(names(DataTemp)[id2006], 2,5),mean)
levelplot((DataTemp2006), main = "2006 Kelvin")
levelplot(kelv_to_celsius(DataTemp2006), main = "2006 Celsius")
#2007
DataTemp2007<-stackApply(DataTemp[[id2007]],substr(names(DataTemp)[id2007], 2,5),mean)
levelplot(DataTemp2007, main = "2007 Kelvin")
levelplot(kelv_to_celsius(DataTemp2007), main = "2007 Celsius")
#2008
DataTemp2008<-stackApply(DataTemp[[id2008]],substr(names(DataTemp)[id2008], 2,5),mean)
levelplot(DataTemp2008, main = "2008 Kelvin")
levelplot(kelv_to_celsius(DataTemp2008), main = "2008 Celsius")
#2009
DataTemp2009<-stackApply(DataTemp[[id2009]],substr(names(DataTemp)[id2009], 2,5),mean)
levelplot(DataTemp2009, main = "2009 Kelvin")
levelplot(kelv_to_celsius(DataTemp2009), main = "2009 Celsius")
#2010
DataTemp2010<-stackApply(DataTemp[[id2010]],substr(names(DataTemp)[id2010], 2,5),mean)
levelplot(DataTemp2010, main = "2010 Kelvin")
levelplot(kelv_to_celsius(DataTemp2010), main = "2010 Celsius")
#2011
DataTemp2011<-stackApply(DataTemp[[id2011]],substr(names(DataTemp)[id2011], 2,5),mean)
levelplot(DataTemp2011, main = "2011 Kelvin")
levelplot(kelv_to_celsius(DataTemp2011), main = "2011 Celsius")
#2012
DataTemp2012<-stackApply(DataTemp[[id2012]],substr(names(DataTemp)[id2012], 2,5),mean)
levelplot(DataTemp2012, main = "2012 Kelvin")
levelplot(kelv_to_celsius(DataTemp2012), main = "2012 Celsius")
#2013
DataTemp2013<-stackApply(DataTemp[[id2013]],substr(names(DataTemp)[id2013], 2,5),mean)
levelplot(DataTemp2013, main = "2013 Kelvin")
levelplot(kelv_to_celsius(DataTemp2013), main = "2013 Celsius")
#2014
DataTemp2014<-stackApply(DataTemp[[id2014]],substr(names(DataTemp)[id2014], 2,5),mean)
levelplot(DataTemp2014, main = "2014 Kelvin")
levelplot(kelv_to_celsius(DataTemp2014), main = "2014 Celsius")
#2015
DataTemp2015<-stackApply(DataTemp[[id2015]],substr(names(DataTemp)[id2015], 2,5),mean)
levelplot(DataTemp2015, main = "2015 Kelvin")
levelplot(kelv_to_celsius(DataTemp2015), main = "2015 Celsius")
#2016
DataTemp2016<-stackApply(DataTemp[[id2016]],substr(names(DataTemp)[id2016], 2,5),mean)
levelplot(DataTemp2016, main = "2016 Kelvin")
levelplot(kelv_to_celsius(DataTemp2016), main = "2016 Celsius")
#2017
DataTemp2017<-stackApply(DataTemp[[id2017]],substr(names(DataTemp)[id2017], 2,5),mean)
levelplot(DataTemp2017, main = "2017 Kelvin")
levelplot(kelv_to_celsius(DataTemp2017), main = "2017 Celsius")
#2018
DataTemp2018<-stackApply(DataTemp[[id2018]],substr(names(DataTemp)[id2018], 2,5),mean)
levelplot(DataTemp2018, main = "2018 Kelvin")
levelplot(kelv_to_celsius(DataTemp2018), main = "2018 Celsius")
#2019
DataTemp2019<-stackApply(DataTemp[[id2019]],substr(names(DataTemp)[id2019], 2,5),mean)
levelplot(DataTemp2019, main = "2019 Kelvin")
levelplot(kelv_to_celsius(DataTemp2019), main = "2019 Celsius")
#2020
DataTemp2020<-stackApply(DataTemp[[id2020]],substr(names(DataTemp)[id2020], 2,5),mean)
levelplot(DataTemp2020, main = "2020 Kelvin")
levelplot(kelv_to_celsius(DataTemp2020), main = "2020 Celsius")

#-----------------------------------------------------------------------------------------

library(maptools) ; library(maps)
#pour extraire une valeur moyenne sur un polygone
library(sf) # librairie utilisee pour manipuler les formats spatiaux de type sf
library(ggplot2)

#recuperer le fichier div 
#div est un data.frame et un objet sf : aka un tableau avec une colonne qui
#contient l information geographique (colonne geometry)
plot(div)
plot(div["F_DIVISION"])

#NOS DIFERENTS FILTRES POUR CHACUNE DE NOS ZONES D INTERET 

div8ab <- div%>%dplyr::filter(F_DIVISION%in%c("27.8.a","27.8.b"))
div4c7d <- div%>%dplyr::filter(F_DIVISION%in%c("27.4.c" , "27.7.d"))
divtotal <- div%>%dplyr::filter(F_DIVISION%in%c("27.8.a","27.8.b","27.7.d","27.4.c"))


#Test pour filtrer notre carte avec nos zones 8ab, 4c et 7d, soit en faisant des decoupages 
#selon les zones CIEM d interet
#Un exemple 
summary(div)
coco <- mask(kelv_to_celsius(DataTemp2020), mask = divtotal, inverse=FALSE)
levelplot(coco) 


#AFFICHER NOS DIFF2RENTS FILTRES POUR CHACUNE DE NOS ZONES D INTERRET SUR UNE CARTE 
ggplot()+
  geom_sf(data=div8ab,aes(geometry=geometry,fill=F_DIVISION))+
  geom_sf_label(data=div8ab,aes(geometry=geometry,label=F_DIVISION))+
  geom_sf(data=div4c,aes(geometry=geometry,fill=F_DIVISION))+
  geom_sf_label(data=div4c,aes(geometry=geometry,label=F_DIVISION))+
  geom_sf(data=div7d,aes(geometry=geometry,fill=F_DIVISION))+
  geom_sf_label(data=div7d,aes(geometry=geometry,label=F_DIVISION))+
  coord_sf(ylim=c(43,54),xlim=c(-9,8))+
  borders(fill= "dark grey") 
#-----------------------------------------------------------------------------------------
#CALCUL DE NOS VALEURS MOYENNES DE SST PAR ANNEE EN FONCTION DE NOS ZONES D INTERET 

#2006
T2006_8ab <- kelv_to_celsius(raster::extract(DataTemp2006, div8ab, fun=mean,na.rm=T))
T2006_4c <- kelv_to_celsius(raster::extract(DataTemp2006,div4c, fun=mean,na.rm=T))
T2006_7d <- kelv_to_celsius(raster::extract(DataTemp2006,div7d, fun=mean,na.rm=T))

#2007
T2007_8ab <- kelv_to_celsius(raster::extract(DataTemp2007, div8ab, fun=mean,na.rm=T))
T2007_4c <- kelv_to_celsius(raster::extract(DataTemp2007,div4c, fun=mean,na.rm=T))
T2007_7d <- kelv_to_celsius(raster::extract(DataTemp2007,div7d, fun=mean,na.rm=T))

#2008
T2008_8ab <- kelv_to_celsius(raster::extract(DataTemp2008, div8ab, fun=mean,na.rm=T))
T2008_4c <- kelv_to_celsius(raster::extract(DataTemp2008,div4c, fun=mean,na.rm=T))
T2008_7d <- kelv_to_celsius(raster::extract(DataTemp2008,div7d, fun=mean,na.rm=T))

#2009
T2009_8ab <- kelv_to_celsius(raster::extract(DataTemp2009, div8ab, fun=mean,na.rm=T))
T2009_4c <- kelv_to_celsius(raster::extract(DataTemp2009,div4c, fun=mean,na.rm=T))
T2009_7d <- kelv_to_celsius(raster::extract(DataTemp2009,div7d, fun=mean,na.rm=T))

#2010
T2010_8ab <- kelv_to_celsius(raster::extract(DataTemp2010, div8ab, fun=mean,na.rm=T))
T2010_4c <- kelv_to_celsius(raster::extract(DataTemp2010,div4c, fun=mean,na.rm=T))
T2010_7d <- kelv_to_celsius(raster::extract(DataTemp2010,div7d, fun=mean,na.rm=T))

#2011
T2011_8ab <- kelv_to_celsius(raster::extract(DataTemp2011, div8ab, fun=mean,na.rm=T))
T2011_4c <- kelv_to_celsius(raster::extract(DataTemp2011,div4c, fun=mean,na.rm=T))
T2011_7d <- kelv_to_celsius(raster::extract(DataTemp2011,div7d, fun=mean,na.rm=T))

#2012
T2012_8ab <- kelv_to_celsius(raster::extract(DataTemp2012, div8ab, fun=mean,na.rm=T))
T2012_4c <- kelv_to_celsius(raster::extract(DataTemp2012,div4c, fun=mean,na.rm=T))
T2012_7d <- kelv_to_celsius(raster::extract(DataTemp2012,div7d, fun=mean,na.rm=T))

#2013
T2013_8ab <- kelv_to_celsius(raster::extract(DataTemp2013, div8ab, fun=mean,na.rm=T))
T2013_4c <- kelv_to_celsius(raster::extract(DataTemp2013,div4c, fun=mean,na.rm=T))
T2013_7d <- kelv_to_celsius(raster::extract(DataTemp2013,div7d, fun=mean,na.rm=T))

#2014
T2014_8ab <- kelv_to_celsius(raster::extract(DataTemp2014, div8ab, fun=mean,na.rm=T))
T2014_4c <- kelv_to_celsius(raster::extract(DataTemp2014,div4c, fun=mean,na.rm=T))
T2014_7d <- kelv_to_celsius(raster::extract(DataTemp2014,div7d, fun=mean,na.rm=T))

#2015
T2015_8ab <- kelv_to_celsius(raster::extract(DataTemp2015, div8ab, fun=mean,na.rm=T))
T2015_4c <- kelv_to_celsius(raster::extract(DataTemp2015,div4c, fun=mean,na.rm=T))
T2015_7d <- kelv_to_celsius(raster::extract(DataTemp2015,div7d, fun=mean,na.rm=T))

#2016
T2016_8ab <- kelv_to_celsius(raster::extract(DataTemp2016, div8ab, fun=mean,na.rm=T))
T2016_4c <- kelv_to_celsius(raster::extract(DataTemp2016,div4c, fun=mean,na.rm=T))
T2016_7d <- kelv_to_celsius(raster::extract(DataTemp2016,div7d, fun=mean,na.rm=T))

#2017
T2017_8ab <- kelv_to_celsius(raster::extract(DataTemp2017, div8ab, fun=mean,na.rm=T))
T2017_4c <- kelv_to_celsius(raster::extract(DataTemp2017,div4c, fun=mean,na.rm=T))
T2017_7d <- kelv_to_celsius(raster::extract(DataTemp2017,div7d, fun=mean,na.rm=T))

#2018
T2018_8ab <- kelv_to_celsius(raster::extract(DataTemp2018, div8ab, fun=mean,na.rm=T))
T2018_4c <- kelv_to_celsius(raster::extract(DataTemp2018,div4c, fun=mean,na.rm=T))
T2018_7d <- kelv_to_celsius(raster::extract(DataTemp2018,div7d, fun=mean,na.rm=T))

#2019
T2019_8ab <- kelv_to_celsius(raster::extract(DataTemp2019, div8ab, fun=mean,na.rm=T))
T2019_4c <- kelv_to_celsius(raster::extract(DataTemp2019,div4c, fun=mean,na.rm=T))
T2019_7d <- kelv_to_celsius(raster::extract(DataTemp2019,div7d, fun=mean,na.rm=T))

#2020
T2020_8ab <- kelv_to_celsius(raster::extract(DataTemp2020, div8ab, fun=mean,na.rm=T))
T2020_4c <- kelv_to_celsius(raster::extract(DataTemp2020,div4c, fun=mean,na.rm=T))
T2020_7d <- kelv_to_celsius(raster::extract(DataTemp2020,div7d, fun=mean,na.rm=T))

#Data.frame avec les valeurs moyennes de SST de 2006 a 2020
#8ab
SST_8ab <- c(T2006_8ab , T2007_8ab , T2008_8ab , T2009_8ab , T2010_8ab , T2011_8ab , T2012_8ab , 
             T2013_8ab , T2014_8ab , T2015_8ab , T2016_8ab , T2017_8ab , T2018_8ab , T2019_8ab , T2020_8ab)
table_SST_8ab <- data.frame(year, SST_8ab)
table_SST_8ab

#7d
SST_7d <- c(T2006_7d , T2007_7d , T2008_7d , T2009_7d , T2010_7d , T2011_7d , T2012_7d , 
            T2013_7d , T2014_7d , T2015_7d , T2016_7d , T2017_7d , T2018_7d , T2019_7d , T2020_7d)
table_SST_7d <- data.frame(year, SST_7d)
table_SST_7d

#4c
SST_4c <- c(T2006_4c , T2007_4c , T2008_4c , T2009_4c , T2010_4c , T2011_4c , T2012_4c , 
            T2013_4c , T2014_4c , T2015_4c , T2016_4c , T2017_4c , T2018_4c , T2019_4c , T2020_4c)

table_SST_4c <- data.frame(year, SST_4c)
table_SST_4c

#----------------------------------------------------------------------------------------------------
#On peut aissu extraire la moyenne image par image avec cellStats 
#ici calcul de la valeur moyenne de chaque image annuelle

#tsDataTemp<-cellStats(DataTemp,mean)   #ne pas faire dure trop longtemps 
#plot(tsDataTemp)

#pour extraire une valeur moyenne sur un polygone
library(sf) # librairie utilisee pour manipuler les formats spatiaux de type sf
library(ggplot2)

levelplot(Data_chl_1997_2021)

load("./data/div.rdata")
load("C:/Users/vmartin/Desktop/Stage/Donnees/Donnees environnementales/Tempe_GulfStream_NorthSea_1982_2020_n54e8s43w-9/div (1).rdata")
#div est un data.frame et un objet sf : aka un tableau avec une colonne qui
#contient l information geographique (colonne geometry)

plot(div)
plot(div["F_DIVISION"])

#avec ggplot et des trucs pour que cela soit choli
ggplot()+
  geom_sf(data=div,aes(geometry=geometry,fill=F_DIVISION))+
  geom_sf_label(data=div,aes(geometry=geometry,label=F_DIVISION))+
  coord_sf(ylim=c(30,80),xlim=c(-40,50))+
  borders("world")

+geom_sf(data=div,aes(geometry=geometry,fill=F_DIVISION))+coord_sf()+borders("world")
#extract permet de resumer l information des images par polygones (ici la
#moyenne)
uu<-raster::extract(DataTemp,div,fun="mean",na.rm=T)

#on peut aussi travailler avec un seul polygone
#div1<-div%>%dplyr::filter(F_DIVISION%in%c("27.7.d","27.7.e"))
#exemple avec 8ab
div8ab<-div%>%dplyr::filter(F_DIVISION%in%c("27.8.a","27.8.b"))
div4c<-div%>%dplyr::filter(F_DIVISION%in%c("27.4.c"))
div7d<-div%>%dplyr::filter(F_DIVISION%in%c("27.7.d"))

plot(div8ab["F_DIVISION"])
#ou avec ggplot
ggplot()+
  geom_sf(data=div8ab,aes(geometry=geometry,fill=F_DIVISION))+
  geom_sf_label(data=div8ab,aes(geometry=geometry,label=F_DIVISION))+
  coord_sf(ylim=c(40,60),xlim=c(-10,10))+
  borders("world")

ggplot()+
  geom_sf(data=div4c7d,aes(geometry=geometry,fill=F_DIVISION))+
  geom_sf_label(data=div8ab,aes(geometry=geometry,label=F_DIVISION))+
  coord_sf(ylim=c(40,60),xlim=c(-10,10))+
  borders("world")
#ensuite extrat en utilisant div1
uu<-extract(DataTemp2009,div8ab,fun="mean",na.rm=T)

#------------------------------------------------------
#library(cartography)
#library(data.table)
#library(dplyr)
#library(ggplot2)
#library(ggthemes)
#library(GISTools)
#library(installr)
#library(maptools)
#library(maps)
#library(mapdata)
#library(plyr)
#library(readxl)
#library(rgdal)
#library(rgeos)
#library(raster)
#library(rasterVis)
#library(sf)
#library(sp)


## raster
#myRaster <- raster(xmn=-100, xmx=100, ymn=-60, ymx=60)
#myRaster <- init(myRaster, runif)

## polygon shapefile
#ext <- as.vector(extent(myRaster))

#boundaries <- map('worldHires', fill=TRUE,
#   xlim=ext[1:2], ylim=ext[3:4],
#  plot=FALSE)


#levelplot(coco) + layer(boundaries)

## read the map2SpatialPolygons help page for details
#IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
#bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
#                             proj4string= WGS84(projection(coco)))


#levelplot(myRaster) + layer(sp.polygons(bPols))



#rep <- "N:/05_VIE_LABOS/02_RHPEB/04_RESS_HUMAINES/01_STAGIAIRES/Valentine MARTIN_2022/"

#shp0 <- readOGR(dsn = paste(rep,"geo_ICES/ICES_areas",sep=""),
#                layer = 'ICES_Areas_20160601_cut_dense_3857')

#map <- ggplot(shp0) + 
#  geom_polygon(aes(x = long, y = lat, group = group), colour = "black", fill = NA)+ 
#  theme_void() + 
#  coord_sf(xlim = c(-9,8), ylim = c(43,54)) 

#-----------------------------------------------------------------------------------


#Trouver des infos sur les donnees
#file.info(Data_chl_1997_2007)
#et
#Data_SST_2006_2020
#ou 
#print(Data_SST_2006_2020)
#L affichage standard de raster affiche les premieres cartes uniquement
#plot(Data_SST_2006_2020)
#Plotter une couche precise
#plot(Data_SST_2006_2020[[5479]])
#ou
#a <- raster(Data_SST_2006_2020, layer=5479)
#plot(a)

#Calcul d une carte moyenne de la variable d interet
#mData_SST_2006_2020<-mean(Data_SST_2006_2020,na.rm=T) #Long donc ne pas mettre en route
#Plot avec le package raster
#plot(mData_SST_2006_2020)
#ou Plot avec RasterVis
#levelplot(mData_SST_2006_2020)


#---------------------------------------------------------------------------------------------

#-----CALCUL D UNE VALEUR MOYENNE PAR REGROUPEMET DE CARTES : DES EXEMPLES------

#subtr permet de decouper ces noms pour avoir l annee ou le mois ou autre
#substr("X2009.01.01",2,5) #renvoit les carateres 2 a 8 de la chaîne de caracteres 
#consideree (pour considere que l annee)
#stackApply permet d appliquer un operateur statistique sur un ensemble de
#cartes defini par un vecteur passe dans le deuxieme argument de la fonction
#ici dans ce cas precis : cacul de la moyenne annuelle

#Ex : Faire des regroupement par mois dans les 100 premieres images
#Data_SST_2006_2020_month_100<-stackApply(DataTemp[[1:100]],substr(names(Data_SST_2006_2020)[1:100],7,8),mean)
#levelplot(Data_SST_2006_2020_month_100)

#ou faire des regoupement pour n avoir que janvier 2006
#id <- substr(names(Data_SST_2006_2020), 2,8)
#id2 <- which(id == "2006.01")
#id[id2]
#id2
#DataTemp_01_2006<-stackApply(Data_SST_2006_2020[[id2]],substr(names(Data_SST_2006_2020)[id2],7,8),mean)
#levelplot(DataTemp_01_2006)

# test bouclade pour calcul moyenne saisonnière

#2006_2020_C
rez <-data.frame(year=rep(NA,15),Mean_SST=rep(NA,15), Ecart_Type_SST=rep(NA,15))                     #permet de creer un tableau vide avec le bon nb de colonnes et de lignes 
for(id2 in 2006:2020){                                                                                                      #indique le parametre qui va varier 
  i <- id2 -2005                                                                                                            #permet de donner le numero de la ligne dans le tableau 
  #id2<-2007                                                                                                                #exemple avec une annee pour tester 
  divnum <- div8ab                                                                                                          #selection de la zone a extraire 
  id <- substr(names(Data_SST_2006_2020_C), 2,5)                                                                            #indique ce qu il faut selectionner dans le nom de ma map 
  idyear <- which(id == id2)                                                                                                #indique qu une partie du nom de ma map doit correspondre a mon annee 
  map_mean_SST <- stackApply(Data_SST_2006_2020_C[[idyear]],substr(names(Data_SST_2006_2020_C)[idyear], 2,5),mean)                 #selectionne les maps correspondantes et calcule une map moyenne pour chaque annee 
  result_SST_2006_2020_8ab$Mean_SST[i] <- raster::extract(map_mean_SST,  divnum, fun=mean,na.rm=T)                               #calcule la moyenne de ces maps moyennes et les mets dans la bonne colonne  
  result_SST_2006_2020_8ab$Ecart_Type_SST[i]  <- raster::extract(map_mean_SST,  divnum, fun=sd,na.rm=T)                          #calcule l ecart-type de ces maps moyennes t les mets dans la bonne colonne
  result_SST_2006_2020_8ab$year[i] <- id2                                                                                          #met l annee dans la colonne correspondante 
}
result_SST_2006_2020_8ab
plot(result_SST_2006_2020_8ab$year, result_SST_2006_2020_8ab$Mean_SST)




#Carte Moyenne annee 1982 et 2020

#2020
idyear <- substr(names(Data_SST_2006_2020_C), 2,5)
id2020 <- which(idyear == "2020")
idyear[id2020]
id2020

Data_SST_2020_C_mean<-stackApply(Data_SST_2006_2020_C[[id2020]],substr(names(Data_SST_2006_2020_C)[id2020],2,5),mean)
levelplot(Data_SST_2020_C_mean, main = "Temperature moyenne observée en 2020 en Altantique Nord-Ouest")

#1982
idyear2 <- substr(names(Data_SST_1982_1989_C), 2,5)
id1982 <- which(idyear2 == "1982")
idyear2[id1982]
id1982

Data_SST_1982_C_mean <-stackApply(Data_SST_1982_1989_C[[id1982]],substr(names(Data_SST_1982_1989_C)[id1982],2,5),mean)
levelplot(Data_SST_1982_C_mean, main = "Temperature moyenne observée en 1982 en Altantique Nord-Ouest")

