library(dplyr)
library(devtools)
library("ggpubr")
#-------------------------JEU DE DONNEES EXISTANTS -----------------------------

#result_SST_1983_2020_totalarea_W
#result_chl_1998_2021_totalarea_W 
#result_NAO_1822_2021_W

#result_L50_FI_8ab_2 
#result_L50_FI_4c7d_2
#result_L50_MI_8ab_2
#result_L50_MI_4c7d_2
#result_a50_FI_8ab_2
#result_a50_FI_4c7d_2

#result_L50_F_8ab 
#result_L50_F_4c7d
#result_L50_M_8ab
#result_L50_M_4c7d
#result_a50_F_8ab
#result_a50_F_4c7d
#result_a50_M_8ab
#result_a50_M_4c7d

#------------------------------DONNEES ENV--------------------------------------
#Renommer les colonnes 
result_NAO_1822_2021_W2 <- result_NAO_1822_2021_W %>%
  rename(value = Sum_NAO_pos_W)%>%
  dplyr :: select(year, value)%>% 
  mutate (ET = NA)%>%
  mutate (area = NA)%>%
  mutate (datatype ="NAO index +")
result_NAO_1822_2021_W2

result_chl_1998_2021_totalarea_W2 <-result_chl_1998_2021_totalarea_W %>%
  rename(value =  Mean_chl_W)%>%
  mutate (datatype = "Chl-a (mg.m^-3) (log10)" )%>% 
  rename(ET = Ecart_Type_chl_W)%>%
  mutate (area_nom = ifelse(area %in% "4c7d", "Manche-Mer du Nord", "Golfe de Gascogne"))%>%
  mutate (value = log10(value))
result_chl_1998_2021_totalarea_W2

result_SST_1983_2020_totalarea_W2 <- result_SST_1983_2020_totalarea_W %>% 
  rename(value =  Mean_SST_W)%>%
  rename(ET = Ecart_Type_SST_W)%>%
  mutate (datatype ="SST (°C)")%>%
  mutate (area_nom = ifelse(area %in% "4c7d", "Manche-Mer du Nord", "Golfe de Gascogne"))
result_SST_1983_2020_totalarea_W2

result_chl_STT2<- rbind(result_chl_1998_2021_totalarea_W2, result_SST_1983_2020_totalarea_W2)
result_chl_STT2


Data_fishery <-pivot_longer(matlaneff,col=3:5)
Data_fishery
Data_fishery_2000_2020_totalarea <- Data_fishery %>%
  rename(datatype = name)%>%
  rename(area = zone)%>%
  mutate (area_nom = ifelse(area %in% "7d4c", "Manche-Mer du Nord", "Golfe de Gascogne"))%>%
  mutate (datatype = ifelse(datatype %in% "lan", "Débarquements (tonnes)", 
                                     ifelse (datatype %in% "fday", "Nombre de jour de pêche (jour)","Capture par unité d'effort \n(débarquements en tonnes / jour de pêche)"))) 
Data_fishery_2000_2020_totalarea 



Data_fishery_2000_2020_totalarea_separees <- matlaneff%>%
  rename("Débarquements (t)"= lan )%>%
  rename("Nombre de jour de pêche (j)" = fday )%>% 
  rename("Capture par unité d'effort (t/j)" = cpue)%>%
  mutate (area = ifelse(zone %in% "7d4c", "4c7d", "8ab"))%>%
  dplyr:: select(-"zone")
Data_fishery_2000_2020_totalarea_separees
#------------------------GRAPH SST ET CHL-A-------------------------------------

a <- ggplot(result_chl_STT2, 
       mapping = aes(x = year, y = value, color=area_nom)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  #geom_errorbar(aes(ymin = value - ET , ymax = value + ET ), width = 0.5) + 
  labs(title = "Evolution de la moyenne hivernale de la SST, [Chl-a] dans le Golf de Gascogne et en Manche/Mer du Nord",
       #caption = "Barres d erreur: ecart-type",
       x = "Temps (an)", 
       y = "") + 
  scale_x_continuous(limits=c(2000, 2021), breaks = seq(2000, 2021, 5)) +
  facet_wrap(datatype ~ area_nom, scale ="free", ncol = 2)+
  scale_colour_discrete(name= "Zones CIEM : ", labels = c("8a et 8b", "4c et 7d")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
a

#------------------------GRAPH SUM NAO +----------------------------------------
b <- ggplot(result_NAO_1822_2021_W, aes(x = year, y = Sum_NAO_pos_W)) +
  geom_bar(stat='identity') +
  scale_x_continuous(limits=c(2000, 2021), breaks = seq(2000, 2021, 5)) +
  labs(title = "Evolution de l'occurence des indice NAO positifs",
       x = "Temps (an)", 
       y = "Occurence d'indice NAO positif", 
       fill = "Nombre d'indices \nNAO poisitif :") +
  theme_bw()+
  geom_col(aes(fill = Sum_NAO_pos_W)) +
  geom_smooth(color="red") + 
  theme(plot.title = element_text(hjust = 0.5, vjust=2))
b


#------------------------GRAPH SUM TOTAL FISHEY----------------------------------------

#quick plot
c <- ggplot(Data_fishery_2000_2020_totalarea ,
            mapping = aes(x = year, y = value, color = area_nom))+
  geom_point()+ 
  geom_path()+
  geom_smooth()+
  labs(title = "Evolution de la pêche du Rouget barbet de roche dans le Golf de Gascogne et en Manche/Mer du Nord",
       #caption = "Barres d erreur: ecart-type",
       x = "Temps (an)", 
       y = "") +
  facet_wrap(datatype~area_nom,scale="free_y",ncol=2) + 
  #scale_x_continuous(limits=c(2000, 2021), breaks = seq(2000, 2021, 5)) +
  scale_colour_discrete(name= "Zones CIEM : ", labels = c("8a et 8b", "4c et 7d")) +
  theme(plot.title = element_text(hjust = 0.8)) +
  theme_bw()
c

#------------------ARRANGEMENT POUR AVOIR TT LES GRAPHS-------------------------
ggpubr :: ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)





#------------------------AGREGER LES DONNEES------------------------------------
#---------------------------------L50-------------------------------------------
#F
result_L50_F_8ab_2 <- result_L50_F_8ab %>%
  mutate(sex = "F")%>%
  mutate(area = "8ab")
result_L50_F_8ab_2 

result_L50_F_4c7d_2 <- result_L50_F_4c7d %>%
  mutate(sex = "F")%>%
  mutate(area = "4c7d")
result_L50_F_4c7d_2

#M
result_L50_M_8ab_2 <-result_L50_M_8ab %>%
  mutate(sex = "M")%>%
  mutate(area = "8ab")
result_L50_M_8ab_2

result_L50_M_4c7d_2<- result_L50_M_4c7d%>%
  mutate(sex = "M")%>%
  mutate(area = "4c7d")
result_L50_M_4c7d_2

#Regrouper les tableaux des deux zones pour les F
result_L50_F_totalarea_2 <- rbind(result_L50_F_4c7d_2, result_L50_F_8ab_2)
#Regrouper les tableaux des deux zones pour les M
result_L50_M_totalarea_2 <- rbind(result_L50_M_4c7d_2, result_L50_M_8ab_2)
#Regrouper les M et F
result_L50_FetM_totalarea_2 <- rbind(result_L50_F_totalarea_2, result_L50_M_totalarea_2)

#Renommer les colonnes 
result_L50_FetM_totalarea_2 <- result_L50_FetM_totalarea_2%>% 
  rename.variable("L50", "value_L50")%>%
  rename.variable("Ecart_Type", "Ecart_Type_L50")%>%
  rename.variable( "R2", "R2_L50")
result_L50_FetM_totalarea_2


#---------------------------------a50-------------------------------------------
#F
result_a50_F_8ab_2 <- result_a50_F_8ab%>%
  mutate(sex = "F")%>%
  mutate(area = "8ab")
result_a50_F_8ab_2

result_a50_F_4c7d_2<- result_a50_F_4c7d%>%
  mutate(sex = "F")%>%
  mutate(area = "4c7d")
result_a50_F_4c7d_2

#M
result_a50_M_8ab_2 <- result_a50_M_8ab%>%
  mutate(sex = "M")%>%
  mutate(area = "8ab")
result_a50_M_8ab_2

result_a50_M_4c7d_2<- result_a50_M_4c7d%>%
  mutate(sex = "M")%>%
  mutate(area = "4c7d")
result_a50_M_4c7d_2

#Regrouper les tableaux des deux zones pour les F
result_a50_F_totalarea_2 <- rbind(result_a50_F_4c7d_2, result_a50_F_8ab_2)
#Regrouper les tableaux des deux zones pour les M
result_a50_M_totalarea_2 <- rbind(result_a50_M_4c7d_2, result_a50_M_8ab_2)
#Regrouper les M et F
result_a50_FetM_totalarea_2 <- rbind(result_a50_F_totalarea_2, result_a50_M_totalarea_2)

#Renommer les colonnes 
result_a50_FetM_totalarea_2 <- result_a50_FetM_totalarea_2%>% 
  rename.variable("a50", "value_a50")%>%
  rename.variable("Ecart_Type", "Ecart_Type_a50")%>%
  rename.variable( "R2", "R2_a50")
result_a50_FetM_totalarea_2

#Regrouper a50 et L50
result_a50_et_L50 <- left_join(result_a50_FetM_totalarea_2, result_L50_FetM_totalarea_2, by = c("year","sex", "area"))
result_a50_et_L50
ncol(result_a50_et_L50)

#----------------------Ajouter les valeurs environnnementales-------------------
#SST
result_a50_et_L50_SST <- left_join(result_a50_et_L50, result_SST_1983_2020_totalarea_W, by = c("year","area"))
result_a50_et_L50_SST
ncol(result_a50_et_L50_SST)

#Chl
result_a50_et_L50_SST_Chl <- left_join(result_a50_et_L50_SST, result_chl_1998_2021_totalarea_W, by = c("year","area"))
ncol(result_a50_et_L50_SST_Chl)

#NAO
result_a50_et_L50_SST_Chl_NAO <- left_join(result_a50_et_L50_SST_Chl, result_NAO_1822_2021_W, by = "year")
nrow(result_a50_et_L50_SST_Chl_NAO)


result_a50_et_L50_SST_Chl_NAO_Fishery <- left_join(result_a50_et_L50_SST_Chl_NAO, Data_fishery_2000_2020_totalarea_separees, by = c("year", "area"))

result_a50_et_L50_SST_Chl_NAO_Fishery

Data_Matu_Env_Fish <- result_a50_et_L50_SST_Chl_NAO_Fishery%>% 
  relocate(sex, .after = year)%>% 
  relocate(area, .after = sex)
Data_Matu_Env_Fish


#for(i in 1:16){
#i<-1
currentyear<-idyear[i]
datatmpyear <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year==currentyear)%>%
  filter(newarea=="4c7d")%>%
  mutate(mature=as.factor(mature)) 
if(nrow(datatmpyear)>2){
  reztmp <-fl50(datatmpyear,niter=100,graph=T)
  #reztmp$L50
  result_L50_MI_4c7d$L50[i] <- median(reztmp$L50)
  result_L50_MI_4c7d$R2[i]  <- reztmp$R2
  result_L50_MI_4c7d$Ecart_Type[i]  <- sd(reztmp$L50)
  
}
result_L50_MI_4c7d$year[i] <- currentyear 
#}
result_L50_MI_4c7d



corchl_1998_2021_4c7d_S
corchl_1998_2021_4c7d_W
corchl_1998_2021_8ab_S
corchl_1998_2021_8ab_W

corSST_1982_2020_4c7d_S
corSST_1983_2020_4c7d_W
corSST_1982_2020_8ab_S
corSST_1983_2020_8ab_W

corNAO_1821_2021_S_mean
corNAO_1821_2021_S_sum_pos
corNAO_1822_2021_W_mean
corNAO_1822_2021_W_sum_pos


p.value_chl_4c7d_S <- corchl_1998_2021_4c7d_S$p.value
p.value_chl_4c7d_W <- corchl_1998_2021_4c7d_W$p.value
p.value_chl_8ab_S <- corchl_1998_2021_8ab_S$p.value
p.value_chl_8ab_W <- corchl_1998_2021_8ab_W$p.value

p.value_SST_4c7d_S <- corSST_1982_2020_4c7d_S$p.value
p.value_SST_4c7d_W <- corSST_1983_2020_4c7d_W$p.value
p.value_SST_8ab_S <- corSST_1982_2020_8ab_S$p.value
p.value_SST_8ab_W <- corSST_1983_2020_8ab_W$p.value

p.value_NAO_S_mean <- corNAO_1821_2021_S_mean$p.value
p.value_NAO_S_sum_pos <- corNAO_1821_2021_S_sum_pos$p.value
p.value_NAO_W_mean <- corNAO_1822_2021_W_mean$p.value
p.value_NAO_W_sum_pos <- corNAO_1822_2021_W_sum_pos$p.value

rho_chl_4c7d_S <- corchl_1998_2021_4c7d_S$estimate
rho_chl_4c7d_W <- corchl_1998_2021_4c7d_W$estimate
rho_chl_8ab_S <- corchl_1998_2021_8ab_S$estimate
rho_chl_8ab_W <- corchl_1998_2021_8ab_W$estimate

rho_SST_4c7d_S <- corSST_1982_2020_4c7d_S$estimate
rho_SST_4c7d_W <- corSST_1983_2020_4c7d_W$estimate
rho_SST_8ab_S <- corSST_1982_2020_8ab_S$estimate
rho_SST_8ab_W <- corSST_1983_2020_8ab_W$estimate

rho_NAO_S_mean <- corNAO_1821_2021_S_mean$estimate
rho_NAO_S_sum_pos <- corNAO_1821_2021_S_sum_pos$estimate
rho_NAO_W_mean <- corNAO_1822_2021_W_mean$estimate
rho_NAO_W_sum_pos <- corNAO_1822_2021_W_sum_pos$estimate



table_rho <- list(c(rho_chl_4c7d_S, rho_chl_4c7d_W, rho_chl_8ab_S, rho_chl_8ab_W, rho_SST_4c7d_S, 
               rho_SST_4c7d_W, rho_SST_8ab_S, rho_SST_8ab_W, rho_NAO_S_mean, rho_NAO_S_sum_pos, 
               rho_NAO_W_mean, rho_NAO_W_sum_pos))
table_rho


table_p.value <- list(c(p.value_chl_4c7d_S, p.value_chl_4c7d_W, p.value_chl_8ab_S, p.value_chl_8ab_W, 
                        p.value_SST_4c7d_S, p.value_SST_4c7d_W, p.value_SST_8ab_S, p.value_SST_8ab_W, 
                        p.value_NAO_S_mean, p.value_NAO_S_sum_pos, p.value_NAO_W_mean, 
                        p.value_NAO_W_sum_pos))

table_p.value

#-------------------------Faire des tableaux------------------------------------

#Manche-Mer du Nord et Golfe de Gascogne confondues : 

DataMatu_MUR_mean_finish <- DataMatu_MUR2 %>%
  filter(newarea == c("8a-b", "4c-7d"))%>%
  group_by(matStage, sex, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls))%>% 
  mutate(medianLen = median (lenCls))%>% 
  mutate(stdErrorLen = std.error(lenCls))%>%
  mutate(meanAge = mean (age ))%>% 
  mutate(medianAge = median (age ))%>% 
  mutate(stdErrorAge = std.error(age ))

table(DataMatu_MUR_mean_finish$matStage,DataMatu_MUR_mean_finish$sex) 
table(DataMatu_MUR_mean_finish$age,DataMatu_MUR_mean_finish$sex) 

table(DataMatu_MUR_mean_finish$meanLen, DataMatu_MUR_mean_finish$matStage)
table(DataMatu_MUR_mean_finish$stdErrorLen, DataMatu_MUR_mean_finish$matStage)


table(DataMatu_MUR_mean_finish$meanAge, DataMatu_MUR_mean_finish$matStage)
table(DataMatu_MUR_mean_finish$stdErrorAge, DataMatu_MUR_mean_finish$matStage)
#


#-------------------------Carte Moyenne annee 1982 et 2020----------------------

#------------------------------SST----------------------------------------------
#---------------------------HIVER 1983------------------------------------------
library(grid)

#Creation un indice hivernal pour les donnees de SST
nom_map_SST_W<-names(Data_SST_1982_2020_C) 
idmois<-substr(nom_map_SST_W,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_SST_W,2,5)
#correspondant au caractère donnant l annee
idsaison_W<-rep("",length(nom_map_SST_W))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 


#Boucle pour affecter l indice W au mois de janvier, fevrier, mars de 1983 et decembre de 1982 

#hiver
winter1983<-(idyear==1983) & (idmois %in%c("01","02","03"))
winter1982<- (idyear == 1982) & (idmois %in%c("12"))
winter1982_83 <-winter1983 | winter1982
idsaison_W[winter1982_83]<-paste0("W",1983)  
idsaison_W[winter1982_83]

#Filtrage : ne garde que les images ou idsaison W n est pas vide
imgtmp_W_1983<-Data_SST_1982_2020_C[[which(idsaison_W!="")]]
names(imgtmp_W_1983)
map_W_1983_SST<-idsaison_W[idsaison_W!=""]
map_W_1983_SST

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
Moy_saison_SST_1983_W <- stackApply(Data_SST_1982_2020_C, map_W_1983_SST,mean)           
plot(Moy_saison_SST_1983_W)
names(Moy_saison_SST_1983_W)
Moy_saison_SST_1983_W

levelplot(Moy_saison_SST_1983_W, 
          main = "Temperature moyenne observée lors de l'hivers 1983 en Altantique Nord-Ouest", 
          at=seq(8, 18, by = 0.1), margin=list(axis=gpar(col = 'black', fontsize = 9)))

#------------------------HIVER 2020---------------------------------------------
#Creation un indice hivernal pour les donnees de SST
nom_map_SST_W<-names(Data_SST_1982_2020_C) 
idmois<-substr(nom_map_SST_W,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_SST_W,2,5)
#correspondant au caractère donnant l annee
idsaison_W<-rep("",length(nom_map_SST_W))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 


#Boucle pour affecter l indice W au mois de janvier, fevrier, mars de 2020 et decembre de 2019 

#hiver
winter2020<-(idyear==2020) & (idmois %in%c("01","02","03"))
winter2019<- (idyear == 2019) & (idmois %in%c("12"))
winter2019_20 <-winter2020 | winter2019
idsaison_W[winter2019_20]<-paste0("W",2020)  
idsaison_W[winter2019_20]

#Filtrage : ne garde que les images ou idsaison W n est pas vide
imgtmp_W_2020<-Data_SST_1982_2020_C[[which(idsaison_W!="")]]
names(imgtmp_W_2020)
map_W_2020_SST<-idsaison_W[idsaison_W!=""]
map_W_2020_SST

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
Moy_saison_SST_2020_W <- stackApply(Data_SST_1982_2020_C, map_W_2020_SST,mean)           
plot(Moy_saison_SST_2020_W)
names(Moy_saison_SST_2020_W)
Moy_saison_SST_2020_W

levelplot(Moy_saison_SST_2020_W, 
          main = "Temperature moyenne observée lors de l'hivers 2020 en Altantique Nord-Ouest", 
          at=seq(8, 18, by = 0.1), margin=list(axis=gpar(col = 'black', fontsize = 9)))
   

#------------------------------Chl----------------------------------------------
#---------------------------HIVER 1998------------------------------------------
library(grid)

#Creation un indice hivernal pour les donnees de SST
nom_map_SST_W<-names(Data_chl_1997_2021) 
idmois<-substr(nom_map_SST_W,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_SST_W,2,5)
#correspondant au caractère donnant l annee
idsaison_W<-rep("",length(nom_map_SST_W))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 


#Boucle pour affecter l indice W au mois de janvier, fevrier, mars de 1998 et decembre de 1997 

#hiver
winter1998<-(idyear==1998) & (idmois %in%c("01","02","03"))
winter1997<- (idyear == 1997) & (idmois %in%c("12"))
winter1997_98 <-winter1998 | winter1997
idsaison_W[winter1997_98]<-paste0("W",1998)  
idsaison_W[winter1997_98]

#Filtrage : ne garde que les images ou idsaison W n est pas vide
imgtmp_W_1998<-Data_chl_1997_2007[[which(idsaison_W!="")]]
names(imgtmp_W_1998)
map_W_1998_SST<-idsaison_W[idsaison_W!=""]
map_W_1998_SST

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
Moy_saison_SST_1998_W <- stackApply(Data_chl_1997_2021, map_W_1998_SST,mean)           
plot(Moy_saison_SST_1998_W)
names(Moy_saison_SST_1998_W)
Moy_saison_SST_1998_W


library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(11, "YlGn"))
levelplot(Moy_saison_SST_1998_W, 
          col.regions= cols, 
          main = "Temperature moyenne observée lors de l'hivers 1998 en Altantique Nord-Ouest", 
          at=seq(0, 50, by = 0.01),  
          margin=list(axis=gpar(col = 'black', fontsize = 10)))

#---------------------------HIVER 2021------------------------------------------
library(grid)

#Creation un indice hivernal pour les donnees de SST
nom_map_SST_W<-names(Data_chl_1997_2021) 
idmois<-substr(nom_map_SST_W,7,8)
#correspondant au caractère donnant le mois 
idyear<-substr(nom_map_SST_W,2,5)
#correspondant au caractère donnant l annee
idsaison_W<-rep("",length(nom_map_SST_W))
#affecte le nom "" dans un vecteur de la meme longeur que le jeu de donnees 


#Boucle pour affecter l indice W au mois de janvier, fevrier, mars de 2021 et decembre de 2020 

#hiver
winter2021<-(idyear==2021) & (idmois %in%c("01","02","03"))
winter2020<- (idyear == 2020) & (idmois %in%c("12"))
winter2020_21 <-winter2021 | winter2020
idsaison_W[winter2020_21]<-paste0("W",2021)  
idsaison_W[winter2020_21]

#Filtrage : ne garde que les images ou idsaison W n est pas vide
imgtmp_W_2021<-Data_chl_1997_2021[[which(idsaison_W!="")]]
names(imgtmp_W_2021)
map_W_2021_SST<-idsaison_W[idsaison_W!=""]
map_W_2021_SST

#Selection des maps dont l indice est plein et Calcul d'une map moyenne pour chaque saison W et S 
Moy_saison_SST_2021_W <- stackApply(Data_chl_1997_2007, map_W_2021_SST,mean)           
plot(Moy_saison_SST_2021_W)
names(Moy_saison_SST_2021_W)
Moy_saison_SST_2021_W


library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(11, "YlGn"))
levelplot(Moy_saison_SST_2021_W, 
          col.regions= cols, 
          main = "Temperature moyenne observée lors de l'hivers 2021 en Altantique Nord-Ouest", 
          at=seq(0, 50, by = 0.01),  
          margin=list(axis=gpar(col = 'black', fontsize = 10)))


#







