#Red mullet fishery data

#Package 
#install.packages("knitr")
library("knitr")

#Donnees de debarquements (tonnes) de rouget par annee, division CIEM, rectangle, engin, atlantique nord, pavillon fr
laneff <- readRDS("C:/Users/vmartin/Desktop/Stage/GIT/Rouget_MATO/data_confidential/Pêche SACROIS/laneff.rds")
laneff
#rect = sous-division CIEM 
#gear = type d engin 
#fday = jour en mer 
#class_mpa = longueur des navires en m 


table(laneff$area)

table(laneff$year)

 
#Donnees de debarquements (tonnes) de rouget par annee, division CIEM (extraction 8ab, 4c7b), cpue = catch per unit effort (debarquement/par journee en mer)
matlaneff <- readRDS("C:/Users/vmartin/Desktop/Stage/GIT/Rouget_MATO/data_confidential/Pêche SACROIS/matlaneff.rds")
matlaneff
#extraction 8ab, 4c7b 



#Calcul moyenne débarquements par année 
View(matlaneff)
table(laneff$year)

#Landings by area
#Landings in ton & percentage, with fishing effort over the period 2000-2022 by area.
tab_landings_area <-laneff%>%group_by(area)%>%                                         #groupement par zone CIEM
  summarise(landings_tot=round(sum(lan_tot),fishery_day=sum(fday)))%>%                 #somme des debarquements et des jour de pêche par zone CIEM
  mutate(percentage_landings=round(100*landings_tot/sum(landings_tot),3))%>%           #calcul du pourcentage des debarquements par zone CIEM 
  ungroup()%>%
  arrange(desc(percentage_landings))                                                   #rearrangement des pourcentages de façon croissante 
kable(tab_landings_area)

#Landings in ton & percentage, with effort over the period 2000-2022 by fishing gear.
tab_landings_gear<-laneff%>%group_by(gear)%>%                                       #groupement par type d engin 
  summarise(landings_tot=round(sum(lan_tot),1),fishery_day=round(sum(fday),1))%>%   #somme des debarquements et des jour de pêche par type d engin 
  mutate(percentage_landings=round(100*landings_tot/sum(landings_tot),3))%>%        #calcul du pourcentage des debarquements par type d engin
  ungroup()%>%
  arrange(desc(percentage_landings))                                                #rearrangement des pourcentages de façon croissante 
kable(tab_landings_gear)
write.table(tab_landings_gear, "p.csv", row.names=FALSE, sep=";",dec=",", na=" ")


#landings by ICES division
tsarea<-laneff%>%
  group_by(area=sub("27.","",area),year)%>%
  summarise(lan_tot=sum(lan_tot,na.rm=T))
tsarea

ggplot(tsarea,aes(x=year,y=lan_tot)) +
  geom_bar(stat="identity") +
  facet_grid(area~.,scale="free") +
  xlab("Year")+ylab("Landings (t)") +
  theme_bw() +
  ggtitle("French landings for red mullet")+
  scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, 50))

#landings by gear
tsareamet<-laneff%>%
  group_by(area=sub("27.","",area),year,gear)%>%
  summarise(lan_tot=sum(lan_tot,na.rm=T))

ggplot(tsareamet,aes(x=year,y=lan_tot,colour=gear,fill=gear)) +
  geom_bar(stat="identity") +
  facet_grid(area~.,scale="free") +
  xlab("Year") +
  ylab("Landings (t)") +
  theme_bw() +
  ggtitle("French landings for red mullet") +
  scale_y_continuous(limits=c(0, 100), breaks = seq(0, 100, 50))

#landings by gear
tsareamet<-laneff%>%
  group_by(area = sub("27.","",area),year,gear)%>%
  summarise(fishery_day_total = sum(fday))

ggplot(tsareamet,aes(x=year,y=fishery_day_total,colour=gear,fill=gear)) +
  geom_bar(stat="identity") +
  facet_grid(area~.,scale="free") +
  xlab("Year") +
  ylab("Effort (fishing days)") +
  theme_bw() +
  ggtitle("French effort of vessels with red mullet landings") +
  scale_y_continuous(limits=c(0, 20000), breaks = seq(0,20000, 10000))


#load geographical object for mapping
#rect
load("C:/Users/vmartin/Desktop/Stage/GIT/Rouget_MATO/data/Données divisions/rect.rdata")
#div
load("C:/Users/vmartin/Desktop/Stage/GIT/Rouget_MATO/data/Données divisions/div.rdata")

plot(div)
plot(div["F_DIVISION"])
plot(rect)
div8ab <- div%>%dplyr::filter(F_DIVISION%in%c("27.8.a" , "27.8.b"))
plot(div8ab)
div4c7d <- div%>%dplyr::filter(F_DIVISION%in%c("27.4.c" , "27.7.d"))
plot(div4c7d)

#prepa geo data
rect<-rect%>%filter(ICESNAME%in%unique(laneff$rect))%>%
  transmute(rect=ICESNAME,div=F_DIVISION,label=ICESNAME,geometry)

rectxy<-st_coordinates(st_centroid(rect))

rect$x<-round(rectxy[,1],2)
rect$y<-round(rectxy[,2],2)
#div
div<-div%>%
  transmute(div=F_DIVISION,label=ETIQUETTE,geometry)%>%
  filter(div%in%unique(rect$div))

divxy<-st_coordinates(st_centroid(div))

div$x<-divxy[,1]
div$y<-divxy[,2]
#prep laneff for mapping
rlaneff<-laneff%>%group_by(year,rect)%>%
  summarise(lan_tot=sum(lan_tot),fday=sum(fday))%>%
  ungroup()%>%
  #remove empty rect
  filter(rect!="")%>%
  #add rect coordinates
  left_join(rect,by=c("rect"))


rx<-c(-13,7)#range(rlaneff$x)
ry<-c(43,55)#range(rlaneff$y)

#Carte de chaleur des debarquements de rouget 
pltlanyear<-ggplot()+
  geom_raster(data=rlaneff,aes(x=x,y=y,fill=lan_tot),stat="identity",alpha=1)+
  geom_sf(data=div,fill=NA,colour="black",lwd=.3,alpha=.5)+
  scale_fill_distiller(palette='Spectral',name="Débarquements (tonnes)")+
  facet_wrap(~year,drop=FALSE)+
  borders("world",fill="grey",colour=NA,alpha=.5)+
  coord_sf(xlim=rx,ylim=ry)+
  theme_bw()+
  ggtitle("Débarquements français de rouget barbet de roche")+
  xlab("Longitude")+ylab("Latitude")+
  theme(legend.position="bottom",
        strip.text.y=element_text(size=6,angle=0))
print(pltlanyear)


#Carte de chaleur des jours de peche de rouget
plteffyear<-ggplot()+
  geom_raster(data=rlaneff,aes(x=x,y=y,fill=fday),stat="identity",alpha=1)+
  geom_sf(data=div,fill=NA,colour="black",lwd=.3,alpha=.5)+
  scale_fill_distiller(palette='Spectral',name="Landings (tons)")+
  facet_wrap(~year,drop=FALSE)+
  borders("world",fill="grey",colour=NA,alpha=.5)+
  coord_sf(xlim=rx,ylim=ry)+
  theme_bw()+
  ggtitle("Fishings days for french vessels with red mullet landings")+
  xlab("Longitude")+ylab("Latitude")+
  theme(legend.position="bottom",
        strip.text.y=element_text(size=6,angle=0))
print(plteffyear)


pipo<-pivot_longer(matlaneff,col=3:5)
pipo

#quick plot
ggplot(pipo,aes(x=year,y=value))+
  geom_point()+
  geom_path()+
  geom_smooth()+
  facet_wrap(name~zone,scale="free_y",ncol=2)








