                               #MANCHE / MER DU NORD

####-------------------------------ICES 7.d_7.e----------------------------------------######
#--------------------------------------------------------------------------------------------


####---------------------------ICES 1903-1949_7.d/e--------------------------------------######
ICES1903_1949_MUR_7 <- ICES1903_1949 %>% 
  filter(FAO_Species_Name %in% c("Surmullet"), FAO_Area %in% c("VIId,e"))%>%  convert(num(7))
ICES1903_1949_MUR_7


####---------------------------ICES 1950_2005_7.d/e--------------------------------------######

ICES1950_2010_MUR_7 <- ICES1950_2010 %>% 
  filter(Species %in% c("Striped mullet","Surmullets(=Red mullets) nei","Red mullet"), 
         Division %in% c("VII d+e (not specified)", "VII d", "VII e"))
ICES1950_2005_MUR_7  <- ICES1950_2010_MUR_7[, -c(59:64)] 

ICES1950_2005_MUR_7
####---------------------------ICES 2006_2019_7.d/e--------------------------------------######
ICES2006_2019_MUR_7 <- ICES2006_2019 %>% 
  filter(Species %in% c("MUR"), Area %in% c("27.7.d", "27.7.e"))

ICES1903_1949_MUR_7


                                #GOLFE DE GASCOGNE

####-------------------------------ICES 8.a_8.b----------------------------------------######
#--------------------------------------------------------------------------------------------



####---------------------------ICES 1903-1949_8.a/b--------------------------------------######
ICES1903_1949_MUR_8 <- ICES1903_1949 %>% 
  filter(FAO_Species_Name %in% c("Surmullet"), FAO_Area %in% c("VIIIa","VIII"))%>%  convert(num(7))
ICES1903_1949_MUR_8


####---------------------------ICES 1950_2005_8.a/b--------------------------------------######

ICES1950_2010_MUR_8 <- ICES1950_2010 %>% 
  filter(Species %in% c("Striped mullet","Surmullets(=Red mullets) nei","Red mullet"), 
         Division %in% c("VIII a", "VIII b", "VIII c" ,"VIII (not specified)", 
                         "VIII d (not specified)","VIII d1", "VIII d2",
                         "VIII e (not specified)"))
ICES1950_2005_MUR_8  <- ICES1950_2010_MUR_8 [, -c(59:64)] 

ICES1950_2005_MUR_8
####---------------------------ICES 2006_2019_8.a/b--------------------------------------######
ICES2006_2019_MUR_8 <- ICES2006_2019 %>% 
  filter(Species %in% c("MUR"), Area %in% c("27.8","27.d", "27.7.e"))

ICES1903_1949_MUR_7

