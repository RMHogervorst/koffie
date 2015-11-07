################################################################
#date: 6-1-15
#by: Roel Hogervorst
#description: het grote koffieproject.
#Rversion:R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet"
# 22-6-15 
#R version 3.2.0 (2015-04-16) -- "Full of Ingredients"
# R version 3.2.2 (2015-08-14) -- "Fire Safety" 
# aanpassing 31-10-15
################################################################
#klaarmaken van workspace
## Clear R workspace ####
rm(list = ls() ) 	#dit verwijdert alle bestanden uit de workspace.
#set working directory (mocht je niet in het goede project zitten)
setwd("~/dre-roel map/R/koffie")

#openen van bestanden ####
##hierbij hebben we uit het log gekopieert naar de .txt bestanden
#die laden we. 
RoelsKoffie<-read.csv(file="koffie.txt", header=FALSE, as.is=T)
SamenKoffie<-read.csv(file="koffie met dre.txt", header=FALSE, as.is=T)
DreKoffie <-read.csv(file="coffee dre.txt",header=FALSE, as.is=T)
#variabelen toevoegen
RoelsKoffie$cat <-"roel alleen"  #categorische variabele
SamenKoffie$cat <- "Samen"
DreKoffie$cat <- "Dre alleen"
#eindresultaat is nu drie dataframes met 3 naamloze kolommen.
Coffee <-rbind(RoelsKoffie, SamenKoffie, DreKoffie) #combineren tot 1 bestand.
colnames(Coffee) <- c("datum", "tijd", "categorie") # namen aan kolommen geven.
Coffee$counter<-1  #koffiecounter toevoegen
### functiechecks is het dataframe correct?####
head(Coffee) #als deze correct is, is de rest ook correct.
head(DreKoffie)
head(RoelsKoffie)
head(SamenKoffie)
unique(Coffee$categorie) #zitten ze er alledrie in.
class(Coffee$datum)
class(Coffee$tijd)
class(Coffee$categorie) #is nu allemaal character.
unique(Coffee$counter)  #maar 1.
#einde checks.

library(dplyr)
#kolommen in het juiste type zetten en nieuwe variabelen aanmaken. ####
Coffee$datum <- as.Date(Coffee$datum, "%m-%d-%Y") #verandert datum in date format.
Coffee$datetime<- as.POSIXct(paste(Coffee$datum, Coffee$tijd, sep=" "), 
                             format="%Y-%m-%d %H:%M")
Coffee$dag<-weekdays(Coffee$datum) #creeert variabele dag.
Coffee$getaluur <- gsub("[^[:digit:]]", "", Coffee$tijd) #combineer getallen tot 1.
Coffee$getaluur <- as.numeric(Coffee$getaluur) # verander in getallen.
Coffee$dagdeel[which(Coffee$getaluur <1200)] <- "ochtend"  #wanneer getaluur is onder de 1200 dus voor twaalf uur, maak in andere variable ochend
Coffee$dagdeel[which(Coffee$getaluur >= 1200 & Coffee$getaluur <1800)] <- "middag"  
Coffee$dagdeel[which(Coffee$getaluur >=1800)] <- "avond"  
Coffee$tijd2<-sapply(strsplit(Coffee$tijd,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       } ) #van https://stackoverflow.com/questions/5186972/how-to-convert-time-mmss-to-decimal-form-in-r
##checks.
class(Coffee$datetime)  #"POSIXct" "POSIXt"
head(Coffee$datetime)
class(Coffee$tijd)
class(Coffee$dag)  #character
head(Coffee$dag) #geeft dagen weer in het nederlands.
head(Coffee$getaluur) #check werking: zijn het getallen?
class(Coffee$getaluur) # check type: is het numeric?
head(Coffee)
qplot(dagdeel, data= Coffee)
qplot(dag, data= Coffee)
## einde checks.

##optionele tussenstap om het bestand weg te schrijven####

saveRDS(Coffee, file = "Coffee.Rda")
Coffee <-readRDS(file = "Coffee.Rda")  #om het weer in te laden.
rm(DreKoffie, RoelsKoffie, SamenKoffie)  #deze hebben we niet meer nodig.
###############


#Grafieken####
library(ggplot2) ##we hebben de ggplot2 nodig voor deze awesome grafieken.
library(dplyr)  #hebben we misshien ook nodig
#qplot(dag, getaluur, data = Coffee, color = categorie,  alpha = I(1 / 2)) #plot voor koffiemomenten op de dag.
plotdaguur <- qplot(dag, getaluur, data = Coffee, color = categorie) + geom_hline(aes(yintercept= 1200))
# dagen van de week zijn verkeerd.
plotdaguur + scale_x_discrete(
        limits=c("maandag","dinsdag","woensdag", "donderdag", "vrijdag", 
                 "zaterdag", "zondag")) +geom_jitter(size=3)
ggsave("wekelijkskoffiegebruik.png", width=6, height=4) #kopieer naar bestand.

qplot(datum, tijd2, data=Coffee)+ geom_hline(aes(yintercept= 12))  #AANPASSING 3-11-15
#dit object bestaat nu uit de data, een 12 uur lijn, en de assen zijn goed.

#g = ggplot(aes (x= dag,y= dagdeel, data = Coffee ))
#g = g + scale_x_discrete(
#  limits=c("maandag","dinsdag","woensdag", "donderdag", "vrijdag", 
#           "zaterdag", "zondag"))
# g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
# g = g + geom_violin(colour = "black", size = 2)
# g = g + xlab("Type of spray") + ylab("Insect count")
# g

#per persoon apart.####
Roel <- filter(Coffee, categorie == "roel alleen")
Samen <- filter(Coffee, categorie == "Samen")
#plots
Rplot<- qplot(dag, getaluur, data = Roel)
Rplot + scale_x_discrete(limits=c("maandag","dinsdag","woensdag", "donderdag", "vrijdag", 
                 "zaterdag", "zondag"))
Rplot
ggsave("RoelPlotWeek.png", width=6, height=4) #kopieer naar bestand.
#boxplt van Roel over een week.
Rboxplot<-qplot(dag, getaluur, data = Roel, geom = "boxplot")
Rboxplot + scale_x_discrete(limits=c("maandag","dinsdag","woensdag", "donderdag", "vrijdag", 
                                     "zaterdag", "zondag")) + geom_hline(aes(yintercept= 1200))
#werkt niet
sBoxplot<- qplot(dag, getaluur, data = Samen, geom = "boxplot")
sBoxplot + scale_x_discrete(limits=c("maandag","dinsdag","woensdag", "donderdag", "vrijdag", 
                                     "zaterdag", "zondag")) + geom_hline(aes(yintercept= 1200))
ggsave("wekelijkskoffiegebruikRoeloverdag.png", width=6, height=4)

#boxplot samen over een week.
Splot<-qplot(dag, getaluur, data = Samen, geom = "boxplot")
Splot + scale_x_discrete(limits=c("maandag","dinsdag","woensdag", "donderdag", "vrijdag", 
                                  "zaterdag", "zondag")) + geom_hline(aes(yintercept= 1200))
ggsave("wekelijkskoffiegebruiksamenoverdag.png", width=6, height=4)

#koffie per dag.
Cplotdag<- qplot(dag, data = Coffee, geom = "histogram",  color = categorie)
Cplotdag + scale_x_discrete(limits=c("maandag","dinsdag","woensdag", "donderdag", "vrijdag", 
                                       "zaterdag", "zondag"))
ggsave("totaalkoffiegebruik.png", width=6, height=4)
#summaries maken?

#dingen die vin deed.
# DENSITY IN GGPLOT OPROEPEN EN DAN FACETTEN OP DAG.#####
plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=5))
# > plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=10))
# > plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=100))
# > plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=50))
# > plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=300))
# > plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=50))
# > plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=25))
# > plot(density( Coffee[Coffee$dag=="maandag", ]$getaluur, bw=100))

#table(Coffee$dag, Coffee$dagdeel) #werkt, maar ordering is niet goed.
Overzichttabel <- table(factor(Coffee$dag, 
             levels = c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag", "zondag"))
      , factor(Coffee$dagdeel, levels =  c("ochtend", "middag", "avond")))
Overzichttabel<- as.data.frame.matrix(Overzichttabel ) #omzetten in dataframe
mutate(Overzichttabel, perdag = ochtend + middag + avond)



#KNMI data  ####
#tijd koffie is van 2014-11-13 - 2015-6-5
#
#alternatief: nieuws in periode opzoeken en classificeren?
#
library(readr)
KNMI<-read_csv("KNMI_20150605.txt",skip = 24, 
               col_names = c("STN","YYYYMMDD",   "TG",   "TN",  "TNH",   "TX",  "TXH", "T10N",   
                             "SQ",    "Q",   "DR",   "RH",   "PG",   "PX",  "PXH"), 
               trim_ws = TRUE)
#source("C:/Users/roel/Documents/docs/actief/Projecten/paplusdatabestanden/code/nuttigescripts.R")
#eigenschappen_dataset(KNMI)
library(dplyr)
library(lubridate)
KNMI<-KNMI %>%
  mutate(datum = as.Date(fast_strptime(as.character(YYYYMMDD), "%Y%m%d")))%>%  #zet datum om in date formaat.
  mutate(etmaalGemiddeldeTempGradenCelsius = TG/10 , zonneschijnUren = SQ/10, 
         stralingJoulePerVierkanteCm = Q, EtmaalNeerslagMM = RH/10, gemLuchtdrukHectoPascal = PG/10)%>%
  select(datum, etmaalGemiddeldeTempGradenCelsius, zonneschijnUren, stralingJoulePerVierkanteCm,
         EtmaalNeerslagMM,gemLuchtdrukHectoPascal ) 
#missings definieeren.
KNMI$zonneschijnUren[KNMI$zonneschijnUren == -1] <- NA
KNMI$EtmaalNeerslagMM[KNMI$EtmaalNeerslagMM == -1] <- NA

#graph that shit
qplot(datum, etmaalGemiddeldeTempGradenCelsius, data= KNMI, 
      color = zonneschijnUren)+  scale_fill_brewer(type = "div")
qplot(datum, etmaalGemiddeldeTempGradenCelsius, data= KNMI, 
      color = stralingJoulePerVierkanteCm)  #+  scale_fill_brewer(type = "div")

#Combineer Roel en Weer ####
#Coffee naar per dag bestand.
Roel <- filter(Coffee, categorie == "roel alleen") #idem als hierboven
koffieRoelPerDag<-aggregate( Roel$counter, by =list(datum = Roel$datum), sum)
names(koffieRoelPerDag)[2] <- "aantal"
#combineren to 1 dataset
library(dplyr)
anti_join(koffieRoelPerDag, KNMI, by= "datum")  #check, 0 rijen?
koffieWeer<-left_join(koffieRoelPerDag, KNMI, by = "datum") #left want ik hoef geen weer waar ik geen koffie dronk

#plotten
library(ggplot2)
qplot(datum, aantal, data = koffieWeer)+geom_line()
plot(density( koffieWeer$aantal, bw=10))
qplot(datum, etmaalGemiddeldeTempGradenCelsius, data= koffieWeer, color = aantal, size =4, geom = c("point","smooth"))
qplot(datum, EtmaalNeerslagMM, data= koffieWeer, color = aantal, size =3)
qplot(factor(aantal), etmaalGemiddeldeTempGradenCelsius, data = koffieWeer)+ geom_boxplot()
plot <- ggplot(koffieWeer, aes(x = datum, y= etmaalGemiddeldeTempGradenCelsius, color = aantal))
plot = plot +  geom_point() + geom_smooth()
plot

#koffie en zon
viool <- ggplot(koffieWeer, aes(factor(aantal), zonneschijnUren))
viool+ geom_violin() + ggtitle("zon per koffieaantal") + xlab("aantal koppen koffie") +ylab("aantal uren zon op de dag")

#luchtdruk
qplot(gemLuchtdrukHectoPascal, data = koffieWeer)
#luchtdruk per koffie aantal.
g<- ggplot(data = koffieWeer ,aes(datum, gemLuchtdrukHectoPascal, aantal))
g + geom_point() + facet_wrap(~ aantal) 

#temperatuur
h<- ggplot(data = koffieWeer ,aes(datum, etmaalGemiddeldeTempGradenCelsius, aantal))
h + geom_point() + facet_wrap(~ aantal) 

#straling
i<- ggplot(data = koffieWeer ,aes(datum, stralingJoulePerVierkanteCm, aantal))
i + geom_point() + facet_wrap(~ aantal) 

#etmaalneerslag
j<- ggplot(data = koffieWeer ,aes(datum, EtmaalNeerslagMM, aantal))
j + geom_point() + facet_wrap(~ aantal) 


#beursdata maakt bestand koffieWeerBeurs
library(readr)
library(dplyr)
beurs<- read_csv("YAHOO-INDEX_AEX.csv") #readr package herkent direct als datum
qplot(Date, Close, data = beurs)#plot van closing AEX. 
koffieWeerBeurs<-left_join(koffieWeer, beurs, by = c("datum" = "Date"))
#namen: datum, aantal, etmaalGemiddeldeTempGradenCelsius, zonneschijnUren
# stralingJoulePerVierkanteCm, EtmaalNeerslagMM, gemLuchtdrukHectoPascal,
# Open, High, Low, Close, Volume, Adjusted Close
library(ggplot2)
#iets met settings van kleuren is raar. bij errors voer uit:
#theme_set(theme_grey())
k<- ggplot(data = koffieWeerBeurs ,aes(datum, Close, aantal))
k + geom_point() + facet_wrap(~ aantal) 

i<- ggplot(data = koffieWeerBeurs ,aes(datum, Close, color = etmaalGemiddeldeTempGradenCelsius))
i + geom_point( size =3) +scale_color_gradient2(high="red") #rood
i + geom_point( size =3) +scale_color_gradientn(colours = rainbow(3)) #hoog contrast regenboog

#plot temperatuur onder koers
library(cowplot)
A<- ggplot(data = koffieWeerBeurs ,aes(datum, Close))+ geom_point()
B<- qplot(datum, etmaalGemiddeldeTempGradenCelsius  ,data = koffieWeerBeurs)
plot_grid(A, B, align = "h", nrow = 2)

C<-qplot(datum, aantal, data=koffieWeerBeurs)
plot_grid(A, C, nrow = 2, align = "h")

cor(koffieWeerBeurs$aantal, koffieWeerBeurs$Close, use = "complete.obs")

library(GGally)
ggpairs(data=koffieWeerBeurs, 
        columns = 2:10,
        upper = list(continuous = "density"),
        lower = list(combo = "facetdensity")
        )

ggscatmat(data = koffieWeerBeurs, columns = 2:10) #eenvoudigste plot
