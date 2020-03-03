#Eva Najera
#Project 1
#Due Nov 28 2018 1159p

#1. Option 1 : Create an animated graph similar to the Hans Rosling graph shown in class using
#data from kaggle at this URL
#https://www.kaggle.com/gemartin/world-bank-data-1960-to-2016. Let x be life expectancy
#and y be fertility rate. Include features in your graphic to represent population and continent
#of each country. You will need to use information from the internet to get the continent of
#each country. The Hans Rosling animated graph is here: https://www.youtube.com/watch?
#  v=jbkSRLYSojo

#Package installation
install.packages("readr")
install.packages("rvest")
install.packages("ggplot2")
install.packages("magick")
install.packages("stringr")

#Webscraping Code
library(rvest)
library(stringr)
url = "https://www.worldatlas.com/cntycont.htm"
page <-read_html(url)
mynodes <- html_nodes(page,'.miscTxt')
nodes<-page%>%
        html_nodes("ul")%>% html_text()
Africa<-nodes[5]
Asia<-nodes[6]
Europe <-nodes[7]
N.America<-nodes[8]
Oceania<-nodes[9]
S.America<-nodes[10]

#Function to assign continent names based on scraped data
getCont<- function(country){
  if (str_detect(Asia, country)){name <- "Asia"}
  else if (str_detect(Africa, country)){name <- "Africa"}
  else if (str_detect(Oceania, country)){name <- "Oceania"}
  else if (str_detect(Europe, country)){name <- "Europe"}
  else if (str_detect(N.America, country)){name <- "NAmerica"}
  else if (str_detect(S.America, country)){name <- "SAmerica"}
  else { name <- "unknown"}
  return (name)
}

#Reading in data from files
library(readr)
life_expectancy <- read_csv("life_expectancy.csv")
fertility_rate <- read_csv("fertility_rate.csv")
country_population <- read_csv("country_population.xls")



result<-lapply(country_population[[1]], getCont) #applies my function to all countries
continents<-cbind(result) 
names(continents)[1]<-"ContinentName"
names(country_population)[1]<-"CountryName" #changes column name to CountryName
nameData<-cbind(country_population$CountryName, continents) #binds Country names to continents names

#unknowns
#matching some values manually
#[[row,column]]
nameData[[1,2]]<-"SAmerica"
nameData[[6,2]]<-"Africa"
nameData[[10,2]]<-"Oceania"
nameData[[18,2]]<-"Africa"
nameData[[26,2]]<-"NAmerica"
nameData[[30,2]]<-"Asia"
nameData[[35,2]]<-"Europe"
nameData[[40,2]]<-"Africa"
nameData[[42,2]]<-"Africa"
nameData[[43,2]]<-"Africa"
nameData[[46,2]]<-"Africa"
nameData[[48,2]]<-"NAmerica"
nameData[[50,2]]<-"SAmerica"
nameData[[51,2]]<-"NAmerica"
nameData[[72,2]]<-"Europe"
nameData[[77,2]]<-"Europe"
nameData[[78,2]]<-"Oceania"
nameData[[83,2]]<-"Europe"
nameData[[85,2]]<-"Africa"
nameData[[90, 2]]<-"NAmerica"


#Graphing, saving plots
library(ggplot2)
myPngs<-NULL
j=1
length<-ncol(country_population)
for (i in 5:length ){
  b<-cbind(nameData[], country_population[i], life_expectancy[i], fertility_rate[i])
  year <- names(b)[3]
  print (year)
  names(b)[1]<-"CountryName"
  names(b)[2]<-"Continent"
  names(b)[3]<-"Population"
  names(b)[4]<-"LifeExpectancy"
  names(b)[5]<-"FertilityRate"
  b$Continent<-factor(unlist(b$Continent))
  p<-paste(c("p", j, ".png"), collapse="") # concatenates p with integer
  print (p)
  myPngs[j]<-p
  cleaned.up<-subset(b, Continent!="unknown" & (!is.na(LifeExpectancy)) & (!is.na(FertilityRate)))
  pl<-ggplot(data=cleaned.up, aes(y=FertilityRate, x=LifeExpectancy, color=Continent, size=Population)) + geom_point()+theme_bw()+ggtitle(year)
  pl+geom_point()+guides(colour = guide_legend(override.aes = list(size=4)))+scale_color_brewer(type="div", palette="Spectral")
  pl + xlim(20,90)+ ylim(0, 10)
  ggsave(p,height=9,width=12,dpi=72)
  j=j+1
}

#pngs into animated graph
library(magick)
imgs<-image_read(myPngs)#turns png into magick object
img.anim <- image_animate(imgs, fps = 4, dispose = "background")
image_write(img.anim, paste("myanimation.gif"))

image_read("myanimation.gif")







