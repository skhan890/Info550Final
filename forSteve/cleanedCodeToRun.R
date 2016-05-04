# setwd("C:/Users/Sars/Documents/Spring 2016/Software Engineering/Project/BattleAPI")
library(jsonlite)
library(dplyr)
library(plyr)
library(rCharts)
library(tidyr)
library(splitstackshape)
library(printr)

## Three datasets that I received from the API (loading them to save my API calls)
load("data/top1000.RData")
load("data/heroItems.RData")
load("data/itemData.RData")

##########################
# RESEARCH QUESTION 1
############################
############################
# To get Chart "r1"
##########################
# Run this:
############################
results <- plyr::ldply(1:1000, function(i) {
  
  tempHero<-(top1000$row$player[[i]])$data[[1]] %>%
    filter(id %in% c("HeroBattleTag","HeroClass","HeroId"))  %>%
    select(string,number)
  
  (c(i,tempHero[1,1],tempHero[3,2],tempHero[2,1]))
})

colnames(results)<-c("Rank","HeroBattleTag","HeroId","HeroClass")


top1000Stats<- lapply(1:1000, function(i) {
  as.character(t(as.data.frame(heroItems[[i]]["stats"])))
})

heroStats<-do.call(rbind.data.frame, top1000Stats) # coercing the character into a data frame
heroStats<-cbind(heroStats,1:1000,results$HeroClass) #adding the rank (1:1000) and the class of the hero
colnames(heroStats)<-c(names(heroItems[[1]]["stats"]$stats),"Rank","HeroClass") #

r1 <- rPlot(damage ~ Rank, data = heroStats, type = 'point', color = 'HeroClass')
r1$addControls('x', 'rank', names(heroStats))
r1

##########################
# RESEARCH QUESTION 2
############################
############################
# To get charts sets100Plot and sets1000Plot
##########################
##########################
# Run this:
############################

setItems<- lapply(1:1000, function(i) {
  (heroItems[[i]]["items"][[1]][1][[1]]["setItemsEquipped"][[1]])
})

uniqueSetItems <- plyr::ldply(1:1000, function(i) {
  return(c(i,  # Rank of player
           results$HeroClass[i],   # class of player
           paste(setItems[[i]], sep=" ", collapse=",") # Set Item: squish the set items into one vector
  ) 
  )
})

# setting the column names to the data frame
colnames(uniqueSetItems)<-c("rank","class","uniqueSet")

############################

uniqueItems<-as.matrix(unique(unlist(strsplit(uniqueSetItems$uniqueSet,","))))

# getting rid of any NA values
uniqueItems<-uniqueItems[!uniqueItems[,1] == "NA", ]



betterUniqueSetNames<-cSplit(uniqueSetItems, "uniqueSet", sep=",")
betterUniqueSetNames<-betterUniqueSetNames %>% gather(clothing,itemID,uniqueSet_1:uniqueSet_7)

itemsWithSet<- plyr::ldply(1:144, function(i) {
c(itemData[[i]]$id,itemData[[i]]$name,(itemData[[i]]$set$name))
})
colnames(itemsWithSet)<-c("itemID","itemName","setName")



heroSetDataFinal<-inner_join(betterUniqueSetNames,itemsWithSet, by=c("itemID"))


setClean<-function(x){
  as.data.frame(xtabs(~ setName + class , x))[!as.data.frame(xtabs(~ setName + class , x))$Freq==0, ]
}

classSets1000<-setClean(heroSetDataFinal) #all 1000 top players
classSets100<-setClean(filter(heroSetDataFinal, (rank%in%(1:100)))) #only the top 100 ranked players

sets1000Plot <- nPlot(
  Freq ~ class, 
  group = "setName",
  data = classSets1000,
  type= "multiBarChart"
)

sets100Plot <- nPlot(
  Freq ~ class, 
  group = "setName",
  data = classSets100,
  type= "multiBarChart"
)

sets1000Plot
sets100Plot
###################################################################################################### 
