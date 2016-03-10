### Soccer data
### Jeremy L Hour
### 10 mars 2016

### Set working directory
setwd("//ulysse/users/JL.HOUR/1A_These/EconoRandom")

rm(list=ls())

### Load packages
library(devtools)
install_github('jalapic/engsoccerdata', username = "jalapic")
library(engsoccerdata)
data(package="engsoccerdata") 
library("ggplot2")

### Calcio data
data(italycalcio)

teams <- unique(italycalcio[,"home"])

# Get average number of goal per team per game wheather you are at home or away
HVgoals <- data.frame(TeamName=sort(teams),
                      hgoal=tapply(italycalcio[,"hgoal"],italycalcio[,"home"],mean),
                      vgoal=tapply(italycalcio[,"vgoal"],italycalcio[,"visitor"],mean),
                      size=tapply(italycalcio[,"hgoal"],italycalcio[,"visitor"],length)+
                            tapply(italycalcio[,"vgoal"],italycalcio[,"visitor"],length)
  )

ggplot(HVgoals, aes(x=hgoal, y=vgoal)) +
  geom_point(shape=1, aes(size = size)) + 
  geom_smooth() +
  scale_x_continuous(name="Home games") +
  scale_y_continuous(name="Away games") +
  ggtitle("Mean number of goals") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position="none")


### Poisson regressions for number of goals
coefcollect <- matrix(,ncol=2,nrow=length(teams))
j <- 0
for(i in teams){
  j <- j+1
  print(i)
  newdata <- rbind(italycalcio[italycalcio$home==i,],
               italycalcio[italycalcio$visitor==i,])
  newdata[,"Home"] <- ifelse(newdata$home==i,1,0)
  newdata[,"Goals"] <- ifelse(newdata$home==i,newdata$hgoal,newdata$vgoal)
  
  Poissonreg <- glm(Goals ~ Home, family="poisson", data=newdata)
  coefcollect[j,] <- coef(Poissonreg)
}

# Simulation
ht <- which(teams=="Lazio Roma")
at <- which(teams=="Juventus")
grange <- 0:5

homeprob <- dpois(grange,lambda=exp(sum(coefcollect[ht,])))
awayprob <- dpois(grange,lambda=exp(sum(coefcollect[at,1])))

# Simulated result
simRes <- homeprob %*% t(awayprob)

# On data
index <- (italycalcio[,"home"]=="Lazio Roma")*(italycalcio[,"visitor"]=="Juventus")
index <- as.logical(index)

D <- matrix(,ncol=6, nrow=6)

for(i in 1:6){
  for(j in 1:6){
    D[i,j] <- sum( (italycalcio[index,"hgoal"]==(i-1)) * (italycalcio[index,"vgoal"]==(j-1)) )
  }
}

HistoricRes <- D/sum(index)
