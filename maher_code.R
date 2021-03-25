# Necessary packages
library(readxl) 
install.packages("openxlsx")  
library(openxlsx) 
install.packages("car") 
library(car)  
library(dplyr)  
install.packages("tidyverse") 
library(tidyverse) 
library(caret)
library(zoo)
library(data.table)
install.packages("numDeriv") 
library(numDeriv) 
install.packages("alabama") 
library(alabama)
install.packages("gdata")
library(gdata)
install.packages("e1071")
library(e1071)



s1819<- read.csv("prem1819.csv",header=TRUE)  
s1718<- read.csv("prem1718.csv",header=TRUE)  
s1617<- read.csv("prem1617.csv",header=TRUE)  
s1516<- read.csv("prem1516.csv",header=TRUE)  
s1415<- read.csv("prem1415.csv",header= TRUE) 
s1314<- read.csv("prem1314.csv",header= TRUE) 

s1718 <- s1718[,-c(11)]
s1617 <- s1617[,-c(11)]
s1516 <- s1516[,-c(11)]
s1415 <- s1415[,-c(11)]
s1314 <- s1314[,-c(11)]

s1314$Date <- strptime(as.character(s1314$Date), "%d/%m/%Y")
s1415$Date <- strptime(as.character(s1415$Date), "%d/%m/%Y")
s1516$Date <- strptime(as.character(s1516$Date), "%d/%m/%Y")
s1617$Date <- strptime(as.character(s1617$Date), "%d/%m/%Y")
s1718$Date <- strptime(as.character(s1718$Date), "%d/%m/%Y")
s1819$Date <- strptime(as.character(s1819$Date), "%d/%m/%Y")
s1314$Date <- as.Date(s1314$Date)
s1415$Date <- as.Date(s1415$Date)
s1516$Date <- as.Date(s1516$Date)
s1617$Date <- as.Date(s1617$Date)
s1718$Date <- as.Date(s1718$Date)
s1819$Date <- as.Date(s1819$Date)
# other leagues
sp1819 <- read.csv("SP1819.csv", header=TRUE)

i1819 <- read.csv("I1819.csv", header=TRUE)

sp1819$Date <- strptime(as.character(sp1819$Date), "%d/%m/%Y")
i1819$Date <- strptime(as.character(i1819$Date), "%d/%m/%Y")

sp1819reduced <- select(sp1819, -c(8:61))

i1819reduced <- select(i1819, -c(8:61))

#Removing variables after column 23 to reduce the dataset 
s1819reduced<-select(s1819,-c(23:64))  
s1718reduced<-select(s1718,-c(23:67))  
s1617reduced<-select(s1617,-c(23:67))  
s1516reduced<-select(s1516,-c(23:67))  
s1415reduced<-select(s1415,-c(23:70)) 
s1314reduced<-select(s1314,-c(23:70))  

s1819reduced <- s1819reduced %>% mutate(
  Season = 1819
)
s1718reduced <- s1718reduced %>% mutate(
  Season = 1718
)
s1617reduced <- s1617reduced %>% mutate(
  Season = 1617
)
s1516reduced <- s1516reduced %>% mutate(
  Season = 1516
)
s1415reduced <- s1415reduced %>% mutate(
  Season = 1415
)
s1314reduced <- s1314reduced %>% mutate(
  Season = 1314
)

mastersample<-rbind(s1314reduced,s1415reduced,s1516reduced,s1617reduced,s1718reduced,s1819reduced)
mastersample$Season <- as.numeric(mastersample$Season)
mastersample$FTHG <- as.integer(mastersample$FTHG)
mastersample$FTAG <- as.integer(mastersample$FTAG)



#Maher (with home att, home def, away att, away def)

df <- i1819#mastersample %>% filter(Season == 1819) #sp1819
df <- apply(df, 1, function(row){data.frame(date=c(row['Date'],row['Date']),team=c(row['HomeTeam'], row['AwayTeam']), opponent=c(row['AwayTeam'], row['HomeTeam']), goals=c(row['FTHG'], row['FTAG']),home=c(1, 0),conceded = c(row['FTAG'],row['FTHG']))})
df <- do.call(rbind, df)
df$date <- as.Date(df$date)
df$goals <- as.integer(paste(df$goals))
df$conceded <- as.integer(paste(df$conceded))
# Sample is all but last 6 weeks of 1819 season, then use to predict matches in second half
dftrain <- head(df,-71)#df %>% filter(date<"2019-03-30") #head(df,-80)
# last 6 weeks as testing data
dftest <- tail(df,71)#df %>% filter(date>="2019-03-30") #tail(df,80)
teams <- as.data.frame(levels(df$team))
teams <- teams %>% mutate(
  homeatt=NA,
  homedef=NA,
  awayatt=NA,
  awaydef=NA,
  att=NA,
  def=NA,
  m2homeatt=NA,
  m2homedef=NA,
  m2awayatt=NA,
  m2awaydef=NA
) #in terms of parameters, homeatt=alpha, homedef=gamma, awayatt=delta, awaydef=beta

#Don't run the line below twice in a row it might get confused!
teams <- teams %>% rename(
  Team = `levels(df$team)`
)

teams$Team <- as.character(paste(teams$Team))
dftrain$team <- as.character(paste(dftrain$team))
dftrain$opponent <- as.character(paste(dftrain$opponent))

homegoals = sum((dftrain %>% filter(home==1))$goals) #in Maher paper, this is Sx, total no.home goals half way through season
awaygoals = sum((dftrain %>% filter(home==0))$goals) #in Maher paper, this is Sy, total no.away goals half way through season

#Model 2 constant k^2 and constraints
k2 <- awaygoals/homegoals
sumalpha = 20
sumbeta = sumalpha

for(i in 1:nrow(teams)){
  #odd rows is when team of interest (team i) is at home, even is away
  team_i <- teams$Team[i]
  #Team of interest's matches as of 32 weeks through 2018/2019 season
  i_games <- dftrain %>% filter(team == team_i)
  i_home <- dftrain %>% filter(team == team_i,home == 1)
  i_away <- dftrain %>% filter(team == team_i,home == 0)

  
  #Team i home attack parameter
  
  teams$m2homeatt[i] <- (sum(i_home$goals)+sum(i_away$goals))/((1+k2)*(homegoals/sumalpha))
  
  #Team i away defence parameter
  
  teams$m2awaydef[i] <-  (sum(i_home$conceded)+sum(i_away$conceded))/((1+k2)*(awaygoals/(sumbeta*k2)))
  
  #Team i home defence parameter

  teams$m2homedef[i] <- sqrt(k2)*teams$m2awaydef[i]
  
  #Team i away attack parameter
 
  teams$m2awayatt[i] <- sqrt(k2)*teams$m2homeatt[i]
  
}


#home team goals (this is Leicester home vs Man Utd)
dpois(0:5,teams$homeatt[1]*teams$awaydef[2])
#away team goals
dpois(0:5,teams$homedef[1]*teams$awayatt[2])
#Matrix of probs of goals, rows is home team, cols is away team, probs are multiplied together
scores <- dpois(0:5,teams$homeatt[1]*teams$awaydef[2]) %o% dpois(0:5,teams$homedef[1]*teams$awayatt[2])
rownames(scores) <- 0:5 
colnames(scores) <- 0:5 
scores <- as.matrix(scores)
print(scores) 

probhomewin = sum(lowerTriangle(scores))
probdraw = sum(diag(scores))
probawaywin = sum(upperTriangle(scores))

print(probhomewin)
print(probdraw)
print(probawaywin)

# Initialise expected observations of each number of goals for home,away goals, used for Chi-squared
home0 = 0
home1 = 0
home2 = 0
home3 = 0
home4 = 0
home5 = 0
home6 = 0
home7 = 0
home8 = 0
away0 = 0
away1 = 0
away2 = 0
away3 = 0
away4 = 0
away5 = 0
away6 = 0
away7 = 0
away8 = 0




#Doing predictions for all the test data (ie second half of season 18/19)

#dftest <- dftest[odd_indices,]
#Add actual result so that the ability to predict using Maher model can be assessed
dftest <- dftest %>% mutate(
  result=ifelse(goals>conceded,'H',ifelse(goals<conceded,'A','D'))
)

#Add columns for extra info that will be filled later
dftest <- dftest %>% mutate(
  m2homeatt=NA,
  m2homedef=NA,
  m2awayatt=NA,
  m2awaydef=NA,
  probhomewin_m2=NA,
  probdraw_m2=NA,
  probawaywin_m2=NA,
  prediction_m2=NA,
  scorepred_m2=NA,
  predcorrect_m2=NA,
)
#homeatt=NA,
#homedef=NA,
#awayatt=NA,
#awaydef=NA,
#probhomewin_all=NA,
#probdraw_all=NA,
#probawaywin_all=NA,
#prediction_all=NA,
#predcorrect_all=NA,

for(i in 1:nrow(dftest)){
  if(i %% 2 != 0){
  print(i)
  #Home team
  team_a <- dftest$team[i]
  #Away team
  team_b <- dftest$opponent[i]
  #Extract relevant teams attacking, defensive strength
  ratings_a <- teams %>% filter(Team == team_a)
  ratings_b <- teams %>% filter(Team == team_b)
  
  #a_homeatt <- ratings_a$homeatt
  #a_homedef <- ratings_a$homedef
  #b_awayatt <- ratings_b$awayatt
  #b_awaydef <- ratings_b$awaydef
  
  # Model 2
  a_m2homeatt <- ratings_a$m2homeatt
  a_m2homedef <- ratings_a$m2homedef
  b_m2awayatt <- ratings_b$m2awayatt
  b_m2awaydef <- ratings_b$m2awaydef

  # Add info to table of test data
  #dftest$homeatt[i] <- a_homeatt
  #dftest$homedef[i] <- a_homedef
  #dftest$awayatt[i] <- b_awayatt
  #dftest$awaydef[i] <- b_awaydef
  #Model 2
  dftest$m2homeatt[i] <- a_m2homeatt
  dftest$m2homedef[i] <- a_m2homedef
  dftest$m2awayatt[i] <- b_m2awayatt
  dftest$m2awaydef[i] <- b_m2awaydef

  #same for even rows
  #dftest$homeatt[i+1] <- a_homeatt
  #dftest$homedef[i+1] <- a_homedef
  #dftest$awayatt[i+1] <- b_awayatt
  #dftest$awaydef[i+1] <- b_awaydef
  dftest$m2homeatt[i+1] <- a_m2homeatt
  dftest$m2homedef[i+1] <- a_m2homedef
  dftest$m2awayatt[i+1] <- b_m2awayatt
  dftest$m2awaydef[i+1] <- b_m2awaydef
  
  #Calculation of probabilities
  # Model 1
  #scores_all <- dpois(0:8,a_homeatt*b_awaydef) %o% dpois(0:8,a_homedef*b_awayatt)
  #rownames(scores_all) <- 0:8
  #colnames(scores_all) <- 0:8
  #scores_all <- as.matrix(scores_all)
  
  #dftest$probhomewin_all[i] = sum(lowerTriangle(scores_all))
  #dftest$probdraw_all[i] = sum(diag(scores_all))
  #dftest$probawaywin_all[i] = sum(upperTriangle(scores_all))
  #dftest$probhomewin_all[i+1] = sum(lowerTriangle(scores_all))
  #dftest$probdraw_all[i+1] = sum(diag(scores_all))
  #dftest$probawaywin_all[i+1] = sum(upperTriangle(scores_all))
  
  
  #probhomewin = dftest$probhomewin_all[i]
  #probdraw = dftest$probdraw_all[i]
  #probawaywin = dftest$probawaywin_all[i]
  #Prediction and is it correct
  #dftest$prediction_all[i] <- ifelse(probhomewin>probawaywin&&probhomewin>probdraw,'H',ifelse(probawaywin>probhomewin&&probawaywin>probdraw,'A','D'))
  #dftest$predcorrect_all[i] <- ifelse(dftest$prediction_all[i]==dftest$result[i],1,0)
  #dftest$prediction_all[i+1] <- ifelse(probhomewin>probawaywin&&probhomewin>probdraw,'H',ifelse(probawaywin>probhomewin&&probawaywin>probdraw,'A','D'))
  #dftest$predcorrect_all[i+1] <- ifelse(dftest$prediction_all[i]==dftest$result[i],1,0)
  
  # Model 2
  scores_m2 <- dpois(0:8,a_m2homeatt*b_m2awaydef) %o% dpois(0:8,a_m2homedef*b_m2awayatt)
  rownames(scores_m2) <- 0:8 
  colnames(scores_m2) <- 0:8
  scores_m2 <- as.matrix(scores_m2)
  
  dftest$probhomewin_m2[i] = sum(lowerTriangle(scores_m2))
  dftest$probdraw_m2[i] = sum(diag(scores_m2))
  dftest$probawaywin_m2[i] = sum(upperTriangle(scores_m2))
  dftest$probhomewin_m2[i+1] = sum(lowerTriangle(scores_m2))
  dftest$probdraw_m2[i+1] = sum(diag(scores_m2))
  dftest$probawaywin_m2[i+1] = sum(upperTriangle(scores_m2))
  
  
  probhomewin = dftest$probhomewin_m2[i]
  probdraw = dftest$probdraw_m2[i]
  probawaywin = dftest$probawaywin_m2[i]
  #Prediction and is it correct
  dftest$prediction_m2[i] <- ifelse(probhomewin>probawaywin&&probhomewin>probdraw,'H',ifelse(probawaywin>probhomewin&&probawaywin>probdraw,'A','D'))
  dftest$predcorrect_m2[i] <- ifelse(dftest$prediction_m2[i]==dftest$result[i],1,0)
  dftest$prediction_m2[i+1] <- ifelse(probhomewin>probawaywin&&probhomewin>probdraw,'H',ifelse(probawaywin>probhomewin&&probawaywin>probdraw,'A','D'))
  dftest$predcorrect_m2[i+1] <- ifelse(dftest$prediction_m2[i]==dftest$result[i],1,0)
  
  
  # Contents for Chi-squared test
  
  home0 = home0 + sum(scores_m2[1,])
  home1 = home1 + sum(scores_m2[2,])
  home2 = home2 + sum(scores_m2[3,])
  home3 = home3 + sum(scores_m2[4,])
  home4 = home4 + sum(scores_m2[5,])
  home5 = home5 + sum(scores_m2[6,])
  home6 = home6 + sum(scores_m2[7,])
  home7 = home7 + sum(scores_m2[8,])
  home8 = home8 + sum(scores_m2[9,])
  
  away0 = away0 + sum(scores_m2[,1])
  away1 = away1 + sum(scores_m2[,2])
  away2 = away2 + sum(scores_m2[,3])
  away3 = away3 + sum(scores_m2[,4])
  away4 = away4 + sum(scores_m2[,5])
  away5 = away5 + sum(scores_m2[,6])
  away6 = away6 + sum(scores_m2[,7])
  away7 = away7 + sum(scores_m2[,8])
  away8 = away8 + sum(scores_m2[,9])
  
  }
}
#Percentage accuracy
#percentcorrectmaher_all <- sum(dftest$predcorrect_all)/nrow(dftest)
accuracy_maher <- sum(dftest$predcorrect_m2)/nrow(dftest)
# Output to include in results section
odd_index <- seq(1,151,2)
dftesthalf <- dftest[odd_index,]

# Goodness-of-fit 
#######################

# required since expected frequencies lower than 5 when adding home4, home5,...,home8, similar for away goals but add from 2 needed here
homege3 = home3 + home4 + home5 + home6 + home7 + home8
awayge2 = away2 + away3 + away4 + away5 + away6 + away7 + away8
# Observed frequencies
acthome0 = nrow(dftesthalf %>% filter(goals==0))
acthome1 = nrow(dftesthalf %>% filter(goals==1))
acthome2 = nrow(dftesthalf %>% filter(goals==2))
acthomege3 = nrow(dftesthalf %>% filter(goals>2))

actaway0 = nrow(dftesthalf %>% filter(conceded==0))
actaway1 = nrow(dftesthalf %>% filter(conceded==1))
actawayge2 = nrow(dftesthalf %>% filter(conceded>1))

# Collect obs, expected frequencies in separate vectors for home, away
# Home
mah.h.obs <- c(acthome0,acthome1,acthome2,acthomege3)
print(mah.h.obs)
mah.h.exp <- c(home0,home1,home2,homege3)
print(mah.h.exp)
# Away
mah.a.obs <- c(actaway0,actaway1,actawayge2)
print(mah.a.obs)
mah.a.exp <- c(away0,away1,awayge2)
print(mah.a.exp)
# Test statistics
#Home
chisq.test.stat.h.mah <- sum((mah.h.obs-mah.h.exp)^2 /mah.h.exp)
# degrees of freedom
deg.free.h.mah <- length(mah.h.obs)-2
# NOTE LOWER TAIL IS FALSE
p.val.h.mah <- pchisq(chisq.test.stat.h.mah, deg.free.h.mah, lower.tail = FALSE)
#p-value for home goals
print(p.val.h.mah)
#Away
chisq.test.stat.a.mah <- sum((mah.a.obs-mah.a.exp)^2 /mah.a.exp)
# degrees of freedom
deg.free.a.mah <- length(mah.a.obs)-2
p.val.a.mah <- pchisq(chisq.test.stat.a.mah, deg.free.a.mah, lower.tail = FALSE)
#p-value for away goals 
print(p.val.a.mah)





################END OF CHI-SQUARED TEST









# Plots- for Brighton vs Southampton - the first game in test set for full model
plotrange <- 0:8 
predictHome_all <- dftesthalf$homeatt[1]*dftesthalf$awaydef[1]
predictAway_all <- dftesthalf$awayatt[1]*dftesthalf$homedef[1]
hp <- dpois(plotrange, predictHome_all) 
ap <- dpois(plotrange, predictAway_all) 
plot(plotrange, hp, type="b", ylim=range(hp, ap), main="Goals, Brighton vs Southampton for full Maher model", xlab="Number of goals", ylab="Probability") 
points(plotrange, ap, type="b", pch=24) 
legend(x=4, y=0.4, legend=c("Southampton", "Brighton"), pch=c(21, 24)) 

# Plots- for Brighton vs Southampton - the first game in test set for second model
plotrange <- 0:8 
predictHome_m2 <- dftesthalf$m2homeatt[1]*dftesthalf$m2awaydef[1]
predictAway_m2 <- dftesthalf$m2awayatt[1]*dftesthalf$m2homedef[1]
hpm2 <- dpois(plotrange, predictHome_m2) 
apm2 <- dpois(plotrange, predictAway_m2) 
plot(plotrange, hpm2, type="b", ylim=range(hpm2, apm2), main="Goals, Brighton vs Southampton for Maher model", xlab="Number of goals", ylab="Probability") 
points(plotrange, apm2, type="b", pch=24) 
legend(x=4, y=0.4, legend=c("Southampton", "Brighton"), pch=c(21, 24)) 

# Plot of goal difference, full model
scores_all <- dpois(0:8,predictHome_all) %o% dpois(0:8,predictAway_all)
rownames(scores_all) <- 0:8
colnames(scores_all) <- 0:8
scores_all <- as.matrix(scores_all)
goalDiff <- -8:8
goalDiff <- as.data.frame(goalDiff)
goalDiff <- goalDiff %>% mutate(
  Prob = NA
)
for (i in 1:17){
  goalDiff$Prob[i] <- sum(scores_all[row(scores_all)==col(scores_all)+(i-9)])
}

# Plot of goal difference, second simpler model
scores_m2 <- dpois(0:8,predictHome_m2) %o% dpois(0:8,predictAway_m2)
rownames(scores_m2) <- 0:8
colnames(scores_m2) <- 0:8
scores_m2 <- as.matrix(scores_m2)
goalDiff <- goalDiff %>% mutate(
  Prob_m2 = NA
)
for (i in 1:17){
  goalDiff$Prob_m2[i] <- sum(scores_m2[row(scores_m2)==col(scores_m2)+(i-9)])
}
# The plot with both on

plot(goalDiff$goalDiff,goalDiff$Prob_m2,type="b",main="Goal difference, Brighton vs Southampton for Maher model",xlab="Goal difference",ylab="Probability")

#legend(x=0,y=5,legend=c("Full Maher model","Simpler Maher Model"),pch=c(21, 24))

firstteams <- teams %>% filter(Team=='Bournemouth'|Team=='Brighton'|Team=='Burnley'|Team=='Crystal Palace'|Team=='Fulham'|Team=='Huddersfield'|Team=='Leicester'|Team=='Man City'|Team=='Man United'|Team=='Southampton'|Team=='Watford'|Team=='Wolves')

firstteams <- firstteams[order(firstteams$Team),]
firstteams$m2awaydef

# CONFUSION MATRIX PRODUCED IN MAHER RESULTS SECTION OF REPORT
confusionMatrix(as.factor(paste(dftesthalf$prediction_m2)),as.factor(paste(dftesthalf$result)))






