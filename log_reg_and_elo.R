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

#Removing variables that will be ignored
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
mastersample <- mastersample %>% mutate(
  hgd = FTHG - FTAG,
  agd = FTAG - FTHG
)

awayresultall<-ifelse(mastersample$FTR=="H",0,ifelse(mastersample$FTR=="D",0.5,1))
homeresultall <- ifelse(mastersample$FTR=="H",1,ifelse(mastersample$FTR=="D",0.5,0))
masterrededit <- cbind(mastersample,homeresultall,awayresultall)

masterrededit$homeresultall <- as.numeric(paste(masterrededit$homeresultall)) #need paste to it doesnt change 0, 0.5 and 1 to something else
masterrededit$awayresultall <- as.numeric(paste(masterrededit$awayresultall))
year1819 <- masterrededit %>% filter(Season==1819)
dfmas <- apply(year1819, 1, function(row){data.frame(date=c(row['Date'],row['Date']),team=c(row['HomeTeam'], row['AwayTeam']), opponent=c(row['AwayTeam'], row['HomeTeam']), goals=c(row['FTHG'], row['FTAG']), conceded = c(row['FTAG'],row['FTHG']),home=c(1, 0),TS = c(row['HS'],row['AS']),OS = c(row['AS'],row['HS']),TST = c(row['HST'],row['AST']),OST = c(row['AST'],row['HST']),TF = c(row['HF'],row['AF']),OF = c(row['AF'],row['HF']),TC = c(row['HC'],row['AC']),OC = c(row['AC'],row['HC']),result=c(row['homeresultall'],row['awayresultall']), season=c(row['Season'],row['Season']))})
dfmas <- do.call(rbind, dfmas)
dfmas$goals <- as.numeric(paste(dfmas$goals))
dfmas$conceded <- as.numeric(paste(dfmas$conceded))
dfmas$TS <- as.numeric(paste(dfmas$TS))
dfmas$OS <- as.numeric(paste(dfmas$OS))
dfmas$TST <- as.numeric(paste(dfmas$TST))
dfmas$OST <- as.numeric(paste(dfmas$OST))
dfmas$TF <- as.numeric(paste(dfmas$TF))
dfmas$OF <- as.numeric(paste(dfmas$OF))
dfmas$TC <- as.numeric(paste(dfmas$TC))
dfmas$OC <- as.numeric(paste(dfmas$OC))
#dfmas <- dfmas %>% mutate(
#  gd = goals-conceded
#)
#dfmas$season <- as.numeric(paste(dfmas$season))
dfmas_elo <- dfmas %>% mutate(
  ELO = 0,
  opp_ELO = 0
)
#Treating this as the test season
#df1819 <- dfmas_elo %>% 
#  filter(season == 1819)
#This is training
#dfsamp_elo <- dfmas_elo %>% 
# filter(season < 1819)
dfsamp_elo <- dfmas_elo
dfsamp_elo <- dfsamp_elo %>% mutate(
  ELO = 0,
  opp_ELO = 0,
)

# Make table t_elo, which will contain ELO rating of team for reference
t_elo <- levels(masterrededit$HomeTeam)
t_elo <- data.frame(matrix(unlist(t_elo), nrow=length(t_elo), byrow=TRUE))
t_elo <- t_elo %>% mutate(
  ELO=1500
) 
# ensure dates in correct format
dfsamp_elo$date <- as.Date(dfsamp_elo$date)
# name column in t_elo table
dfmas_elo$team <- as.character(dfmas_elo$team) #this is required in order for team_A etc to be defined right
t_elo <- t_elo %>% 
  rename(
    team=matrix.unlist.t_elo...nrow...length.t_elo...byrow...TRUE.
  )
t_elo$team <- as.character(t_elo$team)
dfsamp_elo$result <- as.numeric(paste(dfsamp_elo$result))
dfsamp_elo$goals <- as.numeric(paste(dfsamp_elo$goals))
dfsamp_elo$conceded <- as.numeric(paste(dfsamp_elo$conceded))

dfsamp_elo <- dfsamp_elo %>% mutate(
  rollgoalteam = 0,
  rollgoalopp = 0,
  rollconcteam = 0,
  rollconcopp = 0,
  rTS = 0,
  rOS = 0,
  rTST = 0,
  rOST = 0,
  rTF = 0,
  rOF = 0,
  rTC = 0,
  rOC = 0
)
###ELO rating system
for(i in 1:nrow(dfsamp_elo)){
  #execute when i is odd
  if(i %% 2 != 0){
    print(i)
    team_A <- dfsamp_elo$team[i]
    team_B <- dfsamp_elo$team[i+1]
    
    df_team_A <- dfsamp_elo %>% filter(team == team_A)
    df_team_B <- dfsamp_elo %>% filter(team == team_B)
    
    df_team_A$date <- as.Date(df_team_A$date)
    df_team_B$date <- as.Date(df_team_B$date)
    
    df_team_A_date <- df_team_A %>% filter(date <= dfsamp_elo$date[i])
    df_team_B_date <- df_team_B %>% filter(date <= dfsamp_elo$date[i+1])
    #Goals
    df_team_A_goals <- as.vector(df_team_A_date$goals)
    df_team_B_goals <- as.vector(df_team_B_date$goals)
    # Other stats
    df_team_A_TS <- as.vector(df_team_A_date$TS)
    df_team_B_TS <- as.vector(df_team_B_date$TS)
    df_team_A_TST <- as.vector(df_team_A_date$TST)
    df_team_B_TST <- as.vector(df_team_B_date$TST)
    df_team_A_TF <- as.vector(df_team_A_date$TF)
    df_team_B_TF <- as.vector(df_team_B_date$TF)
    df_team_A_TC <- as.vector(df_team_A_date$TC)
    df_team_B_TC <- as.vector(df_team_B_date$TC)
    
    
    
    # Calculations of averages over last 5 matches for each team in the matchup
    rollgoalA <- frollmean(df_team_A_goals,5)
    rollgoalB <- frollmean(df_team_B_goals,5)
    rTSA <- frollmean(df_team_A_TS,5)
    rTSB <- frollmean(df_team_B_TS,5)
    rTSTA <- frollmean(df_team_A_TST,5)
    rTSTB <- frollmean(df_team_B_TST,5)
    rTFA <- frollmean(df_team_A_TF,5)
    rTFB <- frollmean(df_team_B_TF,5)
    rTCA <- frollmean(df_team_A_TC,5)
    rTCB <- frollmean(df_team_B_TC,5)
    
    rollgoalA <- append(rollgoalA,0,after=0)
    rollgoalB <- append(rollgoalB,0,after=0)
    rTSA <- append(rTSA,0,after=0)
    rTSB <- append(rTSB,0,after=0)
    rTSTA <- append(rTSTA,0,after=0)
    rTSTB <- append(rTSTB,0,after=0)
    rTFA <- append(rTFA,0,after=0)
    rTFB <- append(rTFB,0,after=0)
    rTCA <- append(rTCA,0,after=0)
    rTCB <- append(rTCB,0,after=0)
    
    rollgoalA <- head(rollgoalA,-1)
    rollgoalB <- head(rollgoalB,-1)
    rTSA <- head(rTSA,-1)
    rTSB <- head(rTSB,-1)
    rTSTA <- head(rTSTA,-1)
    rTSTB <- head(rTSTB,-1)
    rTFA <- head(rTFA,-1)
    rTFB <- head(rTFB,-1)
    rTCA <- head(rTCA,-1)
    rTCB <- head(rTCB,-1)
    
    #Same with conceded goals
    df_team_A_conc <- as.vector(df_team_A_date$conceded)
    df_team_B_conc <- as.vector(df_team_B_date$conceded)
    
    rollconcA <- frollmean(df_team_A_conc,5)
    rollconcB <- frollmean(df_team_B_conc,5)
    
    rollconcA <- append(rollconcA,0,after=0)
    rollconcB <- append(rollconcB,0,after=0)
    
    rollconcA <- head(rollconcA,-1)
    rollconcB <- head(rollconcB,-1)
    
    # Ensure averages are calculated correctly for first few entries
    
    if(length(rollgoalA)>1){
      rollgoalA[2] <- mean(c(df_team_A_goals[1]))
      rollconcA[2] <- mean(c(df_team_A_conc[1]))
      rTSA[2] <- mean(c(df_team_A_TS[1]))
      rTSTA[2] <- mean(c(df_team_A_TST[1]))
      rTFA[2] <- mean(c(df_team_A_TF[1]))
      rTCA[2] <- mean(c(df_team_A_TC[1]))
    }
    if(length(rollgoalA)>2){
      rollgoalA[3] <- mean(c(df_team_A_goals[1],df_team_A_goals[2]))
      rollconcA[3] <- mean(c(df_team_A_conc[1],df_team_A_conc[2]))
      rTSA[3] <- mean(c(df_team_A_TS[1],df_team_A_TS[2]))
      rTSTA[3] <- mean(c(df_team_A_TST[1],df_team_A_TST[2]))
      rTFA[3] <- mean(c(df_team_A_TF[1],df_team_A_TF[2]))
      rTCA[3] <- mean(c(df_team_A_TC[1],df_team_A_TC[2]))
    }
    if(length(rollgoalA)>3){
      rollgoalA[4] <- mean(c(df_team_A_goals[1],df_team_A_goals[2],df_team_A_goals[3]))
      rollconcA[4] <- mean(c(df_team_A_conc[1],df_team_A_conc[2],df_team_A_conc[3]))
      rTSA[4] <- mean(c(df_team_A_TS[1],df_team_A_TS[2],df_team_A_TS[3]))
      rTSTA[4] <- mean(c(df_team_A_TST[1],df_team_A_TST[2],df_team_A_TST[3]))
      rTFA[4] <- mean(c(df_team_A_TF[1],df_team_A_TF[2],df_team_A_TF[3]))
      rTCA[4] <- mean(c(df_team_A_TC[1],df_team_A_TC[2],df_team_A_TC[3]))
    }
    if(length(rollgoalA)>4){
      rollgoalA[5] <- mean(c(df_team_A_goals[1],df_team_A_goals[2],df_team_A_goals[3],df_team_A_goals[4]))
      rollconcA[5] <- mean(c(df_team_A_conc[1],df_team_A_conc[2],df_team_A_conc[3],df_team_A_conc[4]))
      rTSA[5] <- mean(c(df_team_A_TS[1],df_team_A_TS[2],df_team_A_TS[3],df_team_A_TS[4]))
      rTSTA[5] <- mean(c(df_team_A_TST[1],df_team_A_TST[2],df_team_A_TST[3],df_team_A_TST[4]))
      rTFA[5] <- mean(c(df_team_A_TF[1],df_team_A_TF[2],df_team_A_TF[3],df_team_A_TF[4]))
      rTCA[5] <- mean(c(df_team_A_TC[1],df_team_A_TC[2],df_team_A_TC[3],df_team_A_TC[4]))
    }
    if(length(rollgoalB)>1){
      rollgoalB[2] <- mean(c(df_team_B_goals[1]))
      rollconcB[2] <- mean(c(df_team_B_conc[1]))
      rTSB[2] <- mean(c(df_team_B_TS[1]))
      rTSTB[2] <- mean(c(df_team_B_TST[1]))
      rTFB[2] <- mean(c(df_team_B_TF[1]))
      rTCB[2] <- mean(c(df_team_B_TC[1]))
    }
    if(length(rollgoalB)>2){
      rollgoalB[3] <- mean(c(df_team_B_goals[1],df_team_B_goals[2]))
      rollconcB[3] <- mean(c(df_team_B_conc[1],df_team_B_conc[2]))
      rTSB[3] <- mean(c(df_team_B_TS[1],df_team_B_TS[2]))
      rTSTB[3] <- mean(c(df_team_B_TST[1],df_team_B_TST[2]))
      rTFB[3] <- mean(c(df_team_B_TF[1],df_team_B_TF[2]))
      rTCB[3] <- mean(c(df_team_B_TC[1],df_team_B_TC[2]))
    }
    if(length(rollgoalB)>3){
      rollgoalB[4] <- mean(c(df_team_B_goals[1],df_team_B_goals[2],df_team_B_goals[3]))
      rollconcB[4] <- mean(c(df_team_B_conc[1],df_team_B_conc[2],df_team_B_conc[3]))
      rTSB[4] <- mean(c(df_team_B_TS[1],df_team_B_TS[2],df_team_B_TS[3]))
      rTSTB[4] <- mean(c(df_team_B_TST[1],df_team_B_TST[2],df_team_B_TST[3]))
      rTFB[4] <- mean(c(df_team_B_TF[1],df_team_B_TF[2],df_team_B_TF[3]))
      rTCB[4] <- mean(c(df_team_B_TC[1],df_team_B_TC[2],df_team_B_TC[3]))
    }
    if(length(rollgoalB)>4){
      rollgoalB[5] <- mean(c(df_team_B_goals[1],df_team_B_goals[2],df_team_B_goals[3],df_team_B_goals[4]))
      rollconcB[5] <- mean(c(df_team_B_conc[1],df_team_B_conc[2],df_team_B_conc[3],df_team_B_conc[4]))
      rTSB[5] <- mean(c(df_team_B_TS[1],df_team_B_TS[2],df_team_B_TS[3],df_team_B_TS[4]))
      rTSTB[5] <- mean(c(df_team_B_TST[1],df_team_B_TST[2],df_team_B_TST[3],df_team_B_TST[4]))
      rTFB[5] <- mean(c(df_team_B_TF[1],df_team_B_TF[2],df_team_B_TF[3],df_team_B_TF[4]))
      rTCB[5] <- mean(c(df_team_B_TC[1],df_team_B_TC[2],df_team_B_TC[3],df_team_B_TC[4]))
    }
    #Add relevant goals scored, conceded to dfsamp_elo
    
    #df_team_A$rollgoalmean <- rollgoalA[length(rollgoalA)]
    #df_team_A$rollconcmean <- rollconcA[length(rollconcA)]
    #df_team_B$rollgoalmean <- rollgoalB[length(rollgoalB)]
    #df_team_B$rollconcmean <- rollconcB[length(rollconcB)]
    
    df_team_A_date <- df_team_A %>% filter(date <= dfsamp_elo$date[i])
    df_team_B_date <- df_team_B %>% filter(date <= dfsamp_elo$date[i+1])
    
    
    dfsamp_elo$rollgoalteam[i] <- rollgoalA[length(rollgoalA)] #df_team_A_date$rollgoalmean
    dfsamp_elo$rollgoalteam[i+1] <- rollgoalB[length(rollgoalB)] #df_team_B_date$rollgoalmean
    dfsamp_elo$rTS[i] <- rTSA[length(rTSA)]
    dfsamp_elo$rTS[i+1] <- rTSB[length(rTSB)]
    dfsamp_elo$rTST[i] <- rTSTA[length(rTSTA)]
    dfsamp_elo$rTST[i+1] <- rTSTB[length(rTSTB)]
    dfsamp_elo$rTF[i] <- rTFA[length(rTFA)]
    dfsamp_elo$rTF[i+1] <- rTFB[length(rTFB)]
    dfsamp_elo$rTC[i] <- rTCA[length(rTCA)]
    dfsamp_elo$rTC[i+1] <- rTCB[length(rTCB)]
    
    dfsamp_elo$rollconcteam[i] <- rollconcA[length(rollconcA)] #df_team_A_date$rollconcmean
    dfsamp_elo$rollconcteam[i+1] <- rollconcB[length(rollconcB)] #df_team_B_date$rollconcmean
    
    dfsamp_elo$rollgoalopp[i] <- rollgoalB[length(rollgoalB)] #df_team_B_date$rollgoalmean
    dfsamp_elo$rollgoalopp[i+1] <- rollgoalA[length(rollgoalA)] #df_team_A_date$rollgoalmean
    dfsamp_elo$rOS[i] <- rTSB[length(rTSB)]
    dfsamp_elo$rOS[i+1] <- rTSA[length(rTSA)]
    dfsamp_elo$rOST[i] <- rTSTB[length(rTSTB)]
    dfsamp_elo$rOST[i+1] <- rTSTA[length(rTSTA)]
    dfsamp_elo$rOF[i] <- rTFB[length(rTFB)]
    dfsamp_elo$rOF[i+1] <- rTFA[length(rTFA)]
    dfsamp_elo$rOC[i] <- rTCB[length(rTCB)]
    dfsamp_elo$rOC[i+1] <- rTCA[length(rTCA)]
    
    
    dfsamp_elo$rollconcopp[i] <- rollconcB[length(rollconcB)] #df_team_B_date$rollconcmean
    dfsamp_elo$rollconcopp[i+1] <- rollconcA[length(rollconcA)] #df_team_A_date$rollconcmean
    
    
    # Start of elo calcs
    result_A <- as.numeric(paste(dfsamp_elo$result[i]))
    result_B <- as.numeric(paste(dfsamp_elo$result[i+1]))
    
    ELO_A <- as.numeric(t_elo[t_elo$team == team_A,"ELO"])
    ELO_B <- as.numeric(t_elo[t_elo$team == team_B,"ELO"])
    
    #Put current ELO into dfsamp_elo add relevant goals scored, conceded
    dfsamp_elo$ELO[i] <- ELO_A
    dfsamp_elo$ELO[i+1] <- ELO_B
    dfsamp_elo$opp_ELO[i] <- ELO_B
    dfsamp_elo$opp_ELO[i+1] <- ELO_A
    
    
    
    # update ELO
    r_A <- 10^(ELO_A/400)
    r_B <- 10^(ELO_B/400)
    
    e_A <- r_A/(r_A+r_B)
    e_B <- r_B/(r_A+r_B)
    
    ELO_update_A <- ELO_A + 40*(result_A-e_A)
    ELO_update_B <- ELO_B + 40*(result_B-e_B)
    
    #update team ELOs
    
    t_elo[t_elo$team == team_A,"ELO"] <- ELO_update_A
    t_elo[t_elo$team == team_B,"ELO"] <- ELO_update_B
    
    
    
  }
  
}
#######Predicting team to win that has highest elo 

dfsamp_elo <- dfsamp_elo %>% mutate(
  ELO=as.numeric(ELO),
  opp_ELO = as.numeric(opp_ELO),
  ELO_diff = ELO-opp_ELO,
  ELO_pred = ifelse(ELO>opp_ELO,1,ifelse(ELO<opp_ELO,0,0.5)),
  ELO_correct = ifelse(ELO_pred == result,1,0),
)


#######win prob/linear regression

odd_index <- seq(1,4559,2)
set.seed(100)
dfsamp_elo$result <- as.factor(dfsamp_elo$result)
mastersample <- mastersample %>% mutate(
  ELO_home = (as.data.frame(dfsamp_elo$ELO))[odd_index,],
  ELO_away = (as.data.frame(dfsamp_elo$opp_ELO))[odd_index,],
  ELO_diff = (as.data.frame(dfsamp_elo$ELO_diff))[odd_index,],
)
#TF=team fouls, OF=opp fouls, TC=team corners, OC=opp corners

dfsamp_elo$TS <- as.numeric(paste(dfsamp_elo$TS))
dfsamp_elo$OS <- as.numeric(paste(dfsamp_elo$OS))
dfsamp_elo$TST <- as.numeric(paste(dfsamp_elo$TST))
dfsamp_elo$OST <- as.numeric(paste(dfsamp_elo$OST))
dfsamp_elo$TF <- as.numeric(paste(dfsamp_elo$TF))
dfsamp_elo$OF <- as.numeric(paste(dfsamp_elo$OF))
dfsamp_elo$TC <- as.numeric(paste(dfsamp_elo$TC))
dfsamp_elo$OC <- as.numeric(paste(dfsamp_elo$OC))
##### Now consider modelling, training data, testing data defined
dftest <- dfsamp_elo%>% filter(date>="2019-03-30")
dftrain <- dfsamp_elo%>% filter(date<"2019-03-30")
dftrain$result <- as.factor(paste(dftrain$result))
dftrain$home <- as.factor(paste(dftrain$home))
dftest$home <- as.factor(paste(dftest$home))
# Build models
# Full model
dftrain$home
logreg_m1 <- glm(result~ELO_diff+home+rollgoalteam+rollgoalopp+rollconcteam+rollconcopp+rTS+rOS+rTST+rOST+rTF+rOF+rTC+rOC,family=binomial,data=dftrain %>% filter(result != 0.5)) #warning here
step(logreg_m1,direction="both")
# Stepwise regression final model
logreg_mstep <- glm(result ~ ELO_diff + home  + rTST + rOST, family = binomial, data = dftrain %>% filter(result != 0.5))
# Include these counterparts in another model see if that improves things
#logreg_medit <- glm(result ~ ELO_diff + home + rollgoalteam + rollgoalopp + rTS + rOST, family = binomial, data = dftrain)
#Predictions for each model
pred_logreg_m1 <- predict(logreg_m1,newdata=dftest,type="response")
pred_logreg_m1 <- as.data.frame(pred_logreg_m1)
pred_logreg_mstep <- predict(logreg_mstep,newdata=dftest,type="response")
pred_logreg_mstep <- as.data.frame(pred_logreg_mstep)
team_index = seq(1,nrow(pred_logreg_m1)-1,2)
opp_index = seq(2,nrow(pred_logreg_m1),2)
# Getting predictions in format to put into training table to analyse results of predictions
pred_m1_team <- pred_logreg_m1$pred_logreg_m1[team_index]
pred_m1_opp <- pred_logreg_m1$pred_logreg_m1[opp_index]

pred_mstep_team <- pred_logreg_mstep$pred_logreg_mstep[team_index]
pred_mstep_opp <- pred_logreg_mstep$pred_logreg_mstep[opp_index]
#lr=logistic regression, add probs to table so that result prediction can be done
dftest_pred <- dftest
dftest_pred$lrm1_team <- as.numeric(paste(pred_m1_team))
dftest_pred$lrm1_opp <- as.numeric(paste(pred_m1_opp))
dftest_pred$lrmstep_team <- as.numeric(paste(pred_mstep_team))
dftest_pred$lrmstep_opp <- as.numeric(paste(pred_mstep_opp))

dftest_pred <- dftest_pred %>% mutate(
  lrm1_pred = ifelse(lrm1_team>0.5,1,ifelse(lrm1_team==0.5,0.5,0)),
  lrmstep_pred = ifelse(lrmstep_team>0.5,1,ifelse(lrmstep_team==0.5,0.5,0))
)
dftest_pred <- dftest_pred %>% mutate(
  lrm1_corr = ifelse(lrm1_pred==result,1,0),
  lrmstep_corr = ifelse(lrmstep_pred==result,1,0)
)
# proportion correct for each logistic model
accuracy_lrm1 <- sum(dftest_pred$lrm1_corr)/nrow(dftest_pred)
accuracy_lrmstep <- sum(dftest_pred$lrmstep_corr)/nrow(dftest_pred)


# proportion correct for ELO with test weeks like above
accuracy_elo <- sum(dftest$ELO_correct)/nrow(dftest)




# Table comparing percentage accuracy, needs editing
acctable <- data.frame(percentright_glm1,percentrightELO1819,percentrightgdm1,percentrightgdm2) #note gd and glm produce the same out_elout when same variables considered



dfsamp_elo$result <- as.numeric(paste(dfsamp_elo$result))
pred1819$result <- as.numeric(paste(pred1819$result))




#acctable <- data.frame(percentright_glm1,percentrightELO1819,percentrightdt11819,percentrightgdm1,percentrightgdm2)
# remove match duplication
odd_indices <- seq(1,151,2)
dftesthalf_pred <- dftest_pred[odd_indices,]
install.packages("writexl")
library("writexl")
write_xlsx(dftesthalf_pred,"~/Desktop/Ryear2\\dflogreg.xlsx")

# CONFUSION MATRIX FROM RESULTS SECTION
dftesthalf_pred2 <- dftesthalf_pred %>% filter(result !=0.5)
dftesthalf_pred2 <- dftesthalf_pred2 %>% mutate(
  Pred2full = ifelse(lrm1_pred==1,'H','A'),
  Result2 = ifelse(result==1,'H','A'),
  Pred2step = ifelse(lrmstep_pred==1,'H','A')
)
# Full model
confusionMatrix(as.factor(paste(dftesthalf_pred2$Pred2full)),as.factor(paste(dftesthalf_pred2$Result2)))
# Stepwise regression end model
confusionMatrix(as.factor(paste(dftesthalf_pred2$Pred2step)),as.factor(paste(dftesthalf_pred2$Result2)))




