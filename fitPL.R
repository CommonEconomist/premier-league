#### Predicting Premier League results ####
## Using a Bayesian hierarchical model 
## Based on Baio & Biangiardo (2010)
## http://discovery.ucl.ac.uk/16040/
## Using data from the Premier League
## http://www.football-data.co.uk/data.php

source("load.R")

#### Past season: 2014/15  ####
pl0<-pl[pl$Season=="2014/15",]

## Input data
teams<-sort(unique(c(pl0$HomeTeam,pl0$AwayTeam)))
nteams<-length(unique(c(pl0$HomeTeam,pl0$AwayTeam)))
ngames=nrow(pl0) 

y1=pl0$HomeGoals
y2=pl0$AwayGoals

hometeam=as.numeric(factor(pl0$HomeTeam,levels=teams))
awayteam=as.numeric(factor(pl0$AwayTeam,levels=teams))

## Fit model (this takes about 5 minutes)
season.p<-jags(data=list(ngames=ngames,y1=y1,y2=y2,hometeam=hometeam,
                      awayteam=awayteam,nteams=nteams),inits=NULL,
            parameters.to.save=c("att","def","home"),
            model.file=M,n.chains=4,n.iter=10000,n.burnin=2500)
print(season.p,digits.summary=3)
rankPlot(season.p)
effPlot(season.p)



## Overview average defence and attack effect

#### Current season: 2015/16 ####

## Input data
teams<-sort(unique(c(pl1$HomeTeam,pl1$AwayTeam)))
nteams<-length(unique(c(pl1$HomeTeam,pl1$AwayTeam)))
ngames=nrow(pl1) 

y1=pl1$HomeGoals
y2=pl1$AwayGoals

hometeam=as.numeric(factor(pl1$HomeTeam,levels=teams))
awayteam=as.numeric(factor(pl1$AwayTeam),levels=teams)

## Run regression (this takes about 5 minutes)
season.c<-jags(data=list(ngames=ngames,y1=y1,y2=y2,hometeam=hometeam,
                      awayteam=awayteam,nteams=nteams),inits=NULL,
            parameters.to.save=c("att","def","home"),
            model.file=M,n.chains=4,n.iter=10000,n.burnin=2500)
rankPlot(season.c)

#### Current season, controlling for past season ####

## Input data
teams<-sort(unique(c(pl1$HomeTeam,pl1$AwayTeam,pl0$HomeTeam,pl0$AwayTeam)))
nteams<-length(teams)
ngames=nrow(pl0)*2 

y1=c(pl0$HomeGoals,pl1$HomeGoals)
y2=c(pl0$AwayGoals,pl1$AwayGoals)

hometeam=as.numeric(c(factor(pl0$HomeTeam,levels=teams),factor(pl1$HomeTeam,levels=teams)))
awayteam=as.numeric(c(factor(pl0$AwayTeam,levels=teams),factor(pl1$AwayTeam,levels=teams)))

## Run regression (this takes about 5 minutes)
season.c2<-jags(data=list(ngames=ngames,y1=y1,y2=y2,hometeam=hometeam,
                      awayteam=awayteam,nteams=nteams),inits=NULL,
            parameters.to.save=c("att","def","home"),
            model.file=M,n.chains=4,n.iter=10000,n.burnin=2500)
rankPlot(season.c2)
