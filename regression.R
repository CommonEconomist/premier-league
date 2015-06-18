#******************************************************************************
# This version:  18-06-2015
# First version: 25-05-2014
# Modeling match results for the Premier League 1993-2014
#******************************************************************************

setwd("[SET DIRECTORY]")

## Libraries
library(coda)
library(R2jags)
library(rjags)
library(jagstools)
library(ggplot2)
library(mcmcplots)
library(stringr)
library(reshape)
library(plyr)

## Load data
df<-read.csv("premier_league.csv",header=TRUE,sep=",",
             row.names=NULL,stringsAsFactors=FALSE)

#### Minor adjustments ####

df$Date<-as.Date(df$Date,"%Y-%m-%d")
df<-df[order(df$Date),]

## Recode variable for MatchResult: -1 = Away win, 0= draw, 1 = Home win
df$MatchResult<-sign(df$HomeGoals-df$AwayGoals)

df$HomeTeam<-as.character(df$HomeTeam)
df$AwayTeam<-as.character(df$AwayTeam)

#### Figure: Premier League participation ####
qplot(Season,HomeTeam,data=df,ylab="",xlab = "",group=HomeTeam)+
  theme_classic()+geom_line(size=3)+geom_point(size=3)

#### Figure: Distribution of goals

hist(c(df$AwayGoals,df$HomeGoals),xlim=c(-0.5,10),breaks=-1:9+0.5,
     main = "Distribution of goals scored in the Premier League",
     ylab="",xlab="",las=1,tck=.02)
lines(seq(0,10),8524*dpois(seq(0,10),mean(c(df$AwayGoals,df$HomeGoals))),
      col="firebrick3",lwd=3)

#### Model estimation ####
d<-na.omit(df) # Not needed with latest data

## Create indices
teams<-sort(unique(c(d$HomeTeam,d$AwayTeam)))
seasons<-unique(d$Season)
Season=as.numeric(factor(d$Season,levels=seasons))

HomeGoals=d$HomeGoals
AwayGoals=d$AwayGoals
HomeTeam = as.numeric(factor(d$HomeTeam,levels = teams))
AwayTeam=as.numeric(factor(d$AwayTeam,levels=teams))

n_teams=length(teams)
n_games=nrow(d) 
n_seasons=length(seasons)

## Fitting the data
data_list<-list(HomeGoals=HomeGoals,AwayGoals=AwayGoals,
                HomeTeam=HomeTeam,AwayTeam=AwayTeam,Season=Season,
                n_teams=n_teams,n_games=n_games,n_seasons=n_seasons)
parameters<-c("home_baseline","away_baseline","skill",
              "season_sigma","group_sigma","group_skill")                  
model.file <- "poisson.txt"

# Run regression
system.time(m1<- jags(data=data_list,inits=NULL,parameters.to.save=parameters,
        model.file=model.file,n.chains=4,n.iter=10000,n.burnin=2500,n.thin=2))

# Process results
lm1<-as.mcmc(m1)
mm1<-as.matrix(lm1)


#### Rank teams 2014/15 season ####

team_skill<-mm1[,str_detect(string=colnames(mm1),"skill\\[22,")]
team_skill<-(team_skill-rowMeans(team_skill))+mm1[, "home_baseline[22]"]
team_skill<-exp(team_skill)
colnames(team_skill)<-teams

# Teams that actually played this season
last.season<-as.vector(unique(df[df$Season=="2014/15",]$HomeTeam))

team_skill<-team_skill[,last.season]
team_skill<-team_skill[,order(colMeans(team_skill),decreasing = T)]

## Plot ranking
par(mar=c(3,1,2,1))
caterplot(team_skill,labels.loc="above",val.lim = c(0.7, 3.8),
          style="plain",bty="n",tck=.02)
# Leaves something to be desired

#### In-sample predictions ####

col_name <- function(name, ...) {
    paste0(name, "[", paste(..., sep = ","), "]")
}

n<-nrow(mm1)
m_pred <- sapply(1:nrow(df), function(i) {
  home_team <- which(teams == df$HomeTeam[i])
  away_team <- which(teams == df$AwayTeam[i])
  season <- which(seasons == df$Season[i])
  print(season)
  print(home_team)
  print(away_team)
  home_skill <- mm1[, col_name("skill", season, home_team)]
  away_skill <- mm1[, col_name("skill", season, away_team)]
  home_baseline <- mm1[, col_name("home_baseline", season)]
  away_baseline <- mm1[, col_name("away_baseline", season)]

  home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))
  home_goals_table <- table(home_goals)
  away_goals_table <- table(away_goals)
  match_results <- sign(home_goals - away_goals)
  match_results_table <- table(match_results)

  mode_home_goal <- as.numeric(names(home_goals_table)[ which.max(home_goals_table)])
  mode_away_goal <- as.numeric(names(away_goals_table)[ which.max(away_goals_table)])
  match_result <-  as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(home_goals), 1)

  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(home_goals), mean_away_goal = mean(away_goals),
    rand_home_goal = home_goals[rand_i], rand_away_goal = away_goals[rand_i],
    rand_match_result = match_results[rand_i])
})

m_pred<-t(m_pred) # Transpose

## Check the accuracy of the predictions

# Number of scored goals
mean(df$HomeGoals == m_pred[, "mode_home_goal"],na.rm=TRUE) # 34 % correct
mean(df$AwayGoals == m_pred[, "mode_away_goal"],na.rm=TRUE) # 39 % correct

mean((df$HomeGoals-m_pred[, "mean_home_goal"])^2,na.rm=TRUE) # MSE: 1.48
mean((df$AwayGoals-m_pred[, "mean_away_goal"])^2,na.rm=TRUE) # MSE: 1.11

# The match results
mean(df$MatchResult == m_pred[, "match_result"],na.rm=TRUE) # 53%, a bit low


