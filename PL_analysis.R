#==============================================================================
# This version:  07-08-2014
# First version: 25-05-2015
# Modeling match results for the Premier League 1993-2014
# Based on: 
# http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-one/
# http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-two/
# http://www.sumsar.net/blog/2013/08/modeling-match-results-in-la-liga-part-three/
#==============================================================================

## Libraries
library(coda)
library(R2jags)
library(rjags)
library(jagstools)
library(ggplot2)
library(mcmcplots)
library(stringr)

setwd("~/Dropbox/Glasnost/Premier_League")

## Load the data
load("Premier_League.Rdata")

# Adjust variable for the date
PLeague$Date<-as.Date(PLeague$Date,"%d/%m/%Y")
PLeague<-PLeague[order(PLeague$Date),]

## Plot of all teams that played in the Premier League
qplot(Season, HomeTeam, data=PLeague, ylab="Team", xlab = "Particicipation by Season")

## If not done: Recode variable for MatchResult:
# -1 = Away win, 0= draw, 1 = Home win
PLeague$MatchResult<-sign(PLeague$HomeGoals-PLeague$AwayGoals)

## Create data frame omitting coming season's fixtures
d<-na.omit(PLeague)

## Model indices
teams<-sort(unique(c(d$HomeTeam,d$AwayTeam)))
seasons<-unique(d$Season)
Season=as.numeric(factor(d$Season,levels=seasons))

## Model variables
HomeGoals=d$HomeGoals
AwayGoals=d$AwayGoals
HomeTeam=as.numeric(factor(d$HomeTeam,levels=teams))
AwayTeam=as.numeric(factor(d$AwayTeam,levels=teams))

## Some more indices
n_teams=length(teams)
n_games=nrow(d) 
n_seasons=length(seasons)

## Plot distribution of goals scored
hist(c(AwayGoals,HomeGoals),xlim=c(-0.5,9),breaks=-1:9+0.5,
     main = "Distribution of goals scored by a team in a Premier League match.",
     ylab="",xlab="",col="steelblue4")


## Model estimation

data_list<-list(HomeGoals=HomeGoals,AwayGoals=AwayGoals,HomeTeam=HomeTeam,
                  AwayTeam=AwayTeam,Season=Season,n_teams=n_teams,n_games=n_games,
                  n_seasons=n_seasons)
parameters<-c("home_baseline","away_baseline","skill","season_sigma","group_sigma",
              "group_skill")                  
model.file <- "poisson.txt"

# Estimate the model (55m13s)
system.time(m1<- jags(data=data_list,inits=NULL,parameters.to.save=parameters,
        model.file=model.file,
        n.chains=4,n.iter=10000,n.burnin=2500,n.thin=5))

# Process results
lm1<-as.mcmc(m1)
mm1<-as.matrix(lm1)

## Ranking of teams for the 2013-2014 season

team_skill<-mm1[,str_detect(string=colnames(mm1),"skill\\[21,")]
team_skill<-(team_skill-rowMeans(team_skill))+mm1[, "home_baseline[21]"]
team_skill<-exp(team_skill)
colnames(team_skill)<-teams
team_skill<-team_skill[,c("Arsenal","Aston Villa","Cardiff","Chelsea","Crystal Palace",
            "Everton","Fulham","Hull","Liverpool","Man City",
            "Man United","Newcastle","Norwich","Southampton",
            "Stoke","Sunderland","Swansea","Tottenham",
            "West Brom","West Ham")]
team_skill<-team_skill[,order(colMeans(team_skill),decreasing = T)]
par(mar=c(3,1,2,1))
caterplot(team_skill,labels.loc="above",val.lim = c(0.7, 3.8))

## Predictions

col_name <- function(name, ...) {
    paste0(name, "[", paste(..., sep = ","), "]")
}

n <- nrow(mm1)
m_pred <- sapply(1:nrow(PLeague), function(i) {
  home_team <- which(teams == PLeague$HomeTeam[i])
  away_team <- which(teams == PLeague$AwayTeam[i])
  season <- which(seasons == PLeague$Season[i])
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




