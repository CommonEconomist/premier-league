# Modeling match results in the Premier League

This is an attempt at predicting the scores for the Premier League. The model used is based on the work by Rasmus Bååth  who did something similar for La Liga in Spain, which can be found [here](http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-one/), [here](http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-two/), and [here](http://www.sumsar.net/blog/2013/08/modeling-match-results-in-la-liga-part-three/).
At the moment there are some issues with actually predicting the results for the coming season (2014/15). Time permitting I will update the results shortly. 


### Housekeeping

We start with some housekeeping which means some minor adjustments to the data.
The raw data was taken from [this website](http://www.football-data.co.uk/data.php). 
Due to some discrepancies adjustments where made in a spreadsheet program to create csv-files with an identical structure. 
The data used in this script and available in the repository is thus what Hadley would call [tidy data](http://vita.had.co.nz/papers/tidy-data.pdf).
The data covers all Premier League seasons from 1993/94 onwards. 

```R
## Libraries
library(coda)
library(R2jags)
library(rjags)
library(jagstools)
library(ggplot2)
library(mcmcplots)
library(stringr)

## Load the data
load("Premier_League.Rdata")

# Adjust variable for the date
PLeague$Date<-as.Date(PLeague$Date,"%d/%m/%Y")
PLeague<-PLeague[order(PLeague$Date),]

## If not done: Recode variable for MatchResult: -1 = Away win, 0= draw, 1 = Home win
PLeague$MatchResult<-sign(PLeague$HomeGoals-PLeague$AwayGoals)
```

### Teams in the Premier League

A quick and simple plot of all the teams that have played in the Premier League since the 1993/94 season. 

```R
## Plot of all teams that played in the Premier League
qplot(Season, HomeTeam, data=PLeague, 
ylab="Team", xlab = "Particicipation by Season")
```
![](https://raw.githubusercontent.com/CommonEconomist/Premier_League/master/participation.png)

As the plot shows only a handful of teams have never been relegated: 
* Arsenal
* Aston Villa
* Chelsea
* Everton
* Liverpool
* Manchester United
* Tottenham

## Prepare data for analysis

```R 
## Create data frame omitting coming season's fixtures
d<-na.omit(PLeague)
cur<-PLeague[-1:-8144,]

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
```
### Estimating the model

The match results are going to be modeled using a Poisson regression. 
As the figure below shows the scored goals follow roughly a Poisson process.

```R
## Plot distribution of goals scored
hist(c(AwayGoals,HomeGoals),xlim=c(-0.5,9),breaks=-1:9+0.5,
     main = "Distribution of goals scored by a team in a Premier League match.",
     ylab="",xlab="",col="steelblue4)"
```
![](https://raw.githubusercontent.com/CommonEconomist/Premier_League/master/goals.png)

The model is defined as:


```R
# Poisson model
# Predicting and ranking Premier League football teams

model {
for(i in 1:n_games) {
  HomeGoals[i] ~ dpois(lambda_home[Season[i], HomeTeam[i],AwayTeam[i]])
  AwayGoals[i] ~ dpois(lambda_away[Season[i], HomeTeam[i],AwayTeam[i]])
}

for(season_i in 1:n_seasons) {
  for(home_i in 1:n_teams) {
    for(away_i in 1:n_teams) {
      lambda_home[season_i, home_i, away_i] <- exp( home_baseline[season_i] + skill[season_i, home_i] - skill[season_i, away_i])
      lambda_away[season_i, home_i, away_i] <- exp( away_baseline[season_i] + skill[season_i, away_i] - skill[season_i, home_i])
    }
  }
}

skill[1, 1] <- 0 
for(j in 2:n_teams) {
  skill[1, j] ~ dnorm(group_skill, group_tau)
}

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)

home_baseline[1] ~ dnorm(0, 0.0625)
away_baseline[1] ~ dnorm(0, 0.0625)

for(season_i in 2:n_seasons) {
  skill[season_i, 1] <- 0 
  for(j in 2:n_teams) {
    skill[season_i, j] ~ dnorm(skill[season_i - 1, j], season_tau)
  }
  home_baseline[season_i] ~ dnorm(home_baseline[season_i - 1], season_tau)
  away_baseline[season_i] ~ dnorm(away_baseline[season_i - 1], season_tau)
}

season_tau <- 1/pow(season_sigma, 2) 
season_sigma ~ dunif(0, 3) 
}
```

Estimating the model took about 55 minutes on my computer (Intel Core i3 CPU M 380 @ 2.53GHz × 4).

```R
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
```
### Ranking the teams for the 2013/14 season

I use the results to rank the teams on their skill. 
In general the model does a pretty decent job although Manchester United is overvalued based on their actual ranking (7th).

```R
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
```
![](https://raw.githubusercontent.com/CommonEconomist/Premier_League/master/ranking.png)

### In-sample predictions

This section is still under development as I am having some troubles with generating the predictions for the coming season. 
But we can still assess the performance of the model based on the in-sample prediction:


```R
## In-sample predictions

col_name <- function(name, ...) {
    paste0(name, "[", paste(..., sep = ","), "]")
}

n <- nrow(mm1)
m_pred <- sapply(1:nrow(d), function(i) {
  home_team <- which(teams == d$HomeTeam[i])
  away_team <- which(teams == d$AwayTeam[i])
  season <- which(seasons == d$Season[i])
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
```
I check the accurcay by looking at the predicted number of goals versus the actual number and by checking the predicted match result. 

```R
# Number of scored goals
mean(d$HomeGoals == m_pred[, "mode_home_goal"]) # 34 % correct
mean(d$AwayGoals == m_pred[, "mode_away_goal"]) # 39 % correct

mean((d$HomeGoals-m_pred[, "mean_home_goal"])^2) # MSE: 1.49
mean((d$AwayGoals-m_pred[, "mean_away_goal"])^2) # MSE: 1.11
```
The prediction accuracy is not very high but it is acceptable. 

```R
# The match results
mean(d$MatchResult == m_pred[, "match_result"]) # 53%
```
The correct number of predicted match results is only 53% which is fairly low, essentially as good as your average television pundit. 

## Update (15-08-2014):

Just as a small exercise a prediction for the first game of the 2014/15 Premier League: Manchester United vs. Swansae City.

Prediction is based on the skill of each team during the previous season:

```R
home_team <- which(teams == "Man United")
away_team <- which(teams == "Swansea")
season <- which(seasons == "2013/14")
home_skill <- mm1[, col_name("skill", season, home_team)]
away_skill <- mm1[, col_name("skill", season, away_team)]
home_baseline <- mm1[, col_name("home_baseline", season)]
away_baseline <- mm1[, col_name("away_baseline", season)]

home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))

1/c("Man United" = mean(home_goals > away_goals), Draw = mean(home_goals == away_goals),
    Swansea = mean(home_goals < away_goals))
```
The model gives the following odds:


```R
Man United       Draw    Swansea 
  1.431981   5.309735   8.823529 
```

**Result**: Manchester United 1 Swansea City 2
