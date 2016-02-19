#### Libraries, data, and functions for predicting Premier League ranking ####

#### LOAD ####

## Libraries
library(R2jags)
library(rjags)
library(jagstools)
library(mcmcplots)
library(stringr)

## Data
pl<-read.csv("PremierLeague.csv",header=TRUE,             # Historical PL data
             sep=",",row.names=NULL,stringsAsFactors=FALSE)
pl1<-read.csv("PremierLeagueCurrent.csv",header=TRUE,     # Current PL season
             sep=",",row.names=NULL,stringsAsFactors=FALSE)

#### Functions ####

## Regression model 
M<-function() {
  # LIKELIHOOD AND RANDOM EFFECT MODEL FOR THE SCORING PROPENSITY
  for (g in 1:ngames) {
    # Observed number of goals scored by each team
    y1[g] ~ dpois(theta[g,1])
    y2[g] ~ dpois(theta[g,2])
    # Predictive distribution for the number of goals scored
    ynew[g,1] ~ dpois(theta[g,1])
    ynew[g,2] ~ dpois(theta[g,2])
    # Average Scoring intensities (accounting for mixing components)
    log(theta[g,1]) <- home + att[hometeam[g]] + def[awayteam[g]] 
    log(theta[g,2]) <- att[awayteam[g]] + def[hometeam[g]]
  }
  # 1. BASIC MODEL FOR THE HYPERPARAMETERS # prior on the home effect
  home ~ dnorm(0,0.0001)
  
  # Trick to code the "sum-to-zero" constraint 
  for (t in 1:nteams) {
    att.star[t] ~ dnorm(mu.att,tau.att) 
    def.star[t] ~ dnorm(mu.def,tau.def) 
    att[t] <- att.star[t] - mean(att.star[])
    def[t] <- def.star[t] - mean(def.star[]) 
  }
  # priors on the random effects 
  mu.att ~ dnorm(0,0.0001) 
  mu.def ~ dnorm(0,0.0001) 
  tau.att ~ dgamma(.01,.01) 
  tau.def ~ dgamma(.01,.01)
}

## Plot function for estimation results
rankPlot<-function(x){
  ## Plot results from JAGS model fit
  ## Ranking teams based on estimated skills.
  
  # Results to matrix
  mm<-as.mcmc(x)
  mm<-as.matrix(mm)
  
  # Calculate team skill
  attack<-mm[,str_detect(string=colnames(mm),"att")]
  defence<-mm[,str_detect(string=colnames(mm),"def")]
  team_skill<-mm[,"home"]+(attack-rowMeans(attack))-(defence-rowMeans(defence))
  team_skill<-exp(team_skill)
  colnames(team_skill)<-teams
  team_skill<-team_skill[,order(colMeans(team_skill),decreasing = T)]  
  
  # Plot settings
  par(mar=c(3,2,2,1),family="serif")
  
  # Plot ranking
  caterplot(team_skill,labels.loc="above",val.lim = c(0.5, 4),
          cex=1,col="black",lwd=c(3,5),style="plain",bty="n",axes=FALSE)
  axis(1, tick=F)
}

#### Plot function average effects ####
effPlot<-function(x){
  # Get data for average effects
  att<-x$BUGSoutput$summary[1:20,1]
  def<-x$BUGSoutput$summary[21:40,1]
  
  # Plot settings
  par(mar=c(5,4,2,2),pty="s",family="serif")
  
  # Empty plot
  plot(att,def,type="n",axes=F,xlab="Goal scoring effect",ylab="Defense effect",
     xlim=c(-.5,.5),ylim=c(-.5,.5))
  
    abline(v=0,lty=2,lwd=1);abline(h=0,lty=2,lwd=1) # Guiding lines
    text(att,def,teams,cex=.8)   # Teams
  
  # Axis
  axis(1,tick=F);axis(2, tick=F,las=1)
  minimalrug(att,side=1,line=-.5,lwd=2);minimalrug(def,side=2,line=-.5,lwd=2)  
}




#### Fancyaxis ####
library(devtools)
source_url("https://raw.githubusercontent.com/sjmurdoch/fancyaxis/master/fancyaxis.R")
