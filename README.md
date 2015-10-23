# Predicting Premier League Results

The English Premier League is arguably the most popular [national football competition](http://www.nytimes.com/2015/04/02/upshot/globalization-under-attack-on-the-soccer-field.html) in the world.
There are a number of interesting analyses available on this particular competition such as
* the [division hierarchy](fivethirtyeight.com/features/beneath-the-premier-league-stands-the-great-football-pyramid-of-england/)
* [home-field advantage](http://fivethirtyeight.com/features/home-field-advantage-english-premier-league/)
* a long historical overview of [match results](http://fivethirtyeight.com/features/in-126-years-english-football-has-seen-13475-nil-nil-draws/)
* the [shift in league domination](http://fivethirtyeight.com/features/the-long-migration-of-english-football/)

Here I attempt to model the match results of the Premier League using Bayesian estimation of a Poisson model.
The model I use is taken from the paper by Baio & Biangiardo ([2010](http://discovery.ucl.ac.uk/16040/)).
For now, I'll focus on one season (2014/15), which is similar to the approach of Baio & Biangiardo (BB), who focus on the Serie A. 
In contrast, Bååth ([2015](http://www.sumsar.net/papers/baath_2015_modeling_match_resluts_in_soccer.pdf)), focussing on La Liga, uses all available data.
An earlier version of this work on the Premier League used his model, but in the interest of computation time, I decided to take another route. 
Moreover, the average tenure of a Premier League manager is just over [1 year](http://www.theguardian.com/football/2015/jun/05/managers-sacked-more-quickly-lma).

I use match data from [Football-Data](http://www.football-data.co.uk/data.php) which covers the Premier League from the 1993/94 season onwards. 
Below some figures that summarise the results. 
Hopefully in the near future I will also focus on match-prediction, rather than only the aggregates, and improve the model. 

### Goal scoring versus defence, average effect for 2014/15 season
![](http://i.imgur.com/jiFJQut.png)

### Fitted ranking for 2014/15 season
![](http://i.imgur.com/kUKLxGO.png)

### Predicted ranking for 2015/16 season
![](http://i.imgur.com/D21vWDA.png)

### Predicted ranking for 2015/16 season, taking into account 2014/15 results
![](http://i.imgur.com/hxOedML.png)


