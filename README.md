# Modeling Premier League results

**UPDATE IN PROGRESS**

The English Premier League is arguably the [most popular national football competition](http://www.nytimes.com/2015/04/02/upshot/globalization-under-attack-on-the-soccer-field.html) in the world.
There are a number of interesting analyses provided by [FiveThirtyEight](http://fivethirtyeight.com) on this particular competitions such as the [division hierarchy](fivethirtyeight.com/features/beneath-the-premier-league-stands-the-great-football-pyramid-of-england/), [home-field advantage](http://fivethirtyeight.com/features/home-field-advantage-english-premier-league/), [a longer history on match results](http://fivethirtyeight.com/features/in-126-years-english-football-has-seen-13475-nil-nil-draws/), and the [shift in league domination](http://fivethirtyeight.com/features/the-long-migration-of-english-football/).
Here I attempt to model the match results of the Premier League using Bayesian estimation of a Poisson model based -entirely- on the work by Rasmus Bååth on the the Spanish La Liga. 
See his posts [here](http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-one/), [here](http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-two/), and [here](http://www.sumsar.net/blog/2013/08/modeling-match-results-in-la-liga-part-three/).

I use match data from [Football-Data](http://www.football-data.co.uk/data.php) which covers the premier league from the 1993/94 season onwards.
The data that I use is available in the `premier_league.csv` file, for the actual estimation see `regression.R`. 
