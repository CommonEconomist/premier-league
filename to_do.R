
## Season prediction
pl_forecast <- df[is.na(df$HomeGoals), c("Season", "Date", "HomeTeam",
    "AwayTeam")]
m_forecast <- m_pred[is.na(df$HomeGoals), ]
pl_forecast$mean_home_goals <- round(m_forecast[, "mean_home_goal"], 1)
pl_forecast$mean_away_goals <- round(m_forecast[, "mean_away_goal"], 1)
pl_forecast$mode_home_goals <- m_forecast[, "mode_home_goal"]
pl_forecast$mode_away_goals <- m_forecast[, "mode_away_goal"]
pl_forecast$predicted_winner <- ifelse(m_forecast[, "match_result"] ==
    1, pl_forecast$HomeTeam, ifelse(m_forecast[, "match_result"] == -1,
    pl_forecast$AwayTeam, "Draw"))

rownames(pl_forecast) <- NULL

pl_sim <- df[is.na(df$HomeGoals), c("Season", "Date", "HomeTeam",
    "AwayTeam")]
pl_sim$home_goals <- m_forecast[, "rand_home_goal"]
pl_sim$away_goals <- m_forecast[, "rand_away_goal"]
pl_sim$winner <- ifelse(m_forecast[, "rand_match_result"] == 1, pl_forecast$HomeTeam,
    ifelse(m_forecast[, "rand_match_result"] == -1, pl_forecast$AwayTeam,
        "Draw"))

rownames(pl_sim) <- NULL

## Calculate standings end of the season

# Reshape to long format
md<-melt(pl_sim,measure.vars=c("HomeTeam","AwayTeam"),variable_name="location")
md <- rename(md, c(value = "team"))
md <- md[order(md$Date, md$team), c("Date","team", "location", "winner")]

# Calculate points per game
md$pts <- as.integer(with(md, ifelse((team == winner), 3,
                                       ifelse(winner == "Draw", 1, 0))))

md<-ddply(md,.(team),transform,total=cumsum(pts))


# Look at standings end of season
end<-md[md$Date=="2015-05-24",]
end$rank <- rank(-end$total, ties.method = "min")
end<-end[order(end$rank),]
