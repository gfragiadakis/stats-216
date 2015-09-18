## Work from section 2
## Stats 216
## written by GKFragiadakis
## September 18th, 2015

#-----------------------------

# Problem 1
# load data
data_directory <- "Documents/courses/STATS216/stats-216/"
games <- read.csv(paste(data_directory,"games.csv", sep=""),as.is = TRUE)
teams <- read.csv(paste(data_directory,"teams.csv", sep=""),as.is = TRUE)

# get all teams in one vector because there isn't perfect overlap
all.teams <- sort(unique(c(teams$team,games$home,games$away)))

#-----------------------------

# Problem 2
# rank teams on regular season games

reg_season_games <- games[games$gameType == "REG", ]
# could add up all the wins for each team and sort based on that

## Function to compute a team's total margin of victory
total.margin <- function(team) {
  with(games,
       sum(homeScore[home==team])
       + sum(awayScore[away==team])
       - sum(homeScore[away==team])
       - sum(awayScore[home==team]))
}
## Compute total margin for each team
margins <- sapply(teams$team, total.margin)
names(margins) <- teams$team
rank.table <- cbind("Margin" = margins,
                    "Margin Rank" = rank(-margins,ties="min"),
                    "AP Rank" = teams$apRank,
                    "USAT Rank" = teams$usaTodayRank)
margin.top25 <- order(margins,decreasing=TRUE)[1:25]
rank.table[margin.top25,]

#----------------------
# Problem 3
# we now want a linear model
# our response variable Y is the margin (home - away)
Y <- games$homeScore - games$awayScore
# we have a predictor for each of j teams, presented as dummy variables
X <- as.data.frame(matrix(0,dim(games)[1], length(all.teams)))
names(X) <- all.teams

for (tm in all.teams){
  X[,tm] <- 1*(games$home==tm) - 1*(games$away==tm)
}

# use Stanford as baseline (remove that column from our matrix)
X <- X[ , names(X) != "stanford-cardinal"]
reg.season.games <- which(games$gameType=="REG")

mod <- lm(Y ~ 0 + ., data = X, subset= reg.season.games)



