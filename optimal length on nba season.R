
# # load the libraries and simulation data

setwd("C:/Users/flour/Dropbox/Nick's stuff/PSU/STAT 497")
# setwd("~/Dropbox/Nick's stuff/PSU/STAT 497")
# library used to merge data
library(data.table)
# library used to rename columns
library(plyr)
# install.packages("dplyr")
library(dplyr)


nba2015 <- read.table("Project497D/bball2015.txt", header = TRUE, comm = "#")
gameresults <- read.table("Project497D/leagues_NBA_2015_games_games.txt", header = TRUE, comm = "#")

nba2014 <- read.table("Project497D/bball2014.txt", header = TRUE, sep="\t")


# # demonstrate the linear relationship between winning percentage and point differential

nba2015.ordered <- nba2015[order(-nba2015$DIFF),]
model <- lm(nba2015.ordered$PCT~nba2015.ordered$DIFF)
ci = predict(model,data.frame(PCT = nba2015.ordered$DIFF),level = 0.99, interval="confidence")

par(mfrow=c(1,1))
plot(0:10, 0:10, type = "n", xlab = "", ylab = "", xlim = c(-10,10), ylim = c(0.2, 0.8))
polygon(x=c(nba2015.ordered$DIFF, rev(nba2015.ordered$DIFF)), y = c(ci[,2], rev(ci[,3])), col="gray50", border = "red")
par(new=TRUE)
with(nba2015.ordered, plot(DIFF, PCT, xlab = "Average Point Differential", ylab = "Winning Percentage", main = "Point Differential vs. Winning Percentage", pch=0, xlim = c(-10,10), ylim = c(0.2, 0.8)))
abline(model,lty=2)


# # run the logistic regression to estimate relationship between home court, point differential and win probability

a<-merge(nba2015, gameresults, by.x = "Team", by.y = "Home")
a<-plyr::rename(a, c("Team" = "HomeTeam"))
b<-merge(nba2015, a, by.x = "Team", by.y = "Visitor")
b<-plyr::rename(b, c("Team" = "RoadTeam"))
b$AvgDiff <- b$DIFF.x - b$DIFF.y
b$win <- as.numeric(0)
b[which(b$Difference>0),]$win <- 1

# run a logistic regression to get the impact of point differential and home court advantage
trans.fit <- glm(win~AvgDiff, family="binomial", data=b)

trans.fit$coefficients

# plot the game by game against season average
with(b, plot(AvgDiff, win, xlab = "Road Team - Home Team Average Point Differential", ylab = "Road Team Win = 1, Road Team Loss = 0", main = "Average Point Differential vs. Outcome"))
lines(cbind(b$AvgDiff, trans.fit$fitted)[order(b$AvgDiff), ], col="red")


# # create a schedule

# simulate the point differential
# point_diff_sd <- sd(nba2015$DIFF)
# nba2015$DIFF <- rnorm(30,0,point_diff_sd)

# create a "schedule"
schedule<-expand.grid(nba2015$Team, nba2015$Team)
schedule<-plyr::rename(schedule, c("Var1" = "RoadTeam", "Var2" = "HomeTeam"))
# remove instances where a team plays itself
schedule<-subset(schedule, RoadTeam!=HomeTeam)
# get team ids in order to create "schedule"
schedule<-merge(schedule, nba2015, by.x="RoadTeam", by.y="Team")
schedule<-plyr::rename(schedule, c("TeamID" = "RoadTeamID", "DIFF" = "RoadDiff"))
schedule<-merge(schedule, nba2015, by.x="HomeTeam", by.y="Team")
schedule<-plyr::rename(schedule, c("TeamID" = "HomeTeamID", "DIFF" = "HomeDiff"))
schedule<-subset(schedule, select = c("RoadTeam","HomeTeam","RoadTeamID","HomeTeamID", "RoadDiff","HomeDiff"))
# each gameid corresponds to one home game and one road game for each team
schedule$GameID<-schedule$RoadTeamID-schedule$HomeTeamID
schedule[which(schedule$GameID < 0),]$GameID <- schedule[which(schedule$GameID < 0),]$GameID + 30

# calculate the probability of road team win
schedule$NetDiff<-schedule$RoadDiff - schedule$HomeDiff
# schedule$RoadWinLogit <- summary(trans.fit)$coef[1,1] + schedule$NetDiff*summary(trans.fit)$coef[2,1]
schedule$RoadWinLogit <- -0.507 + schedule$NetDiff*0.1612
# schedule$RoadWinLogit <- summary(trans.fit)$coef[1,1] + schedule$NetDiff*mean(regression.slope.all[,1])
schedule$RoadWinProb <- exp(schedule$RoadWinLogit)/(1+exp(schedule$RoadWinLogit))
schedule<-subset(schedule, select = c("RoadTeam","HomeTeam","GameID","NetDiff","RoadWinProb"))

# make the schedule "longer" by copying the original schedule
schedule2 <- schedule
schedule2$GameID <- schedule2$GameID+29
schedule <- rbind(schedule, schedule2)
schedule2$GameID <- schedule2$GameID+29
schedule <- rbind(schedule, schedule2)

# add dummies for results
set.seed(1)

# j = number of trials
j <- 300

for (n in 1:j) {
  schedule[[paste("r",n, sep="")]]<-0
}


## create alternate schedules for impact of different differentials
# impact of high predictive value
schedule.high <- schedule
high.RoadWinLogit <- summary(trans.fit)$coef[1,1] + schedule.high$NetDiff*0.2
schedule.high$RoadWinProb <- exp(high.RoadWinLogit)/(1+exp(high.RoadWinLogit))
# impact of low predictive value
schedule.low <- schedule
low.RoadWinLogit <- summary(trans.fit)$coef[1,1] + schedule.low$NetDiff*0.12
schedule.low$RoadWinProb <- exp(low.RoadWinLogit)/(1+exp(low.RoadWinLogit))
# mixed high/low
schedule.mix <- schedule
mix.RoadWinLogit <- summary(trans.fit)$coef[1,1] + schedule.mix$NetDiff*0.142
mix.RoadWinLogit[615:nrow(schedule.mix)] <- summary(trans.fit)$coef[1,1] + schedule.mix$NetDiff[615:nrow(schedule.mix)]*0.25
schedule.mix$RoadWinProb <- exp(mix.RoadWinLogit)/(1+exp(mix.RoadWinLogit))
# schedule.mix$RoadWinProb[1:615] <- schedule.low$RoadWinProb[1:615]
# schedule.mix$RoadWinProb[616:nrow(schedule.mix)] <- schedule.high$RoadWinProb[616:nrow(schedule.mix)]


# # run the simulation
# simulate j seasons
for (n in (1:nrow(schedule))) {
  schedule[n,6:(6+j-1)] <- sample(x=c(0,1),size=j,replace=TRUE,prob=c(1-schedule$RoadWinProb[n],schedule$RoadWinProb[n]))
  schedule.high[n,6:(6+j-1)] <- sample(x=c(0,1),size=j,replace=TRUE,prob=c(1-schedule.high$RoadWinProb[n],schedule.high$RoadWinProb[n]))
  schedule.low[n,6:(6+j-1)] <- sample(x=c(0,1),size=j,replace=TRUE,prob=c(1-schedule.low$RoadWinProb[n],schedule.low$RoadWinProb[n]))
  schedule.mix[n,6:(6+j-1)] <- sample(x=c(0,1),size=j,replace=TRUE,prob=c(1-schedule.mix$RoadWinProb[n],schedule.mix$RoadWinProb[n]))
}


# # calculate outputs for the simulation

ID.values <- names(table(nba2015$Team))

# Rank.change <- matrix(0, ncol = j, nrow = 86)
MSE.series <- matrix(0, ncol = j, nrow = 87)
MSE.series.high <- matrix(0, ncol = j, nrow = 87)
MSE.series.low <- matrix(0, ncol = j, nrow = 87)
MSE.series.mix <- matrix(0, ncol = j, nrow = 87)

for (n in 6:j+5) {
  Rating.cumsum <- unlist(sapply(ID.values, 
                                 function(x) cumsum(1 + schedule[,n][schedule$RoadTeam == x] - schedule[,n][schedule$HomeTeam == x]))) 
  
  Rating.rank <- t(apply(-Rating.cumsum, 1, rank))
  
  nba2015.sorted <- nba2015[order(nba2015$Team),]
  
  MSE <- Rating.rank[1:87,] - t(matrix(t(rank(-nba2015.sorted$DIFF)),nrow=30, ncol=87))
  MSE.series[,n-5] <- rowSums(MSE * MSE)
} 
  # Rank.change[,n-5] <- t(rowSums(abs(Rating.rank[2:nrow(Rating.rank),] - Rating.rank[1:nrow(Rating.rank)-1,])))

for (n in 6:j+5) {
### high
  Rating.cumsum.high <- unlist(sapply(ID.values, 
                                 function(x) cumsum(1 + schedule.high[,n][schedule.high$RoadTeam == x] - schedule.high[,n][schedule.high$HomeTeam == x]))) 
  
  Rating.rank.high <- t(apply(-Rating.cumsum.high, 1, rank))
  
  MSE.high <- Rating.rank.high[1:87,] - t(matrix(t(rank(-nba2015.sorted$DIFF)),nrow=30, ncol=87))
  MSE.series.high[,n-5] <- rowSums(MSE.high * MSE.high)
}

for (n in 6:j+5) {
### low
  Rating.cumsum.low <- unlist(sapply(ID.values, 
                                 function(x) cumsum(1 + schedule.low[,n][schedule.low$RoadTeam == x] - schedule.low[,n][schedule.low$HomeTeam == x]))) 
  
  Rating.rank.low <- t(apply(-Rating.cumsum.low, 1, rank))
  
  MSE.low <- Rating.rank.low[1:87,] - t(matrix(t(rank(-nba2015.sorted$DIFF)),nrow=30, ncol=87))
  MSE.series.low[,n-5] <- rowSums(MSE.low * MSE.low)

}

for (n in 6:j+5) {
  ### mix
  Rating.cumsum.mix <- unlist(sapply(ID.values, 
                                     function(x) cumsum(1 + schedule.mix[,n][schedule.mix$RoadTeam == x] - schedule.mix[,n][schedule.mix$HomeTeam == x]))) 
  
  Rating.rank.mix <- t(apply(-Rating.cumsum.mix, 1, rank))
  
  MSE.mix <- Rating.rank.mix[1:87,] - t(matrix(t(rank(-nba2015.sorted$DIFF)),nrow=30, ncol=87))
  MSE.series.mix[,n-5] <- rowSums(MSE.mix * MSE.mix)
  
}

# Rank.changesum <- rowSums(Rank.change) / j

MSE.seriessum <- rowSums(MSE.series) / j
MSE.seriessum.high <- rowSums(MSE.series.high) / j
MSE.seriessum.low <- rowSums(MSE.series.low) / j
MSE.seriessum.mix <- rowSums(MSE.series.mix) / j

# Game Leverage
# plot(x = 2*c(5:50), y = Rank.changesum[5:50]/30, col = "black", xlab = "Length of Season", ylab = "Rankings Change (Two Games)", main = "Change in Rankings as Season Progresses", type="l")


# plot both together
# plot(0:1, 0:1, type = "n", xlab = "Length of Season", ylab = "Measure vs. Ten Game Season", main = "NBA Season Optimal Length", xlim=c(0,100), ylim = c(0,1))
# lines(x = 2*c(5:50), y = MSE.seriessum[5:50]/MSE.seriessum[5], col = "orange")
# lines(x = 2*c(5:50), y = Rank.changesum[5:50]/Rank.changesum[5], col = "green")
# legend("topright", pch=19, legend=c("Ranking MSE","Game Leverage"), col=c("orange","green"), inset = 0.1, bty = "n")



# # compare the simulation to historical data
regression.coeffs <- matrix(0, ncol = 2, nrow = 20)
regression.coeffs.first <- matrix(0, ncol = 2, nrow = 20)
regression.coeffs.second <- matrix(0, ncol = 2, nrow = 20)
point.diff.coeffs <- matrix(0, ncol = 2, nrow = 20)
correl.mtx <- matrix(0, ncol = 2, nrow = 20)


# "Error" in rankings
plot(x = 2*c(5:50), y = MSE.seriessum[5:50], col = "black", xlab = "Length of Season", ylab = "MSE (Results vs. Point Differential)", main = "Accuracy of Standings", ylim = c(0,1200), type="l", lwd = 3)
#lines(x = 2*c(5:50), y = MSE.seriessum.high[5:50], col = "red", xlab = "Length of Season", ylab = "MSE (Results vs. Point Differential)", main = "Accuracy of Standings", ylim = c(0,1200), type="l", lty = 5, lwd = 3)
#lines(x = 2*c(5:50), y = MSE.seriessum.low[5:50], col = "blue", xlab = "Length of Season", ylab = "MSE (Results vs. Point Differential)", main = "Accuracy of Standings", ylim = c(0,1200), type="l", lty = 5, lwd = 3)
#legend("topright",fill=c("black","red","blue"),legend=c("Median (0.16)","High (0.20)","Low (0.12)"), inset = 0.08, bty = "n")
#lines(x = 2*c(5:50), y = MSE.seriessum.mix[5:50], col = "green", xlab = "Length of Season", ylab = "MSE (Results vs. Point Differential)", main = "Accuracy of Standings", ylim = c(0,1200), type="l", lty = 5, lwd = 1.5)

## Run analysis on historical seasons
z <- 1
  
for (s in c("1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")) { 
  
  t <- read.table(paste("Project497D/allgames_nba_",s,".txt",sep=""), header = TRUE, sep = ",", quote = "")
  
  ID.values <- names(table(t[,3]))
  
  t$betterDate <- as.Date(t[,1], format = "%a %b %d %Y")
  
  t$roadwin <- t[,4] > t[,6]
  
  gamenumber <- sapply(ID.values, function(x) cumsum(t[,3] == x) + cumsum(t[,5] == x))
  
  wincount <- sapply(ID.values, function(x) cumsum(t[,3] == x & t[,11] == TRUE) + cumsum(t[,5] == x & t[,11] == FALSE))
  
  recordbygame <- t(sapply(1:max(gamenumber), function(y) sapply(1:length(ID.values), function(x) wincount[match(y,gamenumber[,x]),x])))
  
  standings <- t(apply(-recordbygame, 1, rank))
  
  pointdifferential <- (aggregate(t,list(t$Visitor.Neutral),mean)[,5] - aggregate(t,list(t$Visitor.Neutral),mean)[,7] + aggregate(t,list(t$Home.Neutral),mean)[,7] - aggregate(t,list(t$Home.Neutral),mean)[,5])/2

  t.f <- t[1:round(nrow(t)/2),]
  pointdifferential.first <- (aggregate(t.f,list(t.f$Visitor.Neutral),mean)[,5] - aggregate(t.f,list(t.f$Visitor.Neutral),mean)[,7] + aggregate(t.f,list(t.f$Home.Neutral),mean)[,7] - aggregate(t.f,list(t.f$Home.Neutral),mean)[,5])/2

  t.s <- t[(round(nrow(t)/2)+1):nrow(t),]
  pointdifferential.second <- (aggregate(t.s,list(t.s$Visitor.Neutral),mean)[,5] - aggregate(t.s,list(t.s$Visitor.Neutral),mean)[,7] + aggregate(t.s,list(t.s$Home.Neutral),mean)[,7] - aggregate(t.s,list(t.s$Home.Neutral),mean)[,5])/2

  pointdifferential.df <- data.frame(ID.values, pointdifferential, pointdifferential.first, pointdifferential.second)
  pointdifferential.df$chg <-  pointdifferential.df$pointdifferential.second - pointdifferential.df$pointdifferential.first
  
  if (z == 1) {pointdifferential.df.ts <- data.frame(s, ID.values, pointdifferential, pointdifferential.first, pointdifferential.second)} 
    else {pointdifferential.df.ts <- rbind(pointdifferential.df.ts, data.frame(s, ID.values, pointdifferential, pointdifferential.first, pointdifferential.second))}
    
  MSE <- standings -  t(matrix(t(rank(-pointdifferential)),nrow=length(ID.values), ncol=max(gamenumber)))
  MSE.TS <- rowSums(MSE * MSE)
  
  diff.cutoff <- 5
  #MSE.top20 <- MSE * matrix((standings[round(max(gamenumber)/2),] <= 15),ncol = length(ID.values), nrow = max(gamenumber))
  #MSE.top20 <- MSE * t(matrix((abs(pointdifferential.df$chg) <= diff.cutoff & abs(pointdifferential.df$chg/pointdifferential.df$pointdifferential) > 0.5),ncol = max(gamenumber), nrow = length(ID.values)))
  #MSE.top20.TS <- length(ID.values)/sum(abs(pointdifferential.df$chg) <= diff.cutoff & abs(pointdifferential.df$chg/pointdifferential.df$pointdifferential) > 0.5)*rowSums(MSE.top20 * MSE.top20)
  MSE.top20 <- MSE * t(matrix((abs(pointdifferential.df$chg) <= diff.cutoff & pointdifferential.df$pointdifferential.second > 0),ncol = max(gamenumber), nrow = length(ID.values)))
  MSE.top20.TS <- length(ID.values)/sum(abs(pointdifferential.df$chg) <= diff.cutoff & pointdifferential.df$pointdifferential.second > 0)*rowSums(MSE.top20 * MSE.top20)
  
  MSE.vsactual <- standings - t(matrix(t(standings[max(gamenumber),]),nrow=length(ID.values), ncol=max(gamenumber)))
  MSE.vsactual.TS <- rowSums(MSE.vsactual * MSE.vsactual)
    
  MSE.first <- standings -  t(matrix(t(rank(-pointdifferential.first)),nrow=length(ID.values), ncol=max(gamenumber)))
  MSE.first.TS <- rowSums(MSE.first * MSE.first)

  MSE.second <- standings -  t(matrix(t(rank(-pointdifferential.second)),nrow=length(ID.values), ncol=max(gamenumber)))
  MSE.second.TS <- rowSums(MSE.second * MSE.second)  
    
  lines(x = 1:max(gamenumber), y = MSE.TS, type="l", col="gray")
  
  assign(paste("MSE.",s, sep=""), MSE.TS)
  assign(paste("MSE.top20.",s, sep=""), MSE.top20.TS)
  assign(paste("MSE.first.",s, sep=""), MSE.first.TS)
  assign(paste("MSE.second.",s, sep=""), MSE.second.TS)
  assign(paste("MSE.vsactual.",s, sep=""), MSE.vsactual.TS)
  
  # calculate the logistic regression
  a<-merge(pointdifferential.df, t, by.x = "ID.values", by.y = "Visitor.Neutral")
  a<-plyr::rename(a, c("ID.values" = "RoadTeam"))
  a<-plyr::rename(a, c("pointdifferential" = "VisitorDiff"))
  b<-merge(pointdifferential.df, a, by.x = "ID.values", by.y = "Home.Neutral")
  b<-plyr::rename(b, c("ID.values" = "HomeTeam"))
  b<-plyr::rename(b, c("pointdifferential" = "HomeDiff"))
  b$AvgDiff <- b$VisitorDiff - b$HomeDiff

  # full season
  trans.fit <- glm(roadwin~AvgDiff, family="binomial", data=b)
  regression.coeffs[z,] <- trans.fit$coefficients
  
  # half season
  b$firstdiff <- b$pointdifferential.first.y - b$pointdifferential.first.x
  trans.fit <- glm(roadwin~firstdiff, family="binomial", data=b[1:nrow(b)/2,])
  regression.coeffs.first[z,] <- trans.fit$coefficients 

  # second half season
  b$seconddiff <- b$pointdifferential.second.y - b$pointdifferential.second.x
  trans.fit <- glm(roadwin~seconddiff, family="binomial", data=b[nrow(b)/2+1:nrow(b),])
  regression.coeffs.second[z,] <- trans.fit$coefficients 
  
  # regress second half point differential on first half point differential
  diff.fit <- lm(pointdifferential.second~pointdifferential.first, data=pointdifferential.df)
  point.diff.coeffs[z,] <- diff.fit$coefficients
  
  # calculate the correlation of point diff with standings and win count
  correl.mtx[z,1] <- cor(pointdifferential,standings[nrow(standings),])
  correl.mtx[z,2] <- cor(pointdifferential,wincount[nrow(wincount),])
  
  z <- z + 1
  
}

pointdifferential.df$chg <-  pointdifferential.df$pointdifferential.second - pointdifferential.df$pointdifferential.first
pointdifferential.df.ts$chg <-  pointdifferential.df.ts$pointdifferential.second - pointdifferential.df.ts$pointdifferential.first

MSE.average = (MSE.1997 + MSE.1998 + MSE.2000 + MSE.2001 + MSE.2002 + MSE.2003 + MSE.2004 + MSE.2005 + MSE.2006 + MSE.2007 + MSE.2008 + MSE.2009 + MSE.2010 + MSE.2011 + MSE.2013 + MSE.2014 + MSE.2015 + MSE.2016)/18

MSE.top20.average = (MSE.top20.1997 + MSE.top20.1998 + MSE.top20.2000 + MSE.top20.2001 + MSE.top20.2002 + MSE.top20.2003 + MSE.top20.2004 + MSE.top20.2005 + MSE.top20.2006 + MSE.top20.2007 + MSE.top20.2008 + MSE.top20.2009 + MSE.top20.2010 + MSE.top20.2011 + MSE.top20.2013 + MSE.top20.2014 + MSE.top20.2015 + MSE.top20.2016)/18

MSE.vsactual.average = (MSE.vsactual.1997 + MSE.vsactual.1998 + MSE.vsactual.2000 + MSE.vsactual.2001 + MSE.vsactual.2002 + MSE.vsactual.2003 + MSE.vsactual.2004 + MSE.vsactual.2005 + MSE.vsactual.2006 + MSE.vsactual.2007 + MSE.vsactual.2008 + MSE.vsactual.2009 + MSE.vsactual.2010 + MSE.vsactual.2011 + MSE.vsactual.2013 + MSE.vsactual.2014 + MSE.vsactual.2015 + MSE.vsactual.2016)/18

# MSE.first.average = (MSE.first.1997 + MSE.first.1998 + MSE.first.2000 + MSE.first.2001 + MSE.first.2002 + MSE.first.2003 + MSE.first.2004 + MSE.first.2005 + MSE.first.2006 + MSE.first.2007 + MSE.first.2008 + MSE.first.2009 + MSE.first.2010 + MSE.first.2011 + MSE.first.2013 + MSE.first.2014 + MSE.first.2015 + MSE.first.2016)/18

# MSE.second.average = (MSE.second.1997 + MSE.second.1998 + MSE.second.2000 + MSE.second.2001 + MSE.second.2002 + MSE.second.2003 + MSE.second.2004 + MSE.second.2005 + MSE.second.2006 + MSE.second.2007 + MSE.second.2008 + MSE.second.2009 + MSE.second.2010 + MSE.second.2011 + MSE.second.2013 + MSE.second.2014 + MSE.second.2015 + MSE.second.2016)/18

# lines(x = 1:82, y = MSE.average, col = "green", lwd = 3)
lines(x = 1:82, y = MSE.top20.average, col = "orange", lwd = 3)
# lines(x = 1:82, y = MSE.vsactual.average, col = "blue", lwd = 3)
# legend("topright",fill=c("black","green", "orange"),legend=c("Simulation","Actual: All", "Actual: Stable"), inset = 0.08, bty = "n")
legend("topright",fill=c("black", "orange"),legend=c("Simulation", "Actual ex Outliers"), inset = 0.08, bty = "n")


# lines(x = 1:82, y = MSE.first.average, col = "blue", lty = 5)
# lines(x = 1:82, y = MSE.second.average, col = "blue", lty = 5)


lines(x = 1:66, y = MSE.2012, col = "orange")

lines(x = 1:50, y = MSE.1999, col = "orange")


regression.slope.all <- matrix(0, ncol = 3, nrow = 20)
regression.slope.all[,1] <- regression.coeffs[,2]
regression.slope.all[,2] <- regression.coeffs.first[,2]
regression.slope.all[,3] <- regression.coeffs.second[,2]

barplot(t(regression.slope.all), beside = TRUE, col = c("black", "blue", "green"))
barplot(t(regression.coeffs.second[,2] - regression.coeffs.first[,2]), beside= TRUE)

median(regression.coeffs[,1])
median(regression.slope.all[1:9,1])
median(regression.slope.all[1:9,2])
median(regression.slope.all[1:9,3])

barplot(t(regression.coeffs[,2]), beside = TRUE, col = c("blue"), names.arg = c(1997:2016), main = "Point Differential Regression Coefficient", xlab = "Season")
# barplot(regression.coeffs[,1]/regression.coeffs[,2])

pointdifferential.df.ts.excl <- pointdifferential.df.ts[which(pointdifferential.df.ts$s != "1999" & pointdifferential.df.ts$s != "2012"),]

histPercent <- function(x, ...) {
  H <- hist(pointdifferential.df.ts.excl$chg, plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, main = "Distribution of Change in Point Differential", ylab = "First Half Minus Second Half", freq = FALSE, xlab = NULL, labels = labs, ylim=c(0, 1.08*max(H$density)),...)
}

histPercent(islands, col="gray")

hist(pointdifferential.df.ts.excl$chg, main = "Distribution of Change in Point Differential", ylab = "First Half Minus Second Half", freq = FALSE, xlab = NULL, labels = TRUE)
sum(abs(pointdifferential.df.ts$chg >= 4) & pointdifferential.df.ts$pointdifferential.second < 0 & s != 1999 & s != 2012)
sum(abs(pointdifferential.df.ts$pointdifferential.first - pointdifferential.df.ts$pointdifferential.second) > -1)

# get the teams with big changes and negative differentials
excl.list <- pointdifferential.df.ts[which(abs(pointdifferential.df.ts$chg) > 5 & pointdifferential.df.ts$pointdifferential.second < 0 & pointdifferential.df.ts$s != "1999" & pointdifferential.df.ts$s != "2012"),]
# are good teams more likely to beat bad teams as the season progresses?
# if so is this because good teams' point differential improves as the season goes on, or is it another effect?