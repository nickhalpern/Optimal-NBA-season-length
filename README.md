# What is the Optimal Length of the NBA Season? 
### (Hint: A lot shorter than it is now!)

## Summary

o	We can use the average point differential as a proxy for how good a team is.  Literature shows that point differential is more predictive of future record than current record.

o	Using this, we an estimate the probability Team A beats Team B based on difference in point differential, and which team is the home team.  Then we can simualate an entire season, and see how quickly the simulated standings converge to the point differential rankings.

o	Standings are always "improving" but there is a step drop-off around 40 games.  The simulated data is corroborated by actual win-loss progression from the past twenty years.  *This analysis suggests the NBA season could be half as long without significantly affecting the "accuracy" of win-loss standings.*



## Introduction and Methodology

In this analysis, I study the optimal length of the NBA season. I consider the question from a competitive perspective, and ignore financial implications.  My underlying assumption is that at any point during the season, a team’s ability is best measured by its point differential rather than its record.

As shorthand, I use the following terms:

o	Better teams have a more positive point differential than worse teams.   Teams with a more positive / less negative point differentials on average have better records than teams with less positive / more negative point differentials.

o	As more games are played, standings improve as they tend to converge to the rankings which are implied by average point differential.  I measure error as the mean squared error (MSE) between the ranking of point differential compared to the standings based on wins and losses.

o	The optimal length of the season balances two conflicting considerations:

-	If there are too few games, the final standings will be less accurate: worse teams are more likely to finish with better records than better teams.

-	If there are too many games, the latter part of the season will be full of games which have little bearing on final standings.

To see how these considerations are affected by the length of a season, I ran 10,000 simulated seasons using parameters from actual NBA seasons.  

Average point differential is commonly assumed to be a better indicator of team skill than winning percentage.  Over the course of the season, winning percentage is over 95% correlated with point differential.  The chart below graphs the average point differential against winning percentage for the 2014-15 season. 

![Point Differential vs. Winning Percentage](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba1.png)

Given the high correlation between winning percentage and point differential, I run a simple logistic regression with road team victory as the dependent variable, and independent variable difference in average point differential (plus an intercept, which is interpreted as the road team disadvantage).

![Point Differential vs. Single Game Outcome](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba2.png)

The regression coefficient of average differential is stable over the past twenty full seasons (including the two lockout years).  This suggests that it is valid to use parameters from the logistic regression in a large simulation. 

![Stability of Regression](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba3.png)

## Output of the Simulation

Given a logistic model and average point differentials it is simple to simulate NBA seasons and measure the improvement in standings as the season progresses.

The charts below show the average MSE as the season progresses and average amount of movement in standings per team (per two games).  These are actually similar effects: as the better teams rise in the standings, there are fewer games where a team with a worse record beats a team with a better record.  Because the measure of error and the measure of game importance are capturing similar effects, the shapes of the curves are nearly identical.

The first measure, the error between the actual standings and what the standings would be if they correlated perfectly with point differential, shows a flattening at 40 games and becomes nearly flat at 60 games.  From 40 games to 50 games, the error decreases by 16%; from 60 games to 70 games, the error decreases by just 6%.

![Accuracy of Standings](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba4.png)

As the season progresses, we see a similar decline in the turnover in the standings.  By the 40th game, the average team moves by 0.6 places (in a 30 team league) up or down every two games played.  By the 60th game, the average team moves by just 0.4 places every two games.  Compared to an 82 game season, the games at the end of a 60 game season would be 28% more influential to the final standings.

![Change in Standings](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba5.png)

Based only on simulation results, these measures show that the NBA season could be 40-60 games in length, with the result that the best teams end up with the best records and the importance of each game is greater.  In the following section I examine how closely the simulation mirrors data from the past twenty seasons.  For simplicity, I focus only on the MSE measure.

## Comparison to Past Twenty Seasons

The simulation simplifies a good deal: there is no account for a change in a team’s performance over the course of the season (injuries, trades, improvement/deterioration in play), long stretches of games at home or on the road, or unbalanced strength of schedule, to name a few.

The actual average error as the season progresses is different from the simulation, most notably in its slope.  The steeper decline suggests that more games are necessary for the standings to reflect the end-of-season average point differentials closely.

![Accuracy of Standings](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba6.png)

The differences are attributable to two broad factors:

1.	Average point differential can vary dramatically over the course of the season.   This can be due to:
a.	regression to the mean: average second-half point differential is approximately 80% of first-half point differential 
b.	in-season trades (for example, in 2011-12 Jazz traded Deron Williams in February)
c.	major injury (for example, in 2014-15 Kevin Durant broke his foot and missed 2/3 of the season)
d.	other unknown factors 

2.	The regression coefficient on difference in point differential is higher in the second half of the season than in the first half (using either full season or half season point differential as the independent variable).  That is, holding difference in average point differential constant, the better team is more likely to win as the season progresses.

The dominant effect is (1) - the fact that the regression coefficient changes over the course of the season is interesting but is not significant enough to be a material contributor to the shape of the MSE curve.  

For the purposes of this analysis, the most important factor is whether teams have enough games to achieve a record reflecting their skill.  It is then important to differentiate between meaningful changes in a team’s record, and less meaningful changes.  In other words, the season should be long enough that teams have the opportunity to improve but not so long that injuries play an increasing role in the outcomes. 

As seen in the chart below, approximately 80% of teams (excluding lockout years) have a second half point differential which is within 4 points of their first half point differential.  

![Change in Differential](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba7.png)

Trades and injuries are frequent events during the course of the season (and the latter becomes more likely the longer the season).  In order to filter out teams which have large swings in performance and which do not tend to be relevant in the playoffs, I exclude teams which meet both of the following criteria:

o	have a change in point differential from first half to second half of at least 5 points (either positive or negative)

o	have a negative second half point differential

This filter eliminates 22 of 533 observations (533 is 18 non-lockout seasons consisting of 29 or 30 teams).  Among the outliers excluded are the 2011-12 Utah Jazz (traded Deron Williams at the trade deadline; their differential was 7.9 points worse in the second half), 2014-15 Phoenix Suns (traded Goren Dragic and Isiah Thomas at the trade deadline; their differential was 7.3 points worse in the second half), and the 2007-08 Portland Trail Blazers (Brandon Roy injured after the All-Star Break; their differential was 5.0 points worse in the second half).  Many of the teams meeting these criteria are young squads who were outscored in the first half, and differential further worsened as the season progressed (2009-10 LA Clippers, 2012-13 Orlando Magic, 2012-13 Detroit Pistons).  It also excludes a handful of seasons without an obvious cause for a change in point differential.  The most notable is the inexplicable regular season collapse of the 2013-14 Indiana Pacers, who are an exception since they recovered to reach the Eastern Conference Finals.

With these seasons excluded, the actual MSE over the course of the season resembles closely the simulation.  In particular, the simulated MSE and the observed MSE with outliers excluded are approximately parallel after 40 games.  This suggests that the conclusions from the simulation also hold for data from actual NBA seasons.

![Accuracy of Standings](https://github.com/nickhalpern/Optimal-NBA-season-length/blob/master/nba_images/nba8.png)

## Conclusion

The NBA season has been 82 games long since the 1967-68 season, with the exception of two seasons shortened by lockouts.  Ignoring the financial implications of a shorter or longer schedule, what are the important competitive considerations?

1.	Quality of the entertainment: anecdotal evidence suggests that a very short season does not give teams enough time to coalesce, while too many games translates to games with low stakes and less than maximum effort.
2.	Fairness: there is an element of randomness in every game played, therefore the longer the season (all else constant), the more likely that the best teams will finish with the best records.

An additional consideration is the risk of injuries, which have been shown to increase when players play more total games and/or play with less rest between games.

The two points are related.  If there are very few games, each one will be very important, but it is very likely that due to chance better teams will finish behind worse teams in the final standings (the NFL model).  On the other hand, if the schedule is very long, it is likely that the best teams will finish with the best records (ignoring the risk of injuries) but each game will be relatively unimportant (the MLB model).  How many games are necessary for the proverbial cream to rise to the top while ensuring that each game is important?  

The simulation, corroborated by data from the past 20 NBA seasons, suggests that after approximately 40 games, the benefit of additional games diminishes significantly and the frequency of games which impact the standings falls sharply.  Advocates for a shorter NBA season typically suggest a cut to 70-75 games.  This analysis shows that the season could be made considerably shorter with little effect on the integrity of the standings.
