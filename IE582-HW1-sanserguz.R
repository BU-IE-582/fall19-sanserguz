#IE 582

#TASK 1
#1.1

library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
setwd("~/Downloads/UniStuff/IE 582/HW1/IE582Fall2019_data_files")
matches <- read.csv(file="matches.csv",header=TRUE)

goalinfo <- subset(matches, match_status == 'Finished' & league_id == 148,select = c("match_hometeam_score","match_awayteam_score"))
awaygoals = goalinfo$match_awayteam_score
homegoals = goalinfo$match_hometeam_score

#Histograms:
hist(homegoals, main= "Home Team Goals for English Premiere League", xlab="Home Goals", xlim=c(0,8), ylab="Number of Games", col="magenta", breaks = 20)
hist(awaygoals, main= "Away Team Goals for English Premiere League", xlab="Away Goals", xlim=c(0,8), ylab="Number of Games", col="magenta", breaks = 20)
hist(homegoals - awaygoals, main= "Goal Difference", xlab="Home Goals - Away Goals", ylab="Number of Games", col="magenta")

#1.2
hist(homegoals, main= "Home Team Goals for English Premiere League", xlab="Home Goals", xlim=c(0,8), ylab="Number of Games", col="magenta", breaks = 20)
pois = dpois(0:10, mean(homegoals))*length(homegoals)
lines(0:10, pois)

hist(awaygoals, main= "Away Team Goals for English Premiere League", xlab="Away Goals", xlim=c(0,8), ylab="Number of Games", col="magenta", breaks = 20)
pois = dpois(0:10, mean(awaygoals))*length(awaygoals)
lines(0:10, pois)

# Histograms were constructed and line representing poisson distribution with lambda equaling average number of goals for both histograms were drawn. 
# It can be observed that poisson distribution fits well with both the home team scores and the away team scores as the fitted line accurately follows histogram bar peaks.
# As Mirza and Fejes [1] argue in their paper; as goal difference is defined as the difference of two independent poissonly distributed random variables (home and away scores), it follows a special distribution called skellam distribution.

#TASK 2

# Bookmakers chosen for this part are: ’10Bet’, ‘UniBet’, ‘Pinnacle’ and ‘Marathonbet.’

matches <- read.csv(file="matches.csv",header=TRUE)
bets <- read.csv(file="bets.csv",header=TRUE)
epl <- subset(matches, league_id == 148,select = c("match_id","epoch"))


scores <- subset(matches, match_status == 'Finished' & league_id == 148,select = c("match_id","match_hometeam_score","match_awayteam_score"))
scores = cbind(scores, outcome=0)
scores = setorder(scores,match_id)
for(i in 1:618){if(scores[i,2] > scores[i,3]){scores[i,4]=1}else if(scores[i,2] < scores[i,3]){scores[i,4]=2}else{scores[i,4]=0}}
scores[2:3] = NULL

# Match IDs and outcomes (1 for home win, 2 for away win and 0 for tie) are stored in “scores” data frame that will be used in Task 2 and 3.

## Bookmaker 1 - ‘10Bet’

betsinfo <- subset(bets, odd_bookmakers == '10Bet' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id)
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

# Storing probabilities in a data table called “probs”:

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"

# Normalization:

probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal

#Plot

plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task2: 10bet")

# 10 bins of equal length are defined between -1 and 1. 

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
summary(bins)

tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
summary(bins2)

ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task2: 10bet")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")


## Bookmaker 2 - ‘UniBet’

# Same method is implemented to achieve the model for each of the bookmakers as follows.

betsinfo <- subset(bets, odd_bookmakers == 'Unibet' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id)
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"

# Normalization:

probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal

plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task 2: Unibet")

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
summary(bins)

tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
summary(bins2)

ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task 2: Unibet")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")


## Bookmaker 3 - ‘Pinnacle’

betsinfo <- subset(bets, odd_bookmakers == 'Pinnacle' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id)
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"

# Normalization:

probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal

plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task 2: Pinnacle")

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
summary(bins)

tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
summary(bins2)

ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task 2: Pinnacle")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")


## Bookmaker 4 - ‘Marathonbet’

betsinfo <- subset(bets, odd_bookmakers == 'Marathonbet' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id)
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"

# Normalization:

probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal

plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task 2: Marathonbet")

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
summary(bins)

tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
summary(bins2)

ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task 2: Marathonbet")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")

# In this part, these 4 bookmakers was selected as they have larger data sets compared to some other bookmakers. 
# The reason behind this choice is to reach a more robust statistical inference basing on a larger sample data. Since the objective is to decide whether it is possible to make money IN THE LONG RUN by abusing the inadequacy of bookmaker’s calculations, low amount of data would not provide reliable results.
# Estimated probability of draws from data files (mapped red points) are found to be generally below the proposed probabilities with a few exceptions.


#TASK 3

booking <- read.csv(file="booking.csv",header=TRUE)
goals <- read.csv(file="goals.csv",header=TRUE)
matches <- read.csv(file="matches.csv",header=TRUE)
bets <- read.csv(file="bets.csv",header=TRUE)

scores <- subset(matches, match_status == 'Finished' & league_id == 148,select = c("match_id","match_hometeam_score","match_awayteam_score"))
scores = cbind(scores, outcome=0)
scores = setorder(scores,match_id)
for(i in 1:618){if(scores[i,2] > scores[i,3]){scores[i,4]=1}else if(scores[i,2] < scores[i,3]){scores[i,4]=2}else{scores[i,4]=0}}
scores[2:3] = NULL

# Two cases described in homework description were  selected to be the noise to be eliminated.
# First is the case of an early red card. Premiere league matches with a red card within the first 15 minutes was picked from the data file booking.csv. 
# Second, it is decided to remove the matches with a goal scored during the extra time which changed the outcome of the match.
# Matches selected for removal are stored in a vector called “noises”. 


epl <- subset(matches, league_id == 148,select = c("match_id","epoch"))
earlyred <- subset(booking, as.numeric(as.character(time)) <= 15 & card == "red card",select = c("match_id"))
earlyred = merge(earlyred, epl)
lategoal <- subset(goals,as.character(time) > 90)
candidates <- subset(matches, match_status == 'Finished' & league_id == 148 & (match_hometeam_score == match_awayteam_score | match_hometeam_score == match_awayteam_score - 1 | match_hometeam_score == match_awayteam_score + 1),select = c("match_id","match_hometeam_score","match_awayteam_score"))
lategoal = merge (lategoal,candidates)
lategoal = subset(lategoal, (match_hometeam_score > match_awayteam_score & (as.numeric(away_scorer) == 1)) | (match_hometeam_score < match_awayteam_score & (as.numeric(home_scorer) == 1)) | match_hometeam_score == match_awayteam_score)
noises = sort(unique(c(earlyred$match_id,lategoal$match_id)))


# 4 Matches were removed because of booking noise.
# 23 Matches were removed because of late goals.
# In total, 27 matches out of 628 English Premiere League matches were removed to evade the noise.
# Third and fourth subtasks of Task 2 is repeated for cleared data of all 4 bookmakers as below.

## Task 3 - For Bookmaker 1 - ‘10Bet’

# Method for task 2 is repeated. But in this case subset “betsinfo” does not hold information about matches that might cause noise, as described above.
# Match ID’s for noises were excluded while subsetting bets.csv data file, and the rest of the method is iterated accordingly. 

betsinfo <- subset(bets, odd_bookmakers == '10Bet' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id & !(match_id %in% noises))
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"
probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3: 10bet")

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3: 10bet")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")

## Task 3 - For Bookmaker 2 - ‘Unibet’

betsinfo <- subset(bets, odd_bookmakers == 'Unibet' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id & !(match_id %in% noises))
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"
probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3: Unibet")

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3: Unibet")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")

## Task 3 - For Bookmaker 3 - ‘Pinnacle’

betsinfo <- subset(bets, odd_bookmakers == 'Pinnacle' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id & !(match_id %in% noises))
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"
probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3: Pinnacle")

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3:Pinnacle")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")

## Task 3 - For Bookmaker 4 - ‘Marathonbet’

betsinfo <- subset(bets, odd_bookmakers == 'Marathonbet' & (variable == 'odd_1'| variable == 'odd_x'| variable == 'odd_2') & match_id %in% epl$match_id & !(match_id %in% noises))
prob = 1/betsinfo$value
betsinfo2 <- cbind(betsinfo,prob)
betsinfo3 <- setorder(betsinfo2,match_id,odd_epoch)

probs<- subset(betsinfo3,variable == 'odd_1',select = c("match_id","odd_epoch","prob"))
names(probs)[3] <- "phome"
probs <- cbind(probs,oddaway = subset(betsinfo3,variable == 'odd_2',select = c("prob")))
names(probs)[4] <- "paway"
probs <- cbind(probs,oddtie = subset(betsinfo3,variable == 'odd_x',select = c("prob")))
names(probs)[5] <- "ptie"
probs$ptotal = probs$phome +probs$paway +probs$ptie
probs$n_phome = probs$phome/probs$ptotal
probs$n_paway = probs$paway/probs$ptotal
probs$n_ptie= probs$ptie/probs$ptotal
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3:10bet")

model <- merge(probs, scores, by="match_id")
model$diff= model$n_phome-model$n_paway
bins = cut(model$diff,seq(-1,1,0.2))
tiedgames <- subset(model, outcome == 0,select = c("diff"))
bins2 = cut(tiedgames$diff,seq(-1,1,0.2))
ytie=summary(bins2)/summary(bins)
plot(probs$n_phome-probs$n_paway,probs$n_ptie,xlab="P(Home Win) - P(Away Win)",ylab="P(Tie)",main="Task3: Marathonbet")
points(seq(-0.9,0.9,0.2),ytie, pch=24 ,col="red")

# All in all, we removed matches with somewhat extreme conditions which might cause distortion in the performance measurments of bookmakers’s calculations. 
# It is observed that mapped red points, estimated probabilities for draw, have generally descended a bit between Task 2 and 3.
# This descent means that making money in the long run through playing draw is even slimmer than before.
# Thus, it can be concluded that bookmakers are indeed making adequate calculations in their probabilities that work in their favor.


