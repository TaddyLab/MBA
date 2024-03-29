######################################
#### Factors
######################################

##### *** rollcall voting *** #####

votes <- read.csv("rollcall-votes.csv")
votes[1:6,1:5]

pcavote <- prcomp(votes, scale=TRUE)
plot(pcavote, main="")
mtext(side=1, "Rollcall-Vote Principle Components",  line=1, font=2)

## interpreting vote factors
legis <- read.csv("rollcall-members.csv", strings=T)
votepc <- predict(pcavote) # scale(votes)%*%pcavote$rotation
#png('VOTEpc.png', width=8, height=5, units="in", res=720)
plot(votepc[,1:2], pch=21, bg=(4:2)[legis$party], main="", bty="n")
#dev.off()

# big scores on pc1 are left and right ideologically
votepc[order(votepc[,1])[1:3],1]
votepc[order(-votepc[,1])[1:3],1]

# big scores -/+ on pc 2?
head(sort(votepc[,2]))

# look at the loadings
phi <- pcavote$rotation[,1:2]

## the 1st is traditional left-right
#png('VOTEloads.png', width=8, height=5, units="in", res=720)
hist(phi[,1], main="", xlab="1st Principle Component Vote-Rotations",
     col=8, border=grey(.9))
abline(v=phi[884,1], col=2)
text(x=phi[884,1], y=550, "Afford. Health (amdt.)", xpd=TRUE, col=2, font=3)
abline(v=phi[25,1], col=4)
text(x=phi[25,1], y=550, "TARP", xpd=TRUE, col=4, font=3)
#dev.off()

## trying to interpret the 2nd factor
phi[order(abs(phi[,2]), decreasing=TRUE)[1:5],2]
## attendance!
sort(rowSums(votes==0), decreasing=TRUE)[1:6]





