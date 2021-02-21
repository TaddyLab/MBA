## for more economically detailed analysis, see
## https://stats.idre.ucla.edu/stat/data/hsbdemo

library(mlogit)
data(Heating)

y <- Heating$depvar
H <- Heating[,-c(1:2)]
H$incFac <- factor(H$income)
x <- sparse.model.matrix( ~ .*(incFac + region), data=naref(H))[,-1]

library(glmnet)
system.time(
	netfit <- cv.glmnet(x, y, family="multinomial")
)

library(distrom)
cl = makeCluster(detectCores())
cl

system.time(
	dmrfit <- dmr(cl, x, y, verb=TRUE, cv=TRUE, lmr=1e-3, free=1:10)
)

plot(dmrfit)
