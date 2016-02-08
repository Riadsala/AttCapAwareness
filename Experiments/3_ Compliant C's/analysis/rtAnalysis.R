library(ggplot2)
library(lme4)
library(car)
library(dplyr)
fDat = read.csv("fixations.csv")
rDat = read.csv("responses.csv")
fDat$observer = as.factor(fDat$observer)
rDat$pathLength = 0
rDat$nFix = 0

colours <- rainbow_hcl(4, start = 90, end = -30)

for (tr in 1:nrow(rDat))
{
	fix = fDat[which((fDat$trial==rDat$trial[tr]) & (fDat$observer==rDat$observer[tr])),]
	rDat$nFix[tr] = nrow(fix)
	if (nrow(fix)>1)
	{
		amp = sqrt((fix$x[1])^2+(fix$y[1])^2)
		for (f in 2:nrow(fix))
		{
			amp = sqrt((fix$x[f]-fix$x[f-1])^2+(fix$y[f]-fix$y[f-1])^2)
			rDat$pathLength[tr] = rDat$pathLength[tr] + amp
		}
		rm(amp)
	}
	else
	{
		rDat$pathLength[tr] = NaN
	}
}

rDat$congC = as.numeric(rDat$tc == rDat$dc)
rDat$congC[which(rDat$dc==0)] = 2
rDat$congC = as.factor(rDat$congC)
levels(rDat$congC) = c('incongruent', 'congruent', 'no distracter')

# rDat$thoughNoAttCdap = as.factor(rDat$thoughNoAttCap)

# now classify by pathlength
dat = rDat[-which(rDat$nFix<2),]
dat$pathLength = dat$pathLength/256
dat$captured = (abs(dat$pathLength - 1) > 0.2)
dat$observer = as.factor(dat$observer)

#  remove some outliers - for now, worst 1% of data
# dat = filter(dat, RT<= quantile(dat$RT, 0.99))

adat = aggregate(RT~observer+captured+congC+thoughtNoAttCap, dat, "median")
adat2  = (dat 
		%>% group_by(captured, congC, thoughtNoAttCap) 
		%>% summarise(
			meanRT=mean(RT), 
			nTrials=length(RT),
			stder =sd(RT)/sqrt(16)))

plt = ggplot(adat2, aes(x=congC, y=meanRT, ymax=meanRT+1.96*stder, ymin=meanRT-1.96*stder))
plt = plt + geom_point() + geom_errorbar() + geom_path()
plt = plt + facet_grid(captured~thoughtNoAttCap)
plt

plt = ggplot(adat, aes(x=thoughtNoAttCap, y=RT, fill=congC)) 
plt = plt + geom_boxplot() + facet_grid(.~captured)
plt



model <- lmer(RT ~ congC + captured + (thoughtNoAttCap + congC + captured | observer), data=filter(dat, congC!="no distracter"))

model <- lmer(RT ~ congC * captured * thoughtNoAttCap + (thoughtNoAttCap + congC + captured | observer), dat)