library(ggplot2)
library(lme4)
library(car)
library(dplyr)
fDat = read.csv("fixations.csv")
rDat = read.csv("responses.csv")
fDat$observer = as.factor(fDat$observer)
rDat$pathLength = 0
rDat$nFix = 0

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

# remove incorrect trials, and trials with NA pathlength
rDat$okTrial = 
	rDat$targDiscrim==1 &
	is.finite(rDat$pathLength) &
	rDat$RT > 0.150 & rDat$RT < 3

aggregate(okTrial ~ observer, rDat, "mean")

dat = filter(rDat, okTrial==1)


aggregate(RT ~observer+congC, dat, "median")

# now classify by pathlength


dat$pathLength = dat$pathLength/256
dat$captured = (abs(dat$pathLength - 1) > 0.2)
dat$observer = as.factor(dat$observer)

#  remove some outliers - for now, worst 1% of data
# dat = filter(dat, RT<= quantile(dat$RT, 0.99))



dat$captured = as.factor(dat$captured)
levels(dat$captured) = c("direct", "capture")
dat$thoughtNoAttCap = as.factor(dat$thoughtNoAttCap)
levels(dat$thoughtNoAttCap) = c("captured", "direct")
names(dat)[6] = "thought"

adat = aggregate(RT ~observer+captured+congC+thought, filter(dat,congC!="no distracter"), "median")
icdat = select(filter(adat, congC=="incongruent"), observer, captured, thought, RT)
names(icdat)[4] = "incongruent_RT"
ccdat = select(filter(adat, congC=="congruent"), observer, captured, thought, RT)
names(ccdat)[4] = "congruent_RT"
compDat = merge(ccdat, icdat)
rm(icdat, ccdat)
compDat$RT_diff = compDat$congruent_RT - compDat$incongruent_RT

cmpPlot = ggplot(compDat, aes(x=observer, y=RT_diff))+geom_bar(stat="identity")
cmpPlot = cmpPlot + facet_grid(captured~thought)
cmpPlot = cmpPlot + theme_linedraw()
cmpPlot = cmpPlot + scale_y_continuous(name="congruent RT - incongruent RT")
ggsave("rt2.pdf")

adat2  = (dat 
		%>% group_by(captured, congC, thought) 
		%>% summarise(
			meanRT=mean(RT), 
			nTrials=length(RT),
			stder =sd(RT)/sqrt(16)))

plt = ggplot(adat2, aes(x=congC, y=meanRT, ymax=meanRT+1.96*stder, ymin=meanRT-1.96*stder))
plt = plt + geom_point() + geom_errorbar() + geom_path()
plt = plt + facet_grid(captured~thought)
plt

plt = ggplot(adat, aes(x=thought, y=RT, fill=congC)) 
plt = plt + geom_boxplot(notch=T) + facet_grid(.~captured)
plt



model <- lmer(RT ~ congC + captured + (thought + congC + captured | observer), data=filter(dat, congC!="no distracter"))

model <- lmer(RT ~ congC * captured * thought + (thought + congC + captured | observer), filter(dat, congC!="no distracter"))