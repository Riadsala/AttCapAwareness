library(ggplot2)
library(lme4)
library(car)
library(dplyr)
fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("responseCapture.csv")


fDat$observer = as.factor(fDat$observer)

# remove incorrect trials, and trials with NA pathlength
rDat$okTrial = 
	rDat$targDiscrim==1 &
	is.finite(rDat$pathLength) &
	rDat$RT > 0.150 & rDat$RT < 3

aggregate(okTrial ~ observer, rDat, "mean")

dat = filter(rDat, okTrial==1)


#  remove some outliers - for now, worst 1% of data
# dat = filter(dat, RT<= quantile(dat$RT, 0.99))

adat  = (filter(dat, congC!="no distracter")
		%>% group_by(observer, thought, captured, congC, distracter) 
		%>% summarise(
			medianRT=median(RT),
			nTrials = length(RT)))
write.csv(adat, "summaryData.txt", row.names=FALSE)
adat$observer = as.factor(adat$observer)


plt = ggplot(adat, aes(x=observer, y=nTrials, fill=congC))+geom_bar(stat="identity", position=position_dodge())
plt = plt + facet_grid(captured~thought)
plt = plt + theme_light()
plt = plt + scale_y_continuous(name="median RT")
ggsave("../plots/nTrialsByCondition.pdf", width=8, height=6)


plt = ggplot(filter(adat, captured!="none"), aes(x=observer, y=medianRT, fill=congC))+geom_bar(stat="identity", position=position_dodge())
plt = plt + facet_grid(captured~thought)
plt = plt + theme_bw()
plt = plt + scale_y_continuous(name="median RT")
ggsave("../plots/MedianRT.pdf", width=8, height=6)



adat = aggregate(RT ~observer+captured+congC+thought, filter(dat,congC!="no distracter"), "median")
icdat = select(filter(adat, congC=="incongruent"), observer, captured, thought, RT)
names(icdat)[4] = "incongruent_RT"
ccdat = select(filter(adat, congC=="congruent"), observer, captured, thought, RT)
names(ccdat)[4] = "congruent_RT"
compDat = merge(ccdat, icdat)
rm(icdat, ccdat)
compDat$RT_diff = compDat$incongruent_RT - compDat$congruent_RT 

# m = lmer(RT_diff ~ captured * thought + (captured + thought|observer), compDat)




cmpPlot = ggplot(compDat, aes(x=observer, y=RT_diff))+geom_bar(stat="identity")
cmpPlot = cmpPlot + facet_grid(captured~thought)
cmpPlot = cmpPlot + theme_linedraw()
cmpPlot = cmpPlot + scale_y_continuous(name="median incongruent RT - median congruent RT")
ggsave("../plots/differenceInMedianRT.pdf")

adat2  = (compDat 
		%>% group_by(captured, thought) 
		%>% summarise(
			meanRTdiff=mean(RT_diff), 
			nPeople=length(RT_diff),
			stder =sd(RT_diff)/sqrt(nPeople),
			lower = meanRTdiff-1.96*stder,
			upper = meanRTdiff+1.96*stder))

agPlt = ggplot(filter(adat2, captured!="none") , aes(x=captured, y=meanRTdiff, ymin=lower, ymax=upper, colour=thought))
agPlt = agPlt + geom_point() + geom_errorbar()
agPlt = agPlt + theme_light() + scale_y_continuous("mean incongruent - congruent RT")
agPlt = agPlt + scale_colour_brewer(palette = "Set1")
ggsave("../plots/meanRT.pdf", width=5, height=5)

# model <- lmer(RT ~ congC + captured + (thought + congC + captured | observer), data=filter(dat, congC!="no distracter"))

# model <- lmer(RT ~ congC * captured * thought + (thought + congC + captured | observer), filter(dat, congC!="no distracter"))