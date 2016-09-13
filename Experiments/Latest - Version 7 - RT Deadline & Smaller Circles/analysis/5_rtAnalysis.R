library(ggplot2)
library(lme4)
library(car)
library(dplyr)
library(tidyr)
library(ez)
fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("responseCapture.csv")

rDat$observer = as.factor(rDat$observer)
fDat$observer = as.factor(fDat$observer)

# remove incorrect trials, and trials with NA pathlength
rDat$okTrial = 
	rDat$targDiscrim==1 &
	is.finite(rDat$pathLength) &
	rDat$RT > 0.150 & rDat$RT < 3

aggregate(okTrial ~ observer, rDat, "mean")

dat = filter(rDat, okTrial==1)


# m = lmer(data=filter(dat, captured=="fixated distracter", congC!="no distracter"), 
# 	RT ~ congC * thought + (congC * thought|observer), 
# 	control=lmerControl(optimizer="bobyqa"))
# ci95 = confint(m, method="boot")

# mdat = data.frame(effect=c("incongruent C", "incorrectly thought direct", "interaction"),
# 	estimate=fixef(m)[2:4], 
# 	lower=c(ci95["congCincongruent",1],ci95["thoughtdirect",1], ci95["congCincongruent:thoughtdirect",1]),
# 	upper=c(ci95["congCincongruent",2],ci95["thoughtdirect",2], ci95["congCincongruent:thoughtdirect",2]))

# mplt = ggplot(mdat, aes(x=effect, y=estimate, ymin=lower, ymax=upper))
# mplt = mplt + geom_point() + geom_errorbar()
# mplt = mplt + theme_bw()
# mplt = mplt +scale_y_continuous(name="estimate effect on RT (seconds)")
# ggsave("../plots/modelFit.pdf")

#  remove some outliers - for now, worst 1% of data
# dat = filter(dat, RT<= quantile(dat$RT, 0.99))
dat = select(dat, -distracter)
adat  = (filter(dat, congC!="no distracter")
		%>% group_by(observer, thought, captured, congC) 
		%>% summarise(
			medianRT=median(RT),
			nTrials = length(RT)))


plt = ggplot(adat, aes(x=observer, y=nTrials, fill=congC))+geom_bar(stat="identity", position=position_dodge())
plt = plt + facet_grid(captured~thought)
plt = plt + theme_light()
plt = plt + scale_y_continuous(name="number of trials")
ggsave("../plots/nTrialsByCondition.pdf", width=8, height=6)


# simple analysis - only take conditions in which we have > minN trials
minN = 15
adat$medianRT[adat$nTrials<minN] = NaN

plt = ggplot(filter(adat, captured!="none"), aes(x=observer, y=medianRT, fill=congC))+geom_bar(stat="identity", position=position_dodge())
plt = plt + facet_grid(captured~thought)
plt = plt + theme_bw()
plt = plt + scale_y_continuous(name="median RT")
ggsave("../plots/MedianRT.pdf", width=8, height=6)

adat = adat[complete.cases(adat),]



adat$congC = droplevels(adat$congC)
adat = filter(adat, captured == "fixated distracter")
adat$captured = droplevels(adat$captured)

adat$condition = paste(adat$congC, adat$thought)
wideDat =spread(select(ungroup(adat), observer, condition, medianRT), condition, medianRT)



m = aov(data=filter(adat, captured=="fixated distracter"), 
	medianRT ~ thought *congC  + Error(observer / (thought*congC)))
summary(m)

cmpPlot = ggplot(compDat, aes(x=observer, y=RT_diff))+geom_bar(stat="identity")
cmpPlot = cmpPlot + facet_grid(captured~thought)
cmpPlot = cmpPlot + theme_linedraw()
cmpPlot = cmpPlot + scale_y_continuous(name="median incongruent RT - median congruent RT")
ggsave("../plots/differenceInMedianRT.pdf")

# jsut take fixatedDistracter condition
compDat = filter(compDat, captured == "fixated distracter")


adat2  = (compDat 
		%>% group_by(captured, thought) 
		%>% summarise(
			meanRTdiff=mean(RT_diff), 
			nPeople=length(RT_diff),
			stder =sd(RT_diff)/sqrt(nPeople),
			lower = meanRTdiff-1.96*stder,
			upper = meanRTdiff+1.96*stder))

agPlt = ggplot(filter(adat2, captured!="none"), aes(x=captured, y=meanRTdiff, ymin=lower, ymax=upper, colour=thought))
agPlt = agPlt + geom_point() + geom_errorbar()
agPlt = agPlt + theme_light() + scale_y_continuous("mean incongruent - congruent RT")
agPlt = agPlt + scale_colour_brewer(palette = "Set1")
ggsave("../plots/meanRT.pdf", width=5, height=5)

# model <- lmer(RT ~ congC + captured + (thought + congC + captured | observer), data=filter(dat, congC!="no distracter"))

# model <- lmer(RT ~ congC * captured * thought + (thought + congC + captured | observer), filter(dat, congC!="no distracter"))