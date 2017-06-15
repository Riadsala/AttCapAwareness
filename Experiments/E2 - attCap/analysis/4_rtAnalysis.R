library(ggplot2)
library(dplyr)
# library(tidyr)
library(scales)
library(lme4)
# library(brms)

fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("responseCapture.csv")

fDat$observer = as.factor(fDat$observer)
rDat$observer = as.factor(rDat$observer)

# reorder factor levels
rDat$congC = factor(rDat$congC, levels=levels(rDat$congC)[c(3,1,2)])	

# remove incorrect trials
source("removeBadTrials.R")
rDat = removeBadTrials(rDat)

###############################################################
# back to proper analysis now we've done SPSS output for students
###############################################################

# first look at no-distracter, and congruency RT effects
aDat = aggregate(RT~observer+congC, rDat, "median")
plt = ggplot(rDat, aes(x=congC, y=RT)) + geom_violin(fill="#79D0E1", draw_quantiles=c(0.25,0.5,0.75), size=0.75)
plt = plt + geom_point(data=aDat, aes(y=RT), colour="#9A4330", size=0.5)
plt = plt + geom_path(data=aDat, aes(x=congC, y=RT, group=observer), colour="#9A4330", size=0.25)
plt = plt + scale_x_discrete(name="distracter congruency")
plt = plt + scale_y_continuous(name="reaction time (ms)")
plt = plt + theme_bw()
ggsave("../graphs/congC_RT.png", width=5, height=5)
ggsave("../graphs/congC_RT.pdf", width=5, height=5)
rm(aDat)

# verify there is a congruency effect
# only look at distracter trials
rDat = filter(rDat, distracter==1)

m = lmer(data=rDat,RT ~ congC + (congC|observer))
ci = confint(m, method="boot")
# and vertify using log-transformed RT. 
mLog = lmer(data=rDat, log(RT) ~ congC + (congC|observer))
ci = confint(mLog, method="boot")

# now take only trials with a distracer fixation
rDat = filter(rDat, lookedAtDist)

# plot distribution of distracter dwell times
plt = ggplot(rDat , aes(x=distDwell, fill=thought)) + geom_density(alpha=0.5)
plt = plt + scale_x_continuous("log(distracter dwell time (ms))", expand=c(0,0), trans=log2_trans(), breaks=c(12.5, 25, 50,100, 150, 200, 300,400, 500))
plt = plt + scale_y_continuous(expand=c(0,0.01))
plt = plt + theme_bw() 
plt = plt + scale_fill_brewer(palette=3, name="response", direction=-1)
plt = plt + coord_trans(x="log2")
ggsave("../graphs/dwellTime.pdf")


#  calculate aRT  (additional response time)
rDat$aRT = rDat$RT - rDat$distDwell


rDat = droplevels(filter(rDat, aRT>0))

#  logisitic regression to predict correctly noticing Error
rDat$cong2 = -1
rDat$cong2[rDat$congC=="congruent"] = 1
m = glmer((thought=="no") ~ cong2 * scale(log(distDwell)) * scale(log(aRT)) 
	+ (1|observer), 
	data = rDat,
	family = "binomial")
ci = confint(m, method="boot")

#  congruency and awareness predicint aRT
m = lmer(aRT ~ congC * thought + (thought|observer), rDat)
ci = confint(m, method="boot")




# # is the congruency effect modulated by awareness?
# m = brm(data=dat, 
# 	RT-distDwell ~ congC*thought + (congC*thought|observer),
# 	family="normal",
# 	control = list(adapt_delta = 0.90))

# m = brm(data=dat, 
# 	as.numeric(thought) ~ congC*RT*distDwell + (congC*thought|observer),
# 	family="binomial")


# # look at RT distribution
# plt = ggplot(dat, aes(x=(RT-distDwell))) + geom_histogram()
# plt

m = lmer(data=dat,	(RT-distDwell) ~ congC * thought + (congC*thought|observer), 
	control=lmerControl(optimizer="bobyqa"))
ci95 = confint(m, method="boot")

mdat = data.frame(effect=c("incongruent C", "incorrectly thought direct", "interaction"),
	estimate=fixef(m)[2:4], 
	lower=c(ci95["congCincongruent",1],ci95["thoughtdirect",1], ci95["congCincongruent:thoughtdirect",1]),
	upper=c(ci95["congCincongruent",2],ci95["thoughtdirect",2], ci95["congCincongruent:thoughtdirect",2]))

mplt = ggplot(mdat, aes(x=effect, y=estimate, ymin=lower, ymax=upper))
mplt = mplt + geom_point() + geom_errorbar()
mplt = mplt + theme_bw()
mplt = mplt +scale_y_continuous(name="estimate effect on RT (seconds)")
ggsave("../graphs/modelFit.pdf")



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