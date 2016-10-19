library(dplyr)

removeBadTrials <- function(rDat)
{
	# look at accuracy before removing incorrect trials
	accDat = aggregate(targDiscrim ~ observer + congC, rDat, "mean")
	print(aggregate(targDiscrim~congC, accDat, "mean"))
	print(aggregate(targDiscrim~observer, accDat, "mean"))
	
	# remove outlier person
	pAccDat = aggregate(targDiscrim~observer, accDat, "mean")
	print(pAccDat)
	# rDat = filter(rDat, observer!=pAccDat$observer[pAccDat$targDiscrim<0.8])
	# fDat = filter(fDat, observer!=pAccDat$observer[pAccDat$targDiscrim<0.8])
	write.csv(aggregate(targDiscrim~observer+congC, rDat, "mean"), "accuracy.txt")
	# remove all remaining incorrect trials
	rDat = filter(rDat, targDiscrim==1)
	# only take trials with target fixation
	rDat = filter(rDat, lookedAtTarg)

	return(rDat)

}
