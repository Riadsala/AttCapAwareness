library(ggplot2)
library(dplyr)
library(colorspace)
options(digits=3)

fDat = read.csv("fixations.csv")
rDat = read.csv("response.csv")
fDat$observer = factor(fDat$observer)
rDat$observer = as.factor(rDat$observer)

aDat = aggregate(targDiscrim~observer+congC, rDat, "mean")

plt = ggplot(aDat, aes(x=congC, y=targDiscrim, fill=observer))
plt = plt + geom_bar(stat="identity", position=position_dodge())
plt = plt + scale_y_continuous(name="proportion correct",  limits=c(0,1), expand=c(0,0))
plt = plt + scale_x_discrete(name="condition")
plt = plt + theme_bw()
ggsave("../graphs/accPlot.pdf", height=5, width=5)


#  classify trial
dat$type = 'error'
dat$type[as.logical(dat$lengthOK) & (dat$lookedAtTarg==TRUE)] = "direct"
# dat$type[dat$pathLength>1.2] = "tooLong"
# dat$type[dat$lookedAtDist==TRUE] = "fixatedDistracter"

dat$type = as.factor(dat$type)

dat$thought = as.factor(dat$thought)
levels(dat$thought) = c("bad", "good")
levels(dat$observer) = c("1", "2", "3", "4", "5", "6")

# plot basic results for distracter trials. 
plt = ggplot(filter(dat, distracter==1), aes(x=type, fill=thought)) + geom_bar(stat="count") + facet_grid(~observer)
plt = plt + theme_bw() + scale_y_continuous(name="number of trials") + scale_x_discrete(name=" ")
plt = plt + theme(legend.position="top") + scale_fill_discrete(name="responded that the trial was:")
plt = plt + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plt = plt + scale_fill_brewer(type="seq", palette=3, name="response", direction=-1)
ggsave("../graphs/capturedAndThoughtA.pdf", width=10, height=4)
ggsave("../graphs/capturedAndThoughtA.png", width=10, height=4)




# calculate prec and recall for each person
dat_pr = data.frame(person=character(), stat=factor(levels=c('accuracy', 'precision', 'rec')), val=numeric())

for (person in levels(dat$observer))
{
	pdat = dat[which(dat$observer==person),]
	prec = sum(pdat$type=="error" & pdat$thought=="bad")/sum(pdat$thought=="bad")
	recall = sum(pdat$type=="error" & pdat$thought=="bad")/sum(pdat$type=="error")
	acc = mean((pdat$type=="error") == (pdat$thought=="bad"))
	
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="precision", val=prec))
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="recall", val=recall))
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="F1", val=2*prec*recall/(prec+recall)))
	dat_pr= rbind(dat_pr, data.frame(person=person, stat="accuracy", val=acc))
}

dat_pr$stat = factor(dat_pr$stat, levels=c('accuracy', 'precision', 'recall', "F1"))
plt = ggplot(dat_pr, aes(x=stat, y=val)) + geom_boxplot(fill="grey")
plt = plt + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
ggsave("../graphs/f1score.pdf", height=5, width=5)
ggsave("../graphs/f1score.png", height=5, width=5)
