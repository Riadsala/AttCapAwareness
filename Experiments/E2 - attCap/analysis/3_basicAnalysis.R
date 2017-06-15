library(ggplot2)
library(dplyr)
library(colorspace)
options(digits=3)

fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("responseCapture.csv")
fDat$observer = factor(fDat$observer)
rDat$observer = factor(rDat$observer)


levels(rDat$observer) = 1:length(levels(rDat$observer))
levels(rDat$observer)[31:36] = c('a', 'b', 'c', 'd','e', 'f')

rDat$thought = factor(rDat$thought, levels(rDat$thought)[c(2,1)])

# plot basic results for distracter trials. 
plt = ggplot(filter(rDat, distracter==1), aes(x=type, fill=thought)) + geom_bar(stat="count") + facet_wrap(~observer, nrow=2)
plt = plt + theme_bw() + scale_y_continuous(name="number of trials") + scale_x_discrete(name=" ")
plt = plt + theme(legend.position="top") + scale_fill_discrete(name="responded that the trial was:")
plt = plt + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plt = plt + scale_fill_brewer(type="seq", palette=3, name="response", direction=1)
ggsave("../graphs/capturedAndThoughtA.pdf", width=10, height=4)
ggsave("../graphs/capturedAndThoughtA.png", width=10, height=4)

# calculate prec and recall for each person
dat_pr = data.frame(person=character(), stat=factor(levels=c('accuracy', 'precision', 'rec')), val=numeric())

rDat = filter(rDat, distracter==1)


for (person in levels(rDat$observer))
{
	pdat = rDat[which(rDat$observer==person),]
	prec = sum(pdat$type=="error" & pdat$thought=="no")/sum(pdat$thought=="no")
	recall = sum(pdat$type=="error" & pdat$thought=="no")/sum(pdat$type=="error")
	acc = mean((pdat$type=="error") == (pdat$thought==FALSE))
	
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


