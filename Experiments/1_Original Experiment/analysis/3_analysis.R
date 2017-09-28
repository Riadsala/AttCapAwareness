library(ggplot2)
library(dplyr)
library(colorspace)
options(digits=3)

fDat <- as.tibble(read.csv("fixations2.csv"))
dat <- as.tibble(read.csv("responses2.csv"))
dat$thoughtNoAttCap = factor(dat$thoughtNoAttCap, levels(dat$thoughtNoAttCap)[c(2,1)])


# plot basic results for distracter trials. 
dat = filter(dat, distracter==1)

plt = ggplot(dat, aes(x=type, fill=thoughtNoAttCap)) + geom_bar(stat="count") + facet_grid(~observer)
plt = plt + theme_bw() + scale_y_continuous(name="number of trials") + scale_x_discrete(name=" ")
plt = plt + theme(legend.position="top") 
plt = plt + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plt = plt + scale_fill_brewer(type="seq", palette=3, name="response:", direction=1)
ggsave("../graphs/capturedAndThoughtA.pdf", width=10, height=4)
ggsave("../graphs/capturedAndThoughtA.png", width=10, height=4)

# calculate prec and recall for each person
dat_pr = data.frame(person=character(), stat=factor(levels=c('accuracy', 'precision', 'rec', "F1")), val=numeric())
dat_error_recall =data.frame(person=character(), errorRate=numeric(), recall=numeric())

for (person in levels(dat$observer))
{
	pdat = dat[which(dat$observer==person),]
	prec = sum(pdat$type=="error" & pdat$thoughtNoAttCap=="no")/sum(pdat$thoughtNoAttCap=="no")
	recall = sum(pdat$type=="error" & pdat$thoughtNoAttCap=="no")/sum(pdat$type=="error")
	acc = mean((pdat$type=="error") == (pdat$thoughtNoAttCap==FALSE))
	
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="precision", val=prec))
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="recall", val=recall))
	# dat_pr = rbind(dat_pr, data.frame(person=person, stat="F1", val=2*prec*recall/(prec+recall)))
	dat_pr= rbind(dat_pr, data.frame(person=person, stat="accuracy", val=acc))

	dat_error_recall = rbind(dat_error_recall, data.frame(person=person, errorRate = mean(filter(pdat, distracter==1)$type=="error"), recall=recall))
}

dat_pr$stat = factor(dat_pr$stat, levels=c('accuracy', 'precision', 'recall', "F1"))

plt = ggplot(dat_pr, aes(x=stat, y=val)) + geom_boxplot(fill="grey")
plt = plt + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
ggsave("../graphs/f1score.pdf", height=5, width=5)
ggsave("../graphs/f1score.png", height=5, width=5)

plt = ggplot(dat_error_recall, aes(x=1-errorRate, y=recall))
plt = plt + geom_point() #+ geom_smooth(method=lm, colour="black")
plt = plt + theme_bw() + scale_x_continuous(name="proportion of distracter trials with direct eye movements")
ggsave("../graphs/recall_corr.png", height=5, width=5)
ggsave("../graphs/recall_corr.pdf", height=5, width=5)

#  now compute prec/recall by distracter distance


# calculate prec and recall for each person and distracer distance
dat_pr = data.frame(person=character(), dist = numeric(), stat=factor(levels=c('accuracy', 'precision', 'rec', "F1")), val=numeric())
dat_error_recall =data.frame(person=character(), errorRate=numeric(), recall=numeric())

for (person in levels(dat$observer))
{
	for (d in 1:3){
		pdat = dat[which(dat$observer==person & dat$distdist == d),]

		prec = sum(pdat$type=="error" & pdat$thoughtNoAttCap=="no")/sum(pdat$thoughtNoAttCap=="no")
		recall = sum(pdat$type=="error" & pdat$thoughtNoAttCap=="no")/sum(pdat$type=="error")
		acc = mean((pdat$type=="error") == (pdat$thoughtNoAttCap==FALSE))
		
		dat_pr = rbind(dat_pr, data.frame(person=person, dist = d, stat="precision", val=prec))
		dat_pr = rbind(dat_pr, data.frame(person=person, dist = d, stat="recall", val=recall))
		# dat_pr = rbind(dat_pr, data.frame(person=person, dist = d, stat="F1", val=2*prec*recall/(prec+recall)))
		dat_pr= rbind(dat_pr, data.frame(person=person, dist = d, stat="accuracy", val=acc))
	}
}
dat_pr$dist = as.factor(dat_pr$dist)
plt = ggplot(dat_pr, aes(x=stat, y=val, fill = dist)) + geom_boxplot()
plt = plt + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position="top")
plt = plt + scale_fill_brewer()
ggsave("../graphs/f1score2.pdf", height=5, width=5)
ggsave("../graphs/f1score2.png", height=5, width=5)