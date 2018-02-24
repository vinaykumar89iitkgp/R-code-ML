
theurl<-"https://raw.githubusercontent.com/Mrinal4Github/LearnDataScience/master/MBA%20Starting%20Salaries%20Data.csv"
#reads it into a data frame from a csv format URL
proj2<-read.table(file=theurl,header=TRUE,",")

proj2_2=arrange(proj2,gmat_tot)
datapoints=nrow(proj2_2)

no_of_classes=round(1+3.322*log(datapoints))
Range=max(proj2_2[,"gmat_tot"])-min(proj2_2[,"gmat_tot"])
class_width=round(Range/no_of_classes)
modified_dataset=classifier_class_gmatcol(proj2_2,min(proj2_2[,"gmat_tot"]),max(proj2_2[,"gmat_tot"]),no_of_classes,class_width)
modified_dataset1=classifier_class_gmatcol_number(modified_dataset,min(proj2_2[,"gmat_tot"]),max(proj2_2[,"gmat_tot"]),no_of_classes,class_width)

#x=LETTERS[1:no_of_classes]
#classtable<-data.frame(x)
#classtable_new=mutate(classtable,"class_frequency"=0)
#x=table(modified_dataset$gmatclassifier)

#y=1
#while(y!=(no_of_classes)+1)
#{
#  classtable_new[y,"class_frequency"]<-nrow(filter(modified_dataset,gmatclassifier==LETTERS[y]))
#  y=y+1
#  print(y)
#}

x=modified_dataset1$gmatclassifier
hist(x,breaks=40,col="yellow")

y=table(modified_dataset1$gmatclassifier)
y1=cbind(y)
y2=cbind(cumsum(y1))
y3=cbind(y2,y1)
colnames(y3)<-c(fd,cfd)
y4=y2/nrow(proj2_2)
y3=cbind(y4,y2,y1)

barplot(y3[,1],main="Barplot of RCFD",col="yellow",xlab="Classes",ylab="Normalised cumulative frequency") #rfcd barplot
barplot(y3[,2],main="Barplot of CFD",col="yellow",xlab="Classes",ylab="Cumulative frequency") #cfd barplot
barplot(y3[,3],main="Barplot of FD",col="yellow",xlab="Classes",ylab="Frequency") #fd barplot

