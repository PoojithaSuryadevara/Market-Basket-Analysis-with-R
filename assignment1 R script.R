#install packages
install.packages("arulesViz")
library(arules)
library(arulesViz)
# get and set directories.
getwd()
setwd("/users/poojitha/documents/data mining")
#read the transaction data and assigning it to class object.
FDGROC <- read.transactions("/users/poojitha/documents/data mining/transactions.txt",sep=",",format = "single",cols=c(1,2))
#summary of transaction data and most frequent items.
FDGROC
summary(FDGROC)
#view the observations.
head(FDGROC)
tail(FDGROC)
inspect(FDGROC[1:7])
#plot frequencies of frequent items 
itemFrequency(FDGROC[ ,1:7])
itemFrequencyPlot(FDGROC,support=0.10)
itemFrequencyPlot(FDGROC, top=10)
#apply apiori algorithm and generate the rules.
FD_RULES <- apriori(FDGROC, parameter =list(support=0.01,confidence=0.5,minlen=2))
inspect(FD_RULES[1:6])

#1.find the purchase patterns of Wine.

FD_RULES1 <- apriori(data=FDGROC,parameter =list(support=0.003,confidence=0.09,minlen=2),appearance  = list(rhs="Wine",default="lhs"),control = list(verbose=F) )
FD_RULES1 <- sort(FD_RULES1,decreasing=T,by="support")
inspect(FD_RULES1[390:396])
FD_RULES1 <- apriori(data=FDGROC,parameter =list(support=0.001,confidence=0.10,minlen=2),appearance  = list(lhs="Wine",default="rhs"),control = list(verbose=F) )
FD_RULES1 <- sort(FD_RULES1,decreasing=T,by="confidence")
inspect(FD_RULES1[15:20])
plot(FD_RULES1,method="graph",interactive = T)

#find the purchase patterns of Beer.
FD_RULES1 <- apriori(data=FDGROC,parameter =list(support=0.01,confidence=0.08),appearance  = list(rhs="Beer",default="lhs"),control = list(verbose=F) )
FD_RULES1 <- sort(FD_RULES1,decreasing=T,by="confidence")
inspect(FD_RULES1[10:16])

FD_RULES1 <- apriori(data=FDGROC,parameter =list(support=0.09,confidence=0.09),appearance  = list(lhs="Beer",default="rhs"),control = list(verbose=F) )
FD_RULES1 <- sort(FD_RULES1,decreasing=T,by="support")
inspect(FD_RULES1[8:13])
plot(FD_RULES1,method ="graph",interactive = T,shading ="confidence")



#2.purchase patterns for canned food vs fresh food
CANNED_RULES <- apriori(data=FDGROC, parameter= list (support=0.003, confidence=0.7),appearance  = list(rhs="Canned Fruit",default="lhs"),control = list(verbose=F))

summary(CANNED_RULES)
inspect(CANNED_RULES[1:7])
SUB_RULES <- subset(CANNED_RULES, subset=lhs %ain% c(" Canned Vegetables", "Fresh Fruits"))
plot(CANNED_RULES,method ="scatterplot",interactive = T, shading = "confidence")

FRESH_RULES <- apriori(data=FDGROC, parameter= list (support=0.006, confidence=0.7),appearance  = list(rhs ="Fresh Fruit",default="lhs"),control = list(verbose=F))
summary(FRESH_RULES)
inspect(FRESH_RULES[1:7])
plot(FRESH_RULES,method ="scatterplot",interactive = T, shading = "confidence")


plot(FRESH_RULES,method ="scatterplot",interactive=T)


#3.purchace patterns for large vs small transactions

LTRANS <- (FDGROC[size(FDGROC)>30])
inspect(LTRANS[1:5])
summary(LTRANS)
image(LTRANS, method="scatterplot", shading="confidence")


STRANS <-(FDGROC[size(FDGROC)<10])
 inspect(STRANS[1:7])
 summary(STRANS)




#4.purchase patterns for Aspirin.
 
PATTERN1 <- apriori(data=FDGROC, parameter= list(support=0.004,confidence=0.02), appearance = list(default="rhs", lhs="Aspirin"))
PATTERN1 <- sort(PATTERN1,decreasing=T,by="support")
inspect(PATTERN1[95:102])
summary(PATTERN1)


plot(PATTERN1,method="scatterplot", interactive=T,shading="confidence")

#5.purchase patterns for Batteries.
PATTERN2 <- apriori(data=FDGROC, parameter= list(support=0.004,confidence=0.02), appearance = list(default="rhs", lhs="Batteries"))
PATTERN2 <- sort(PATTERN2,decreasing=T,by="confidence")
inspect(PATTERN2[130:140])
plot(PATTERN2,method="matrix3D", interactive=T,shading="confidence")



