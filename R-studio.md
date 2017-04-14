# BSS-documents
library(agricolae)
download.packages(agricolae)
 d1 <- read.csv(file.choose(),header=TRUE, sep=";")
d1
summary(d1)
aov(d1$Percentage.of.words.corresponding.to.the.mood~d1$Design, data = d1)
a1 <- aov(d1$Percentage.of.words.corresponding.to.the.mood~d1$Design, data = d1)
a1

summary(a1)
class(d1$Design)

model.tables(a1, "means")
TukeyHSD(a1)
plot(a1)
d1$Design <- as.factor(d1$Design)
class(d1$Design)
TukeyHSD(a1)
plot(a1)
attach(d1)
names(d1)
class(Participants)
class(Happy)
levels(Participants)
boxplot(Happy~Participants)
d1
str(d1)
shapiro.test(d1$Neutral)
chisq.test(d1$Neutral)
boxplot (d1$Happy, d1$Sad, d1$Neutral, d1$Errors)

do.aov <- aov(d1$Neutral ~ d1$Participants)
summary(do.aov)
do1.aov <- aov(d1$Happy ~ d1$Participants)
summary(do1.aov)
do2.aov <- aov(d1$Sad ~ d1$Participants)
summary(do2.aov)
aov(Happy~Participants)

H1 = aov(Happy~Participants)
H1
S2 = aov(Sad~Participants)
S2
N3 = aov(Neutral~Participants)
N3

summary(H1)
attributes(H1)
coefficients(H1)
TukeyHSD(H1)

bc <- table()
bc  
barplot(d1)

da
summary(da)
pairwise.t.test( x = da, p.adjust.method = "none" )
pairwise.t.test(x=da)
posthoc <- TukeyHSD(x=da, 'd1$Percentage.of.words.corresponding.to.the.mood', conf.level=0.95)
TukeyHSD(da)
