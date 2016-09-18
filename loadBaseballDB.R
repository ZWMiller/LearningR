require(stats)
setwd("/Users/zach/Documents/LearningR/scripts")
data <- read.csv("/Users/zach/Documents/LearningR/baseballDatabase/baseballdatabank-master/core/Batting.csv")
playerID <- data$playerID
yearID <- data$yearID
stint <- data$stint
teamID <- data$teamID
lgID <- data$lgID
G <- data$G
AB <- data$AB
R <- data$R
H <- data$H
X2B <- data$X2B
X3B <- data$X3B
HR <- data$HR
RBI <- data$RBI
SB <- data$SB
CS <- data$CS
BB <- data$BB
SO <- data$SO
IBB <- data$IBB
HBP <- data$HBP
SH <- data$SH
SF <- data$SF
GIDP <- data$GIDP
data$AVG <- data$H/data$AB
data$OBP <- (data$H+data$BB+data$IBB+data$HBP)/(data$AB+data$BB+data$IBB+data$HBP)
data$ISO <- (data$X2B+2*data$X3B+3*data$HR)/data$AB



#Example subset
data.sub <- subset(data, yearID >= 1990 & yearID < 2005 & AB > 50)
data.sub2 <- subset(data, yearID >= 1975 & yearID < 1990 & AB > 50)
data.skim <- subset(data, AB+BB+HBP > 100)
OBP.new <- data.sub$OBP
AVG.new <- data.sub$AVG
ISO.new <- data.sub$ISO
OBP.old <- data.sub2$OBP
AVG.old <- data.sub2$AVG
ISO.old <- data.sub2$ISO

# Make and save some plots!
pdf("battingStats.pdf")
df <- data.frame(AVG.new,OBP.new)
plot(df, main="MLB OBP vs AVG 1990-2005\nIndiv. Season, Min. 50 AB", xlab="AVG", ylab="OBP", xlim=c(0,0.5), ylim=c(0,0.7))
df.modl <- lm(OBP.new ~ AVG.new, data=df)
coef.new <- round(coef(df.modl),3)
eq.new <- paste0("OBP = ", coef.new[2], "*AVG + ", coef.new[1])
mtext(eq.new, 3, line=-2)
abline(df.modl, col="red", lwd=2)

df2 <- data.frame(AVG.old,OBP.old)
plot(df2, main="MLB OBP vs AVG 1975-1990\nIndiv. Season, Min. 50 AB", xlab="AVG", ylab="OBP", xlim=c(0,0.5), ylim=c(0,0.7))
df2.modl <- lm(OBP.old ~ AVG.old, data=df2)
coef.old <- round(coef(df2.modl),3)
eq.old <- paste0("OBP = ", coef.old[2], "*AVG + ", coef.old[1])
mtext(eq.old, 3, line=-2)
abline(df2.modl, col="red", lwd=2)

h.new<-hist(ISO.new, breaks=25, plot=F)
h.new$density = h.new$counts/sum(h.new$counts)*100
h.old<-hist(ISO.old, breaks=25, plot=F)
h.old$density = h.old$counts/sum(h.old$counts)*100
plot(h.new, main="MLB ISO 1990-2005\nIndiv. Season, Min. 50 AB", xlab="ISO", ylab="Percentage (%)", xlim=c(0,0.5), freq=F)
plot(h.old, main="MLB ISO 1975-1990\nIndiv. Season, Min. 50 AB", xlab="ISO", ylab="Percentage (%)", xlim=c(0,0.5), freq=F)

# Aggregate Example
data3.avg<-aggregate(AVG~yearID,data.skim,mean)
data3.obp<-aggregate(OBP~yearID,data.skim,mean)
data3.iso<-aggregate(ISO~yearID,data.skim,mean)
plot(data3.avg$yearID,data3.avg$AVG,col="red",type="o",xlab="Year",ylab="<AVG>",main="Mean Batting Average vs Year\n Min. 100 PA", ylim=c(0,0.5), xlim=c(1880,2020))
plot(data3.obp$yearID,data3.obp$OBP,col="red",type="o",xlab="Year",ylab="<OBP>",main="Mean On Base Percentage vs Year\n Min. 100 PA", ylim=c(0,0.5), xlim=c(1880,2020))
plot(data3.iso$yearID,data3.iso$ISO,col="red",type="o",xlab="Year",ylab="<ISO>",main="Mean Isolated Power vs Year\n Min. 100 PA", ylim=c(0,0.5), xlim=c(1880,2020))

dev.off()
