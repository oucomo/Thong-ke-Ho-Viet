install.packages(c("rcarbon", "truncnorm", "WaveletComp", "foreach", "doParallel"),
		 dependencies=TRUE)

library(rcarbon)
library(truncnorm)
library(WaveletComp)
library(foreach)
library(doParallel)

load("dataTJ2020.Rdata")

belo100 <- dataTJ2020[[1]]
belo200 <- dataTJ2020[[2]]
belo400 <- dataTJ2020[[3]]
belo800 <- dataTJ2020[[4]]

tpattern1 <- dataTJ2020[[5]]
tpattern2 <- dataTJ2020[[6]]
tpattern3 <- dataTJ2020[[7]]
tpattern4 <- dataTJ2020[[8]]

saaminor <- dataTJ2020[[9]]

## Calculate total population size
saaminor$sum <- rowSums(saaminor[, 4:5], na.rm=TRUE)
saaminor$sum[saaminor$sum==0] <- NA

## N. of taxpayers in historica Kemi Lappi region in N. Finland.
## Peltojärvi Sámi community is excluded.
saamifin <- dataTJ2020[[10]]
saamifin$sum <- rowSums(saamifin[,2:9], na.rm=TRUE)
saamifin$sum[saamifin$sum==0] <- NA

wpop <- dataTJ2020[[11]]

## Let's make it a little shorter
wpop <- wpop[wpop$age < 30000,]

t_col <- function(color, percent = 50, name = NULL) {

#	  color = color name
#	percent = % transparency
#	   name = an optional name for the color

## Get RGB values for named color
  rgb.val <- col2rgb(color)

## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
	       max = 255,
	       alpha = (100-percent)*255/100,
	       names = name)

## Save the color
  invisible(t.col)

  }
## END

ethnogr <- local({
     con <- textConnection(
       "\"Group\"	\"Growth rate (%)\"	\"Reference\"
\"Dobe !Kung\"	\"0.26\"	\"Howell, N., 2000\"
\"Agta\"	\"1.8\"	\"Early, J.D., Headland, T.N., 1998\"
\"Asmat\"	\"1.9\"	\"Van Arsdale, P.W., 1978.\"
\"Hadza\"	\"1.6\"	\"Blurton Jones, N.G., 2016.\"
\"Ache\"	\"2\"	\"Hill, K., Hurtado, A.M., 1996.\""
     )
     res <- utils::read.table(
       con,
       header    = TRUE,
       row.names = NULL,
       sep       = "\t",
       as.is     = TRUE
     )
     close(con)
     res
   })
archgr <- local({
     con <- textConnection(
       "\"Region\"	\"Growth rate (%)\"	\"Time range\"	\"Reference\"
\"Australia\"	\"0.045\"	\"40,000 - 0\"	\"Williams, A.N., 2013\"
\"Australia\"	\"0.04\"	\"5000 - 0\"	\"Johnson, C.N., Brook, B.W., 2011\"
\"Wyoming and Colorado\"	\"0.04\"	\"13,000 - 6000\"	\"Zahid, H.J. et al., 2016\"
\"South America\"	\"0.132\"	\"14,000 - 6000\"	\"Goldberg, A., et al. 2016\"
\"Kuril Islands\"	\"0.2\"	\"2500 - 2000\"	\"Fitzhugh, B., et al. 2016\"
\"Big Horn Basin (WY)\"	\"0.31\"	\"Several periods\"	\"Kelly, R.L., 2013\""
     )
     res <- utils::read.table(
       con,
       header    = TRUE,
       row.names = NULL,
       sep       = "\t",
       as.is     = TRUE
     )
     close(con)
     res
   })
samigr <- local({
     con <- textConnection(
       "\"Region\"	\"/Start date/\"	\"/End date/\"	\"/t/\"	\"/N_0/\"	\"/N_t/\"	\"/r (%)/\"
\"N. Norway\"	\"1559\"	\"1602\"	\"43\"	\"137\"	\"302\"	\"1.84\"
\"N. Norway\"	\"1679\"	\"1752\"	\"73\"	\"106\"	\"523\"	\"2.19\"
\"N. Finland\"	\"1555\"	\"1605\"	\"50\"	\"72\"	\"111\"	\"0.87\"
\"N. Finland\"	\"1620\"	\"1645\"	\"25\"	\"85\"	\"188\"	\"3.18\"
\"N. Finland\"	\"1655\"	\"1692\"	\"37\"	\"86\"	\"229\"	\"2.65\""
     )
     res <- utils::read.table(
       con,
       header    = TRUE,
       row.names = NULL,
       sep       = "\t",
       as.is     = TRUE
     )
     close(con)
     res
   })
## Population growth rates in ethnographic data
par(cex.axis=0.7)
par(fig=c(0,0.35,0.60,1), mar=c(3,4,1,3))
plot(NA, xlim=c(0.5,2.5), ylim=c(0.01, 10), xlab="Index", ylab="",
     log="y", yaxt="n", xaxt="n")
text(-0.6, 0.3, "Population growth rate (%)", xpd=NA, srt=90)
at.y <- outer(1:9, 10^(-2:1))
lab.y <- ifelse(log10(at.y) %% 1 == 0, at.y, NA)
abline(h=at.y, col="lightgrey", lty=2)
axis(2, at=at.y, labels=lab.y, las=1, tcl=-0.3, mgp=c(3, 0.5,0))
text(-1.1, 17, "(a)", xpd=NA)

## Population growth rates in archaeological data
stripchart(ethnogr[,2], method="jitter", jitter=0.1, vertical=TRUE, pch=21,
	   cex=1.5, bg=t_col("#FAAB18",40), col="black", add=TRUE, at=1)
text(1, 0.002, "ethno", xpd=NA, srt=90)
stripchart(archgr[,2], method="jitter", jitter=0.15, vertical=TRUE, pch=21,
	   cex=1.5, bg=t_col("#588300", 40), col="black", add=TRUE, at=2)
text(2, 0.002, "arch", xpd=NA, srt=90)

## Simulated H-G population dynamics according to Belovsky (1988) 
par(fig=c(0.33,0.66,0.79,0.99), mar=c(1,2,0,1), new=TRUE)
plot(belo100, type="l", las=1, axes=FALSE, xlab="", ylab="", ylim=c(0,4))
axis(side=2, at=seq(0,4,1), las=2, tcl=-0.3, mgp=c(3, 0.5,0))
abline(h=mean(belo100$density, na.rm=TRUE))
text(250,2, "0.8%", cex=0.7)
text(150, 3.8, "Productivity 100 g/m2", cex=0.7, font=2)
text(-90, 0, "Population density (#/100km2)", xpd=NA, srt=90)
text(-136, 4, "(b)", xpd=NA)

par(fig=c(0.66,0.99,0.79,0.99), mar=c(1,2,0,1), new=TRUE)
plot(belo200, type="l", las=1, axes=FALSE, xlab="", ylab="", ylim=c(0,15))
axis(side=2, at=seq(0,15,5), las=2, tcl=-0.3, mgp=c(3, 0.5,0))
abline(h=mean(belo200$density[belo200$time > 70], na.rm=TRUE))
text(250,4, "1.2%", cex=0.7)
text(150, 14, "Productivity 200 g/m2", cex=0.7, font=2)

par(fig=c(0.33,0.66,0.6,0.8), mar=c(1,2,0,1), new=TRUE)
plot(belo400, type="l", las=1, axes=FALSE,  xlab="", ylab="", ylim=c(0,70))
axis(side=2, at=seq(0,60,20), las=2, tcl=-0.3, mgp=c(3, 0.5,0))
axis(side=1, at=seq(0,300, 50), tcl=-0.3, mgp=c(3, 0.5,0))
abline(h=mean(belo400$density[belo400$time > 70], na.rm=TRUE))
text(75,10, "3%", cex=0.7)
text(200,10,"0.4%", cex=0.7)
text(150, 66, "Productivity 400 g/m2", cex=0.7, font=2)
text(350, -30, "Time in years", xpd=NA)

par(fig=c(0.66,0.99,0.6,0.8), mar=c(1,2,0,1), new=TRUE)
plot(belo800, type="l", las=1, axes=FALSE, xlab="", ylab="", ylim=c(0,200))
axis(side=2, at=seq(0,160,40), las=2, tcl=-0.3, mgp=c(3, 0.5,0))
axis(side=1, at=seq(0,300, 50), tcl=-0.3, mgp=c(3, 0.5,0))
abline(h=mean(belo800$density[belo800$time > 90], na.rm=TRUE))
text(200,10,"3%", cex=0.7)
text(150, 190, "Productivity 800 g/m2", cex=0.7, font=2)

## Changes in Sámi population size in Northern Norway
par(fig=c(0,1,0.25,0.55),mar=c(3,5,1,1), new=TRUE)
plot(saaminor$year, saaminor$sum, type="n", lwd=1.7, las=1,xaxt="n", yaxt="n",
     xlim=c(1550,1750), xlab="", ylab="Population size")
abline(h=seq(50,500,50), v=seq(1550,1750,10), col="darkgrey", lty=3)
lines(saaminor$year, saaminor$sum, lwd=2, col="#990000")
axis(side=2, at=seq(50,500,50), las=2, tcl=-0.3)
text(1580, 300, paste(samigr[1, 7],"%"), cex=0.7)
text(1700, 350, paste(samigr[2, 7],"%"), cex=0.7)
text(1570,450, "N. Norway", cex=1)
text(1507, 520, "(c)", xpd=NA)

## Changes in the number of Sámi taxpayers in Northern Finland
par(fig=c(0,1,0.05,0.35), new=TRUE)
plot(saamifin$year, saamifin$sum, type="n", lwd=1.7, las=1, xlim=c(1550,1750),yaxt="n",
     xaxt="n", xlab="", ylab="N. of taxpayers")
mtext("Years (AD)", side=1, line=2, cex=1)
abline(h=seq(80,220,20), v=seq(1550,1750,10), col="darkgrey", lty=3)
lines(saamifin$year, saamifin$sum, type="l", cex=1, pch=19, lwd=2, col="#1380A1")
axis(side=1, at=seq(1550,1750,10), lab=rep("",21), tcl=-0.3)
axis(side=2, at=seq(80,220,20), las=2, tcl=-0.3)
axis(side=1, at=seq(1550,1750,50), tcl=-0.4)
axis(side=1, at=seq(1550,1750,10), lab=rep("",21), tcl=-0.3)
text(1580, 120, paste(samigr[3, 7],"%"), cex=0.7)
text(1620, 150, paste(samigr[4, 7],"%"), cex=0.7)
text(1670, 185, paste(samigr[5, 7],"%"), cex=0.7)
text(1570,200, "N. Finland", cex=1)
text(1507, 230, "(d)", xpd=NA)

## Parameter values for the simulation
startyr <- 10000
yearRange1 <- c(startyr,startyr - length(tpattern1)+1)
ndates1 <- length(yearRange1[1]:yearRange1[2])
dnorm <- TRUE
dnormspd <- TRUE
spdnorm <- TRUE
pweights1 <- tpattern1/sum(tpattern1) ## Temporal distribution follows tpattern1     
samplesize <- 5000 ## Number of calendar dates in each sample
error <- round(rtruncnorm(samplesize, a=20, b=80, mean = 50, sd = 15),0) 
nsim <- 9 ## Number of samples/simulation rounds
sim1 <- matrix(NA,nrow=ndates1,ncol=nsim)
n_cores <- detectCores()/2 ## parallel processing will use half of the available cores

cl <- makeCluster(n_cores)
registerDoParallel(cl)

sim1 <- foreach(i = 1:nsim,
	       export=c(yearRange1, samplesize, pweights1, error, dnorm, dnormspd, spdnorm),
	       .packages="rcarbon", .combine=cbind) %dopar% {
		 randomDates1.1<-round(sample(yearRange1[1]:yearRange1[2],size=samplesize,
					      replace=TRUE, prob=pweights1))
		 randomSDs1<-sample(size=length(randomDates1.1),error,replace=TRUE)
		 randomDates1.2<-uncalibrate(randomDates1.1)$rCRA
		 calibdates1 <- calibrate(x=randomDates1.2, errors=randomSDs1,
					  timeRange=yearRange1,
					  calCurves='intcal13', normalised=dnorm,
					  ncores = n_cores, verbose = FALSE)
		 spd(calibdates1, timeRange=yearRange1, datenormalised = dnormspd,
		     spdnormalised = spdnorm, verbose = FALSE)$grid$PrDens
	       }

stopCluster(cl)

res <- as.data.frame(sim1) # matrix as data frame
res$date <- yearRange1[1]:yearRange1[2] # add time variable
res$tpattern <- tpattern1 # add the underlying temporal pattern to the dataset

yearRange2 <- c(startyr,startyr - length(tpattern3)+1)
pweights2 <- tpattern3/sum(tpattern3)

randomDates2.1<-round(sample(yearRange2[1]:yearRange2[2],size=samplesize,replace=TRUE,
			   prob=pweights2))
randomSDs2<-sample(size=length(randomDates2.1),error,replace=TRUE)
randomDates2.2<-uncalibrate(randomDates2.1)$rCRA
calibdates2 <- calibrate(x=randomDates2.2, errors=randomSDs2, timeRange=yearRange2,
			 calCurves='intcal13', normalised=dnorm, ncore=n_cores, verbose=FALSE)
sim2 <- as.data.frame(spd(calibdates2, timeRange=yearRange2, datenormalised = dnormspd,
			  spdnormalised = spdnorm, verbose=FALSE)$grid)

yearRange3 <- c(startyr,startyr - length(tpattern2)+1)
pweights3 <- tpattern2/sum(tpattern2)

randomDates3.1<-round(sample(yearRange3[1]:yearRange3[2],size=samplesize,
			   replace=TRUE, prob=pweights3))
randomSDs3<-sample(size=length(randomDates3.1),error,replace=TRUE)
randomDates3.2<-uncalibrate(randomDates3.1)$rCRA
calibdates3 <- calibrate(x=randomDates3.2, errors=randomSDs3, timeRange=yearRange3,
			calCurves='intcal13', normalised=dnorm, ncores=n_cores, verbose=FALSE)
sim3 <- as.data.frame(spd(calibdates3, timeRange=yearRange3, datenormalised = dnormspd,
			  spdnormalised = spdnorm, verbose=FALSE)$grid)

## Exponential fit
## Add new time variable "yr" to the simulated data just for calculating the exp fit.
## This is only to get positive coefficient
res$yr <- 1:4120 

## Fitting the exponential model to the firts sample
## of the simulated data
expfit1 <- lm(log(res[,1]) ~ yr, data=res)
grSIM1 <- summary(expfit1)$coefficients[[2]]

# Predicted 
exprob1 <- exp(predict(expfit1))

## "Growth rate" for environmental productivity 
n0 <- 100 # productivity at the beginning
nT <- 800 # productivity in the end
t <- 3000 # time, 9500-6500 yrs ago
grEP <- log(nT/n0)/t

## Exponential fits for the fig 2c-d

## Adding time variables just to get calculate positive growth rates
sim2$yr <- 1:nrow(sim2) 
sim3$yr <- 1:nrow(sim3)

fitSim2 <- lm(log(PrDens) ~ yr, data=sim2)
grSIM2 <-  summary(fitSim2)$coefficients[[2]]
exprobSim2 <- exp(predict(fitSim2))

fitSim3 <- lm(log(PrDens) ~ yr, data=sim3)
grSIM3 <- summary(fitSim3)$coefficients[[2]]
exprobSim3 <- exp(predict(fitSim3))

par(mar=c(3,5,1,5), mfcol=c(2,2), cex.lab=1)
##par(fig=c(0,1,0.45,0.95), mar=c(3,5,1,1))
plot(res$date*-1, res$tpattern, type="l", xlim=c(-10000,-5800),
     ylab="Population density (#/100km2)", las=1, xaxt="n", yaxt="n")
text(-10000, 130, "(a)")
axis(1, at=seq(-10000,-6000, by=1000), lab=seq(10000,6000, by=-1000), tcl=-0.3)
axis(2, at=seq(0,140, by=20), las=2, hadj=0.7, tcl=-0.3)

##par(fig=c(0,1,0.02,0.52),new=TRUE, mar=c(3,5,1,1))
plot(res$date*-1, res[, 1], type="l", xlim=c(-10000,-5800),
     xaxt="n", yaxt="n", xlab="", ylab="Summed probability")
text(-10000, 0.00058, "(b)")
mtext( "Years ago", side=1, line=2)
axis(1, at=seq(-10000,-6000, by=1000), lab=seq(10000,6000, by=-1000), tcl=-0.3)
axis(2, at=seq(0e-00,8e-04, by=2e-04), lab=c("0.0000","0.0002","0.0004","0.0006", "0.0008"),
     las=2, hadj=0.7,  tcl=-0.3)
lines(res$date*-1, exprob1, col="red")
text(-9000, 0.0005, paste("Growth rate=", round(grSIM1*100, 2),"%"))
text(-8600, 0.00045, paste("Rate of ghange in env. prod=", round(grEP*100, 2),"%"))


##par(mar=c(3,5,1,5))
plot(sim2$calBP*-1, tpattern3, type="l", col="darkgrey",
     xlim=c(-10000,-7500), xaxt="n", yaxt="n", xlab="",
     ylab="Population density (#/100km2)", lwd=1.5)
text(-10000, 130, "(c)")
mtext("Summed probability", side=4, line=4, cex=1)
axis(side=2, at=seq(0,140,20), las=2, tcl=-0.3)
axis(side=1, at=seq(-10000,-7500, 500), lab=seq(10000,7500,-500),
     tcl=- 0.3)#, lab=c("10,000","9,500", "9,000", "8,500", "8,000", "7,500"))
par(new=TRUE)
plot(sim2$calBP*-1, sim2$PrDens, type="l",
     xlim=c(-10000,-7500), lwd=2, xaxt="n", yaxt="n", xlab="",
     ylab="", axes=FALSE)
axis(side=4, at=seq(0.0001,0.0004, 0.0001),
     lab=c("0.0001","0.0002","0.0003","0.0004"), las=2, tcl=-0.3)
text(-9000, 0.00015, paste("Growth rate=", round(grSIM2*100, 2), "%"))
lines(sim2$calBP*-1, exprobSim2, lwd=2, col="red")

plot(sim3$calBP*-1, tpattern2, type="l", col="darkgrey",
     xlim=c(-10000,-7500), yaxt="n", xaxt="n", xlab="",
     ylab="Population density (#/100km2)", lwd=1.5)
text(-10000, 130, "(d)")
mtext("Summed probability", side=4, line=4)
mtext("Years ago", side=1, line=2)
axis(side=2, at=seq(0,140,20), las=2, tcl=-0.3)
axis(side=1, at=seq(-10000,-7500, 500), lab=seq(10000,7500,-500), tcl=-0.3)
par(new=TRUE)
plot(sim3$calBP*-1, sim3$PrDens, type="l",
     xlim=c(-10000,-7500), lwd=2, xaxt="n",yaxt="n", xlab="", ylab="", axes=FALSE)
axis(side=4, at=seq(0,0.0012, 0.0002),las=2, tcl=-0.3)
text(-9200, 0.0003, paste("Growth rate=", round(grSIM3*100, 2),"%"))
lines(sim3$calBP*-1, exprobSim3, lwd=2, col="red")

## This is the regional long-term scale, i.e., archaeological proxy
## Requires some more simulation using yet another underlying pattern
## (tpattern4)

startyr2 <- 17000
yearRange4 <- c(startyr2,startyr2 - length(tpattern4)+1)
pweights4 <- tpattern4/sum(tpattern4)       

randomDates4.1<-round(sample(yearRange4[1]:yearRange4[2],size=samplesize,
			   replace=TRUE, prob=pweights4))       
randomSDs4<-sample(size=length(randomDates4.1),error,replace=TRUE)
randomDates4.2<-uncalibrate(randomDates4.1)$rCRA

calibdates4 <- calibrate(x=randomDates4.2, errors=randomSDs4, timeRange=yearRange4,
			 calCurves='intcal13', normalised=dnorm,
			 ncores=n_cores, verbose=FALSE)
simpal <- as.data.frame(spd(calibdates4, timeRange=yearRange4,
			    datenormalised = dnormspd, spdnormalised = spdnorm,
			    verbose=FALSE)$grid)

par(mfrow=c(3,1), mar=c(3,5,1,1))
## Global scale population growth based on Biraben 1979
plot(wpop$age*-1, wpop$popsize, type="l", xlim=c(-30000,0), axes=F,
     xlab="", ylab="Global population size (billions)")
axis(side=1, at=seq(-30000,0, 10000), lab=seq(30000,0,-10000),
     pos=-0.3e+09, tcl=-0.3, mgp=c(3,0.5,0))
axis(side=1, at=c(seq(-25000,-5000, 10000)), lab=rep("",3),
     tck=-0.03, pos=-0.3e+09, tcl=-0.2)
axis(side=2, at=seq(0,8e+09, 2e+09), lab=seq(0,8,2), las=2, tcl=-0.3)
axis(side=2, at=seq(1e+09,7e+09, 2e+09), lab=rep("",4), las=2, tcl=-0.2)
text(-30000,  7.5e+09, "(a)")

## Plotting the regional long-term scale
plot(simpal$calBP*-1, simpal$PrDens, type="l", axes=FALSE,
     ylab="Summed probability", xlab="", xlim=c(-17000,-11000))
## mtext("Summed probability", side=2, line=3, cex=1)
axis(1, at=seq(-17000,-11000, by=1000), lab=seq(17000,11000, by=-1000),
     tcl=-0.3, mgp=c(3,0.5,0))
axis(1, at=seq(-16500,-11500, by=1000), lab=rep("",6), tcl=-0.2, mgp=c(3,0.5,0))
axis(2, at=seq(0e-00,4e-04, by=1e-04), las=2, hadj=0.7, tcl=-0.3)
text(-17000,  3.5e-04, "(b)")

## And finally te "true" population dynamics
plot(simpal$calBP*-1, tpattern4, type="l", axes=F, ylab="Population density (#/100km2)",
     xlab="", xlim=c(-14700,-13200), ylim=c(0,150))
mtext("Years ago", side=1, line=2)
## mtext("Population density (#/100km2)", side=2, line=3, cex=1)
axis(1, at=seq(-14700,-13200, by=300), lab=seq(14700,13200, by=-300),
     tcl=-0.3, mgp=c(3,0.5,0))
axis(1, at=seq(-14550,-13350, by=300), lab=rep("",5), tcl=-0.2, mgp=c(3,0.5,0))
axis(2, at=seq(0,140, by=10), las=2, hadj=0.7, tcl=-0.3)
text(-14700,  125, "(c)")

## Plotting individual Saami communities (excluding Peltojärvi)

## Vector of individual communities in 'correct order'
communities <- c("Inari", "Kittila", "Sodankyla", "Sompio", "Kemikyla", "Kuolajarvi",
		 "Kitkajarvi", "Maanselka")
commNames <- c("Inari", "Kittiä", "Sodankylä", "Sompio", "Kemikylä", "Kuolajärvi",
	       "Kitkajärvi", "Maanselkä")

## For loop to create a multipanel plot
par(mfrow=c(4,2), mar=c(4,4,1,1))
for(i in 1:length(communities)){
  plot(saamifin$year, saamifin[,communities[i]], type="l", xlab="", ylab="", col="black",
       lwd=1, las=1, xaxt="n", tcl=-0.3, main=commNames[i])
  axis(side=1, at=seq(1550, 1700,50), tcl=-0.3)
  axis(side=1, at=seq(1550, 1700, 10), lab=rep("", length(seq(1550, 1700,10))), tcl=-0.2)
}
text(1530, -10, "Year AD", xpd=NA)
text(1330, 75, "N. of taxpayers", xpd=NA, srt=90)

## First, a layout with the top panel having only the
## underlying simulated population dynamics (tpattern)
layout(matrix(c(1,0,0,
		2,3,4,
		5,6,7,
		8,9,10), 4,3, byrow = TRUE))

## Plotting the underlying dynamics
par(mar=c(3,4,1,1))
plot(res$date*-1, res$tpattern, type="l", xlim=c(-10000,-5800),
xaxt="n", xlab="", ylab="Population density (#/100km2)", las=2)
axis(1, at=seq(-10000,-6000, by=1000), lab=seq(10000,6000, by=-1000), tck=-0.04, mgp=c(3,0.2,0))

## Plotting the rest using for loop
for(i in 1:9){
  plot(res$date*-1, res[,i], type="l", ylim=c(0,0.0008), xlim=c(-10000,-5800),
       xaxt="n", yaxt="n", xlab="", ylab="")
  axis(1, at=seq(-10000,-6000, by=1000), lab=seq(10000,6000, by=-1000),
       tck=-0.04, mgp=c(3,0.2,0))
  axis(2, at=seq(0e-00,8e-04, by=2e-04), las=2, hadj=0.7, tcl=-0.2)
}

## Adding axis labels
text(-13800, -2.2e-04, "Years ago", xpd=NA)
text(-23000, 2.2e-03, "Summed probability", xpd=NA, srt=90)

cl <- makeCluster(n_cores)
registerDoParallel(cl)

## strt<-Sys.time()
    wc_list <- foreach(i = 1:9, .packages="WaveletComp") %dopar% {
	analyze.coherency(res, my.pair=c(i, 11),
			  loess.span=1, n.sim=100, window.type.t=3, window.type.s=3,
			  window.size.t=1, window.size.s=1, verbose=FALSE)
    }

stopCluster(cl)

## print(Sys.time()-strt)

# Plotting the wavelet coherence

par(mfrow=c(3,3), mar=c(4,5,1,5))
for(i in 1:9){
    wc.image(wc_list[[i]], which.image="wc", color.key="interval",
	 spec.time.axis=list(at=c(1,1000,2000,3000,4000), labels=c(10000,9000,8000,7000,6000)),
	 periodlab="", plot.ridge=FALSE, graphics.reset=FALSE,
	 legend.params = list(width=1.2, shrink = 0.9, mar = 12.1,
			      n.ticks = 6, label.digits = 1,
			      label.format = "f",lab = NULL,
			      lab.line = 1.5))
}

text(-17200, 19, "Period", xpd=NA, srt=90, cex=1.5)
text(5900, 19, "Wavelet coherence level", xpd=NA, srt=-90, cex=1.5)
text(-4300, -1, "Years ago", xpd=NA, cex=1.5)
