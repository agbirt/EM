

###TO CLEAR THE GLOBAL ENVIRONMENT
rm(list = ls())


###LOAD LIBRARIES
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(colorRamps)


######GLOBALS######
inputpath <- 'E:/Raw Data- 170216/Base/Base 1.0/BASE_100_ES1_2016_10_200/' 

#desired analysis time
starttime <- -2
endtime <- 22

emission_scenario <- "" 
year <- ""
time_aggregation <- ""
space_aggregation <- ""
var <- ""
resulttable <- data.frame()
######END GLOBALS######


###GET DETAILS FROM FOLDER SCENARIO
getanalysisdetails <- function(path) 
{
  foldername <<- basename(path)
  scenariodetails_vector <- c(unlist(strsplit(foldername,'_')))
  strategy <<- scenariodetails_vector[1]
  demand <<- scenariodetails_vector[2]
  emission_scenario <<- scenariodetails_vector[3]
  year <<- scenariodetails_vector[4]
  time_aggregation <<- scenariodetails_vector[5]
  space_aggregation <<- scenariodetails_vector[6]
}


setup_resultstable<-function(variable)
{
  xvariable <- list()
  resulttable <<- data.frame(xvariable, xvariable, xvariable, xvariable)
}
  


###EL PASO DIFFERENCE ANALYSIS
Fulltiffanalysis<- function(variable)
{
  for (i in 1 : timelength)
  {
    time <- i*as.numeric(time_aggregation)
    date <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
    date <- date+(time*60) - 2*60*60
    date <- format(date,"%H:%M")
    
    filename<-paste(foldername,"_", variable, "_", time, sep='')
    str_name<-paste(inputpath, filename,".tif",sep='')
    
    ras<-raster(str_name)
    
    ras_table <- as.data.frame(ras,xy=TRUE)
    ras_table <- subset(ras_table, ras_table[,3]>=0)
    
    subresulttable <- cbind(time,date,ras_table)
    colnames(subresulttable) <- c("Time (min)", "Time (hr:min)", "X", "Y",variable)
    resulttable <<- rbind(resulttable,subresulttable)
  }
  outtablename <- paste(outpath,"/", variable, "_distribution.txt", sep = "")
  write.table(resulttable, outtablename, sep="\t", row.names = FALSE)
} 


graphdetails <- function(variable)
{
  if (variable == "CO2")
  {
    yscale <<- 1
    minval1<<- 0.00
    maxval1<<- 1000000
    ylabel <<- "CO2 (grams)"
    Plot1_ylabel <<- "CO2 (grams)"
  }
  else if (variable == "SPEED")
  {
    yscale <<- 1
    minval1<<- 0.00
    maxval1<<- 70
    ylabel <<- "Speed (mph)"
    Plot1_ylabel <<- "Total"
    
  }
  else if (variable == "VMT")
  {
    yscale <<- 1000
    minval1<<- 0.00
    maxval1<<- 1000
    ylabel <<- "VMT (*1000)"
    Plot1_ylabel <<- "Total"
  }
}

outputgraph <- function(variable)
{
    outgraphname <- paste(outpath,"/", variable, "_distribution.png", sep = "")
    title <- paste(variable," @ El Paso", sep="")
    xaxistitle = "Time"
    
    timelocs <- seq(0, endtime-starttime, by=4)*60
    timeset <- seq(starttime,endtime, by=4)*60
    date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
    date1 <- date1+(timeset*60)
    timelabels <- format(date1,"%H:%M")
    
    png(outgraphname,width = 4, height = 4, units = 'in', res = 600)
    par(oma=c(1,1.5,1,1),mar=c(2,1,1,1),mfrow=c(1,1))
    #bottom, left, top, right
   
    plot(resulttable[1:nrow(resulttable),1],resulttable[1:nrow(resulttable),5], xlab = "", ylim=c(0,1000000),ylab=Plot1_ylabel ,type="p", cex=0.01,col="red", xaxt = "n", yaxt = "n")
    #lines(sumtable[1:timelength,1],sumtable[1:timelength,4],xlab="Time",ylab=variable, col="green")
    mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
    axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
    axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.2, 3.2, 2.2, 3.2), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
    mtext(text=ylabel,side=2,line=2.2,outer=FALSE, cex = 0.5)
    mtext(text="Time",side=1,line=1.2,outer=FALSE, cex = 0.5)
    title(main=title, xlab="", ylab="",cex.main=0.75)
    
    # c(bottom, left, top, right)
    # par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    # plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
    # legend("bottom", c(SD1, SD2, "Delta"), xpd = TRUE, horiz = TRUE, inset = c(-0,0), bty = "n", lty=c(1,1), col = c("red", "green", "black"), cex = 0.5)
    # 
    # # xpd = TRUE tells R that it is OK to plot outside the region horiz = TRUE
    # # tells R that I want a horizontal legend inset = c(x,y) tells R how to move
    # # the legend relative to the 'bottom' location bty = 'n' means that 'no' box
    # # will be drawn around it pch and col are the types and colors of points cex
    # # = 2 makes the legend twice as large as other fonts
    dev.off()
  }

###THE MAIN FUNCTION
Main<- function()
{
  outpath <<- paste(inputpath, "Analysis/ResponseAnalysis", sep='')
  dir.create(outpath, showWarnings = FALSE)
  
  getanalysisdetails(inputpath)
  timelength <<- ((endtime-starttime)*60)/as.numeric(time_aggregation)
  variables <- c("CO2") #variables of study
  for(i in 1:length(variables))
  {
    var <<-  variables[[i]]
    #sumtable <<- read.delim(paste(inputpath,"Analysis/",foldername,"_",var,".txt",sep = ""))
    setup_resultstable(var)
    Fulltiffanalysis(var)
    graphdetails(var)
    outputgraph(var)
  }
}


Main()
