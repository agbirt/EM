

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
starttime <- -2
endtime <- 24
time_aggregation <- ""
resulttable <- data.frame()
######END GLOBALS######


###GET DETAILS FROM FOLDER SCENARIO
getanalysisdetails <- function(path) 
{
  foldername <- basename(path)
  scenariodetails_vector <- c(unlist(strsplit(foldername,'_')))
  time_aggregation <<- scenariodetails_vector[5]
}

inputtables <- function(variable)
{
  speedinput <- paste(inputpath, basename(inputpath),"_AVHSPEED.txt",sep = '')
  variableinput <- paste(inputpath, basename(inputpath),"_EmissionsperMile_",variable,".txt",sep = '')
  speedtable <<- read.delim(speedinput)
  variabletable <<- read.delim(variableinput)
}


setup_resultstable<-function(variable)
{
  startcol <- 7 + (60*(starttime+2)/as.numeric(time_aggregation))
  endcol <- 6 + (60*(endtime+2)/as.numeric(time_aggregation))
  
  speedtable <<- speedtable[,c(2,startcol:endcol)]
  speedtable <<- speedtable[order(speedtable$Link_name),]
  speedlist <<- data.frame()
  for (i in 2:ncol(speedtable))
  {
    speedlist <<- rbind(speedlist,as.data.frame(speedtable[,i]))
  }
  
  variabletable <<- variabletable[,c(2,startcol:endcol)]
  variabletable <<- variabletable[order(variabletable$Link_name),]
  variablelist <<- data.frame()
  for (i in 2:ncol(variabletable))
  {
    variablelist <<- rbind(variablelist,as.data.frame(variabletable[,i]))
  }
  
  resulttable <<- merge(speedtable,variabletable, by= "Link_name")
  plottable <<- cbind(speedlist,variablelist)
  
  outtablename <- paste(outpath,"/", variable, "_ERversusSpeed.txt", sep = "")
  write.table(resulttable, outtablename, sep="\t", row.names = FALSE)
}


graphdetails <- function(variable)
{
  if (variable == "CO2")
  {
    yscale <<- 1
    minval1<<- 0.00
    maxval1<<- 500000
    ylabel <<- "CO2 Rate"
    Plot1_ylabel <<- "CO2 Rate"
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
  
    outgraphname <- paste(outpath,"/", variable, "_ERversusSpeed_graph2.png", sep = "")
    title <- paste(variable," Rate versus Speed @ El Paso", sep="")
    xaxistitle = "Average Speed (mph)"
    png(outgraphname,width = 4, height = 4, units = 'in', res = 600)
    par(oma=c(1,1.5,1,1),mar=c(2,1,1,1),mfrow=c(1,1))
    #bottom, left, top, right
    
    plot(plottable[1:nrow(plottable),1],plottable[1:nrow(plottable),2], xlab = "", ylim=c(0,500000),ylab=Plot1_ylabel ,type="p", cex=0.01,col="red", xaxt = "n", yaxt = "n")
    #lines(sumtable[1:timelength,1],sumtable[1:timelength,4],xlab="Time",ylab=variable, col="green")
    mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
    axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
    axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75)
    #axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=c(0,60*4, 60*8, 60*12, 60*16, 60*20, 60*24, 60*28, 60*32, 60*36),lwd=1, labels=c("0", "10", "20", "30", "40", "50", "60", "70","80","90"))
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.2, 3.2, 2.2, 3.2), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
    mtext(text=ylabel,side=2,line=2.2,outer=FALSE, cex = 0.5)
    mtext(text="Speed (mph)",side=1,line=1.2,outer=FALSE, cex = 0.5)
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
  outpath <<- paste(inputpath, "ResultAnalysis", sep='')
  dir.create(outpath, showWarnings = FALSE)
  
  getanalysisdetails(inputpath)
  
  variables <- c("CO2") #variables of study
  for(i in 1:length(variables))
  {
    var <-  variables[[i]]
    inputtables(var)
    setup_resultstable(var)
    graphdetails(var)
    outputgraph(var)
  }
}


Main()
