#THIS PROGRAM TAKES:
# 1. LINK FILE FOR EACH VARIABLE FOR THE TWO STRATEGIES

#THIS PROGRAM DOES:
# 1. GET SUBSET TABLES FOR EACH LINK TYPE
# 2. CALCULATE THE AMOUNT OF VARIABLE FOR ALL LINKS AND EACH LINK TYPE SEPARATELY OVER TIME

#THIS PROGRAM OUTPUTS:
# 1. A TABLE SHOWING EACH LINK TYPE AMOUNT OVER TIME FOR EACH VARIABLE, ALSO CUMULATIVES, DELTA, AND CUMULATIVE DELTA
# 2. PLOTS OF CUMULATIVES, DELTAS, AND CULMULATIVE DELTAS (4 GRAPHS)



###TO CLEAR THE GLOBAL ENVIRONMENT
rm(list = ls())


###LOAD LIBRARIES
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(colorRamps)


######GLOBALS######
basefolder <-'F:/Raw Data- 170216/Base/Base 1.0/BASE_100_ES1_2016_10_200/'
improvedfolder <-"F:/Raw Data- 170216/Braided/Braided 1.0/BRAIDED_100_ES1_2016_10_200/"

pathbase <- basefolder
pathimproved <- improvedfolder
outpath <-""

tablebase <- data.frame()
tableimproved <- data.frame()
subtablebase <- data.frame()
subtableimproved <- data.frame()
resulttable <- data.frame()


#the desired time to analyze can be specified
starttime <- 0
endtime <- 24

SDbase <- ""
SDimproved <- ""
emission_scenario <- "" 
year <- ""
time_aggregation <- ""
space_aggregation <- ""
var <- ""
timeintervals <- 0



# analysistype <<- "MOVES"
# linktypes<<- c(2,3,0)
# linktypenames <<- c("Interstate","Arterials","ALL")

analysistype <<- "DYNUST"
linktypes<<- c(3, 1, 5, 0) #0 shows 
linktypenames <<- c("Ramps", "Interstate","Arterials", "All")
######END GLOBALS######


###GET DETAILS FROM FOLDER SCENARIO
getanalysisdetails <- function(path) 
{
  foldername <- basename(path)
  scenariodetails_vector <- c(unlist(strsplit(foldername,'_')))
  strategy <- scenariodetails_vector[1]
  demand <- scenariodetails_vector[2]
  emission_scenario <<- scenariodetails_vector[3]
  year <<- scenariodetails_vector[4]
  time_aggregation <<- scenariodetails_vector[5]
  space_aggregation <<- scenariodetails_vector[6]
  SD <- paste(strategy,demand, sep = "")
  return(SD)
}



###READING THE INPUT TABLES
inputtable <- function()
{
  if (var == "SPEED")
  {
    inputbase <- paste(pathbase, basename(pathbase),"_AVHSPEED.txt",sep = '')
    inputimproved <- paste(pathimproved, basename(pathimproved),"_AVHSPEED.txt",sep = '')
    
    #read volume tables
    volpathbase <- paste(pathbase, basename(pathbase),"_VOL.txt",sep = '')
    volpathimproved <- paste(pathimproved, basename(pathimproved),"_VOL.txt",sep = '')
    voltablebase <<- read.delim(volpathbase)
    voltablebase <<- voltablebase[,c(1:6,startcol:endcol)]
    voltableimproved <<- read.delim(volpathimproved)
    voltableimproved <<- voltableimproved[,c(1:6,startcol:endcol)]
  }
  else
  {
    inputbase <- paste(pathbase, basename(pathbase),"_",var,".txt",sep = '')
    inputimproved <- paste(pathimproved, basename(pathimproved),"_",var,".txt",sep = '')
  }

  tablebase <<- read.delim(inputbase)
  tablebase$DynusT.Link.Type[tablebase$DynusT.Link.Type == 4] <<- 3
  tablebase <<- tablebase[,c(1:6,startcol:endcol)]
  tableimproved <<- read.delim(inputimproved)
  tableimproved$DynusT.Link.Type[tableimproved$DynusT.Link.Type == 4] <<- 3
  tableimproved <<- tableimproved[,c(1:6,startcol:endcol)]
  
}


###SET UP A RESULT TABLE FOR ALL LOCATIONS FOR A CERTAIN VARIABLE
setup_resultstable<-function()
{
  xvariable <- numeric(timeintervals)
  yvariable <- numeric(timeintervals)
  resulttable <<- data.frame(xvariable, yvariable) #,xvariable,xvariable,xvariable,xvariable,xvariable)
  colnames <- c("time(min)","time(hr:min)")
  for (i in 1:length(linktypes))
  {
    print (i)
    resulttable <<- cbind(resulttable, xvariable)
    resulttable <<- cbind(resulttable, xvariable)
    resulttable <<- cbind(resulttable, xvariable)
    resulttable <<- cbind(resulttable, xvariable)
    resulttable <<- cbind(resulttable, xvariable)
    resulttable <<- cbind(resulttable, xvariable)
    colnames <- c(colnames, paste("Base-", linktypenames[i]))
    colnames <- c(colnames, paste("Improved-", linktypenames[i]))
    colnames <- c(colnames, paste("Cumulative Base-", linktypenames[i]))
    colnames <- c(colnames, paste("Cumulative Improved-", linktypenames[i]))
    colnames <- c(colnames, paste("Delta-", linktypenames[i]))
    colnames <- c(colnames, paste("Cumulative_Delta-", linktypenames[i]))
  }
  
  #print(colnames)
  #paste("Base_arterial",var, sep = ""), paste("Base_interstate",var, sep = ""), paste("Improved_arterial",var, sep = ""), paste("Improved_interstate",var, sep = ""))
  colnames(resulttable) <<- colnames;
  
  for (i in 1:timeintervals) 
  {
    xvariable[i] <- i*as.numeric(time_aggregation)
    date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
    date1 <- date1+(i*as.numeric(time_aggregation)*60) +starttime*60*60
    yvariable[i] <- format(date1,"%H:%M")
    resulttable[i,1:2] <<- c(xvariable[i],yvariable[i])
  }
}


#CREATE SUBSETS OF TABLES BASED ON LINK TYPES
createsubtable <- function(linktype)
{
  if(linktype == 0)
  {
    if (analysistype == "MOVES")
    {
      subtablebase <<- tablebase
      subtableimproved <<- tableimproved
    }
    else if (analysistype == "DYNUST")
    {
      subtablebase <<- tablebase
      subtableimproved <<- tableimproved
    }
  }
  
  else
  {
    if (analysistype == "MOVES")
    {
      subtablebase <<- subset(tablebase, Moves.Link.Type == linktype)
      subtableimproved <<- subset(tableimproved, Moves.Link.Type == linktype)
    }
    else if (analysistype == "DYNUST")
    {
      subtablebase <<- subset(tablebase, DynusT.Link.Type == linktype)
      subtableimproved <<- subset(tableimproved, DynusT.Link.Type == linktype)
    }
  }
  
  if (var == "SPEED")
  {
    volsubtablebase <<- voltablebase[match(subtablebase$Link_name,voltablebase$Link_name),]
    volsubtableimproved <<- voltableimproved[match(subtableimproved$Link_name,voltableimproved$Link_name),]
    voltotalbase <<-colSums(volsubtablebase[,7:(timeintervals+6)])
    voltotalimproved <<-colSums(volsubtableimproved[,7:(timeintervals+6)])
  }
}



WHM <- function(varlist, weightlist)
{
  wghtvarlist <- weightlist/varlist
  WHMvar <- sum(weightlist)/sum(wghtvarlist)
  return(WHMvar)
}



###To GET NUMBERS FOR EACH CELL
sumup <- function(table,colindex,voltable)
{
  if (missing(voltable))
  {
    for (i in 1:timeintervals)
    {
      column_total <- colSums(table[i+6])
      resulttable[i,colindex] <<- column_total
    }
  }
  
  else
  {
    lengthlist <- table[,3]
    
    for (i in 1:timeintervals)
    {
      speedlist <- table[,i+6]
      vollist <- voltable[,i+6]
      wghtlist <- vollist*lengthlist
      
      resulttable[i,colindex] <<- WHM(speedlist,wghtlist)
    }
  }
}



###FINALIZING RESULTTABLE
fill_table <- function(colindex)
{
  if (var == "SPEED")
  {
    sumup(subtablebase,colindex,volsubtablebase)
    sumup(subtableimproved,colindex+1,volsubtableimproved)
    
    basevols <- data.frame()
    basemiles <- data.frame()
    basespeeds <- data.frame()
    
    improvedvols <- data.frame()
    improvedmiles <- data.frame()
    improvedspeeds <- data.frame()
    
    for (i in 1:timeintervals)
    {
      resulttable[i, colindex+4] <<- resulttable[i, colindex] - resulttable[i, colindex+1]
      basevols <- rbind(basevols,as.data.frame(volsubtablebase[,i+6]))
      basemiles <- rbind(basemiles,as.data.frame(subtablebase[,3]))
      basespeeds <- rbind(basespeeds,as.data.frame(subtablebase[,i+6]))
      improvedvols <- rbind(improvedvols,as.data.frame(volsubtableimproved[,i+6]))
      improvedmiles <- rbind(improvedmiles,as.data.frame(subtableimproved[,3]))
      improvedspeeds <- rbind(improvedspeeds,as.data.frame(subtableimproved[,i+6]))
      
      if (i == 1)
      {
        resulttable[i, colindex+2] <<- resulttable[i, colindex]
        resulttable[i, colindex+3] <<- resulttable[i, colindex+1]
        resulttable[i, colindex+5] <<- resulttable[i, colindex] - resulttable[i, colindex+1]
      }
      else
      {
        basewghts <- basevols * basemiles
        resulttable[i, colindex+2] <<- WHM(basespeeds,basewghts)
        
        improvedwghts <- improvedvols * improvedmiles
        resulttable[i, colindex+3] <<- WHM(improvedspeeds,improvedwghts)
        # resulttable[i, colindex+2] <<- WHM(resulttable[1:i,colindex],voltotalbase[1:i])
      #   resulttable[i, colindex+3] <<- WHM(resulttable[1:i,colindex+1],voltotalimproved[1:i])
        resulttable[i, colindex+5] <<- resulttable[i, colindex+2] - resulttable[i, colindex+3]
      }
    }
  }
  
  else
  {
    sumup(subtablebase,colindex)
    sumup(subtableimproved,colindex+1)
    for (i in 1:timeintervals)
    {
      resulttable[i, colindex+4] <<- resulttable[i, colindex] - resulttable[i, colindex+1]
      if (i == 1)
      {
        resulttable[i, colindex+2] <<- resulttable[i, colindex] 
        resulttable[i, colindex+3] <<- resulttable[i, colindex+1] 
        resulttable[i, colindex+5] <<- resulttable[i, colindex] - resulttable[i, colindex+1]
      }
      else
      {
        resulttable[i, colindex+2] <<- resulttable[i, colindex] +  resulttable[i-1, colindex+2]
        resulttable[i, colindex+3] <<- resulttable[i, colindex+1] + resulttable[i-1, colindex+3]
        resulttable[i, colindex+5] <<- (resulttable[i-1, colindex+5] + (resulttable[i, colindex] - resulttable[i, colindex+1]))
      }
    }
  }
}
  




graphbuilder <- function(linktype,colindex)
{
  outgraphname <- paste(outpath,"/delta_", SDbase,"_", SDimproved, "_", var,"_", analysistype, "_", linktype , sep = "")
  
  if (var == "CO2")
  {
    scale <- 1000*1000
    title <- paste("CO2 - ", linktype, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 500
    minval2<- 0
    maxval2<- 20000
    minval3<- -10
    maxval3<- 10
    minval4<- -600
    maxval4<- 600
    ylabel <- "CO2 (tons)"
    legtext <- "Legend"
    Plot1_ylabel <- "Total CO2 (tons)"
    Plot2_ylabel <- "Total Cumulative  CO2 (tons)"
    Plot3_ylabel <- "Delta CO2 (tons)"
    Plot4_ylabel <- "Cumulative Delta CO2 (tons)"
  }
  else if (var == "SPEED")
  {
    scale <- 1
    title <- paste("SPEED - ", linktype, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 70
    minval2<- 0
    maxval2<- 100
    minval3<- -50
    maxval3<- 50
    minval4<- -50
    maxval4<- 50
    ylabel <- "Speed (mph)"
    legtext <- "Legend"
    Plot1_ylabel <- "Total"
    Plot2_ylabel <- "Delta"
    Plot3_ylabel <- "Cumulative Delta"
  }
  else if (var == "VMT")
  {
    scale <- 1
    title <- paste("VMT - ", linktype, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 500000
    minval2<- 0
    maxval2<- 30000000
    minval3<- -20000
    maxval3<- 20000
    minval4<- -500000
    maxval4<- 500000
    ylabel <- "VMT"
    legtext <- "Legend"
    Plot1_ylabel <- "Total"
    Plot2_ylabel <- "Delta"
    Plot3_ylabel <- "Cumulative Delta"
  }
  

  timelocs <- seq(0, endtime-starttime, by=2)*60
  timeset <- seq(starttime,endtime, by=2)*60
  date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
  date1 <- date1+(timeset*60)
  timelabels <- format(date1,"%H:%M")
  
  print(outgraphname)
  png(paste(outgraphname, "Emissions_by_link.png", sep = ''),width = 4, height = 4, units = 'in', res = 600)
  par(oma=c(1,1.5,1,1),mar=c(2,1,1,1),mfrow=c(2,2))
  #bottom, left, top, right
  #plot 1
  plot(resulttable[1:nrow(resulttable),1], resulttable[1:nrow(resulttable),colindex] / scale, ylim=c(minval1,maxval1), xlab = "", ylab=Plot1_ylabel ,type="l", col="red", xaxt = "n", yaxt = "n")
  lines(resulttable[1:nrow(resulttable),1], resulttable[1:nrow(resulttable),colindex+1] / scale,xlab="time",ylab=var, col="green")
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
  axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
 
  #plot 2 is the 2nd column of first row
  plot(resulttable[1:nrow(resulttable),1], resulttable[1:nrow(resulttable),colindex+2] / scale, ylim=c(minval2,maxval2), xlab = "", ylab=Plot2_ylabel ,type="l", col="red", xaxt = "n", yaxt = "n")
  lines(resulttable[1:nrow(resulttable),1], resulttable[1:nrow(resulttable),colindex+3] / scale,xlab="time",ylab=var, col="green")
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
  axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
  
  #plot 3  - differences
  plot(resulttable[1:nrow(resulttable),1], resulttable[1:nrow(resulttable),colindex+4] / scale, ylim=c(minval3,maxval3), xlab = "", ylab="",type="l", col="black", xaxt = "n", yaxt = "n")
  lines(c(0,1440),c(0,0),lty =2, col="black", lwd=0.5)
  axis(side=2, padj=2, tck = -0.025,cex.axis= 0.4, cex.lab=0.75)
  axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  
  #plot 4  - cumulative differences
  plot(resulttable[1:nrow(resulttable),1], resulttable[1:nrow(resulttable),colindex+5] / scale, ylim=c(minval4,maxval4), xlab = "", ylab="",type="l", col="black", xaxt = "n", yaxt = "n")
  lines(c(0,1440),c(0,0),col="black", lty =2, lwd=0.5)
  axis(side=2, padj=2, tck = -0.025,cex.axis= 0.4, cex.lab=0.75)
  axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  # #plot an empty graph so that we can place the legend
  # par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 4, 1), new = TRUE)
  # plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  #bottom, left, top, right
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.2, 3.2, 2.2, 3.2), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
  mtext(text=ylabel,side=2,line=2.2,outer=FALSE, cex = 0.5)
  mtext(text="Time",side=1,line=1.2,outer=FALSE, cex = 0.5)
  title(main=title, xlab="", ylab="",cex.main=0.75)
  
  # c(bottom, left, top, right)
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
  legend("bottom", c(SDbase, SDimproved, "Delta"), xpd = TRUE, horiz = TRUE, inset = c(-0,0), bty = "n", lty=c(1,1), col = c("red", "green", "black"), cex = 0.5)
  #
  # xpd = TRUE tells R that it is OK to plot outside the region horiz = TRUE
  # tells R that I want a horizontal legend inset = c(x,y) tells R how to move
  # the legend relative to the 'bottom' location bty = 'n' means that 'no' box
  # will be drawn around it pch and col are the types and colors of points cex
  # = 2 makes the legend twice as large as other fonts
  dev.off()
}


###THE MAIN FUNCTION
Main<- function()
{
  SDbase <<- getanalysisdetails(pathbase)
  SDimproved <<- getanalysisdetails(pathimproved)
  
  timeintervals <<- (endtime-starttime)*60/as.numeric(time_aggregation)
  startcol <<- 7 + (60*(starttime+2)/as.numeric(time_aggregation))
  endcol <<- 6 + (60*(endtime+2)/as.numeric(time_aggregation))

  
  dir.create(paste(pathimproved, "delta_",SDbase,"_", SDimproved, sep=''), showWarnings = FALSE)
  variables <- c("CO2","SPEED","VMT") 
  # variables <- c("CO2")
  
  for(i in 1:length(variables))
  {
    var <<-  variables[[i]]
    
    dir.create(paste(pathimproved, "delta_",SDbase,"_",SDimproved, "/",var, sep=''), showWarnings = FALSE)
    outpath <<- paste(pathimproved, "delta_",SDbase,"_",SDimproved, "/",var,"/linkgraphs", sep='')
    dir.create(outpath, showWarnings = FALSE)
    
    inputtable()
    setup_resultstable()
    
    for(j in 1:length(linktypes))
    {
      colindex = ((j-1)*6)+3 #2 columns for time, 6 columns to hold result for each link type
      createsubtable(linktypes[j])
      fill_table(colindex)
      graphbuilder(linktypenames[j],colindex)
    }
    #Save the results table to folder path
    #print (outpath)
    outfilename <- paste(outpath,"/", analysistype, "_link_type_analysis.txt", sep = "")
    write.table(resulttable, outfilename, sep="\t", row.names = FALSE)
   }
}

Main()


