#THIS PROGRAM TAKES:
# 1. LINK FILE FOR SPEED AND VMTS

#THIS PROGRAM DOES:
# 1. DEFINE THE SPEED BINS
# 2. CALCULATE VMTS IN EACH SPEED BINS

#THIS PROGRAM OUTPUTS:
# 1. HISTOGRAMS OF VMTS BASED ON SPEED BINS
# 2. TABLE OF VMTS FOR EACH SPEED BIN



###TO CLEAR THE GLOBAL ENVIRONMENT
rm(list = ls())


######GLOBALS######
#inputpath <-'C:/Users/a-birt/Syncplicity Folders/ExxonMobil/Test Results/Braid 1.3/Base 1.3/BASE_130_ES1_2016_10_200/' 
#inputpath <-'F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2016_10_200/'
inputpath <-'F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2016_10_200/'

outputpath <- ""

Truckvolume <<- data.frame()
Carvolume <<- data.frame()
AvSpeed <<- data.frame()
result <<- data.frame()
OutputData <<- data.frame()

hours <- 26
strategy <<- ""
demand <<- ""
year <<- ""
emission_scenario <<- ""
time_aggregation <<- ""
space_aggregation<<-""

yscale <<- 10000

#analysistype <<- "MOVES"
# linktypes<<- c(2,3)
# linktypenames <<- c("Interstate","Arterials")
analysistype <<- "DYNUST"
# linktypes<<- c(3, 4, 1, 5)
# linktypenames <<- c("On Ramps","Off Ramps", "Interstate","Arterials")
######END GLOBALS######


###GET DETAILS FROM FOLDER SCENARIO
getanalysisdetails <- function() 
{
  path_vector <- c(unlist(strsplit(inputpath,'/')))
  foldername <- path_vector[length(path_vector)]
  scenariodetails_vector <- c(unlist(strsplit(foldername,'_')))
  strategy <<- scenariodetails_vector[1]
  demand <<- scenariodetails_vector[2]
  emission_scenario <<- scenariodetails_vector[3]
  year <<- scenariodetails_vector[4]
  time_aggregation <<- scenariodetails_vector[5]
  space_aggregation <<- scenariodetails_vector[6]
}


### OPENS TEXT FILES AND SAVES THEM IN DATAFRAMES
OpenFiles <- function ()
{
  #cars VMT
  filename = paste(strategy, "_", demand, "_", emission_scenario, "_", year, "_", time_aggregation, "_", space_aggregation, "_", "CARVMT.txt", sep ="")
  inputfile <- paste(inputpath,filename, sep="")
  print(inputfile)
  CarVolume <<- read.delim(inputfile)
  #trucks VMT
  filename = paste(strategy, "_", demand, "_", emission_scenario, "_", year, "_", time_aggregation, "_", space_aggregation, "_", "TRUCKVMT.txt", sep ="")
  inputfile <- paste(inputpath,filename,sep="")
  TruckVolume <<- read.delim(inputfile)
  #now speed
  filename = paste(strategy, "_", demand, "_", emission_scenario, "_", year, "_", time_aggregation, "_", space_aggregation, "_", "AVHSPEED.txt", sep ="")
  inputfile <- paste(inputpath,filename,sep="")
  AvSpeed <<- read.delim(inputfile)
}


#SETS UP AN OUTPUT TABLE
SetUpOutput <-function()
{
  #colnames <- c("LinkSpeed", "AvSpeed", "TruckVMT", "CarVMT")
  length <- (hours*60) / as.numeric(time_aggregation)
  length <- length * nrow(AvSpeed)
  OutputData <<- data.frame("LinkID"= numeric(length), "LinkType"= numeric(length), "LinkSpeed"= numeric(length), "LinkLength"= numeric(length),"Time"= numeric(length),  "AvSpeed"= numeric(length), "TruckVMT" = numeric(length), "CarVMT" = numeric(length) );
}



insertRow <- function(existingDF, newrow, r) 
{
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)  
}


#CREATE HISTOGRAM
DrawTotalHistograms <- function()
{
  outgraphname <- paste(outputpath, "/VMTatSpeedBins_", strategy, "_", demand, "_", analysistype, sep = "")
  #create a matrix for speed bins
  speedbins <- seq(5, 70, by=2.5)
  speedbin_at <- seq(1,28, by=4)
  speedbin_labels <- seq(0,65, by=5)
  
  maxT = 200
  minT = 0
  maxC = 1000
  minC = 0
  yaxislabelsT = seq(minT, maxT, by=(maxT-minT)/5)
  yaxislabelsC = seq(minC, maxC, by=(maxC-minC)/5)
  #create storage
  #"LinkID"= numeric(length), "LinkType"= numeric(length),
  length <- length(speedbins)
  
  if (analysistype == "DYNUST")
  {
    result <<- data.frame("speed"=numeric(length), "CarVMT_Arterial" = numeric(length), "CarVMT_Ramp" = numeric(length), "CarVMT_Inter" = numeric(length), 
                          "TruckVMT_Arterial" = numeric(length), "TruckVMT_Ramp" = numeric(length), "TruckVMT_Inter" = numeric(length))
    result[,1]<<-speedbins
    count <- 1
    for (i in speedbins)
    {
      VMT <- subset(OutputData, LinkType == 5 & AvSpeed >= i-5 & AvSpeed < i, select=c(CarVMT))
      if (length(VMT) > 0)
      {
        result[count,2]<<- sum(VMT$CarVMT)
      }
      VMT <- subset(OutputData, ((LinkType == 3)|(LinkType == 4))  & AvSpeed >= i-5 & AvSpeed < i, select=c(CarVMT))
      if (length(VMT) > 0)
      {
        result[count,3]<<- sum(VMT$CarVMT)
      }
      VMT <- subset(OutputData, LinkType == 1 & AvSpeed >= i-5 & AvSpeed < i, select=c(CarVMT))
      if (length(VMT) > 0)
      {
        result[count,4]<<- sum(VMT$CarVMT)
      }
      #trucks
      VMT <- subset(OutputData, LinkType == 5 & AvSpeed >= i-5 & AvSpeed < i, select=c(TruckVMT))
      if (length(VMT) > 0)
      {
        result[count,5]<<- sum(VMT$TruckVMT)
      }
      VMT <- subset(OutputData, ((LinkType == 3)|(LinkType == 4)) & AvSpeed >= i-5 & AvSpeed < i, select=c(TruckVMT))
      if (length(VMT) > 0)
      {
        result[count,6]<<- sum(VMT$TruckVMT)
      }
      VMT <- subset(OutputData, LinkType == 1 & AvSpeed >= i-5 & AvSpeed < i, select=c(TruckVMT))
      if (length(VMT) > 0)
      {
        result[count,7]<<- sum(VMT$TruckVMT)
      }
      count<-count+1
    }
    
    png(paste(outgraphname, ".png", sep = ''),width = 4, height = 4, units = 'in', res = 600)
    par(oma=c(3,2,1,1),mar=c(1,1,2,1),mfrow=c(3,2))
    #bottom, left, top, right
    #hist(result$CarVMT_Arterial, breaks = speedbins)
    mp <- barplot(result$CarVMT_Arterial, main="Car - Arterial",cex.main = 0.75,cex.names=0.5, tck = -0.025,xlab="", ylab="", xaxt = "n", yaxt = "n", ylim=c(minC,maxC))
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(result$TruckVMT_Arterial, main="Truck - Arterial", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="",ylim=c(minT,maxT), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    
    barplot(result$CarVMT_Inter, main="Car - Interstate", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="", ylim=c(minC,maxC), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(result$TruckVMT_Inter, main="Truck - Interstate",cex.main = 0.75,cex.names=0.5, tck = -0.025,ylim=c(minT,maxT),xlab="", ylab="", xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    
    barplot(result$CarVMT_Ramp, main="Car - Ramp", cex.main = 0.75,  cex.names=0.5, tck = -0.025,ylim=c(minC,maxC),xlab="", ylab="", xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(result$TruckVMT_Ramp, main="Truck - Ramp",cex.main = 0.75,cex.names=0.5, tck = -0.025,xlab="", ylab="", xaxt = "n", yaxt = "n", ylim=c(minT,maxT))
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT )
    
    
    # #plot an empty graph so that we can place the legend
    par(fig = c(0, 1, 0, 1), oma = c(1, 1, 1, 1), mar = c(2, 2, 2, 2), new = TRUE)
    plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
    #title(main="",xlab="Speed Bin (mph)", ylab="VMT",cex.lab=0.75)
    mtext(text="Speed Bin (mph)" ,side=1,line=1,outer=FALSE, cex = 0.5)
    mtext(text=paste("VMT (x ",yscale, " miles)", sep=""),side=2,line=2,outer=FALSE, cex = 0.5)
    #legend("bottom", c("a", "b"), xpd = TRUE, horiz = TRUE, inset = c(-10,0), bty = "n", lty=c(1,1), col = c("red", "green"), cex = 0.5)
    
    dev.off()
  }
  if (analysistype == "MOVES")
  {
    result <<- data.frame("speed"=numeric(length), "CarVMT_Arterial" = numeric(length), "CarVMT_Inter" = numeric(length), 
                          "TruckVMT_Arterial" = numeric(length), "TruckVMT_Inter" = numeric(length))
    result[,1]<<-speedbins
    count <- 1
    for (i in speedbins)
    {
      VMT <- subset(OutputData, LinkType == 3 & AvSpeed >= i-5 & AvSpeed < i, select=c(CarVMT))
      if (length(VMT) > 0)
      {
        result[count,2]<<- sum(VMT$CarVMT)
      }
      VMT <- subset(OutputData, LinkType == 2 & AvSpeed >= i-5 & AvSpeed < i, select=c(CarVMT))
      if (length(VMT) > 0)
      {
        result[count,3]<<- sum(VMT$CarVMT)
      }

      #trucks
      VMT <- subset(OutputData, LinkType == 3 & AvSpeed >= i-5 & AvSpeed < i, select=c(TruckVMT))
      if (length(VMT) > 0)
      {
        result[count,4]<<- sum(VMT$TruckVMT)
      }
      VMT <- subset(OutputData, LinkType == 2 & AvSpeed >= i-5 & AvSpeed < i, select=c(TruckVMT))
      if (length(VMT) > 0)
      {
        result[count,5]<<- sum(VMT$TruckVMT)
      }
      count<-count+1
    }
    
    png(paste(outgraphname, ".png", sep = ''),width = 4, height = 4, units = 'in', res = 600)
    par(oma=c(3,2,1,1),mar=c(1,1,2,1),mfrow=c(2,2))
    #bottom, left, top, right
    #hist(result$CarVMT_Arterial, breaks = speedbins)
    mp <- barplot(result$CarVMT_Arterial, main="Car - Arterial",cex.main = 0.75,cex.names=0.5, tck = -0.025,xlab="", ylab="", xaxt = "n", yaxt = "n", ylim=c(minC,maxC))
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(result$TruckVMT_Arterial, main="Truck - Arterial", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="",ylim=c(minT,maxT), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    
    barplot(result$CarVMT_Inter, main="Car - Interstate", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="", ylim=c(minC,maxC), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(result$TruckVMT_Inter, main="Truck - Interstate",cex.main = 0.75,cex.names=0.5, tck = -0.025,ylim=c(minT,maxT),xlab="", ylab="", xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=result$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    # #plot an empty graph so that we can place the legend
    par(fig = c(0, 1, 0, 1), oma = c(1, 1, 1, 1), mar = c(2, 2, 2, 2), new = TRUE)
    plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
    #title(main="",xlab="Speed Bin (mph)", ylab="VMT",cex.lab=0.75)
    mtext(text="Speed Bin (mph)" ,side=1,line=1,outer=FALSE, cex = 0.5)
    mtext(text=paste("VMT (x ",yscale, " miles)", sep=""),side=2,line=2,outer=FALSE, cex = 0.5)
    #legend("bottom", c("a", "b"), xpd = TRUE, horiz = TRUE, inset = c(-10,0), bty = "n", lty=c(1,1), col = c("red", "green"), cex = 0.5)
    
    dev.off()
  }
}


###THE MAIN FUNCTION
Main <- function ()
{
  getanalysisdetails()
  #Open all files and strore them in global dataframes
  OpenFiles()
  #declare a data table for storage
  SetUpOutput()
  dir.create(paste(inputpath, "Analysis/", sep =""), showWarnings = FALSE)
  outputpath <<- paste(inputpath, "Analysis/Histograms", sep ="")
  dir.create(outputpath, showWarnings = FALSE)
  #Go through each data frame link by link and then time column by time column and store each VMT value in a vector
  #posted speed, av speed, car VMT, truck VMT
  length <- (hours*60) / as.numeric(time_aggregation)
  #length <- 10 
  count = 1
  rows <- nrow(AvSpeed)
  #extract the link speed row
  linkspeed <- AvSpeed[, 5]
  linklength <- AvSpeed[,3]
  linkid <- AvSpeed[,1]
  if (analysistype == "MOVES")
  {
    linktype <- AvSpeed[["Moves.Link.Type"]]
  }
  else if (analysistype == "DYNUST")
  {
    linktype <- AvSpeed[["DynusT.Link.Type"]]
  }
  
  Av_Speed <- vector(mode="numeric", length=rows)
  TruckVMT <- vector(mode="numeric", length=rows)
  CarVMT <- vector(mode="numeric", length=rows)
  #OutputData <<- data.frame("LinkID"= numeric(length), "LinkType"= numeric(length), "LinkSpeed"= numeric(length), "Time"= numeric(length),  "AvSpeed"= numeric(length), "TruckVMT" = numeric(length), "CarVMT" = numeric(length) );
  
  #print(linkid)
  
    for (i in 1:length)
    {
      time <- seq(i*as.numeric(time_aggregation) , i*as.numeric(time_aggregation), length.out=rows)
      startindex <- (i-1)*rows
      endindex <-(i)*rows
      Av_Speed <- AvSpeed[,i+6]
      TruckVMT <- TruckVolume[, i+6]
      CarVMT <- CarVolume[, i+6]
      TruckVMT <- TruckVMT
      CarVMT <- CarVMT
      # (Av_Speed)
      #print (endindex)
      OutputData[startindex+1:endindex, 1] <<- linkid
      OutputData[startindex+1:endindex, 2] <<- linktype
      OutputData[startindex+1:endindex, 3] <<- linkspeed
      OutputData[startindex+1:endindex, 4] <<- linklength
      OutputData[startindex+1:endindex, 5] <<- time
      OutputData[startindex+1:endindex, 6] <<- Av_Speed
      OutputData[startindex+1:endindex, 7] <<- TruckVMT/yscale
      OutputData[startindex+1:endindex, 8] <<- CarVMT/yscale
     }
  DrawTotalHistograms()
  outtable <- paste(outputpath,"/VMTatSpeedBins_table_", strategy, "_", demand, "_",analysistype, ".txt", sep = "")
  write.table(result, outtable, sep="\t", row.names = FALSE)
  #output vectors to a file
}



Main()