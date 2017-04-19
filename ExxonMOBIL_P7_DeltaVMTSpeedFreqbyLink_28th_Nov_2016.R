###TO CLEAR THE GLOBAL ENVIRONMENT
rm(list = ls())


######GLOBALS######
# basepath <- 'C:/Users/f-sharifi/Desktop/BASE_100_ES1_2016_10_200/'
# improvedpath <- 'C:/Users/f-sharifi/Desktop/BRAIDED_100_ES1_2016_10_200/'
#analysistype <<- "MOVES"
# linktypes<<- c(2,3)
# linktypenames <<- c("Interstate","Arterials")
analysistype <<- "DYNUST"
# linktypes<<- c(3, 4, 1, 5)
# linktypenames <<- c("On Ramps","Off Ramps", "Interstate","Arterials")
yscale <<- 1
######END GLOBALS####


basepath <-'F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2016_10_200/' 
improvedpath <-'F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2016_10_200/'

getnamedetails <- function(path) 
{
  foldername <- basename(path)
  scenariodetails_vector <- c(unlist(strsplit(foldername,'_')))
  strategy <- scenariodetails_vector[1]
  demand <- scenariodetails_vector[2]
  tablename <- paste("VMTatSpeedBins_table_", strategy, "_", demand, "_",analysistype, ".txt", sep = "")
  SD <- paste(strategy,demand, sep = "")
  details <- c(tablename,SD)
  return(details)
}

inputtable <- function()
{
  inputbase <- paste(basepath,"Analysis/Histograms/",getnamedetails(basepath)[1],sep = '')
  inputimproved <- paste(improvedpath,"Analysis/Histograms/",getnamedetails(improvedpath)[1], sep = '')
  tablebase <<- read.delim(inputbase)
  tableimproved <<- read.delim(inputimproved)
  #make a copy of tables for output
}

Setup_resulttables <- function()
{  
  table_delta<<- tablebase
  for (i in 1:nrow(tablebase))
  {
    for (j in 2:ncol(tablebase))
    {
      table_delta[i,j] <<- tablebase[i,j]-tableimproved[i,j] 
    }
  }
}

#CREATE HISTOGRAM
DrawDeltaHistograms <- function()
{
  outgraphname <- paste(outputpath, "/VMTatSpeedBins_deltagraph_", SDbase, "_",SDimproved , "_",analysistype, sep = "")
  #create a matrix for speed bins
  speedbins <- seq(5, 70, by=2.5)
  speedbin_at <- seq(1,28, by=4)
  speedbin_labels <- seq(0,65, by=5)
  
  maxT = 20
  minT = -20
  maxC = 50
  minC = -50
  yaxislabelsT = seq(minT, maxT, by=(maxT-minT)/5)
  yaxislabelsC = seq(minC, maxC, by=(maxC-minC)/5)
  #create storage
  #"LinkID"= numeric(length), "LinkType"= numeric(length),
  length <- length(speedbins)
  
  if (analysistype == "DYNUST")
  {
    count <- 1

    
    png(paste(outgraphname, ".png", sep = ''),width = 4, height = 4, units = 'in', res = 600)
    par(oma=c(3,2,1,1),mar=c(1,1,2,1),mfrow=c(3,2))
    #bottom, left, top, right
    #hist(table_delta$CarVMT_Arterial, breaks = speedbins)
    mp <- barplot(table_delta$CarVMT_Arterial, main="Car - Arterial",cex.main = 0.75,cex.names=0.5, tck = -0.025,xlab="", ylab="", xaxt = "n", yaxt = "n", ylim=c(minC,maxC))
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(table_delta$TruckVMT_Arterial, main="Truck - Arterial", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="",ylim=c(minT,maxT), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    
    barplot(table_delta$CarVMT_Inter, main="Car - Interstate", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="", ylim=c(minC,maxC), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(table_delta$TruckVMT_Inter, main="Truck - Interstate",cex.main = 0.75,cex.names=0.5, tck = -0.025,ylim=c(minT,maxT),xlab="", ylab="", xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    
    barplot(table_delta$CarVMT_Ramp, main="Car - Ramp", cex.main = 0.75,  cex.names=0.5, tck = -0.025,ylim=c(minC,maxC),xlab="", ylab="", xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(table_delta$TruckVMT_Ramp, main="Truck - Ramp",cex.main = 0.75,cex.names=0.5, tck = -0.025,xlab="", ylab="", xaxt = "n", yaxt = "n", ylim=c(minT,maxT))
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT )
    
    
    # #plot an empty graph so that we can place the legend
    par(fig = c(0, 1, 0, 1), oma = c(1, 1, 1, 1), mar = c(2, 2, 2, 2), new = TRUE)
    plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
    #title(main="",xlab="Speed Bin (mph)", ylab="VMT",cex.lab=0.75)
    mtext(text="Speed Bin (mph)" ,side=1,line=1,outer=FALSE, cex = 0.5)
    mtext(text="VMT",side=2,line=2,outer=FALSE, cex = 0.5)
    #legend("bottom", c("a", "b"), xpd = TRUE, horiz = TRUE, inset = c(-10,0), bty = "n", lty=c(1,1), col = c("red", "green"), cex = 0.5)
    
    dev.off()
  }
  if (analysistype == "MOVES")
  {
    
    png(paste(outgraphname, ".png", sep = ''),width = 4, height = 4, units = 'in', res = 600)
    par(oma=c(3,2,1,1),mar=c(1,1,2,1),mfrow=c(2,2))
    #bottom, left, top, right
    #hist(table_delta$CarVMT_Arterial, breaks = speedbins)
    mp <- barplot(table_delta$CarVMT_Arterial, main="Car - Arterial",cex.main = 0.75,cex.names=0.5, tck = -0.025,xlab="", ylab="", xaxt = "n", yaxt = "n", ylim=c(minC,maxC))
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(table_delta$TruckVMT_Arterial, main="Truck - Arterial", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="",ylim=c(minT,maxT), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    
    barplot(table_delta$CarVMT_Inter, main="Car - Interstate", cex.main = 0.75, cex.names=0.5, tck = -0.025, xlab="", ylab="", ylim=c(minC,maxC), xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsC, labels=yaxislabelsC)
    
    
    barplot(table_delta$TruckVMT_Inter, main="Truck - Interstate",cex.main = 0.75,cex.names=0.5, tck = -0.025,ylim=c(minT,maxT),xlab="", ylab="", xaxt = "n", yaxt = "n")
    axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= mp, labels=table_delta$speed)
    axis(side=2,padj=2, tck = -0.025,cex.axis= 0.5, cex.lab=0.75, at= yaxislabelsT, labels=yaxislabelsT)
    
    # #plot an empty graph so that we can place the legend
    par(fig = c(0, 1, 0, 1), oma = c(1, 1, 1, 1), mar = c(2, 2, 2, 2), new = TRUE)
    plot(1, 1, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
    #title(main="",xlab="Speed Bin (mph)", ylab="VMT",cex.lab=0.75)
    mtext(text="Speed Bin (mph)" ,side=1,line=1,outer=FALSE, cex = 0.5)
    mtext(text="VMT",side=2,line=2,outer=FALSE, cex = 0.5)
    #legend("bottom", c("a", "b"), xpd = TRUE, horiz = TRUE, inset = c(-10,0), bty = "n", lty=c(1,1), col = c("red", "green"), cex = 0.5)
    
    dev.off()
  }
}


###THE MAIN FUNCTION
Main <- function ()
{
  inputtable()
  Setup_resulttables()
  SDbase <<- getnamedetails(basepath)[2]
  SDimproved <<- getnamedetails(improvedpath)[2]
  outputpath <<- paste(improvedpath, "delta_",SDbase,"_", SDimproved,"/Delta Histograms", sep='')
  dir.create(outputpath, showWarnings = FALSE)
  DrawDeltaHistograms()
  outtable <- paste(outputpath,"/VMTatSpeedBins_deltatable_", SDbase, "_",SDimproved , "_",analysistype, ".txt", sep = "")
  write.table(table_delta, outtable, sep="\t", row.names = FALSE)
}



Main()