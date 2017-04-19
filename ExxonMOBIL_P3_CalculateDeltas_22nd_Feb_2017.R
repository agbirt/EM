#THIS PROGRAM TAKES:
# 1. TABLE OUTPUTS OF THE FIRST PROGRAM FOR TWO STRATEGIES

#THIS PROGRAM DOES:
# 1. CALCULATE CUMULATIVE AMOUNT OF EACH VARIABLE FOR EACH STRATEGY
# 2. CALCULATE DELTA OF TWO STRATEGIES OVER TIME
# 3. CALCULATE CUMULATIVE DELTA OF TWO STRATEGIES OVER TIME

#THIS PROGRAM OUTPUTS:
# 1. PLOTS OF CUMULATIVES, DELTAS, AND CULMULATIVE DELTAS (3 GRAPHS OR 4 GRAPHS)
# 2. TABLES OF CUMULATIVES, DELTAS, AND CULMULATIVE DELTAS




##TO CLEAR THE GLOBAL ENVIRONMENT
rm(list = ls())


###LOAD LIBRARIES
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(colorRamps)
library(readr)


######GLOBALS######
basefolder <-'E:/Raw Data- 170216/Base/Base 1.0/BASE_100_ES1_2016_10_200/' 
improvedfolder <-'E:/Raw Data- 170216/Braided/Braided 1.0/BASE_100_ES1_2016_10_200/'
pathbase <- basefolder
pathimproved <- improvedfolder
outpath <-""

#desired analysis time
starttime <- -2
endtime <- 22

# analysis <- "link" # can choose to do the El Paso graphs based on links (Program 4 does it)
analysis <- "tiff" 

tablebase <- data.frame()
tableimproved <- data.frame()
table_delta <-data.frame()
table_cumdelta <-data.frame()
table_cumbase <-data.frame()
table_cumimproved <-data.frame()
locs<-list()

SD1 <- ""
SD2 <- ""
emission_scenario <- "" 
year <- ""
time_aggregation <- ""
space_aggregation <- ""
var <- ""

diff <- raster()
crop <- raster()
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


###GET THE LOCATIONS SPECIFICATIONS AND CREATE A LOCATIONS TABLE
SetupLocationsTable<-function ()
{
  strategy <- unlist(strsplit(SD2,parse_number(SD2)))
  
  if (strategy=="BRAIDED"|strategy=="XRAMP")
  {
    # names = c("LeeTRavino", "FredWilson", "Zaragoza") 
    # east = c(375361, 363755, 37459) 
    # north = c(3511365, 3522768, 3505317)
    # width = c(100,100,100)
    # locations_table <<- list(names, east, north, width)
    # output (locations_table)
    #or
    
    loc1=list('Montana-Edgemere',379938,3518944,50)
    loc2=list('Edgemere-Pebble Hills',379949.2,3517148.79,50)
    loc3=list('Pebble Hills-Zaragoza',379942.85,3515629.55,50)
    loc4=list('Montwood-Vista del Sol',379938.87,3513351.29,50)
    loc5=list('Vista del Sol-Pellicano',379930.66,3511582.18,50)
    loc6=list('Zaragoza-Socorro RD',374559.91,3505317.45,50)
    loc7=list('Diana-TransMountain',363695.75,3529270.84,50)
    loc8=list('Hondo Pass-Diana',363699.68,3527686.8,50)
    loc9=list('Dyer-Hondo Pass',363714.63,3526048.24,50)
    loc10=list('Fred Wilson-Dyer',363755.32,3522768.98,50)
    loc11=list('Eastlake-Horizon Blvd',380585.99,3505638.42,50)
    loc12=list('Lee Trevino-Zaragoza',375361.62,3511365.39,50)
    loc13=list('Yarbrough-Lomaland',373323.9,3513243.58,50)
    loc14=list('Geronimo-Airways',367619.26,3517018.38,50)
    loc15=list('Reynolds-Paisano',364871.6,3516657.28,50)
    loc16=list('N Mesa-Redd Rd',351122.36,3524767.94,50)
    loc17=list('Artcraft-Redd Rd',350540.73,3527764.67,50)
   
    locs<<-append(locs,list(loc1))
    #locs<<-append(locs,list(loc1, loc2, loc3, loc4, loc5, loc6, loc7, loc8, loc9, loc10, loc11, loc12, loc13, loc14, loc15, loc16, loc17))
    
    # colnames(locations_table) <<- c("locationID", "Easting", "Northing", "widthxheight")
    
  }
  if (strategy=="ROUNDABOUT")
  {
    loc1=list('Westside-Borderland',346243.1764,3528929.97929999,80)
    loc2=list('Magruder-Hughey',365782.477099999,3517936.53759999,80)
    loc3=list('Darington-Rider',385203.5482,3507516.0906,80)
    
    locs<<-append(locs,list(loc1))
    # locs<<-append(locs,list(loc1, loc2,loc3))
  }
}



###READ THE INPUT TABLES
inputtable <- function()
{
  input1 <- paste(pathbase,"Analysis/", basename(pathbase),"_",var,".txt",sep = '')
  input2 <- paste(pathimproved,"Analysis/", basename(pathimproved),"_",var,".txt",sep = '')
  
  orgtablebase <<- read.delim(input1)
  tableimproved <<- read.delim(input2)
  tablebase <<- orgtablebase[names(tableimproved)] #the base has some more locations related to roundabouts
  
  tablebase <<- tablebase[startrow:endrow,]
  tableimproved <<- tableimproved[startrow:endrow,]
}


###SETUP OUTPUT TABLES
Setup_resulttables <- function()
{  
  table_delta<<- tableimproved
  table_cumdelta <<- tableimproved
  table_cumbase <<-tableimproved
  table_cumimproved <<-tableimproved
  
  for (i in 1:nrow(tableimproved))
  {
    table_delta[i,3] <<- tablebase[i,3]-tableimproved[i,3]
    for (j in 4:ncol(tableimproved))
    {
      table_delta[i, j] <<- -100
      table_cumdelta[i,j] <<- -100
      table_cumimproved[i,j] <<- -100
      table_cumbase[i,j] <<- -100
    }
  }
  
  table_cumdelta[1,3] <<- table_delta[1,3]
  table_cumimproved[1,3] <<- tableimproved[1,3]
  table_cumbase[1,3] <<- tablebase[1,3]
  for (i in 2:nrow(tableimproved))
  {
    table_cumdelta[i,3] <<- table_delta[i,3]+table_cumdelta[i-1,3]
    table_cumimproved[i,3] <<- tableimproved[i,3]+table_cumimproved[i-1,3]
    table_cumbase[i,3] <<- tablebase[i,3]+table_cumbase[i-1,3]
  }
}

 
###EL PASO TABLES AND GRAPHS
Elpasodeltaanalysis <- function()
{
  fill_tables("El Paso",0 )
  if (analysis == "link")
  {
    colno <- -1
  }
  if (analysis == "tiff")
  {
    colno <- 0
  }
  graphbuilder_3graphs("El Paso",colno )
  graphbuilder_4graphs("El Paso", colno )
}


###LOCATIONS TABLES AND GRAPHS
Locationdeltaanalysis <- function(locindex)
{
  thisloc <<- locs[[locindex]]
  fill_tables(thisloc[[1]], locindex )
  graphbuilder_3graphs(thisloc[[1]], locindex )
  graphbuilder_4graphs(thisloc[[1]], locindex )
}  


###CALCULATING OUTPUT TABLES
fill_tables <- function(locname,locindex)  
{
  timearray <- vector()
  base_total<-vector()
  delta<-vector()
  deltacum<-vector()
  cumulativeimproved<-vector()
  cumulativebase<-vector()
  improved_total<-vector()
  for (i in seq(1, timeintervals, by = 1))
  {
    timearray <- c(timearray, (i-1)*as.numeric(time_aggregation))
    #timearray <- c(timearray, (i)*as.numeric(time_aggregation))
    base_total<-c(base_total, tablebase[i,locindex+4])
    #base_total<-c(base_total, table1[i,locindex+4])
    improved_total<-c(improved_total, tableimproved[i,locindex+4])
    #improved_total<-c(improved_total, table2[i,locindex+4])
    delta <- c(delta, tablebase[i,locindex+4]-tableimproved[i,locindex+4])
    #delta <- c(delta, table1[i,locindex+4]-table2[i,locindex+4])
    if (i == 1)
    {
      cumulativeimproved <- c(cumulativeimproved, tableimproved[i,locindex+4])
      cumulativebase <- c(cumulativebase, tablebase[i,locindex+4])
      deltacum <- c(deltacum, delta[i])
      #deltacum <- c(deltacum, delta[i])
    }
    else
    {
      cumulativeimproved <- c(cumulativeimproved, cumulativeimproved[length(cumulativeimproved)] + tableimproved[i,locindex+4])
      cumulativebase <- c(cumulativebase, cumulativebase[length(cumulativebase)] + tablebase[i,locindex+4])
      deltacum <- c(deltacum, (deltacum[length(deltacum)]+delta[i]))
      #deltacum <- c(deltacum, deltacum[length(deltacum)])
      #print(deltacum)
    }
    table_delta[i, locindex+4] <<- delta[i]
    table_cumdelta[i, locindex+4] <<- deltacum[i]
    table_cumbase[i, locindex+4] <<- cumulativebase[i]
    table_cumimproved[i, locindex+4] <<- cumulativeimproved[i]
  }
}


###SAVE OUTPUT TABLES  
SaveOutputs <- function()
{
  outfilename <- paste(outpath,"/delta_", SD1,"_", SD2, "_", var, ".txt",sep = "")
  write.table(table_delta, file = outfilename, sep="\t", row.names = FALSE)
  outfilename <- paste(outpath,"/cumulativedelta_", SD1,"_", SD2, "_", var, ".txt",sep = "")
  write.table(table_cumdelta, file = outfilename, sep = "\t", row.names = FALSE)
  outfilename <- paste(outpath,"/cumulativebase_", SD1,"_", SD2, "_", var, ".txt",sep = "")
  write.table(table_cumbase , file = outfilename, sep = "\t", row.names = FALSE)
  outfilename <- paste(outpath,"/cumulativeimproved_", SD1,"_", SD2, "_", var, ".txt",sep = "")
  write.table(table_cumimproved, file = outfilename, sep = "\t", row.names = FALSE)
}



#CREATE THREE GRAPHS
graphbuilder_3graphs <- function(locname,locindex)
{
  outgraphname <- paste(outpath,"/delta_", SD1,"_", SD2, "_", var, "_",locname, sep = "")
  print(outgraphname)
  if (var == "CO2")
  {
    scale <- 1000*1000
    title <- paste("CO2 - ", locname, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 500
    minval2<- -50
    maxval2<- 50
    minval3<- -500
    maxval3<- 500
    ylabel <- "CO2 (metric tons)"
    legtext <- "Legend"
    Plot1_ylabel <- "Total CO2 (metric tons)"
    Plot2_ylabel <- "Delta CO2 (metric tons)"
    Plot3_ylabel <- "Cumulative Delta CO2 (metric tons)"
  }
  else if (var == "SPEED")
  {
    scale <- 1
    title <- paste("SPEED - ", locname, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 1
    minval2<- -1
    maxval2<- 1
    minval3<- -1
    maxval3<- 1
    ylabel <- "Speed Index"
    legtext <- "Legend"
    Plot1_ylabel <- "Total"
    Plot2_ylabel <- "Delta"
    Plot3_ylabel <- "Cumulative Delta"
  }
  else if (var == "VMT")
  {
    scale <- 1
    title <- paste("VMT - ", locname, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 200000
    minval2<- -10000
    maxval2<- 10000
    minval3<- -50000
    maxval3<- 50000
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
  
  
  png(paste(outgraphname, "_three.png", sep = ''),width = 4, height = 4, units = 'in', res = 600)
  par(oma=c(1,1.5,1,1),mar=c(2,1,1,1),mfrow=c(3,1))
  #bottom, left, top, right
  
  #plot 1
  plot(tablebase[[1]], tablebase[[locindex+4]] / scale, ylim=c(minval1,maxval1), xlab = "", ylab=Plot1_ylabel ,type="l", col="red", xaxt = "n", yaxt = "n")
  lines(tableimproved[[1]], tableimproved[[locindex+4]]  / scale,xlab="time",ylab=var, col="green")
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
  axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
  
  #plot 2  - differences
  plot(table_delta[[1]], table_delta[[locindex+4]] / scale, ylim=c(minval2,maxval2), xlab = "", ylab="",type="l", col="black", xaxt = "n", yaxt = "n")
  lines(c(0,1440),c(0,0),lty =2, col="black", lwd=0.5)
  axis(side=2, padj=2, tck = -0.025,cex.axis= 0.4, cex.lab=0.75)
  axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  
  #plot 3  - cumulative differences
  plot(table_cumdelta[[1]], table_cumdelta[[locindex+4]] / scale, ylim=c(minval3,maxval3), xlab = "", ylab="",type="l", col="black", xaxt = "n", yaxt = "n")
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
  legend("bottom", c(SD1, SD2, "Delta"), xpd = TRUE, horiz = TRUE, inset = c(-0,0), bty = "n", lty=c(1,1), col = c("red", "green", "black"), cex = 0.5)
  
  # xpd = TRUE tells R that it is OK to plot outside the region horiz = TRUE
  # tells R that I want a horizontal legend inset = c(x,y) tells R how to move
  # the legend relative to the 'bottom' location bty = 'n' means that 'no' box
  # will be drawn around it pch and col are the types and colors of points cex
  # = 2 makes the legend twice as large as other fonts
  dev.off()
}


#CREATE FOUR GRAPHS
graphbuilder_4graphs <- function(locname,locindex)
{
  outgraphname <- paste(outpath,"/delta_", SD1,"_", SD2, "_", var, "_",locname, sep = "")
  print(outgraphname)
  if (var == "CO2")
  {
    scale <- 1000*1000
    title <- paste("CO2 - ", locname, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 500
    minval2<- 0
    maxval2<- 25000
    minval3<- -50
    maxval3<- 50
    minval4<- -500
    maxval4<- 500
    ylabel <- "CO2 (metric tons)"
    legtext <- "Legend"
    Plot1_ylabel <- "Total CO2 (metric tons)"
    Plot2_ylabel <- "Total Cumulative  CO2 (metric tons)"
    Plot3_ylabel <- "Delta CO2 (metric tons)"
    Plot4_ylabel <- "Cumulative Delta CO2 (metric tons)"
  }
  else if (var == "SPEED")
  {
    scale <- 1
    title <- paste("SPEED - ", locname, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 1
    minval2<- 0
    maxval2<- 200
    minval3<- -1
    maxval3<- 1
    minval4<- -1
    maxval4<- 1
    ylabel <- "Speed Index"
    legtext <- "Legend"
    Plot1_ylabel <- "Total"
    Plot2_ylabel <- "Delta"
    Plot3_ylabel <- "Cumulative Delta"
  }
  else if (var == "VMT")
  {
    scale <- 1
    title <- paste("VMT - ", locname, sep="")
    xaxistitle = "time"
    minval1<- 0.00
    maxval1<- 2000000
    minval2<- 0
    maxval2<- 200000000
    minval3<- -100000
    maxval3<- 100000
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
  
  png(paste(outgraphname, "_four.png", sep = ''),width = 4, height = 4, units = 'in', res = 600)
  par(oma=c(1,1.5,1,1),mar=c(2,1,1,1),mfrow=c(2,2))
  #bottom, left, top, right
  
  #plot 1
  plot(tablebase[[1]], tablebase[[locindex+4]] / scale, ylim=c(minval1,maxval1), xlab = "", ylab=Plot1_ylabel ,type="l", col="red", xaxt = "n", yaxt = "n")
  lines(tableimproved[[1]], tableimproved[[locindex+4]]  / scale,xlab="time",ylab=var, col="green")
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
  axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
  
  #plot # 2 is the 2nd column of first row
  plot(table_cumbase[[1]], table_cumbase[[locindex+4]] / scale, ylim=c(minval2,maxval2), xlab = "", ylab=Plot2_ylabel ,type="l", col="red", xaxt = "n", yaxt = "n")
  lines(table_cumimproved[[1]], table_cumimproved[[locindex+4]] / scale,xlab="time",ylab=var, col="green")
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
  axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)

  #plot 3  - differences
  plot(table_delta[[1]], table_delta[[locindex+4]] / scale, ylim=c(minval3,maxval3), xlab = "", ylab="",type="l", col="black", xaxt = "n", yaxt = "n")
  lines(c(0,1440),c(0,0),lty =2, col="black", lwd=0.5)
  axis(side=2, padj=2, tck = -0.025,cex.axis= 0.4, cex.lab=0.75)
  axis(side=1,padj=-3, tck = -0.025,cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  
  #plot 4  - cumulative differences
  plot(table_cumdelta[[1]], table_cumdelta[[locindex+4]] / scale, ylim=c(minval4,maxval4), xlab = "", ylab="",type="l", col="black", xaxt = "n", yaxt = "n")
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
  legend("bottom", c(SD1, SD2, "Delta"), xpd = TRUE, horiz = TRUE, inset = c(-0,0), bty = "n", lty=c(1,1), col = c("red", "green", "black"), cex = 0.5)

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
  SD1 <<- getanalysisdetails(pathbase)
  SD2 <<- getanalysisdetails(pathimproved)
  
  dir.create(paste(pathimproved, "delta_",SD1,"_", SD2, sep=''), showWarnings = FALSE)
  
  timeintervals <<- (endtime-starttime)*60/as.numeric(time_aggregation)
  startrow <<- 13+6*starttime
  endrow <<- 12+6*endtime
  
  SetupLocationsTable()
  
  variables <- c("CO2", "SPEED", "VMT")
  #variables <- c("CO2")#variables of study
  for(i in 1:length(variables))
  {
    var <<-  variables[[i]]
    dir.create(paste(pathimproved, "delta_",SD1,"_",SD2, "/",var, sep=''), showWarnings = FALSE)
    outpath <<- paste(pathimproved, "delta_",SD1,"_",SD2, "/",var,"/graphs", sep='')
    dir.create(outpath, showWarnings = FALSE)
    inputtable()
    Setup_resulttables()
    Elpasodeltaanalysis()
    for(x in 1:length(locs))
    {
      Locationdeltaanalysis(x)
    }
    SaveOutputs()
  }
}

# PreMain<-function()
# {
#   improvedfolders <-c()
#   basefolders<-c()
#   
#   basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2016_10_200/')
#   basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2016_10_200/')
#   basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2016_10_200/')
#   basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2016_10_200/')
#   basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2016_10_200/')
#   basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2016_10_200/')
#   basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2016_10_200/')
#   
#   improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2016_10_200/')
#   improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2016_10_200/')
#   improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2016_10_200/')
#   improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2016_10_200/')
#   improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2016_10_200/')
#   improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2016_10_200/')
#   improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.40/BRAIDED_140_ES1_2016_10_200/')
#   for (i in 1:length(improvedfolders))
#   {
#     improvedfolder <- improvedfolders[[i]]
#     basefolder <- basefolders[[i]]
#     pathbase <- basefolder
#     pathimproved <- improvedfolder
#     Main()
#   }
# 
# }

#PreMain()
Main()

