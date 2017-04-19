#THIS PROGRAM TAKES:
# 1. ALL TIFF FILES FOR EACH VARIABLE
# 2. LINK FILE FOR EACH VARIABLE

#THIS PROGRAM DOES:
# 1. CALCULATE THE VARIABLE FOR THE SPECIFIC TIME FOR ALL EL PASO AND LOCATIONS INDIVIDUALLY
# 2. CALCULATE OVERALL VARIABLE OVER THE LINKS FOR SPECIFIC TIME

#THIS PROGRAM OUTPUTS:
# 1. A TABLE FOR EACH VARIABLE WITH LINK AMOUNT AND TIFF AMOUNT FOR ALL EL PASO AND TIFF AMOUNT FOR EACH LOCATION OVER TIME
# 2. PLOTS FOR EL PASO AND EACH LOCATION INDIVIDUALLY ON EACH TIME
# 3. TEMPORAL GRAPH FOR EL PASO AND EACH LOCATION INDIVIDUALLY




###TO CLEAR THE GLOBAL ENVIRONMENT
rm(list = ls())


###LOAD THE LIBRARIES
library(rgdal)
library(gtools)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(colorRamps)


#################GLOBALS
# inputpath <- 'E:/Raw Data- 170216/Base/Base 1.0/BASE_100_ES1_2016_10_200/'
inputpath <- "E:/Raw Data- 170216/Braided/Braided 1.0/BASE_100_ES1_2016_10_200/"

setwd(inputpath)

coloffset<-4 #offset for full tiff analysis column

hours <- 26 #total analysis time in input

#desired graph timing
starttime <- -2
endtime <- 24 

files_list <- list()
variable_list <- list()
resulttable <- data.frame()
locs <-list()
crop_table <-list()

foldername <- ""
strategy <- ""
demand <- ""
emission_scenario <- "" 
year <- ""
time_aggregation <- "" 
space_aggregation <- ""
#################END GLOBALS


###GET DETAILS FROM FOLDER SCENARIO
getanalysisdetails <- function(path) 
{
  foldername <- basename(path)
  scenariodetails_vector <- c(unlist(strsplit(foldername,'_')))
  strategy <<- scenariodetails_vector[1]
  demand <<- scenariodetails_vector[2]
  emission_scenario <<- scenariodetails_vector[3]
  year <<- scenariodetails_vector[4]
  time_aggregation <<- scenariodetails_vector[5]
  space_aggregation <<- scenariodetails_vector[6]
}


###GET THE LOCATIONS SPECIFICATIONS AND CREATE A LOCATIONS TABLE
SetupLocationsTable<-function ()
{
  if (strategy=="BASE"|strategy=="BRAIDED"|strategy=="XRAMP")
  {
    # names = c("LeeTRavino", "FredWilson", "Zaragoza") 
    # east = c(375361, 363755, 37459) 
    # north = c(3511365, 3522768, 3505317)
    # width = c(100,100,100)
    # locs <<- list(names, east, north, width)
    # output (locs)
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
    
    #locs<<-append(locs,list(loc1, loc2, loc3, loc4, loc5, loc6, loc7, loc8, loc9, loc10, loc11, loc12, loc13, loc14, loc15, loc16, loc17))
    locs<<-append(locs,list(loc1))
    
    # colnames(locs) <<- c("locationID", "Easting", "Northing", "widthxheight")
  }
  if (strategy=="BASE"|strategy=="ROUNDABOUT")
  {
    loc1=list('Westside-Borderland',346243.1764,3528929.97929999,80)
    loc2=list('Magruder-Hughey',365782.477099999,3517936.53759999,80)
    loc3=list('Darington-Rider',385203.5482,3507516.0906,80)
    
    # locs<<-append(locs,list(loc1, loc2,loc3))
    locs<<-append(locs,list(loc1))
  }
} 



###SET UP A RESULT TABLE FOR ALL LOCATIONS FOR A CERTAIN VARIABLE
setup_resultstable<-function(variable)
{
  xvariable <- numeric(timeintervals)
  yvariable <- numeric(timeintervals)
  zvariable <- numeric(timeintervals)
  resulttable <<- data.frame(xvariable,xvariable,xvariable,xvariable)
  col_names <<- c("time(min)","time(hr:min)", paste("links_amount_",variable, sep = ""), paste("tiff_amount_",variable, sep = ""))
  for (i in 1:length(locs))
  {
    col_names <- c(col_names, locs[[i]][1])
    resulttable <<- cbind(resulttable, xvariable)
  }
  colnames(resulttable) <<- col_names;
  for (i in 1:timeintervals)
  {
    yvariable[i] <- i*as.numeric(time_aggregation)
    date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
    date1 <- date1+(i*as.numeric(time_aggregation)*60)-2*60*60
    zvariable[i] <- format(date1,"%H:%M")
    
    resulttable[i,1:2] <<- c(yvariable[i],zvariable[i])
  }
}


###EXTRACT THE TIFFS FOR A CERTAIN VARIABLE FROM ALL TIFFS STORAGE
getvariabletiffs <- function(variable) 
{
  for(i in 1:timeintervals)
  {
    filepath <- paste(inputpath,basename(inputpath),"_",variable,"_",i*as.numeric(time_aggregation),".tif",sep = "")
    variable_list <<- c(variable_list,filepath)
  }
}



###ANALYSIS OF TEXT FILES FOR A CERTAIN VARIABLE
datafileanalysis <- function(var)
{
  if (var == "SPEED")
  {
    filename <- paste(strategy, demand,emission_scenario,year,time_aggregation,space_aggregation,"AVHSPEED", sep = "_")
  }
  if (var == "CO2"|var == "VMT")
  {
    filename <- paste(strategy, demand,emission_scenario,year,time_aggregation,space_aggregation,var, sep = "_")
  }
  
  filepath <- paste(inputpath,filename,".txt", sep = "")
  datatable <- read.delim(filepath)

  
  if (var == "SPEED")
  {
   volpath <- paste(inputpath,paste(strategy, demand,emission_scenario,year,time_aggregation,space_aggregation,"VOLUME", sep = "_"),".txt", sep = "")
   voltable <- read.delim(volpath)
   voltable <- voltable[match(datatable$Link_name,voltable$Link_name),]
   
   lengthlist <- datatable[,3]
   
   for (i in 1:timeintervals)
   {
     speedlist <- datatable[,i+6]
     vollist <- voltable[,i+6]
     
     wghtlist <- vollist*lengthlist
     wghtspeedlist <- wghtlist/speedlist
     
     WHMspeed <- sum(wghtlist)/sum(wghtspeedlist)
     
     resulttable[i,3] <<- WHMspeed
   }
  }
   
  else
  {
    for (i in 1:timeintervals)
    {
      column_total <- colSums(datatable[i+6])
      resulttable[i,3] <<- column_total
    }
  }
}


###ANALYSIS OF TIFFS FOR A CERTAIN VARIABLE
Fulltiffanalysis <- function(variable)
{
  #declare a results table
  xvariable <- numeric(length(variable_list))
  
  #go through each tiff open it
  for (i in 1:length(variable_list))
  {
    #open the tiff
    filename <- variable_list[[i]]
    #tiff_file <- data.frame(readGDAL(filename[1]))
    tiff_file<-raster(filename[1])
    tiff_file[tiff_file<0]<- NA
    a=StandardGISPlot(tiff_file, variable, i, "El Paso")
    
    if (variable == "SPEED")
    {
      xvariable[i] = cellStats(tiff_file, 'mean', na.rm=TRUE)
    }
    else
    {
      xvariable[i] = cellStats(tiff_file, 'sum', na.rm=TRUE)
    }

    resulttable[i,4] <<- data.frame(xvariable[i])
  }
  output_location_graph(variable,"EL Paso",0)
}


###GET LOCATIONS VALUES, SAVE THEM IN RESULT TABLE, SHOW THEM ON MAP, AND PLOT THEM FOR A CERTAIN VARIABLE AND LOCATION
Locationtiffanalysis <- function(variable, locationindex)
{
  #declare a results table
  zvariable <- numeric(length(variable_list))

  #go through each tiff open it
  for (i in 1:length(variable_list))
  {
    #open the tiff
    filename <- variable_list[[i]]
    #tiff_file <- data.frame(readGDAL(filename[1]))
    tiff_file<-raster(filename[1])
    tiff_file[tiff_file<0]<- NA
    #clip tiff to a location
    loc=locs[[locationindex]]
    # print(loc)
    #define the crop extent order is tlx, brx, tly, bry
    cellsize<-as.numeric(space_aggregation)
    tlx = loc[[2]]-loc[[4]]*cellsize
    brx = loc[[2]]+loc[[4]]*cellsize
    tly = loc[[3]]-loc[[4]]*cellsize
    bry = loc[[3]]+loc[[4]]*cellsize
    cropbox <-c(tlx,brx,tly, bry)
    
    croptiff <- crop(tiff_file, cropbox)
    
    a=StandardGISPlot(croptiff, variable, i, loc)
    
    #sum all of the tiff
    if (variable == "SPEED")
    {
      zvariable[i] = cellStats(croptiff, 'mean', na.rm=TRUE)
    }
    else
    {
      zvariable[i] = cellStats(croptiff, 'sum', na.rm=TRUE)
    }
    #add to [result] table
    resulttable[i,locationindex+coloffset] <<- data.frame(zvariable[i])
  }
  output_location_graph(variable,loc,locationindex)
}


###SPECIFIES THE DETAILS FOR PLOTTING EACH VARIABLE
plotdetails <-function(variable)
{
  if (variable == "SPEED")
  {
    minval<<- 0
    maxval<<- 1
    increment <- 0.02
    brks <<- seq(minval, maxval, by=increment)
    #print(brks)
    mycolours <<- matlab.like(length(brks))
    mycolours <<- rev(mycolours)
    legtext <<- "Speed Index"
  }
  if (variable == "CO2")
  {
    minval<<- 0
    maxval<<- 100000
    increment <- maxval / 2000
    brks <<- seq(minval, maxval+increment, by=increment)
    #print(brks)
    mycolours <<- matlab.like(length(brks))
    #colorRampPalette(colorRamps)
    #mycolours=green2red(length(brks))
    legtext <<- "CO2 (g)"
  }
  if (variable == "VMT")
  {
    minval<<- 0
    maxval<<- 1000
    increment <- maxval/50
    brks <<- seq(minval, maxval, by=increment)
    #print(brks)
    mycolours <<- matlab.like(length(brks))
    
    #colorRampPalette(colorRamps)
    #mycolours=green2red(length(brks))
    # reverse VMT - More VMYT is better
    legtext <<- "VMT"
  }
  
  if (variable == "NOX")
  {
    minval<<- 0
    maxval<<- 400
    increment <- maxval/50
    brks <<- seq(minval, maxval, by=increment)
    #print(brks)
    mycolours <<- matlab.like(length(brks))
    
    #colorRampPalette(colorRamps)
    #mycolours=green2red(length(brks))
    # reverse VMT - More VMYT is better
    legtext <<- "NOX (g)"
  }
}


###TO PLOT THE NETWORK ON MAP
StandardGISPlot<-function(tif, var, timeindex, loc)
{
  copytif <- tif
  copytif[copytif[]>maxval] <- maxval
  
  date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
  date1 <- date1+(timeindex*as.numeric(time_aggregation)*60)- 2*60*60
  title <- paste(loc[[1]], " ", var , "@", demand, " :", format(date1,"%H:%M:%S"))
  print (title)

  dir.create(paste(inputpath, "Analysis/", sep =""), showWarnings = FALSE)
  dir.create(paste(inputpath, "Analysis/", var, "@", loc[[1]], "/", sep =""), showWarnings = FALSE)
  outputpath <- paste(inputpath, "Analysis", paste(var, "@", loc[[1]], "/", sep= ""),strategy, sep = "/")
  outputpath <- paste(outputpath, demand, loc[[1]], var, (timeindex*as.numeric(time_aggregation)) , sep = "_")
  
  png(paste(outputpath, ".png",sep = ""))
  #maxpixels <- 500*500
  #plot (tif)
  #image(tif, zlim=c(minval,maxval/2), main = title, xlab = "UTM Westing Coordinate (m)",ylab = "UTM Northing Coordinate (m)")
  labelnames <- seq(minval, maxval,  by=(maxval/4))
  #labelnames <- 10^labels 
  plot(copytif, breaks=brks,  col = mycolours, useRaster=TRUE, colNA=NA, main = title,
       xlab = "UTM Westing Coordinate (m)",
       ylab = "UTM Northing Coordinate (m)",
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=seq(minval, maxval, by=(maxval/4)),
                      labels=labelnames,
                      cex.axis=0.6),
       legend.args=list(text=legtext, side=4, font=2, line=2.5, cex=0.8))
  #plot(tif,  useRaster=TRUE, colNA=NA, main = title, xlab = "UTM Westing Coordinate (m)",ylab = "UTM Northing Coordinate (m)")
  
  #image(tif,  main = title, interpolate = FALSE, xlab = "UTM Westing Coordinate (m)",ylab = "UTM Northing Coordinate (m)")
  if (loc == "El Paso")
  {
    for (i in 1:length(locs))
    {
      # symbols(locs[[i]][[2]],locs[[i]][[3]], squares = 2*locs[[i]][[4]]*as.numeric(space_aggregation), add = TRUE, inches= FALSE)  #S1
      symbols(locs[[i]][[2]],locs[[i]][[3]], circles = (xmax(tif)-xmin(tif))/50, add = TRUE, inches= FALSE, lwd =2) #S2
      #symbols(locs[[i]][[2]],locs[[i]][[3]], circles = 1000, add = TRUE, inches= FALSE) #S3
    }
  }
  else
  {
    for (i in 1:length(locs))
    {
      # symbols(locs[[i]][[2]],locs[[i]][[3]], squares = 2*locs[[i]][[4]]*as.numeric(space_aggregation), add = TRUE, inches= FALSE)  #S1
      symbols(locs[[i]][[2]],locs[[i]][[3]], circles = (xmax(tif)-xmin(tif))/50, add = TRUE, inches= FALSE, lwd =2) #S2
      #symbols(locs[[i]][[2]],locs[[i]][[3]], circles = 1000, add = TRUE, inches= FALSE) #S3
    }
  }
  # if (loc == "El Paso")
  # {
  #  for (i in 1:length(locs))
  #  {
  #   # symbols(locs[[i]][[2]],locs[[i]][[3]], squares = 2*locs[[i]][[4]]*as.numeric(space_aggregation), add = TRUE, inches= FALSE)  #S1
  #   symbols(locs[[i]][[2]],locs[[i]][[3]], circles = (xmax(tif)-xmin(tif))/100, add = TRUE, inches= FALSE) #S2
  #   #symbols(locs[[i]][[2]],locs[[i]][[3]], circles = 1000, add = TRUE, inches= FALSE) #S3
  #   }
  # }
  # else
  # {
  #   # symbols(loc[[2]],loc[[3]], circles = (xmax(tif)-xmin(tif))/100 , add = TRUE, inches= FALSE) #S2
  #   symbols(loc[[2]],loc[[3]], circles = 1000, add = TRUE, inches= FALSE) #S3
  # }
  dev.off()
  #(tif, main = title, interpolate = FALSE, xlab = "UTM Westing Coordinate (m)",ylab = "UTM Northing Coordinate (m)")
}


graphdetails <- function(variable)
{
  if (variable == "CO2")
  {
    yscale <<- 1000*1000
    minval1<<- 0.00
    maxval1<<- 500
    ylabel <<- "CO2 (metric tons)"
    Plot1_ylabel <<- "Total CO2 (metric tons)"
  }
  else if (variable == "SPEED")
  {
    yscale <<- 1
    minval1<<- 0.00
    maxval1<<- 70.00
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


###OUTPUT GRAPHS FOR A CERTAIN VARIABLE
output_location_graph <- function(variable,loc,locindex)
{
  outputpath <- paste(inputpath, "Analysis", paste(variable, "@", loc[[1]], "/", sep= ""),strategy, sep = "/")
  outputpath <- paste(outputpath,demand, loc[[1]], variable, sep = "_")
  
  title <- paste(variable," @ ", loc[[1]], sep="")
  xaxistitle = "time"
  startrow <- 13+6*starttime
  endrow <- 12+6*endtime
  timelocs <- seq(0, endtime-starttime, by=2)*60
  timeset <- seq(starttime,endtime, by=2)*60
  date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
  date1 <- date1+(timeset*60)
  timelabels <- format(date1,"%H:%M")
  
  png(paste(outputpath, ".png", sep = ""),width = 4, height = 4, units = 'in', res = 600)
  par(oma=c(1,1.5,1,1),mar=c(2,1,1,1),mfrow=c(1,1))
  #bottom, left, top, right
  
  #plot 1
  plot(resulttable[startrow:endrow,1],resulttable[startrow:endrow,locindex+coloffset] / yscale, ylim=c(minval1,maxval1), xlab = "", ylab=Plot1_ylabel ,type="l", col="red", xaxt = "n", yaxt = "n")
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


###OUTPUT TABLES FOR A CERTAIN VARIABLE
output_resulttable <- function(variable)
{
  # dir.create(paste(inputpath, "Analysis/", sep =""), showWarnings = FALSE)
  outputpath <- paste(inputpath, "Analysis", strategy, sep = "/")
  outputpath <- paste(outputpath, demand, emission_scenario, year, time_aggregation, space_aggregation, variable, sep = "_")
  write.table(resulttable,paste(outputpath, ".txt",sep = ""), sep = "\t", row.names = FALSE)
}


#OUTPUT LINK VS TIFF AMOUNTS FOR EACH VARIABLE
output_variable_graph <- function(variable)
{
  outputpath <- paste(inputpath, "Analysis", strategy, sep = "/")
  outputpath <- paste(outputpath, demand, emission_scenario, year, time_aggregation, space_aggregation, variable, sep = "_")
  
  if (!(variable == "SPEED"))
  {
    title <- paste(variable, " - Link vs TIFF", sep = "")
  }
  else 
  {
    title <- paste(variable, " - Link", sep = "")
  }
  xaxistitle = "time"
  
  startrow <- 13+6*starttime
  endrow <- 12+6*endtime
  timelocs <- seq(0, endtime-starttime, by=2)*60
  timeset <- seq(starttime,endtime, by=2)*60
  date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
  date1 <- date1+(timeset*60)
  timelabels <- format(date1,"%H:%M")
  
  png(paste(outputpath, ".png",sep = ""), width = 4, height = 4, units = 'in', res = 600)
  par(oma=c(1,1.5,1,1),mar=c(2,1,1,1),mfrow=c(1,1))
  #bottom, left, top, right

  #plot 1
  plot(resulttable[startrow:endrow,1],resulttable[startrow:endrow,3] / yscale, ylim=c(minval1,maxval1), xlab = "", ylab=Plot1_ylabel ,type="l", col="red", xaxt = "n", yaxt = "n")
  
  if (!(variable == "SPEED"))
  {
    lines(resulttable[startrow:endrow,1],resulttable[startrow:endrow,4] / yscale,xlab="time",ylab=var, col="green")
  }
  
  mtext(text="",side=2,line=1.0,outer=FALSE, cex = 0.5)
  axis(side=2, padj=2, tck = -0.025, cex.axis= 0.4, cex.lab=0.1)
  axis(side=1, padj=-3, tck = -0.025, cex.axis= 0.4, cex.lab=0.75, at=timelocs,lwd=1, labels=timelabels)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(3.2, 3.2, 2.2, 3.2), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
  mtext(text=ylabel,side=2,line=2.2,outer=FALSE, cex = 0.5)
  mtext(text="Time",side=1,line=1.2,outer=FALSE, cex = 0.5)
  title(main=title, xlab="", ylab="",cex.main=0.75)
  
  # c(bottom, left, top, right)
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n"  )
  legend("bottom", c("Link","TIFF"), xpd = TRUE, horiz = TRUE, inset = c(-0,0), bty = "n", lty=c(1,1), col = c("red", "green"), cex = 0.5)
  
  dev.off()
}


###THE MAIN FUNCTION WHICH RUNS ALL THE PROGRAM
Main <- function() 
{
  getanalysisdetails(inputpath)
  timeintervals <<- (hours*60) / as.numeric(time_aggregation)
  SetupLocationsTable()
  
  variables <- c("CO2","SPEED","VMT") #variables of study
  # variables <- c("SPEED")
  for(i in 1:length(variables))
  {
    var<- variables[[i]]
    setup_resultstable(var)
    getvariabletiffs(var)
    datafileanalysis(var)
    plotdetails(var)
    graphdetails(var)
    Fulltiffanalysis(var)
    for(x in 1:length(locs))
    {
      Locationtiffanalysis(var, x)
    }
    output_resulttable(var)
    output_variable_graph(var)
    variable_list <<- NULL
  }
}

PreMain<- function() 
{
  inputpaths <-c()
  #inputpaths <- c("F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2016_10_200/")
  #inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2016_10_200/")
  #inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2016_10_200/")
  # inputpaths <- c( "F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2016_10_200/")
  # # #Braid
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2016_10_200/")
  #inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2016_10_200/")
  #inputpaths <- c(inputpaths, "F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2016_10_200/")
  # #Xramp
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2016_10_200/")
  # inputpaths <- c(inputpaths, "F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2016_10_200/")
  
  # #Base Years
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2048_20_200/")
  
  #Braid Years
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2048_20_200/")
  # 
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2024_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2032_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2040_20_200/")
  # inputpaths <- c(inputpaths,"F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2048_20_200/")
  
  #XRAMP
  
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2024_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2032_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2040_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2048_20_200/")
  
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2024_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2032_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2040_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2048_20_200/")
  
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2024_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2032_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2040_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2048_20_200/")
  
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2024_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2032_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2040_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2048_20_200/")
  
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2024_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2032_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2040_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2048_20_200/")
  
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2024_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2032_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2040_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2048_20_200/")
  
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2024_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2032_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2040_20_200/")
  inputpaths <- c(inputpaths,"F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2048_20_200/")
  
  
  print (inputpaths)
  for (i in 1:length(inputpaths))
  {
    files_list <<- list()
    variable_list <<- list()
    resulttable <<- data.frame()
    locs <<-list
    crop_table <-list
    inputpath <<-inputpaths[[i]]
    print(inputpaths[[i]])
    setwd(inputpaths[[i]])
    Main()
    files_list <<- list()
  }
}

Main()
#PreMain()



