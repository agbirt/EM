#THIS PROGRAM TAKES:
# 1. TIFF FILES OF A SAME VARIABLE FOR TWO STRATEGIES

#THIS PROGRAM DOES:
# 1. CALCULATE THAT VARIABLE DIFFERENCE BETWEEN TWO STRATEGIES OVER TIME TIME FOR ALL EL PASO AND LOCATIONS INDIVIDUALLY

#THIS PROGRAM OUTPUTS:
# 1. PLOTS OF DIFFERENCES FOR EACH TIME AND LOCATION
# 2. SAVES PLOTS AS PNG AND TIFF



###TO CLEAR THE GLOBAL ENVIRONMENT
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

starttime <- -2
endtime <- 24

path1 <- basefolder
path2 <- improvedfolder
SD1 <- ""
SD2 <- ""
emission_scenario <- "" 
year <- ""
time_aggregation <- ""
space_aggregation <- ""
diff <- raster()
crop <- raster()
locs<-list()
var <- ""
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
    
    # locs<<-append(locs,list(loc1, loc2, loc3, loc4, loc5, loc6, loc7, loc8, loc9, loc10, loc11, loc12, loc13, loc14, loc15, loc16, loc17))
    locs<<-append(locs,list(loc1))
    # colnames(locations_table) <<- c("locationID", "Easting", "Northing", "widthxheight")
    
  }
  if (strategy=="ROUNDABOUT")
  {
    loc1=list('Westside-Borderland',346243.1764,3528929.97929999,80)
    loc2=list('Magruder-Hughey',365782.477099999,3517936.53759999,80)
    loc3=list('Darington-Rider',385203.5482,3507516.0906,80)
    
    # locs<<-append(locs,list(loc1, loc2,loc3))
    locs<<-append(locs,list(loc1))
  }
}


###SPECIFIES THE DETAILS FOR PLOTTING EACH VARIABLE
plotdetails <-function(variable)
{
  if (variable == "SPEED")
  {
    minval<<- -0.25
    maxval<<- 0.25
    increment <- 0.001
    increment <<- maxval / 100
    brks <<- seq(minval, maxval, by=increment)
    #print(brks)
    mycolours <<- colorRampPalette(brewer.pal(11, "PiYG"))(length(brks))
    mycolours <<- rev(mycolours)
    #colorRampPalette(colorRamps)
    #mycolours=green2red(length(brks))
    
    legtext <<- "Delta Speed Index"
  }
  if (variable == "CO2")
  {
    minval<<- -50000
    maxval<<- 50000
    increment <<- maxval / 100
    brks <<- seq(minval, maxval, by=increment)
    #print(brks)
    mycolours <<- colorRampPalette(brewer.pal(11, "PiYG"))(length(brks))
    #colorRampPalette(colorRamps)
    #mycolours=green2red(length(brks))
    
    legtext <<- "Delta CO2 (g)"
  }
  if (variable == "VMT")
  {
    minval<<- -50
    maxval<<- 50
    increment <- maxval / 100
    brks <<- seq(minval, maxval, by=increment)
    #print(brks)
    mycolours <<- colorRampPalette(brewer.pal(11, "PiYG"))(length(brks))
    #colorRampPalette(colorRamps)
    #mycolours=green2red(length(brks))
    rev(mycolours) # reverse VMT - More VMYT is better
    legtext <<- "Delta VMT"
   }
}


###TO PLOT THE NETWORK ON MAP
StandardGISPlot<-function(tif, variable, filetime, loc)
{
  tif[tif[]>maxval] <- maxval
  tif[tif[]<minval] <- minval
  
  date1 <- as.POSIXlt("000101 00:00", format = "%y%m%d %H:%M")
  date1 <- date1+(filetime*60)+(starttime*60*60)
  #title <- paste(loc[[1]], " ", variable , "@", SD1,"_", SD2, " :", format(date1,"%H:%M:%S"))
  title <- paste(loc[[1]], " ", variable , " ", SD1," minus ", SD2)
  #plot(tif)
  outpng <- paste("delta_", SD1,"_", SD2, "_", variable, "_", loc[[1]],"_", filetime,  sep="")
  outpngpath <- paste(path2, "delta_", SD1,"_", SD2, "/",variable, "/",loc, "/", "png","/", outpng, sep = "")
  print(outpngpath)
  png(paste(outpngpath, ".png"), width = 5, height = 4, units = 'in', res = 600)
  par(cex=0.5, oma=c(0,1,0,0),mar=c(6,4,4,2))
  #plot(tif, breaks=brks, col = mycolours, main = title, interpolate = FALSE, xlab = "UTM Westing Coordinate (m)",ylab = "UTM Northing Coordinate (m)")
  #image(tif,  main = title, interpolate = FALSE, xlab = "UTM Westing Coordinate (m)",ylab = "UTM Northing Coordinate (m)")
  plot(tif, breaks=brks,  col = mycolours, useRaster=TRUE, colNA=NA, main = "", 
       xlab = "UTM Westing Coordinate (m)",
       ylab = "UTM Northing Coordinate (m)",
       legend.width=1, legend.shrink=0.75,
       axis.args=list(at=seq(minval, maxval, by=(maxval/4)),
                      labels=seq(minval, maxval, by=(maxval/4)), 
                      cex.axis=0.6),
       legend.args=list(text=legtext, side=4, font=2, line=2.5, cex=0.4))
  title(main=title, line=0.75, cex.main = 0.75)
  #mtext(text=title ,side=3,line=1,outer=FALSE, cex = 0.5)
  legend("topright", legend=c(format(date1,"%H:%M:%S")), cex=0.75, bty="n")
  for (i in 1:length(locs))
    {
      # symbols(locations_table[[i]][[2]],locations_table[[i]][[3]], squares = 2*locations_table[[i]][[4]]*as.numeric(space_aggregation), add = TRUE, inches= FALSE)  #S1
      # symbols(locations_table[[i]][[2]],locations_table[[i]][[3]], circles = (xmax(tif)-xmin(tif))/100, add = TRUE, inches= FALSE) #S2
      symbols(locs[[i]][[2]],locs[[i]][[3]], circles = 1000, add = TRUE, inches= FALSE) #S3
    }
  dev.off()
  #(tif, main = title, interpolate = FALSE, xlab = "UTM Westing Coordinate (m)",ylab = "UTM Northing Coordinate (m)")
}


###EL PASO DIFFERENCE ANALYSIS
FullDeltatiffanalysis<- function(var)
{
  outpath <-paste(path2, "delta_",SD1,"_",SD2, "/",var, "/El Paso", sep='')
  dir.create(outpath, showWarnings = FALSE)
  dir.create(paste(outpath,"/","tiff", sep = ""), showWarnings = FALSE)
  dir.create(paste(outpath,"/","png", sep = ""), showWarnings = FALSE)
  #go through each time period

  for (i in 1 : timelength)
  {
    #costruct filename base vs improved
    time <- (12+6*starttime+i)*as.numeric(time_aggregation)
    filename1<-paste(basename(path1),"_", var, "_", time, sep='')
    filename2<-paste(basename(path2),"_", var, "_", time, sep='')
    str_name1<-paste(path1, filename1,".tif",sep='')
    str_name2<-paste(path2, filename2,".tif",sep='')
    
    #open first raster#open second raster#subtract
    ras1<-raster(str_name1)
    ras2<-raster(str_name2)
    ras1[ras1[]<0]<- NA 
    ras2[ras2[]< 0]<- NA 
    diff<<- (ras1 - ras2)
    #save to folder
    outtiff <- paste("delta_",SD1,"_", SD2, "_", var, "_Elpaso_", time, ".tif", sep="")
    outtiffpath <- paste(outpath, "/", "tiff","/", outtiff, sep = "")
    writeRaster(diff, outtiffpath, format="GTiff", overwrite=TRUE)
    #save png output
    StandardGISPlot(diff,var, time, "El Paso")
  }
} 
  

#EACH LOCATION DIFFERENCE ANALYSIS
LocationDeltatiffanalysis <- function(var, x)
{
  thisloc <- locs[[x]]
  locname <- thisloc[[1]]
  outpath <-paste(path2, "delta_", SD1,"_", SD2, "/",var, "/",locname, sep='')
  dir.create(outpath, showWarnings = FALSE)
  dir.create(paste(outpath,"/","tiff", sep = ""), showWarnings = FALSE)
  dir.create(paste(outpath,"/","png", sep = ""), showWarnings = FALSE)
  
  #go through each time period
  
  for (i in 1 : timelength)
  {
    time <- (12+6*starttime+i)*as.numeric(time_aggregation)
    filename1<-paste(basename(path1),"_", var, "_", time, sep='')
    filename2<-paste(basename(path2),"_", var, "_", time, sep='')
    str_name1<-paste(path1, filename1,".tif",sep='')
    str_name2<-paste(path2, filename2,".tif",sep='')
    
    #open first raster#open second raster#subtract
    ras1<-raster(str_name1)
    ras2<-raster(str_name2)
    ras1[ras1[]<0]<- NA 
    ras2[ras2[]<0]<- NA 
    diff<<- (ras1 - ras2)
    cellsize<-as.numeric(space_aggregation)
    tlx = thisloc[[2]]-thisloc[[4]]*cellsize
    brx = thisloc[[2]]+thisloc[[4]]*cellsize
    tly = thisloc[[3]]-thisloc[[4]]*cellsize
    bry = thisloc[[3]]+thisloc[[4]]*cellsize
    cropbox <-c(tlx,brx,tly, bry)
    crop <- crop(diff, cropbox) 
    #save to folder
    outtiff <- paste("delta_", SD1,"_", SD2, "_", var, "_",locname, "_", time, ".tif", sep="")
    outtiffpath <- paste(outpath, "/", "tiff","/", outtiff, sep = "")
    writeRaster(crop, outtiffpath, format="GTiff", overwrite=TRUE)
    #save png output
    StandardGISPlot(crop,var, time, thisloc)
  }
}
  

###THE MAIN FUNCTION
Main<- function()
{
  SD1 <<- getanalysisdetails(path1)
  SD2 <<- getanalysisdetails(path2)
  SetupLocationsTable()
  dir.create(paste(path2, "delta_",SD1,"_", SD2, sep=''), showWarnings = FALSE)
  timelength <<- ((endtime-starttime)*60)/as.numeric(time_aggregation)
  variables <- c("CO2", "SPEED", "VMT")#variables of study
  # variables <- c("VMT")
  for(i in 1:length(variables))
  {
    var <<-  variables[[i]]
    dir.create(paste(path2, "delta_",SD1,"_",SD2, "/",var, sep=''), showWarnings = FALSE)
    plotdetails(var)
    FullDeltatiffanalysis(var)
    for(x in 1:length(locs))
    {
        LocationDeltatiffanalysis(var, x)
    }
  }
}

premain<-function()
{
  improvedfolders <-c()
  basefolders<-c()
  
  basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2016_10_200/')
  basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2016_10_200/')
  basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2016_10_200/')
  basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2016_10_200/')
  basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2016_10_200/')
  basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2016_10_200/')
  basefolders <- c(basefolders, 'F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2016_10_200/')
  
  improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2016_10_200/')
  improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2016_10_200/')
  improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2016_10_200/')
  improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2016_10_200/')
  improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2016_10_200/')
  improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2016_10_200/')
  improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/Braid 1.40/BRAIDED_140_ES1_2016_10_200/')
  
  # improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2016_10_200/')
  # improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2016_10_200/')
  # improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2016_10_200/')
  # improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2016_10_200/')
  # improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2016_10_200/')
  # improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2016_10_200/')
  # improvedfolders <- c(improvedfolders, 'F:/Raw Exxon Data/X Ramp 1.40/XRAMP_140_ES1_2016_10_200/')
  
  
  for (i in 1:length(improvedfolders))
  {
    improvedfolder <- improvedfolders[[i]]
    basefolder <- basefolders[[i]]
    path1 <- basefolder
    path2 <- improvedfolder
    Main()
  }
}

Main()
