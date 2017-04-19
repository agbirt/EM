rm(list = ls())
total <<- data.frame()
###GET DETAILS FROM FOLDER SCENARIO
getanalysisdetails <- function(inputpath) 
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

SummarizeResults <- function()
{
  variable = "CO2"
  filename = "Analysis/BASE_100_ES1_2016_10_200_CO2.txt"
  paths <- c()
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.0/BASE_100_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.1/BASE_110_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.2/BASE_120_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.25/BASE_125_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.3/BASE_130_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.35/BASE_135_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Base 1.4/BASE_140_ES1_2048_20_200/" )
  
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.0/BRAIDED_100_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.1/BRAIDED_110_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.2/BRAIDED_120_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.25/BRAIDED_125_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.3/BRAIDED_130_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.35/BRAIDED_135_ES1_2048_20_200/" )
  # ####
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2016_10_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2024_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2032_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2040_20_200/" )
  # paths = c(paths, "F:/Raw Exxon Data/Braid 1.4/BRAIDED_140_ES1_2048_20_200/" )
  
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2016_10_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2024_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2032_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2040_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.0/XRAMP_100_ES1_2048_20_200/" )
  ####
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2016_10_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2024_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2032_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2040_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.1/XRAMP_110_ES1_2048_20_200/" )
  ####
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2016_10_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2024_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2032_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2040_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.2/XRAMP_120_ES1_2048_20_200/" )
  ####
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2016_10_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2024_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2032_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2040_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.25/XRAMP_125_ES1_2048_20_200/" )
  ####
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2016_10_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2024_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2032_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2040_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.3/XRAMP_130_ES1_2048_20_200/" )
  ####
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2016_10_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2024_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2032_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2040_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.35/XRAMP_135_ES1_2048_20_200/" )
  ####
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2016_10_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2024_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2032_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2040_20_200/" )
  paths = c(paths, "F:/Raw Exxon Data/X Ramp 1.4/XRAMP_140_ES1_2048_20_200/" )
  
  #go through all the paths
  for (i in 1:length(paths))
  {
    getanalysisdetails(paths[[i]])
    filename <- paste(strategy, "_", demand, "_", emission_scenario, "_", year, "_", time_aggregation,"_", space_aggregation,"_", variable, ".txt", sep="")
    file <- paste(paths[[i]], "Analysis/", filename ,sep = "")
    print (file)
    localtable  <- read.delim(file)
    localtable$year <-year
    localtable$demand <-demand
    localtable$filepath <-file
    if (i==1)
    {
      total <<- data.frame(localtable)
    }
    else
    {
      total <<- rbind(total, localtable)
      
    }
    
    
  }
  
  outpath <- paste("F:/Raw Exxon Data/summary_", strategy, ".txt", sep="")
  write.table(total, file = outpath, sep="\t", row.names = FALSE)
}

SummarizeResults()