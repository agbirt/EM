###TO CLEAR THE GLOBAL ENVIRONMENT
rm(list = ls())

inputpath <- 'E:/Raw Data- 170216/Braided/Braided 1.0/BASE_100_ES1_2016_10_200/'

setwd(inputpath)

hours <- 26
#################END GLOBALS


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



###ANALYSIS OF TEXT FILES FOR A CERTAIN VARIABLE
Voltable_create <- function(var)
{
  filename <- paste(strategy, demand,emission_scenario,year,time_aggregation,space_aggregation,"VMT", sep = "_")
  filepath <- paste(inputpath,filename,".txt", sep = "")
  
  VMTtable <<- read.delim(filepath)
  Voltable <<- VMTtable
  
  length <- (hours*60) / as.numeric(time_aggregation)

  for (i in 1:length)
  {
   Voltable[,i+6] <<- VMTtable[,i+6]/VMTtable[,3] 
  }
}



###THE MAIN FUNCTION WHICH RUNS ALL THE PROGRAM
Main <- function() 
{
  getanalysisdetails()
  Voltable_create()
  tablename <- paste(strategy, demand,emission_scenario,year,time_aggregation,space_aggregation,"VOLUME", sep = "_") 
  write.table(Voltable,paste(inputpath, tablename,".txt",sep = ""), sep = "\t", row.names = FALSE)
}

Main()


