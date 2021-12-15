options(encoding = "UTF-8")

library(tidyverse)
library(corrr)
library(lubridate)
library(lhs)
library(furrr)
library(geosphere)
library(ggrepel)

CPLEX <- !(sum(str_detect(commandArgs(TRUE), regex("noCPLEX", ignore_case = TRUE))) > 0)

# Load and prepare data ---------------------------------------------------
source("src/Functions.R")
source("src/parameters.R")

CPLEX <- CPLEX & UseCPLEX
if (CPLEX) {
  Sys.setenv(PATH = paste(CplexPath, Sys.getenv("PATH"), sep = ":"))
  Sys.setenv(LD_LIBRARY_PATH = paste(CplexPath, Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))  
}

Series <- readRDS(SeriesFile)
PlantData <- readRDS(PlantDataFile)
# Define peak load value.
MaxLoad <- AveLoad / -colMeans(Series["Demand"])  
PlantData <- CalculateCosts(PlantData)
#Capacity constraints (set to high values in order to be inactive)
PlantData <- mutate(PlantData, PMax = 600000)
PlantData <- mutate(PlantData, PMin = 0)
PlantData <- mutate(PlantData, WMax = PMax * CF.pos / AveLoad, WMin = PMin * CF.pos / AveLoad)  # Convert power restrictions to weight restrictions.
PlantData <- mutate(PlantData, GroupWind = if_else(Type == "Wind", 1, 0), GroupPV = if_else(Type == "PV", 1, 0),
                    GroupThermal = if_else(Type == "Thermal" | Type == "ThermalFlat", 1, 0), 
                    GroupStorage = if_else(Type == "Storage", `Storage capacity`, 0))

# Execute optimizations ---------------------------------------------------
if (CPLEX) {
  source("src/RunScenarios.R")
  saveRDS(list(ExecParam = ExecParam, MainResults = MainResults, 
               PlantResults = PlantResults, PlantDataUsed = PlantData, 
               isComplete = FALSE, Parameters = read_lines("src/parameters.R")), 
          ResultsFile)
} else {
  list2env(readRDS(ResultsFile), envir = .GlobalEnv)
}

# Post-optimization calculations ------------------------------------------
if (!get0("isComplete", ifnotfound = FALSE)) { # If results are not complete, i.e., post-calculations are missing, then make calculations. 
  source("src/PostOptmCalc.R")
  SaveResults <- list(ExecParam = ExecParam, MainResults = MainResults, 
                    PlantResults = PlantResults, MultiplierResults = MultiplierResults,
                    PlantDataUsed = PlantData, isComplete = TRUE,
                    Parameters = read_lines("src/parameters.R"))
  saveRDS(SaveResults, ResultsFile)
}

# Make figures ------------------------------------------------------------
source("src/Plots.R", print.eval = TRUE)
