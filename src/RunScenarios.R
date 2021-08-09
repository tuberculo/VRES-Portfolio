Notes <- ""
beta <- 0
# Make new demand columns
Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = -1)
# Get samples
set.seed(99L)
Y <- GetSamples(Series, NVaRSamples, DUP, FALSE, FALSE)

# Replace "Demand" column with "OrigDemand" data in PlantData tibble 
PlantData <- bind_rows(PlantData, PlantData %>% filter(name == "Demand") %>% 
                         mutate(name = replace(name, name == "Demand", "OrigDemand"), 
                                Used = replace(Used, name == "OrigDemand", FALSE))) 

# Run with observed load. 
rm(CovarMat)
PlantData[!PlantData$Type %in% c("Wind", "PV", "Load"), "Used"] <- FALSE
RunNModes("Wind and PV, real load")

# Rename load column in results
PlantResults <- mutate(PlantResults, Plant = replace(Plant, Plant == "Demand", "OrigDemand"))

# Run with flat load.
Series <- Series %>% mutate(Demand = FlatDemand)
Y <- Y %>% mutate(Demand = FlatDemand)
rm(CovarMat)
MaxLoad <- AveLoad / -colMeans(Series["Demand"])  # Update max load value
# Replace "Demand" column with "FlatDemand" data in PlantData tibble 
PlantData <- bind_rows(PlantData, PlantData %>% filter(name == "Demand") %>% 
                         mutate(name = replace(name, name == "Demand", "FlatDemand"),
                                Used = replace(Used, name == "FlatDemand", FALSE)))
#Update capacity factor data for FlatDemand.
PlantData[PlantData$name == "FlatDemand", c("CF", "CF.pos")] <- list(-AveLoad/MaxLoad, AveLoad/MaxLoad)
PlantData[PlantData$name == "Demand", c("CF", "CF.pos")] <- list(-AveLoad/MaxLoad, AveLoad/MaxLoad)
RunNModes("Wind and PV, flat load")

# Run PV half cost scenario
InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] <- 
  InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] / 2
PlantData <- CalculateCosts(PlantData)

ModelFile <- "src/MinCost_IsoGen.mod"
Notes <- paste0("Wind and PV, flat load, PV half cost", " — ", str_sub(ModelFile, 5, -5))
print(paste0("Running: ", Notes))
source("src/RunAndGetResults.R")
# Restore original cost values.
InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] <- 
  InputCosts[InputCosts$Type == "PV", c("Capex", "OeM", "VCost")] * 2
PlantData <- CalculateCosts(PlantData)
Series <- Series %>% mutate(Demand = OrigDemand)
MaxLoad <- AveLoad / -colMeans(Series["Demand"])  # Revert update max load value

# Rename load column in results
PlantResults <- mutate(PlantResults, Plant = replace(Plant, Plant == "Demand", "FlatDemand"))

ExecParam <- separate(ExecParam, Notes, into = c("AvailPlants", "OptimParam"), sep = " — ", remove = FALSE)
