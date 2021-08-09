# Folder where CPLEX is installed -----------------------------------------

CplexPath <- "/opt/ibm/ILOG/CPLEX_Studio1210/opl/bin/x86-64_linux/"
# Data files --------------------------------------------------------------
SeriesFile <- "data/WindSolar.rds"
PlantDataFile <- "data/Plantdata.rds"
ResultsFile <- "results/Results.rds"

# Threads -----------------------------------------------------------------
# Number of threads to use in functions "future_bind_column_Var_CVaR()" and "future_bind_column_multiplier()").
# This option has no impact on CPLEX.
# Set this value to 1, unless there is enough available RAM. 
NumThreads <- 1

# Use CPLEX? --------------------------------------------------------------
UseCPLEX <- TRUE


# Costs -----------------------------------------------------------------
InputCosts <- tribble(~Type, ~Capex, ~OeM, ~VCost,
                         #       $/kW, $/(kW-y), $/MWh
                         "Wind", 4800, 90, 0,
                         "PV", 3500, 50, 0,
                         "Thermal", 2700, 240, 439, # Not used
                         "ThermalFlat", 3800, 140, 272, # Not used
                         "Storage", 6000, 60, 0 #Not used
)

# Risk parameters --------------------------------------------------------------------
NVaRSamples <- 3000
DUP <- 5 # LHS parameter
RiskSampFlag <- 1
ListBeta <- c(0.95)
ListOmega <- c(0)
DataFile <- "Model.dat"


# Load --------------------------------------------------------------------
AveLoad <- 100000 # Average system load.

# Limits ------------------------------------------------------------------
#  Limits by group
PowerLimits <- tribble(~Type, ~MinPower, ~MaxPower,
                       "Wind", 0, 900000,
                       "PV", 0, 900000,
                       "Thermal", 0, 500000,
                       "Storage", 0, 500000
                       #                      ,"Group1", 0, 100000
                       #                      ,"Group2", 0, 100000
)
WeightLimits <- tribble(~Type, ~MinW, ~MaxW,
                        "Wind", 0, 10,
                        "PV", 0, 10,
                        "Thermal", 0, 10,
                        "Storage", 0, 10
                        #                      ,"Group1", 0, 100000
                        #                      ,"Group2", 0, 100000
)

# Execution options -------------------------------------------------------
ByWeight <- FALSE
NRuns <- 51
NTimeslices <- 1 # Not used