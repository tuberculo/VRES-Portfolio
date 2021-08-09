if (!exists("ExecParam")) {## Initialize results tibble.
  ExecParam <- tibble(idExec = numeric(), #
                      Notes = character(), # Generic notes
                      Weight = logical(), # True if the model is by relative weight
                      Samples = numeric(), # How many samples were used to CVaR
                      NSimulations = numeric(), # How many points in the curve
                      Beta = numeric(), omega = numeric(), # Accepted risk, CVaR limit
                      isSamp = logical(), # Are samples used?
                      ExecTime = numeric()) # Execution time
  MainResults <- tibble()
  PlantResults <- tibble()
}
# Run optimization -----------------------------------------------------------------

x <- RunModel(Series, ModelFile, DataFile, NRuns)

Results <- ReadOutput(x[[1]], Plants) # Valores numa só coluna.
ResultsRow <- pivot_wider(Results, names_from = Variable, values_from = Value) # Each row has a sim result.

idExe <- pmax(max(ExecParam$idExec) + 1, 1)
ExecParam <- add_row(ExecParam, idExec = idExe, NSimulations = NRuns, Beta = beta, omega = omega, 
                     Weight = ByWeight, ExecTime = as.numeric(x[[2]], units = "mins"),
                     isSamp = as.logical(RiskSampFlag), 
                     Notes = Notes, Samples = nrow(Y) * RiskSampFlag)
MainResults <- bind_rows(MainResults, mutate(select(ResultsRow, -any_of(c(Plants, 'Beta', 'omega'))), 
                                             idExec = idExe, .before = Sim)) # Remove columns that should be in ExecParam
PlantResultsTemp <- mutate(pivot_longer(select(ResultsRow, Sim, Plants), Plants, names_to = "Plant",
                                        values_to = "Value"), idExec = idExe, .before = Sim)
PlantResultsTemp <- left_join(PlantResultsTemp, select(PlantData, name, Type, UF, `Storage capacity`, CostMWh), by = c("Plant" = "name"))
PlantResults <- bind_rows(PlantResults, PlantResultsTemp)

