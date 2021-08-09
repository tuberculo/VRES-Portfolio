# Create columns Wc and Capacity based on Weight parameter
# Capacity: nominal capacity in MW. Wc and Wr: generation over load. RC2L: Capacity over load. GenShare: Generation over total generation (pu). CapShare: Capacity over total capacity (pu).
PlantResults <- left_join(PlantResults, select(PlantData, name, CF, CF.pos), by = c("Plant" = "name")) 
PlantResults <- mutate(left_join(PlantResults, select(ExecParam, idExec, Weight)), 
                       Capacity = case_when(!Weight ~ Value, Weight ~ Value * AveLoad / CF.pos), 
                       Wc = case_when(Weight ~ Value, !Weight ~ Value * CF.pos / AveLoad), .after = Value)
PlantResults <- mutate(PlantResults, Wr = Wc * CF / CF.pos, RelCaptoLoad = Wc / CF.pos, .after = Wc) 
MainResults <- left_join(select(MainResults, !any_of(c("SumWc", "SumWr", "SumRC2L", "SumCapacity"))), 
                         group_by(filter(PlantResults, str_detect(Plant, "Demand", negate = TRUE)), idExec, Sim) %>% 
                           summarise(SumWc = sum(Wc), SumWr = sum(Wr), SumRC2L = sum(RelCaptoLoad), 
                                     SumCapacity = sum(Capacity)), by = c("idExec", "Sim"))
# Values both for weight and capacity. Keep original values 
MainResults <- mutate(left_join(MainResults, select(ExecParam, idExec, Weight)), 
                      W_Cost = case_when(!Weight ~ Cost / 8760 / AveLoad * 1e6, Weight ~ Cost), 
                      W_Expected_VaR = case_when(!Weight ~ Expected_VaR / AveLoad, Weight ~ Expected_VaR),
                      W_Variance = case_when(!Weight ~ Variance / (AveLoad^2), Weight ~ Variance),
                      W_SD = case_when(!Weight ~ Standard_deviation / AveLoad, Weight ~ Standard_deviation),
                      Cap_Cost = case_when(!Weight ~ Cost, Weight ~ Cost * 8760 * AveLoad / 1e6), 
                      Cap_Expected_VaR = case_when(!Weight ~ Expected_VaR, Weight ~ Expected_VaR * AveLoad),
                      Cap_Variance = case_when(!Weight ~ Variance, Weight ~ Variance * (AveLoad^2)),
                      Cap_SD = case_when(!Weight ~ Standard_deviation, Weight ~ Standard_deviation * AveLoad)
) %>% select(-Weight) %>% relocate(Cost, Expected_VaR, Standard_deviation, Variance, .after = last_col())
PlantResults <- left_join(MainResults, PlantResults) %>% 
  mutate(GenShare = Wc / SumWc, CapShare = RelCaptoLoad / SumRC2L, .after = RelCaptoLoad) %>% 
  select(colnames(PlantResults), CapShare, GenShare) %>% 
  relocate(CapShare, GenShare, .after = RelCaptoLoad)
PlantResults <- select(PlantResults, -Weight, -CF, -CF.pos)
MainResults <- CalcResulMCVaR(Serie = Series)
MainResults <- mutate(left_join(MainResults, select(ExecParam, idExec, Weight)), 
                      W_VaR = case_when(!Weight ~ VaR / AveLoad, Weight ~ VaR), 
                      W_CVaR = case_when(!Weight ~ CVaR / AveLoad, Weight ~ CVaR),
                      Cap_VaR = case_when(!Weight ~ VaR, Weight ~ VaR * AveLoad), 
                      Cap_CVaR = case_when(!Weight ~ CVaR, Weight ~ CVaR * AveLoad),
) %>% select(-Weight) %>% relocate(W_VaR, W_CVaR, .after = W_Expected_VaR) %>% relocate(Cap_VaR, Cap_CVaR, .after = Cap_Expected_VaR)
#Calculate HHI
HHI <- group_by(PlantResults, idExec, Sim) %>% 
  filter(str_detect(Plant, "Demand", negate = TRUE)) %>% 
  summarise(HHIcap = sum(CapShare^2), HHIgen = sum(GenShare^2))
MainResults <- left_join(MainResults %>% select(!contains("HHI")), HHI, by = c("idExec", "Sim"))
# Calculate geographic diversification 
pr <- group_by(left_join(PlantResults, PlantData, by = c("Plant" = "name")), idExec, Sim) 
MainResults <- left_join(MainResults %>% select(-any_of("GeoDivIndex")), # Remove "GeoDivIndex" column if it already exists.
                         bind_cols(summarise(pr), GeoDivIndex = unlist(group_map(pr, ~CalcGeoDivIndex(.))) / 1000))
# Calculate non-geographic diversification (Euclidean) 
distanceWS <- dist(x = t(Series[-1]))
MainResults <- left_join(MainResults %>% select(-any_of("DivIndex")), # Remove "DivIndex" column if it already exists.
                         bind_cols(summarise(pr), DivIndex = unlist(group_map(pr, ~CalcDivIndex(., DistMat = distanceWS)))))

# Label scenarios
ExecParam <- mutate(ExecParam, OptType = factor(case_when(str_detect(ExecParam$Notes, "MinCost_IsoRisk") ~ "MinCost_IsoRisk",
                                                          str_detect(ExecParam$Notes, "MaxGen_IsoCap") ~ "MaxGen_IsoCap",
                                                          str_detect(ExecParam$Notes, "MinCost_IsoCap") ~ "MinCost_IsoCap",
                                                          str_detect(ExecParam$Notes, "MinCost_IsoGen") ~ "MinCost_IsoGen",
                                                          str_detect(ExecParam$Notes, "MaxGen_IsoCost") ~ "MaxGen_IsoCost",
                                                          str_detect(ExecParam$Notes, "MinCap_IsoGen") ~ "MinCap_IsoGen",)))
CodeIndex <- tribble(~Codename, ~PaperName, ~AvailPlants, ~OptType, ~omega, ~Beta,
                     "A1", "Trad_Flat", "Wind and PV, flat load", "MaxGen_IsoCap", 0, 0.95,
                     "A2", "Trad_Obs", "Wind and PV, real load", "MaxGen_IsoCap", 0, 0.95,
                     "B1", "Cost_Flat", "Wind and PV, flat load", "MinCost_IsoGen", 0, 0.95,
                     "B2", "Cost_Obs", "Wind and PV, real load", "MinCost_IsoGen", 0, 0.95,
                     "C1", "CVaR_Flat", "Wind and PV, flat load", "MinCost_IsoRisk", 0, 0.95,
                     "C2", "CVaR_Obs", "Wind and PV, real load", "MinCost_IsoRisk", 0, 0.95,
                     "B1_LCPV", "Cost_Flat_lcpv", "Wind and PV, flat load, PV half cost", "MinCost_IsoGen", 0, 0.95,
)
ExecParam <- left_join(ExecParam, CodeIndex)

CovarMat1 <- cov(Series[-1]) 
# Calculate variance without demand.
MainResults <- MainResults %>% group_by(idExec, Sim) %>% 
  mutate(W_Variance_no_Load = Calc1Variance(idExec, Sim)) %>% ungroup()

betas <- ListBeta

if (NumThreads == 1) {
  plan(sequential)
} else {
  plan(multisession, workers = NumThreads)
}

MultiplierResults <- select(MainResults, idExec, Sim)

Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = Demand * MaxLoad / AveLoad)
for (i in 1:length(betas)) {
  MultiplierResultsObs <- future_bind_column_Var_CVaR(MultiplierResults, betas[i], Serie = Series)
  MultiplierResultsObs <- future_bind_column_multiplier(MultiplierResultsObs, betas[i], Serie = Series)
}
MultiplierResultsObs <- mutate(MultiplierResultsObs, MultLoadType = "Observed", .after = Sim)

# Series <- Series %>% mutate(OrigDemand = -1 * AveLoad / MaxLoad, FlatDemand = -1)
# for (i in 1:length(betas)) {
#   MultiplierResultsFlat <- future_bind_column_Var_CVaR(MultiplierResults, betas[i], Serie = Series)
#   MultiplierResultsFlat <- future_bind_column_multiplier(MultiplierResultsFlat, betas[i], Serie = Series)
# }
# MultiplierResultsFlat <- mutate(MultiplierResultsFlat, MultLoadType = "Flat", .after = Sim)
plan(sequential)
Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = -1)
#MultiplierResults <- bind_rows(MultiplierResultsObs, MultiplierResultsFlat)
MultiplierResults <- MultiplierResultsObs