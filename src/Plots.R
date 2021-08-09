VariancePlant <- cov(Series[-1]) %>% diag()
SDplant <- sqrt(VariancePlant)

## ----PrepareAuxData
# Prepare data
Correl <- correlate(Series[-1] %>% mutate(Demand = -Demand) %>% 
                      select(-any_of(c("FlatDemand", "OrigDemand"))), use = "everything")
plotcorrel <- stretch(Correl) %>%
  left_join(select(PlantData, name, Type.x = Type), by = c("x" = "name")) %>%
  left_join(select(PlantData, name, Type.y = Type), by = c("y" = "name")) %>% drop_na()

CorrelStats <- plotcorrel %>% group_by(Type.x, Type.y) %>% 
  summarise(Min = min(r), Mean = mean(r), Max = max(r), Median = median(r))

# Initialize .pdf file -----------------------------------------------------
pdf("Figures.pdf", width = 0, height = 0, paper = "a4r")

## ----PlantsCorrel
p <- ggplot(plotcorrel) + geom_histogram(aes(x = r, fill = Type.y), bins = 40)

p + facet_wrap(~Type.x, scales = "free_y") + scale_y_continuous() + theme_light() + 
  theme(legend.position = "bottom") + 
  labs(fill = "Component type", x = "Pearson correlation", y = "Number of locations")

## ----distXcor

# Distance scatterplot
distance <- as_cordf(distm(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used,
                                     c("long", "lat")]))
colnames(distance) <- c(colnames(distance[1]),
                                 deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")]))
distance$term <- deframe(PlantData[PlantData$Type %in% c("Wind", "PV") & PlantData$Used, c("name")])
distance <- stretch(distance) %>% mutate(dist = r / 1000, .keep = "unused")
# Compare only by the same type (wind to wind and PV to PV)
left_join(plotcorrel, distance) %>% distinct(r, dist, .keep_all = TRUE) %>% filter(Type.x == Type.y) %>%
    ggplot(aes(x = dist, y = r, fill = Type.x)) + 
  geom_point(shape = 21, stroke = 0.1) + geom_smooth(aes(color = Type.x)) + 
  labs(x = "Distance (km)", y = "Pearson correlation", fill = "Technology", color = "Technology") + 
  xlim(0, NA) + theme_light()

## ----CompareFrontierCost
SeleCode <- c("A1", "B1", "B1_LCPV")
PlotDataCompare <- left_join(ExecParam, MainResults) %>% 
               mutate(PortSD = W_SD / SumRC2L) 

PlotCompare <- ggplot() +
    geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% 
                 filter(Type %in% c("Wind", "PV")),
               mapping = aes(x = SD, y = CF, color = Type)) +
    geom_line(PlotDataCompare %>% filter(Codename %in% SeleCode),
              mapping = aes(x = PortSD, y = SumWc / SumRC2L, color = PaperName), 
              size = 0.5) + 
  geom_point(PlotDataCompare %>% 
               filter(Codename == "B1") %>% filter(PortSD == min(PortSD)), 
             mapping = aes(x = PortSD, y = SumWc / SumRC2L)) + 
  geom_label_repel(PlotDataCompare %>% 
               filter(Codename == "B1") %>% 
               filter(PortSD == min(PortSD)), mapping = aes(x = PortSD, y = SumWc / SumRC2L, label = "MinCV")) + 
    labs(x = "normalized standard deviation (%)", y = "Capacity factor (%)", color = "Models and technologies") + theme_light() +
    scale_color_brewer(palette = "Dark2") + scale_x_continuous(labels = scales::percent) + scale_y_log10(labels = scales::percent) 

PlotCompare + theme(legend.position = "none")
PlotCompare + coord_cartesian(xlim = c(0.1, 0.23), ylim = c(0.34, 0.535))

## ----CompareFrontierCost2
SeleCode <- c("A1", "B1")
PlotCompare <- ggplot() +
  geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% filter(Type %in% c("Wind", "PV")),
                      mapping = aes(x = SD, y = CostMWh, color = Type)) +
  geom_path(PlotDataCompare %>% filter(Codename %in% SeleCode),
            mapping = aes(x = PortSD, y = W_Cost / SumWc, color = PaperName), 
            size = 0.5) +
  geom_point(PlotDataCompare %>% 
               filter(Codename == "B1") %>% filter(PortSD == min(PortSD)), 
             mapping = aes(x = PortSD, y = W_Cost / SumWc)) + 
  geom_label_repel(PlotDataCompare %>% 
               filter(Codename == "B1") %>% 
               filter(PortSD == min(PortSD)), mapping = aes(x = PortSD, y = W_Cost / SumWc, label = "MinCV")) + 
  labs(x = "normalized standard deviation (%)", y = "Cost (BRL/MWh)", color = "Models and technologies") + theme_light() +
  scale_color_brewer(palette = "Dark2") + scale_x_continuous(labels = scales::percent)

PlotCompare + theme(legend.position = "none")
PlotCompare + coord_cartesian(xlim = c(0.1, 0.23), ylim = c(121, 185))

## ----CompareRunsSidebySide
PlotCodenames <- c("A1", "B1", "B1_LCPV")
PlotIds <- ExecParam[match(PlotCodenames, ExecParam$Codename), "idExec"][[1]]
#Paper names
PlotCodenames <- ExecParam[match(PlotCodenames, ExecParam$Codename), "PaperName"][[1]]
names(PlotCodenames) <- PlotIds

PlotData <- PlotCompGeneric(PlotIds, UseSimilar = TRUE, correction = 1, MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD", n = 5)

SimiPanLabel <- PlotData %>% filter(idExec %in% PlotIds[-1]) %>% 
  mutate(Rel_SD = scales::label_percent(accuracy = 0.1)(round(Rel_SD, digits = 3))) %>% 
  select(SimiPan, Rel_SD) %>% distinct() %>% deframe()

ggplot(PlotData) + geom_histogram(aes(weight = CapShare, fill = Type, x = CF.pos), bins = 10) + 
  labs(x = "Power plant capacity factor", y = "Share of portfolio capacity") + 
  scale_x_continuous(labels = scales::label_percent(accuracy = 1.0), guide = guide_axis(check.overlap = TRUE)) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  facet_grid(idExec~SimiPan, labeller = labeller(SimiPan = SimiPanLabel, idExec = PlotCodenames)) +
  theme_light()

## ----PVShareDemand
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" 
         | OptType == "MaxGen_IsoCap" 
         | OptType == "MinCost_IsoGen" 
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind and PV, real load"
         | AvailPlants == "Wind and PV, flat load"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By omega
  ) %>% pull(idExec) %>% unique()
# PV share with and without load.
ModelName <- c(MaxGen_IsoCap = "Trad",
               MinCost_IsoGen = "Cost",
               MinCost_IsoRisk = "CVaR")
PVshare <- filter(left_join(ExecParam, left_join(MainResults, PlantResults)), 
                  str_detect(Plant, "Demand", negate = TRUE), idExec %in% Selec) %>% 
  group_by(idExec, OptType, AvailPlants, Sim, W_Variance_no_Load, 
           W_SD, SumRC2L, Type, Codename) %>% 
  summarise(SumCapShare = sum(CapShare), 
            SumGenShare = sum(GenShare)) %>% 
  filter(Type == "PV")
ggplot(PVshare, aes(x = sqrt(W_Variance_no_Load) / SumRC2L, 
                    y = SumCapShare, color = AvailPlants)) + 
  geom_line() + theme_light() + 
  labs(x = "Normalized standard deviation (plants only)", y = "PV capacity share in portfolio", 
       linetype = "Load", color = "Demand type") + scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent) + scale_colour_discrete(labels = c("Flat", "Observed")) + 
  facet_wrap(~OptType, scales = "free_x", labeller = labeller(OptType = ModelName)) + 
  theme(legend.position = "bottom")

## ----FrontierRisk
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" 
         | OptType == "MaxGen_IsoCap" 
         | OptType == "MinCost_IsoGen" 
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind and PV, flat load"
         #| AvailPlants == "Wind and PV, flat load, PV half cost"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By omega
  ) %>% pull(idExec) %>% unique()

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec), aes(x = W_SD / SumRC2L * 100, y = SumWc / SumRC2L * 100,
                  color = PaperName)) + geom_path() + theme_light() +
  labs(x = "Normalized standard deviation (%)", y = "Capacity factor (%)", color = "Model") +
  theme(legend.position = "bottom")

## ----Diversification
Selec <- ExecParam %>%
  filter(OptType == ""                 #By OptType
         | OptType == "MinCost_IsoRisk" 
         | OptType == "MaxGen_IsoCap" 
         | OptType == "MinCost_IsoGen" 
         , AvailPlants == ""           # By load type     
         | AvailPlants == "Wind and PV, real load"
         | AvailPlants == "Wind and PV, flat load"
         , Beta %in% c(0.95) # By Beta
         , omega %in% c(0) # By omega
  ) %>% pull(idExec) %>% unique()
IndexName <- c(DivIndex = "ED",
               GeoDivIndex = "GD",
               HHIgen = "HHI",
               InvHHI = "HHI^-1")
ModelName <- c(MaxGen_IsoCap = "Trad",
               MinCost_IsoGen = "Cost",
               MinCost_IsoRisk = "CVaR")

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec) %>% 
         mutate(InvHHI = 1 / HHIgen) %>% 
         pivot_longer(c(GeoDivIndex, DivIndex, InvHHI), 
                      names_to = "diversiType", 
                      values_to = "diversiValue")) +
geom_path(aes(x = SumWc / SumRC2L * 100, y = diversiValue, color = AvailPlants)) + 
  facet_grid(diversiType~OptType, scales = "free_y", 
             labeller = labeller(diversiType = as_labeller(IndexName, default = label_parsed), 
                                 OptType = ModelName)) + 
  theme(legend.position = "bottom") + 
  scale_colour_discrete(labels = c("Flat", "Observed")) + 
  labs(x = "Portfolio capacity factor (%)", 
       y = "Portfolio diversity index", color = "Load type") + theme_light()


## ----IsoRiskMultiplier
beta <- 0.95
Risk <- 1 - beta
riskname <- sym(paste0("SameRiskMulti_", Risk * 100, "%"))
LoadMName <- c("Wind and PV, flat load" = "Flat load", "Wind and PV, real load" = "Observed load")
ggplot(left_join(ExecParam, MainResults) %>% 
         select(-any_of(colnames(MultiplierResults %>% ungroup() %>% 
                                   select(-c(idExec, Sim))))) %>% left_join(MultiplierResults) %>% 
         filter(idExec %in% Selec, MultLoadType == "Observed"), 
       aes(x = sqrt(W_Variance_no_Load) * !!riskname, y = W_Cost * !!riskname, 
           color = OptType)) + 
  geom_path() + theme_light() + 
  theme(legend.position = "bottom") + scale_y_log10(limits = c(NA, 800)) + 
  scale_x_log10(limits = c(NA, 3.00), labels = scales::percent) + 
  facet_grid(~AvailPlants, labeller = labeller(AvailPlants = LoadMName)) + 
  labs(x = "Standard deviation", y = "Cost per MWh of load (BRL/MWh)", color = "Model type") + 
  scale_color_discrete(labels = ModelName) 

## ----EnergyBalanceDistribution
PlotCodenames <- c("A2", "B2", "C2")
PlotIds <- ExecParam[match(PlotCodenames, ExecParam$Codename), "idExec"][[1]]
names(PlotCodenames) <- PlotIds
Series <- Series %>% mutate(OrigDemand = Demand, FlatDemand = -1)
x <- left_join(ExecParam, MainResults) %>% left_join(MultiplierResults %>% filter(MultLoadType == "Observed")) %>% 
  filter(idExec %in% PlotIds, Sim %in% c(1, 25, 51)) %>% 
  select(idExec, Sim, `SameRiskMulti_5%`)
SimuNames <- c(`1` = "Lowest",
               `25` = "Intermediate",
               `51` = "Highest")
# Calculate balances
balances <- map_dfr(1:nrow(x), 
                    ~CalcResul1Balance(x[[., "idExec"]], 
                                       x[[., "Sim"]], 
                                       Series, 
                                       x[[., "SameRiskMulti_5%"]])) %>% 
  rename(BalanceSame = Balance)
balances <- left_join(balances, map_dfr(1:nrow(x), ~CalcResul1Balance(x[[., "idExec"]], 
                                                                      x[[., "Sim"]],
                                                                      Series)) %>% rename(BalanceOrig = Balance))
# Percentile, mean shortage, mean positive values:
Probab <- balances %>% group_by(idExec, Sim) %>% 
  summarise(percentile5 = quantile(BalanceSame, 0.05), 
            ShortageValue = mean(pmin(BalanceSame, 0)), 
            ShortageProb = length(BalanceSame[BalanceSame < 0]) / n(), 
            PosiLowValue = mean(pmin(pmax(BalanceSame, 0), percentile5) - pmin(BalanceSame - percentile5, 0) - percentile5), 
            PosiLowProb = length(BalanceSame[BalanceSame >= 0 & BalanceSame < percentile5]) / n(), 
            Low5Mean = mean(BalanceSame[BalanceSame < percentile5]), 
            Short = ShortageValue / ShortageProb, PosiLow = PosiLowValue / PosiLowProb)

SummaryData <- Probab %>% 
  left_join(left_join(balances, ExecParam) %>% 
              group_by(idExec, OptType, AvailPlants, Codename, PaperName, Sim) %>% 
              summarise(MeanBal = mean(BalanceSame), 
                        yvalue = CalcCumDens(BalanceSame / AveLoad)))

ggplot(left_join(balances, ExecParam), aes(x = BalanceSame / AveLoad, color = factor(Sim))) +
geom_density() +
  geom_point(aes(x = percentile5 / AveLoad, color = factor(Sim), y = yvalue), SummaryData) +
  geom_vline(aes(xintercept = MeanBal / AveLoad, color = factor(Sim)), linetype = 2, SummaryData) + 
  geom_vline(aes(xintercept = 0), alpha = 0.3) + 
  geom_label_repel(aes(x = MeanBal / AveLoad, label = scales::percent(MeanBal / AveLoad, accuracy = 1), y = 0.4), SummaryData) +
  facet_grid(PaperName~.) + theme_light() + 
  labs(x = "Balance", y = "Density probability", color = "Standard deviation") +
  scale_color_discrete(labels = SimuNames) + theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::label_percent()) + 
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) 
# Close .ps file
dev.off()
