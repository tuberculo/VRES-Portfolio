VariancePlant <- cov(Series[-1]) %>% diag()
SDplant <- sqrt(VariancePlant)

NewPaperNames <- tribble(~PaperName, ~PaperName2,
                         "Trad_Flat", "G-Cap^{Flat}",
                         "Trad_Obs", "G-Cap^{Dem}",
                         "Cost_Flat", "C-Gen^{Flat}",
                         "Cost_Obs", "C-Gen^{Dem}",
                         "CVaR_Flat", "C-CVaR^{Flat}",
                         "CVaR_Obs", "C-CVaR^{Dem}",
                         "Cost_Flat_lcpv", "C-Gen[LPV]^{Flat}")
ExecParam <- left_join(ExecParam, NewPaperNames)

## ----PrepareAuxData
# Prepare data
Correl <- correlate(Series[-1] %>% mutate(Demand = -Demand) %>% 
                      select(-any_of(c("FlatDemand", "OrigDemand"))), use = "everything")
plotcorrel <- stretch(Correl) %>%
  left_join(select(PlantData, name, Type.x = Type), by = c("x" = "name")) %>%
  left_join(select(PlantData, name, Type.y = Type), by = c("y" = "name")) %>% drop_na()

CorrelStats <- plotcorrel %>% group_by(Type.x, Type.y) %>% 
  summarise(Min = min(r), Mean = mean(r), Max = max(r), Median = median(r))

## ----PlantsCorrel
unique(plotcorrel$Type.x) -> TypeNames
names(TypeNames) <- TypeNames
TypeNames["Load"] <- "Demand"

ggplot(plotcorrel %>% mutate(Type.y = factor(Type.y, 
                                                  levels = c("PV", "Wind", "Load"),
                                                  labels = c("PV", "Wind", "Demand")))) + 
  geom_histogram(aes(x = r, fill = Type.y, y = after_stat(ncount)), 
                 bins = 40, alpha = 0.75, position = "identity") + 
  facet_wrap(~Type.x, labeller = as_labeller(TypeNames)) + 
  scale_y_continuous() + theme_light() + 
  theme(legend.position = "bottom") + 
  labs(fill = "Component type", x = "Pearson correlation", y = "Normalized count")
ggsave("PlantsCorrel-1.pdf", height = 12, width = 18, units = "cm")

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
ggsave("distXcor-1.pdf", height = 12, width = 18, units = "cm")

## ----CompareFrontier capacity factor
SeleCode <- c("A1", "B1", "B1_LCPV")
PlotDataCompare <- left_join(ExecParam, MainResults) %>% 
               mutate(PortSD = W_SD / SumRC2L) 
ModelName <- PlotDataCompare %>% filter(Codename %in% SeleCode) %>% distinct(PaperName2) %>% deframe()
ColorCodes <- tibble(Code = c("M1", "M2", "M3", "A1", "A2"), 
                     Names = c(ModelName, c("Wind", "PV")))

PlotCompare <- ggplot() +
  geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% 
               filter(Type %in% c("Wind", "PV")) %>% left_join(ColorCodes, by = c("Type" = "Names")),
             mapping = aes(x = SD, y = CF, color = Code)) +
  geom_line(PlotDataCompare %>% filter(Codename %in% SeleCode) %>% left_join(ColorCodes, by = c("PaperName2" = "Names")),
            mapping = aes(x = PortSD, y = SumWc / SumRC2L, color = Code), 
            size = 0.5) + 
  geom_point(PlotDataCompare %>% 
               filter(Codename == "B1") %>% filter(PortSD == min(PortSD)), 
             mapping = aes(x = PortSD, y = SumWc / SumRC2L)) + 
  geom_label_repel(PlotDataCompare %>% 
                     filter(Codename == "B1") %>% 
                     filter(PortSD == min(PortSD)), mapping = aes(x = PortSD, y = SumWc / SumRC2L, label = "MinCV")) + 
  labs(x = "Normalized standard deviation", y = "Capacity factor", color = "Models and\ntechnologies") + theme_light() +
  scale_color_brewer(palette = "Set1",
                     labels = parse(text = deframe(ColorCodes)),
                     breaks = ColorCodes$Code, 
                     guide = guide_legend(override.aes = # 
                                            list(shape = c(rep(NA, length(ModelName)),
                                                           rep(16, length(c("Wind", "PV")))),
                                                 linetype = c(rep(1, length(ModelName)),
                                                              rep(NA, length(c("Wind", "PV"))))))) +
  scale_x_continuous(labels = scales::percent) + scale_y_log10(labels = scales::percent) 

PlotCompare + theme(legend.position = "none")
ggsave("CompareFrontierCF.pdf", height = 12, width = 18, units = "cm", scale = 0.75)
PlotCompare + coord_cartesian(xlim = c(0.1, 0.23), ylim = c(0.34, 0.535))
ggsave("CompareFrontierCF_zoom.pdf", height = 12, width = 18, units = "cm", scale = 0.75)

## ----CompareFrontierCost
SeleCode <- c("A1", "B1")
ModelName <- PlotDataCompare %>% filter(Codename %in% SeleCode) %>% distinct(PaperName2) %>% deframe()
PlotCompare <- ggplot() +
  geom_point(left_join(PlantData, SDplant %>% enframe(value = "SD")) %>% filter(Type %in% c("Wind", "PV")) %>% 
               left_join(ColorCodes, by = c("Type" = "Names")),
                      mapping = aes(x = SD, y = CostMWh, color = Code)) +
  geom_path(PlotDataCompare %>% filter(Codename %in% SeleCode) %>% left_join(ColorCodes, by = c("PaperName2" = "Names")),
            mapping = aes(x = PortSD, y = W_Cost / SumWc, color = Code), 
            size = 0.5) +
  geom_point(PlotDataCompare %>% 
               filter(Codename == "B1") %>% filter(PortSD == min(PortSD)), 
             mapping = aes(x = PortSD, y = W_Cost / SumWc)) + 
  geom_label_repel(PlotDataCompare %>% 
               filter(Codename == "B1") %>% 
               filter(PortSD == min(PortSD)), mapping = aes(x = PortSD, y = W_Cost / SumWc, label = "MinCV")) + 
  labs(x = "Normalized standard deviation", y = "Cost (BRL/MWh)", color = "Models and\ntechnologies") + theme_light() +
  scale_color_brewer(palette = "Set1",
                     labels = parse(text = deframe(ColorCodes)),
                     breaks = ColorCodes$Code, 
                     guide = guide_legend(override.aes = # 
                                            list(shape = c(rep(NA, length(ModelName)),
                                                           rep(16, length(c("Wind", "PV")))),
                                                 linetype = c(rep(1, length(ModelName)),
                                                              rep(NA, length(c("Wind", "PV"))))))) +
  scale_x_continuous(labels = scales::percent)

PlotCompare + theme(legend.position = "none")
ggsave("CompareFrontierCost.pdf", height = 12, width = 18, units = "cm", scale = 0.75)
PlotCompare + coord_cartesian(xlim = c(0.1, 0.23), ylim = c(121, 185))
ggsave("CompareFrontierCost_zoom.pdf", height = 12, width = 18, units = "cm", scale = 0.75)

## ----CompareRunsSidebySide
PlotCodenames <- c("A1", "B1", "B1_LCPV")
PlotIds <- ExecParam[match(PlotCodenames, ExecParam$Codename), "idExec"][[1]]
#Paper names
PlotCodenames <- ExecParam[match(PlotCodenames, ExecParam$Codename), "PaperName2"][[1]]
names(PlotCodenames) <- PlotIds

PlotData <- PlotCompGeneric(PlotIds, UseSimilar = TRUE, correction = 1, MR = mutate(MainResults, Rel_SD = W_SD / SumRC2L), By = "Rel_SD", n = 5)

SimiPanLabel <- PlotData %>% filter(idExec %in% PlotIds[-1]) %>% 
  mutate(Rel_SD = scales::label_percent(accuracy = 0.1)(round(Rel_SD, digits = 3))) %>% 
  select(SimiPan, Rel_SD) %>% distinct() %>% deframe()

ggplot(PlotData) + geom_histogram(aes(weight = CapShare, fill = Type, x = CF.pos), bins = 10) + 
  labs(x = "Power plant capacity factor", y = "Share of portfolio capacity") + 
  scale_x_continuous(labels = scales::label_percent(accuracy = 1.0), guide = guide_axis(check.overlap = TRUE)) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  facet_grid(idExec~SimiPan, labeller = labeller(SimiPan = SimiPanLabel, 
                                                 idExec = as_labeller(PlotCodenames, default = label_parsed))) +
  theme_light()
ggsave("CompareRunsSidebySide-1.pdf", height = 12, width = 18, units = "cm")


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
ModelName <- c(MaxGen_IsoCap = "G-Cap",
               MinCost_IsoGen = "C-Gen",
               MinCost_IsoRisk = "C-CVaR")
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
  scale_y_continuous(labels = scales::percent) + scale_colour_discrete(labels = c("Flat demand", "Observed demand")) + 
  facet_wrap(~OptType, scales = "free_x", labeller = labeller(OptType = as_labeller(ModelName, default = label_parsed))) + 
  theme(legend.position = "bottom")
ggsave("PVShareDemand-1.pdf", height = 12, width = 18, units = "cm")


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

ggplot(left_join(ExecParam, MainResults) %>% 
         filter(idExec %in% Selec), aes(x = W_SD / SumRC2L * 100, y = SumWc / SumRC2L * 100,
                  color = PaperName2)) + geom_path() + theme_light() +
  labs(x = "Normalized standard deviation (%)", y = "Capacity factor (%)", color = "Model") +
  scale_color_discrete(labels = scales::label_parse()) +
  theme(legend.position = "bottom")
ggsave("FrontierRisk-1.pdf", height = 12, width = 18, units = "cm")


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

ggplot(left_join(ExecParam, MainResults) %>% filter(idExec %in% Selec) %>% 
         mutate(InvHHI = 1 / HHIgen) %>% 
         pivot_longer(c(GeoDivIndex, DivIndex, InvHHI), 
                      names_to = "diversiType", 
                      values_to = "diversiValue")) +
  geom_path(aes(x = SumWc / SumRC2L * 100, y = diversiValue, color = AvailPlants)) + 
  facet_grid(diversiType~OptType, scales = "free_y", 
             labeller = labeller(diversiType = IndexName, 
                                 OptType = ModelName, .default = label_parsed)) + 
  theme(legend.position = "bottom") + 
  scale_colour_discrete(labels = c("Flat demand", "Observed demand")) + 
  labs(x = "Portfolio capacity factor (%)", 
       y = "Portfolio diversity index", color = "Demand type") + theme_light()
ggsave("Diversification-1.pdf", height = 12, width = 18, units = "cm")

## ----IsoRiskMultiplier
beta <- 0.95
Risk <- 1 - beta
riskname <- sym(paste0("SameRiskMulti_", Risk * 100, "%"))
LoadMName <- c("Wind and PV, flat load" = "Flat demand", "Wind and PV, real load" = "Observed demand")
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
  scale_color_discrete(labels = parse(text = ModelName)) 
ggsave("IsoRiskMultiplier-1.pdf", height = 12, width = 18, units = "cm")

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
balances <- map_dfr(1:nrow(x), # Balance with adjusted capacity
                    ~CalcResul1Balance(x[[., "idExec"]], 
                                       x[[., "Sim"]], 
                                       Series, 
                                       x[[., "SameRiskMulti_5%"]])) %>% 
  rename(BalanceSame = Balance)
balances <- left_join(balances, map_dfr(1:nrow(x), ~CalcResul1Balance(x[[., "idExec"]], # Balance with original capacity
                                                                      x[[., "Sim"]],
                                                                      Series)) %>% 
                        rename(BalanceOrig = Balance))
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
              group_by(idExec, OptType, AvailPlants, Codename, PaperName2, Sim) %>% 
              summarise(MeanBal = mean(BalanceSame), 
                        yvalue = CalcCumDens(BalanceSame / AveLoad)))

ggplot(left_join(balances, ExecParam), aes(x = BalanceSame / AveLoad, color = factor(Sim))) +
  geom_density() +
  geom_point(aes(x = percentile5 / AveLoad, color = factor(Sim), y = yvalue), SummaryData) +
  geom_vline(aes(xintercept = MeanBal / AveLoad, color = factor(Sim)), linetype = 2, SummaryData) + 
  geom_vline(aes(xintercept = 0), alpha = 0.3) + 
  geom_label_repel(aes(x = MeanBal / AveLoad, label = scales::percent(MeanBal / AveLoad, accuracy = 1), y = 0.4), SummaryData) +
  facet_grid(PaperName2~., labeller = label_parsed) + theme_light() + 
  labs(x = "Balance", y = "Density probability", color = "Standard deviation") +
  scale_color_discrete(labels = SimuNames) + theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::label_percent()) + 
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) 
ggsave("EnergyBalanceDistribution-1.pdf", height = 12, width = 18, units = "cm")
