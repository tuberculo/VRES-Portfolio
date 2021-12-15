# Functions -----------------------------------------------------------------

ExportOPL <- function(x, Name, File, string = FALSE, continua = TRUE, AsArray = FALSE){
  # Export data to CPLEX (create or append in a .dat file)
  # Arguments: x: variable to export; Name: variable name in .dat file; File: file name; 
  # string: Export as string?; continua: FALSE = create file, TRUE = append in existing file.
  if (is.vector(x)) {
    if (string) {
      if (is.numeric(x)) {
        texto <- paste(c(paste(Name, "= { "), paste(x, collapse = ", ")," };"), collapse = "")
      } 
      else {
        texto <- paste(c(paste(Name, "= { \""), paste(x, collapse = "\", \""),"\" };"), collapse = "")
      }
    }
    else if (NROW(x) == 1 & !AsArray) {
      texto <- paste(c(paste(Name, "= "), paste(x, collapse = ", "),";"), collapse = "")
    }
    else {
      texto <- paste(c(paste(Name, "= [ "), paste(x, collapse = ", ")," ];"), collapse = "")
    }
  } else {
    if (!is.array(x)) {
      x <- as.matrix(x)
    }
    D <- dim(x)
    L <- length(D) #Matrix dimensions
    valor0 <- paste(c(paste(Name, "= [ ")), collapse = "")
    M <- expand.grid(lapply(D,seq)[L:1]) # List all combinations.
    M <- M[L:1] # 
    N <- expand.grid(lapply(D,seq)[(L - 1):1]) 
    N <- N[(L - 1):1] 
    N <- rbind(N[1,], N, N[nrow(N),])
    N[1,] <- 0 
    N[nrow(N),] <- 0  # Fill last line with 0.
    y <- matrix(x[as.matrix(M)], ncol = (D[L]), byrow = TRUE) 
    # Count how many times the dimension change
    NOpenBrackets  <- rowSums((N - lag(N, 1)) != 0, na.rm = TRUE)
    NCloseBrackets <- rowSums((lead(N, 1) - N) != 0, na.rm = TRUE)
    texto <- sapply(2:(nrow(N) - 1), function(i) paste0(paste0(rep("[", NOpenBrackets[i]), collapse = ""), paste(y[i - 1, ], collapse = ", "), paste0(rep("]", NCloseBrackets[i]), collapse = ""), ","))
    texto <- c(valor0, texto, paste("];", collapse = "")) 
  }
  write.table(texto, File, append = continua, quote = FALSE, row.names = FALSE, col.names = FALSE)
}

Optimize <- function(N = 100, ModFile = "portfolio.mod", DatFile = "portfolio.dat"){
  # Run a scenario
  # N: number of iterations;
  if (file.exists("outputOPL.txt")) file.remove("outputOPL.txt")
  SD <- 99999999999
  VConFlag <- 0 #Deactivate constraints related to variance.
  # Minimize variance to obtain extreme point of the frontier  
  CostFlag <- 0
  print(paste0("Minimize variance", " - ", date()))
  system(sprintf("oplrun -e -D SD_Max=%f -D CostFlag=%d -D VConFlag=%d -D RiskSampFlag=%d %s %s", 
                 SD, CostFlag, VConFlag, RiskSampFlag, ModFile, DatFile, sep = ""))  
  # Minimize objective function without variance constraint to obtain the other extreme point. 
  CostFlag <- 1
  print(paste0("Minimize cost", " - ", date()))
  system(sprintf("oplrun -e -D SD_Max=%f -D CostFlag=%d -D VConFlag=%d -D RiskSampFlag=%d %s %s",
                 SD, CostFlag, VConFlag, RiskSampFlag, ModFile, DatFile, sep = ""))
  Limits <- ReadOutput("outputOPL.txt")
  assign("Pre_resul", Limits, envir = .GlobalEnv)
  file.remove("outputOPL.txt")
  # Lower and upper bound SD
  Limits <- select(filter(Limits, Variable == "Standard_deviation"), Value) 
  SD <- as.double(Limits[1,] * 1.00001) # Add a small tolerance to the first iteration to avoid infeasibeality. 
  #M <- N - 1
  Passo <- as.double((Limits[2,] - SD)/(N - 1)) # Uniform increments in max SD constraint.
  VConFlag <- 1 # Activate constraints related to variance
  for (i in 1:N) {
    cat("Iteration:", i, "- Standard deviation:", SD, sep = " ")
    system(sprintf("oplrun -e -D SD_Max=%f -D CostFlag=%d -D VConFlag=%d -D RiskSampFlag=%d %s %s",
                   signif(SD, 6), CostFlag, VConFlag, RiskSampFlag, ModFile, DatFile, sep = ""))
    SD <- SD + Passo # Increments standard deviation.
    Variancia <- SD^2 
  }
  if (!dir.exists("out")) dir.create("out")
  nome <- paste0("out/OptOut - ","N=", N, " - ", DatFile, " - ", format(Sys.time(), "%Y%m%d_%H-%M-%S"), ".txt")
  file.rename("outputOPL.txt", nome)
  print(paste0("Results saved on file: ", nome))
  nome
}

ReadOutput <- function(OutFile, PlantsName = Plants){
  # Read CPLEX results file.
  te1 <- c(read_file(OutFile))
  # Remove brackets, double spaces and line breaks.
  te2 <- gsub("[][]", "", gsub("[ ]{2,}"," ",gsub("\n","",gsub("\r","",te1))))
  te3 <- read.delim(text = te2, header = F, sep = "*") 
  te4 <- te3 
  te4 <- as_tibble(lapply(te4, function(x) gsub(";","\n", x))) # Turn ";" into line break
  te4[,ncol(te4)] <- NULL # Remove last column (it is empty).
  Nomes_variáveis <- as_tibble(read.delim(text = as.character(te4[ncol(te4)]), 
                                          header = FALSE, sep = "\n")) %>% 
    separate(V1, c("names", "value"))
  Dimen <- dim(Nomes_variáveis)
  Nomes_variáveis <- Nomes_variáveis[,1] # Get names from first column
  te <- NULL
  for (i in 1:(ncol(te4))) {
     te5 <- read.delim(text = as.character(te4[i]), header = F, sep = "\n") %>% 
       separate(V1, c("names", "value"), sep = " ", extra = "merge")
     te5 <- add_column(te5, Sim = i, .before = 1) # Insert Sim number.
     te6a <- select(slice(te5, 1:(NROW(te5) - 1)), 1:3) 
     te6a <- spread(te6a, "names", "value")
     VariáveisSaída <- colnames(te6a[-1]) 
     te6b <- slice(te5, n()) %>% select(-"names") %>% 
       separate(value, into = PlantsName, sep = " ") # Get plants results. 
     te7 <- left_join(te6a ,te6b, "Sim")
     te8 <- as_tibble(gather(te7, VariáveisSaída, PlantsName, key = "Variable", value = "Value"))
     te <- bind_rows(te, te8)
  }
te$Value <- parse_double(te$Value)
te
}

PrepareData <- function(Serie = SeriecomUTE, T. = 1, LoadCol = "Demand") {
  # Create variables: Plants, CF, Cost, MaxPower, MinPower, Groups, GroupLHS, CovarMat
  # Extract names, costs and CF.
  PData <- PlantData[PlantData$Used == TRUE, c("name", "CostMWh", "CF", "CF.pos")] 
  Plants <<- PData[[1]] # Extract names.
  Cost <<- deframe(PData[c(1,2)])
  CF <<- deframe(PData[c(1,3)])
  CF.pos <<- deframe(PData[c(1,4)])
  
  f <- 1 + ByWeight # If "ByWeight" f <- 2
  MaxPower <<- PlantData[PlantData$Used == TRUE, c("PMax", "WMax")][[f]]
  MinPower <<- PlantData[PlantData$Used == TRUE, c("PMin", "WMin")][[f]]
  
  Groups <<- colnames(select(filter(PlantData, Used == TRUE), contains("Group"))) # Get groups names
  GroupLHS <<- t(select(filter(PlantData, Used == TRUE), contains("Group"))) # Which plant is in each group.
  #Carga <<- AveLoad
  Serie <- Serie[c("timedate", PlantData[PlantData$Used == TRUE, c("name")][[1]])]
  DuraPer <- 24/T.
  Serie <- group_by(mutate(Serie, HOUR = hour(timedate), timeslice = floor(HOUR / DuraPer)), timeslice)
  CFs <- summarise(Serie, across(c(-timedate, -HOUR), mean)) # Average capacity factor per timeslice.
  Load <<- -select(CFs, all_of(LoadCol)) * MaxLoad # Average load per timeslice.
  # If CovarMat already exists with the same parameters, skip.
  if (!exists("CovarMat")) CovarMat <- 0
  if (all(CovarMat[[1]] == c(ByWeight, T.))) {
    print(paste0("Skipping covariance calculation", " - ", date()))
  } else {
    print(paste0("Start covariance calculation", " - ", date()))
    CovarMat <- group_map(select(Serie, -timedate, -HOUR), ~cov(.)) # Covariance matrix
    CovarMatN <- map(1:T., ~t(t(CovarMat[[.]])/CF.pos)/CF.pos) # Normalize
    print(paste0("End covariance calculation", " - ", date()))
    CovarMat <- aperm(array(unlist(CovarMat), dim = c(nrow(CovarMat[[1]]), ncol(CovarMat[[1]]), length(CovarMat))),
                          c(3, 1, 2)) # Convert to 3D array, timeslice first.
    CovarMatN <- aperm(array(unlist(CovarMatN), dim = c(nrow(CovarMatN[[1]]), ncol(CovarMatN[[1]]), length(CovarMatN))),
                      c(3, 1, 2)) # Convert to 3D array, timeslice first.
    CovarMat <<- list(c(Weight = ByWeight, NTimeslices = T.), CovarMat, CovarMatN) # Insert metadata
  }
  CFs <<- select(CFs, -timeslice)
}

MakeDatFile <- function(DatFile, T. = T.) {
  # Export all variables needed to run the model in a .dat file.
  print(paste0("Begin file export", " - ", date()))
  ExportOPL(Plants, "Plants", DatFile, TRUE, FALSE)
  ExportOPL(Cost, "Cost", DatFile)
  ExportOPL(1:T., "Timeslice", DatFile, TRUE)
  ExportOPL(Groups, "Groups", DatFile, TRUE) #  Groups names.
  ExportOPL(GroupLHS, "GroupLHS", DatFile)  #  Left-hand side of group restrictions.
  if (ByWeight) {
    ExportOPL(t(t(CFs) / CF.pos), "RelCF", DatFile)
    ExportOPL(deframe(select(WeightLimits, MaxW)), "MaxWGroup", DatFile)  #  Right-hand side of group max power restrictions.
    ExportOPL(deframe(select(WeightLimits, MinW)), "MinWGroup", DatFile)  #  Right-hand side of group min power restrictions.
    ExportOPL(MaxPower, "MaxW", DatFile)
    ExportOPL(MinPower, "MinW", DatFile)
    ExportOPL(CovarMat[[3]], "CovarianceN", DatFile)
  } else {
    ExportOPL(MaxLoad, "MaxLoad", DatFile)
    ExportOPL(CF, "CF", DatFile)
    ExportOPL(CF.pos, "CFpos", DatFile)
    ExportOPL(deframe(select(PowerLimits, MaxPower)), "MaxPGroup", DatFile)  #  Right-hand side of group max power restrictions.
    ExportOPL(deframe(select(PowerLimits, MinPower)), "MinPGroup", DatFile)  #  Right-hand side of group min power restrictions.
    ExportOPL(MaxPower, "MaxPot", DatFile)
    ExportOPL(MinPower, "MinPot", DatFile)
    ExportOPL(CFs, "CFs", DatFile)
    ExportOPL(CovarMat[[2]], "Covariance", DatFile)
  }
  # Matrix with samples. Divided by plant CF if it is byWeight.
  tempY <- matrix(0, nrow = 2, ncol = length(Plants))
  nameY <- "Y"
  if (RiskSampFlag) {
    tempY <- Y[PlantData[PlantData$Used == TRUE, c("name")][[1]]] # Select plants used in the corresponding order.
  }
  if (ByWeight) {
    tempY <- t(t(tempY)/CF.pos)
    nameY <- "Y_N"
  }
  ExportOPL(nrow(tempY), "Mtot", DatFile, FALSE)
  ExportOPL(tempY, nameY, DatFile, FALSE, TRUE)
  ExportOPL(omega, "omega", DatFile, FALSE, TRUE)
}

RunModel <- function(InputData = SeriecomUTE, ModFile = "portfolio.mod",
          DatFile = "portfolio.dat", NSimul = 10, T. = 1, byBeta = FALSE, ...) {
  PrepareData(Serie = InputData, T. = T., ...)
  gc()
  MakeDatFile(DatFile, T. = T.)
  print(paste0("Call solver", " - ", date()))
  if (byBeta) {
    x <- Otimi_change_beta(NSimul, ModFile, DatFile)
  } else {
    ExportOPL(beta, "beta", DatFile, FALSE, TRUE)
    it <- Sys.time()
    x <- Optimize(NSimul, ModFile, DatFile)
    ft <- Sys.time()
    x <- list(x, ft - it)
  }
}

CalcFRC <- function(rate = 0.08, LifeTime = 20) {
  # Capital recovery factor
  rate * (1 + rate)^LifeTime / ((1 + rate)^LifeTime - 1 )
}

CalculateCosts <- function(PD = PlantData) {
  
  # Calculate capital recovery factor
  FRC <- CalcFRC(0.08, 20)
  # Remove existing data
  PD <- select(PD, -any_of(c("Capex", "OeM", "VCost", "FixedCost", "CostMWh")))
  PD <- left_join(PD, InputCosts, by = c("Type"))
  PD <- mutate(PD, FixedCost = ((Capex * FRC + OeM) * 1000) / (CF.pos / replace_na(`Storage capacity`, 1) * 8760), CostMWh = FixedCost + VCost)
  # Define cost for "load plant" as equal to 0.
  PD[PD$Type == "Load", "CostMWh"] <- 0
  PD
}

GetSamples <- function(x = SeriesComplete, n = 1000, dup. = 5, AcceptLowerN = FALSE, UsefastLHS = FALSE) { 
  # Use Latin Hypercube Sampling to get evenly distributed samples in terms of years, months and hours.
  i <- 1
  FirstYear <- year(min(x$timedate))
  nYears <- ceiling(difftime(max(x$timedate), min(x$timedate))/365)
  if (n <= 15000 & !UsefastLHS) { # improvedLHS is too slow for large n, force random if n is greater than 15000.
    LHSsample <- improvedLHS(n, 3, dup = dup.) 
  } else {
    LHSsample <- randomLHS(n, 3)
  }
  repeat {
    distInt <- mutate(as_tibble(t(t(LHSsample) * c(12, 24, nYears))) %>% `colnames<-`(c("Month", "Hour", "Year")), Month = ceiling(Month), Hour = floor(Hour), Year = floor(Year))
    # Remove lines that does not have a match.
    distInt <- filter(distInt, !(Year == (nYears - 1) & Month > month(max(x$timedate))))
    # Count number of lines to resample.
    N_Resample <- n - nrow(distInt)
    if (N_Resample == 0 | AcceptLowerN) {
      break
    }
    print(paste0("Resampling. Iteration: ", i, " - ", date()))
    i <- i + 1
    LHSsample <- augmentLHS(LHSsample, N_Resample)
  }
  distInt <- count(distInt, Month, Hour, Year)
  x <- mutate(x, Month = month(timedate), Hour = hour(timedate), Year = year(timedate))
  # Get samples from each combination of month, hour and year in distInt. 
  Sample_in_group <- function(z) {
    S <- x[x$Month == distInt[[z, 1]] & x$Hour == distInt[[z, 2]] & x$Year == (distInt[[z, 3]] + FirstYear), ]
    S <- S[sample.int(nrow(S), distInt[[z, 4]], TRUE), ]
  }
  Y <- lapply(1:nrow(distInt), Sample_in_group) %>% tibble() %>% unnest(cols = c(.))
  Y <- select(Y, -timedate, -Year, -Hour, -Month)
}

CalcResul1CVaR <- function(i, s, b = beta, Serie = Series, multiplier = 1) {
  Selection <- PlantResults$idExec == i & PlantResults$Sim == s
  PR <- PlantResults[Selection,]
  weight <- filter(ExecParam, idExec == i)$Weight
  Colu <- ifelse(weight, "RelCaptoLoad", "Capacity")
  bal <- as.matrix(Serie[deframe(PR[PR$Type != "Load", "Plant"])]) %*% 
    (deframe(PR[PR$Type != "Load", Colu]) * multiplier) +
    as.matrix(Serie[deframe(PR[PR$Type == "Load", "Plant"])]) %*% 
    (deframe(PR[PR$Type == "Load", Colu])) 

  out <- tibble(`Risk (%)` =  (1 - b) * 100, VaR = quantile(bal, 1 - b, type = 1))
  mutate(out, CVaR = CalcCVaR(bal, b, UpperTail = FALSE))
}

CalcResulMCVaR <- function(result = MainResults, ...) { 
  # Calculate with beta value used in optimization
  func1 <- function(x) {
    if (x %% 100 == 0) print(paste0("line: ", x, "/", nrow(result)))
    bind_cols(idExec = result$idExec[x], Sim = result$Sim[x], 
              CalcResul1CVaR(result$idExec[x], 
                             result$Sim[x], 
                             left_join(result, select(ExecParam, idExec, Beta), by = "idExec")$Beta[x],
                             ...))
  }
  res <- map_dfr(1:nrow(result), func1)
  result[c("VaR", "CVaR")] <- res[c("VaR", "CVaR")]
  result
}
# Find value to multiply installed capacity to achieve a given level of risk.
FindMultiplier <- function(i, s, bs = 0.95, Serie = Series, ...) {
  SubsettoCalc1CVaR <- function(i, s, Serie) {
    Selection <- PlantResults$idExec == i & PlantResults$Sim == s
    PR <- PlantResults[Selection,]
    weight <- filter(ExecParam, idExec == i)$Weight
    Colu <- ifelse(weight, "RelCaptoLoad", "Capacity")
    SerMatrix <- as.matrix(Serie[deframe(PR[PR$Type != "Load", "Plant"])])
    CapVector <- deframe(PR[PR$Type != "Load", Colu])
    SerMatrixLoad <- as.matrix(Serie[deframe(PR[PR$Type == "Load", "Plant"])])
    CapVectorLoad <- deframe(PR[PR$Type == "Load", Colu])
    list(SerMatrix = SerMatrix, CapVector = CapVector, 
         SerMatrixLoad = SerMatrixLoad, CapVectorLoad = CapVectorLoad)
  }
  CalcResul1CVaRMatrix <- function(MatX, MatY, multiplier = 1, b = beta) {
    bal <- multiplier * MatX + MatY
    CalcCVaR(bal, b, UpperTail = FALSE)
  }
  SData <- SubsettoCalc1CVaR(i, s, Serie)
  MatX <- with(SData,  SerMatrix %*% CapVector)
  MatY <- with(SData, SerMatrixLoad %*% (CapVectorLoad))
  sapply(bs, function(bs) 
    optimize(function(x) abs(CalcResul1CVaRMatrix(MatX, MatY, b = bs, 
                                                  multiplier = x)), 
             ...)$minimum)
}

# Bind a column to MainResults containing the multiplier
future_bind_column_multiplier <- function(df, b = 0.95, cname = "SameRiskMulti", ...) {
  print(paste0("CalcMultiplier: ", b * 100, "%", " - ", date()))
  cname <- paste0(cname, "_", (1 - b) * 100, "%")
  df <- select(df, -any_of(!!cname))
  bind_cols(df, 
            !!cname := future_map_dbl(1:nrow(df), 
                                      function(n) {
                                        if (n %% 100 == 1) print(paste0("line: ", n, "/", nrow(df), " - ", date()))
                                        FindMultiplier(i = df[[n, "idExec"]],
                                                       s = df[[n, "Sim"]], b = b, ...)
                                      }))
}
# Bind columns to MainResults containing values of VaR and CVaR for a given beta.
future_bind_column_Var_CVaR <- function(df, b = 0.95, varname = "VaR", cvarname = "CVaR", ...) {
  print(paste0("CalcVaRCVaR: ", b * 100, "%", " - ", date()))
  varname <- paste0(varname, "_", (1 - b) * 100, "%")
  cvarname <- paste0(cvarname, "_", (1 - b) * 100, "%")
  res <- future_map_dfr(1:nrow(df), 
                 function(n) {
                   if (n %% 100 == 1) print(paste0("line: ", n, "/", nrow(df), " - ", date()))
                   CalcResul1CVaR(i = df[[n, "idExec"]],
                                  s = df[[n, "Sim"]], b = b, ...)
                 })
  res <- rename(res, !!varname := VaR, !!cvarname := CVaR)
  df <- select(df, -any_of(c(!!varname, !!cvarname)))
  bind_cols(df, res) 
}

GetSimilarCases <- function(x1, x2, correction = 1, By = "W_Variance", MR = MainResults) {
  #This function finds the most similar variance value in the other curve.
  MR <- ungroup(MR) # Ungroup to in order to row_number function below work correctly.
  V <- filter(MR, idExec == x1) %>% bind_rows(filter(MR, idExec == x2)) %>% # Filter only required cases (idExec)
    mutate(aux = case_when(idExec == x2 ~ .data[[By]] / correction^2, TRUE ~ .data[[By]])) %>% # Apply variance correction
    select(idExec, Sim, .data[[By]], aux)
  V <- mutate(V, row = row_number()) 
  d <- as.matrix(dist(V$aux, diag = TRUE, upper = TRUE)) 
  # Find out minimum distance values from one idExec compared to the other
  MinValues1 <- sapply(filter(V, idExec == x2) %>% pull(row), function(x) min(d[filter(V, idExec == x1) %>% pull(row), x]))
  MinValues2 <- sapply(filter(V, idExec == x1) %>% pull(row), function(x) min(d[filter(V, idExec == x2) %>% pull(row), x]))
  # Create a new tibble with a column (SimiliarTo) showing the most similar variance in the other case.
  V1 <- left_join(V, as_tibble(match(MinValues1, d) %>% 
                                 arrayInd(dim(d), useNames = TRUE)), by = c("row")) %>% 
    drop_na() %>% mutate(diff = MinValues1, idExecTo = x1)
  V2 <- left_join(V, as_tibble(match(MinValues2, d) %>% 
                                 arrayInd(dim(d), useNames = TRUE)), by = c("row" = "col")) %>% 
    rename(col = row.y) %>% drop_na() %>% mutate(diff = MinValues2, idExecTo = x2)
  V <- bind_rows(V1, V2) %>% mutate(SimilarTo = Sim[col]) %>% 
    select(idExec, idExecTo, Sim, SimilarTo, .data[[By]], aux, diff)
  arrange(V, diff) %>% distinct(idExec, SimilarTo, .keep_all = TRUE) %>% arrange(idExec, Sim)
}

PlotCompGeneric <- function(IDs, UseSimilar = FALSE, n = 6, ...) {
  # Plot comparison of different simulation with similar SD. 
  equiv <- filter(MainResults, idExec == IDs[1]) %>% mutate(SimilarTo = Sim) %>% select(idExec, Sim, SimilarTo)
  if (UseSimilar) { # Group by similarity or by simulation order?
    for (i in 2:length(IDs)) {
      equiv <- bind_rows(equiv, GetSimilarCases(IDs[1], IDs[i], ...) %>% 
                           filter(idExecTo == IDs[1])) 
    }
  }
  dataplot <- filter(left_join(MainResults, PlantResults) %>% ungroup() %>% 
                       left_join(PlantData, by = c("Plant" = "name", "Type", "Storage capacity", "UF")), 
                     idExec %in% IDs, str_detect(Plant, "Demand", negate = TRUE), # Remove load.
                     !(Plant %in% c("UTE_Inflex."))) # Remove flat power plant 
  
  dataplot <- left_join(dataplot, equiv, by = c("idExec", "Sim")) %>% rename(SimiPan = SimilarTo) %>% 
    drop_na(SimiPan)
  u <- dataplot %>% arrange(SimiPan) %>% drop_na(idExecTo) %>% distinct(SimiPan) %>% deframe() # Unique values in order
  l <- length(u)
  if (l > n) { # Select a subset of "simulations" if there are too many of them.
    SelectedSimiPan <- u[round(seq(from = 1, to = l,
                                   by = (l - 1) / (n - 1)))]
    dataplot <- filter(dataplot, SimiPan %in% SelectedSimiPan)
  } else {
    dataplot <- filter(dataplot, SimiPan %in% u) # Remove unused SimiPan rows.
  }
  dataplot
}

CalcResul1Balance <- function(i, s, Serie = SeriesComplete, m = 1) {
  Selection <- PlantResults$idExec == i & PlantResults$Sim == s
  PR <- PlantResults[Selection,]
  weight <- filter(ExecParam, idExec == i)$Weight
  Colu <- ifelse(weight, "RelCaptoLoad", "Capacity")
  PR <- mutate(PR, Capacity = if_else(str_detect(Plant, "Demand"), .data[[Colu]], .data[[Colu]] * m))
  bal <- as.matrix(Serie[deframe(PR[, "Plant"])]) %*% (deframe(PR[, Colu]))
  bind_cols(idExec = i, Sim = s, Serie[1], Balance = as.vector(bal))
}

Calc1Variance <- function(i, s, WithLoad = FALSE) {
  SR <- PlantResults[PlantResults$idExec == i & PlantResults$Sim == s, c("Plant", "RelCaptoLoad", "Capacity")]
  if (!WithLoad) SR <- filter(SR, str_detect(Plant, "Demand", negate = TRUE))
  CovarMat2 <- CovarMat1[deframe(SR["Plant"]), deframe(SR["Plant"])] # Select only columns and rows used.
  deframe(deframe(SR["RelCaptoLoad"]) %*% CovarMat2 %*% deframe(SR["RelCaptoLoad"]))
}

RunNModes <- function(SeriesDesc = "") {
  rm(CovarMat, pos = ".GlobalEnv")
  # Run CVaR model
  RiskSampFlag <<- 1
  ModelFile <<- "src/MinCost_IsoRisk.mod"
  for (i in 1:length(ListBeta)) {
    beta <<- ListBeta[i]
    for (j in 1:length(ListOmega)) {
      omega <<- ListOmega[j]
      Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5), " - beta=", beta, " - omega=", omega)
      print(paste0("Running: ", Notes))
      source("src/RunAndGetResults.R")
    }
  }
  # Run Trad model
  RiskSampFlag <<- 0
  ModelFile <<- "src/MaxGen_IsoCap.mod"
  Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5))
  print(paste0("Running: ", Notes))
  source("src/RunAndGetResults.R")
  # Run Cost model
  ModelFile <<- "src/MinCost_IsoGen.mod"
  Notes <<- paste0(SeriesDesc, " — ", str_sub(ModelFile, 5, -5))
  print(paste0("Running: ", Notes))
  source("src/RunAndGetResults.R")
}

CalcGeoDivIndex <- function(x, param = "GenShare", correction = FALSE) { # Correction: if TRUE, does not consider the distance of a point to itself (0).
  #sum(GenShare[i] * GenShare[j] * dist[i][j])
  disti <- distm(x[c("long", "lat")]) # Matrix with distance .
  produto <- tcrossprod(deframe(x[c(param)])) # Matrix with the crossproduct of param.
  tot <- sum(disti * produto, na.rm = TRUE)
  tot <- ifelse(correction, tot * (nrow(x) / (nrow(x) - 1)), tot)
}

CalcDivIndex <- function(x, param = "GenShare", DistMat = distancesSC, correction = FALSE) { # Correction: if TRUE, does not consider the distance of a point to itself (0).
  #sum(GenShare[i] * GenShare[j] * dist[i][j])
  PlantList <- x$Plant
  PlantList <- PlantList[str_detect(PlantList, "Demand", negate = TRUE)] # Remove load
  disti <- as.matrix(DistMat)[PlantList, PlantList] # Matrix with distance .
  produto <- tcrossprod(deframe(x[x$Plant %in% PlantList, c(param)])) # Matrix with the crossproduct of param.
  tot <- sum(disti * produto, na.rm = TRUE)
  tot <- ifelse(correction, tot * (nrow(x) / (nrow(x) - 1)), tot)
}

CalcCumDens <- function(vec, quant = 0.05) { # Calculate density value when cumulative probability is at least "quant"
  Dens <- density(vec)
  Area <- cumsum(diff(Dens$x) * na.omit(Dens$y + lag(Dens$y))/2)
  #cumsum(diff(dens$x) * dens$y)
  res <- Dens$y[sum(Area <= quant) + 2]
  res
}

CalcCVaR <- function(x, probs = 0.5, UpperTail = TRUE) {
  # Based on http://www-iam.mathematik.hu-berlin.de/~romisch/SP01/Uryasev.pdf
  sapply(probs, function(prob) {
    if (!UpperTail) x <- -x
    VaR <- quantile(x, prob, type = 1)
    psi <- mean(x <= VaR)
    lambda <- (psi - prob) / (1 - prob)
    CVaRp <- mean(x[x > VaR])
    if (is.nan(CVaRp)) {
      lambda <- 1
      CVaRp <- 0
    }
    (lambda * VaR + (1 - lambda) * CVaRp) * 
      ifelse(UpperTail, 1, -1)
  })
}
