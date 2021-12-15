# VRES-Portfolio

This repository contains code that reproduces the results of the paper *Improvements to Modern Portfolio Theory based models applied to electricity systems*. Pre-print is available at: <https://arxiv.org/abs/2105.08182>.

#### Abstract:

> With the increase of variable renewable energy sources (VRES) share in electricity systems, manystudies were developed in order to determine their optimal technological and spatial mix. Modern PortfolioTheory (MPT) has been frequently applied in this context. However, some crucial aspects, important inenergy planning, are not addressed by these analyses. We, therefore, propose several improvements andevaluate how each change in formulation impacts results. More specifically, we address generation costs, system demand, and firm energy output, present a formal model and apply it to the case of Brazil. Wefound that, after including our proposed modifications, the resulting efficient frontier differs strongly fromthe one obtained in the original formulation. Portfolios with high output standard deviation are not ableto provide a firm output level at competitive costs. Furthermore, we show that diversification plays animportant role in smoothing output from VRES portfolios.

## Requirements

-   R
-   CPLEX

If required packages are not installed, run `install.packages(c("tidyverse", "corrr", "lubridate", "lhs", "furrr", "geosphere", "ggrepel"))` in R.

## Input files

The following files are required to run the model:

-   **parameters.R**: Define main execution parameters.
-   **Plantdata.rds**: Power plants data, except time series.
-   **WindSolar.rds**: Time series for wind and PV power plants. Available at <https://zenodo.org/record/4924020>.

## How to run

### From command line

    Rscript main.R

### From Rstudio

Open **main.R** and press Ctrl+Shift+S (Source)

### CPLEX path

The path to CPLEX oplrun executable must be defined in file **parameters.R** before running the script. In Linux systems the default installation folder (for version 12.10) is **/opt/ibm/ILOG/CPLEX_Studio1210/opl/bin/x86-64_linux/**. This is probably not necessary in Windows systems. If CPLEX is not available in the system, it is possible to download the file **Results.rds** containing the results and generate the figures. This file contains the results from an execution using **CPLEX** 12.10.0, **R** 4.0.3, **tidyverse** 1.3.0, **lubridate** 1.7.9.2, **lhs** 1.1.1, **furrr** 0.2.2, **corrr** 0.4.3, **geosphere** 1.5-10. To run without CPLEX, there are two possible ways:

-   Run `Rscript main.R noCPLEX`, in command line.
-   Change CPLEX variable to FALSE in file **parameters.R**.

### Number of threads

Change variable **NumThreads** in **parameters.R** file to define the number of threads to be used in some routines. This option has no impact on CPLEX optimization. Each thread requires RAM space, so it is recommended to not set this value too high, unless there is enough available RAM.

## Expected output

After running the script, file **Results.rds** is created in **results/** folder. Name and location of file **Results.rds** can be changed in the **parameters.R** file before script execution. All figures are also exported in PDF format.

### Difference in results

Results from CPLEX can vary slightly depending on the system specs, operating system, CPLEX Version, etc. However, those differences are minimal and do not impact the conclusions.

## Results structure

**File Results.rds** contains a list of tibbles, as described below.

-   **ExecParam**: Each row is a scenario and the parameters used to run it.
-   **MainResults**: In this tibble, each row is an optimization run. All data, except what is related to individual plants, is here, such as cost, standard deviation, portfolio mean generation, etc.
-   **PlantResults**: Individual power plant capacity for each run.
-   **MultiplierResults**: VaR, CVaR and multiplier for each run in order to achieve the required CVaR value.
