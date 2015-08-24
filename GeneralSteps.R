## General steps

## load local functions and packages
source(file="plot_me_funk.R")
source(file="getNextRise_funk.R")
source(file="getStormPolys_funk.R")
source(file="getPotentialStormData_funk.R")
source(file="table_me_funk.R")
source(file="getPlotDate_funk.R")
source(file="findStorm_funk.R")
source(file="plotPotStormByYear.R")
source(file="plotIndvPotStorms.R")
library(zoo)
library(doBy)

## get observed precip data
source(file="GetPrecipData.R")

## get observed flow data
source(file="ObservedData.R")

## estimate flow data
source(file="EstimateFlow.R")

## caclulate StreamStats
source(file="StreamStatsCalc.R")