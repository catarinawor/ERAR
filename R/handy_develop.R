#=========================================================================
#handy scripts to be ran during development 
#do not expect any organization or documentation in this file
#=========================================================================

#initial list of settings
M <- list(
    isCombineAge2And3=c(TRUE,TRUE),
    isCombineAge5And6=c(FALSE,FALSE),
    isReplicateCohShak = FALSE,
    StockListView =c("HAR","ATN"),
    datbse="../data/CIS2018_Pilot_Small.accdb",
    ArithmeticMeanFlag = TRUE,
    MaxCalendarYear=2017,
    isTraceCalc = FALSE,
    isTraceByBroodYr = TRUE,
    isTraceByCalendarYr = FALSE,
    traceThisShakerMethod = "C",
    Average_Maturation_Rate = "SelectNumberCompleteBroods",
    LastCompleteBroodsUsed = 9,
    MeanMatType = "ArithmeticMean",
    PNVAlgorithm = "FisherySpecific",
    ShakerMethod ="C",
    IncompleteYearAlgorithm= "New",
    RoundRecoveriesByTagCode= FALSE,
    traceThisYear = NA,
    traceThisAge =NA,
    traceThisFishery =NA

    )


#put this file in Rbuild ignore


#load in all accessory functions - to be deleted if/when we turn this into a package
#not working yet - since I still have a bunch of crap in the R folder - nee to clean it up and leave just functions
#devtools::load_all()

funcfiles <- list.files(getwd(),pattern="func",full.names=TRUE)
    
for(i in 1:length(funcfiles)){
    
    source(funcfiles[i])

}



M <- StartCohortAnalysis_Click(M)



#CalculateButton_Click(M)


#implementation of package usage
setwd("C:\\Users\\worc\\Documents\\ERA")
usethis::use_r("func_TotalCatch_Age")
setwd("C:\\Users\\worc\\Documents\\ERA\\R")



names(M)


names(D)


RODBC::odbcCloseAll()