#=======================================================
#ERA function CalcCalendarYearCatch.R()
#Translated from VB ERA CIS code
#February 2019
#Author: Catarina Wor
#=======================================================






#' @title CalcCalendarYearCatch
#'
#' @description   
#'  
#' 
#' @param M A list passed to StartCohortAnalysis_Click
#' 
#' @param D  A list containing stock specific info
#'
#' @details 
#'
#' @return 
#' 
#' @export
#'
#' @examples
#' 
#' 
CalcCalendarYearCatch <- function(D){

    allBY <- D$FirstBY:D$LastBY
    allCY <- (D$FirstBY + D$OceanStartAge):M$LastCalendarYear
    allAge <- D$OceanStartAge:D$MaxAge

    CalendarYearLandedCatch <- matrix(0,nrow=length(M$RunYearList),ncol=M$NumberPSCFisheries)


    for(CalYr in 1:length(allCY)){
        for(PSCFishery in 1:M$NumberPSCFisheries){
            for(Age in allAge){
                BroodYear <- allCY[CalYr] - Age
                if(BroodYear >= D$FirstBY & BroodYear <= D$LastBY){
                    
                    BYind <- which(allBY==BroodYear)
                    
                    if(!D$MissingBroodYearFlag[BYind]){

                        CalendarYearLandedCatch[CalYr, PSCFishery] <- CalendarYearLandedCatch[CalYr, PSCFishery] + D$LandedCatch[PSCFishery, Age, BYind]
                        #print(CalendarYearLandedCatch[CalYr, PSCFishery])
                        #print(paste("PSCFishery =",PSCFishery,"Age =", Age,"BYind =", BYind,"CY =", allCY[CalYr] ))
                        

                        if(M$isTraceCalc & M$isTraceByCalendarYr & allCY[CalYr] >= M$traceThisYear & PSCFishery == M$traceThisFishery){
                            sink("../logs/debug_CalYrCatchID.log", append=TRUE)
                            cat("1339 CalYrCat", PSCFishery, AllCY[CalYr], BroodYear, Age, CalendarYearLandedCatch[CalYr, PSCFishery],
                             LandedCatch[PSCFishery, Age,AllBY==BroodYear], D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear==BroodYear])
                            sink()                                                                                                                  
                        }
                    }
                }
            }
        }
    }

    return( list(CalendarYearLandedCatch=CalendarYearLandedCatch))



	#original vb code
	#===============================================================
    #Dim BroodYear As Integer
    #'calculate Landed Catch for each calendar year and fishery
    #For CalYr As Integer = FirstBY + OceanStartAge To LastCalendarYear
    #  For PSCFishery As Integer = 1 To NumberPSCFisheries
    #    For Age = OceanStartAge To MaxAge
    #      BroodYear = CalYr - Age
    #      If BroodYear >= FirstBY And BroodYear <= LastBY Then
    #        If MissingBroodYearFlag(BroodYear) = False Then
    #          CalendarYearLandedCatch(CalYr, PSCFishery) = CalendarYearLandedCatch(CalYr, PSCFishery) + LandedCatch(PSCFishery, Age, BroodYear)
    #          If isTraceCalc = True And isTraceByCalendarYr = True And CalYr >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CalYrCatchID, "1339 CalYrCat", PSCFishery, CalYr, BroodYear, Age, CalendarYearLandedCatch(CalYr, PSCFishery), LandedCatch(PSCFishery, Age, BroodYear), RelRatio(BroodYear))
    #        End If
    #      End If
    #    Next Age
    #  Next PSCFishery
    #Next CalYr
	#




}




