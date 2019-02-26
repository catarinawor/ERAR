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

    allBY<-D$FirstBY:D$LastBY
    allCY<-(D$FirstBY + D$OceanStartAge):M$LastCalendarYear
    allAge<-D$OceanStartAge:D$MaxAge

    CalendarYearLandedCatch <-matrix(NA,nrow=length(M$RunYearList),ncol=M$NumberPSCFisheries)


    for(CalYr in 1:length(allCY)){
        for(PSCFishery in 1:M$NumberPSCFisheries){
            for(Age in allAge){
                BroodYear < CalYr - Age
                if(BroodYear >= D$FirstBY & BroodYear <= D$LastBY){
                    BYind<-which(allBY==BroodYear)
                    if(D$MissingBroodYearFlag[which(allBY==BroodYear)]){

                        D$CalendarYearLandedCatch[CalYr, PSCFishery] = D$CalendarYearLandedCatch[CalYr, PSCFishery] + D$LandedCatch[PSCFishery, Age, BroodYear]
                     #          If isTraceCalc = True And isTraceByCalendarYr = True And CalYr >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CalYrCatchID, "1339 CalYrCat", PSCFishery, CalYr, BroodYear, Age, CalendarYearLandedCatch(CalYr, PSCFishery), LandedCatch(PSCFishery, Age, BroodYear), RelRatio(BroodYear))

                    }
                }

            }
        }

    }




}



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




