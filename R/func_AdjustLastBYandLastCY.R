#=======================================================
#ERA function AdjustLastBYandLastCY.R()
#Translated from VB ERA CIS code
#February 2019
#Author: Catarina Wor
#=======================================================






#' @title AdjustLastBYandLastCY.R
#'
#' @description   
#'  
#' 
#' @param M A list passed to StartCohortAnalysis_Click
#' 
#' @param D  A list containing stock specific info
#'
#' @details adjust LastCalendarYear for situations where tagging stopped for a stock
#'
#' @return 
#' 
#' @export
#'
#' @examples
#' 
#' 
AdjustLastBYandLastCY<- function(D){


    allBY<-D$FirstBY:D$LastBY

    for(i in 1:length(allBY)){
        if(!D$MissingBroodYearFlag[i]){
            D$LastBY<-allBY[i]
        }
    }

    return(D)
}



	#original vb code
	#===============================================================
    #For BroodYear As Integer = FirstBY To LastBY
    #  If MissingBroodYearFlag(BroodYear) = False Then
    #    LastBY = BroodYear
    #  End If
    #Next BroodYear
    #'adjust LastCalendarYear for situations where tagging stopped for a stock
    #If LastCalendarYear > LastBY + MaxAge Then LastCalendarYear = LastBY + MaxAge
	




}




