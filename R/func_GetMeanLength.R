#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================




#' @title GetMeanLength
#'
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#'
#' @details
#'
#' @return D: A list 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetMeanLength <- function(){

#'Get MeanLength,StandardDeviation for current ERAStock
    
    lengthstDev<-matrix(NA,nrow=MaxAge, ncol=LastCalendarYear)
    meanLength<-matrix(NA,nrow=MaxAge,ncol=LastCalendarYear)
  
    ERASQL = "SELECT CalendarYear, Age, MeanLength, StandardDeviation FROM ERA_Stock INNER JOIN MeanLength ON ERA_Stock.SuperStock = MeanLength.SuperStock WHERE ERAStock = '" & CurrentStock & "' and TimePeriod = '" & TimePeriod & "'"
    #read from database
    
    for (i in 1:length(CISDataReader)) {
        CY <- CISDataReader[1]
        Age <- CISDataReader[2]
        meanLength[Age, CY] = CISDataReader[3]
        lengthstDev[Age, CY] = CISDataReader[4]

    }
    
  
}




