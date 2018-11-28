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
#' @param D A list that includes the outputs of GetIMData
#'
#' @details
#'
#' @return A list 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetMeanLength <- function(D,M){



#'Get MeanLength,StandardDeviation for current ERAStock
    dta <- RODBC::odbcConnectAccess2007(M$datbse)         

    ERASQL = paste0("SELECT CalendarYear, Age, MeanLength, StandardDeviation FROM ERA_Stock INNER JOIN MeanLength ON ERA_Stock.SuperStock = MeanLength.SuperStock WHERE ERAStock = '", D$CurrentStock, "' and TimePeriod = '", D$TimePeriod, "'")
    
    df1 <- sqlQuery( dta , query = ERASQL )

    CY <- df1[,1]
    Age <- df1[,2]

    meanLength <- tidyr::spread(df1[,c("CalendarYear","Age", "MeanLength")],key=CalendarYear,value= MeanLength )
    lengthstDev <- tidyr::spread(df1[,c("CalendarYear","Age", "StandardDeviation")],key=CalendarYear,value= StandardDeviation  )

    #=================================================================

    return(list(GetMeanLength_CY=CY,
        GetMeanLength_Age=Age,
        meanLength=meanLength,
        lengthstDev=lengthstDev
        ))
  
}




