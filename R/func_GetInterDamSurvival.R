#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)



#' @title GetInterDamSurvival
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
GetInterDamSurvival <- function(){

      #'Set default InterDamSurvival Rate to 1 for each year, change below if stock has rates in InterDamSurvival table
        for(CalendarYear in (FirstBY + OceanStartAge):(LastBY + MaxAge)){
            AdultInterDamSurvivalRate[CalendarYear] <- 1
            JackInterDamSurvivalRate[CalendarYear] <- 1
        }
        
        #Get InterDamSurvival Rates for each stock
        ERASQL = "SELECT CalendarYear, AdultInterDamSurvivalRate,JackInterDamSurvivalRate FROM ERA_Stock INNER JOIN InterDamSurvival ON ERA_Stock.SuperStock = InterDamSurvival.SuperStock WHERE ERAStock = '" & CurrentStock & "' and TimePeriod ='" & TimePeriod & "' and CalendarYear <=" & LastBY + MaxAge
        #read from database
        #ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
        #CISDataReader = ERACommand.ExecuteReader()
        
        for (i in 1:(length(CISDataReader[0]))){
             
            CalendarYear = CISDataReader[1]
            AdultInterDamSurvivalRate[CalendarYear] = CISDataReader[2]
            JackInterDamSurvivalRate[CalendarYear] = CISDataReader[3]

            if(is.na(AdultInterDamSurvivalRate)){
                print(paste("cannot find interdam survival rate for " ,CurrentStock ," in InterDamSurvival table"))
            }

        }
        
}




