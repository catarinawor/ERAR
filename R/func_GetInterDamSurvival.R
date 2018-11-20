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
GetInterDamSurvival <- function(D,M){

    

      #'Set default InterDamSurvival Rate to 1 for each year, change below if stock has rates in InterDamSurvival table
    AdultInterDamSurvivalRate <- NULL
    JackInterDamSurvivalRate <- NULL

        for(CalendarYear in (D$FirstBY + D$OceanStartAge):(D$LastBY + D$MaxAge)){
            AdultInterDamSurvivalRate[CalendarYear] <- 1
            JackInterDamSurvivalRate[CalendarYear] <- 1
        }
        
        #Get InterDamSurvival Rates for each stock
        ERASQL = "SELECT CalendarYear, AdultInterDamSurvivalRate,JackInterDamSurvivalRate FROM ERA_Stock INNER JOIN InterDamSurvival ON ERA_Stock.SuperStock = InterDamSurvival.SuperStock WHERE ERAStock = '" , D$CurrentStock ,"' and TimePeriod ='" , M$TimePeriod , "' and CalendarYear <=", D$LastBY + D$MaxAge
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



        #original Vb version of the ERA
        #==========================================================================================
        # 'Set default InterDamSurvival Rate to 1 for each year, change below if stock has rates in InterDamSurvival table
        #For CalendarYear As Integer = FirstBY + OceanStartAge To LastBY + MaxAge
        #    AdultInterDamSurvivalRate(CalendarYear) = 1
        #    JackInterDamSurvivalRate(CalendarYear) = 1
        #Next CalendarYear
        #'Get InterDamSurvival Rates for each stock
        #ERASQL = "SELECT CalendarYear, AdultInterDamSurvivalRate,JackInterDamSurvivalRate FROM ERA_Stock INNER JOIN InterDamSurvival ON ERA_Stock.SuperStock = InterDamSurvival.SuperStock WHERE ERAStock = '" & CurrentStock & "' and TimePeriod ='" & TimePeriod & "' and CalendarYear <=" & LastBY + MaxAge
        #ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
        #CISDataReader = ERACommand.ExecuteReader()
        #Try
        #    Do While CISDataReader.Read()
        #        Dim CalendarYear As Integer
        #        CalendarYear = CISDataReader(0)
        #        AdultInterDamSurvivalRate(CalendarYear) = CISDataReader(1)
        #        JackInterDamSurvivalRate(CalendarYear) = CISDataReader(2)
        #    Loop
        #Catch
        #    MsgBox("cannot find interdam survival rate for " & CurrentStock & " in InterDamSurvival table")
        #End Try
        #CISDataReader.Close()
        
}




