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
#' @description  This function retrieves the inter dam survival for a specific ERA stock, if inter dam survival datadoes not exist (i.e. no dams), then sets the interdam to 1
#' 
#' 
#'
#' @param M A list passed to MainSub
#' 
#' @param D A list with stock specific information after  GetTaggedReleaseByBrood 
#'
#' @details
#'
#' @return D: A list containing informaton on CalendarYear, AdultInterDamSurvivalRate, JackInterDamSurvivalRate 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetInterDamSurvival <- function(D,M){


    dta <- RODBC::odbcConnectAccess2007(M$datbse)      

    #'Set default InterDamSurvival Rate to 1 for each year, change below if stock has rates in InterDamSurvival table
    AdultInterDamSurvivalRate <- numeric(length=length((D$FirstBY + D$OceanStartAge):(D$LastBY + D$MaxAge)))+1
    JackInterDamSurvivalRate <-numeric(length=length((D$FirstBY + D$OceanStartAge):(D$LastBY + D$MaxAge)))+1

       
    #Get InterDamSurvival Rates for each stock
    ERASQL = paste0("SELECT CalendarYear, AdultInterDamSurvivalRate, JackInterDamSurvivalRate FROM ERA_Stock INNER JOIN InterDamSurvival ON ERA_Stock.SuperStock = InterDamSurvival.SuperStock WHERE ERAStock = '" , D$CurrentStock ,"' and TimePeriod ='", D$TimePeriod, "' and CalendarYear <= ", D$LastBY + D$MaxAge)
        
    df1 <- sqlQuery( dta , query = ERASQL )

        
    if( nrow(df1)==0){
        nome<-"../logs/GetInterDamSurvival.log"
        sink(nome, append=T)
        cat(paste("cannot find interdam survival rate for " ,D$CurrentStock ," in InterDamSurvival table"))
        sink()

        return(list( CalendarYear =(D$FirstBY + D$OceanStartAge):(D$LastBY + D$MaxAge),
        AdultInterDamSurvivalRate = AdultInterDamSurvivalRate,
        JackInterDamSurvivalRate =JackInterDamSurvivalRate))

    }else{
        return(list( CalendarYear =df1[,1],
        AdultInterDamSurvivalRate = df1[,2],
        JackInterDamSurvivalRate =df1[,3]))

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


 





