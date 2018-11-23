#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)



#' @title GetSurvivalRates
#'
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#' @param D A list with stock specific information after GetInterDamSurvival
#'
#' @details
#'
#' @return  A list containing Age and mortality rates, as well as a logic character stating if the data was read in correctly or not
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetSurvivalRates <- function(D,M){

    isOK <- FALSE #equivalent to isOK
    #Get Survival Rates for current ERAStock

    dta <- RODBC::odbcConnectAccess2007(M$datbse)         

    #read from database
    ERASQL = paste0("SELECT Age, NaturalMortalityRate FROM ERA_Stock INNER JOIN NaturalMortality ON ERA_Stock.SuperStock = NaturalMortality.SuperStock WHERE ERAStock = '" , D$CurrentStock , "' and TimePeriod ='" , TimePeriod , "'")
   
    df1 <- sqlQuery( dta , query = ERASQL )

    if(nrow(df1)==0){
        nome <- "GetSurvivalRates.log"
        sink(nome, append=T)
        cat(paste("Warning: Program will stop because either age or NaturalMortalityRate is missing in NaturalMortality table for ",D$CurrentStock))
        sink()
       D2<-(list( isOK= isOK))

    }else{

        D2<-(list(Age = df1$Age,
        SurvivalRate = 1 - df1$NaturalMortalityRate,
        isOK = TRUE))
    }

    return(D2)
    

    # Dim isOK As Boolean
    #    isOK = False
    #    'Get Survival Rates for current ERAStock
    #    ERASQL = "SELECT Age, NaturalMortalityRate FROM ERA_Stock INNER JOIN NaturalMortality ON ERA_Stock.SuperStock = NaturalMortality.SuperStock WHERE ERAStock = '" & CurrentStock & "' and TimePeriod ='" & TimePeriod & "'"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    Do While CISDataReader.Read()
    #        Dim Age As Integer = CISDataReader(0)
    #        SurvivalRate(Age) = 1 - CISDataReader(1)
    #        isOK = True
    #    Loop
    #    If isOK = False Then
    #        MsgBox("Warning: Program will stop because either age or NaturalMortalityRate is missing in NaturalMortality table for " & CurrentStock)
    #        End
    #    End If
    #    CISDataReader.Close()
        
}




