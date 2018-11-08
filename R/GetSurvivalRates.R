#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


GetSurvivalRates <- function(){

    isOK = FALSE
    #Get Survival Rates for current ERAStock

    #read from database
    ERASQL = "SELECT Age, NaturalMortalityRate FROM ERA_Stock INNER JOIN NaturalMortality ON ERA_Stock.SuperStock = NaturalMortality.SuperStock WHERE ERAStock = '" & CurrentStock & "' and TimePeriod ='" & TimePeriod & "'"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()

    for (i in 1:length(CISDataReader[1])){
        Age <- CISDataReader[1]
        SurvivalRate[Age] <- 1 - CISDataReader[2]
        isOK <- TRUE
    }
    if(!isOK){
        print(paste("Warning: Program will stop because either age or NaturalMortalityRate is missing in NaturalMortality table for ",CurrentStock))
    }
    

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




