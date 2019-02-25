#=======================================================
#ERA function BuildERAFisheryToPSCFisheryList() a
#Translated from VB ERA CIS code
#February 2019
#Author: Catarina Wor
#=======================================================






#' @title BuildERAFisheryToPSCFisheryList
#'
#' @description   
#'  
#' 
#' @param M A list passed to StartCohortAnalysis_Click
#' 
#' @param PSCFish  number of PSC fisheries
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
BuildERAFisheryToPSCFisheryList <- function(PSCFish,M){


    #'*******************************************************************
    #'  BuildERAFisheryToPSCFisheryList
    #'   Looks up all ERAFisheries that map to this PSCFishery 
    #'   as defined in the ERA_ERAFisheryToPSCFisheryMapping table and builds up 
    #'   an array that can be used in loops
    #'**************************************************************************
    

    sSQL <-paste0("Select ERA_ERAFishery.Id FROM ERA_PSCFishery",
    " INNER JOIN (ERA_ERAFishery INNER JOIN ERA_ERAFisheryToPSCFisheryMapping", 
    " ON ERA_ERAFishery.Name = ERA_ERAFisheryToPSCFisheryMapping.ERAFishery)",
    " ON ERA_PSCFishery.Name = ERA_ERAFisheryToPSCFisheryMapping.PSCFishery",
    " WHERE ERA_PSCFishery.ID = ",PSCFish, " order by ERA_ERAFishery.Id")

    #dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
    
    ids <- RODBC::sqlQuery( M$chnl , query =sSQL)
    
    NumberFisheries<-0
    ERA2PSC<-NULL
    
    if(nrow(ids)>0){
        for(i in 1:nrow(ids)){
            NumberFisheries <- NumberFisheries + 1
            FisheryId <-ids[[1]][i]
            ERA2PSC[NumberFisheries] = FisheryId #'array of ERAFisheries that map to PSCFishery in question
        }
    }else{
        sink("../logs/FisheryTranslation.log")
        MsgBox("You don't have an entry in the ERA_ERAFisheryToPSCFisheryMapping for PSCFishery #" & PSCFish & ".")
        sink()
    }
    

    
   
   return<-list(NumberERA2PSCFisheries=NumberFisheries,ERA2PSC=ERA2PSC)

    #    '*******************************************************************
    #    '  BuildERAFisheryToPSCFisheryList
    #    '   Looks up all ERAFisheries that map to this PSCFishery 
    #    '   as defined in the ERA_ERAFisheryToPSCFisheryMapping table and builds up 
    #    '   an array that can be used in loops
    #    '**************************************************************************
    #    Dim sSQL As String
    #    'sSQL = "select ERAFishery " & _
    #    '"from ERA_ERAFisheryToPSCFisheryMapping where PSCFishery = " & PSCFishery & " order by ERAFishery"
    #
    #    sSQL = "Select ERA_ERAFishery.Id FROM ERA_PSCFishery " _
    #        & " INNER JOIN (ERA_ERAFishery INNER JOIN ERA_ERAFisheryToPSCFisheryMapping " _
    #        & " ON ERA_ERAFishery.Name = ERA_ERAFisheryToPSCFisheryMapping.ERAFishery) " _
    #        & " ON ERA_PSCFishery.Name = ERA_ERAFisheryToPSCFisheryMapping.PSCFishery" _
    #        & " WHERE ERA_PSCFishery.ID = " & PSCFishery & " order by ERA_ERAFishery.Id"
    #
    #    Dim FisheryId As Int32
    #    Dim NumberFisheries As Int32
    #    ReDim ERA2PSC(30)
    #    Try
    #      ERACommand = New OleDbCommand(sSQL, CISDBConnection)
    #      CISDataReader = ERACommand.ExecuteReader()
    #      While CISDataReader.Read()
    #        NumberFisheries = NumberFisheries + 1
    #        FisheryId = CISDataReader(0)
    #        ERA2PSC(NumberFisheries) = FisheryId 'array of ERAFisheries that map to PSCFishery in question
    #      End While
    #    Catch
    #      MsgBox("You don't have an entry in the ERA_ERAFisheryToPSCFisheryMapping for PSCFishery #" & PSCFishery & ".")
    #    Finally
    #      CISDataReader.Close()
    #      NumberERA2PSCFisheries = NumberFisheries
    #    End Try
	


}

