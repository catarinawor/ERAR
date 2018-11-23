#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


#' @title GetTaggedReleaseByBrood
#'
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#' @param D A list with stock specific information after  GetMaxReleaseSize 
#'
#' @details
#'
#' @return A list containing information on RelRatioo, BroodYear, CWTRelease, and TotalRelease
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetTaggedReleaseByBrood <- function(D,M){

    #read from database

    #establish connection with database
    dta <- RODBC::odbcConnectAccess2007(M$datbse)      

     ERASQL = paste0("SELECT BroodYear, SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count))as CWTRelease, SUM(CWTMark1Count+IIf(ISNULL(CWTMark2Count),0,CWTMark2Count)) + Sum(IIF(ISNULL(NonCWTMark1Count),0,NonCWTMark1Count)+IIF(ISNULL(NonCWTMark2Count),0,NonCWTMark2Count)) as TotalRelease FROM ERA_WireTagCode WHERE CASStock IN ('", D$CASStockString[[1]], "') and BroodYear <= " , D$LastBY, " AND NOT ExcludeTagCodeFromERA = -1" , " Group By BroodYear")

    df1 <- sqlQuery( dta , query = ERASQL )
    #object that is supposed to store read in data

        
    #expansion up to the largest release size across all brood years for a stock
    RelRatio <- D$MaxRelease / df1$CWTRelease
    
    
    return(list(RelRatio=RelRatio, BroodYear=df1$BroodYear, CWTRelease=df1$CWTRelease, TotalRelease=df1$TotalRelease))

    

    #Original VB code
    #========================================================================================
    #Dim BroodYear As Integer
    #    'obtain total Tagged Release for each brood year
    #    ERASQL = "SELECT BroodYear, SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count))as CWTRelease, SUM(CWTMark1Count+IIf(ISNULL(CWTMark2Count),0,CWTMark2Count)) + Sum(IIF(ISNULL(NonCWTMark1Count),0,NonCWTMark1Count)+IIF(ISNULL(NonCWTMark2Count),0,NonCWTMark2Count)) as TotalRelease FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ") and BroodYear <= " & LastBY & " AND NOT ExcludeTagCodeFromERA = -1" & " Group By BroodYear"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    Do While CISDataReader.Read()
    #        BroodYear = CISDataReader(0)
    #        CWTRelease(BroodYear) = CISDataReader(1)
    #        TotalRelease(BroodYear) = CISDataReader(2)
    #        'expansion up to the largest release size across all brood years for a stock
    #        RelRatio(BroodYear) = MaxRelease / CWTRelease(BroodYear)
    #    Loop
    #    CISDataReader.Close()

}




