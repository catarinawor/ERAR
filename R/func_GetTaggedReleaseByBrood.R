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
GetTaggedReleaseByBrood <- function(){

    #read from database
    ERASQL = "SELECT BroodYear, SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count))as CWTRelease, SUM(CWTMark1Count+IIf(ISNULL(CWTMark2Count),0,CWTMark2Count)) + Sum(IIF(ISNULL(NonCWTMark1Count),0,NonCWTMark1Count)+IIF(ISNULL(NonCWTMark2Count),0,NonCWTMark2Count)) as TotalRelease FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ") and BroodYear <= " & LastBY & " AND NOT ExcludeTagCodeFromERA = -1" & " Group By BroodYear"
    #object that is supposed to store read in data
    CISDataReader 

    for (i in 1:length(CISDataReader[1])) {

        BroodYear <- CISDataReader[1]
        CWTRelease[BroodYear] <- CISDataReader[2]
        TotalRelease[BroodYear] <- CISDataReader[3]
        #expansion up to the largest release size across all brood years for a stock
        RelRatio[BroodYear] = MaxRelease / CWTRelease[BroodYear]
    
    }
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




