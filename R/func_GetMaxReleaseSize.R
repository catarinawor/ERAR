#=======================================================
#ERA function GetCASStocks()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================



#' @title GetMaxReleaseSize
#'
#' @description  Returns the maximum amount (counts) amount of releases for each stock
#' 
#' 
#'
#' @param M A list passed to MainSub
#' @param D A list with stock specific information after GetFirstAndLastBY
#' 
#'
#' @details
#'
#' @return  A list contining the MaxRelease, the maximum amount of tags released for a stock in the time series and MaxReleaseErr, sn integer that indicates if the CASStockString is present on the database  
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetMaxReleaseSize <- function(D,M){



	# 'get maximum release size from all brood years for the ERAStock,limit brood year to LastCalendarYear-OceanStartAge
     #   'original CIS code includes nonCWTMarkCount in MaxRelease but not in CWTRelease(BroodYear)

     #establish connection with database
     #dta <- RODBC::odbcConnectAccess2007(M$datbse)      


     if(!M$isReplicateCohShak){
          #'Coshak do not include nonCWTMarkCount in both MaxRelease and CWTRelease(BroodYear) 
     	ERASQL = paste0("SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) + Sum(NonCWTMark1Count+IIF(ISNULL(NonCWTMark2Count),0,NonCWTMark2Count)) FROM ERA_WireTagCode WHERE CASStock IN ('",D$CASStockString[[1]] , "') and BroodYear <= ", D$LastBY, " AND NOT ExcludeTagCodeFromERA = -1" , " Group By BroodYear)")

     }else{
     	 ERASQL = paste0("SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease FROM ERA_WireTagCode WHERE CASStock IN ('" , D$CASStockString[[1]] , "') and BroodYear <= " , D$LastBY , " AND NOT ExcludeTagCodeFromERA = -1", " Group By BroodYear)")

     }

     df1 <- sqlQuery( M$chnl , query = ERASQL )
       MaxReleaseErr <- 0


     MaxRelease <- df1[[1]]

     if(is.na(D$CASStockString[[1]])|is.null(D$CASStockString[[1]])){
          logname<-paste("../logs/",D$CurrentStock,"_GetMaxReleaseSize.log")
          sink(logname)
          cat(paste("please open ERA_CASStockToERAStockMapping and see if ", D$CASStockString[[1]] ," is missing in the CASStock field (column).  Program is going to stop"))
          sink()
          MaxReleaseErr <- 1
     }
     
    

     #If isReplicateCohShak = False Then
     #       ERASQL = "SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) + Sum(NonCWTMark1Count+IIF(ISNULL(NonCWTMark2Count),0,NonCWTMark2Count)) FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ") and BroodYear <= " & LastBY & "AND NOT ExcludeTagCodeFromERA = -1" & " Group By BroodYear)"
     #       'Coshak do not include nonCWTMarkCount in both MaxRelease and CWTRelease(BroodYear) 
     #   Else
     #       ERASQL = "SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ") and BroodYear <= " & LastBY & "AND NOT ExcludeTagCodeFromERA = -1" & " Group By BroodYear)"
     #   End If
     #  ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
     #  CISDataReader = ERACommand.ExecuteReader()
     #   CISDataReader.Read()
     #   Try
     #       MaxRelease = CISDataReader(0)
     #   Catch
     #       MsgBox("please open ERA_CASStockToERAStockMapping and see if " & CASStockString & " is missing in the CASStock field (column).  Program is going to stop")
     #       End
     #   End Try
     #   CISDataReader.Close()

	 return(list(MaxRelease=MaxRelease,MaxReleaseErr=MaxReleaseErr))


     #============================================================================================================
     # Original VB code

     # 'get maximum release size from all brood years for the ERAStock,limit brood year to LastCalendarYear-OceanStartAge
     #   'original CIS code includes nonCWTMarkCount in MaxRelease but not in CWTRelease(BroodYear)
     #   If isReplicateCohShak = False Then
     #       ERASQL = "SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) + Sum(NonCWTMark1Count+IIF(ISNULL(NonCWTMark2Count),0,NonCWTMark2Count)) FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ") and BroodYear <= " & LastBY & "AND NOT ExcludeTagCodeFromERA = -1" & " Group By BroodYear)"
     #       'Coshak do not include nonCWTMarkCount in both MaxRelease and CWTRelease(BroodYear) 
     #   Else
     #       ERASQL = "SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ") and BroodYear <= " & LastBY & "AND NOT ExcludeTagCodeFromERA = -1" & " Group By BroodYear)"
     #   End If
     
     #   ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
     #   CISDataReader = ERACommand.ExecuteReader()
     #   CISDataReader.Read()
     #   Try
     #       MaxRelease = CISDataReader(0)
     #   Catch
     #       MsgBox("please open ERA_CASStockToERAStockMapping and see if " & CASStockString & " is missing in the CASStock field (column).  Program is going to stop")
     #       End
     #   End Try
     #   CISDataReader.Close()

} 




