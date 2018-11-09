#=======================================================
#ERA function GetCASStocks()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================



#' @title GetMaxReleaseSize
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
GetMaxReleaseSize <- function(curr_stk){



	#get maximum release size from all brood years for the ERAStock,limit brood year to LastCalendarYear-OceanStartAge
    #original CIS code includes nonCWTMarkCount in MaxRelease but not in CWTRelease(BroodYear)
     
     isReplicateCohShak <- F

     if(!isReplicateCohShak){
     	ERASQL = paste0("SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) + Sum(NonCWTMark1Count+IIF(ISNULL(NonCWTMark2Count),0,NonCWTMark2Count)) FROM ERA_WireTagCode WHERE CASStock IN ('", CASStockString[[1]] , "') and BroodYear <= " , LastBY , "AND NOT ExcludeTagCodeFromERA = -1" , " Group By BroodYear)"

               )
     }else{
     	#ERASQL = "SELECT Max(CWTRelease) FROM (SELECT BroodYear,SUM(CWTMark1Count+IIF(ISNULL(CWTMark2Count),0,CWTMark2Count)) as CWTRelease FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ") and BroodYear <= " & LastBY & "AND NOT ExcludeTagCodeFromERA = -1" & " Group By BroodYear)"
     }
      MaxReleaseErr<-0
     #read teh max release from database using the SQL queries above
     #MaxRelease <- CISDataReader(0)

     if(is.na(MaxRelease)){
     	print(paste("please open ERA_CASStockToERAStockMapping and see if",CASStockString,"is missing in the CASStock field (column).  Program is going to stop"))
     	MaxReleaseErr<-1
     }

     dta <- RODBC::odbcConnectAccess2007(dbse)   #specifies the file path   

     (df2 <- sqlQuery( dta , query = ERASQL ))

      MaxRelease = df2[1,1]

    

     print(ERASQL)


     ?print
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

} 



#Fom Gf plots  inst/SQL get data

#' @ export
	#' @rdname get_data
	##get_iphc_sets <- function(species, usability = NULL) {
	##  .q <- read_sql("get-iphc-set-level.sql")
	##  .q <- inject_filter("AND C.SPECIES_CODE IN", species, sql_code = .q)
	##  .d <- run_sql("GFBioSQL", .q)
	##  .d$species <- tolower(.d$species)
	###
	###    if (!is.null(usability)) {
	###     .d <- filter(.d, usability_code %in% usability)
	###   }
	##  as_tibble(.d)
	##}




