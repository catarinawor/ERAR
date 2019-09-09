#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


#' @title GetFirstAndLastBY
#'
#' @description   Get first and last brood years for the CASStocks corresponding to the current ERAstock
#' 
#' 
#'
#' @param M A list passed to MainSub
#' @param D A list with stock specific output from GetMaxReleaseSize
#' @param ERAstock Counter for loop over all ERA stocks
#'
#' @details Retrives information for each stock in CASStockString and adjusts the Brood year depending on the age of the stock and the available data on the data base.
#'
#' @return A list that includes: ERA stock specific youngestAge, FirstBY, LastAvailableBY, LastPossibleBY, LastBY. The list also contaisn an error (erro) integer indicating wether or not the CASStockString is present in the data base. 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetFirstAndLastBY <- function(D,M,ERAStock){
#dbse,CASStockString,LastCalendarYear,OceanStartAge

    #dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path   

    ERASQL <- paste0("SELECT Min(BroodYear) AS MinOfBroodYear, Max(BroodYear) AS MaxOfBroodYear FROM ERA_WireTagCode WHERE CASStock IN ('", as.character(D$CASStockString[[1]]) ,"') AND NOT ExcludeTagCodeFromERA = (-1)")
    
    df1 <- RODBC::sqlQuery( M$chnl , query = ERASQL )
    
    err_FirstAndLastBY<-0

    FirstBY<-df1[1,1]

    LastAvailableBY<-df1[1,2]
    
    if(is.na(FirstBY) | is.na(LastAvailableBY)){

        sink("../logs/GetFirstAndLastBY.log")
        cat(paste("please open ERA_CASStockToERAStockMapping and see if " , M$CASStockString[[1]] , " is missing in the CASStock field (column).  Program is going to stop"))
        sink()
        err_FirstAndLastBY<-1

    }



	#Get first and last brood years for the ERAstock
	#need to read these from the data
	#LastAvailableBY<- 
    #ERASQL = "SELECT Min(BroodYear) AS MinOfBroodYear, Max(BroodYear) AS MaxOfBroodYear FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ")" & " AND NOT ExcludeTagCodeFromERA = -1"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    CISDataReader.Read()
	
	# last brood to run based on user-selected LastCalendarYear.  The last year in the listbox is determined by the last year of IM data.
	LastPossibleBY<- M$LastCalendarYear - D$OceanStartAge
    
    #set LastBy to lesser of LastAvailableBY or LastPossibleBY
    if(LastAvailableBY < LastPossibleBY){
    	 LastBY <- LastAvailableBY
    }else{
    	LastBY <- LastPossibleBY
    } 
    

    #'find youngest age and compare with OceanStartAge
    ERASQL2 <- paste0("SELECT Min(age) FROM ERA_CWDBRecovery as r INNER JOIN ERA_WireTagCode as t ON r.TagCode = t.TagCode  WHERE CASStock IN ('",D$CASStockString[[1]] , "')")
    
    df2 <- RODBC::sqlQuery( M$chnl , query = ERASQL2 )

    youngestAge <- df2[[1]]

    if(youngestAge < D$OceanStartAge & !M$isCombineAge2And3[ERAStock]){
        if(youngestAge == 1){
            sink(Log_OlderThanMaxAge_ID,append = TRUE)
            cat(paste(M$ShakerMethod, youngestAge,  "will not be used in Exploitation Rate Analysis.\n"))
            sink()
        }else if(youngestAge > 1){
            sink(Log_OlderThanMaxAge_ID,append = TRUE)
            cat(paste("Youngest age =", youngestAge ," is less than OceanStartAge in SuperStock.  Did you forget to combine age 2 and 3?\n"))
            sink()

        }

    }

     D <- list(youngestAge=youngestAge,
        FirstBY=FirstBY,
        LastAvailableBY=LastAvailableBY,
        LastPossibleBY = LastPossibleBY,
        LastBY = LastBY,
        err_FirstAndLastBY = err_FirstAndLastBY)

     return(D)

    
    #==================================================================================================
    #Original version of the VB code
    # Dim LastPossibleBY, LastAvailableBY As Integer
    #    Dim youngestAge As Integer
    #    'Get first and last brood years for the ERAstock
    #    ERASQL = "SELECT Min(BroodYear) AS MinOfBroodYear, Max(BroodYear) AS MaxOfBroodYear FROM ERA_WireTagCode WHERE CASStock IN (" & CASStockString & ")" & " AND NOT ExcludeTagCodeFromERA = -1"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    CISDataReader.Read()
    #    Try
    #        FirstBY = CISDataReader(0)
    #        LastAvailableBY = CISDataReader(1) ' last brood available in database
    #    Catch
    #        MsgBox("please open ERA_CASStockToERAStockMapping and see if " & CASStockString & " is missing in the CASStock field (column).  Program is going to stop")
    #        End
    #    End Try
    #    CISDataReader.Close()
    #
    #    LastPossibleBY = LastCalendarYear - OceanStartAge 'last brood to run based on user-selected LastCalendarYear.  The last year in the listbox is determined by the last year of IM data.
    #    'set LastBy to lesser of LastAvailableBY or LastPossibleBY
    #    If LastAvailableBY < LastPossibleBY Then
    #        LastBY = LastAvailableBY
    #    Else
    #        LastBY = LastPossibleBY
    #    End If
    #
    #    'find youngest age and compare with OceanStartAge
    #    ERASQL = "SELECT Min(age) FROM ERA_CWDBRecovery as r INNER JOIN ERA_WireTagCode as t ON r.TagCode = t.TagCode  WHERE CASStock IN (" & CASStockString & ")"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    CISDataReader.Read()
    #    youngestAge = CISDataReader(0)
    #    CISDataReader.Close()
    #
    #    If (youngestAge < OceanStartAge) And isCombineAge2And3(ERAStock) = False Then
    #        If youngestAge = 1 Then
    #            WriteLine(Log_OlderThanMaxAge_ID, ShakerMethod, "", youngestAge, "", "will not be used in Exploitation Rate Analysis.")
    #        ElseIf youngestAge > 1 Then
    #            MsgBox("Youngest age = " & youngestAge & " is less than OceanStartAge in SuperStock.  Did you forget to combine age 2 and 3?")
    #        End If
    #    End If





}




