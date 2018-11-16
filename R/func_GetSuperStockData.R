#=======================================================
#ERA function GetCASStocks()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#Read in data straight from SQL database

#=======================================================





#' @title GetSuperStockData
#'
#' @description  Get Terminal Net Switch Age, OceanStartAge, and MaxAge for the ERAstock
#' 
#' 
#' @param curr_stk A string specifying the current ERAstock 
#' 
#' @param dbse  database location, defined in M 
#'
#' @details
#'
#' @return D: A list Terminal Net Switch Age, OceanStartAge, and MaxAge for the current ERA stock
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetSuperStockData <- function(curr_stk,dbse){

    dta <- RODBC::odbcConnectAccess2007(dbse)   #specifies the file path   

    ERASQL = paste0("Select TerminalNetAge,OceanStartAge,MaxAge,SuperStock.SuperStock from SuperStock INNER JOIN ERA_Stock ON SuperStock.SuperStock = ERA_Stock.SuperStock Where ERA_Stock.ERAStock = '",curr_stk,"'")
    df1 <- RODBC::sqlQuery( dta , query = ERASQL )

    D<-list(TermNetSwitchAge = df1[[1]],
        OceanStartAge = df1[[2]],
        MaxAge = df1[[3]],
        SuperStock = df1[[4]])

	# 'Get Terminal Net Switch Age, OceanStartAge, and MaxAge for the ERAstock
    #    ERASQL = "Select TerminalNetAge,OceanStartAge,MaxAge,SuperStock.SuperStock from SuperStock INNER JOIN ERA_Stock ON SuperStock.SuperStock = ERA_Stock.SuperStock Where ERA_Stock.ERAStock = '" & CurrentStock & "'"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    CISDataReader.Read()
    #    TermNetSwitchAge = CISDataReader(0)
    #    OceanStartAge = CISDataReader(1)
    #    MaxAge = CISDataReader(2)
    #    SuperStock = CISDataReader(3)
    #    CISDataReader.Close()

    return(D)
	 

} 



