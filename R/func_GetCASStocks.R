#=======================================================
#ERA function GetCASStocks()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


#' @title GetCASStocks
#'
#' @description  Get the array of CASStocks corresponding to the current ERAStock (e.g. ANB,ACI,ALP,ADM,AHC for ERA stock AKS)
#' 
#' 
#'
#' @param curr_stk A string specifying the current ERAstock, defined in D 
#' 
#' @param dbse  database location, defined in M 
#'
#' @details 
#'
#' @return CASStockString: a string or vector
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetCASStocks <- function(curr_stk=CurrentStock,dbse=M$datbse){

	#curr_stk=CurrentStock
	#dbse=M$datbse

	require(RODBC)

	#dta <- RODBC::odbcConnectAccess2007(dbse)   #specifies the file path
	

	ERASQL <- paste0("Select CASStock from ERA_CASStockToERAStockMapping Where ERAStock = '",curr_stk, "'")

	#  single quotes need to exist. 

	CASStockString <- RODBC::sqlQuery( M$chnl , query = ERASQL )

	

	# 'Get the array of CASStocks corresponding to the current ERAStock (e.g. ANB,ACI,ALP,ADM,AHC for ERA stock AKS)
	#CASStockString <- df1$CASStock[df1$ERAStock==curr_stk]

	


	# 'Get the array of CASStocks corresponding to the current ERAStock (e.g. ANB,ACI,ALP,ADM,AHC for ERA stock AKS)
    #   ERASQL = "Select CASStock from ERA_CASStockToERAStockMapping Where ERAStock = '" & CurrentStock & "'"
    #    Dim CASStockTable As New DataTable
    #    CISDataAdapter.Fill(CASStockTable)
    #    Dim NumberOfCASStocks As Integer = CASStockTable.Rows.Count
    #    For CASStock As Integer = 1 To NumberOfCASStocks
    #        CASStockString = CASStockString & "'" & CASStockTable.Rows.Item(CASStock - 1).Item(0) & "'" & ","
    #    Next
    #    Try
    #        CASStockString = Microsoft.VisualBasic.Left(CASStockString, CASStockString.Length - 1)
    #    Catch
    #        MsgBox("Cannot find ERAStock " & CurrentStock & " in ERA_CASStockToERAStockMapping.  Program is going to stop.")
    #        End
    #    End Try

	return(CASStockString)
	 

} 




