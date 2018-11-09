#=======================================================
#ERA function GetPSCFisheries()
#Translated from VB ERA CIS code
#November 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


#' @title GetERAStocks
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
GetERAStocks <- function(M){
	

	# 'Select available ERAStocks from database
	dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
    ERASQL <- "Select DISTINCT CASStock from ERA_WiretagCode ORDER by CASStock"

	ERAStockTable <-  RODBC::sqlQuery( dta , query = ERASQL )
    
	names(ERAStockTable) <- "ERAStockAcronym" 

	D <- list(ERAStockTable=ERAStockTable)
	return(D)



	# Original ERA code
	# 'Select available ERAStocks from database
    #    ERASQL = "Select DISTINCT CASStock from ERA_WiretagCode ORDER by CASStock"
    #    CISDataAdapter = New OleDbDataAdapter(ERASQL, CISDBConnection)
    #    Dim ERAStockTable As New DataTable
    #    CISDataAdapter.Fill(ERAStockTable)
    #    If StockListBox.Items.Count = 0 Then 'populate listbox only if empty
    #        'Populate a Checked list box with checkbox for each ERAstock
    #        For Each row As DataRow In ERAStockTable.Rows
    #            ERAStockAcronym = row.Item(0)
    #            StockListBox.Items.Add(ERAStockAcronym)
    #        Next
    #    End If
    #    For stk As Integer = 0 To StockListBox.Items.Count - 1
    #        StockListBox.SetItemCheckState(stk, CheckState.Unchecked)
    #    Next stk

} 





