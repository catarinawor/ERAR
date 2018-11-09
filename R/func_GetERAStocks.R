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
#' @description  I think this function is supposed to chech which stocks were selected by the user? Not super sure. 
#' but this is what the function is doing right now. 
#' 
#' 
#'
#' @param M A list passed to StartCohortAnalysis_Click and appended to teh outputs of GetPSCFisheries and 
#'  GetCalendarYears
#'
#' @details
#'
#' @return A list with one object : ERAStockTable: a vector listing all ERA stock acronyms and StockListBox,
#'  which stocks were selected by the user.
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

    StockListBox <- match(M$StockListView,ERAStockTable[,1])

	D <- list(ERAStockTable=ERAStockTable,StockListBox=StockListBox)
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





