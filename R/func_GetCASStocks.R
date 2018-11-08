#=======================================================
#ERA function GetCASStocks()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


GetCASStocks <- function(curr_stk,dbse){

	#curr_stk=CurrentStock
	#dbse=M$datbse

	require(RODBC)

	dta <- RODBC::odbcConnectAccess2007(dbse)   #specifies the file path
	#df1 <- RODBC::sqlFetch(dta, "ERA_CASStockToERAStockMapping")   #loads the table called 'bar' in the original Access file


	(ERASQL <- paste0("Select CASStock from ERA_CASStockToERAStockMapping Where ERAStock = '",curr_stk, "'"))

	#  single quotes need to exist. 

	CASStockString <- RODBC::sqlQuery( dta , query = ERASQL )

	

	# 'Get the array of CASStocks corresponding to the current ERAStock (e.g. ANB,ACI,ALP,ADM,AHC for ERA stock AKS)
	#CASStockString <- df1$CASStock[df1$ERAStock==curr_stk]

	#read sql query from file WITH variables and cleaning
	#.q <- read_sql("GetCASStocksSQLquery.sql")
	#if needs to change code
  	#.q <- inject_filter("AND C.SPECIES_CODE IN", curr_stk, sql_code = .q)



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




