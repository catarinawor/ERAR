#=======================================================
#ERA function GetPSCFisheries()
#Translated from VB ERA CIS code
#November 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


#' @title GetCalendarYears
#'
#' @description  Check which years have incidental mortality data available, and trim Calendar years available in
#'  the database to just those that are equal or lower that the finalyera set by the user and has incidental mortality data 
#' available.
#' 
#' 
#' @param M A list passed to StartCohortAnalysis_Click
#'
#' @details
#'
#' @return D: A list containing the following object: LastCalendarYear and RunYearList
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetCalendarYears <- function(M){

	#make sure to select calendar years for which there is Incidental mortality data
	#the original function also checks the final year with the user.

	nonIMPSCfisheries <- c("ESCAPEMENT","XCA ESC STRAY","XUS ESC STRAY")

	NumberIMPSCFisheries <- M$NumberPSCFisheries - sum(!is.na(match(M$PSCFisheryName,nonIMPSCfisheries)))

    dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path

    #which calendar years have Im data and what is the total number of fisheries
	ERASQL0 <- "SELECT Count(ERA_IMInputs.PSCFishery) AS CountOfPSCFishery, ERA_IMInputs.CalendarYear
	FROM ERA_IMInputs GROUP BY ERA_IMInputs.CalendarYear"

    df0 <-  RODBC::sqlQuery( dta , query = ERASQL0 )

    #which years have recovery data
    ERASQL <- "SELECT ERA_CWDBRecovery.RunYear FROM ERA_CWDBRecovery GROUP BY ERA_CWDBRecovery.RunYear"

    df1 <- RODBC::sqlQuery( dta , query = ERASQL )
   
	ERAyear <- df0$CalendarYear[df0$CountOfPSCFishery[which(df0$CalendarYear==df1[,1])]==NumberIMPSCFisheries]

    LastCalendarYear<-max(ERAyear[ERAyear<=M$MaxCalendarYear])
   

   	if(length(ERAyear[ERAyear<M$MaxCalendarYear])<length(df0$CalendarYear[df0$CalendarYear<M$MaxCalendarYear])){

   		sink("../logs/IncidentalMortalityMissing.log")
   		cat("ERA_IMInputs has fewer years than ERA_CWDBRecovery.  This may affect Alaska and Canada where there is an extra year of recovery data.  Meanwhile, only years in ERA_IMInputs will be used.\n")
 		sink()

   	}


	

	D <- list(LastCalendarYear=LastCalendarYear,RunYearList=ERAYear)
	return(D)

	#Original ERA code
	#Dim LastCalendarYearItemNumber As Integer
    #    Dim RunYearList As New List(Of String)
    #    'create a list of run years available in the ERA_CWDBRecovery table
    #    ERASQL = "SELECT DISTINCT RunYear FROM ERA_CWDBRecovery"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    Do While CISDataReader.Read()
    #        RunYearList.Add(CISDataReader(0))
    #        LastCalendarYearItemNumber = LastCalendarYearItemNumber + 1
    #        LastCalendarYear = CISDataReader(0)
    #    Loop
    #    CISDataReader.Close()
    #    For YR As Integer = 0 To RunYearList.Count - 1
    #        Call CheckIMData2(RunYearList(YR))
    #        If RemoveYearFlag = True Then
    #            ' don't include year in dropdown because IM data is not entered
    #            RunYearList.RemoveAt(YR)
    #            LastCalendarYearItemNumber = LastCalendarYearItemNumber - 1
    #        Else
    #            'include year in dropdown
    #        End If
    #    Next YR
    #    If RemoveYearFlag = True Then MsgBox("ERA_IMInputs has fewer years than ERA_CWDBRecovery.  This may affect Alaska and Canada where there is an extra year of recovery data.  Meanwhile, only years in ERA_IMInputs will appear next in the next screen.")
    #    If LastYearCheckedListBox.Items.Count = 0 Then 'populate listbox only if empty
    #        For i As Integer = 0 To RunYearList.Count - 1
    #            LastYearCheckedListBox.Items.Add(RunYearList.Item(i))
    #        Next i
    #        LastYearCheckedListBox.SetItemChecked(RunYearList.Count - 1, True)
    #    End If
    #    RunYearLabel.Visible = True
    #    Me.Refresh()

} 

