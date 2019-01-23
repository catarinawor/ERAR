#=======================================================
#ERA function BuildERAFisheryIdList() and BuildPSCFisheryIdList()
#Translated from VB ERA CIS code
#December 2018
#Author: Catarina Wor
#=======================================================






#' @title BuildPSCFisheryIdList
#'
#' @description   
#'  
#' 
#' @param M A list passed to StartCohortAnalysis_Click
#' 
#' @param PSCFish  number of PSC fisheries
#'
#' @details
#'
#' @return 
#' 
#' @export
#'
#' @examples
#' 
#' 
BuildPSCFisheryIdList <- function(M,PSCFish){

	#'**************************************************************************
    # ' BuildPSCFisheryIdList
    # '   Looks up all fine scale Fisheries that map to this PSCFishery 
    # '   as defined in the ERA_FisheryPSCFisheryMapping table and builds up the string
    # '   representing the list of Ids for a SQL statement
    #'**************************************************************************


	sSQL = paste0("SELECT ERA_Fishery.Id, ERA_FisheryPSCFisheryMapping.StartMonthDay, ERA_FisheryPSCFisheryMapping.EndMonthDay" , 
         " FROM ERA_PSCFishery INNER JOIN (ERA_Fishery INNER JOIN ERA_FisheryPSCFisheryMapping" ,
         " ON ERA_Fishery.Name = ERA_FisheryPSCFisheryMapping.Fishery) ON ERA_PSCFishery.Name = ERA_FisheryPSCFisheryMapping.PSCFishery",
         " where(PSCFishery = '", M$PSCFisheryName[PSCFish], "') order by Fishery")
	
	dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
	
	df1 <- RODBC::sqlQuery( dta , query =sSQL)

    NumberFisheries <- nrow(df1)
    FisheryId <- df1$Id
    StartMonthDay <- df1$StartMonthDay
    EndMonthDay <- df1$EndMonthDay
    FisheryIdList <- paste("(", toString(df1$Id),")")
    DateRangeClause <- " "

    sink("../logs/BuildPSCFisheryIdList.log",append=T)
        cat(paste("You don't have an entry in the ERA_FisheryPSCFisheryMapping table for PSCFishery #" , PSCFish, ".\n"))
    sink()

    return(list(NumberFisheries=NumberFisheries,
        FisheryId=FisheryId,
        StartMonthDay=StartMonthDay,
        EndMonthDay=EndMonthDay,
        FisheryIdList=FisheryIdList,
        DateRangeClause=DateRangeClause))
   

	#original vb code
	#===============================================================
	#'**************************************************************************
    #    ' BuildPSCFisheryIdList
    #    '   Looks up all fine scale Fisheries that map to this PSCFishery 
    #    '   as defined in the ERA_FisheryPSCFisheryMapping table and builds up the string
    #    '   representing the list of Ids for a SQL statement
    #    '**************************************************************************
    #    Dim sSQL As String
    #    'sSQL = "select Fishery, StartMonthDay, EndMonthDay " & _
    #    '"from ERA_FisheryPSCFisheryMapping where PSCFishery = " & PSCFishery & " order by Fishery"
	#
    #    sSQL = "SELECT ERA_Fishery.Id, ERA_FisheryPSCFisheryMapping.StartMonthDay, ERA_FisheryPSCFisheryMapping.EndMonthDay" _
    #    & " FROM ERA_PSCFishery INNER JOIN (ERA_Fishery INNER JOIN ERA_FisheryPSCFisheryMapping" _
    #    & " ON ERA_Fishery.Name = ERA_FisheryPSCFisheryMapping.Fishery) ON ERA_PSCFishery.Name = ERA_FisheryPSCFisheryMapping.PSCFishery" _
    #    & " where(PSCFishery = " & PSCFishery & " order by Fishery"
	#
    #    Dim FisheryId As Int32
    #    Dim NumberFisheries As Int32
    #    Dim StartMonthDay As Object, EndMonthDay As Object
    #    FisheryIdList = "("
    #    Try
    #        ERACommand = New OleDbCommand(sSQL, CISDBConnection)
    #        CISDataReader = ERACommand.ExecuteReader()
    #        While CISDataReader.Read()
    #            NumberFisheries = NumberFisheries + 1
    #            FisheryId = CISDataReader(0)
    #            StartMonthDay = If(IsDBNull(CISDataReader(1)), DBNull.Value, CISDataReader(1))
    #            EndMonthDay = If(IsDBNull(CISDataReader(2)), DBNull.Value, CISDataReader(2))
    #            If NumberFisheries > 1 Then
    #                FisheryIdList = FisheryIdList & ", "
    #            End If
    #            FisheryIdList = FisheryIdList & FisheryId.ToString 'comma separated list of fine scale fisheries that map to PSCFishery in question
    #            'Build up the date range clause, if applicable
    #            'NOTE: it is assumed that if StartDate and EndDate are defined,
    #            'then there will only be one fishery id for this PSC Fishery
    #            'If Not StartMonthDay Is DBNull.Value And Not EndMonthDay Is DBNull.Value Then
    #            '    Dim StartMonth As Int32, EndMonth As Int32
    #            '    Dim StartMonthStartDay As Int32
    #            '    Dim EndMonthEndDay As Int32
    #            '    StartMonth = Convert.ToInt32(Convert.ToString(StartMonthDay).Substring(0, 2))
    #            '    StartMonthStartDay = Convert.ToInt32(Convert.ToString(StartMonthDay).Substring(2, 2))
    #            '    EndMonth = Convert.ToInt32(Convert.ToString(EndMonthDay).Substring(0, 2))
    #            '    EndMonthEndDay = Convert.ToInt32(Convert.ToString(EndMonthDay).Substring(2, 2))
    #            '    DateRangeClause = " and ( ( (" & StartMonth.ToString & " = " & EndMonth.ToString & ") and (Month(r.RecoveryDate) = " & StartMonth.ToString & ") and (Day(r.RecoveryDate) between " & StartMonthStartDay.ToString & " and " & EndMonthEndDay.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " <> " & EndMonth.ToString & ") and (Month(r.RecoveryDate) = " & StartMonth.ToString & ") and (Day(r.RecoveryDate) >= " & StartMonthStartDay.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " <> " & EndMonth.ToString & ") and (Month(r.RecoveryDate) = " & EndMonth.ToString & ") and (Day(r.RecoveryDate) <= " & EndMonthEndDay.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " <> " & EndMonth.ToString & ") and (Month(r.RecoveryDate) > " & StartMonth.ToString & ") and (Month(r.RecoveryDate) < " & EndMonth.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " > " & EndMonth.ToString & ") and (Month(r.RecoveryDate) > " & StartMonth.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " > " & EndMonth.ToString & ") and (Month(r.RecoveryDate) < " & StartMonth.ToString & ") and (Month(r.RecoveryDate) < " & EndMonth.ToString & ")))"
    #            'Else
    #            DateRangeClause = Nothing
    #            'End If
    #        End While
    #    Catch
    #        MsgBox("You don't have an entry in the ERA_FisheryPSCFisheryMapping table for PSCFishery #" & PSCFishery & ".")
    #    Finally
    #        CISDataReader.Close()
    #        If FisheryIdList.Length > 1 Then
    #            FisheryIdList = FisheryIdList & ")"
    #        Else
    #            FisheryIdList = "(0)"
    #        End If
    #    End Try




}





#' @title BuildPSCFisheryIdList
#'
#' @description  Get  ERAFisheryIdList for a specific ERA fishery. Also StartMonthDay and EndMonthDay for fisheries that have spatio temporal delimitation
#'  
#' 
#' @param M A list passed to StartCohortAnalysis_Click
#' 
#' @param ERAFishery Number/name of PSC fishery
#'
#' @details
#'
#' @return 
#' 
#' @export
#'
#' @examples
#' 
#' 
BuildERAFisheryIdList <- function(M,ERAFishery){

	sSQL <- paste0("SELECT ERA_Fishery.ID, ERA_FisheryERAFishery.StartMonthDay, ERA_FisheryERAFishery.EndMonthDay ",
    	" FROM ERA_ERAFishery INNER JOIN (ERA_FisheryERAFishery INNER JOIN ERA_Fishery ",
    	" ON ERA_FisheryERAFishery.Fishery = ERA_Fishery.name) ",
   		" ON ERA_FisheryERAFishery.ERAFishery = ERA_ERAFishery.name ",
     	" WHERE ERA_ERAFishery.ID = ", ERAFishery , " ORDER BY ERA_Fishery.ID")


	dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
	
	df1 <- RODBC::sqlQuery( dta , query =sSQL)
 

	 FisheryIdList <-df1$ID
	 StartMonthDay <-df1$ID
	 EndMonthDay <-df1$ID

	 return(list( FisheryIdList= FisheryIdList,StartMonthDay=StartMonthDay,EndMonthDay=EndMonthDay ))

	#ERA original vb code
	#==============================================================
	#  '**************************************************************************
    #    ' BuildERAFisheryIdList
    #    '   Looks up all fine scale Fisheries that map to this ERAFishery 
    #    '   as defined in the ERA_FisheryERAFisheryMapping table and builds up the string
    #    '   representing the list of Ids for a SQL statement
    #    '**************************************************************************
    #    Dim sSQL As String
    #    'sSQL = "select Fishery, StartMonthDay, EndMonthDay " & _
    #    '"from ERA_FisheryERAFishery where ERAFishery = " & ERAFishery & " order by Fishery"
	#
    #    sSQL = "SELECT ERA_Fishery.ID, ERA_FisheryERAFishery.StartMonthDay, ERA_FisheryERAFishery.EndMonthDay " _
    #    & " FROM ERA_ERAFishery INNER JOIN (ERA_FisheryERAFishery INNER JOIN ERA_Fishery " _
    #    & " ON ERA_FisheryERAFishery.Fishery = ERA_Fishery.name) " _
    #    & " ON ERA_FisheryERAFishery.ERAFishery = ERA_ERAFishery.name " _
    #    & " WHERE ERA_ERAFishery.ID = " & ERAFishery & " ORDER BY ERA_Fishery.ID"
	#
    #    Dim FisheryId As Int32
    #    Dim NumberFisheries As Int32
    #    Dim StartMonthDay As Object, EndMonthDay As Object
    #    FisheryIdList = "("
    #    Try
    #        ERACommand = New OleDbCommand(sSQL, CISDBConnection)
	#
    #        Try
    #            CISDataReader = ERACommand.ExecuteReader()
    #        Catch ex As Exception
    #            MsgBox(ex.Message)
    #        End Try
	#
    #        While CISDataReader.Read()
    #            NumberFisheries = NumberFisheries + 1
    #            FisheryId = CISDataReader(0)
    #            StartMonthDay = If(IsDBNull(CISDataReader(1)), DBNull.Value, CISDataReader(1))
    #            EndMonthDay = If(IsDBNull(CISDataReader(2)), DBNull.Value, CISDataReader(2))
    #            If NumberFisheries > 1 Then
    #                FisheryIdList = FisheryIdList & ", "
    #            End If
    #            FisheryIdList = FisheryIdList & FisheryId.ToString 'comma separated list of fine scale fisheries that map to ERAFishery in question
    #            'Build up the date range clause, if applicable
    #            'NOTE: it is assumed that if StartDate and EndDate are defined,
    #            'then there will only be one fishery id for this PSC Fishery
    #            'If Not StartMonthDay Is DBNull.Value And Not EndMonthDay Is DBNull.Value Then
    #            '    Dim StartMonth As Int32, EndMonth As Int32
    #            '    Dim StartMonthStartDay As Int32
    #            '    Dim EndMonthEndDay As Int32
    #            '    StartMonth = Convert.ToInt32(Convert.ToString(StartMonthDay).Substring(0, 2))
    #            '    StartMonthStartDay = Convert.ToInt32(Convert.ToString(StartMonthDay).Substring(2, 2))
    #            '    EndMonth = Convert.ToInt32(Convert.ToString(EndMonthDay).Substring(0, 2))
    #            '    EndMonthEndDay = Convert.ToInt32(Convert.ToString(EndMonthDay).Substring(2, 2))
    #            '    DateRangeClause = " and ( ( (" & StartMonth.ToString & " = " & EndMonth.ToString & ") and (Month(r.RecoveryDate) = " & StartMonth.ToString & ") and (Day(r.RecoveryDate) between " & StartMonthStartDay.ToString & " and " & EndMonthEndDay.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " <> " & EndMonth.ToString & ") and (Month(r.RecoveryDate) = " & StartMonth.ToString & ") and (Day(r.RecoveryDate) >= " & StartMonthStartDay.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " <> " & EndMonth.ToString & ") and (Month(r.RecoveryDate) = " & EndMonth.ToString & ") and (Day(r.RecoveryDate) <= " & EndMonthEndDay.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " <> " & EndMonth.ToString & ") and (Month(r.RecoveryDate) > " & StartMonth.ToString & ") and (Month(r.RecoveryDate) < " & EndMonth.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " > " & EndMonth.ToString & ") and (Month(r.RecoveryDate) > " & StartMonth.ToString & ") ) " & _
    #            '                        "  or ( (" & StartMonth.ToString & " > " & EndMonth.ToString & ") and (Month(r.RecoveryDate) < " & StartMonth.ToString & ") and (Month(r.RecoveryDate) < " & EndMonth.ToString & ")))"
    #            'Else
    #            DateRangeClause = Nothing
    #            'End If
    #        End While
    #    Catch
    #        MsgBox("You don't have an entry in the ERA_FisheryERAFishery table for ERAFishery #" & ERAFishery & ".")
    #    Finally
    #        CISDataReader.Close()
    #        If FisheryIdList.Length > 1 Then
    #            FisheryIdList = FisheryIdList & ")"
    #        Else
    #            FisheryIdList = "(0)"
    #        End If
    #    End Try
	
}


