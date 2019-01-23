
#ERA function StartCohortAnalysis_Click()
#Translated from VB ERA CIS code
#January 2019
#Author: Catarina Wor








#' @title StartCohortAnalysis_Click
#'
#' @description  This function writes the Settings log file, i.e.: which ERA options did you choose for
#'  Average_Maturation_Rate, MeanMatType,PNVAlgorithm,ShakerMethod, IncompleteYearAlgorithm, and 
#' RoundRecoveriesByTagCode. It also retrieves additional data from the database using the following sub 
#' routines: GetPSCFisheries, GetERAFisheries, GetCalendarYears, GetERAStocks.
#'    
#'
#' @param M A list with inital settings to be used in the ERA
#'
#' @details
#'
#' @return A list containing all initial settings contained in M plus additional data gathered from other 
#' subfunctions: GetPSCFisheries, GetERAFisheries, GetCalendarYears, GetERAStocks
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
StartCohortAnalysis_Click <- function(M){

    #=====================================

    do.call(file.remove, list(list.files("../logs", full.names = TRUE)))

    #'verify settings with user before continuing to run program
    #print("Settings specs are in ../logs/Settings.log")

    sink("../logs/Settings.log")
    cat("You have selected the following settings:\n")

    if(M$Average_Maturation_Rate == "LongTermAverage"){

        cat("Average Maturation Rate: Long Term Average\n")

    }else if(M$Average_Maturation_Rate == "SelectNumberCompleteBroods" ){
    
        cat(paste("Average Maturation Rate: Last= ", M$LastCompleteBroodsUsed,"Complete Broods \n"))
    
    }


    if(M$MeanMatType=="ArithmeticMean"){
    
        cat("Arithmetic Average Maturity Rates \n")
    
    }else{
    
       cat("Geometic Average Maturity Rates \n")
    
    }

    if(M$PNVAlgorithm=="StockSpecific"){

        cat("Proportion Not Vulnerable setting: Stock Specific \n")

    }else if(M$PNVAlgorithm=="FisherySpecific"){

        cat("Proportion Not Vulnerable setting: Fishery Specific \n")

    }

    if(M$ShakerMethod == 1){

        cat("Using Shaker Method 1 \n")
    
    }else if(M$ShakerMethod == 4){
    
        cat("Using Shaker Method 4 \n")
    
    }

    if(M$IncompleteYearAlgorithm=="New"){

        cat("Use new incomplete brood year algorithm\n")

    }else if(M$IncompleteYearAlgorithm=="Historic"){

        cat("Use historic incomplete brood year algorithm\n")

    }
    
    if(M$RoundRecoveriesByTagCode== TRUE){
       
        cat("Round recoveries as in CAS CFiles \n")
        M$isReplicateCohShak <- TRUE

    }else{

        cat("Do NOT round recoveries\n")
        M$isReplicateCohShak <- FALSE


    }

    cat("If you need to change these settings, please change options in the input list M and re-sun the program.\n")

    sink()


    d <- GetPSCFisheries(M)
    M2 <- append(M,d)

    if(M$isReplicateCohShak){

        d1 <- GetERAFisheries(M2)
        M2 <- append(M2,d1)

    }

    d2 <- GetCalendarYears(M2)
    M2 <- append(M2,d2)

    d3 <- GetERAStocks(M2)
    M2 <- append(M2,d3)


    return(M2)




    # Version of the ERA used
    #    Dim SettingsMsg As MsgBoxResult
    #    'verify settings with user before continuing to run program
    #    If LongTermAverage.Checked = True Then
    #        SettingsSelected = SettingsSelected & vbCr & vbCr & "Average Maturation Rate: Long Term Average"
    #        SaveSettings = "Avg Mat Rate: Long Term Avg; "
    #    Else
    #        SettingsSelected = SettingsSelected & vbCr & vbCr & "Average Maturation Rate: Last " & MatRateNumberBroods.NumBroodsUpDown.Value & " Complete Broods"
    #        SaveSettings = "Avg Mat Rate: Last " & MatRateNumberBroods.NumBroodsUpDown.Value & " Complete Broods; "
    #    End If
    #    If StockSpecificPNV.Checked = True Then
    #        SettingsSelected = SettingsSelected & vbCr & "PNV setting: Stock Specific"
    #        SaveSettings = SaveSettings & "Stock Specific PNV; "
    #    Else
    #        SettingsSelected = SettingsSelected & vbCr & "PNV setting: Fishery Specific"
    #        SaveSettings = SaveSettings & "Fishery Specific PNV; "
    #    End If
    #
    #    If ArithmeticMean.Checked = True Then
    #        SettingsSelected = SettingsSelected & vbCr & "Arithmetic Average Maturity Rates"
    #        SaveSettings = SaveSettings & "Arithmetic Avg Mat Rates; "
    #    Else
    #        SettingsSelected = SettingsSelected & vbCr & "Geometic Average Maturity Rates"
    #        SaveSettings = SaveSettings & "Geometic Avg Mat Rates; "
    #    End If
    #
    #    If ShakerMethod1Menu.Checked = True Then
    #        SettingsSelected = SettingsSelected & vbCr & "Shaker Method 1"
    #        SaveSettings = SaveSettings & "Shaker Method 1; "
    #    Else
    #        SettingsSelected = SettingsSelected & vbCr & "Shaker Method 4"
    #        SaveSettings = SaveSettings & "Shaker Method 4; "
    #    End If
    #
    #    If NewIncBYRadio.Checked = True Then
    #        SettingsSelected = SettingsSelected & vbCr & "Use new incomplete brood year algorithm"
    #        SaveSettings = SaveSettings & "New Incomplete Brood Year Algorithm; "
    #    Else
    #        SettingsSelected = SettingsSelected & vbCr & "Use historic incomplete brood year algorithm"
    #        SaveSettings = SaveSettings & "Historic Incomplete Brood Year Algorithm; "
    #    End If

    #   If chkRoundRecoveriesByTagCode.Checked = True Then
    #       SettingsSelected = SettingsSelected & vbCr & "Round recoveries as in CAS CFiles"
    #       SaveSettings = SaveSettings & "Round recoveries as in CAS CFiles; "
    #       isReplicateCohShak = True
    #   ElseIf chkDoNOTRoundRecoveriesByTagCode.Checked = True Then
    #       SettingsSelected = SettingsSelected & vbCr & "Do NOT round recoveries"
    #       SaveSettings = SaveSettings & "Do NOT round recoveries; "
    #        isReplicateCohShak = False
    #    End If

    #    SettingsSelected = SettingsSelected & vbCr & vbCr & "If you need to change these settings, please click cancel and make the changes in the Settings menu, otherwise click ok."
    #    SettingsMsg = MsgBox(SettingsSelected, MsgBoxStyle.OkCancel, "Settings Selected")
    #    If SettingsMsg = MsgBoxResult.Cancel Then
    #        Exit Sub
    #    End If
    #    DatabaseNameLabel.Visible = False
    #    PluginLabel.Visible = False
    #    Me.Cursor = Cursors.WaitCursor
    #    If CISDBConnection.State = ConnectionState.Closed Then
    #        CISDBConnection.Open()
    #    End If
    #    Call GetPSCFisheries()
    #    If isReplicateCohShak = True Then Call GetERAFisheries()
    #    Call GetCalendarYears()
    #    Call GetERAStocks()
    #    MenuStrip1.Visible = False
    #    LastYearCheckedListBox.Visible = True
    #    StockListView.Visible = True
    #    RunYearLabel.Visible = True
    #    StocksToRunLabel.Visible = True
    #    AllStocksCheckBox.Visible = True
    #    CalculateButton.Visible = True
    #    Me.Cursor = Cursors.Default



}

