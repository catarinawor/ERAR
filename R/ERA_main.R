#===================================================================
#GENERAL INFORMATION
#This is an attempt to reproduce a version of the ERA program i the CIS database.
#This file is a simplified version of the current progam  and is not intended to replace the current method
#Comments fro original version of the model were preserved.
# Code was also written to maximize similarity between this version and the
# original VB version, functions are equivalent to the SUBs in the original code
#Author: Catarina Wor 
#Date: October 2018
# written in long form and with the intent to closely match the available vb code as an exercise
#===================================================================





#===================================================================
#VARIABLE DEFINTIONS -- this needs work

#-----------------#
#Indices/counters
#-----------------#
#ERAStock counter for user selected ERA stocks. 
#


#-----------------#
#Controls and flags
#-----------------#
#ArithmeticMeanFlag <- 1
#isTraceCalc <- FALSE
#isTraceByBroodYr <- TRUE
#isTraceByCalendarYr <- FALSE
#traceThisShakerMethod <- "C"




#-----------------#
#data
#-----------------#
#number stocks being modelled

#ERAStockArray
#maximum year in the data analysis


#-----------------#
#model variables
#-----------------#
#No idea if this is differnt from the MaxCalendarYear


#===================================================================

#load in all the sub functions
#this was only useful before this became a package
#funclib<- getwd()
#func_files <- list.files(funclib,pattern="func_",full.name=TRUE)

#for(fn in 1:length(func_files)){
#	source(func_files[fn])
#}

#***********************
#Settings
#####Average_Maturation_Rate
#LongTermAverage
#SelectNumberCompleteBroods

#####Mean type for Maturation rates
#ArithmeticMean
#GeometricMean

#I do not know what this is check the: Proportion non vulnerable 
###PNV Algorythm 
#StockSpecific
#FisherySpecific

#####IncompleteYearAlgorithm
#New
#Historic

#==========================================
#list of packages required to run this model
#RODBC






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

    sink("../logs/Settings.log")
    cat("You have selected the following settings:\n")

    if(M$Average_Maturation_Rate == "LongTermAverage"){

        cat("Average Maturation Rate: Long Term Average\n")

    }else if(M$Average_Maturation_Rate == "SelectNumberCompleteBroods" ){
    
        cat(paste("Selected to use the last", M$LastCompleteBroodsUsed,"Brood Years \n"))
    
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




#' @title CalculateButton_Click 
#'
#' @description  
#' 
#' 
#'
#' @param M A list. Otput of StartCohortAnalysis_Click()
#'
#' @details  This function calls the maisn ERA routine. utputs a few log files if there are any errors relating
#'
#' @return A list containing all initial M items plus additional output from MainSub
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
CalculateButton_Click  <- function(M){

   

    if(is.na(M$LastYearCheckedListBox)){

        sink("../logs/YearSelectionError.log")
        cat("Error: You must select a year before continuing.\n")
        cat("will continue assuming that the last year in the time series is the selected year\n")
        sink()

    }

    if(is.na(sum(M$StockListBox))){

        sink("../logs/StockListError.log")
        cat("Error: You must select at least one stock before continuing.\n")
        sink()

    }

    LastCalendarYear <- M$LastYearCheckedListBox
    MaxCalendarYear <- M$LastYearCheckedListBox
    

    if(length(M$isCombineAge2And3) != length(M$StockListBox)){
        
        sink("../logs/CombineAge.log")
        cat("Error: You must state if isCombineAge2And3 for each stock \n")
        sink()

    }

    if(length(M$isCombineAge5And6) != length(M$StockListBox)){

        sink("../logs/CombineAge.log")
        cat("Error: You must state if isCombineAge5And6 for each stock \n")
        sink()
        
    }

    M$ERAStockArray <- as.character(M$ERAStockTable[M$StockListBox,1])
    
    M$NumStocks <- length(M$ERAStockArray)


    
    D1<- MainSub(M)
    
    return(D1)

    # Original Version of the ERA 
    # If LastYearCheckedListBox.CheckedItems.Count = 0 Then
    #
    #        MsgBox("You must select a year before continuing.", , "Error")
    #        Exit Sub
    #    End If
    #    If StockListView.CheckedItems.Count = 0 Then
    #        MsgBox("You must select at least one stock before continuing.", , "Error")
    #        Exit Sub
    #    End If
    #    LastCalendarYear = LastYearCheckedListBox.CheckedItems(0)
    #    MaxCalendarYear = LastCalendarYear
    #    ReDim ERAStockArray(StockListView.CheckedItems.Count - 1)
    #    ReDim isCombineAge2And3(StockListView.CheckedItems.Count - 1)
    #    ReDim isCombineAge5And6(StockListView.CheckedItems.Count - 1)
    #    For stk As Integer = 0 To StockListView.CheckedItems.Count - 1
    #        ERAStockArray(stk) = StockListView.CheckedItems(stk).Text
    #        If StockListView.CheckedItems(stk).SubItems.Item(1).Text = Chr(254) Then 'checked
    #            isCombineAge2And3(stk) = True
    #        ElseIf StockListView.CheckedItems(stk).SubItems.Item(1).Text = Chr(168) Then 'unchecked
    #            isCombineAge2And3(stk) = False
    #        End If
    #
    #        If StockListView.CheckedItems(stk).SubItems.Item(2).Text = Chr(254) Then 'checked
    #            isCombineAge5And6(stk) = True
    #        ElseIf StockListView.CheckedItems(stk).SubItems.Item(2).Text = Chr(168) Then 'unchecked
    #            isCombineAge5And6(stk) = False
    #        End If
    #
    #        'for this to work, need to get OceanStartAge from SuperStock
    #        'If isCombineAge2And3(stk) = True Then
    #        '    If OceanStartAge <> 3 Then
    #        '        MsgBox("Coshak will NOT combine age 2 and 3 if start age in SuperStock table = 2")
    #        '        isCombineAge2And3(stk) = False
    #        '    Else
    #        '        UserSettings = UserSettings & "Combine age 2 and 3"
    #        '    End If
    #        'End If

    #        'If isCombineAge5And6(stk) = True Then
    #        '    If OceanStartAge <> 2 Then
    #        '        MsgBox("Coshak will NOT combine age 5 and 6 if start age in SuperStock table = 3")
    #        '        isCombineAge5And6(stk) = False
    #        '    Else
    #        '        UserSettings = UserSettings & "Combine age 5 and 6"
    #        '    End If
    #        'End If

    #    Next
    #    NumStocks = ERAStockArray.Length
    #    CalculateButton.Visible = False
    #    RunYearLabel.Visible = False
    #    LastYearCheckedListBox.Visible = False
    #    StockListView.Visible = False
    #    StocksToRunLabel.Visible = False
    #    AllStocksCheckBox.Visible = False
    #    Call MainSub()
    
   



}



#' @title MainSub 
#'
#' @description  Main routine that calculates the ERA, It calls a long list of subroutines 
#' 
#' 
#'
#' @param M A list. Output of StartCohortAnalysis_Click()
#'
#' @details It will create a list d with all the stock specific information
#'
#' @return A list ??? maybe something else? 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
MainSub<-function(M){


    for(ERAStock in 1:M$NumStocks){


        # Testing run - to be deleted
        # ERAStock <- 1

    	#CW Rfresh note
    	#potentially something relating to "read from database" or read from user inputs
 		#read in or refresh all user and database specified inputs
 		# Me.Refresh() 

        #list that will store all the ERAstock specific info
        D <- list()    
        #'set the current stock from the user selected ERAStock array

        #set TimePeriod to 1 for now, adjust code later when timesteps are functional
        D$TimePeriod <- 1

        #' timestep is only used when printing to output table, I think it is redundant with TimePeriod
        D$TimeStep <- 1 
        
        D$CurrentStock <- M$ERAStockArray[ERAStock]


        #'reset LastCalendarYear to MaxCalendarYear for each stock, LastCalendarYear is adjusted in GETIMDATA for some stocks
        M$LastCalendarYear <- M$MaxCalendarYear
        
        #'Loop through CalendarYear shaker method and BroodYear Shaker method
		
		#This part of the code outputs details about a total mortality calculations to a log file
		#-------------------------------------------------------------------------
        # "logfile Note:  all catches and escapements were expanded to the maximum level of release found for all brood years by the variable named RelRatio(BY) = MaxRelease/CWTRelease(BY)
        #This is to prevent bias resulting from differential levels of marking between brood years when the sub-legal CNR mortalities and Ocean Exploitation Rate are calculated.  This is because
        # CWT catch is compared to Actual Catch to calculate the proportion of the total catch assigned to the CWT stock.  If one brood year is under- or over- represented, it will bias the over-all assignment of sub-legal mortalities.  RelRatio is removed. e.g. catch(BY) = catch(BY)/RelRatio(BY), in the data below and in the database tables.
         
        #the following found in the ERA_Output_BroodYearExploitationRate table can be reconstructed from the data below
        #Cohort is after natural mortality")
        #totalMortality = LandedCatch + LegalDropoffMortality + SublegalShakerMortalities + SublegalDropoffMortalities + LegalCNRMortality + LegalCNRDropoffs + SubLegalCNRMortality + SubLegalCNRDropoffs")
        #Escapement includes Cost Rec, Pers Use, Sub, and Terminal Fishery Stray but does not include Esc Stray
        #AEQLandedCatchTotalRun = (AEQ * LandedCatch) + Escape
        #AEQTotalMortTotalRun = (AEQ * TotalMort) + Escape
        #LandedCatchTerminalRun = Catch + Escape
        #TotalMortTerminalRun = TotalMort + Escape
        #AEQPreterminalLandedCatchAllAges = AEQLandedCatchTotalRun - TerminalLandedCatchAllAges - Escape
        #AEQPreterminalTotalMortsAllAges = AEQTotalMortTotalRun - TerminalTotalMortsAllAges - Escape
        #TerminalLandedCatchAllAges = Terminal LandedCatch
        #TerminalTotalMortsAllAges = Terminal TotalMort

        #This is not the data that is being written, isntead just headers to a csv
        #Log_TotalMortDetails_ID<-data.frame("ShakerMethod"=ShakerMethod, "BY"=BY, "Age"=Age, "Fishery"=Fishery, "Total"=Total, "LandedCatch"=LandedCatch, 
        #	"LegalDropoffMortality"=LegalDropoffMortality, 
        #	"SublegalShakerMortalities"= SublegalShakerMortalities, 
        #	"SublegalDropoffMortalities"=SublegalDropoffMortalities,
        #	"LegalCNRMortality"=LegalCNRMortality, "LegalCNRDropoffs"=LegalCNRDropoffs, 
        #	"SubLegalCNRMortality"=SubLegalCNRMortality, 
        #	"SubLegalCNRDropoffs"=SubLegalCNRDropoffs, 
        #	"CohortAfterNatMort"=CohortAfterNatMort, "TermRun"=TermRun,
        #    "Escape"=Escape, "CanadaEscStray"=CanadaEscStray, "USEscStray"=USEscStray, 
        #    "AEQ"=AEQ, "RelRatio"=RelRatio)

         
        #This part of the code writes out another log file for fish that are observed to be older than the user set maximum age  
        #-------------------------------------------------------------------------  
         
        #build a log for fish that are observed to be older than the user set maximum age -- the following are older than MaxAge or younger than OceanStartAge specified in the CIS table named SuperStock
        Log_OlderThanMaxAge_ID <- paste0("../logs/","OlderThanMaxAge.log")
        sink(Log_OlderThanMaxAge_ID, append=T)
        cat(paste(D$CurrentStock,"\n"))
        cat("the following are older than MaxAge or younger than OceanStartAge specified in the CIS table named SuperStock.\n")
        cat("ShakerMethod BY age fishery \n")
        sink()
            
        #Log_OlderThanMaxAge_ID <- data.frame("ShakerMethod"ShakerMethod, "BY"=BY, "age"=age, "fishery"=fishery)
		#-------------------------------------------------------------------------  
        
        #I think these are only here to ensure that testing and comparison with CAs is efficient
        M$isTraceCalc <- FALSE
        M$isTraceByBroodYr <- TRUE
        M$isTraceByCalendarYr <- FALSE
        M$traceThisShakerMethod <- "C"

        if(M$isTraceCalc){

        	traceThisYear <- 1981
            traceThisFishery <- 58
            traceThisAge <-2
        	
        	#the vb code then creates and leave open a bunch of debug files 

            debug_calcEstmCohrtID <- list()
            debug_CalYrCatchID <- list()
            debug_AvgMatRteID <- list()
            debug_totCatAgeID <- list()
            debug_EncounterRateID <- list()
            debug_LegalDropoffID <- list()
            debug_CalYrShakersID <- list()
            debug_ShakerID <- list()
            debug_subLegalCNRID <- list()
            debug_LegalCNRID <- list()
            debug_CatchID <- list()
            debug_CohortID <- list()
            debug_Cohort_IncompleteBroodID <- list()
            debug_EscapeID <- list()
            debug_CNRNoDirID <- list()
            debug_MatRteID <- list()
            debug_SumMatAgeCatID <- list()
            debug_terminalFlagID <- list()
            debug_matShakersID <- list()
            debug_terminalCatchID <- list()

        }

        for(ShakCalcFlg in 1:2){

            #to be deleted
            #ShakCalcFlg<- 1            

        	#loop through once using the brood year method and then again using the calendar year method

        	#This version does not need a progress bar
         	#SetupProgressBar()

         	if(ShakCalcFlg == 1){

         		ShakerMethod <- "C"

         	}else if(ShakCalcFlg == 2) {

         		 ShakerMethod <- "B"

         	}

         	#reset CASStockString between shaker methods'
            #CASStockString <- NA
                
            #reset LastCompleteBroodYear to nothing between shaker methods and stocks
            #LastCompleteBroodYear <- NA

            #reset pass between shaker methods and stocks
            #pass <- 0
            
            #update stock label as progress is made - use this inestead of progerss bar
            ERAStockLabel.Text <- paste("Stock:", D$CurrentStock, "(", ERAStock , "of", M$NumStocks, ") Shaker Method:", ShakerMethod)
            MainSublog <- paste0("../logs/",D$CurrentStock,"_MainSub.log")
            sink(MainSublog)
            cat(paste(ERAStockLabel.Text,"\n"))
            sink()
            #ERAStockLabel.Visible <- TRUE
            #lblStatus.Visible <- FALSE

            D$CASStockString<-GetCASStocks(curr_stk=D$CurrentStock,dbse=M$datbse)
            
            #Get TermNetSwitchAge,OceanStartAge ,  MaxAge, SuperStock 
            D1<-GetSuperStockData(curr_stk=D$CurrentStock,dbse=M$datbse)
            D<-append(D,D1)
            #TermNetSwitchAge <- D$TermNetSwitchAge
            #OceanStartAge  <- D$OceanStartAge 
            #MaxAge  <- D$MaxAge 
            #SuperStock  <- D$SuperStock
            
                
            if(M$isCombineAge2And3[ERAStock]){
            	
            	if(D$OceanStartAge != 3){
            		
                    combineage23log <- paste0("../logs/",D$CurrentStock,"_CombineAge2And3.log")
                    sink(combineage23log)
                    cat(paste("Coshak will NOT combine age 2 and 3 for",D$CurrentStock, "if OceanStartAge in SuperStock table = 2.  Hint:  Look in ERA_Stock to find the corresponding SuperStock."))
                    sink()
            		
            		M$isCombineAge2And3[ERAStock] <- FALSE
            	
            	}else if(!M$isCombineAge5And6[ERAStock]){

            		if(M$ShakCalcFlg == 1){
            			#IDK -  what is this line doing? Just resetting the user setting?
            			# this seems unnecessary to me. 
            			#UserSettings = SaveSettings & "Combine age 2 & 3"
                        M$isCombineAge2And3[ERAStock] <- TRUE
            		}
            	}
            }

            if(M$isCombineAge5And6[ERAStock]){
            	if(D$OceanStartAge!= 2){

                    combineage56log <- paste0("../logs/",D$CurrentStock,"_CombineAge5And6.log")
                    sink(combineage56log)
                    cat(paste("Coshak will NOT combine age 5 and 6 ", D$CurrentStock, " if OceanStartAge in SuperStock table = 3.  Hint:  Look in ERA_Stock to find the corresponding SuperStock.\n"))
                    sink()
                
            	}else if(!M$isCombineAge2And3[ERAStock]){

            		if(M$ShakCalcFlg == 1){
            			#IDK -  what is this line doing? Just resetting the user setting?
            			# this seems unnecessary to me. 
            			#UserSettings = SaveSettings & "Combine age 5 & 6"
                        M$isCombineAge5And6[ERAStock] <-  TRUE
            		}
            	}
            }

            if(M$isCombineAge2And3[ERAStock]& M$isCombineAge5And6[ERAStock]){
                if(M$ShakCalcFlg == 1){
                  # UserSettings = SaveSettings & "Combine age 2 & 3; Combine age 5 & 6"  
                }
            }

            sink("../logs/MainSub.log")
            cat("Get First and Last Brood Year\n")
            sink()
           
            D1 <- GetFirstAndLastBY(D,M, ERAStock)
            try(if(D1$err_FirstAndLastBY ==1 ) stop(" MainSub stopped check  ../GetFirstAndLastBY.log"))            
            D <- append(D,D1)


            D1 <- GetMaxReleaseSize(D,M)
            try(if(D1$MaxReleaseErr == 1 )stop(" MainSub stopped check  ../GetMaxReleaseSize.log"))            
            D <- append(D,D1)

            #RedimensionArrays()

            sink(MainSublog, append=TRUE)
            cat("Get Tagged Release By Brood... \n")
            sink()

        
            D1 <- GetTaggedReleaseByBrood(D,M)
            D <- append(D,D1)

            D1 <- GetInterDamSurvival(D,M)
            D <- append(D,D1)

            D1 <- GetSurvivalRates(D,M)
            try(if(!D1$isOK ) stop(" MainSub stopped check  ../logs/GetSurvivalRates.log"))            
            D <- append(D,D1)
           

            D1<-GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates(D, M) #get within by flag, average mat rates, PNVregion try(if(D1$longerr ) stop(" MainSub stopped check  ../logs/GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates.log"))     
            D <- append(D,D1)  
            
            sink(MainSublog, append=TRUE)
            cat("Get PSL Data\n")
            sink()
         
            
            D1 <- GetIMData(D,M)
            try(if(D1$GetIMDataErr ) stop(" MainSub stopped check  ../logs/GetIMDataErr.log"))     
            D <- append(D,D1)  
            

            if(M$PNVAlgorithm=="StockSpecific"){
                # this is not used st the moment so these functions are not working properly
                D1<-GetMeanLength(D,M)
                D <- append(D,D1)

                D1<-GetSizeLimitLengthVulnerable(D,M)
                D <- append(D,D1)

                #need to implement this one - there is something wrong with the get mean length function. 
                D1<-CreatePNV(D,M)
                D <- append(D,D1)
            }


            D1<-SetTerminalFishery(D,M)
            D$terminal <- D1

            sink(MainSublog, append=TRUE)
            cat("Get Landed Catch and Escapement\n")
            sink()


            if(D$WithinBYWeightFlag){
                #not implemented yet$
            }else{
                #in progress
                D1 <- CalcLandedCatchAndEscapement(M,D)
                D <- append(D,D1)
            }
            #   lblStatus.Text = " "
  #              lblStatus.Visible = True
  #              Me.Refresh()
  #              If withinBYWeightFlag = True Then
  #                  Call CalcLandedCatchAndEscapementWithBYWeights()
  #              Else
  #                  Call CalcLandedCatchAndEscapement()
  #              End If
           


            




        } #next ShakCalcFlg

    } #next ERAStock
}

    
#Original ERA Mainsub in VB
#'for test only -- manually add URB fish that are missing in the 2016 CAS database but present in 2016 CFiles
#        'so differences related to with missing records in CAS database go away
#        'and therefore isolate differences that have nothing to do with missing records in CAS database
#        isreplicate2016URBdata = False
#
#        If ArithmeticMean.Checked = True Then
#            ArithmeticMeanFlag = True
#        Else
#            ArithmeticMeanFlag = False
#        End If
#
#        ERAStopWatch.Reset()
#        'Loop through cohort analysis for each ERA stock
#        'Use the Me. keyword to ensure that you are passing the specific instance of the class in which the code is curremtly executing
#        For Me.ERAStock = 0 To NumStocks - 1
#            '***Open new Excel workbook for test output
#            'oXL = CreateObject("Excel.application")
#            'oXL.DisplayAlerts = False
#            'oWB = oXL.Workbooks.Add()
#            Me.Refresh()
#            ERAStopWatch.Start()
#            'set TimePeriod to 1 for now, adjust code later when timesteps are functional
#            TimePeriod = 1
#            TimeStep = 1 ' timestep is only used when printing to output table, I think it is redundant with TimePeriod
#            'set the current stock from the user selected ERAStock array
#            CurrentStock = ERAStockArray(ERAStock)
#            'reset LastCalendarYear to MaxCalendarYear for each stock,LastCalendarYear is adjusted in GETIMDATA for some stocks
#            LastCalendarYear = MaxCalendarYear
#            'Loop through CalendarYear shaker method and BroodYear Shaker method
#
#            'This part of the code outputs details about a total mortality calculations to a log file
#            Log_TotalMortDetails_ID = FreeFile()
#
#            FileOpen(Log_TotalMortDetails_ID, DBFilePath & "\" & CurrentStock & "_TotalMortDetails.csv", OpenMode.Output)
#            WriteLine(Log_TotalMortDetails_ID, "Note:  all catches and escapements were expanded to the maximum level of release found for all brood years")
#            WriteLine(Log_TotalMortDetails_ID, "by the variable named RelRatio(BY) = MaxRelease/CWTRelease(BY)")
#            WriteLine(Log_TotalMortDetails_ID, "This is to prevent bias resulting from differential levels of marking between brood years")
#            WriteLine(Log_TotalMortDetails_ID, "when the sub-legal CNR mortalities and Ocean Exploitation Rate are calculated.  This is because")
#            WriteLine(Log_TotalMortDetails_ID, "CWT catch is compared to Actual Catch to calculate the proportion of the total catch assigned to the")
#            WriteLine(Log_TotalMortDetails_ID, "CWT stock.  If one brood year is under- or over- represented, it will bias the over-all assignment of")
#            WriteLine(Log_TotalMortDetails_ID, "sub-legal mortalities.  RelRatio is removed. e.g. catch(BY) = catch(BY)/RelRatio(BY), in the data below and in the database tables.")
#            WriteLine(Log_TotalMortDetails_ID)
#            WriteLine(Log_TotalMortDetails_ID, "the following found in the ERA_Output_BroodYearExploitationRate table can be reconstructed from the data below")
#            WriteLine(Log_TotalMortDetails_ID, "Cohort is after natural mortality")
#            WriteLine(Log_TotalMortDetails_ID, "totalMortality = LandedCatch + LegalDropoffMortality + SublegalShakerMortalities + SublegalDropoffMortalities + LegalCNRMortality + LegalCNRDropoffs + SubLegalCNRMortality + SubLegalCNRDropoffs")
#            WriteLine(Log_TotalMortDetails_ID, "Escapement includes Cost Rec, Pers Use, Sub, and Terminal Fishery Stray but does not include Esc Stray")
#            WriteLine(Log_TotalMortDetails_ID, "AEQLandedCatchTotalRun = (AEQ * LandedCatch) + Escape")
#            WriteLine(Log_TotalMortDetails_ID, "AEQTotalMortTotalRun = (AEQ * TotalMort) + Escape")
#            WriteLine(Log_TotalMortDetails_ID, "LandedCatchTerminalRun = Catch + Escape")
#            WriteLine(Log_TotalMortDetails_ID, "TotalMortTerminalRun = TotalMort + Escape")
#            WriteLine(Log_TotalMortDetails_ID, "AEQPreterminalLandedCatchAllAges = AEQLandedCatchTotalRun - TerminalLandedCatchAllAges - Escape")
#            WriteLine(Log_TotalMortDetails_ID, "AEQPreterminalTotalMortsAllAges = AEQTotalMortTotalRun - TerminalTotalMortsAllAges - Escape")
#            WriteLine(Log_TotalMortDetails_ID, "TerminalLandedCatchAllAges = Terminal LandedCatch")
#            WriteLine(Log_TotalMortDetails_ID, "TerminalTotalMortsAllAges = Terminal TotalMort")
#            WriteLine(Log_TotalMortDetails_ID)
#            WriteLine(Log_TotalMortDetails_ID, "ShakerMethod", "BY", "Age", "Fishery", "Total", "LandedCatch", "LegalDropoffMortality", "SublegalShakerMortalities", "SublegalDropoffMortalities",
#                      "LegalCNRMortality", "LegalCNRDropoffs", " SubLegalCNRMortality", "SubLegalCNRDropoffs", "CohortAfterNatMort", "TermRun",
#                      "Escape", "CanadaEscStray", "USEscStray", "AEQ", "RelRatio")
#            Log_OlderThanMaxAge_ID = FreeFile()
#            FileOpen(Log_OlderThanMaxAge_ID, DBFilePath & "\" & CurrentStock & "_OlderThanMaxAge.csv", OpenMode.Output)
#            WriteLine(Log_OlderThanMaxAge_ID, "the following are older than MaxAge or younger than OceanStartAge specified in the CIS table named SuperStock.")
#            WriteLine(Log_OlderThanMaxAge_ID, "ShakerMethod", "BY", "age", "fishery")
#
#            'debugID = FreeFile()
#            'FileOpen(debugID, DBFilePath & "\" & CurrentStock & "_debugAEQ_Output_ByStock.csv", OpenMode.Output)
#
#            isTraceCalc = False
#            isTraceByBroodYr = True
#            isTraceByCalendarYr = False
#            traceThisShakerMethod = "C"
#
#            If isTraceCalc = True Then
#                traceThisYear = 1981
#                traceThisFishery = 58
#                traceThisAge = 2
#
#                debug_calcEstmCohrtID = FreeFile()
#                FileOpen(debug_calcEstmCohrtID, DBFilePath & "\" & CurrentStock & "_debug_CIS_calcEstmCohrt.csv", OpenMode.Output)
#
#                debug_CalYrCatchID = FreeFile()
#                FileOpen(debug_CalYrCatchID, DBFilePath & "\" & CurrentStock & "_debug_CIS_CalYrCatch.csv", OpenMode.Output)
#
#                debug_AvgMatRteID = FreeFile()
#                FileOpen(debug_AvgMatRteID, DBFilePath & "\" & CurrentStock & "_debug_CIS_AvgMatRte.csv", OpenMode.Output)
#
#                debug_totCatAgeID = FreeFile()
#                FileOpen(debug_totCatAgeID, DBFilePath & "\" & CurrentStock & "_debug_CIS_totCatAge.csv", OpenMode.Output)
#
#                debug_EncounterRateID = FreeFile()
#                FileOpen(debug_EncounterRateID, DBFilePath & "\" & CurrentStock & "_debug_CIS_encounterRate.csv", OpenMode.Output)
#
#                debug_LegalDropoffID = FreeFile()
#                FileOpen(debug_LegalDropoffID, DBFilePath & "\" & CurrentStock & "_debug_CIS_legalDropoff.csv", OpenMode.Output)
#
#                debug_CalYrShakersID = FreeFile()
#                FileOpen(debug_CalYrShakersID, DBFilePath & "\" & CurrentStock & "_debug_CIS_CalYrShakers.csv", OpenMode.Output)
#
#                debug_ShakerID = FreeFile()
#                FileOpen(debug_ShakerID, DBFilePath & "\" & CurrentStock & "_debug_CIS_Shaker.csv", OpenMode.Output)
#
#                debug_subLegalCNRID = FreeFile()
#                FileOpen(debug_subLegalCNRID, DBFilePath & "\" & CurrentStock & "_debug_CIS_sublegalCNR.csv", OpenMode.Output)
#
#                debug_LegalCNRID = FreeFile()
#                FileOpen(debug_LegalCNRID, DBFilePath & "\" & CurrentStock & "_debug_CIS_legalCNR.csv", OpenMode.Output)
#
#                debug_CatchID = FreeFile()
#                FileOpen(debug_CatchID, DBFilePath & "\" & CurrentStock & "_debug_CIS_catch.csv", OpenMode.Output)
#
#                debug_CohortID = FreeFile()
#                FileOpen(debug_CohortID, DBFilePath & "\" & CurrentStock & "_debug_CIS_cohort.csv", OpenMode.Output)
#
#                debug_Cohort_IncompleteBroodID = FreeFile()
#                FileOpen(debug_Cohort_IncompleteBroodID, DBFilePath & "\" & CurrentStock & "_debug_CIS_cohort_incompletebrood.csv", OpenMode.Output)
#
#                debug_EscapeID = FreeFile()
#                FileOpen(debug_EscapeID, DBFilePath & "\" & CurrentStock & "_debug_CIS_escape.csv", OpenMode.Output)
#
#                debug_CNRNoDirID = FreeFile()
#                FileOpen(debug_CNRNoDirID, DBFilePath & "\" & CurrentStock & "_debug_CIS_CNRNoDir.csv", OpenMode.Output)
#
#                debug_MatRteID = FreeFile()
#                FileOpen(debug_MatRteID, DBFilePath & "\" & CurrentStock & "_debug_CIS_MatRate.csv", OpenMode.Output)
#
#                debug_SumMatAgeCatID = FreeFile()
#                FileOpen(debug_SumMatAgeCatID, DBFilePath & "\" & CurrentStock & "_debug_CIS_SumMatAgeCat.csv", OpenMode.Output)
#
#                debug_terminalFlagID = FreeFile()
#                FileOpen(debug_terminalFlagID, DBFilePath & "\" & CurrentStock & "_debug_CIS_terminalflag.csv", OpenMode.Output)
#
#                debug_matShakersID = FreeFile()
#                FileOpen(debug_matShakersID, DBFilePath & "\" & CurrentStock & "_debug_CIS_matShakers.csv", OpenMode.Output)
#
#                debug_terminalCatchID = FreeFile()
#                FileOpen(debug_terminalCatchID, DBFilePath & "\" & CurrentStock & "_debug_CIS_terminalCatch.csv", OpenMode.Output)
#            End If
#
#            For ShakCalcFlg As Integer = 1 To 2 'loop through once using the brood year method and then again using the calendar year method
#                Call SetupProgressBar()
#                If ShakCalcFlg = 1 Then
#                    ShakerMethod = "C"
#                ElseIf ShakCalcFlg = 2 Then
#                    ShakerMethod = "B"
#                End If
#                'reset CASStockString between shaker methods
#                CASStockString = Nothing
#                'reset LastCompleteBroodYear to nothing between shaker methods and stocks
#                LastCompleteBroodYear = Nothing
#                'reset pass between shaker methods and stocks
#                pass = 0
#                'update stock label as progress is made
#                ERAStockLabel.Text = "Stock: " & CurrentStock & " ( " & ERAStock + 1 & " of " & NumStocks & " ) Shaker Method: " & ShakerMethod
#                ERAStockLabel.Visible = True
#                lblStatus.Visible = False
#                Me.Refresh()
#                Call GetCASStocks()
#                Call GetSuperStockData()
#                If isCombineAge2And3(ERAStock) = True Then
#                    If OceanStartAge <> 3 Then
#                        MsgBox("Coshak will NOT combine age 2 and 3 for " & CurrentStock & " if OceanStartAge in SuperStock table = 2.  Hint:  Look in ERA_Stock to find the corresponding SuperStock.")
#                        isCombineAge2And3(ERAStock) = False
#                    ElseIf isCombineAge5And6(ERAStock) = False Then
#                        If ShakCalcFlg = 1 Then UserSettings = SaveSettings & "Combine age 2 & 3"
#                    End If
#                End If

#                If isCombineAge5And6(ERAStock) = True Then
#                    If OceanStartAge <> 2 Then
#                        MsgBox("Coshak will NOT combine age 5 and 6 " & CurrentStock & " if OceanStartAge in SuperStock table = 3.  Hint:  Look in ERA_Stock to find the corresponding SuperStock.")
#                        isCombineAge5And6(ERAStock) = False
#                    ElseIf isCombineAge2And3(ERAStock) = False Then
#                        If ShakCalcFlg = 1 Then UserSettings = SaveSettings & "Combine age 5 & 6"
#                    End If
#                End If
#
#                If isCombineAge2And3(ERAStock) = True And isCombineAge5And6(ERAStock) = True Then
#                    If ShakCalcFlg = 1 Then UserSettings = SaveSettings & "Combine age 2 & 3; Combine age 5 & 6"
#                End If
#
#                lblStatus.Text = " Get First and Last Brood Year"
#                lblStatus.Visible = True
#                Me.Refresh()
#                Call GetFirstAndLastBY()
#                Call GetMaxReleaseSize()
#                Call RedimensionArrays()
#                lblStatus.Text = " Get Tagged Release By Brood"
#                lblStatus.Visible = True
#                Me.Refresh()
#                Call GetTaggedReleaseByBrood()
#                Call GetInterDamSurvival()
#                Call GetSurvivalRates()
#                Call GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates() 'get within by flag, average mat rates, PNVregion
#                lblStatus.Text = " Get PSL Data"
#                lblStatus.Visible = True
#                Me.Refresh()
#                Call GetIMData(CurrentStock, PNVRegion)
#                If StockSpecificPNV.Checked Then
#                    Call GetMeanLength()
#                    Call GetSizeLimitLengthVulnerable()
#                    Call CreatePNV()
#                    'parei aqui
#                End If
#                Call SetTerminalFishery()
#                lblStatus.Text = " Get Landed Catch and Escapement"
#                lblStatus.Visible = True
#                Me.Refresh()
#                If withinBYWeightFlag = True Then
#                    Call CalcLandedCatchAndEscapementWithBYWeights()
#                Else
#                    Call CalcLandedCatchAndEscapement()
#                End If
#                Call AdjustLastBYandLastCY()
#                lblStatus.Text = " Calculate Total Calendar Year Catch"
#                lblStatus.Visible = True
#                Me.Refresh()
#                Call CalcCalendarYearCatch()
#                'Call TestLandedCatch("LandedCatch")
#                Call CheckIMData()
#                lblStatus.Text = " Calculate Cohort"
#                lblStatus.Visible = True
#                Me.Refresh()
#                'compute cohort sizes without CNR or Shakers, this is necessary to compute shakers and CNR
#                If Me.HistoricIncBYRadio.Checked = True Then
#                    Call CalcCohort()
#                ElseIf Me.NewIncBYRadio.Checked = True Then
#                    Call CalcCohort_IncompleteBrood()
#                End If
#                'Call TestCohortSize("BeforeIM")
#                Do
#                    pass = pass + 1
#                    lblStatus.Text = " Pass: " & pass & " Calculate Shakers"
#                    lblStatus.Visible = True
#                    Me.Refresh()
#                    If ShakerMethod1Menu.Checked = True Then
#                        Call ShakerMethod1()
#                    ElseIf ShakerMethod4Menu.Checked = True Then
#                        Call ShakerMethod4()
#                    End If
#                    lblStatus.Text = " Pass: " & pass & " Calculate CNR mortalities"
#                    lblStatus.Visible = True
#                    Me.Refresh()
#                    Call CalcCNR()
#                    lblStatus.Text = " Pass: " & pass & " Recalculate Cohort"
#                    lblStatus.Visible = True
#                    Me.Refresh()
#                    'compute cohort sizes with CNR and Shakers
#                    If Me.HistoricIncBYRadio.Checked = True Then
#                        Call CalcCohort()
#                    ElseIf Me.NewIncBYRadio.Checked = True Then
#                        Call CalcCohort_IncompleteBrood()
#                    End If
#                    'If pass = 1 Then
#                    'Call TestCohortSize("AfterIMPass" & pass)
#                    'Call TestIncidentalMortality("Pass" & pass)
#                    'End If
#                    If pass > 99 Then Exit Do
#                Loop Until RepeatPass = False
#                ProgressBar1.PerformStep()
#                Call ResetCatches()
#                lblStatus.Text = " Calculate Brood Year Exploitation Rates"
#                lblStatus.Visible = True
#                Me.Refresh()
#                Call CalcBroodYearExploitRates()
#                'Call TestHarvestRates()
#                'FileClose()
#                lblStatus.Text = " Write Output"
#                lblStatus.Visible = True
#                Me.Refresh()
#                Call OutputToDatabase()
#                'Call TestAEQMatRates()
#            Next ShakCalcFlg
#            'oWB.SaveAs(IO.Path.GetDirectoryName(DatabaseName) & "\" & CurrentStock & "_CIS")
#            'oXL.Quit()
#            FileClose(Log_TotalMortDetails_ID)
#            FileClose(Log_OlderThanMaxAge_ID)
#        Next ERAStock
#        Dim TotalRunTime As Single = Math.Round(ERAStopWatch.Elapsed.TotalMinutes, 1)
#        MsgBox("Cohort analysis completed. Output is in database. The total run time for " & NumStocks & " stocks was: " & TotalRunTime & " minutes.", , "Done")
#
#        'only enable File,Output,PlugIns, and Help menus
#        LoadMenu.Enabled = True
#        EditMenu.Enabled = True
#        SettingsMenu.Enabled = True
#        RunMenu.Enabled = True
#        ERAStockLabel.Visible = False
#        PluginsMenu.Enabled = True
#        OutputMenu.Enabled = True
#        MenuStrip1.Visible = True
#        ERALabel.Visible = False
#        ProgressBar1.Visible = False
#        DatabaseNameLabel.Visible = True
#        PluginLabel.Visible = True








