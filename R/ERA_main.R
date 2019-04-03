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






#' @title CalculateButton_Click 
#'
#' @description  
#' 
#' 
#'
#' @param M  A list. Otput of StartCohortAnalysis_Click()
#'
#' @details  This function calls the main ERA routine. It also outputs a few log files if there are any errors relating to Year Selection (M$LastYearCheckedListBox is not NA),  Stock List (M$StockListError is not NA), length of isCombineAge2And3 and isCombineAge4And6
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


    if( is.na(M$LastYearCheckedListBox) ){

        sink("../logs/YearSelectionError.log")
        cat("Error: You must select a year before continuing.\n")
        cat("will continue assuming that the last year in the time series is the selected year\n")
        sink()

    }

    if( is.na(sum(M$StockListBox)) ){

        sink("../logs/StockListError.log")
        cat("Error: You must select at least one stock before continuing.\n")
        sink()

    }

    LastCalendarYear <- M$LastYearCheckedListBox
    MaxCalendarYear <- M$LastYearCheckedListBox
    

    if( length(M$isCombineAge2And3) != length(M$StockListBox) ){
        
        sink("../logs/CombineAge.log")
        cat("Error: You must state if isCombineAge2And3 for each stock \n")
        sink()

    }

    if( length(M$isCombineAge5And6) != length(M$StockListBox) ){

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


    for( ERAStock in 1:M$NumStocks ){


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
        Log_TotalMortDetails <- paste0("../logs/", D$CurrentStock,"_TotalMortDetails.log")
        sink(Log_TotalMortDetails, append=T)
        cat(paste(D$CurrentStock,"\n"))
        cat("Note:  all catches and escapements were expanded to the maximum level of release found for all brood years\n")
        cat("by the variable named RelRatio(BY) = MaxRelease/CWTRelease(BY)\n")
        cat("This is to prevent bias resulting from differential levels of marking between brood years \n")
        cat("when the sub-legal CNR mortalities and Ocean Exploitation Rate are calculated. This is because\n")
        cat("CWT catch is compared to Actual Catch to calculate the proportion of the total catch assigned to the\n")
        cat("CWT stock.  If one brood year is under- or over- represented, it will bias the over-all assignment of\n")
        cat(
            "sub-legal mortalities.  RelRatio is removed. e.g. catch(BY) = catch(BY)/RelRatio(BY), 
            in the data below and in the database tables.\n"
        )
        cat("\n")
        cat("the following found in the ERA_Output_BroodYearExploitationRate table can be reconstructed from the data below\n")
        cat("Cohort is after natural mortality\n")
        cat(
            "totalMortality = LandedCatch + LegalDropoffMortality + SublegalShakerMortalities + SublegalDropoffMortalities + 
            LegalCNRMortality + LegalCNRDropoffs + SubLegalCNRMortality + SubLegalCNRDropoffs\n"
        )
        cat("Escapement includes Cost Rec, Pers Use, Sub, and Terminal Fishery Stray but does not include Esc Stray\n")
        cat("AEQLandedCatchTotalRun = (AEQ * LandedCatch) + Escape\n")
        cat("AEQTotalMortTotalRun = (AEQ * TotalMort) + Escape\n")
        cat("LandedCatchTerminalRun = Catch + Escape\n")
        cat("TotalMortTerminalRun = TotalMort + Escape\n")
        cat("AEQPreterminalLandedCatchAllAges = AEQLandedCatchTotalRun - TerminalLandedCatchAllAges - Escape\n")
        cat("AEQPreterminalTotalMortsAllAges = AEQTotalMortTotalRun - TerminalTotalMortsAllAges - Escape\n")
        cat("TerminalLandedCatchAllAges = Terminal LandedCatch\n")
        cat("TerminalTotalMortsAllAges = Terminal TotalMort\n")
        cat("\n")
        cat(paste(
            "ShakerMethod", "BY", "Age", "Fishery", "Total", "LandedCatch", "LegalDropoffMortality", "SublegalShakerMortalities", 
            "SublegalDropoffMortalities","LegalCNRMortality", "LegalCNRDropoffs", " SubLegalCNRMortality", "SubLegalCNRDropoffs",
             "CohortAfterNatMort", "TermRun","Escape", "CanadaEscStray","USEscStray", "AEQ", "RelRatio","\n"
            )
        )
        sink()

         
        #This part of the code writes out another log file for fish that are observed to be older than the user set maximum age  
        #-------------------------------------------------------------------------  
         
        #build a log for fish that are observed to be older than the user set maximum age -- 
        #the following are older than MaxAge or younger than OceanStartAge specified in the CIS table named SuperStock
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

        if( M$isTraceCalc ){

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

        for( ShakCalcFlg in 1:2 ){

            #to be deleted
            #ShakCalcFlg<- 1            

        	#loop through once using the brood year method and then again using the calendar year method

        	#This version does not need a progress bar
         	#SetupProgressBar()

         	if( ShakCalcFlg == 1 ){

         		ShakerMethod <- "C"

         	}else if( ShakCalcFlg == 2 ) {

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

            D$CASStockString <- GetCASStocks(curr_stk=D$CurrentStock,dbse=M$datbse)
            
            #Get TermNetSwitchAge,OceanStartAge ,  MaxAge, SuperStock 
           
            D1 <- GetSuperStockData(curr_stk=D$CurrentStock,dbse=M$datbse)
            D <- append(D,D1)
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
           
            D1 <- GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates(D, M)   
            D <- append(D,D1)  
            
            sink(MainSublog, append=TRUE)
            cat("Get PSL Data\n")
            sink()
         
            D1 <- GetIMData(D,M)
            try(if(D1$GetIMDataErr ) stop(" MainSub stopped check  ../logs/GetIMDataErr.log"))     
            D <- append(D,D1)  
            
            if( M$PNVAlgorithm=="StockSpecific" ){
            
                # this is not used st the moment so these functions are not working properly
                D1 <- GetMeanLength(D,M)
                D <- append(D,D1)

                D <- append(D,D1)

                #need to implement this one - there is something wrong with the get mean length function. 
                D1 <- CreatePNV(D,M)
                D <- append(D,D1)
            }


            D1 <- SetTerminalFishery(D,M)
            D$terminal <- D1

            sink(MainSublog, append=TRUE)
            cat("Get Landed Catch and Escapement\n")
            sink()


            if( D$WithinBYWeightFlag ){
                #not implemented yet$
                stop("Routine not implemented - set D$WithinBYWeightFlag to 0")

            }else{
                #in progress
                D1 <- CalcLandedCatchAndEscapement(D,M)
                D <- append(D,D1)
            }

            D <- AdjustLastBYandLastCY(D)

            D1 <- CalcCalendarYearCatch(D)
            D <- append(D,D1)

            D1 <- CheckIMData(D,M)
            D <- append(D,D1)

            #'compute cohort sizes without CNR or Shakers, this is necessary to compute shakers and CNR
            if(M$IncompleteYearAlgorithm=="Historic"){
                #CalcCohort()

            }else if(M$IncompleteYearAlgorithm=="New"){

                #CalcCohort_IncompleteBrood()
            }

           

            




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








