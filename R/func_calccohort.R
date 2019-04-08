#=======================================================
#ERA function CalcCohort
#Translated from VB ERA CIS code
#March 2019
#Author: Catarina Wor
#=======================================================






#' @title CheckIMData
#'
#' @description function where the actual ERa is done, this one is for complete cohorts only. 
#'  
#' @param M A list passed to StartCohortAnalysis_Click
#' 
#' @param D  A list containing stock specific info
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
CalcCohort <- function(D,M){


	allBY <- D$FirstBY:D$LastBY
    #allCY <- (D$FirstBY + D$OceanStartAge):M$LastCalendarYear
    allAge <- D$OceanStartAge:D$MaxAge



    NumberOceanCohorts <- NULL
	SumAEQ <- NULL
	SumMatRate <- NULL
	SumPreTermER <- NULL
	PreTermER <- matrix(NA, nrow=length(allBY),ncol=7)
	Cohort <- matrix(NA, nrow=length(allBY),ncol=7)

	SurvivalRate <- D$survivaldf$SurvivalRate

	if( M$Average_Maturation_Rate == "SelectNumberCompleteBroods" ){
		
		NumAvgYears <- sum(allBY < (M$LastCalendarYear-D$MaxAge))
	
	}else{
		
		NumAvgYears <- M$LastCompleteBroodsUsed
	
	}

	NumAvgYears <- MatRateNumberBroods.NumBroodsUpDown.Value

	for( BYind in seq_along(allBY) ){

		#'skip all missing brood years

		if( !D$MissingBroodYearFlag[BYind] ){
			
			#'the first time, this sub is called then Cohort(BroodYear, OceanStartAge) is empty or zero
			TestCohortNum <- Cohort[BYind, D$OceanStartAge]

			#'set survival rate of maxage+1 for use in AEQ equations below
			SurvivalRate[ which(D$survivaldf$Age==D$MaxAge) + 1 ] <- SurvivalRate[ which(D$survivaldf$Age==D$MaxAge) ]
			
			#'loop through each age
			for( age in D$LastAge[BYind]:D$OceanStartAge ){
				
				#'sum up total catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff), first time through will just be landed catch
				TotalCatch <- TotalCatch_Age(D, M, BY=BYind, age)
				#'sum up total terminal catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff)
         		if( M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod ){

         			sink("../logs/debug_terminalCatchID.log", append=TRUE)
            		cat(paste("2183 CalcCohort callTotalTerminalCatch_Age", M$ShakerMethod, BYind, age))
            		sink()

         		}

         		TotalTerminalCatch <- TotalTerminalCatch_Age(D, M, BYind, age)
         		#'subtract total terminal catch from total catch to get total PreTerm catch
         		TotalPreTermCatch <- TotalCatch - TotalTerminalCatch

         		if(M$isTraceCalc){

         			sink("../logs/debug_CohortID.log", append=TRUE)
            		cat(paste("line 1925","totCat", TotalCatch, "totTermCat", TotalTerminalCatch))
            		sink()

         		}

         		#'estimate cohort size for last available age of an incomplete cohort, otherwise estimate cohort size from backwards cohort analysis
         		if( !D$CompleteBYFlag[BYind] & age == D$LastAge[BYind] & D$pass > 1 ){
         		
         			Cohort[BYind, age] <- CalcEstmCohrt2( D, M, BYind, age ) / D$survivaldf$SurvivalRate[ survivaldf$Age==age ]
         			#'If isTraceCalc = True and shakermethod = traceThisShakerMethod And BroodYear>= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1818 compl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age), SurvivalRate(age))
         			
         			if( M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod ){
         				sink("../logs/debug_Cohort_IncompleteBroodID.log", append=TRUE)
            			cat(paste("2158 CalcCohort", M$ShakerMethod, BYind, age, Cohort[BYind, D$OceanStartAge]))
            			sink()
         			}
         			
         		}else{
         			
         			Cohort[ BYind, age ] <- (Cohort[ BYind, age + 1 ] +  Escape[ BYind, age ] + TotalCatch)/ SurvivalRate[ which(D$survivaldf$Age==age) ]
         		
         		}

         		if( age == D$OceanStartAge & Cohort[BYind, D$OceanStartAge] == 0){

         			sink("../logs/debug_ERROR_noCWT.log", append=TRUE)
            		cat(paste("Brood Year ", allBY[BYind], " has NO CWT recoveries at all!"))
            		sink()
         			ErrCaption = "Brood Year " & allBY[BYind] & " has NO CWT recoveries at all!"
            		ErrMessage = "Hint: If the brood year has no recoveries, decrement last brood year in the .CM1 file and remove from the .CDS file."
            		ErrMessage = ErrMessage & "  If intermediate brood year, treat as missing brood and remove from .CDS file."
         		}

			}
		}
	}

	#original vb code
		#	 Dim ActDif As Single
		#    Dim CohortAfterPreTermFishery As Single
		#    Dim CohortBeforePreTermFishery As Single
		#    Dim TerminalRun As Single
		#    Dim TotalCatch, TotalTerminalCatch As Single
		#    Dim TotalPreTermCatch As Single
		#    Dim NumberOceanCohorts(MaxAge) As Integer
		#    Dim SumAEQ(MaxAge) As Single
		#    Dim SumMatRate(MaxAge) As Single
		#    Dim SumPreTermER(MaxAge) As Single
		#    ReDim PreTermER(LastBY, MaxAge)
		#    ReDim AveragePreTermER(MaxAge)
		#    NumAvgYears = MatRateNumberBroods.NumBroodsUpDown.Value
		#    RepeatPass = False
		#    For BroodYear As Integer = FirstBY To LastBY
		#      'skip all missing brood years
		#      If MissingBroodYearFlag(BroodYear) = False Then
		#        'the first time, this sub is called then Cohort(BroodYear, OceanStartAge) is empty or zero
		#        TestCohortNum = Cohort(BroodYear, OceanStartAge)
		#        'set survival rate of maxage+1 for use in AEQ equations below
		#        SurvivalRate(MaxAge + 1) = SurvivalRate(MaxAge)
		#        'loop through each age
		#        For age As Integer = LastAge(BroodYear) To OceanStartAge Step -1
		#          'sum up total catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff), first time through will just be landed catch
		#          TotalCatch = TotalCatch_Age(BroodYear, age, pass)
		#          'sum up total terminal catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff)
		#          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2183 CalcCohort callTotalTerminalCatch_Age", ShakerMethod, BroodYear, age)
		#          TotalTerminalCatch = TotalTerminalCatch_Age(BroodYear, age, pass)
		#          'subtract total terminal catch from total catch to get total PreTerm catch
		#          TotalPreTermCatch = TotalCatch - TotalTerminalCatch
		#          If isTraceCalc = True Then WriteLine(debug_CohortID, "line 1925", "totCat", TotalCatch, "totTermCat", TotalTerminalCatch)
		#          'estimate cohort size for last available age of an incomplete cohort, otherwise estimate cohort size from backwards cohort analysis
		#          If CompleteBYFlag(BroodYear) = False And age = LastAge(BroodYear) And pass > 1 Then
		#            Cohort(BroodYear, age) = CalcEstmCohrt2(BroodYear, age) / SurvivalRate(age)
		#            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_Cohort_IncompleteBroodID, "2158 CalcCohort", ShakerMethod, BroodYear, age, Cohort(BroodYear, age))
		#          Else
		#            Cohort(BroodYear, age) = (Cohort(BroodYear, age + 1) + Escape(BroodYear, age) + TotalCatch) / SurvivalRate(age)
		#          End If
		#
		#          If age = OceanStartAge And Cohort(BroodYear, OceanStartAge) = 0 Then
		#            ErrCaption = "Brood Year " & BroodYear & " has NO CWT recoveries at all!"
		#            ErrMessage = "Hint: If the brood year has no recoveries, decrement last brood year in the .CM1 file and remove from the .CDS file."
		#            ErrMessage = ErrMessage & "  If intermediate brood year, treat as missing brood and remove from .CDS file."
		#            Call PrintError(ErrMessage, ErrCaption)
		#          End If
		#          CohortAfterPreTermFishery = Cohort(BroodYear, age) * SurvivalRate(age) - TotalPreTermCatch
		#          TerminalRun = TotalTerminalCatch + Escape(BroodYear, age)
		#          If ReadAvgMatRteFlg = False Then
		#            'rates not from ERA_Stock table, instead calculate from data
		#            If age = LastAge(BroodYear) And CompleteBYFlag(BroodYear) = True Then 'And CohortAfterPreTermFishery > 0 Then
		#              MatRate(BroodYear, age) = 1
		#            ElseIf CohortAfterPreTermFishery <= 0 Then
		#              MatRate(BroodYear, age) = 0
		#            Else
		#              MatRate(BroodYear, age) = TerminalRun / CohortAfterPreTermFishery
		#              If isTraceCalc = True Then WriteLine(debug_MatRteID, "2189 MatRte ", BroodYear, age, MatRate(BroodYear, age), TerminalRun, CohortAfterPreTermFishery)
		#            End If
		#            'WriteLine(debugID, "calcCohort", BroodYear, age, ShakerMethod, MatRate(BroodYear, age))
		#          Else
		#            'average mat. rates from ERA_Stock table
		#            MatRate(BroodYear, age) = AverageMatRate(age)
		#          End If
		#          'compute adult equivalent factor
		#          If age = LastAge(BroodYear) And CompleteBYFlag(BroodYear) = False Then 'use average adult equivalent for incomplete brood
		#            AEQ(BroodYear, age) = MatRate(BroodYear, age) + ((1 - MatRate(BroodYear, age)) * SurvivalRate(age + 1) * AverageAEQ(age + 1))
		#          Else
		#            AEQ(BroodYear, age) = MatRate(BroodYear, age) + ((1 - MatRate(BroodYear, age)) * SurvivalRate(age + 1) * AEQ(BroodYear, age + 1))
		#          End If
		#          CohortBeforePreTermFishery = Cohort(BroodYear, age) * SurvivalRate(age)
		#          'Compute Ocean harvest rates
		#          If CohortBeforePreTermFishery > 0 Then
		#            PreTermER(BroodYear, age) = TotalPreTermCatch / CohortBeforePreTermFishery
		#          Else
		#            PreTermER(BroodYear, age) = 0
		#          End If
		#          If LongTermAverage.Checked = True Then
		#            'ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using longterm average
		#            If CompleteBYFlag(BroodYear) = True Then
		#              SumAEQ(age) = SumAEQ(age) + AEQ(BroodYear, age)
		#              SumPreTermER(age) = SumPreTermER(age) + PreTermER(BroodYear, age)
		#              NumberOceanCohorts(age) = NumberOceanCohorts(age) + 1
		#              'ACCUMULATE MATURATION RATES IF NOT USING INPUT AVERAGE
		#              'note: because AverageMatRate(age) = SumMatRate(age) / NumberCompleteBroods%
		#              'therefore SumMatRate(age) is not needed or used if ReadAvgMatRteFlg% = 1
		#              If ReadAvgMatRteFlg = False Then 'rates not from ERA_Stock table, instead calculate from data
		#                SumMatRate(age) = SumMatRate(age) + MatRate(BroodYear, age)
		#              End If
		#            End If
		#          Else
		#            'ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using average number of years specified BroodYear NumAvgYears
		#            If BroodYear = LastCompleteBroodYear Then
		#              For BroodYearNumber As Integer = 0 To NumAvgYears - 1
		#                SumAEQ(age) = SumAEQ(age) + AEQ(BroodYear - BroodYearNumber, age)
		#                SumPreTermER(age) = SumPreTermER(age) + PreTermER(BroodYear - BroodYearNumber, age)
		#                NumberOceanCohorts(age) = NumberOceanCohorts(age) + 1
		#                If ReadAvgMatRteFlg = False Then 'rates not from .cm1 file, instead calculate from data
		#                  SumMatRate(age) = SumMatRate(age) + MatRate(BroodYear - BroodYearNumber, age)
		#                End If
		#              Next BroodYearNumber
		#              NumberCompleteBroods = NumAvgYears
		#            End If
		#          End If
		#        Next age
		#        ActDif = 100 * (System.Math.Abs(Cohort(BroodYear, OceanStartAge) - TestCohortNum) / Cohort(BroodYear, OceanStartAge))
		#        If ActDif > TESTDIF And RepeatPass = False Then RepeatPass = True
		#      End If
		#    Next BroodYear
		#    'COMPUTE AVERAGE MATURATION RATES, OCEAN HARVEST RATES, AND ADULT EQUIVALENTS
		#    For Age = OceanStartAge To MaxAge
		#      If NumberOceanCohorts(Age) = 0 Then
		#        'there are never any ocean recoveries of this age
		#        AveragePreTermER(Age) = 0
		#      Else
		#        AveragePreTermER(Age) = SumPreTermER(Age) / NumberOceanCohorts(Age)
		#      End If
		#      If Age <> MaxAge Then
		#        'rates not from ERA_Stock table, instead calculate from data
		#        If ReadAvgMatRteFlg = False Then
		#          AverageMatRate(Age) = SumMatRate(Age) / NumberCompleteBroods
		#          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_AvgMatRteID, "2178 avgMatRte", Age, AverageMatRate(Age), "accum", SumMatRate(Age), "numBroods", NumberCompleteBroods)
		#        End If
		#        AverageAEQ(Age) = SumAEQ(Age) / NumberCompleteBroods
		#      Else
		#        'rate for oldest age not from .cm1 file, instead calculate from data
		#        If ReadAvgMatRteFlg = False Then AverageMatRate(MaxAge) = 1
		#        AverageAEQ(MaxAge) = 1
		#      End If
		#    Next Age
		#    'PUT AVERAGE VALUES INTO ARRAYS FOR INCOMPLETE BROODS
		#    For BY = FirstBY To LastBY
		#      If CompleteBYFlag(BY) = False Then
		#        For Age = OceanStartAge To MaxAge
		#          AEQ(BY, Age) = AverageAEQ(Age)
		#          MatRate(BY, Age) = AverageMatRate(Age)
		#          PreTermER(BY, Age) = AveragePreTermER(Age)
		#        Next Age
		#      End If
		#    Next BY

	

}









#' @title CalcEstmCohrt2
#'
#' @description . 
#'  
#' @param BroodYear integer indicating brood year
#' 
#' @param Age integer indicating age
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
CalcEstmCohrt2 <- function( D, M, BroodYear,Age ){

	#'This subroutine increases current age cohort sizes to correct for the lack of
    #'recoveries in older age classes in incomplete brood years (used by Calendar Year shaker
    #'option and Brood Year shaker option with oldest age and in final calculation of harvest rates).
    #'local variables

    if( D$MissingBroodYearFlag[BroodYear] ){
    
    	return(NULL)
    
    }else{
    
    	if( M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod ){
    
    		sink("../logs/debug_terminalCatchID.log", append=TRUE)
            cat(paste("2522 CalcEstmCohrt2 call TotalTerminalCatch_age",M$ShakerMethod,BroodYear, Age, "\n"))
            sink()
    
    	}

    	TerminalRun <- TotalTerminalCatch_Age(D, M, BroodYear, D$LastAge[BroodYear]) + D$Escape[BroodYear, D$LastAge[BroodYear]]
    	#'USE AVERAGE MATURATION RATE TO ESTIMATE COHORT SIZE AFTER MATURATION FOR INCOMPLETE BROOD
    	Chort <- (1 - D$AverageMatRate[D$LastAge[BroodYear]]) * TerminalRun / D$AverageMatRate[D$LastAge[BroodYear]]
    
    	if( M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear >= M$traceThisYear ){
    	
    		sink("../logs/debug_Cohort_IncompleteBroodID.log", append=TRUE)
            cat(paste("1991 calcestmcohrt2",M$ShakerMethod,BroodYear, Age,
            	Chort, "avgMatRte", D$AverageMatRate[D$LastAge[BroodYear]], "termRun", TerminalRun, "\n"))
            sink()
    	
    	}
    	#'RECONSTRUCT COHORT SIZES BroodYear ADDING CATCH, ESCAPEMENT, AND NATURAL MORTALITY
    	for( A in D$LastAge[BroodYear]:Age ){
    	
    		Chort <- Chort + D$Escape[BroodYear, A] + TotalCatch_Age( D, M, BroodYear, A ) 
    	
    		if( M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear >= M$traceThisYear ){
    	
    			sink("../logs/debug_Cohort_IncompleteBroodID.log", append=TRUE)
            	cat(paste("1996 calcestmcohrt2",M$ShakerMethod,BroodYear, A, "accum", Chort, "escape",
            	D$Escape[BroodYear, A], "totCat", TotalCatch_Age(D, M, BroodYear, A), "surv", 
            	D$survivaldf$SurvivalRate[survivaldf$Age==A], "\n"))
            	sink() 
    		}

    		if( A != Age ){ Chort = Chort/ D$survivaldf$SurvivalRate[survivaldf$Age==A] }
    	}
    	#'COHORT SIZE AFTER NATURAL MORTALITY
    	if( is.na(Chort) ){

    		return(0.0)

    	}else{

    		return(Chort)
    	
    	}

    }


	#Function CalcEstmCohrt2(ByRef BroodYear As Integer, ByRef Age As Integer) As Single
    #'This subroutine increases current age cohort sizes to correct for the lack of
    #'recoveries in older age classes in incomplete brood years (used by Calendar Year shaker
    #'option and Brood Year shaker option with oldest age and in final calculation of harvest rates).
    #'local variables
    #Dim Chort, TerminalRun As Single
    #If MissingBroodYearFlag(BroodYear) = True Then Exit Function
    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2522 CalcEstmCohrt2 call TotalTerminalCatch_age", ShakerMethod, BroodYear, Age)
    #
    #TerminalRun = TotalTerminalCatch_Age(BroodYear, LastAge(BroodYear), pass) + Escape(BroodYear, LastAge(BroodYear))
    #'USE AVERAGE MATURATION RATE TO ESTIMATE COHORT SIZE AFTER MATURATION FOR INCOMPLETE BROOD
    #Chort = (1 - AverageMatRate(LastAge(BroodYear))) * TerminalRun / AverageMatRate(LastAge(BroodYear))
    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1991 calcestmcohrt2", ShakerMethod, BroodYear, Age, Chort, "avgMatRte", AverageMatRate(LastAge(BroodYear)), "termRun", TerminalRun)
    #'RECONSTRUCT COHORT SIZES BroodYear ADDING CATCH, ESCAPEMENT, AND NATURAL MORTALITY
    #For A As Integer = LastAge(BroodYear) To Age Step -1
    #  Chort = Chort + Escape(BroodYear, A) + TotalCatch_Age(BroodYear, A, pass)
    #  If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1996 calcestmcohrt2", ShakerMethod, BroodYear, A, "accum", Chort, "escape", Escape(BroodYear, A), "totCat", TotalCatch_Age(BroodYear, A, pass), "surv", SurvivalRate(A))
    #  If A <> Age Then Chort = Chort / SurvivalRate(A)
    #Next A
    #'COHORT SIZE AFTER NATURAL MORTALITY
    #If Single.IsNaN(Chort) Then
    #  CalcEstmCohrt2 = 0.0
	#
    #Else
    #  CalcEstmCohrt2 = Chort
	#
    #End If
  	#End Function


}