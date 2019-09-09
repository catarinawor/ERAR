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

  print("enter CalcCohort")

	allBY <- D$FirstBY:D$LastBY
  #allCY <- (D$FirstBY + D$OceanStartAge):M$LastCalendarYear
  allAge <- D$OceanStartAge:D$MaxAge

  NumberOceanCohorts <- NULL
	SumAEQ <- rep(0,D$MaxAge)
  AverageAEQ <- NULL
	SumMatRate <- rep(0,D$MaxAge)
	SumPreTermER <- rep(0,D$MaxAge)
  AveragePreTermER <- NULL
  AverageMatRate <- NULL
	PreTermER <- matrix(NA, nrow = length(allBY),ncol = 7 )
	Cohort <- matrix(0, nrow = length(allBY),ncol = 7 )
	MatRate <- matrix(0, nrow = length(allBY),ncol = 7 )

	AEQ <- matrix(0, nrow=length(allBY),ncol = D$MaxAge + 1 )
	SumAEQ <- rep(0,D$MaxAge)
	SumPreTermER <- rep(0,D$MaxAge)
	NumberOceanCohorts <- rep(0,D$MaxAge)

	SurvivalRate <- D$survivaldf$SurvivalRate
  NumberCompleteBroods<-D$NumberCompleteBroods
  pass<-D$pass

  RepeatPass <- FALSE

	if( M$Average_Maturation_Rate == "SelectNumberCompleteBroods" ){
		#This is not selecting the number of complte broods but choosing all the complete broods. 
		#NumAvgYears <- sum(allBY < (M$LastCalendarYear-D$MaxAge))
		NumAvgYears <- M$NumAvgYears	
	}else{		
		NumAvgYears <- M$LastCompleteBroodsUsed
	}

	for( BYind in seq_along(allBY) ){

		#'skip all missing brood years
		if( !D$MissingBroodYearFlag$Flag[ BYind ] & D$CompleteBYFlag$Flag[BYind]){
			
			#'the first time, this sub is called then Cohort(BroodYear, OceanStartAge) is empty or zero
			TestCohortNum <- Cohort[ BYind, D$OceanStartAge ]

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
        if( !D$CompleteBYFlag$Flag[BYind] & !is.na(D$CompleteBYFlag$Flag[BYind]) & age == D$LastAge[BYind] & pass > 1 ){
         		
         	Cohort[BYind, age] <- CalcEstmCohrt2( D, M, BYind, age ) / D$survivaldf$SurvivalRate[ D$survivaldf$Age==age ]
         	#'If isTraceCalc = True and shakermethod = traceThisShakerMethod And BroodYear>= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1818 compl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age), SurvivalRate(age))
         			
         	if( M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod ){
         		sink("../logs/debug_Cohort_IncompleteBroodID.log", append=TRUE)
            cat(paste("2158 CalcCohort", M$ShakerMethod, BYind, age, Cohort[BYind, D$OceanStartAge]))
            sink()
         	}
         			
        }else{
         			
         	Cohort[ BYind, age ] <- (Cohort[ BYind, age + 1 ] +  D$Escape[ BYind, age ] + TotalCatch)/ SurvivalRate[ which(D$survivaldf$Age==age) ]
         		
        }

        if( age == D$OceanStartAge & Cohort[BYind, D$OceanStartAge] == 0){

         	sink("../logs/debug_ERROR_noCWT.log", append=TRUE)
          cat(paste("Brood Year ", allBY[BYind], " has NO CWT recoveries at all!"))
          sink()
         	ErrCaption = paste("Brood Year ", allBY[BYind] , " has NO CWT recoveries at all! \n")
          ErrMessage = paste("Hint: If the brood year has no recoveries, decrement last brood year in the .CM1 file and remove from the .CDS file.\n")
          ErrMessage = paste(ErrMessage, " If intermediate brood year, treat as missing brood and remove from .CDS file.\n")
         	stop(paste(ErrCaption,ErrMessage))
        }

        CohortAfterPreTermFishery <- Cohort[ BYind, age ] * SurvivalRate[ age ] - TotalPreTermCatch
        TerminalRun <- TotalTerminalCatch + D$Escape[ BYind, age ] 
   
        if( !D$ReadAvgMatRteFlg ){
          #'rates not from ERA_Stock table, instead calculate from data
          if( age == D$LastAge[ BYind ] & D$CompleteBYFlag$Flag[ BYind ] &!is.na(D$CompleteBYFlag$Flag[ BYind ])){
          	MatRate[ BYind, age ] <- 1	
          }else if( CohortAfterPreTermFishery <= 0 ){
          	MatRate[ BYind, age ] <- 0
          }else{		
          	MatRate[ BYind, age ] <-  TerminalRun / CohortAfterPreTermFishery
          	if(M$isTraceCalc){
          		sink("../logs/debug_MatRteID.log", append=TRUE)
          		cat(paste("2189 MatRte", BYind, age, MatRate[ BYind, age ], TerminalRun, CohortAfterPreTermFishery,"\n"))
          		sink()
          	}
          }
        }else{
          MatRate[ BYind, age ] <- D$AverageMatRate[ age-1 ]	
        }

        # 'compute adult equivalent factor
        if(age == D$LastAge[ BYind ] & !D$CompleteBYFlag$Flag[ BYind ]){	
          #'use average adult equivalent for incomplete brood
          AEQ[ BYind, age ] <- MatRate[ BYind, age ] + ((1 - MatRate[ BYind, age ]) * SurvivalRate[ which(D$survivaldf$Age==age ) + 1 ] * AverageAEQ[age + 1])
        }else{
          AEQ[ BYind, age ] <- MatRate[ BYind, age ] + ((1 - MatRate[ BYind, age ]) * SurvivalRate[ which(D$survivaldf$Age==age) + 1 ]  * AEQ[ BYind, age + 1 ] )
        }
        CohortBeforePreTermFishery <- Cohort[ BYind, age ] * SurvivalRate[ which(D$survivaldf$Age==age) ]
        # 'Compute Ocean harvest rates
        if(CohortBeforePreTermFishery > 0 ){
          PreTermER[ BYind, age ] <- TotalPreTermCatch / CohortBeforePreTermFishery
        }else{
          PreTermER[ BYind, age ] <- 0
        }
        #parei aqui
        if(M$LongTermAverage){
          #ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using longterm average
          if(D$CompleteBYFlag$Flag[BYind]){
          	SumAEQ[age] <- SumAEQ[age] + AEQ[ BYind,age ]
          	SumPreTermER[age] <- SumPreTermER[age] + PreTermER[ BYind,age ]
          	NumberOceanCohorts[age] <- NumberOceanCohorts[age] + 1 
          				
            #'ACCUMULATE MATURATION RATES IF NOT USING INPUT AVERAGE
            #'note: because AverageMatRate(age) = SumMatRate(age) / NumberCompleteBroods%
            #'therefore SumMatRate(age) is not needed or used if ReadAvgMatRteFlg% = 1
          				
          	if( !D$ReadAvgMatRteFlg ){
          		SumMatRate[ age ] <- SumMatRate[ age ]  + MatRate[ BYind, age ]
          	}
          }
        }else{
          #'ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using average 
          #number of years specified BroodYear NumAvgYears
          if(allBY[ BYind ] == D$LastCompleteBroodYear ){
            
            for(BroodYearNumber in 1:NumAvgYears){
           		SumAEQ[age] <- SumAEQ[age] + AEQ[BYind - BroodYearNumber, age ]
           		SumPreTermER[age] <- sum(c(SumPreTermER[age], PreTermER[BYind - BroodYearNumber, age ]),na.rm=t)
              NumberOceanCohorts[age] <-  NumberOceanCohorts[age] + 1

              if(!D$ReadAvgMatRteFlg){
                #'rates not from .cm1 file, instead calculate from data
                SumMatRate[age] <- SumMatRate[age] + MatRate[BYind - BroodYearNumber, age ] 
              }	
           	}
            NumberCompleteBroods <- NumAvgYears
          }
        }           	 
			}
      ActDif <- 100 * (abs(Cohort[BYind, D$OceanStartAge] - TestCohortNum) / Cohort[BYind, D$OceanStartAge])
      if( ActDif > M$TESTDIF &  !RepeatPass ) RepeatPass <- TRUE
		}
	}
  #'COMPUTE AVERAGE MATURATION RATES, OCEAN HARVEST RATES, AND ADULT EQUIVALENTS
  for(age in D$OceanStartAge:D$MaxAge){
    if(NumberOceanCohorts[age] == 0){
      #'there are never any ocean recoveries of this age
       AveragePreTermER[age] <- 0
    }else{
      AveragePreTermER[age] <- SumPreTermER[age] / NumberOceanCohorts[age]
    }

    if(age != D$MaxAge){
      #'rates not from ERA_Stock table, instead calculate from data
      if(!D$ReadAvgMatRteFlg){
        AverageMatRate[age] = SumMatRate[age] / NumberCompleteBroods
        if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod){
          sink("../logs/debug_AvgMatRteID.log", append=TRUE)
          cat(paste("2178 avgMatRte", age, AverageMatRate[age], "accum", SumMatRate[age], "numBroods", NumberCompleteBroods,"\n"))
          sink()
        }
      }
      AverageAEQ[age] = SumAEQ[age] / NumberCompleteBroods
    }else{
      #'rate for oldest age not from .cm1 file, instead calculate from data
      if(!D$ReadAvgMatRteFlg) AverageMatRate[D$MaxAge] <- 1
      AverageAEQ[D$MaxAge] <- 1
    }
  }
  #'PUT AVERAGE VALUES INTO ARRAYS FOR INCOMPLETE BROODS
  for(BYind in seq_along(allBY) ){
    if(!D$CompleteBYFlag$Flag[BYind]){
      for(age in D$OceanStartAge:D$MaxAge){
        AEQ[BYind, age] <- AverageAEQ[age]
        MatRate[BYind, age] <- AverageMatRate[age]
        PreTermER[BYind, age] <- AveragePreTermER[age]
      }
    }
  }

  return(list(new=list(Cohort=Cohort,
          AEQ=AEQ,
          MatRate=MatRate,
          PreTermER= PreTermER,
          AverageAEQ=AverageAEQ,
          AverageMatRate =AverageMatRate,
          AveragePreTermER=AveragePreTermER,
          NumberOceanCohorts =NumberOceanCohorts,
          SumAEQ = SumAEQ,
          SumMatRate = SumMatRate,
          SumPreTermER = SumPreTermER),
     old=list(NumberCompleteBroods=NumberCompleteBroods,
              RepeatPass= RepeatPass)
  ))






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
  #print("Enter CalcEstmCohrt2")

	#'This subroutine increases current age cohort sizes to correct for the lack of
    #'recoveries in older age classes in incomplete brood years (used by Calendar Year shaker
    #'option and Brood Year shaker option with oldest age and in final calculation of harvest rates).
    #'local variables
    #BroodYear<-BYind
    #Age<-age
    if( D$MissingBroodYearFlag$Flag[BroodYear] ){
    
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
            	D$survivaldf$SurvivalRate[D$survivaldf$Age==A], "\n"))
            	sink() 
    		}

    		if( A != Age ){ Chort = Chort/ D$survivaldf$SurvivalRate[D$survivaldf$Age==A] }
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






#' @title CalcEstmCohrt
#'
#' @description . 
#'  
#' @param BroodYear integer indicating brood year
#' 
#' @param Age integer indicating age
#'
#' @details This subroutine estimates future age cohort sizes so that shakers can be 
#' calculated for incomplete brood years (Brood Year shaker option only).
#'
#' @return 
#' 
#' @export
#'
#' @examples
#' 
#' 
CalcEstmCohrt <- function( D, M, BY,Age ){

  #print("Enter CalcEstmCohrt")
  stopifnot(!D$MissingBroodYearFlag$Flag[BY])

  for(CurrAge in (D$LastAge[BY]+1):Age){
    PrevAge <- CurrAge - 1
    if(PrevAge == D$LastAge[BY]){
      #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2501 CalcEstmCohrt call TotalTerminalCatch_Age", ShakerMethod, BroodYear, Age)
      MatRun <- TotalTerminalCatch_Age(D, M, BY, PrevAge, D$pass) + D$Escape[BY, PrevAge]
    }else{
      #'Use Average Ocean Harvest Rate at current age and Avg Maturity Rate to
      #'estimate mature run
      MatRun <- cohrt * (1 - D$AveragePreTermER[CurrAge]) * D$AverageMatRate[PrevAge]
    }
    #'Use Avg Mat Rate to estimate incomplete cohort
      cohrt <- (1 - D$AverageMatRate[PrevAge]) * MatRun / D$AverageMatRate[PrevAge]
      #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1974 calcestmcohrt", ShakerMethod, BroodYear, CurrAge, cohrt, (1 - AverageMatRate(PrevAge)), MatRun, AverageMatRate(PrevAge))
      cohrt <- cohrt * D$survivaldf$SurvivalRate[D$survivaldf$Age==CurrAge]
  }
  if(is.na(cohrt)){
    CalcEstmCohrt <- 0
  }else{
    CalcEstmCohrt <- cohrt
  }

  return(CalcEstmCohrt)


  #========================================================================
  #Original vb code
  #========================================================================
  #'This subroutine estimates future age cohort sizes so that shakers can be
  #  'calculated for incomplete brood years (Brood Year shaker option only).
  #  Dim cohrt, MatRun As Single
  #  Dim CurrAge, PrevAge As Integer
  #  If MissingBroodYearFlag(BroodYear) = True Then Exit Function
  #  For CurrAge = LastAge(BroodYear) + 1 To Age
  #    PrevAge = CurrAge - 1
  #    If PrevAge = LastAge(BroodYear) Then
  #      If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2501 CalcEstmCohrt call TotalTerminalCatch_Age", ShakerMethod, BroodYear, Age)
  #      
  #      MatRun = TotalTerminalCatch_Age(BroodYear, PrevAge, pass) + Escape(BroodYear, PrevAge)
  #    Else
  #      'Use Average Ocean Harvest Rate at current age and Avg Maturity Rate to
  #      'estimate mature run
  #      MatRun = cohrt * (1 - AveragePreTermER(CurrAge)) * AverageMatRate(PrevAge)
  #    End If
  #    'Use Avg Mat Rate to estimate incomplete cohort
  #    cohrt = (1 - AverageMatRate(PrevAge)) * MatRun / AverageMatRate(PrevAge)
  #    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1974 calcestmcohrt", ShakerMethod, BroodYear, CurrAge, cohrt, (1 - AverageMatRate(PrevAge)), MatRun, AverageMatRate(PrevAge))
  #    cohrt = cohrt * SurvivalRate(CurrAge)
  #  Next CurrAge
  #  If Single.IsNaN(cohrt) Then
  #    CalcEstmCohrt = 0.0
  #  Else
  #
  #    CalcEstmCohrt = cohrt
  #  End If



}
