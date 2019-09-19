#=======================================================
#ERA function CalcCohort
#Translated from VB ERA CIS code
#March 2019
#Author: Catarina Wor
#=======================================================






#' @title CalcCohort
#'
#' @description function where the actual ERA is done, this one is for complete cohorts only. 
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





#' @title CalcCohort_IncompleteBrood
#'
#' @description function where the actual ERA is done, this one is for incomplete cohorts only. 
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
CalcCohort_IncompleteBrood <- function(D,M){

  allBY <- D$FirstBY:D$LastBY

  NumberOceanCohorts <- rep(0,D$MaxAge)
  AvgAdltEqv <- rep(0,D$MaxAge)
  SumAEQ <- rep(0,D$MaxAge)
  #AverageAEQ <- NULL
  SumMatRate <- rep(0,D$MaxAge)
  SumPreTermER <- rep(0,D$MaxAge)

  #AveragePreTermER <- NULL
  AverageMatRate <- NULL
  #PreTermER <- matrix(NA, nrow = length(allBY),ncol = 7 )
  

  if("Cohort" %in% names(D)){
    Cohort <- D$Cohort
  }else{
    Cohort <- matrix(0, nrow = length(allBY),ncol = 7 )
  }
  if("MatRate" %in% names(D)){
    MatRate <- D$MatRate
  }else{
    MatRate <- matrix(0, nrow = length(allBY),ncol = 7 )
  }

  if("AEQ" %in% names(D)){
    AEQ <- D$AEQ
  }else{
    AEQ <- matrix(0, nrow = length(allBY),ncol = 7 )
  }

  if("AEQ" %in% names(D)){
    PreTermER <- D$PreTermER
  }else{
    PreTermER <- matrix(0, nrow = length(allBY),ncol = 7 )
  }

  if("AEQ" %in% names(D)){
    AveragePreTermER <- D$AveragePreTermER
  }else{
    AveragePreTermER <- matrix(0, nrow = length(allBY),ncol = 7 )
  }


  #

  #AEQ <- matrix(0, nrow=length(allBY),ncol = D$MaxAge + 1 )
  #SumAEQ <- rep(0,D$MaxAge)
  #SumPreTermER <- rep(0,D$MaxAge)
  #NumberOceanCohorts <- rep(0,D$MaxAge)


  
  #'determine the last complete brood year
  LastCompleteBroodYear<-max(D$CompleteBYFlag$BY[D$CompleteBYFlag$Flag])
  
  # 'Set AEQ, Mat rate, and ocean HR totals to 1 prior to calculating the products for use in a geometric mean, this eliminates multiplication by zero for the first BY in the avg. calcs.

  if(!M$ArithmeticMeanFlag){
    for(age in D$OceanStartAge:D$MaxAge){
      SumAEQ[age] <- 1
      SumPreTermER[age] <- 1
      SumMatRate[age] <- 1
    }
  }

  for(BroodYear in seq_along(allBY)){
    #'if missing brood year, then skip to next brood year that is not missing
    if(D$MissingBroodYearFlag$Flag[BroodYear]){
      next
    }     
    # 'the first time, this sub is called then Cohort(BroodYear, OceanStartAge) is empty or zero
    TestCohortNum <- Cohort[BroodYear, OceanStartAge]
  
    for(age in D$LastAge[BroodYear]:D$OceanStartAge){
      #'sum up total catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff), first time through will just be landed catch
      TotalCatch <- TotalCatch_Age(D, M, BY=BroodYear, age)
      #'sum up total terminal catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff)
      #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2340 CalcCohrt_inc callTotalTerminalCatch_Age", ShakerMethod, BroodYear, age)
      TotalTerminalCatch <- TotalTerminalCatch_Age(D, M, BY=BroodYear, age)

      #'subtract total terminal catch from total catch to get total PreTerm catch
      TotalPreTermCatch <- TotalCatch - TotalTerminalCatch
      
      #'estimate cohort size for last available age of an incomplete cohort, otherwise estimate cohort size from backwards cohort analysis
      #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "2078 CaclCohortinCompl BY", ShakerMethod, BroodYear, age, "totCat", TotalCatch, "totTermCat", TotalTerminalCatch)
      
      if(!D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY==allBY[BroodYear]] & age == D$LastAge[BroodYear] & pass > 1){
        Cohort[BroodYear,age] <- CalcEstmCohrt2(D, M, BroodYear, age) / D$survivaldf$SurvivalRate[D$survivaldf$Age==age]
        #'If isTraceCalc = True and shakermethod = traceThisShakerMethod And BroodYear>= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1818 compl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age), SurvivalRate(age))
        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_Cohort_IncompleteBroodID, "1818 CalcCohortincompl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age))
      }else{
        Cohort[BroodYear, age] <- (Cohort[BroodYear, age + 1] + D$Escape[BroodYear, age] + TotalCatch) / D$survivaldf$SurvivalRate[D$survivaldf$Age==age]
        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1821 CalcCohortinCompl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age), Cohort(BroodYear, age + 1), Escape(BroodYear, age), TotalCatch, SurvivalRate(age))
               
      }
      
      if(age == D$OceanStartAge & Cohort[BroodYear, D$OceanStartAge] == 0){
        ErrCaption <- paste("Brood Year", BroodYear, "has NO CWT recoveries at all! \n")
        ErrMessage <- paste("Hint: If the brood year has no recoveries, decrement last brood year")# in the .CM1 file and remove from the .CDS file."
        ErrMessage <- paste(ErrMessage,"If intermediate brood year, treat as missing brood")# and remove from .CDS file."
        error(paste(ErrCaption,ErrMessage))  
      }
      CohortAfterPreTermFishery <- Cohort[BroodYear, age] * D$survivaldf$SurvivalRate[age] - TotalPreTermCatch
      TerminalRun <- TotalTerminalCatch + D$Escape[BroodYear, age]                
      
      if(!D$ReadAvgMatRteFlg){
        #'rates not from ERA_Stock table, instead calculate from data
        if(age == D$LastAge[BroodYear] & D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY==allBY[BroodYear]] & CohortAfterPreTermFishery > 0  ){
          MatRate[BroodYear, age] <- 1
          #If isTraceCalc = True Then WriteLine(debug_MatRteID, "2587 MatRte ", MatRate(BroodYear, age))           
        }else if(CohortAfterPreTermFishery <= 0){
          MatRate[BroodYear, age] <- 0
          #If isTraceCalc = True Then WriteLine(debug_MatRteID, "2590 MatRte ", MatRate(BroodYear, age))
        }else{
          MatRate[BroodYear, age] <- TerminalRun / CohortAfterPreTermFishery
          #If isTraceCalc = True Then WriteLine(debug_MatRteID, "2591 MatRte ", CohortAfterPreTermFishery, Cohort(BroodYear, age), SurvivalRate(age), TotalPreTermCatch)
          #If isTraceCalc = True Then WriteLine(debug_MatRteID, "2592 MatRte ", TerminalRun, TotalTerminalCatch, Escape(BroodYear, age))
          #If isTraceCalc = True Then WriteLine(debug_MatRteID, "2593 MatRte ", ShakerMethod, BroodYear, age, MatRate(BroodYear, age), TerminalRun / RelRatio(BroodYear), CohortAfterPreTermFishery / RelRatio(BroodYear), TotalTerminalCatch / RelRatio(BroodYear), Escape(BroodYear, age) / RelRatio(BroodYear), Cohort(BroodYear, age) / RelRatio(BroodYear), SurvivalRate(age), TotalPreTermCatch / RelRatio(BroodYear), RelRatio(BroodYear))
        }
        #'If isTraceCalc = True And shakermethod = traceThisShakerMethod Then WriteLine(debugID, "incompleteBrood", BroodYear, age, ShakerMethod, MatRate(BroodYear, age))
      }else{
        #'average mat. rates from ERA_Stock table
        MatRate[BroodYear, age] <- D$AverageMatRate[age]
      }      

      #'compute adult equivalent factor
      if(age == D$LastAge[BroodYear] & !D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY==BroodYear]){
        #'use average adult equivalent for incomplete brood
        AEQ[BroodYear, age] <- MatRate[BroodYear, age] + ((1 - MatRate[BroodYear, age]) * D$survivaldf$SurvivalRate[D$survivaldf$Age==(age+1)] * AverageAEQ[age + 1])
      }else{
        AEQ[BroodYear, age] <- MatRate[BroodYear, age] + ((1 - MatRate[BroodYear, age]) * D$survivaldf$SurvivalRate[D$survivaldf$Age==(age+1)] * AEQ[BroodYear, age + 1])
      }
      CohortBeforePreTermFishery = Cohort[BroodYear, age] * D$survivaldf$SurvivalRate[D$survivaldf$Age == age]
      #'Compute Ocean harvest rates
      if(CohortBeforePreTermFishery > 0){
        PreTermER[BroodYear, age] <- TotalPreTermCatch / CohortBeforePreTermFishery
      }else{
        PreTermER[BroodYear, age] <- 0
      }
      if(M$ArithmeticMeanFlag){
        #'Summation of ADULT EQUIVALENTS,MATURATION RATES, and Ocean HRs FOR COMPLETE BROODS, for use in arithmetic mean
        if(M$LongTermAverage){
          #'ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using longterm average
          if(D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY==allBY[BroodYear]]){
            SumAEQ[age] <- SumAEQ[age] + AEQ[BroodYear, age]
            SumPreTermER[age] <- SumPreTermER[age] + PreTermER[BroodYear, age]
            NumberOceanCohorts[age] <- NumberOceanCohorts[age] + 1
            #'ACCUMULATE MATURATION RATES IF NOT USING INPUT AVERAGE
            #'note: because AverageMatRate(age) = SumMatRate(age) / NumberCompleteBroods%
            #'therefore SumMatRate(age) is not needed or used if ReadAvgMatRteFlg% = 1
            if(!D$ReadAvgMatRteFlg){
              #'rates not from ERA_Stock table, instead calculate from data
              if(MatRate[BroodYear, age] == 0){
                MatRate[BroodYear, age] <- 0.0000001 
                #'prevents SumMatRte = 0
              }
              SumMatRate[age] <- SumMatRate[age] + MatRate[BroodYear, age]
              #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_AvgMatRteID, "2625 avgMatRte LTA", BroodYear, age, SumMatRate(age), MatRate(BroodYear, age))
            }
          }
        }else{
          #'ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using average number of years specified BroodYear NumAvgYears
          if(allBY[BroodYear] == D$LastCompleteBroodYear){
            for(BroodYearNumber in 0:(M$NumAvgYears - 1)){
              SumAEQ[age] <- SumAEQ[age] + AEQ[BroodYear - BroodYearNumber, age] 
              SumPreTermER[age] <- SumPreTermER[age] + PreTermER[BroodYear - BroodYearNumber, age]
              NumberOceanCohorts[age] <- NumberOceanCohorts[age] + 1
              if(!D$ReadAvgMatRteFlg){
                if(MatRate[BroodYear - BroodYearNumber, age] == 0 ){
                  #'prevents SumMatRte = 0
                  MatRate[BroodYear - BroodYearNumber, age] = 0.0000001
                }
                SumMatRate[age] = SumMatRate[age] + MatRate[BroodYear - BroodYearNumber, age]
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_AvgMatRteID, "2638 avgMatRte", BroodYear - BroodYearNumber, age, SumMatRate(age), MatRate(BroodYear - BroodYearNumber, age))
              }
            }
            #NumberCompleteBroods = NumAvgYears
          } 
        }
      }else{
        #'Product of ADULT EQUIVALENTS,MATURATION RATES, and Ocean HRs FOR COMPLETE BROODS, for use in geometric mean
        if(M$LongTermAverage){
          #'Product of ADULT EQUIVALENTS AND MATURATION RATES and Ocean HRs FOR COMPLETE BROODS using longterm average
          if(D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY==allBY[BroodYear]]){
            SumAEQ[age] <- SumAEQ[age] * AEQ[BroodYear, age]
            if(PreTermER[BroodYear, age] == 0){
              PreTermER[BroodYear, age] <- 0.0000001  
            }
            SumPreTermER[age] <- SumPreTermER[age] * PreTermER[BroodYear, age]
            NumberOceanCohorts[age] <- NumberOceanCohorts[age] + 1
            if(D$ReadAvgMatRteFlg == 0){
              #'rates not from .cm1 file, instead calculate from data
              if( MatRate[BroodYear, age] == 0){
                #'prevents SumMatRte = 0
                MatRate(BroodYear, age) <- 0.0000001 
              }
              SumMatRate[age] = SumMatRate[age] * MatRate[BroodYear, age]
            } 
          }
        }else{
          #'Product of  ADULT EQUIVALENTS AND MATURATION RATES and Ocean HRs FOR COMPLETE BROODS using average number of years specified by NumAvgYears
          if(BroodYear == D$LastCompleteBroodYear){
            for(BY in 0:(M$NumAvgYears-1)){
              SumAEQ[age] = SumAEQ[age] * AEQ[BroodYear - BY, age]
              if(PreTermER[BroodYear, age] == 0){
                PreTermER[BroodYear, age] <- 0.0000001
              }
              SumPreTermER[age] <- SumPreTermER[age] * PreTermER[BroodYear - BY, age]
              NumberOceanCohorts[age] <- NumberOceanCohorts[age] + 1
              if(D$ReadAvgMatRteFlg ==0 ){
                #'rates not from .cm1 file, instead calculate from data
                if(MatRate[BroodYear- BY,age] == 0){
                  #'prevents a TotMatRate of 0
                  MatRate[BroodYear - BY, age] <- 0.0000001
                }
                SumMatRate[age] <- SumMatRate[age] * MatRate[BroodYear - BY, age]
              }
            }
            NumberCompleteBroods = NumAvgYears
          }
        }
      }
    }
    ActDif = 100 * (abs(Cohort[BroodYear, OceanStartAge] - TestCohortNum)/ Cohort[BroodYear, OceanStartAge])
    if(ActDif > TESTDIF & !D$RepeatPass ){
      RepeatPass = True
    }
  }
  #'COMPUTE AVERAGE MATURATION RATES, OCEAN HARVEST RATES, AND ADULT EQUIVALENTS
  for(age in D$OceanStartAge:D$MaxAge){
    if(M$ArithmeticMeanFlag){
      if(D$NumberOceanCohorts[age]==0){
        #'there are never any ocean recoveries of this age
        AveragePreTermER[age] <- 0
      }else{
        AverageMatRate[age] <- SumMatRate[age] / NumberCompleteBroods
      }
      if(age != D$MaxAge){
        #'rates not from ERA_Stock table, instead calculate from data
        if(!D$ReadAvgMatRteFlg){
          AverageMatRate[age] <- SumMatRate[age] / NumberCompleteBroods
        }
        AverageAEQ[age] <- SumAEQ[age] / NumberCompleteBroods
      }else{
        #'rate for oldest age not from .cm1 file, instead calculate from data
        if(!D$ReadAvgMatRteFlg){
          AverageMatRate[D$MaxAge] <- 1
        }
        AverageAEQ[D$MaxAge] <- 1
      }
    }else{
      #'if you want to use the geometric mean
      if(D$NumberOceanCohorts[age]==0){
        #'there are never any ocean recoveries of this age
        AveragePreTermER[age] <- 0
      }else{
        AveragePreTermER[age] = SumPreTermER[age]^(1 / NumberOceanCohorts[age])
      }
      
      if(age != D$MaxAge){
        if(D$ReadAvgMatRteFlg == 0){
          #'rates not from .cm1 file, instead calculate from data
          AverageMatRate[age] = SumMatRate[age] ^ (1 / D$NumberCompleteBroods)
          #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_AvgMatRteID, "2379 avgMatRte", BroodYear, Age, AverageMatRate(Age), "accum", SumMatRate(Age), "numBroods", NumberCompleteBroods)
        }
        AverageAEQ[age] <- SumAEQ[age] ^ (1 / D$NumberCompleteBroods)
      }else{
        if(D$ReadAvgMatRteFlg){
          #'rate for oldest age not from .cm1 file, instead calculate from data
          AverageMatRate[MaxAge] <- 1 
        }
        AverageAEQ[MaxAge] = 1
      }
    } #'ArithmeticMeanFlag = True
  }

    #==========================================================
    #Original VB code
#    Dim ActDif As Single
#        Dim AvgAdltEqv(MaxAge) As Single
#        Dim BroodYear As Integer
#        Dim CohortAfterPreTermFishery As Single
#        Dim CohortBeforePreTermFishery As Single
#        Dim LastCompleteBroodYear As Integer
#        Dim TerminalRun As Single
#        Dim TotalCatch, TotalTerminalCatch As Single
#        Dim TotalPreTermCatch As Single
#        Dim NumberOceanCohorts(MaxAge) As Integer
#        Dim SumAEQ(MaxAge) As Single
#        Dim SumMatRate(MaxAge) As Single
#        Dim SumPreTermER(MaxAge) As Single
#        ReDim PreTermER(LastBY, MaxAge)
#        ReDim AveragePreTermER(MaxAge)
#        NumAvgYears = MatRateNumberBroods.NumBroodsUpDown.Value
#        RepeatPass = False
#
#        'determine the last complete brood year
#        For BY = FirstBY To LastBY
#            'skip over missing brood years, 
#            If MissingBroodYearFlag(BY) = True Then BY = FindBY(BY)
#            If CompleteBYFlag(BY) = True Then
#                LastCompleteBroodYear = BY
#            End If
#        Next BY
#        'Set AEQ, Mat rate, and ocean HR totals to 1 prior to calculating the products for use in a geometric mean, this eliminates multiplication by zero for the first BY in the avg. calcs.
#        If ArithmeticMeanFlag = False Then
#            For Age = OceanStartAge To MaxAge
#                SumAEQ(Age) = 1
#                SumPreTermER(Age) = 1
#                SumMatRate(Age) = 1
#            Next Age
#        End If
#
#        For BroodYear = FirstBY To LastBY
#            'if missing brood year, then skip to next brood year that is not missing
#            If MissingBroodYearFlag(BroodYear) = True Then BroodYear = FindBY(BroodYear)
#            'the first time, this sub is called then Cohort(BroodYear, OceanStartAge) is empty or zero
#            TestCohortNum = Cohort(BroodYear, OceanStartAge)
#            'loop through each age
#            For age = LastAge(BroodYear) To OceanStartAge Step -1
#                'sum up total catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff), first time through will just be landed catch
#                TotalCatch = TotalCatch_Age(BroodYear, age, pass)
#                'sum up total terminal catch BroodYear age (landed catch + legal dropoff+ shakers + shakerdropoffs + legalCNR + sublegalCNR + legalCNRdropoff + sublegalCNRdropoff)
#                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2340 CalcCohrt_inc callTotalTerminalCatch_Age", ShakerMethod, BroodYear, age)
#                TotalTerminalCatch = TotalTerminalCatch_Age(BroodYear, age, pass)
#                'subtract total terminal catch from total catch to get total PreTerm catch
#                TotalPreTermCatch = TotalCatch - TotalTerminalCatch
#                'estimate cohort size for last available age of an incomplete cohort, otherwise estimate cohort size from backwards cohort analysis
#                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "2078 CaclCohortinCompl BY", ShakerMethod, BroodYear, age, "totCat", TotalCatch, "totTermCat", TotalTerminalCatch)
#                If CompleteBYFlag(BroodYear) = False And age = LastAge(BroodYear) And pass > 1 Then
#                    Cohort(BroodYear, age) = CalcEstmCohrt2(BroodYear, age) / SurvivalRate(age)
#                    'If isTraceCalc = True and shakermethod = traceThisShakerMethod And BroodYear>= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1818 compl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age), SurvivalRate(age))
#                    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_Cohort_IncompleteBroodID, "1818 CalcCohortincompl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age))
#                Else
#                    Cohort(BroodYear, age) = (Cohort(BroodYear, age + 1) + Escape(BroodYear, age) + TotalCatch) / SurvivalRate(age)
#                    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_Cohort_IncompleteBroodID, "1821 CalcCohortinCompl BY", ShakerMethod, BroodYear, age, Cohort(BroodYear, age), Cohort(BroodYear, age + 1), Escape(BroodYear, age), TotalCatch, SurvivalRate(age))
#                End If
#
#                If age = OceanStartAge And Cohort(BroodYear, OceanStartAge) = 0 Then
#                    ErrCaption = "Brood Year " & BroodYear & " has NO CWT recoveries at all!"
#                    ErrMessage = "Hint: If the brood year has no recoveries, decrement last brood year in the .CM1 file and remove from the .CDS file."
#                    ErrMessage = ErrMessage & "  If intermediate brood year, treat as missing brood and remove from .CDS file."
#                    Call PrintError(ErrMessage, ErrCaption)
#                End If
#                CohortAfterPreTermFishery = Cohort(BroodYear, age) * SurvivalRate(age) - TotalPreTermCatch
#                TerminalRun = TotalTerminalCatch + Escape(BroodYear, age)
#                If ReadAvgMatRteFlg = False Then
#                    'rates not from ERA_Stock table, instead calculate from data
#                    If age = LastAge(BroodYear) And CompleteBYFlag(BroodYear) = True And CohortAfterPreTermFishery > 0 Then
#                        MatRate(BroodYear, age) = 1
#                        If isTraceCalc = True Then WriteLine(debug_MatRteID, "2587 MatRte ", MatRate(BroodYear, age))
#                    ElseIf CohortAfterPreTermFishery <= 0 Then
#                        MatRate(BroodYear, age) = 0
#                        If isTraceCalc = True Then WriteLine(debug_MatRteID, "2590 MatRte ", MatRate(BroodYear, age))
#                    Else
#                        MatRate(BroodYear, age) = TerminalRun / CohortAfterPreTermFishery
#                        If isTraceCalc = True Then WriteLine(debug_MatRteID, "2591 MatRte ", CohortAfterPreTermFishery, Cohort(BroodYear, age), SurvivalRate(age), TotalPreTermCatch)
#                        If isTraceCalc = True Then WriteLine(debug_MatRteID, "2592 MatRte ", TerminalRun, TotalTerminalCatch, Escape(BroodYear, age))
#                        If isTraceCalc = True Then WriteLine(debug_MatRteID, "2593 MatRte ", ShakerMethod, BroodYear, age, MatRate(BroodYear, age), TerminalRun / RelRatio(BroodYear), CohortAfterPreTermFishery / RelRatio(BroodYear), TotalTerminalCatch / RelRatio(BroodYear), Escape(BroodYear, age) / RelRatio(BroodYear), Cohort(BroodYear, age) / RelRatio(BroodYear), SurvivalRate(age), TotalPreTermCatch / RelRatio(BroodYear), RelRatio(BroodYear))
#                    End If
#                    'If isTraceCalc = True And shakermethod = traceThisShakerMethod Then WriteLine(debugID, "incompleteBrood", BroodYear, age, ShakerMethod, MatRate(BroodYear, age))
#                Else
#                    'average mat. rates from ERA_Stock table
#                    MatRate(BroodYear, age) = AverageMatRate(age)
#                End If
#
#                'compute adult equivalent factor
#                If age = LastAge(BroodYear) And CompleteBYFlag(BroodYear) = False Then 'use average adult equivalent for incomplete brood
#                    AEQ(BroodYear, age) = MatRate(BroodYear, age) + ((1 - MatRate(BroodYear, age)) * SurvivalRate(age + 1) * AverageAEQ(age + 1))
#                Else
#                    AEQ(BroodYear, age) = MatRate(BroodYear, age) + ((1 - MatRate(BroodYear, age)) * SurvivalRate(age + 1) * AEQ(BroodYear, age + 1))
#                End If
#                CohortBeforePreTermFishery = Cohort(BroodYear, age) * SurvivalRate(age)
#                'Compute Ocean harvest rates
#                If CohortBeforePreTermFishery > 0 Then
#                    PreTermER(BroodYear, age) = TotalPreTermCatch / CohortBeforePreTermFishery
#                Else
#                    PreTermER(BroodYear, age) = 0
#                End If
#                If ArithmeticMeanFlag = True Then 'Summation of ADULT EQUIVALENTS,MATURATION RATES, and Ocean HRs FOR COMPLETE BROODS, for use in arithmetic mean
#                    If LongTermAverage.Checked = True Then
#                        'ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using longterm average
#                        If CompleteBYFlag(BroodYear) = True Then
#                            SumAEQ(age) = SumAEQ(age) + AEQ(BroodYear, age)
#                            SumPreTermER(age) = SumPreTermER(age) + PreTermER(BroodYear, age)
#                            NumberOceanCohorts(age) = NumberOceanCohorts(age) + 1
#                            'ACCUMULATE MATURATION RATES IF NOT USING INPUT AVERAGE
#                            'note: because AverageMatRate(age) = SumMatRate(age) / NumberCompleteBroods%
#                            'therefore SumMatRate(age) is not needed or used if ReadAvgMatRteFlg% = 1
#                            If ReadAvgMatRteFlg = False Then 'rates not from ERA_Stock table, instead calculate from data
#                                If MatRate(BroodYear, age) = 0 Then MatRate(BroodYear, age) = 0.0000001 'prevents SumMatRte = 0
#                                SumMatRate(age) = SumMatRate(age) + MatRate(BroodYear, age)
#                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_AvgMatRteID, "2625 avgMatRte LTA", BroodYear, age, SumMatRate(age), MatRate(BroodYear, age))
#                            End If
#                        End If
#                    Else
#                        'ACCUMULATE ADULT EQUIVALENTS AND MATURATION RATES FOR COMPLETE BROODS using average number of years specified BroodYear NumAvgYears
#                        If BroodYear = LastCompleteBroodYear Then
#                            For BroodYearNumber = 0 To NumAvgYears - 1
#                                SumAEQ(age) = SumAEQ(age) + AEQ(BroodYear - BroodYearNumber, age)
#                                SumPreTermER(age) = SumPreTermER(age) + PreTermER(BroodYear - BroodYearNumber, age)
#                                NumberOceanCohorts(age) = NumberOceanCohorts(age) + 1
#                                If ReadAvgMatRteFlg = False Then 'rates not from .cm1 file, instead calculate from data
#                                    If MatRate(BroodYear - BroodYearNumber, age) = 0 Then MatRate(BroodYear - BroodYearNumber, age) = 0.0000001 'prevents SumMatRte = 0
#                                    SumMatRate(age) = SumMatRate(age) + MatRate(BroodYear - BroodYearNumber, age)
#                                    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_AvgMatRteID, "2638 avgMatRte", BroodYear - BroodYearNumber, age, SumMatRate(age), MatRate(BroodYear - BroodYearNumber, age))
#                                End If
#                            Next BroodYearNumber
#                            NumberCompleteBroods = NumAvgYears
#                        End If
#                    End If
#                Else 'Product of ADULT EQUIVALENTS,MATURATION RATES, and Ocean HRs FOR COMPLETE BROODS, for use in geometric mean
#                    If LongTermAverage.Checked = True Then 'Product of ADULT EQUIVALENTS AND MATURATION RATES and Ocean HRs FOR COMPLETE BROODS using longterm average
#                        If CompleteBYFlag(BroodYear) = True Then
#                            SumAEQ(age) = SumAEQ(age) * AEQ(BroodYear, age)
#                            If PreTermER(BroodYear, age) = 0 Then PreTermER(BroodYear, age) = 0.0000001
#                            SumPreTermER(age) = SumPreTermER(age) * PreTermER(BroodYear, age)
#                            NumberOceanCohorts(age) = NumberOceanCohorts(age) + 1
#                            If ReadAvgMatRteFlg = 0 Then 'rates not from .cm1 file, instead calculate from data
#                                If MatRate(BroodYear, age) = 0 Then MatRate(BroodYear, age) = 0.0000001 'prevents SumMatRte = 0
#                                SumMatRate(age) = SumMatRate(age) * MatRate(BroodYear, age)
#                            End If
#                        End If
#                    Else 'Product of  ADULT EQUIVALENTS AND MATURATION RATES and Ocean HRs FOR COMPLETE BROODS using average number of years specified by NumAvgYears
#                        If BroodYear = LastCompleteBroodYear Then
#                            For BY = 0 To NumAvgYears - 1
#                                SumAEQ(age) = SumAEQ(age) * AEQ(BroodYear - BY, age)
#                                If PreTermER(BroodYear, age) = 0 Then PreTermER(BroodYear, age) = 0.0000001
#                                SumPreTermER(age) = SumPreTermER(age) * PreTermER(BroodYear - BY, age)
#                                NumberOceanCohorts(age) = NumberOceanCohorts(age) + 1
#                                If ReadAvgMatRteFlg = 0 Then 'rates not from .cm1 file, instead calculate from data
#                                    If MatRate(BroodYear - BY, age) = 0 Then MatRate(BroodYear - BY, age) = 0.0000001 'prevents a TotMatRate of 0
#                                    SumMatRate(age) = SumMatRate(age) * MatRate(BroodYear - BY, age)
#                                End If
#                            Next BY
#                            NumberCompleteBroods = NumAvgYears
#                        End If
#                    End If
#                End If
#            Next age
#            ActDif = 100 * (System.Math.Abs(Cohort(BroodYear, OceanStartAge) - TestCohortNum) / Cohort(BroodYear, OceanStartAge))
#            If ActDif > TESTDIF And RepeatPass = False Then RepeatPass = True
#        Next BroodYear
#        'COMPUTE AVERAGE MATURATION RATES, OCEAN HARVEST RATES, AND ADULT EQUIVALENTS
#        For Age = OceanStartAge To MaxAge
#            If ArithmeticMeanFlag = True Then
#                If NumberOceanCohorts(Age) = 0 Then
#                    'there are never any ocean recoveries of this age
#                    AveragePreTermER(Age) = 0
#                Else
#                    AveragePreTermER(Age) = SumPreTermER(Age) / NumberOceanCohorts(Age)
#                End If
#                If Age <> MaxAge Then
#                    'rates not from ERA_Stock table, instead calculate from data
#                    If ReadAvgMatRteFlg = False Then
#                        AverageMatRate(Age) = SumMatRate(Age) / NumberCompleteBroods
#                    End If
#                    AverageAEQ(Age) = SumAEQ(Age) / NumberCompleteBroods
#                Else
#                    'rate for oldest age not from .cm1 file, instead calculate from data
#                    If ReadAvgMatRteFlg = False Then AverageMatRate(MaxAge) = 1
#                    AverageAEQ(MaxAge) = 1
#                End If
#            Else 'if you want to use the geometric mean
#                If NumberOceanCohorts(Age) = 0 Then
#                    'there are never any ocean recoveries of this age
#                    AveragePreTermER(Age) = 0
#                Else
#                    AveragePreTermER(Age) = SumPreTermER(Age) ^ (1 / NumberOceanCohorts(Age))
#                End If
#                If Age <> MaxAge Then
#                    If ReadAvgMatRteFlg = 0 Then 'rates not from .cm1 file, instead calculate from data
#                        AverageMatRate(Age) = SumMatRate(Age) ^ (1 / NumberCompleteBroods)
#                        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_AvgMatRteID, "2379 avgMatRte", BroodYear, Age, AverageMatRate(Age), "accum", SumMatRate(Age), "numBroods", NumberCompleteBroods)
#                    End If
#                    AverageAEQ(Age) = SumAEQ(Age) ^ (1 / NumberCompleteBroods)
#                Else
#                    If ReadAvgMatRteFlg = 0 Then AverageMatRate(MaxAge) = 1 'rate for oldest age not from .cm1 file, instead calculate from data
#                    AverageAEQ(MaxAge) = 1
#                End If
#            End If 'ArithmeticMeanFlag = True
#        Next Age
#
#  }
#
#}

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




#' @title FindBY
#'
#' @description . 
#'  
#' @param BY integer indicating brood year
#'
#' @details 'give a brood year where Brood(BY%).MissingByFlg = true
#' 'return the next brood year where Brood(BY%).MissingByFlg = false
#' 
#'
#' @return 
#' 
#' @export
#'
#' @examples
#' 
#' 
FindBY <- function(BY, D){
    
    #'give a brood year where Brood(BY%).MissingByFlg = true
    #'return the next brood year where Brood(BY%).MissingByFlg = false
    
    BY <- BY + 1
    while(D$MissingBroodYearFlag$Flag[BY]){
      if(BY > D$LastBY){
        BY <- D$LastBY
        break
      }else{
        BY <- BY + 1
      }
    }
    
    return(BY)

#-------------------------------------
#Original  VB code

#'give a brood year where Brood(BY%).MissingByFlg = true
#        'return the next brood year where Brood(BY%).MissingByFlg = false
#        Do
#            by = by + 1
#            If by > LastBY Then
#                by = LastBY
#                Exit Do
#            End If
#        Loop Until MissingBroodYearFlag(by) = False
#        FindBY = by


}
