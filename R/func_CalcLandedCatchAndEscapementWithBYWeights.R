
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor


#source(utils.R)



#' @title CalcLandedCatchAndEscapementWithBYWeights
#'
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#' 
#' @param D A list contining the outputs of GetSizeLimitLengthVulnerable
#'
#' @details Calculates stock,age,fishery,year specific PNV values using ND function and input values.
#'
#' @return  
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
CalcLandedCatchAndEscapementWithBYWeights <- function(D,M){

 	#This function is not being used at the moment. 

    #original VB code
    #=============================================================
   
  
}





#' @title CalcLandedCatchAndEscapement
#'
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#' 
#' @param D A list contining the outputs of GetSizeLimitLengthVulnerable
#'
#' @details Calculates stock,age,fishery,year specific PNV values using ND function and input values.
#'
#' @return  
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
CalcLandedCatchAndEscapement <- function(D,M){


	#=============================================================
    #      'NOTE: All catches and escapements are expanded to the maximum level of release found for all brood years
    #    '      by the variable named RelRatio(BY) = MaxRelease/CWTRelease(BY)
    #    '      This is done so that when the calculations for sub-legal CNR mortalities are done, there is no bias
    #    '      introduced resulting from differential levels of marking between brood years.  This is because
    #    '      CWT catch is compared to Actual Catch to calculate the proportion of the total catch assigned to the
    #    '      CWT stock.  If one brood year is under- or over- represented, it will bias the over-all assignment of
    #    '      sub-legal mortalities.  The catches are set back to the original values after the cohort analysis
    #    '      (see SUB ResetCatches) which is what is printed in the cby.out and cyr.out files

   
    AllBY <- D$FirstBY:D$LastBY

    Allages <- D$youngestAge:D$MaxAge
    Escape <-matrix(0,nrow=length(AllBY),ncol=length(Allages))

    LandedCatch <-array(0,dim=c(length()))
   
    tag_code <- ""
#
#
#    
#
#
    
	if(!M$isReplicateCohShak){

		NumFish <-   M$NumberPSCFisheries
        
	}else{

		 NumFish <- M$NumberERAFisheries

	}

    LandedCatch<-array(NA,dim=c(NumFish,7,length(AllBY)))

	#'Loop through each PSCfishery (or ERAFishery if emulating CAS-Coshak) and escapement to obtain catch and escapement
 	for(Fish in 1:NumFish){
        #deleted: 
        #Fish=50

	 sink("../logs/CalcLandedCatchAndEscapement.log",append=T)
		cat(paste0(D$CASStockString,"\n"))
        cat(paste0("Get Landed Catch and Escapement for fishery " , Fish, " of ", NumFish, " fine scale fisheries\n"))
     sink()
#
   #'Create a list of CAS Fisheries mapped to each PSC Fishery with appropriate start and end date ranges
        if(!M$isReplicateCohShak){

    	  d <- BuildPSCFisheryIdList(M,Fish)
          ERASQL <- paste0( "SELECT wtc.BroodYear,r.Age, ",
        "Sum(IIf(f.CASTerminal, r.AdjustedEstimatedNumber,IIf(IsNull(c.ExpansionFactor), r.AdjustedEstimatedNumber, r.AdjustedEstimatedNumber * c.ExpansionFactor))) AS SumEstimatedNumber " ,
        "FROM ((ERA_CWDBRecovery AS r LEFT JOIN ERA_CWDBCatchSample AS c ON (r.Agency = c.Agency) AND (r.RunYear = c.CatchYear) AND (r.CatchSampleId = c.CatchSampleId)) " ,
        "INNER JOIN ERA_Fishery AS f ON r.Fishery = f.Id) INNER JOIN ERA_WireTagCode AS wtc ON r.TagCode = wtc.TagCode " ,
        "WHERE wtc.CASStock IN ('",  D$CASStockString[[1]] ,"') AND Not wtc.ExcludeTagCodeFromERA = -1 AND r.Fishery in " , d$FisheryIdList ," ", d$DateRangeClause , " AND r.age > 1" ,
        " GROUP BY r.Age,wtc.BroodYear ORDER BY wtc.BroodYear, r.Age")

    

      
       }else{#'same as above except by tag code in addition to brood year and age BY ERAFishery instead of PSCFishery
       #not tested

    	  d <- BuildERAFisheryIdList(M,Fish)

          ERASQL <- paste0("SELECT wtc.BroodYear,r.Age, " ,
        "Sum(IIf(IsNull(c.ExpansionFactor), r.AdjustedEstimatedNumber, r.AdjustedEstimatedNumber * c.ExpansionFactor)) AS SumEstimatedNumber, " ,
       "r.TagCode ",
        "FROM ((ERA_CWDBRecovery AS r LEFT JOIN ERA_CWDBCatchSample AS c ON (r.Agency = c.Agency) AND (r.RunYear = c.CatchYear) AND (r.CatchSampleId = c.CatchSampleId)) ",
        "INNER JOIN ERA_Fishery AS f ON r.Fishery = f.Id) INNER JOIN ERA_WireTagCode AS wtc ON r.TagCode = wtc.TagCode ",
        "WHERE wtc.CASStock IN ('", D$CASStockString[[1]] ,"') AND Not wtc.ExcludeTagCodeFromERA = -1 AND r.Fishery in ", d$FisheryIdList ," ", d$DateRangeClause, " AND r.age > 1 ",
        " GROUP BY r.Age, wtc.BroodYear, r.TagCode ORDER BY wtc.BroodYear, r.Age")

        }

        dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
    
        df3 <- RODBC::sqlQuery( dta , query =ERASQL)
   
        BroodYear <- df3$BroodYear
        RecoveryAge <- df3$Age
        SumAdjustedEstimatedNumber <- df3$SumEstimatedNumber

        if(M$isReplicateCohShak){
            #'reproduce CFile numbers
            tag_code <- df3[,4]
       }else{
            tag_code <- rep("",nrow(df3))
       }



       if(!M$isReplicateCohShak){
            FishName <- M$PSCFisheryName[Fish]
        }else{
            #'reproduce CFile numbers
            #not tested
            FishName = M$ERAFisheryName[Fish]
        }

       #'limit data to last calendar year selected by user 
        for(y in 1:nrow(df3)){

            if(BroodYear[y] + RecoveryAge[y] <= M$LastCalendarYear){

                isValidAge = TRUE
                #'do not use recovery records where RecoveryAge > MaxAge+2
                if(RecoveryAge[y] > D$MaxAge & RecoveryAge[y] <= D$MaxAge + 2 ){
                    sink("../logs/Log_OlderThanMaxAge_ID.log", append=TRUE)
                    cat(paste(D$CASStockString[[1]], ShakerMethod, BroodYear[y], RecoveryAge[y], FishName, "assigned to age ", D$MaxAge,"\n"))
                    sink()
                    RecoveryAge[y] <- D$MaxAge
                }

                if(RecoveryAge[y] > D$MaxAge + 2 ){
                    isValidAge = FALSE
                    sink("../logs/Log_OlderThanMaxAge_ID.log", append=TRUE)
                    cat(paste(D$CASStockString[[1]], ShakerMethod, BroodYear[y], RecoveryAge[y], FishName, "will not be used in the cohort analysis\n"))
                    sink()
                }

                if(isValidAge){
                   #'combine ages if requested
                    if(M$isCombineAge2And3[ERAStock] == TRUE & RecoveryAge[y] == 2){
                       RecoveryAge[y] <- 3 
                    }else if(M$isCombineAge5And6[ERAStock] == TRUE & RecoveryAge[y] == 6){
                        RecoveryAge[y] <- 5
                    }

                    #'one more time, check BroodYear + RecoveryAge because of combine age
                    if(BroodYear[y] + RecoveryAge[y] <= M$LastCalendarYear){
                      
                        #'CIS multiplies fine scale fishery and tag code esc by RelRatio BEFORE combining into PSC Fishery
                        #'CAS-Coshak rounds fine scale fishery, then combines into PSC Fishery BEFORE multiplying by RelRatio 
                        if(d$FishName == "ESCAPEMENT" | grepl("ESC",FishName) ){
                           
                            if(!M$isReplicateCohShak){   
                                Esc <- SumAdjustedEstimatedNumber[y] * D$RelRatio[D$CWTRelease_BroodYear==BroodYear[y]]                            
                            }else if(M$isReplicateCohShak){                              
                                #'CAS round escapement before applying IDL
                                #'Esc = Convert.ToInt32(SumAdjustedEstimatedNumber + 0.0000001) ' 0.00000001 
                                
                                Esc <- SumAdjustedEstimatedNumber[y] + 0.0001 
                            }
                            
                            if(RecoveryAge[y] == D$OceanStartAge){                                
                                Escape[which(AllBY==BroodYear[y]),which(Allages==RecoveryAge[y])] <- Escape[which(AllBY==BroodYear[y]),which(Allages==RecoveryAge[y])] + Esc / D$JackInterDamSurvivalRate[which(D$InterDamSurvival_CalendarYear==(BroodYear[y] + RecoveryAge[y]))]

                                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear[y] >= M$traceThisYear){
                                    sink("../logs/debug_EscapeID.log",append=T)
                                    cat(paste0(D$CASStockString,"\n"))
                                    cat(paste("1100 accum fine scale escape", BroodYear[y], RecoveryAge[y], tag_code[y], Escape[which(AllBY==BroodYear[y])&which(Allages==RecoveryAge[y])], Esc, SumAdjustedEstimatedNumber[y], D$JackInterDamSurvivalRate[which(D$InterDamSurvival_CalendarYear==(BroodYear[y] + RecoveryAge[y]))])) 
                                    sink()
                                }

                            }else{
                                Escape[which(AllBY==BroodYear[y]),which(Allages==RecoveryAge[y])] <- Escape[which(AllBY==BroodYear[y]),which(Allages==RecoveryAge[y])] + Esc / D$AdultInterDamSurvivalRate[which(D$InterDamSurvival_CalendarYear==(BroodYear[y] + RecoveryAge[y]))]
                                
                                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear[y] >= M$traceThisYear) {
                                    sink("../logs/debug_EscapeID.log",append=T)
                                    cat(paste0(D$CASStockString,"\n"))
                                    cat(paste("1103 accum fine scale escape", BroodYear[y], RecoveryAge[y], tag_code[y], Escape[which(AllBY==BroodYear[y])&which(Allages==RecoveryAge[y])], Esc, SumAdjustedEstimatedNumber[y], D$AdultInterDamSurvivalRate[which(D$InterDamSurvival_CalendarYear==(BroodYear[y] + RecoveryAge[y]))]))
                                    sink()
                                }

                            }

                        }else{ #catch

                            #'SQL returns fine scale fishery, this is where fine scale fishery is combined into PSC Fishery
                            #'WriteLine(debugID, CurrentStock, Fish, LastBY, BroodYear, RecoveryAge, BroodYear + RecoveryAge, LastCalendarYear)
                            if(!M$isReplicateCohShak){
                                LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])] <- LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])] + SumAdjustedEstimatedNumber[y] + D$RelRatio[which(D$CWTRelease_BroodYear==BroodYear[y])]
                                if(RecoveryAge[y] < D$OceanStartAge){
                                    sink("../logs/LandedCatch.log",append=T)
                                    cat(paste0("Youngest age = ", RecoveryAge[y], " is less than OceanStartAge for " , BroodYear[y] , ".  Did you forget to combine age 2 and 3?  Program is going to crash with an index was outside the bounds of the array error message.")
                                    sink()
                                    stop(" MainSub stopped check  ../LandedCatch.log")
                                }
                            }else{
                                #multiply catch by RelRatio AFTER combining fine scale fishery into PSC Fishery
                                #'SQL query must returning sum by fine scale fishery, not by tag code
                                #'SQL must accum by fine scale fishery before rounding because if Extrapoplated = YES in CWDBRecovery table, then the fractional numbers will get lost 
                                #'because SumAdjustedEstimatedNumber = accum fine scale catches, OK to round here
                                #'add 0.0000001 instead of 0.00000001 so even numbers to round up. The extra decimal place is not an issue for odd numbers.
                                LandedCatch[[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])]] <- LandedCatch[[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])]] + SumAdjustedEstimatedNumber[y] + 0.0001
                                if(RecoveryAge[y] < D$OceanStartAge){
                                    sink("../logs/LandedCatch.log",append=T)
                                    cat(paste0("Youngest age = " & RecoveryAge[y] & " is less than OceanStartAge for " & BroodYear[y] & ".  Did you forget to combine age 2 and 3?  Program is going to crash with an index out of bounds error message.")
                                    sink()
                                    stop(" MainSub stopped check  ../LandedCatch.log")
                                }
                                #'If RecoveryAge = 4 And Fish < 13 And BroodYear = 1975 Then WriteLine(debugID, "1327", ShakerMethod, Fish, RecoveryAge, BroodYear, LandedCatch(Fish, RecoveryAge, BroodYear), SumAdjustedEstimatedNumber, RelRatio(BroodYear))
                                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & M$isTraceByCalendarYr & (BroodYear[y] + RecoveryAge[y]) >= M$traceThisYear + M$traceThisAge & Fish == M$traceThisFishery){
                                    sink("../logs/debug_CatchID.log",append=T)
                                    cat(paste0( "line 1105 FineScaleCatch", BroodYear[y], Fish, RecoveryAge[y], tag_code[y], SumAdjustedEstimatedNumber[y], LandedCatch[Fish, RecoveryAge[y], which(AllBY==BroodYear[y])]))
                                    sink()
                                }

                                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & Fish == 31 & BroodYear[y] >= M$traceThisYear & RecoveryAge[y] = M$traceThisAge){
                                    sink("../logs/debug_CatchID.log",append=T)
                                    cat(paste0("line 1106 FineScaleCatch", BroodYear[y], Fish, RecoveryAge[y], tag_code[y], SumAdjustedEstimatedNumber[y], LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])]))
                                    sink()   
                                }    
                            }#'isReplicateCohShak = False
                        
                            #'legal dropoff mortalities for PSCFisheries, similar code for ERAFisheries is in the next loop below.  
                            if(!M$isReplicateCohShak){
                                if(D$CNRMethod[which(D$CNRMethod[,1]==BroodYear[y] + RecoveryAge[y]), Fish]!=3){
                                    
                                }
                            }

                        }

                                           
#                             
#                        }else{
#                            #catch
#                            #'SQL returns fine scale fishery, this is where fine scale fishery is combined into PSC Fishery
#                            #'WriteLine(debugID, CurrentStock, Fish, LastBY, BroodYear, RecoveryAge, BroodYear + RecoveryAge, LastCalendarYear)
#                            if(M$isReplicateCohShak==False){
#                                LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])] <- LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])] + SumAdjustedEstimatedNumber[y] * RelRatio[D$CWTRelease_BroodYear==BroodYear[y]]
#                                 
#                                if(RecoveryAge[y] < D$OceanStartAge){
#                                    sink("../logs/CombineAge.log",append=T)
#                                    cat(paste0(D$CASStockString))
#                                    cat(paste0("Youngest age = " , RecoveryAge , " is less than OceanStartAge for ", BroodYear, ".  Did you forget to combine age 2 and 3?  Program is going to crash with an index was outside the bounds of the array error message."))
#                                    sink()
#                                }
#                            }else{
#
#                                #'multiply catch by RelRatio AFTER combining fine scale fishery into PSC Fishery
#                                #    'SQL query must returning sum by fine scale fishery, not by tag code
#                                #    'SQL must accum by fine scale fishery before rounding because if Extrapoplated = YES in CWDBRecovery table, then the fractional numbers will get lost 
#                                #    'because SumAdjustedEstimatedNumber = accum fine scale catches, OK to round here
#                                #    'add 0.0000001 instead of 0.00000001 so even numbers to round up. The extra decimal place is not an issue for odd numbers.
#                                    
#                                #    'LandedCatch(Fish, RecoveryAge, BroodYear) = LandedCatch(Fish, RecoveryAge, BroodYear) + (Convert.ToInt32(SumAdjustedEstimatedNumber + 0.0000001))  '0.00000001
#                                LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])] <- LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])] + (SumAdjustedEstimatedNumber + 0.0001) 
#                                    
#                                if(RecoveryAge[y] < D$OceanStartAge){
#                                    sink("../logs/CombineAge.log",append=T)
#                                    cat(paste0(D$CASStockString))
#                                    cat(paste0("Youngest age = " , RecoveryAge , " is less than OceanStartAge for ", BroodYear, ".  Did you forget to combine age 2 and 3?   Program is going to crash with an index out of bounds error message."))
#                                    sink()
#                                }
#
#                                #'If RecoveryAge = 4 And Fish < 13 And BroodYear = 1975 Then WriteLine(debugID, "1327", ShakerMethod, Fish, RecoveryAge, BroodYear, LandedCatch(Fish, RecoveryAge, BroodYear), SumAdjustedEstimatedNumber, RelRatio(BroodYear))
#                                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod &  M$isTraceByCalendarYr & (BroodYear[y] + RecoveryAge[y]) >= M$traceThisYear + M$traceThisAge & Fish == M$traceThisFishery){
#                                    sink("../logs/debug_CatchID.log",append=T)
#                                    cat(paste0(D$CASStockString))
#                                    cat(paste("line 1105 FineScaleCatch", BroodYear[y], Fish, RecoveryAge[y], tag_code[y], SumAdjustedEstimatedNumber[y], LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])]))
#                                    sink()
#                                   
#                                }
#
#                                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & Fish ==31 & Broodyear[y]>= M$traceThisYear & RecoveryAge == M$traceThisAge){
#                                    sink("../logs/debug_CatchID.log",append=T)
#                                    cat(paste0(D$CASStockString))
#                                    cat(paste( "line 1106 FineScaleCatch", BroodYear[y], Fish, RecoveryAge[y], tag_code[y], SumAdjustedEstimatedNumber[y], LandedCatch[Fish,RecoveryAge[y],which(AllBY==BroodYear[y])]))
#                                    sink()
#                                }
#                                
#                            } #'isReplicateCohShak = False
#                            #'legal dropoff mortalities for PSCFisheries, similar code for ERAFisheries is in the next loop below.  
#                            if(M$isReplicateCohShak == FALSE){
#                                if(D$CNRMethod[D$CNRMethod$CalendarYear==(BroodYear[y] + RecoveryAge[y]), Fish+1] != 3){
#                                    
#                                }
#                            }                          
#                        }
#                    }
#                }




    #original VB code
    #=============================================================
    #      'NOTE: All catches and escapements are expanded to the maximum level of release found for all brood years
    #    '      by the variable named RelRatio(BY) = MaxRelease/CWTRelease(BY)
    #    '      This is done so that when the calculations for sub-legal CNR mortalities are done, there is no bias
    #    '      introduced resulting from differential levels of marking between brood years.  This is because
    #    '      CWT catch is compared to Actual Catch to calculate the proportion of the total catch assigned to the
    #    '      CWT stock.  If one brood year is under- or over- represented, it will bias the over-all assignment of
    #    '      sub-legal mortalities.  The catches are set back to the original values after the cohort analysis
    #    '      (see SUB ResetCatches) which is what is printed in the cby.out and cyr.out files
    #    ReDim CalendarYearLandedCatch(LastCalendarYear, NumberPSCFisheries)
    #    Dim temp(NumberPSCFisheries, 7, LastBY) 'if this is dim As Single, rounding get canceled
    #    Dim ERAFish, FisheryID As Integer
    #    Dim Esc As Single
    #    Dim Fish As Integer
    #    Dim FishName As String
    #    Dim NumFish As Integer
    #    Dim tag_code As String = ""
    #    Dim tempPSCCatch(NumberPSCFisheries, 7) 'if this is dim As Single, rounding get canceled
    #    Dim BroodYearCatch, BroodYearEscapement As Single
    #    Dim isValidAge As Boolean
    #    Dim RecoveryAge As Integer
    #    Dim SumAdjustedEstimatedNumber As Single
    #    Dim BroodYear As Integer

    #    If isReplicateCohShak = False Then 'reproduce CFile numbers
    #        NumFish = NumberPSCFisheries
    #    Else
    #        NumFish = NumberERAFisheries
    #    End If

    #    'Loop through each PSCfishery (or ERAFishery if emulating CAS-Coshak) and escapement to obtain catch and escapement
    #    For Fish = 1 To NumFish
    #        lblStatus.Text = " Get Landed Catch and Escapement for fishery " & Fish & " of " & NumFish & " fine scale fisheries"
    #        lblStatus.Visible = True
    #        Me.Refresh()
    #        'Create a list of CAS Fisheries mapped to each PSC Fishery with appropriate start and end date ranges
    #        If isReplicateCohShak = False Then 'reproduce CFile numbers
    #            Call BuildPSCFisheryIdList(Fish)
    #        Else
    #            Call BuildERAFisheryIdList(Fish)
    #        End If
    #        'Sum up adjusted estimated numbers for a brood year and age by PSCFishery for current ERAStock
    #        If isReplicateCohShak = False Then
    #            ERASQL = "SELECT wtc.BroodYear,r.Age, " &
    #            "Sum(IIf(f.CASTerminal, r.AdjustedEstimatedNumber,IIf(IsNull(c.ExpansionFactor), r.AdjustedEstimatedNumber, r.AdjustedEstimatedNumber * c.ExpansionFactor))) AS SumEstimatedNumber " &
    #            "FROM ((ERA_CWDBRecovery AS r LEFT JOIN ERA_CWDBCatchSample AS c ON (r.Agency = c.Agency) AND (r.RunYear = c.CatchYear) AND (r.CatchSampleId = c.CatchSampleId)) " &
    #            "INNER JOIN ERA_Fishery AS f ON r.Fishery = f.Id) INNER JOIN ERA_WireTagCode AS wtc ON r.TagCode = wtc.TagCode " &
    #            "WHERE wtc.CASStock IN (" & CASStockString & ") AND Not wtc.ExcludeTagCodeFromERA = -1 AND r.Fishery in " & FisheryIdList & DateRangeClause & " AND r.age > 1 " &
    #            " GROUP BY r.Age,wtc.BroodYear ORDER BY wtc.BroodYear, r.Age"
    #        Else 'same as above except by tag code in addition to brood year and age BY ERAFishery instead of PSCFishery
    #            ERASQL = "SELECT wtc.BroodYear,r.Age, " &
    #            "Sum(IIf(IsNull(c.ExpansionFactor), r.AdjustedEstimatedNumber, r.AdjustedEstimatedNumber * c.ExpansionFactor)) AS SumEstimatedNumber, " &
    #           "r.TagCode " &
    #            "FROM ((ERA_CWDBRecovery AS r LEFT JOIN ERA_CWDBCatchSample AS c ON (r.Agency = c.Agency) AND (r.RunYear = c.CatchYear) AND (r.CatchSampleId = c.CatchSampleId)) " &
    #            "INNER JOIN ERA_Fishery AS f ON r.Fishery = f.Id) INNER JOIN ERA_WireTagCode AS wtc ON r.TagCode = wtc.TagCode " &
    #            "WHERE wtc.CASStock IN (" & CASStockString & ") AND Not wtc.ExcludeTagCodeFromERA = -1 AND r.Fishery in " & FisheryIdList & DateRangeClause & " AND r.age > 1 " &
    #            " GROUP BY r.Age, wtc.BroodYear, r.TagCode ORDER BY wtc.BroodYear, r.Age"
    #        End If
    #        ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #        CISDataReader = ERACommand.ExecuteReader()
    #        While CISDataReader.Read()
    #            BroodYear = CISDataReader(0)
    #            RecoveryAge = CISDataReader(1)
    #            SumAdjustedEstimatedNumber = CISDataReader(2)
    #            If isReplicateCohShak = True Then 'reproduce CFile numbers
    #                tag_code = CISDataReader(3)
    #            End If
	#
    #            If isReplicateCohShak = False Then
    #                FishName = PSCFisheryName(Fish)
    #            Else 'reproduce CFile numbers
    #                FishName = ERAFisheryName(Fish)
    #            End If
	#
    #            'limit data to last calendar year selected by user 
    #            If BroodYear + RecoveryAge <= LastCalendarYear Then
    #                isValidAge = True
    #                'do not use recovery records where RecoveryAge > MaxAge+2
    #                If RecoveryAge > MaxAge And RecoveryAge <= MaxAge + 2 Then
    #                    WriteLine(Log_OlderThanMaxAge_ID, ShakerMethod, BroodYear, RecoveryAge, FishName, "assigned to age " & MaxAge)
    #                    RecoveryAge = MaxAge
    #                End If
	#
    #                If RecoveryAge > MaxAge + 2 Then
    #                    isValidAge = False
    #                    WriteLine(Log_OlderThanMaxAge_ID, ShakerMethod, BroodYear, RecoveryAge, FishName, "will not be used in the cohort analysis")
    #                End If
	#
    #                If isValidAge = True Then
    #                    'combine ages if requested
    #                    If isCombineAge2And3(ERAStock) = True And RecoveryAge = 2 Then
    #                        RecoveryAge = 3
    #                    End If
    #                    If isCombineAge5And6(ERAStock) = True And RecoveryAge = 6 Then
    #                        RecoveryAge = 5
    #                    End If
	#
    #                    'one more time, check BroodYear + RecoveryAge because of combine age
    #                    If BroodYear + RecoveryAge <= LastCalendarYear Then
    #                        'CIS multiplies fine scale fishery and tag code esc by RelRatio BEFORE combining into PSC Fishery
    #                        'CAS-Coshak rounds fine scale fishery, then combines into PSC Fishery BEFORE multiplying by RelRatio 
    #                        If FishName = "ESCAPEMENT" Or FishName = "ESCAPE" Then
    #                            If isReplicateCohShak = False Then
    #                                Esc = SumAdjustedEstimatedNumber * RelRatio(BroodYear)
    #                            ElseIf isReplicateCohShak = True Then
    #                                'CAS round escapement before applying IDL
    #                                'Esc = Convert.ToInt32(SumAdjustedEstimatedNumber + 0.0000001) ' 0.00000001
    #                                Esc = SumAdjustedEstimatedNumber + 0.0001
    #                            End If
    #                            If RecoveryAge = OceanStartAge Then
    #                                Escape(BroodYear, RecoveryAge) = Escape(BroodYear, RecoveryAge) + Esc / JackInterDamSurvivalRate(BroodYear + RecoveryAge)
    #                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_EscapeID, "1100 accum fine scale escape", BroodYear, RecoveryAge, tag_code, Escape(BroodYear, RecoveryAge), Esc, SumAdjustedEstimatedNumber, JackInterDamSurvivalRate(BroodYear + RecoveryAge))
    #                            Else
    #                                Escape(BroodYear, RecoveryAge) = Escape(BroodYear, RecoveryAge) + Esc / AdultInterDamSurvivalRate(BroodYear + RecoveryAge)
    #                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_EscapeID, "1103 accum fine scale escape", BroodYear, RecoveryAge, tag_code, Escape(BroodYear, RecoveryAge), Esc, SumAdjustedEstimatedNumber, AdultInterDamSurvivalRate(BroodYear + RecoveryAge))
    #                            End If
    #                        Else 'catch
    #                            'SQL returns fine scale fishery, this is where fine scale fishery is combined into PSC Fishery
    #                            'WriteLine(debugID, CurrentStock, Fish, LastBY, BroodYear, RecoveryAge, BroodYear + RecoveryAge, LastCalendarYear)
    #                            If isReplicateCohShak = False Then
    #                                Try
    #                                    LandedCatch(Fish, RecoveryAge, BroodYear) = LandedCatch(Fish, RecoveryAge, BroodYear) + SumAdjustedEstimatedNumber * RelRatio(BroodYear)
    #                                Catch
    #                                    If (RecoveryAge < OceanStartAge) Then
    #                                        MsgBox("Youngest age = " & RecoveryAge & " is less than OceanStartAge for " & BroodYear & ".  Did you forget to combine age 2 and 3?  Program is going to crash with an index was outside the bounds of the array error message.")
    #                                    End If
    #                                End Try
    #                            Else 'multiply catch by RelRatio AFTER combining fine scale fishery into PSC Fishery
    #                                'SQL query must returning sum by fine scale fishery, not by tag code
    #                                'SQL must accum by fine scale fishery before rounding because if Extrapoplated = YES in CWDBRecovery table, then the fractional numbers will get lost 
    #                                'because SumAdjustedEstimatedNumber = accum fine scale catches, OK to round here
    #                                'add 0.0000001 instead of 0.00000001 so even numbers to round up. The extra decimal place is not an issue for odd numbers.
    #                                Try
    #                                    'LandedCatch(Fish, RecoveryAge, BroodYear) = LandedCatch(Fish, RecoveryAge, BroodYear) + (Convert.ToInt32(SumAdjustedEstimatedNumber + 0.0000001))  '0.00000001
    #                                    LandedCatch(Fish, RecoveryAge, BroodYear) = LandedCatch(Fish, RecoveryAge, BroodYear) + (SumAdjustedEstimatedNumber + 0.0001)
    #                                Catch
    #                                    If (RecoveryAge < OceanStartAge) Then
    #                                        MsgBox("Youngest age = " & RecoveryAge & " is less than OceanStartAge for " & BroodYear & ".  Did you forget to combine age 2 and 3?  Program is going to crash with an index out of bounds error message.")
    #                                    End If
    #                                End Try
    #                                'If RecoveryAge = 4 And Fish < 13 And BroodYear = 1975 Then WriteLine(debugID, "1327", ShakerMethod, Fish, RecoveryAge, BroodYear, LandedCatch(Fish, RecoveryAge, BroodYear), SumAdjustedEstimatedNumber, RelRatio(BroodYear))
	#
    #                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And isTraceByCalendarYr = True And (BroodYear + RecoveryAge) >= traceThisYear + traceThisAge And Fish = traceThisFishery Then WriteLine(debug_CatchID, "line 1105 FineScaleCatch", BroodYear, Fish, RecoveryAge, tag_code, SumAdjustedEstimatedNumber, LandedCatch(Fish, RecoveryAge, BroodYear))
    #                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And Fish = 31 And BroodYear >= traceThisYear And RecoveryAge = traceThisAge Then WriteLine(debug_CatchID, "line 1106 FineScaleCatch", BroodYear, Fish, RecoveryAge, tag_code, SumAdjustedEstimatedNumber, LandedCatch(Fish, RecoveryAge, BroodYear))
    #                            End If 'isReplicateCohShak = False
    #                            'legal dropoff mortalities for PSCFisheries, similar code for ERAFisheries is in the next loop below.  
    #                            If isReplicateCohShak = False Then
    #                                If CNRMethod(BroodYear + RecoveryAge, Fish) <> 3 Then
    #                                    LegalDropoffMortality(Fish, RecoveryAge, BroodYear) = LegalDropoffMortality(Fish, RecoveryAge, BroodYear) + LandedCatch(Fish, RecoveryAge, BroodYear) * DropoffRate(BroodYear + RecoveryAge, Fish)
    #                                    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And Fish = traceThisFishery Then WriteLine(debug_LegalDropoffID, "1196 L dropoff", Fish, RecoveryAge, tag_code, LegalDropoffMortality(Fish, RecoveryAge, BroodYear), LandedCatch(Fish, RecoveryAge, BroodYear), DropoffRate(BroodYear + RecoveryAge, Fish))
    #                                    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And Fish = traceThisFishery Then WriteLine(debug_ShakerID, "1196 L dropoff", Fish, RecoveryAge, tag_code, LegalDropoffMortality(Fish, RecoveryAge, BroodYear), LandedCatch(Fish, RecoveryAge, BroodYear), DropoffRate(BroodYear + RecoveryAge, Fish))
    #                                End If
    #                            Else
    #                                'to emulate CAS-Coshak, calculate dropoff after rounding ERAFishery and after combining ERAFishery into PSCFishery in the next loop below
    #                            End If 'sReplicateCohShak = False
    #                        End If 'BroodYear + RecoveryAge <= LastCalendarYear
    #                    End If 'FishName = "ESCAPEMENT" Or FishName = "ESCAPE" 
    #                End If 'isValidAge = True
    #            End If 'BroodYear + RecoveryAge <= LastCalendarYear
    #        End While 'loop thru tag codes, brood years, and ages
    #        CISDataReader.Close()
    #        ProgressBar1.PerformStep()
    #    Next Fish
	#
    #    If isReplicateCohShak = True Then 'combine ERAFisheries into PSCFishery as with CMB spreadsheet
    #        For BroodYear = FirstBY To LastBY
    #            For PSCFishery = 1 To NumberPSCFisheries - 1 'except escapement
    #                'initialize at the start of each PSCFishery and BroodYear
    #                For Age = OceanStartAge To MaxAge
    #                    tempPSCCatch(PSCFishery, Age) = 0
    #                Next Age
    #                BuildERAFisheryToPSCFisheryList(PSCFishery)
    #                For FisheryID = 1 To NumberERA2PSCFisheries
    #                    'accum ERAFishery LandedCatch() into TempPSCCatch() 
    #                    ERAFish = ERA2PSC(FisheryID)
    #                    For Age = OceanStartAge To MaxAge
    #                        'limit data to last calendar year selected by user
    #                        If BroodYear + Age <= LastCalendarYear Then
    #                            'apply relRatio after combine ERAFisheries into PSCFishery 
    #                            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And isTraceByBroodYr = True And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then Write(debug_CatchID, "line 1259 collapse fisheries RelRatio", PSCFishery, ERAFish, BroodYear, Age, "before", tempPSCCatch(PSCFishery, Age))
    #                            tempPSCCatch(PSCFishery, Age) = tempPSCCatch(PSCFishery, Age) + LandedCatch(ERAFish, Age, BroodYear) * RelRatio(BroodYear)
    #                            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And isTraceByBroodYr = True And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CatchID, "after", tempPSCCatch(PSCFishery, Age), LandedCatch(ERAFish, Age, BroodYear), RelRatio(BroodYear))
    #                        End If 'BroodYear + RecoveryAge <= LastCalendarYear
    #                        'If Age = 4 And PSCFishery = 1 And BroodYear = 1975 Then WriteLine(debugID, "1371", ShakerMethod, ERAFish, PSCFishery, Age, BroodYear, tempPSCCatch(PSCFishery, Age), LandedCatch(ERAFish, Age, BroodYear), RelRatio(BroodYear))
    #                    Next Age
    #                Next FisheryID
    #            Next PSCFishery
	#
	#
    #            '*************************************************************************
    #            '*************************************************************************
    #            '*************************************************************************
    #            '*************************************************************************
    #            If isreplicate2016URBdata = True Then
    #                'for test only -- manually add fish that are missing in the CAS database
    #                'so differences related to with missing records in CAS database go away
    #                'and therefore isolate differences that have nothing to do with missing records in CAS database
    #                If BroodYear = 2009 And CurrentStock = "URB" Then
    #                    tempPSCCatch(67, 4) = tempPSCCatch(67, 4) + 150 * RelRatio(BroodYear)
    #                    tempPSCCatch(4, 5) = tempPSCCatch(4, 5) + 3 * RelRatio(BroodYear)
    #                    tempPSCCatch(12, 5) = tempPSCCatch(12, 5) + 2 * RelRatio(BroodYear)
    #                End If
    #                If BroodYear = 2010 And CurrentStock = "URB" Then
    #                    tempPSCCatch(67, 3) = tempPSCCatch(67, 3) + 277 * RelRatio(BroodYear)
    #                End If
    #                If BroodYear = 2011 And CurrentStock = "URB" Then
    #                    tempPSCCatch(67, 2) = tempPSCCatch(67, 2) + 13 * RelRatio(BroodYear)
    #                End If
    #            End If
    #            'when done testing delete the code above that adds fish missing in the database
    #            '*************************************************************************
    #            '*************************************************************************
    #            '*************************************************************************
    #            '*************************************************************************
	#
    #            For PSCFishery = 1 To NumberPSCFisheries - 1 'except escapement
    #                'when all ERAfisheries are combined into PSCFisheries, then copy TempPSCCatch() to LandedCatch()
    #                For Age = OceanStartAge To MaxAge
    #                    'limit data to last calendar year selected by user
    #                    If BroodYear + Age <= LastCalendarYear Then
    #                        LandedCatch(PSCFishery, Age, BroodYear) = tempPSCCatch(PSCFishery, Age)
    #                        If isTraceCalc = True And isTraceByCalendarYr = True And (BroodYear + Age) >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CatchID, "line 1198 combined rounded RelRatio", PSCFishery, BroodYear, Age, LandedCatch(PSCFishery, Age, BroodYear), RelRatio(BroodYear))
    #                        If isTraceCalc = True And isTraceByBroodYr = True And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CatchID, "line 1198 combined rounded RelRatio", PSCFishery, BroodYear, Age, LandedCatch(PSCFishery, Age, BroodYear), RelRatio(BroodYear))
    #                        'when emulating CAS-Coshak 
    #                        'step 1, calculate drop-off here, after rounding and combining ERAFisheries into PSCFisheries
    #                        'step 2 use  DropoffRate(BroodYear + 2, PSCFishery) instead of DropoffRate(BroodYear + Age, PSCFishery) if brood year method
    #                        'else use DropoffRate(BroodYear + age, PSCFishery) if calendar year method
    #                        If ShakerMethod = "B" Then
    #                            LegalDropoffMortality(PSCFishery, Age, BroodYear) = LegalDropoffMortality(PSCFishery, Age, BroodYear) + LandedCatch(PSCFishery, Age, BroodYear) * DropoffRate(BroodYear + 2, PSCFishery)
    #                        ElseIf ShakerMethod = "C" Then
    #                            LegalDropoffMortality(PSCFishery, Age, BroodYear) = LegalDropoffMortality(PSCFishery, Age, BroodYear) + LandedCatch(PSCFishery, Age, BroodYear) * DropoffRate(BroodYear + Age, PSCFishery)
    #                        End If
    #                    Else
    #                        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "1344 L dropoff", PSCFishery, Age, tag_code, LegalDropoffMortality(PSCFishery, Age, BroodYear), LandedCatch(PSCFishery, Age, BroodYear), DropoffRate(BroodYear + 2, PSCFishery))
    #                        'step 3 add sublegal shakers and dropoff (i.e. replicate double count) in sub ShakerMethod1 
    #                        If isTraceCalc = True And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_LegalDropoffID, "1268 L dropofff before doubleCount", PSCFishery, Age, tag_code, LegalDropoffMortality(PSCFishery, Age, BroodYear), LandedCatch(PSCFishery, Age, BroodYear), DropoffRate(BroodYear + 2, PSCFishery))
    #                    End If 'BroodYear + RecoveryAge <= LastCalendarYear
    #                Next Age
    #            Next PSCFishery
	#
    #            'apply RelRatio to Escape
    #            For Age = OceanStartAge To MaxAge
    #                'limit data to last calendar year selected by user
    #                If BroodYear + Age <= LastCalendarYear Then
    #                    Escape(BroodYear, Age) = Escape(BroodYear, Age) * RelRatio(BroodYear)
    #                    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_EscapeID, "1565 rounded esc * relRatio", BroodYear, Age, Escape(BroodYear, Age), RelRatio(BroodYear))
    #                End If 'BroodYear + RecoveryAge <= LastCalendarYear
    #            Next Age
    #        Next BroodYear
    #    End If 'isReplicateCohShak = True
	#
    #    'Sum up totals
    #    For BroodYear = FirstBY To LastBY
    #        BroodYearCatch = 0
    #        BroodYearEscapement = 0
    #        For Age = OceanStartAge To MaxAge
    #            If Age <= LastCalendarYear - BroodYear Then
    #                For PSCFishery = 1 To NumberPSCFisheries - 1 'except escapement
    #                    'WriteLine(debugID, "1408", terminal(PSCFishery, Age), PSCFishery, Age)
    #                    'total (PreTerm and terminal) landed catch BroodYear age
    #                    TotalLandedCatch(BroodYear, Age) = TotalLandedCatch(BroodYear, Age) + LandedCatch(PSCFishery, Age, BroodYear)
    #                    If isTraceCalc = True And isTraceByCalendarYr = True And (BroodYear + Age) >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CatchID, "line 1127 TotalLandedcatch", BroodYear, PSCFishery, Age, TotalLandedCatch(BroodYear, Age), LandedCatch(PSCFishery, Age, BroodYear), RelRatio(BroodYear))
    #                    If isTraceCalc = True And isTraceByBroodYr = True And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CatchID, "line 1127 TotalLandedcatch", BroodYear, PSCFishery, Age, TotalLandedCatch(BroodYear, Age), LandedCatch(PSCFishery, Age, BroodYear), RelRatio(BroodYear))
    #                    'moved to ShakerMethod1 to compare against Coshak12, uncomment here and remove from ShakerMethod1 when done testing
    #                    'total (PreTerm and terminal) Legal Dropoff mortalities BroodYear age
    #                    'TotalLegalDropoffs(BroodYear, age) = TotalLegalDropoffs(BroodYear, age) + LegalDropoffMortality(PSCFishery, age, BroodYear)
    #                    'total (PreTerm and terminal) landed catch BroodYear fishery
    #                    TotalLandedCatch_ByFishery(BroodYear, PSCFishery) = TotalLandedCatch_ByFishery(BroodYear, PSCFishery) + LandedCatch(PSCFishery, Age, BroodYear)
    #                    'total terminal landed catch BroodYear age
    #                    If terminal(PSCFishery, Age) = True And PSCFisheryGear(PSCFishery) <> "STRAY" Then
    #                        TotalTerminalLandedCatch(BroodYear, Age) = TotalTerminalLandedCatch(BroodYear, Age) + LandedCatch(PSCFishery, Age, BroodYear)
    #                        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_SumMatAgeCatID, "1394 sumMatCat", BroodYear, Age, PSCFishery, TotalTerminalLandedCatch(BroodYear, Age), LandedCatch(PSCFishery, Age, BroodYear), PSCFisheryGear(PSCFishery))
    #                        'moved to ShakerMethod1 to compare against Coshak12, uncomment here and remove from ShakerMethod1 when done testing
    #                        'TotalTerminalLegalDropoffs(BroodYear, age) = TotalTerminalLegalDropoffs(BroodYear, age) + LegalDropoffMortality(PSCFishery, age, BroodYear)
    #                    End If
    #                    'If terminal(PSCFishery, Age) = True Then
    #                    '    If isTraceCalc = True and shakermethod = traceThisShakerMethod Then WriteLine(debugID, "1289 termcatch", PSCFishery, Age, BroodYear, LandedCatch(PSCFishery, Age, BroodYear))
    #                    'Else
    #                    '    If isTraceCalc = True and shakermethod = traceThisShakerMethod Then WriteLine(debugID, "1288 catch", PSCFishery, Age, BroodYear, LandedCatch(PSCFishery, Age, BroodYear))
    #                    'End If
    #                Next PSCFishery
    #                'sum up all catch and escapement for brood year
    #                BroodYearCatch += TotalLandedCatch(BroodYear, Age)
    #                BroodYearEscapement += Escape(BroodYear, Age)
    #            End If 'age <= LastCalendarYear - BroodYear
    #        Next Age
    #        'set MissingBroodYearFlag to False if there is escapement for the brood year
    #        'If BroodYearEscapement > 0 Then
    #        If BroodYearCatch > 0 Or BroodYearEscapement > 0 Then
    #            MissingBroodYearFlag(BroodYear) = False
    #            'Get number of ages completed in a broodyear through the last calendar year
    #            If BroodYear + MaxAge <= LastCalendarYear Then 'complete brood years
    #                'Set CompleteBYFlag and get the number of complete broods and lastCompleteBroodYear for the stock
    #                LastAge(BroodYear) = MaxAge
    #                NumberCompleteBroods = NumberCompleteBroods + 1
    #                CompleteBYFlag(BroodYear) = True
    #                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_EncounterRateID, "1432 completeBYFlag", BroodYear, CompleteBYFlag(BroodYear), "max", MaxAge, "BY+max", BroodYear + MaxAge, "lastCalYr", LastCalendarYear)
    #                LastCompleteBroodYear = BroodYear
    #            Else
    #                LastAge(BroodYear) = LastCalendarYear - BroodYear 'last age for incomplete Brood years
    #            End If
    #        Else
    #            MissingBroodYearFlag(BroodYear) = True 'set MissingBroodYearFlag to True if there is no escapement for the brood year
    #        End If
    #    Next BroodYear
    #End Sub
   
  
}
