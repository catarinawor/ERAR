
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
#' @details reads in catch and escapenet levels and expands them to maximum level of releases. also does a bunch of random jiggery pokery that is hard to understand. 
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
    AlloceanAges <-
    Escape <-matrix(0,nrow=length(AllBY),ncol=length(Allages))
    tag_code <- ""
    tempPSCCatch<-matrix(0,nrow=M$NumberPSCFisheries,ncol=7)

    TotalLandedCatch<-matrix(0,nrow=length(AllBY),ncol=7)
    
	if(!M$isReplicateCohShak){
		NumFish <-  M$NumberPSCFisheries        
	}else{
		NumFish <- M$NumberERAFisheries
	}

    LandedCatch<-array(0,dim=c(NumFish,7,length(AllBY)))
    LegalDropoffMortality <- array(0,dim=c(NumFish,7,length(AllBY)))
    TotalLandedCatch_ByFishery<-matrix(0,nrow=length(AllBY),ncol=NumFish)
    TotalTerminalLandedCatch<-matrix(0,nrow=length(AllBY), ncol=7)

    LastAge<-NULL
    NumberCompleteBroods <-0
    CompleteBYFlag<-NULL
    MissingBroodYearFlag<-NULL

    #variables created by Catarina for monitoring purposes
    databasecatch<-list()

	#'Loop through each PSCfishery (or ERAFishery if emulating CAS-Coshak) and escapement to obtain catch and escapement
 	for(Fish in 1:NumFish){
        #deleted: 
        #Fish=81

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

        #dta <- RODBC::odbcConnectAccess2007(M$datbse)   #specifies the file path
        
        EstimatedNumberdf <- RODBC::sqlQuery( M$chnl , query =ERASQL)

        databasecatch[[Fish]]<- EstimatedNumberdf
   
        BroodYear <- EstimatedNumberdf$BroodYear
        RecoveryAge <- EstimatedNumberdf$Age
        SumAdjustedEstimatedNumber <- EstimatedNumberdf$SumEstimatedNumber

        if(M$isReplicateCohShak){
            #'reproduce CFile numbers
            tag_code <- EstimatedNumberdf[,4]
            #'reproduce CFile numbers
            #not tested
            FishName = M$ERAFisheryName[Fish]
        }else{
            tag_code <- rep("",nrow(EstimatedNumberdf))
            FishName <- M$PSCFisheryName[Fish]
        }
      
       #'limit data to last calendar year selected by user 
        if(nrow(EstimatedNumberdf)>0){
            for(ya in 1:nrow(EstimatedNumberdf)){
                if(BroodYear[ya] + RecoveryAge[ya] <= M$LastCalendarYear){
                    #print(paste(Fish,ya))
                    isValidAge = TRUE
                    
                    #'do not use recovery records where RecoveryAge > MaxAge+2
                    if(RecoveryAge[ya] > D$MaxAge & RecoveryAge[ya] <= D$MaxAge + 2 ){
                        sink("../logs/Log_OlderThanMaxAge_ID.log", append=TRUE)
                        cat(paste(D$CASStockString[[1]], ShakerMethod, BroodYear[ya], RecoveryAge[ya], FishName, "assigned to age ", D$MaxAge,"\n"))
                        sink()
                        RecoveryAge[ya] <- D$MaxAge
                    }

                    if(RecoveryAge[ya] > D$MaxAge + 2 ){
                        isValidAge = FALSE
                        sink("../logs/Log_OlderThanMaxAge_ID.log", append=TRUE)
                        cat(paste(D$CASStockString[[1]], ShakerMethod, BroodYear[ya], RecoveryAge[ya], FishName, "will not be used in the cohort analysis\n"))
                        sink()
                    }

                    if(isValidAge){
                        #'combine ages if requested
                        if(M$isCombineAge2And3[ERAStock] == TRUE & RecoveryAge[ya] == 2){
                            RecoveryAge[ya] <- 3 
                        }else if(M$isCombineAge5And6[ERAStock] == TRUE & RecoveryAge[ya] == 6){
                            RecoveryAge[ya] <- 5
                        }

                        #'one more time, check BroodYear + RecoveryAge because of combine age
                        if(BroodYear[ya] + RecoveryAge[ya] <= M$LastCalendarYear){
                          
                            #'CIS multiplies fine scale fishery and tag code esc by RelRatio BEFORE combining into PSC Fishery
                            #'CAS-Coshak rounds fine scale fishery, then combines into PSC Fishery BEFORE multiplying by RelRatio 
                            if(FishName == "ESCAPEMENT" | grepl("ESC",FishName) ){
                               
                                if(!M$isReplicateCohShak){   
                                    Esc <- SumAdjustedEstimatedNumber[ya] * D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear==BroodYear[ya]]                            
                                }else if(M$isReplicateCohShak){                              
                                    #'CAS round escapement before applying IDL
                                    #'Esc = Convert.ToInt32(SumAdjustedEstimatedNumber + 0.0000001) ' 0.00000001                     
                                    Esc <- SumAdjustedEstimatedNumber[ya] + 0.0001 
                                }
                                
                                if(RecoveryAge[ya] == D$OceanStartAge){                                
                                    Escape[which(AllBY==BroodYear[ya]),which(Allages==RecoveryAge[ya])] <- Escape[which(AllBY==BroodYear[ya]),which(Allages==RecoveryAge[ya])] + Esc / D$InterDamSurvivaldf$JackInterDamSurvivalRate[which(D$InterDamSurvivaldf$InterDamSurvival_CalendarYear==(BroodYear[ya] + RecoveryAge[ya]))]

                                    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear[ya] >= M$traceThisYear){
                                        sink("../logs/debug_EscapeID.log",append=T)
                                        cat(paste0(D$CASStockString,"\n"))
                                        cat(paste("1100 accum fine scale escape", BroodYear[ya], RecoveryAge[ya], tag_code[ya], Escape[which(AllBY==BroodYear[ya])&which(Allages==RecoveryAge[ya])], Esc, SumAdjustedEstimatedNumber[ya], D$InterDamSurvivaldf$JackInterDamSurvivalRate[which(D$InterDamSurvivaldf$InterDamSurvival_CalendarYear==(BroodYear[ya] + RecoveryAge[ya]))])) 
                                        sink()
                                    }
                                
                                }else{
                                    Escape[which(AllBY==BroodYear[ya]),which(Allages==RecoveryAge[ya])] <- Escape[which(AllBY==BroodYear[ya]),which(Allages==RecoveryAge[ya])] + Esc / D$InterDamSurvivaldf$AdultInterDamSurvivalRate[which(D$InterDamSurvivaldf$InterDamSurvival_CalendarYear==(BroodYear[ya] + RecoveryAge[ya]))]
                                    
                                    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear[ya] >= M$traceThisYear) {
                                        sink("../logs/debug_EscapeID.log",append=T)
                                        cat(paste0(D$CASStockString,"\n"))
                                        cat(paste("1103 accum fine scale escape", BroodYear[ya], RecoveryAge[ya], tag_code[ya], Escape[which(AllBY==BroodYear[ya])&which(Allages==RecoveryAge[ya])], Esc, SumAdjustedEstimatedNumber[ya], D$InterDamSurvivaldf$AdultInterDamSurvivalRate[which(D$InterDamSurvivaldf$InterDamSurvival_CalendarYear==(BroodYear[ya] + RecoveryAge[ya]))]))
                                        sink()
                                    }
                                }
                            }else{ #catch

                                #'SQL returns fine scale fishery, this is where fine scale fishery is combined into PSC Fishery
                                #'WriteLine(debugID, CurrentStock, Fish, LastBY, BroodYear, RecoveryAge, BroodYear + RecoveryAge, LastCalendarYear)
                                if(!M$isReplicateCohShak){
                                    LandedCatch[Fish,RecoveryAge[ya],which(AllBY==BroodYear[ya])] <- LandedCatch[Fish,RecoveryAge[ya],which(AllBY==BroodYear[ya])] + SumAdjustedEstimatedNumber[ya] * D$RelRatiodf$RelRatio[which(D$RelRatiodf$BroodYear==BroodYear[ya])]
                                    if(RecoveryAge[ya] < D$OceanStartAge){
                                        sink("../logs/LandedCatch.log",append=T)
                                        cat(paste0("Youngest age = ", RecoveryAge[ya], " is less than OceanStartAge for " , BroodYear[ya] , ".  Did you forget to combine age 2 and 3?  Program is going to crash with an index was outside the bounds of the array error message."))
                                        sink()
                                        stop(" MainSub stopped check  ../LandedCatch.log")
                                    }
                                }else{
                                    #multiply catch by RelRatio AFTER combining fine scale fishery into PSC Fishery
                                    #'SQL query must returning sum by fine scale fishery, not by tag code
                                    #'SQL must accum by fine scale fishery before rounding because if Extrapoplated = YES in CWDBRecovery table, then the fractional numbers will get lost 
                                    #'because SumAdjustedEstimatedNumber = accum fine scale catches, OK to round here
                                    #'add 0.0000001 instead of 0.00000001 so even numbers to round up. The extra decimal place is not an issue for odd numbers.
                                    LandedCatch[Fish,RecoveryAge[ya],which(AllBY==BroodYear[ya])] <- LandedCatch[[Fish,RecoveryAge[ya],which(AllBY==BroodYear[ya])]] + SumAdjustedEstimatedNumber[ya] + 0.0001
                                    if(RecoveryAge[ya] < D$OceanStartAge){
                                        sink("../logs/LandedCatch.log",append=T)
                                        cat(paste0("Youngest age = " & RecoveryAge[ya] & " is less than OceanStartAge for " & BroodYear[ya] & ".  Did you forget to combine age 2 and 3?  Program is going to crash with an index out of bounds error message."))
                                        sink()
                                        stop(" MainSub stopped check  ../LandedCatch.log")
                                    }
                                    #'If RecoveryAge = 4 And Fish < 13 And BroodYear = 1975 Then WriteLine(debugID, "1327", ShakerMethod, Fish, RecoveryAge, BroodYear, LandedCatch(Fish, RecoveryAge, BroodYear), SumAdjustedEstimatedNumber, RelRatio(BroodYear))
                                    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & M$isTraceByCalendarYr & (BroodYear[ya] + RecoveryAge[ya]) >= M$traceThisYear + M$traceThisAge & Fish == M$traceThisFishery){
                                        sink("../logs/debug_CatchID.log",append=T)
                                        cat(paste0( "line 1105 FineScaleCatch", BroodYear[ya], Fish, RecoveryAge[ya], tag_code[ya], SumAdjustedEstimatedNumber[ya], LandedCatch[Fish, RecoveryAge[ya], which(AllBY==BroodYear[ya])]))
                                        sink()
                                    }

                                    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & Fish == 31 & BroodYear[ya] >= M$traceThisYear & RecoveryAge[ya] == M$traceThisAge){
                                        sink("../logs/debug_CatchID.log",append=T)
                                        cat(paste0("line 1106 FineScaleCatch", BroodYear[ya], Fish, RecoveryAge[ya], tag_code[ya], SumAdjustedEstimatedNumber[ya], LandedCatch[Fish,RecoveryAge[ya],which(AllBY==BroodYear[ya])]))
                                        sink()   
                                    }    
                                }#'isReplicateCohShak = False
                    
                                #'legal dropoff mortalities for PSCFisheries, similar code for ERAFisheries is in the next loop below.  
                                if(!M$isReplicateCohShak){
                                    if(D$IMdf$CNRMethod[D$IMdf$CalendarYear==BroodYear[ya] + RecoveryAge[ya] &D$IMdf$PSCFishery==Fish]!=3){
                                        LegalDropoffMortality[Fish,RecoveryAge[ya],which(AllBY==BroodYear[ya])] <- LegalDropoffMortality[Fish,RecoveryAge[ya],AllBY==BroodYear[ya]] +
                                                                                                                 LandedCatch[Fish,RecoveryAge[ya],AllBY==BroodYear[ya]] * 
                                                                                                                 D$IMdf$DropoffRate[(BroodYear[ya] + RecoveryAge[ya])==D$IMdf$CalendarYear & D$IMdf$PSCFishery==Fish]
                                        if(is.na(LegalDropoffMortality[Fish,RecoveryAge[ya],AllBY==BroodYear[ya]])){
                                            LegalDropoffMortality[Fish,RecoveryAge[ya],AllBY==BroodYear[ya]] <- 0.0
                                        }
                                    }
                                    
                                    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear[ya] >= M$traceThisYear & Fish== M$traceThisFishery){
                                        sink("../logs/debug_LegalDropoffID.log",append=T)
                                        cat(paste("1196 L dropoff", Fish, RecoveryAge[ya], tag_code[ya], LegalDropoffMortality[Fish, RecoveryAge[ya], BroodYear[ya]], LandedCatch[Fish, RecoveryAge[ya], ya], D$DropoffRate[which((BroodYear[ya] + RecoveryAge[ya])==D$DropoffRate$CalendarYear), Fish+1]))
                                        sink()
                                    }
                                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And Fish = traceThisFishery Then WriteLine(debug_LegalDropoffID, "1196 L dropoff", Fish, RecoveryAge, tag_code, LegalDropoffMortality(Fish, RecoveryAge, BroodYear), LandedCatch(Fish, RecoveryAge, BroodYear), DropoffRate(BroodYear + RecoveryAge, Fish))
                                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And Fish = traceThisFishery Then WriteLine(debug_ShakerID, "1196 L dropoff", Fish, RecoveryAge, tag_code, LegalDropoffMortality(Fish, RecoveryAge, BroodYear), LandedCatch(Fish, RecoveryAge, BroodYear), DropoffRate(BroodYear + RecoveryAge, Fish))
                      
                                }else{
                                    #'to emulate CAS-Coshak, calculate dropoff after rounding ERAFishery and after combining ERAFishery into PSCFishery in the next loop below
                                }#sReplicateCohShak = False
                            }#'FishName = "ESCAPEMENT" Or FishName = "ESCAPE" 
                        }#'BroodYear + RecoveryAge <= LastCalendarYear
                    }# 'isValidAge = True
                } # end of if statement
            }# 'BroodYear + RecoveryAge <= LastCalendarYear
        }else{
            LegalDropoffMortality[Fish,,] <- 0.0
             LandedCatch[Fish,,] <-0.0
        }
    } #next Fish 

    if(M$isReplicateCohShak){ #'combine ERAFisheries into PSCFishery as with CMB spreadsheet
        for(y in 1:length(AllBY)){
            for(Fish in 1:(M$NumberPSCFisheries-1)){ #'except escapement
            #initialize at the start of each PSCFishery and BroodYear
                for(Age in D$OceanStartAge:D$MaxAge){
                    tempPSCCatch[Fish, Age] <- 0
                }
                D2 <- BuildERAFisheryToPSCFisheryList(Fish,M)
                D$NumberERA2PSCFisheries[Fish]<-D2$NumberERA2PSCFisheries
                D$ERA2PSC[[Fish]]<-D2$ERA2PSC
                #'accum ERAFishery LandedCatch() into TempPSCCatch() 
                for(FisheryID in 1:D$NumberERA2PSCFisheries[Fish]){
                    ERAFish = D$ERA2PSC[[Fish]][FisheryID]
                    for(Age in D$OceanStartAge:D$MaxAge){
                        tempPSCCatch[Fish, Age] <- tempPSCCatch[Fish, Age] + LandedCatch[ERAFish, Age, y] * D$RelRatiodf$RelRatio[AllBY[y]==D$RelRatiodf$BroodYear]
                        if(M$isTraceCalc&M$ShakerMethod==M$traceThisShakerMethod&M$isTraceByBroodYr&BroodYear>=M$traceThisYear&Fish==M$traceThisFishery){
                            sink("../logs/CatchID.log",,append=T)
                            cat(paste("after", tempPSCCatch[Fish, Age], LandedCatch[ERAFish, Age, y], D$RelRatiodf$RelRatio[AllBY[y]==D$RelRatiodf$BroodYear]))
                            sink() 
                        }
                    }
                }
            }

            for(Fish in 1:(M$NumberPSCFisheries-1)){
                for(Age in D$OceanStartAge:D$MaxAge){
                    if(AllBY[y]+Age <= M$LastCalendarYear){
                        LandedCatch[Fish, Age, y] = tempPSCCatch[Fish, Age]   
                        if(M$isTraceCalc&M$isTraceByCalendarYr&(BroodYear[y] + Age)>= M$traceThisYear& Fish==M$traceThisFishery){
                            sink("../logs/debug_CatchID.log", append=T)
                            cat(paste("line 1198 combined rounded RelRatio", Fish, BroodYear[y], Age, LandedCatch[Fish, Age, y], D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear ==AllBY[y]]))
                            sink()   
                        }
                        if(M$isTraceCalc&M$isTraceByBroodYr&BroodYear[y]>=M$traceThisYear&Fish == M$traceThisFishery){
                            sink("../logs/debug_CatchID.log")
                            cat(paste("line 1198 combined rounded RelRatio", Fish, BroodYear[y], Age, LandedCatch[Fish, Age, y], D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear ==AllBY[y]]))
                            sink()   
                        }
                        #'when emulating CAS-Coshak 
                        #'step 1, calculate drop-off here, after rounding and combining ERAFisheries into PSCFisheries
                        #'step 2 use  DropoffRate(BroodYear + 2, PSCFishery) instead of DropoffRate(BroodYear + Age, PSCFishery) if brood year method
                        #'else use DropoffRate(BroodYear + age, PSCFishery) if calendar year method

                        if(M$ShakerMethod == "B"){
                            LegalDropoffMortality[Fish, Age, y] = LegalDropoffMortality[Fish, Age, y] + LandedCatch[Fish, Age, y] * D$IMdf$DropoffRate[D$IMdf$CalendarYear==(AllBY[y] + 2)& D$IMdf$PSCFishery== Fish]
                        }else if(M$ShakerMethod == "C"){
                            LegalDropoffMortality[Fish, Age, y] = LegalDropoffMortality[Fish, Age, y] + LandedCatch[Fish, Age, y] *  D$IMdf$DropoffRate[D$IMdf$CalendarYear==(AllBY[y] + Age)& D$IMdf$PSCFishery== Fish]
                        }
                        
                    }else{

                        if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod &  AllBY[y] >= M$traceThisYear & Fish ==M$traceThisFishery ){
                            sink("../logs/debug_ShakerID.log")
                            cat(paste("1344 L dropoff", Fish, Age, tag_code, LegalDropoffMortality[Fish, Age, y], LandedCatch[Fish, Age, y], D$IMdf$DropoffRate[D$IMdf$CalendarYear==(AllBY[y] + 2)& D$IMdf$PSCFishery== Fish]))
                            sink()   
                        }

                        #'step 3 add sublegal shakers and dropoff (i.e. replicate double count) in sub ShakerMethod1 
                        
                        if(M$isTraceCalc & AllBY[y] >= M$traceThisYear & Fish == M$traceThisFishery){
                            sink("../logs/debug_LegalDropoffID.log")
                            cat(paste("1268 L dropofff before doubleCount", Fish, Age, tag_code, LegalDropoffMortality[Fish, Age, y], LandedCatch[PSCFishery, Age, y], D$IMdf$DropoffRate[D$IMdf$CalendarYear==(AllBY[y] + 2)& D$IMdf$PSCFishery== Fish]))
                            sink()   
                        }
                    }# 'BroodYear + RecoveryAge <= LastCalendarYear
                }
            }

            #'apply RelRatio to Escape
            for(Age in 1:length(D$OceanStartAge:D$MaxAge) ){
                #'limit data to last calendar year selected by user
                if((BroodYear[which(BroodYear==AllBY[y])]+Age) <= M$LastCalendarYear){
                    Escape[y,Age] =Escape[y,Age] *  D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear ==AllBY[y]]
                    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod &AllBY[y] >= M$traceThisYear){
                        sink("../logs/debug_EscapeID.log")
                        cat(paste("1565 rounded esc * relRatio", BroodYear[BroodYear==AllBY[y]], Age, Escape[BroodYear==AllBY[y], Age], D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear ==AllBY[y]]))
                        sink()   
                    }
                }
            }
        }
    }  
          
    for(BYind in 1:length(AllBY)){
        BroodYearCatch <- 0
        BroodYearEscapement <- 0
        for(Age in 1:length(Allages)){
            if(Allages[Age] <= M$LastCalendarYear - AllBY[BYind] ){
                for(Fish in 1:(M$NumberPSCFisheries - 1)){

                    #print(paste(BYind,Age,Fish))

                    #'WriteLine(debugID, "1408", terminal(PSCFishery, Age), PSCFishery, Age)
                    #'total (PreTerm and terminal) landed catch BroodYear age
                    TotalLandedCatch[BYind, Age] = TotalLandedCatch[BYind, Age] + LandedCatch[Fish, Age, BYind]

                    if(M$isTraceCalc & M$isTraceByCalendarYr & (AllBY[BYind] + Allages[Age]) >= M$traceThisYear & Fish == M$traceThisFishery){
                        sink("../logs/debug_CatchID.log")
                        cat(paste("line 1127 TotalLandedcatch", AllBY[BYind], Fish, Allages[Age], TotalLandedCatch[BYind, Age], LandedCatch[Fish, Age, BYind], D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear ==AllBY[BYind]]))
                        sink() 
                    }
                    if(M$isTraceCalc & M$isTraceByBroodYr & AllBY[BYind] >= M$traceThisYear & Fish == M$traceThisFishery){
                        sink("../logs/debug_CatchID.log")
                        cat(paste("line 1127 TotalLandedcatch", AllBY[BYind], Fish, Allages[Age], TotalLandedCatch[BYind, Age], LandedCatch[Fish, Age, BYind], D$RelRatiodf$RelRatio[D$RelRatiodf$BroodYear ==AllBY[BYind]]))
                        sink()                       
                    }
                    #'moved to ShakerMethod1 to compare against Coshak12, uncomment here and remove from ShakerMethod1 when done testing
                    # 'total (PreTerm and terminal) Legal Dropoff mortalities BroodYear age
                    #'TotalLegalDropoffs(BroodYear, age) = TotalLegalDropoffs(BroodYear, age) + LegalDropoffMortality(PSCFishery, age, BroodYear)
                    #'total (PreTerm and terminal) landed catch BroodYear fishery

                    TotalLandedCatch_ByFishery[BYind, Fish] = TotalLandedCatch_ByFishery[BYind, Fish] + LandedCatch[Fish , Age,BYind ]
                    #'total terminal landed catch BroodYear age
                    if(D$terminal[Fish,Age] & M$PSCFisheryGear[Fish]!="STRAY"){
                        TotalTerminalLandedCatch[BYind, Age] = TotalTerminalLandedCatch[BYind, Age] + LandedCatch[Fish, Age, BYind]
                        if(M$isTraceCalc & M$ShakerMethod==M$traceThisShakerMethod & AllBY[BYind]>=M$traceThisYear){
                            sink("../logs/debug_SumMatAgeCatID.log")
                            cat(paste("1394 sumMatCat", AllBY[BYind], Allages[Age], Fish, TotalTerminalLandedCatch[BYind, Age], LandedCatch[Fishery, Age, BYind], PSCFisheryGear[PSCFishery]))
                            sink() 
                        }
                        #'moved to ShakerMethod1 to compare against Coshak12, uncomment here and remove from ShakerMethod1 when done testing
                        #'TotalTerminalLegalDropoffs(BroodYear, age) = TotalTerminalLegalDropoffs(BroodYear, age) + LegalDropoffMortality(PSCFishery, age, BroodYear)
                    }
                    #'If terminal(PSCFishery, Age) = True Then
                    #'    If isTraceCalc = True and shakermethod = traceThisShakerMethod Then WriteLine(debugID, "1289 termcatch", PSCFishery, Age, BroodYear, LandedCatch(PSCFishery, Age, BroodYear))
                    #'Else
                    #'    If isTraceCalc = True and shakermethod = traceThisShakerMethod Then WriteLine(debugID, "1288 catch", PSCFishery, Age, BroodYear, LandedCatch(PSCFishery, Age, BroodYear))
                    #'End If
                }
                #'sum up all catch and escapement for brood year
                BroodYearCatch <- BroodYearCatch+ TotalLandedCatch[BYind, Age]
                BroodYearEscapement <- BroodYearEscapement + Escape[BYind, Age]
            } #'age <= LastCalendarYear - BroodYear
        }
        #'set MissingBroodYearFlag to False if there is escapement for the brood year
        #'If BroodYearEscapement > 0 Then
        if(BroodYearCatch > 0 |  BroodYearEscapement > 0){
            MissingBroodYearFlag[BYind] <- FALSE
            #'Get number of ages completed in a broodyear through the last calendar year
            if(AllBY[BYind] + D$MaxAge <= M$LastCalendarYear){
                #'Set CompleteBYFlag and get the number of complete broods and lastCompleteBroodYear for the stock
                LastAge[BYind] = D$MaxAge
                NumberCompleteBroods = NumberCompleteBroods + 1
                CompleteBYFlag[BYind] = TRUE
                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & AllBY[BYind] >= M$traceThisYear){
                    sink("../logs/debug_EncounterRateID.log")
                    cat(paste("1432 completeBYFlag",  AllBY[BYind], CompleteBYFlag[BYind], "max", D$MaxAge, "BY+max", AllBY[BYind] + D$MaxAge, "lastCalYr", M$LastCalendarYear))
                    sink() 
                }
                LastCompleteBroodYear = AllBY[BYind]
            }else{
                LastAge[BYind] = M$LastCalendarYear - AllBY[BYind] #'last age for incomplete Brood years
            }
        }else{
            MissingBroodYearFlag[BYind] = TRUE #'set MissingBroodYearFlag to True if there is no escapement for the brood year
        }
    }
 

    return(list(LandedCatch=LandedCatch,
        TotalLandedCatch=TotalLandedCatch,
    TotalLandedCatch_ByFishery=TotalLandedCatch_ByFishery,
    NumberCompleteBroods=NumberCompleteBroods,
    CompleteBYFlag=CompleteBYFlag,
    MissingBroodYearFlag=MissingBroodYearFlag))
}
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
   
  

