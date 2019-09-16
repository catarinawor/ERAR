#ERA function ShakerMethod1() and ShakerMethod4()
#Translated from VB ERA CIS code
#June 2018 -2019
#Author: Catarina Wor



#' @title CalcCNR
#'
#' @description  Calculate Catch non retention after shakermethod.
#' 
#' 
#' @param M A list passed to MainSub
#' @param D A list containing the outputs of GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates 
#'
#' @details 
#'
#' @return D1: A list 
#'  
#' @export
#'
#' @examples
#' 
#' 
#' 
CalcCNR <- function(D, M){

    #'calculates legal and sublegal CNR using data from ERA_IMInputs database table
    allBY <- D$FirstBY:D$LastBY
    allCalYr <- (D$FirstBY + D$OceanStartAge) : M$LastCalendarYear

    LegalCNRMortality <- array(0, dim=c(M$NumberPSCFisheries, D$MaxAge, length(allBY)))
    LegalCNRDropoffs <- array(0, dim=c(M$NumberPSCFisheries, D$MaxAge, length(allBY)))
    SubLegalCNRMortality <- array(0, dim=c(M$NumberPSCFisheries, D$MaxAge, length(allBY)))
    SubLegalCNRDropoffs <- array(0, dim=c(M$NumberPSCFisheries, D$MaxAge, length(allBY)))

    TotalCNRLegal <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)
    TotalCNRLegalDropoffs <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)
    TotalCNRSubLegal <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)
    TotalCNRSubLegalDropoffs <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)
    TotalTerminalCNRLegal <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)
    TotalTerminalCNRLegalDropoffs <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)
    TotalTerminalCNRSubLegal <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)
    TotalTerminalCNRSubLegalDropoffs <- matrix(0, nrow=length(allBY),ncol=D$MaxAge)

    for(BroodYear in seq_along(allBY)){
        #'skip missing brood years
        if(!D$MissingBroodYearFlag[BroodYear]){
            for(PSCFishery in seq_len(M$NumberPSCFisheries-3)){
                for(age in D$OceanStartAge:D$LastAge[BroodYear]){
                    #If isTraceCalc = True Then WriteLine(debug_subLegalCNRID, "9999 follow BY 2013 age 3 CENTRL T", BroodYear, PSCFishery, age, SubLegalCNRDropoffs(PSCFishery, age, BroodYear))
                    CalYr <- allBY[BroodYear] + age
                    CalYrind <- allCalYr[allCalYr==CalYr]
                    #'If CalYr > LastIMYear Then GoTo SkipAge
                    #'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery And age = traceThisAge Then WriteLine(debug_LegalCNRID, "1957", PSCFishery, BroodYear, age, CalYr, "CNRMethod", CNRMethod(CalYr, PSCFishery))
                    AA =  as.character(D$IMdf$CNRMethod[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery])
                    if(AA=="1"|AA=="4"|AA=="5"){
                        switch(AA, 
                            "1"={
                              # 'season length method
                              ratio = D$IMdf$CNRSeasonLength[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] / D$IMdf$SeasonLength[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]
                            },
                            "4"={
                              # 'season effort method, same algorithms as method 1 and 5 but different data
                              ratio = D$IMdf$CNREffort[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] / D$IMdf$RetentionEffort[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]  
                            },
                            "5"={
                              # 'Monitored Catch and monitored release method
                               ratio = D$IMdf$MonitoredLegalReleases[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] / D$IMdf$MonitoredLegalCatch[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]
                            },
                            {
                               stop('CNR method not defined')
                            }
                        )
                        LegalCNRMortality[PSCFishery, age, BroodYear] <- D$LandedCatch[PSCFishery, age, BroodYear] * ratio * D$IMdf$LegalSelectivityFactor[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] * D$IMdf$LegalIMRate[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]
                        LegalCNRDropoffs[PSCFishery, age, BroodYear] <- D$LandedCatch[PSCFishery, age, BroodYear] * ratio * D$IMdf$LegalSelectivityFactor[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] * D$IMdf$DropoffRate[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]
                        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_LegalCNRID, "1966", "ShakerMethod1", BroodYear, PSCFishery, age, LegalCNRMortality(PSCFishery, age, BroodYear), LandedCatch(PSCFishery, age, BroodYear), ratio, LegalSelectivityFactor(CalYr, PSCFishery), LegalIMRate(CalYr, PSCFishery), CalYr)
                        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_LegalCNRID, "1966", "ShakerMethod1", BroodYear, PSCFishery, age, LegalCNRDropoffs(PSCFishery, age, BroodYear), LandedCatch(PSCFishery, age, BroodYear), ratio, LegalSelectivityFactor(CalYr, PSCFishery), DropoffRate(CalYr, PSCFishery), CalYr)
                        #'**********
                        #'old code included legal dropoffs and sublegal dropoffs in shakers so I added them here, they should be removed in final code
                        SubLegalCNRMortality[PSCFishery, age, BroodYear] <- (D$SublegalShakerMortalities[PSCFishery, age, BroodYear] + D$SublegalDropoffMortalities[PSCFishery, age, BroodYear] + D$LegalDropoffMortality[PSCFishery, age, BroodYear]) * ratio * D$IMdf$SublegalSelectivityFactor[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]
                        #If isTraceCalc = True And BroodYear >= traceThisYear Then WriteLine(debug_subLegalCNRID, "1830 CaclCNR", PSCFishery, BroodYear, age, SubLegalCNRMortality(PSCFishery, age, BroodYear), SublegalShakerMortalities(PSCFishery, age, BroodYear), SublegalDropoffMortalities(PSCFishery, age, BroodYear), LegalDropoffMortality(PSCFishery, age, BroodYear), ratio, SublegalSelectivityFactor(CalYr, PSCFishery))
           
                    }else{

                        switch(AA, 
                            "2"={
                                LegalCNRMortality[PSCFishery, age, BroodYear] <- (D$LandedCatch[PSCFishery, age, BroodYear] * D$IMdf$CNRLegalEncounterEst[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] / D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]) * D$IMdf$LegalIMRate[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]
                                LegalCNRDropoffs[PSCFishery, age, BroodYear] <- (D$LandedCatch[PSCFishery, age, BroodYear] * D$IMdf$CNRLegalEncounterEst[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] / D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]) * D$IMdf$DropoffRate[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]
                                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And age = traceThisAge Then WriteLine(debug_LegalCNRID, "1836 legalCNR", "Shaker", "Method2", BroodYear, PSCFishery, age, LegalCNRMortality(PSCFishery, age, BroodYear), LegalCNRDropoffs(PSCFishery, age, BroodYear), LandedCatch(PSCFishery, age, BroodYear), CNRLegalEncounterEst(CalYr, PSCFishery), LandedCatchEst(CalYr, PSCFishery), LegalIMRate(CalYr, PSCFishery), DropoffRate(CalYr, PSCFishery), CalYr)
                                if(D$CalendarYearShakers[CalYr, PSCFishery] != 0){
                                    CNRSublegalEncounters <- (D$IMdf$CNRSubLegalEncounterEst[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] *
                                                             D$CalendarYearLandedCatch[CalYrind, PSCFishery] / 
                                                             D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]) *
                                                            ((D$SublegalShakerMortalities[PSCFishery, age, BroodYear] + 
                                                                D$SublegalDropoffMortalities[PSCFishery, age, BroodYear] + 
                                                                D$LegalDropoffMortality[PSCFishery, age, BroodYear]) /
                                                                D$CalendarYearShakers[CalYrind, PSCFishery])
                                    
                                    SubLegalCNRMortality[PSCFishery, age, BroodYear] <- CNRSublegalEncounters * 
                                                                                        (D$IMdf$SublegalIMRate[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery] + 
                                                                                         D$IMdf$DropoffRate[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery])
                               }
                            },
                            "3"={
                                #'catchability coefficient method used when there was no retention fishery
                                d2 <- CNRNoDir(CalYr, BroodYear, PSCFishery, age)
                                SubLegalCNRMortality[PSCFishery, age, BroodYear] <- d2$SubLegalCNRMortality
                                LegalCNRMortality[PSCFishery, age, BroodYear] <- d2$LegalCNRMortality
                                
                            },
                            "9"={ #'no cnr fishery or data
                               #'There is no CNR for this year and fishery, go to next year
                            },
                            {
                               stop('CNR method not defined')
                            }
                        )

                    }
                }
            }
        }
    }
    #'MOVED THIS SECTION OF CODE FROM TOP OF ROUTINE SO THAT
    #' CNR MORTALITIES IN LAST PASS WOULD BE AVAILABLE FOR METHOD 3
    #' IN NEXT PASS 9/24/97
    for(BroodYear in seq_along(allBY)){
        for(age in D$OceanStartAge:D$LastAge[BroodYear]){
            D$TotalCNRLegal[BroodYear, age] <- 0
            D$TotalCNRLegalDropoffs[BroodYear, age] <- 0
            D$TotalCNRSubLegal[BroodYear, age] <- 0
            D$TotalCNRSubLegalDropoffs[BroodYear, age] <- 0
            D$TotalTerminalCNRLegal[BroodYear, age] <- 0
            D$TotalTerminalCNRLegalDropoffs[BroodYear, age] <- 0
            D$TotalTerminalCNRSubLegal[BroodYear, age] <- 0
            D$TotalTerminalCNRSubLegalDropoffs[BroodYear, age] <- 0
        }
    }
    #'MOVED THIS SECTION OF CODE FROM LOOP ABOVE FOR REASON NOTED ABOVE
    for(BroodYear in seq_along(allBY)){
        #'skip missing brood years
        if(!D$MissingBroodYearFlag$Flag[D$MissingBroodYearFlag$BY==allBY[BroodYear]]){
            CalYr <- BroodYear + age
            TotalCNRLegal[BroodYear, ] <- apply(LegalCNRMortality[, , BroodYear],2,sum)
            TotalCNRLegalDropoffs[BroodYear, ] <- apply(LegalCNRDropoffs[, , BroodYear],2,sum)
            TotalCNRSubLegal[BroodYear, ] <- apply(SubLegalCNRMortality[, , BroodYear],2,sum)
            TotalCNRSubLegalDropoffs[BroodYear, ] <- apply(SubLegalCNRDropoffs[, , BroodYear],2,sum)
            
            TotalTerminalCNRLegal[BroodYear,] <- apply(LegalCNRMortality[D$terminal[, age], , BroodYear],2,sum)
            TotalTerminalCNRLegalDropoffs[BroodYear,] <- apply(LegalCNRDropoffs[D$terminal[, age], , BroodYear],2,sum)
            TotalTerminalCNRSubLegal[BroodYear,] <- apply(SubLegalCNRMortality[D$terminal[, age], , BroodYear],2,sum)
            TotalTerminalCNRSubLegalDropoffs[BroodYear,] <- apply(SubLegalCNRDropoffs[D$terminal[, age], , BroodYear],2,sum)
        }
    }


    return(list(TotalCNRLegal=TotalCNRLegal,
        TotalCNRLegalDropoffs=TotalCNRLegalDropoffs,
        TotalCNRSubLegal=TotalCNRSubLegal,
        TotalCNRSubLegalDropoffs=TotalCNRSubLegalDropoffs, 
        TotalTerminalCNRLegal=TotalTerminalCNRLegal,
        TotalTerminalCNRLegalDropoffs=TotalTerminalCNRLegalDropoffs,
        TotalTerminalCNRSubLegal=TotalTerminalCNRSubLegal,
        TotalTerminalCNRSubLegalDropoffs=TotalTerminalCNRSubLegalDropoffs,
        LegalCNRMortality = LegalCNRMortality, 
        LegalCNRDropoffs = LegalCNRDropoffs, 
        SubLegalCNRMortality = SubLegalCNRMortality,
        LegalCNRDropoffs = LegalCNRDropoffs))



#Original VB code
#'calculates legal and sublegal CNR using data from ERA_IMInputs database table
#        Dim CNRSublegalEncounters As Single
#        Dim CalYr As Integer
#        Dim ratio As Single
#        For BroodYear As Integer = FirstBY To LastBY
#            'skip missing brood years
#            If MissingBroodYearFlag(BroodYear) = False Then
#                For PSCFishery As Integer = 1 To NumberPSCFisheries
#                    For age As Integer = OceanStartAge To LastAge(BroodYear)
#                        If isTraceCalc = True Then WriteLine(debug_subLegalCNRID, "9999 follow BY 2013 age 3 CENTRL T", BroodYear, PSCFishery, age, SubLegalCNRDropoffs(PSCFishery, age, BroodYear))
#                        CalYr = BroodYear + age
#                        'If CalYr > LastIMYear Then GoTo SkipAge
#                        'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery And age = traceThisAge Then WriteLine(debug_LegalCNRID, "1957", PSCFishery, BroodYear, age, CalYr, "CNRMethod", CNRMethod(CalYr, PSCFishery))
#                        Select Case CNRMethod(CalYr, PSCFishery)
#                            Case 1, 4, 5 'season length, season effort, and monitored release methods, same algorithms, different data in ratio
#                                Select Case CNRMethod(CalYr, PSCFishery)
#                                    Case 1 'season length method
#                                        ratio = CNRSeasonLength(CalYr, PSCFishery) / SeasonLength(CalYr, PSCFishery)
#                                    Case 4 'season effort method, same algorithms as method 1 and 5 but different data
#                                        ratio = CNREffort(CalYr, PSCFishery) / RetentionEffort(CalYr, PSCFishery)
#                                    Case 5 'Monitored Catch and monitored release method
#                                        ratio = MonitoredLegalReleases(CalYr, PSCFishery) / MonitoredLegalCatch(CalYr, PSCFishery)
#                                End Select
#                                LegalCNRMortality(PSCFishery, age, BroodYear) = LandedCatch(PSCFishery, age, BroodYear) * ratio * LegalSelectivityFactor(CalYr, PSCFishery) * LegalIMRate(CalYr, PSCFishery)
#                                LegalCNRDropoffs(PSCFishery, age, BroodYear) = LandedCatch(PSCFishery, age, BroodYear) * ratio * LegalSelectivityFactor(CalYr, PSCFishery) * DropoffRate(CalYr, PSCFishery)
#                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_LegalCNRID, "1966", "ShakerMethod1", BroodYear, PSCFishery, age, LegalCNRMortality(PSCFishery, age, BroodYear), LandedCatch(PSCFishery, age, BroodYear), ratio, LegalSelectivityFactor(CalYr, PSCFishery), LegalIMRate(CalYr, PSCFishery), CalYr)
#                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_LegalCNRID, "1966", "ShakerMethod1", BroodYear, PSCFishery, age, LegalCNRDropoffs(PSCFishery, age, BroodYear), LandedCatch(PSCFishery, age, BroodYear), ratio, LegalSelectivityFactor(CalYr, PSCFishery), DropoffRate(CalYr, PSCFishery), CalYr)
#                                '**********
#                                'old code included legal dropoffs and sublegal dropoffs in shakers so I added them here, they should be removed in final code
#                                SubLegalCNRMortality(PSCFishery, age, BroodYear) = (SublegalShakerMortalities(PSCFishery, age, BroodYear) + SublegalDropoffMortalities(PSCFishery, age, BroodYear) + LegalDropoffMortality(PSCFishery, age, BroodYear)) * ratio * SublegalSelectivityFactor(CalYr, PSCFishery)
#                                If isTraceCalc = True And BroodYear >= traceThisYear Then WriteLine(debug_subLegalCNRID, "1830 CaclCNR", PSCFishery, BroodYear, age, SubLegalCNRMortality(PSCFishery, age, BroodYear), SublegalShakerMortalities(PSCFishery, age, BroodYear), SublegalDropoffMortalities(PSCFishery, age, BroodYear), LegalDropoffMortality(PSCFishery, age, BroodYear), ratio, SublegalSelectivityFactor(CalYr, PSCFishery))
#                            Case 2 'reported encounters method
#                                LegalCNRMortality(PSCFishery, age, BroodYear) = (LandedCatch(PSCFishery, age, BroodYear) * CNRLegalEncounterEst(CalYr, PSCFishery) / LandedCatchEst(CalYr, PSCFishery)) * LegalIMRate(CalYr, PSCFishery)
#                                LegalCNRDropoffs(PSCFishery, age, BroodYear) = (LandedCatch(PSCFishery, age, BroodYear) * CNRLegalEncounterEst(CalYr, PSCFishery) / LandedCatchEst(CalYr, PSCFishery)) * DropoffRate(CalYr, PSCFishery)
#                                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And age = traceThisAge Then WriteLine(debug_LegalCNRID, "1836 legalCNR", "Shaker", "Method2", BroodYear, PSCFishery, age, LegalCNRMortality(PSCFishery, age, BroodYear), LegalCNRDropoffs(PSCFishery, age, BroodYear), LandedCatch(PSCFishery, age, BroodYear), CNRLegalEncounterEst(CalYr, PSCFishery), LandedCatchEst(CalYr, PSCFishery), LegalIMRate(CalYr, PSCFishery), DropoffRate(CalYr, PSCFishery), CalYr)
#                                If CalendarYearShakers(CalYr, PSCFishery) <> 0 Then
#                                    CNRSublegalEncounters = (CNRSubLegalEncounterEst(CalYr, PSCFishery) * CalendarYearLandedCatch(CalYr, PSCFishery) / LandedCatchEst(CalYr, PSCFishery)) * ((SublegalShakerMortalities(PSCFishery, age, BroodYear) + SublegalDropoffMortalities(PSCFishery, age, BroodYear) + LegalDropoffMortality(PSCFishery, age, BroodYear)) / CalendarYearShakers(CalYr, PSCFishery))
#                                    SubLegalCNRMortality(PSCFishery, age, BroodYear) = CNRSublegalEncounters * (SublegalIMRate(CalYr, PSCFishery) + DropoffRate(CalYr, PSCFishery))
#                                    If isTraceCalc = True Then
#                                        If BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then Write(debug_subLegalCNRID, "1837 sublegalCNR", PSCFishery, age, "rpt enc", CNRSubLegalEncounterEst(CalYr, PSCFishery), "rpt cat", LandedCatchEst(CalYr, PSCFishery), "SL IMrate", SublegalIMRate(CalYr, PSCFishery), "dropoffRate", DropoffRate(CalYr, PSCFishery), "relRatio", RelRatio(BroodYear))
#                                        If BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then Write(debug_subLegalCNRID, "CNR sublegal", "method2", SubLegalCNRMortality(PSCFishery, age, BroodYear), "enc", CNRSubLegalEncounterEst(CalYr, PSCFishery), "*calYrCatch", CalendarYearLandedCatch(CalYr, PSCFishery), "/rpt catch*(SL shaker", SublegalShakerMortalities(PSCFishery, age, BroodYear), "+ SL dropoff", SublegalDropoffMortalities(PSCFishery, age, BroodYear), "+ L dropoff)", LegalDropoffMortality(PSCFishery, age, BroodYear), "/calYrShakers", CalendarYearShakers(CalYr, PSCFishery))
#                                        If BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_subLegalCNRID, "*(SL IMRate + dropoff rate)", CalYr)
#                                    End If
#                                End If
#                            Case 3 'catchability coefficient method used when there was no retention fishery
#                                Call CNRNoDir(CalYr, BroodYear, PSCFishery, age)
#                            Case 9 'no cnr fishery or data
#                                'There is no CNR for this year and fishery, go to next year
#                        End Select
#                        'SkipAge:
#                        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_LegalCNRID, "2042", "CNRMethod", CNRMethod(CalYr, PSCFishery), BroodYear, PSCFishery, age, "CNRlegal", LegalCNRMortality(PSCFishery, age, BroodYear), "shakers", LegalCNRDropoffs(PSCFishery, age, BroodYear), "relratio", RelRatio(BroodYear))
#                    Next age
#                Next PSCFishery
#            End If
#        Next BroodYear
#        'MOVED THIS SECTION OF CODE FROM TOP OF ROUTINE SO THAT
#        ' CNR MORTALITIES IN LAST PASS WOULD BE AVAILABLE FOR METHOD 3
#        ' IN NEXT PASS 9/24/97
#        For BroodYear As Integer = FirstBY To LastBY
#            For Age = OceanStartAge To LastAge(BroodYear)
#                TotalCNRLegal(BroodYear, Age) = 0
#                TotalCNRLegalDropoffs(BroodYear, Age) = 0
#                TotalCNRSubLegal(BroodYear, Age) = 0
#                TotalCNRSubLegalDropoffs(BroodYear, Age) = 0
#                TotalTerminalCNRLegal(BroodYear, Age) = 0
#                TotalTerminalCNRLegalDropoffs(BroodYear, Age) = 0
#                TotalTerminalCNRSubLegal(BroodYear, Age) = 0
#                TotalTerminalCNRSubLegalDropoffs(BroodYear, Age) = 0
#            Next Age
#        Next BroodYear
#        'MOVED THIS SECTION OF CODE FROM LOOP ABOVE FOR REASON NOTED ABOVE
#        For BroodYear As Integer = FirstBY To LastBY
#            'skip missing brood years
#            If MissingBroodYearFlag(BroodYear) = False Then
#                For PSCFishery = 1 To NumberPSCFisheries
#                    For age As Integer = OceanStartAge To LastAge(BroodYear)
#                        CalYr = BroodYear + age
#                        TotalCNRLegal(BroodYear, age) += LegalCNRMortality(PSCFishery, age, BroodYear)
#                        TotalCNRLegalDropoffs(BroodYear, age) += LegalCNRDropoffs(PSCFishery, age, BroodYear)
#                        TotalCNRSubLegal(BroodYear, age) += SubLegalCNRMortality(PSCFishery, age, BroodYear)
#                        TotalCNRSubLegalDropoffs(BroodYear, age) += SubLegalCNRDropoffs(PSCFishery, age, BroodYear)
#                        If terminal(PSCFishery, age) = True And PSCFisheryGear(PSCFishery) <> "STRAY" Then
#                            TotalTerminalCNRLegal(BroodYear, age) += LegalCNRMortality(PSCFishery, age, BroodYear)
#                            TotalTerminalCNRLegalDropoffs(BroodYear, age) += LegalCNRDropoffs(PSCFishery, age, BroodYear)
#                            TotalTerminalCNRSubLegal(BroodYear, age) += SubLegalCNRMortality(PSCFishery, age, BroodYear)
#                            TotalTerminalCNRSubLegalDropoffs(BroodYear, age) += SubLegalCNRDropoffs(PSCFishery, age, BroodYear)
#                        End If
#                        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then
#                            WriteLine(debug_LegalDropoffID, "2368 CalcCNR", PSCFishery, age, BroodYear, LegalCNRMortality(PSCFishery, age, BroodYear), LegalCNRDropoffs(PSCFishery, age, BroodYear), SubLegalCNRMortality(PSCFishery, age, BroodYear), SubLegalCNRDropoffs(PSCFishery, age, BroodYear))
#                            WriteLine(debug_terminalCatchID, "2368 CalcCNR fishery specific", PSCFishery, age, BroodYear, LegalCNRMortality(PSCFishery, age, BroodYear), LegalCNRDropoffs(PSCFishery, age, BroodYear), SubLegalCNRMortality(PSCFishery, age, BroodYear), SubLegalCNRDropoffs(PSCFishery, age, BroodYear))
#                            WriteLine(debug_terminalCatchID, "2368 CalcCNR total", PSCFishery, age, BroodYear, TotalTerminalCNRLegal(BroodYear, age), TotalTerminalCNRLegalDropoffs(BroodYear, age), TotalTerminalCNRSubLegal(BroodYear, age), TotalTerminalCNRSubLegalDropoffs(BroodYear, age))
#                        End If
#                    Next age
#                Next PSCFishery
#            End If
#        Next BroodYear
}







#' @title CNRNoDir
#'
#' @description  Calculate Catch non retention after shakermethod when there was no retention fishery.
#' 
#' 
#' @param M A list passed to MainSub
#' @param D A list containing the outputs of GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates 
#'
#' @details 
#'
#' @return D1: A list 
#'  
#' @export
#'
#' @examples
#' 
#' 
#' 
CNRNoDir <- function(YR, BroodYear, PSCFishery,age){

    #'Calculation of legal-sized CNR mortalities
    #    'when estimated encounter of legal-sized During CNR is unknown
    #    'use boat days to estimate
    #    'NOTE: All catches and escapements are expanded to the maximum level of release found for all brood years
    #    '      This is done so that when the calculations for sub-legal CNR mortalities are done, there is no bias
    #    '      introduced resulting from differential levels of marking between brood years.  This is because
    #    '      CWT catch is compared to Actual Catch to calculate the proportion of the total catch assigned to the
    #    '      CWT stock.  If one brood year is under- or over- represented, it will bias the over-all assignment of
    #    '      sub-legal mortalities.  The catches are set back to the original values after the cohort analysis
    #    '      (see SUB ResetCatches)

    #    'local variables
    IMortRt <-matrx(NA, nrow=2, ncol=2)
    sublegal <- 1
    legal <- 2
    Instantaneous <- 1
    Annual <- 2


    #'DM = DropoffRate(YR, PSCFishery) dropoff rate is not used, SHould it be????
    #'Note: removed division by Drop-off rate, Oct. 29 1997; included in external Q
    #'      For this algorithm to work correctly, the q values must be computed
    #'      with dropoff mortality included.
    for(sizeGroup in  sublegal:legal){
        #'get annual landing rate for hypothetical retention fishery:  catchability * selectivity * boat-days
        if(sizeGroup == sublegal){
            SubLegalCatchabilityCoefficient<- D$catchabilitydf$AvgQ[D$catchabilitydf$SizeClass == "S" & D$catchabilitydf$PSCFishery == PSCFishery&D$catchabilitydf$Age==age]
            SublegalSelectivityFactor <- D$IMdf$SublegalSelectivityFactor[D$IMdf$CalendarYear == YR & D$IMdf$PSCFishery == PSCFishery]
            CNREffort <- D$IMdf$CNREffort[D$IMdf$CalendarYear == YR & D$IMdf$PSCFishery == PSCFishery]

            AnnualRetentionEncounterRate <- 1 - exp(-SubLegalCatchabilityCoefficient * SublegalSelectivityFactor  * CNREffort )
        }else if(sizeGroup == sublegal){

            LegalCatchabilityCoefficient <- D$catchabilitydf$AvgQ[D$catchabilitydf$SizeClass == "L" & D$catchabilitydf$PSCFishery == PSCFishery&D$catchabilitydf$Age==age]
            LegalSelectivityFactor <- D$IMdf$LegalSelectivityFactor[D$IMdf$CalendarYear == YR & D$IMdf$PSCFishery == PSCFishery]
            CNREffort <- D$IMdf$CNREffort[D$IMdf$CalendarYear == YR & D$IMdf$PSCFishery == PSCFishery]

            AnnualRetentionEncounterRate <- 1 - exp(- LegalCatchabilityCoefficient * LegalSelectivityFactor * CNREffort)
        }
        #'convert to Instantaneous rate
        InstantaneousRetentionEncounterRate <- -log(1 - AnnualRetentionEncounterRate)

        #'difference between hypothetical retention and CNR rates is
        #'replacement of boat-days with boat-reavailability day
        #'i.e. boat-days /(CNR season in days/availability in days)
        CNRSeasonLength <- D$IMdf$CNRSeasonLength[D$IMdf$CalendarYear == YR & D$IMdf$PSCFishery == PSCFishery]
        Reavailability <- D$IMdf$Reavailability[D$IMdf$CalendarYear == YR & D$IMdf$PSCFishery == PSCFishery]
        tp = CNRSeasonLength / Reavailability

        #'convert hypothetical retention rate to CNR rate
        InstantaneousCNREncounterRate <- InstantaneousRetentionEncounterRate / tp
        #'convert Instantaneous rate back to annual rate
        AnnualCNREncounterRate <- 1 - exp(-InstantaneousCNREncounterRate)

        if(sizeGroup == sublegal){
            IMortRt[Annual, 1] <- AnnualCNREncounterRate
        }else if(sizeGroup == legal){
            LegalIMRate <- D$IMdf$LegalIMRate[D$IMdf$CalendarYear == YR & D$IMdf$PSCFishery == PSCFishery]
            IMortRt[Annual, 1] = AnnualCNREncounterRate * LegalIMRate
        }

        IMortRt[Instantaneous, 1] <- -log(1 - IMortRt[Annual, 1]) #'this is already computed above
        IMortRt[Annual, 2] = 1 - exp(-IMortRt[Instantaneous, 1] * tp) #'this is already computed above

        if(D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY==BroodYear]){
            if(!D$terminal[PSCFishery,age]){
                cohrt <- D$Cohort[BroodYear, age] * D$survivaldf$SurvivalRate[D$survivaldf$Age==age] 
            }else{
                cohrt <- TotalTerminalCatch_Age(D, M, BY, age) + D$Escape[BroodYear, age]
            }
        }else{
            if(M$ShakerMethod == "C"| age <= D$LastAge[BroodYear] ){
                cohrt <- CalcEstmCohrt2(D, M, BroodYear, age)
            }else{
                cohrt <- CalcEstmCohrt(D, M, BroodYear, age)
            }
            if(D$terminal[PSCFishery, age]){
               cohrt <- cohrt * (1 - D$AveragePreTermER[age]) * D$AverageMatRate[age] 
            }
                    
        }
        if(sizeGroup == sublegal){ 
            SubLegalCNRMortality <- IMortRt[Annual, 2] * cohrt
        }else if(sizeGroup == legal){
            LegalCNRMortality <- IMortRt[Annual, 2] * cohrt
        }
    }


    return(list(SubLegalCNRMortality=SubLegalCNRMortality,
                LegalCNRMortality=LegalCNRMortality))


    # -----------------------------------------------------------------------------------------
    #Original VB code

#    Sub CNRNoDir(ByRef YR As Integer, ByRef BroodYear As Integer, ByRef PSCFishery As Integer, ByRef age As Integer) 'no directed fishery
#        'Calculation of legal-sized CNR mortalities
#        'when estimated encounter of legal-sized During CNR is unknown
#        'use boat days to estimate
#        'NOTE: All catches and escapements are expanded to the maximum level of release found for all brood years
#        '      This is done so that when the calculations for sub-legal CNR mortalities are done, there is no bias
#        '      introduced resulting from differential levels of marking between brood years.  This is because
#        '      CWT catch is compared to Actual Catch to calculate the proportion of the total catch assigned to the
#        '      CWT stock.  If one brood year is under- or over- represented, it will bias the over-all assignment of
#        '      sub-legal mortalities.  The catches are set back to the original values after the cohort analysis
#        '      (see SUB ResetCatches)
#
#        'local variables
#        Dim cohrt As Single
#        Dim Annual, Instantaneous As Integer
#        Dim AnnualRetentionEncounterRate, InstantaneousRetentionEncounterRate, AnnualCNREncounterRate, InstantaneousCNREncounterRate As Single
#        Dim IMortRt(1, 1) As Single
#        Dim tp As Single
#        Dim sublegal As Integer = 1
#        Dim legal As Integer = 2
#        Instantaneous = 0
#        Annual = 1
#        'DM = DropoffRate(YR, PSCFishery) dropoff rate is not used, SHould it be????
#        'Note: removed division by Drop-off rate, Oct. 29 1997; included in external Q
#        '      For this algorithm to work correctly, the q values must be computed
#        '      with dropoff mortality included.
#        For sizeGroup As Integer = sublegal To legal
#            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And sizeGroup = sublegal And BroodYear >= traceThisYear Then WriteLine(debug_CNRNoDirID, "2920 SL", SubLegalCatchabilityCoeifficient(age, PSCFishery), SublegalSelectivityFactor(YR, PSCFishery), CNREffort(YR, PSCFishery))
#            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And sizeGroup = legal And BroodYear >= traceThisYear Then WriteLine(debug_CNRNoDirID, "2921 L", LegalCatchabilityCoeifficient(age, PSCFishery), LegalSelectivityFactor(YR, PSCFishery), CNREffort(YR, PSCFishery))
#            'get annual landing rate for hypothetical retention fishery:  catchability * selectivity * boat-days
#            If sizeGroup = sublegal Then
#                AnnualRetentionEncounterRate = 1 - System.Math.Exp(-SubLegalCatchabilityCoeifficient(age, PSCFishery) * SublegalSelectivityFactor(YR, PSCFishery) * CNREffort(YR, PSCFishery))
#            ElseIf sizeGroup = legal Then
#                AnnualRetentionEncounterRate = 1 - System.Math.Exp(-LegalCatchabilityCoeifficient(age, PSCFishery) * LegalSelectivityFactor(YR, PSCFishery) * CNREffort(YR, PSCFishery))
#            End If
#            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CNRNoDirID, "2928 ann", AnnualRetentionEncounterRate)
#            'convert to Instantaneous rate
#            InstantaneousRetentionEncounterRate = -System.Math.Log(1 - AnnualRetentionEncounterRate)
#            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CNRNoDirID, "2932 inst", AnnualRetentionEncounterRate)
#
#            'difference between hypothetical retention and CNR rates is
#            'replacement of boat-days with boat-reavailability day
#            'i.e. boat-days /(CNR season in days/availability in days)
#            tp = CNRSeasonLength(YR, PSCFishery) / Reavailability(YR, PSCFishery)
#
#            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CNRNoDirID, "2873", sizeGroup, YR, "AnnRetenEncRte", AnnualRetentionEncounterRate, "InstantaneousRetentionEncounterRate", InstantaneousRetentionEncounterRate, "tp", tp)
#
#
#            'convert hypothetical retention rate to CNR rate
#            InstantaneousCNREncounterRate = InstantaneousRetentionEncounterRate / tp
#            'convert Instantaneous rate back to annual rate
#            AnnualCNREncounterRate = 1 - System.Math.Exp(-InstantaneousCNREncounterRate)
#            If sizeGroup = sublegal Then
#                IMortRt(Annual, 0) = AnnualCNREncounterRate
#            ElseIf sizeGroup = legal Then
#                IMortRt(Annual, 0) = AnnualCNREncounterRate * LegalIMRate(YR, PSCFishery)
#            End If
#
#            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CNRNoDirID, "2879", sizeGroup, BroodYear, age, YR, "AnnRetenEncRte", LegalCatchabilityCoeifficient(age, PSCFishery), LegalSelectivityFactor(YR, PSCFishery), CNREffort(YR, PSCFishery), "tp", CNRSeasonLength(YR, PSCFishery), Reavailability(YR, PSCFishery), "IMortRT", AnnualCNREncounterRate, LegalIMRate(YR, PSCFishery))
#
#            IMortRt(Instantaneous, 0) = -System.Math.Log(1 - IMortRt(Annual, 0)) 'this is already computed above
#            IMortRt(Annual, 1) = 1 - System.Math.Exp(-IMortRt(Instantaneous, 0) * tp) 'this is already computed above
#            If CompleteBYFlag(BroodYear) = True Then
#                If terminal(PSCFishery, age) = False Then
#                    cohrt = Cohort(BroodYear, age) * SurvivalRate(age)
#                Else
#                    If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "3045 CNRNoDir callTotalTerminalCatch_Age", ShakerMethod, BroodYear, age)
#                    cohrt = TotalTerminalCatch_Age(BroodYear, age, pass) + Escape(BroodYear, age)
#                End If
#            Else
#                If ShakerMethod = "C" Or age <= LastAge(BroodYear) Then
#                    cohrt = CalcEstmCohrt2(BroodYear, age)
#                    If isTraceCalc = True And shakermethod = traceThisShakerMethod Then WriteLine(debug_Cohort_IncompleteBroodID, "3010 CNRNoDir", ShakerMethod, BroodYear, age, PSCFishery, cohrt)
#                Else
#                    cohrt = CalcEstmCohrt(BroodYear, age)
#                End If
#                If terminal(PSCFishery, age) = True Then
#                    cohrt = cohrt * (1 - AveragePreTermER(age)) * AverageMatRate(age)
#                End If
#            End If
#            If sizeGroup = sublegal Then
#                SubLegalCNRMortality(PSCFishery, age, BroodYear) = IMortRt(Annual, 1) * cohrt
#                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_CNRNoDirID, "2899", BroodYear, PSCFishery, age, "CNRNoDir SL", SubLegalCNRMortality(PSCFishery, age, BroodYear), IMortRt(Annual, 1), cohrt, "calYr", YR, "relratio", RelRatio(BroodYear))
#            ElseIf sizeGroup = legal Then
#                LegalCNRMortality(PSCFishery, age, BroodYear) = IMortRt(Annual, 1) * cohrt
#                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear >= traceThisYear Then WriteLine(debug_CNRNoDirID, "2903", BroodYear, PSCFishery, age, "CNRNoDir L", LegalCNRMortality(PSCFishery, age, BroodYear), IMortRt(Annual, 1), cohrt, "calYr", YR, "relratio", RelRatio(BroodYear))
#            End If
#        Next sizeGroup
#
#    End Sub 

}