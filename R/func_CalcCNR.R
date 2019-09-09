#ERA function ShakerMethod1() and ShakerMethod4()
#Translated from VB ERA CIS code
#June 2018 -2019
#Author: Catarina Wor



CalcCNR <- function(D, M){

    #'calculates legal and sublegal CNR using data from ERA_IMInputs database table
    allBY <- D$FirstBY:D$LastBY

    for(BroodYear in seq_along(allBY)){
        #'skip missing brood years
        if(!D$MissingBroodYearFlag[BroodYear]){
            for(PSCFishery in seq_len(M$NumberPSCFisheries))
        }

    }




}


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
