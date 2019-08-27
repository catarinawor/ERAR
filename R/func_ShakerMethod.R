#ERA function ShakerMethod1() and ShakerMethod4()
#Translated from VB ERA CIS code
#June 2018 -2019
#Author: Catarina Wor


#'@title ShakerMethod1
#'
#'@description calculates the sublegal shakers
#' 
#' 
#'@param M A list passed to MainSub
#' 
#'@param D A list contining the outputs of GetSizeLimitLengthVulnerable
#'
#'@details Calculates stock,age,fishery,year specific PNV values using ND function and input values.
#'
#'@return  
#' 
#' 
#'@export
#'
#'@examples
#' 
#' 
ShakerMethod1 <- function(D,M){

    allCalYr <- (D$FirstBY + D$OceanStartAge) : M$LastCalendarYear
    allBY <- D$FirstBY:D$LastBY
    SublegalShakerMortalities <- array(NA, dim=c(M$NumberPSCFisheries, D$MaxAge, D$LastBY)) 
    SublegalDropoffMortalities <- array(NA, dim=c(M$NumberPSCFisheries, D$MaxAge, D$LastBY)) 
    ExtraLegalShakerMortality <- array(NA, dim=c(M$NumberPSCFisheries, D$MaxAge, D$LastBY))
    ExtraLegalShakerDropoffs <- array(NA, dim=c(M$NumberPSCFisheries, D$MaxAge, D$LastBY))

    TempLegalDropoffs <- matrix(0,nrow=length(allBY), ncol= D$MaxAge)
    TempTerminalShakers <- matrix(0,nrow=length(allBY), ncol= D$MaxAge)
    TempSublegalShakers <- matrix(0,nrow=length(allBY), ncol= D$MaxAge)
    TempTerminalShakerDropoffs <- matrix(0,nrow=length(allBY), ncol= D$MaxAge)
    TempTerminalLegalDropoffs <- matrix(0,nrow=D$LastBY, ncol= D$MaxAge)
    TempSublegalShakerDropoffs <- matrix(0,nrow=D$LastBY, ncol= D$MaxAge)

    TotalTerminalShakers <- matrix(0,nrow=length(D$FirstBY:D$LastBY),ncol=D$MaxAge)
    TotalTerminalShakerDropoffs <- matrix(0,nrow=length(D$FirstBY:D$LastBY),ncol=D$MaxAge)

    CalendarYearShakers <- matrix(0,M$LastCalendarYear,M$NumberPSCFisheries)
   

    #'calculates the sublegal shakers

    if(M$isReplicateCohShak){
        lastYear <- max(D$IMdf$CalendarYear) 
    }else{
        lastYear <- M$LastCalendarYear 
    }

    valbroodyr <- TRUE
    valcalyr <- TRUE

    for(PSCFishery in seq_len( M$NumberPSCFisheries - 1 )){
        #'excludes escapement but not strays?
        for(CalYr in seq_along(allCalYr)){
            NonVulnerableCohort <- rep(0, D$MaxAge)
            TotalVulnerableCohort <- 0

            #'Broodyear not defined yet. This makes zero sense. 
            # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2878", BroodYear, PSCFishery, TotalVulnerableCohort, "totVulnChrt should be zero")
            valcalyr <- TRUE

            for(age in D$OceanStartAge:D$MaxAge){
                valbroodyr <- TRUE
                
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2880", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnChrt start of age loop")
                
                #'derive either brood and calendar year from age as appropriate
                # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2882 shakerMethod1", CalYr - Age, Age, CalYr, LastIMDataYear, FirstBY, LastBY, TotalVulnerableCohort)
                if(M$ShakerMethod=="C"){ #'CalendarYr method
                    
                    BroodYear <- allCalYr[CalYr] - age 
                    yr_ <- allCalYr[CalYr]

                }else{ #'Brood Year method
                    
                    BroodYear <- allCalYr[CalYr] - D$OceanStartAge
                    yr_ <- BroodYear + age
                }
                BYind <- which(allBY==BroodYear)
                
                
                if((BroodYear + age) > max(D$IMdf$CalendarYear)){
                    #GoTo NxtAge
                    valbroodyr <- FALSE
                    print("GoTo NxtAge")
                    
                }

                if(M$ShakerMethod == "C"){ #'CalendarYr
                    
                    if(BroodYear < D$FirstBY){
                        stop("BroodYear < D$FirstBY \n Check your data")
                    }

                    if(BroodYear > D$LastBY){
                        valbroodyr <- FALSE
                    }

                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2895 shakerMethod1", ShakerMethod, BroodYear, Age, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), TotalVulnerableCohort)
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "2896 shakerMethod1", ShakerMethod, CalYr, BroodYear, Age, PSCFishery, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), "lastBY", LastBY, TotalVulnerableCohort)
     
                    if(D$MissingBroodYearFlag$Flag[D$MissingBroodYearFlag$BY==BroodYear]){
                        valbroodyr <- FALSE
                    }

                }else if(M$ShakerMethod == "B"){
                    if(BroodYear >  D$LastBY){
                        valcalyr <- FALSE
                    }
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2900 shakerMethod1", ShakerMethod, BroodYear, Age, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), TotalVulnerableCohort)
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "2901 shakerMethod1", ShakerMethod, CalYr, BroodYear, Age, PSCFishery, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), "lastBY", LastBY)
            
                    if(D$MissingBroodYearFlag$Flag[D$MissingBroodYearFlag$BY==BroodYear]){
                        valcalyr <- FALSE
                    }
                }
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2904", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnchrt before cohrt=")

                if(valbroodyr & valcalyr){
                    if( D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY==allBY[BYind]]){
                        if(!D$terminal[PSCFishery, age]){
                            #'PreTerm cohrt after natural mortality
                            cohrt <- D$Cohort[BYind, age] * D$survivaldf$SurvivalRate[D$survivaldf$Age==age]
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_CohortID, "2908 com BY term F", BroodYear, Age, PSCFishery, cohrt, Cohort(BroodYear, Age), SurvivalRate(Age))
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2908 com BY term F", BroodYear, Age, PSCFishery, cohrt, Cohort(BroodYear, Age), SurvivalRate(Age))
                        }else{
                            #'total terminal landed catch + terminal shakers + terminal CNR + escapement
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2911 ShakMeth1 call TotalTerminalcatcg_Age", ShakerMethod, BroodYear, Age)
                            cohrt <- TotalTerminalCatch_Age(D, M, BY=BYind, age=Age) + D$Escape[BYind, age]
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_CohortID, "2914 com BY term T", BroodYear, Age, PSCFishery, "cohort", cohrt, "totalTerminalCatch_Age", TotalTerminalCatch_Age(BroodYear, Age, pass), "Escape", Escape(BroodYear, Age))
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2914 com BY term T", BroodYear, Age, PSCFishery, "cohort", cohrt, "totalTerminalCatch_Age", TotalTerminalCatch_Age(BroodYear, Age, pass), "Escape", Escape(BroodYear, Age))
                        }
                    }else{ #'cohorts estimated for incomplete broods
                        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2917 shakerMethod1", BroodYear, ShakerMethod, "1", Age, LastAge(BroodYear), TotalVulnerableCohort)
                        if(M$ShakerMethod == "C" | age <= D$LastAge[BYind]){
                            cohrt <- CalcEstmCohrt2( D, M, BYind,age )
                            #'If isTraceCalc = True and shakermethod = traceThisShakerMethod And BroodYear>= traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_Cohort_IncompleteBroodID, "2921 incom BY shakCalcFg 1",ShakerMethod, BroodYear, Age, PSCFishery, cohrt, CalcEstmCohrt2(BroodYear, Age))
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_Cohort_IncompleteBroodID, "2921 ShakerMethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt)
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2921 ShakerMethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt)
                        }else{
                           
                            cohrt <- CalcEstmCohrt( D, M, BYind, age)
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_Cohort_IncompleteBroodID, "2924 Shakermethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt, CalcEstmCohrt(BroodYear, Age))
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2924 Shakermethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt, CalcEstmCohrt(BroodYear, Age))
                        }
                        #'for all terminal fishery ages
                        if(D$terminal[PSCFishery,age]){
                            cohrt <- cohrt * (1 - D$AveragePreTermER[age]) * D$AverageMatRate[age]
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_CohortID, "2929 incom BY term", BroodYear, Age, PSCFishery, cohrt, AveragePreTermER(Age), AverageMatRate(Age))
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2929 incom BY term", BroodYear, Age, PSCFishery, cohrt, AveragePreTermER(Age), AverageMatRate(Age))
                        }
                    }#'CompleteBYFlag(BroodYear) = True

                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2932", BroodYear, Age, PSCFishery, TotalVulnerableCohort, "totVulnChrt before summation")
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CohortID, "line 2932", BroodYear, Age, PSCFishery, TotalVulnerableCohort, "totVulnChrt before summation")
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "line 2932", BroodYear, Age, PSCFishery, TotalVulnerableCohort, "totVulnChrt before summation")
                    NonVulnerableCohort[age] <- cohrt * D$PNV[[age - D$OceanStartAge + 1]][BYind + age, PSCFishery + 1]
                    TotalVulnerableCohort <- TotalVulnerableCohort + cohrt * D$PV[[age - D$OceanStartAge + 1]][BYind + age, PSCFishery + 1]

                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2937 nonVuln", ShakerMethod, BroodYear, Age, PSCFishery, NonVulnerableCohort(Age), cohrt, PNV(BroodYear + Age, PSCFishery, Age))
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2938 totVuln", ShakerMethod, BroodYear, Age, PSCFishery, TotalVulnerableCohort, cohrt, PV(BroodYear + Age, PSCFishery, Age))
                    #'If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "line 2937 nonVuln", ShakerMethod, BroodYear, Age, PSCFishery, NonVulnerableCohort(Age), cohrt, PNV(BroodYear + Age, PSCFishery, Age))
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "line 2938 totVuln", ShakerMethod, BroodYear, Age, PSCFishery, TotalVulnerableCohort, cohrt, PV(BroodYear + Age, PSCFishery, Age))
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CohortID, "line 2939 nonVuln", BroodYear, Age, PSCFishery, NonVulnerableCohort(Age), cohrt, PNV(BroodYear + Age, PSCFishery, Age))
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And CalYr = 1983 Then WriteLine(debug_CohortID, "line 2940 totVuln", CalYr, BroodYear, Age, PSCFishery, TotalVulnerableCohort, cohrt, PV(BroodYear + Age, PSCFishery, Age), cohrt * PV(Broo    
                    
                   
                    #If BroodYear + Age > LastIMDataYear Then GoTo NxtAge
                    #If ShakerMethod = "C" Then
                    #}valbroodyr
                }
            } #next age

            if(valcalyr){

                #nxtagefun() 
                for(aged in D$OceanStartAge:D$MaxAge){
                    #'derive either brood and calendar year from age as appropriate
                    if(MShakerMethod == "C"){ 
                    #'CalendarYr
                        BroodYear <- allCalYr[CalYr] - aged
                        yr_ <- allCalYr[CalYr]
                    }else{ 
                        #'Brood Year method
                        BroodYear <- allCalYr[CalYr] - D$OceanStartAge
                        yr_ <- BroodYear + aged
                        if(M$isReplicateCohShak & D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY == BroodYear]){
                            yr_ <- allCalYr[CalYr]
                        }
                    }#'ShakerMethod = "C"
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2960 calyr", yr_, "age", Age, "brood", BroodYear, "ShakerMethod", ShakerMethod, TotalVulnerableCohort)
                    #'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2961", "fishery", PSCFishery, "age", Age, "brood", BroodYear, "FirstBY", FirstBY, "ShakerMethod", ShakerMethod, "yr_", yr_, "LastIMDataYear", LastIMDataYear)   
                    
                    if(BroodYear < D$FirstBY | yr_ > max(DIMdf$CalendarYear)){
                        break
                    }
                    #'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2963", "fishery", PSCFishery, "age", Age, "brood", BroodYear, "yr_", yr_, "CNRMethod", CNRMethod(yr_, PSCFishery), "lastby", LastBY, "ShakerMethod", ShakerMethod)
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "2969", yr_, PSCFishery, CNRMethod(yr_, PSCFishery), TotalVulnerableCohort)
        
                    if(DIMdf$CNRMethod[DIMdf$CalendarYear==yr_& DIMdf$PSCFishery==PSCFishery]!=3){
                        if(TotalVulnerableCohort !=0){
                            encounterRate = 0.00001
                            if(NonVulnerableCohort[age] > 0 & TotalVulnerableCohort > 0){
                                encounterRate <- NonVulnerableCohort[aged] / TotalVulnerableCohort
                            }
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2970 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age) / TotalVulnerableCohort, NonVulnerableCohort(Age), TotalVulnerableCohort)
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_CohortID, "line 2971 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age), TotalVulnerableCohort)
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_matShakersID, "line 2972 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age), TotalVulnerableCohort)
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2970 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age) / TotalVulnerableCohort, NonVulnerableCohort(Age), TotalVulnerableCohort)
                        }else{
                            encounterRate <- 0
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2973 encounterRate & TotalVulnerableCohort=0", BroodYear, Age, PSCFishery, encounterRate, TotalVulnerableCohort)
                            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2973 encounterrate =0", BroodYear, Age, PSCFishery, encounterRate, TotalVulnerableCohort)
                        }

                        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2973", BroodYear, PSCFishery, Age, TotalVulnerableCohort, " totVuln after goto NxtAge and If CNRMethod(yr_, PSCFishery) <> 3")
                         #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "2973", ShakerMethod, PSCFishery, Age, BroodYear, encounterRate, TotalVulnerableCohort)
             
                        if(allBY[BYind] <= D$LastBY){
                
                            SublegalShakerMortalities[PSCFishery, aged, BYind] <- 
                                                    TempCatch * 
                                                    D$IMdf$SublegalIMRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] * 
                                                    encounterRate

                            SublegalDropoffMortalities[PSCFishery, aged, BYind] <- 
                                                    TempCatch * 
                                                    D$IMdf$DropoffRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery== PSCFishery] * 
                                                    encounterRate
                                
                            #'external shaker algorithm needs more development, as is it will produce too many shakers at the older ages
                            if(D$IMdf$RetentionMethod[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] == 1){

                                warning("External Shaker algorithm is not valid.  Do not trust output.\n")
                                #'handles external shaker estimates
                   
                                SublegalShakerMortalities[PSCFishery, age, BYind] <- 
                                                    ((D$IMdf$SubLegalShakerEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] *
                                                    D$LandedCatch[PSCFishery, aged, BYind]) /
                                                    D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]) * 
                                                    D$IMdf$SublegalIMRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]


                                SublegalDropoffMortalities[PSCFishery, age, BYind] <- ((D$IMdf$SubLegalShakerEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] * 
                                                                                D$LandedCatch[PSCFishery, age, BYind] / 
                                                                                D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]) *
                                                                                D$IMdf$DropoffRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]
                            }
                            #'total (PreTerm and terminal) sublegal shakers BroodYear age
                            #'add up sublegal shakers for all fisheries and add in sublegal dropoffs
                            TempSublegalShakers[ BYind, age] <- TempSublegalShakers[ BYind, age] + SublegalShakerMortalities[PSCFishery, age, BYind]
                                
                            # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And Age = traceThisAge Then 
                            TempSublegalShakerDropoffs[BYind, age] <- TempSublegalShakerDropoffs[BYind, age] + SublegalDropoffMortalities[PSCFishery, age, BYind]
                
                            #'totalLegalDropoffs moved here to compare against Coshak12, remove when done testing
                            TempLegalDropoffs[BYind, age] <- TempLegalDropoffs[BYind, age] + LegalDropoffMortality[PSCFishery, age, BYind]
                            #'TempAgeShak(BroodYear, age) = TempAgeShak(BroodYear, age) + SublegalShakerMortalities(PSCFishery, age, BroodYear) + SublegalDropoffMortalities(PSCFishery, age, BroodYear) + LegalDropoffMortality(PSCFishery, age, BroodYear)
                                
                            if(D$terminal[PSCFishery, age-D$OceanStartAge+1]){
                                TempTerminalShakers[BYind, age] <- TempTerminalShakers[BYind, age] + SublegalShakerMortalities[PSCFishery, age, BYind]
                                TempTerminalShakerDropoffs[BYind, age] <- TempTerminalShakerDropoffs[BYind, age] + SublegalDropoffMortalities[PSCFishery, age, BYind]
                                                
                                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And Age = traceThisAge Then WriteLine(debug_matShakersID, "2999 term shakers", BroodYear, Age, PSCFishery, TempSublegalShakers(BroodYear, Age), TempTerminalShakers(BroodYear, Age), SublegalShakerMortalities(PSCFishery, Age, BroodYear), "calYr", CalYr, "catch", TempCatch, "endRte", encounterRate, "shakerRate", SublegalIMRate(CalYr, PSCFishery), "dropoffRate", DropoffRate(CalYr, PSCFishery))
                                #'moved to ShakerMethod1 to compare against Coshak12, remove from ShakerMethod1 when done testing
                                TempTerminalLegalDropoffs[BYind, age] <- TempTerminalLegalDropoffs[BYind, age] + LegalDropoffMortality[PSCFishery, age, BYind]
                                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_terminalCatchID, "3011 ShakerMethod`", BroodYear, Age, PSCFishery, "TerminalLegalDropoffs", TempTerminalLegalDropoffs(BroodYear, Age), "dropoff", LegalDropoffMortality(PSCFishery, Age, BroodYear))
                            }
                        } #end if 'BroodYear <= LastBY
                    } #endif D$IMdf$CNRMethod[D$IMdf$CalendarYear==yr_& D$IMdf$PSCFishery==PSCFishery]!=3
                    # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 3011", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnChrt before next age")
                }
            }
           
            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 3011", BroodYear, PSCFishery, TotalVulnerableCohort, "totVulnChrt before next yr")
        #SkipYR:
        } #CalYr
    } #PSCFishery
    #'code below is necessary outside of loop above so that function TotalCatch_Age works correctly
    
    for(BYind in seq_along(allBY)){
        for(age in D$OceanStartAge:D$MaxAge){
            #'total (PreTerm and terminal) sublegal shakers by age
            #'add up sublegal shakers for all fisheries and add in sublegal dropoffs
            TotalSublegalShakers[BYind, age] <- TempSublegalShakers[BYind, age]
            TotalSublegalShakerDropoffs[BYind, Age] <- TempSublegalShakerDropoffs[BYind, age]
            #'totalLegalDropoffs moved here to compare against Coshak12, remove when done testing
            TotalLegalDropoffs[BYind, age]  <- TempLegalDropoffs[BYind, age] 
            TotalTerminalShakers[BYind, age] <- TempTerminalShakers[BYind, age]
            TotalTerminalShakerDropoffs[BYind, age] <- TempTerminalShakerDropoffs[BYind, age]
            #'moved to ShakerMethod1 to compare against Coshak12, remove from ShakerMethod1 when done testing
            TotalTerminalLegalDropoffs[BYind, age] <- TempTerminalLegalDropoffs[BYind, age] 


        }

    }
    #'calculate shakers for each calendar year and fishery
    #'old code errantly added legal dropoffs to shakers so I have added them for now to allow testing against old code output
    #'************legal dropoffs should be removed from here for final code


    for(CalYr in seq_along(allCalYr){
        for(PSCFishery in seq_len( M$NumberPSCFisheries )){
            if(M$PSCFisheryGear[PSCFishery]=="STRAY"){
                isOK <- FALSE
            }
            if(M$PSCFisheryGear[PSCFishery]=="ESC"){
                isOK <- FALSE
            }
            if(isOK){
                for(age in D$OceanStartAge:D$MaxAge){
                    if(CalYr-age>=D$FirstBY & CalYr-age <= D$LastBY){
                        CalendarYearShakers[CalYr, PSCFishery] <- CalendarYearShakers[CalYr, PSCFishery] +
                         SublegalShakerMortalities[PSCFishery, age, CalYr - age] + 
                         SublegalDropoffMortalities[PSCFishery, age, CalYr - age] + 
                         D$LegalDropoffMortality[PSCFishery, age, CalYr - age]
                         #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And CalYr - Age >= traceThisYear And PSCFishery = 8 Then WriteLine(debug_CalYrShakersID, "3038 CalYrShakers", CalYr, PSCFishery, Age, "accum by fishery", CalendarYearShakers(CalYr, PSCFishery), "shaker+dropoff", (SublegalShakerMortalities(PSCFishery, Age, CalYr - Age) + SublegalDropoffMortalities(PSCFishery, Age, CalYr - Age) + LegalDropoffMortality(PSCFishery, Age, CalYr - Age)), "shaker", SublegalShakerMortalities(PSCFishery, Age, CalYr - Age), "SL dropoff", SublegalDropoffMortalities(PSCFishery, Age, CalYr - Age), "L dropoff", LegalDropoffMortality(PSCFishery, Age, CalYr - Age))
                    }
                }
            }

        }

    }
    
    


    return(list(new=list(TotalTerminalShakers=TotalTerminalShakers,
                    TotalTerminalShakerDropoffs=TotalTerminalShakerDropoffs,
                    CalendarYearShakers=CalendarYearShakers,
                    SublegalShakerMortalities=SublegalShakerMortalities,
                    SublegalDropoffMortalities=SublegalDropoffMortalities,
                    SublegalShakerMortalities=SublegalShakerMortalities),
                old=list(TotalSublegalShakers=TotalSublegalShakers,
                    TotalSublegalShakerDropoffs=TotalSublegalShakerDropoffs,
                    TotalLegalDropoffs=TotalLegalDropoffs)
                
                    ))

} #end of function















#'@title nxtagefun
#'
#'@description function to reproduce the go to statement inside shakerMethos1 
#' 
#'@param M A list passed to MainSub
#' 
#'@param D A list contining the outputs of GetSizeLimitLengthVulnerable
#'
#'@details Calculates stock,age,fishery,year specific PNV values using ND function and input values.
#'
#'@return  
#' 
#' 
#'@export
#'
#'@examples
#' 
#' 
nxtagefun<- function(M, D,
                     PSCFishery=PSCFishery,
                     CalYr=CalYr,
                     BYind=BYind,
                     TotalVulnerableCohort=TotalVulnerableCohort,
                     NonVulnerableCohort=NonVulnerableCohort,
                     TempSublegalShakers =TempSublegalShakers,
                     TempSublegalShakerDropoffs =TempSublegalShakerDropoffs,
                     TempLegalDropoffs =TempLegalDropoffs,
                     TempTerminalShakers= TempTerminalShakers,
                     TempTerminalShakerDropoffs=TempTerminalShakerDropoffs,
                     TempTerminalLegalDropoffs=TempTerminalLegalDropoffs
    ){

    allCalYr <- (D$FirstBY + D$OceanStartAge) : M$LastCalendarYear
    allBY <- D$FirstBY:D$LastBY

    SublegalShakerMortalities <- array(0, dim=c(M$NumberPSCFisheries, D$MaxAge, length(allBY))) 
    SublegalDropoffMortalities <- array(0, dim=c(M$NumberPSCFisheries, D$MaxAge, length(allBY))) 
    
    
    if(M$ShakerMethod=="C"){
        #'calendar year method
        TempCatch <- D$CalendarYearLandedCatch[CalYr, PSCFishery]
    }else{
        #'brood year method
        TempCatch <- D$TotalLandedCatch_ByFishery[BYind, PSCFishery]
    }

    for(aged in D$OceanStartAge:D$MaxAge){
        #'derive either brood and calendar year from age as appropriate
        if(MShakerMethod == "C"){ 
            #'CalendarYr
            BroodYear <- allCalYr[CalYr] - aged
            yr_ <- allCalYr[CalYr]
        }else{ 
            #'Brood Year method
            BroodYear <- allCalYr[CalYr] - D$OceanStartAge
            yr_ <- BroodYear + aged
            
            if(M$isReplicateCohShak & D$CompleteBYFlag$Flag[D$CompleteBYFlag$BY == BroodYear]){
                yr_ <- allCalYr[CalYr]
            }
        }#'ShakerMethod = "C"
        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2960 calyr", yr_, "age", Age, "brood", BroodYear, "ShakerMethod", ShakerMethod, TotalVulnerableCohort)
        #'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2961", "fishery", PSCFishery, "age", Age, "brood", BroodYear, "FirstBY", FirstBY, "ShakerMethod", ShakerMethod, "yr_", yr_, "LastIMDataYear", LastIMDataYear)
         
                    
        if(BroodYear < D$FirstBY | yr_ > max(DIMdf$CalendarYear)){
            break
        }
        #'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2963", "fishery", PSCFishery, "age", Age, "brood", BroodYear, "yr_", yr_, "CNRMethod", CNRMethod(yr_, PSCFishery), "lastby", LastBY, "ShakerMethod", ShakerMethod)
        #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "2969", yr_, PSCFishery, CNRMethod(yr_, PSCFishery), TotalVulnerableCohort)
        
        if(DIMdf$CNRMethod[DIMdf$CalendarYear==yr_& DIMdf$PSCFishery==PSCFishery]!=3){
            if(TotalVulnerableCohort !=0){
                encounterRate = 0.00001
                if(NonVulnerableCohort[age] > 0 & TotalVulnerableCohort > 0){
                    encounterRate <- NonVulnerableCohort[aged] / TotalVulnerableCohort
                }
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2970 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age) / TotalVulnerableCohort, NonVulnerableCohort(Age), TotalVulnerableCohort)
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_CohortID, "line 2971 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age), TotalVulnerableCohort)
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_matShakersID, "line 2972 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age), TotalVulnerableCohort)
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2970 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age) / TotalVulnerableCohort, NonVulnerableCohort(Age), TotalVulnerableCohort)
                   
            }else{
                encounterRate <- 0
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2973 encounterRate & TotalVulnerableCohort=0", BroodYear, Age, PSCFishery, encounterRate, TotalVulnerableCohort)
                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2973 encounterrate =0", BroodYear, Age, PSCFishery, encounterRate, TotalVulnerableCohort)
            }

            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2973", BroodYear, PSCFishery, Age, TotalVulnerableCohort, " totVuln after goto NxtAge and If CNRMethod(yr_, PSCFishery) <> 3")
            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "2973", ShakerMethod, PSCFishery, Age, BroodYear, encounterRate, TotalVulnerableCohort)
             
            if(allBY[BYind] <= D$LastBY){
                
                SublegalShakerMortalities[PSCFishery, aged, BYind] <- 
                                        TempCatch * 
                                        D$IMdf$SublegalIMRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] * 
                                        encounterRate

                SublegalDropoffMortalities[PSCFishery, aged, BYind] <- 
                                        TempCatch * 
                                        D$IMdf$DropoffRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery== PSCFishery] * 
                                        encounterRate
                                
                #'external shaker algorithm needs more development, as is it will produce too many shakers at the older ages
                if(D$IMdf$RetentionMethod[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] == 1){

                    warning("External Shaker algorithm is not valid.  Do not trust output.\n")
                    #'handles external shaker estimates
                    ##########parei aqui
                    SublegalShakerMortalities[PSCFishery, age, BYind] <- 
                                                    ((D$IMdf$SubLegalShakerEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] *
                                                    D$LandedCatch[PSCFishery, aged, BYind]) /
                                                    D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]) * 
                                                    D$IMdf$SublegalIMRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]


                    SublegalDropoffMortalities[PSCFishery, age, BYind] <- ((D$IMdf$SubLegalShakerEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery] * 
                                                                                D$LandedCatch[PSCFishery, age, BYind] / 
                                                                                D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]) *
                                                                                D$IMdf$DropoffRate[D$IMdf$CalendarYear==allCalYr[CalYr] & D$IMdf$PSCFishery==PSCFishery]
                }
                #'total (PreTerm and terminal) sublegal shakers BroodYear age
                #'add up sublegal shakers for all fisheries and add in sublegal dropoffs
                TempSublegalShakers[ BYind, age] <- TempSublegalShakers[ BYind, age] + SublegalShakerMortalities[PSCFishery, age, BYind]
                                
                # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And Age = traceThisAge Then 
                TempSublegalShakerDropoffs[BYind, age] <- TempSublegalShakerDropoffs[BYind, age] + SublegalDropoffMortalities[PSCFishery, age, BYind]
                
                #'totalLegalDropoffs moved here to compare against Coshak12, remove when done testing
                TempLegalDropoffs[BYind, age] <- TempLegalDropoffs[BYind, age] + LegalDropoffMortality[PSCFishery, age, BYind]
                #'TempAgeShak(BroodYear, age) = TempAgeShak(BroodYear, age) + SublegalShakerMortalities(PSCFishery, age, BroodYear) + SublegalDropoffMortalities(PSCFishery, age, BroodYear) + LegalDropoffMortality(PSCFishery, age, BroodYear)
                                
                if(D$terminal[PSCFishery, age-D$OceanStartAge+1]){
                    TempTerminalShakers[BYind, age] <- TempTerminalShakers[BYind, age] + SublegalShakerMortalities[PSCFishery, age, BYind]
                    TempTerminalShakerDropoffs[BYind, age] <- TempTerminalShakerDropoffs[BYind, age] + SublegalDropoffMortalities[PSCFishery, age, BYind]
                                    
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And Age = traceThisAge Then WriteLine(debug_matShakersID, "2999 term shakers", BroodYear, Age, PSCFishery, TempSublegalShakers(BroodYear, Age), TempTerminalShakers(BroodYear, Age), SublegalShakerMortalities(PSCFishery, Age, BroodYear), "calYr", CalYr, "catch", TempCatch, "endRte", encounterRate, "shakerRate", SublegalIMRate(CalYr, PSCFishery), "dropoffRate", DropoffRate(CalYr, PSCFishery))
                    #'moved to ShakerMethod1 to compare against Coshak12, remove from ShakerMethod1 when done testing
                    TempTerminalLegalDropoffs[BYind, age] <- TempTerminalLegalDropoffs[BYind, age] + LegalDropoffMortality[PSCFishery, age, BYind]
                    #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_terminalCatchID, "3011 ShakerMethod`", BroodYear, Age, PSCFishery, "TerminalLegalDropoffs", TempTerminalLegalDropoffs(BroodYear, Age), "dropoff", LegalDropoffMortality(PSCFishery, Age, BroodYear))
                }
            } #end if 'BroodYear <= LastBY
        } #endif D$IMdf$CNRMethod[D$IMdf$CalendarYear==yr_& D$IMdf$PSCFishery==PSCFishery]!=3
        # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 3011", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnChrt before next age")
    }

                return(list(TotalVulnerableCohort=TotalVulnerableCohort,
                    NonVulnerableCohort=NonVulnerableCohort,
                    SublegalShakerMortalities=SublegalShakerMortalities,
                    SublegalDropoffMortalities=SublegalDropoffMortalities,
                    TempSublegalShakers=TempSublegalShakers, 
                    TempSublegalShakerDropoffs = TempSublegalShakerDropoffs,
                    TempLegalDropoffs=TempLegalDropoffs, 
                    TempTerminalShakers=TempTerminalShakers,
                    TempTerminalShakerDropoffs=TempTerminalShakerDropoffs,
                    TempTerminalLegalDropoffs=TempTerminalLegalDropoffs))

}



#==================================
#next age func
#if(M$ShakerMethod=="C"){
                    #    #'calendar year method
                    #    TempCatch <- CalendarYearLandedCatch[CalYr, PSCFishery]
                    #}else{
                    #    #'brood year method
                    #    TempCatch <- TotalLandedCatch_ByFishery[AllBY == BroodYear, PSCFishery]
                    #}
                    #
                    #for(age in D$OceanStartAge:D$MaxAge){
                    #    #'derive either brood and calendar year from age as appropriate
                    #    if(M$ShakerMethod == "C"){ #'CalendarYr
                    #        BroodYear <- allCalYr[CalYr] - age
                    #        yr_ <- allCalYr[CalYr]
                    #    }else{ #'Brood Year method
                    #        BroodYear <- allCalYr[CalYr] - D$OceanStartAge
                    #        yr_ <- BroodYear + age
                    #        if(M$isReplicateCohShak & D$CompleteBYFlag[AllBY == BroodYear]){
                    #            yr_ <- allCalYr[CalYr]
                    #        }
                    #    }#'ShakerMethod = "C"
                    #    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod &PSCFishery==M$traceThisFishery){
                    #        sink("debug_calcEstmCohrtID.log",append =TRUE)
                    #        cat("2960 calyr", yr_, "age", age, "brood", BroodYear, "ShakerMethod", M$ShakerMethod, TotalVulnerableCohort,"\n")
                    #        sink()
                    #    }
                    #    
                    #    if(BroodYear < D$FirstBY | yr_ > max(D$IMdf$CalendarYear)){
                    #        break
                    #    }
                    #
                    #    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear == M$traceThisYear){
                    #        sink("debug_ShakerID.log",append =TRUE)
                    #        cat("2969", yr_, PSCFishery, D$IMdf$CNRMethod[D$IMdf$CalendarYear==yr_& D$IMdf$PSCFishery==PSCFishery], TotalVulnerableCohort,"\n")
                    #        sink()
                    #    }
                    #    if(D$IMdf$CNRMethod[D$IMdf$CalendarYear==yr_& D$IMdf$PSCFishery==PSCFishery]!=3){
                    #        if(TotalVulnerableCohort !=0){
                    #            encounterRate = 0.00001
                    #            if(NonVulnerableCohort[age] > 0 & TotalVulnerableCohort > 0){
                    #                encounterRate <- NonVulnerableCohort[age] / TotalVulnerableCohort
                    #            }
                    #
                    #            if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear == M$traceThisYear){
                    #                sink("debug_EncounterRateID.log",append =TRUE)
                    #                cat("line 2970 encounterrate", BroodYear, age, PSCFishery, encounterRate, NonVulnerableCohort[age] / TotalVulnerableCohort, NonVulnerableCohort[age], TotalVulnerableCohort,"\n")
                    #                sink()
                    #            }
                    #
                    #            if(M$isTraceCalc &M$ShakerMethod ==M$traceThisShakerMethod &BroodYear == M$traceThisYear){
                    #                sink("debug_CohortID.log",append =TRUE)
                    #                cat("line 2971 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort[age], TotalVulnerableCohort,"\n")
                    #                sink()
                    #            }
                    #           
                    #            if(M$isTraceCalc &M$ShakerMethod ==M$traceThisShakerMethod &BroodYear == M$traceThisYear){
                    #                sink("debug_matShakersID.log",append =TRUE)
                    #                cat("line 2972 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort[age], TotalVulnerableCohort,"\n")
                    #                sink()
                    #            }
                    #
                    #            if(M$isTraceCalc &M$ShakerMethod ==M$traceThisShakerMethod &BroodYear == M$traceThisYear){
                    #                sink("debug_ShakerID.log",append =TRUE)
                    #                cat("line 2970 encounterrate", BroodYear, age, PSCFishery, encounterRate, NonVulnerableCohort[age] / TotalVulnerableCohort, NonVulnerableCohort[age], TotalVulnerableCohort,"\n")
                    #                sink()
                    #            }
                    #            
                    #        }else{
                    #            encounterRate <- 0
                    #            
                    #            if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear == M$traceThisYear){
                    #                sink("debug_EncounterRateID.log",append =TRUE)
                    #                cat("line 2973 encounterRate & TotalVulnerableCohort=0", BroodYear, age, PSCFishery, encounterRate, TotalVulnerableCohort,"\n")
                    #                sink()
                    #            }
                    #
                    #
                    #            if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear == M$traceThisYear){
                    #                sink("debug_ShakerID.log",append =TRUE)
                    #                cat("line 2973 encounterrate =0", BroodYear, age, PSCFishery, encounterRate, TotalVulnerableCohort,"\n")
                    #                sink()
                    #            }
                    #        }
                    #        # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2973", BroodYear, PSCFishery, Age, TotalVulnerableCohort, " totVuln after goto NxtAge and If CNRMethod(yr_, PSCFishery) <> 3")
                    #        # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "2973", ShakerMethod, PSCFishery, Age, BroodYear, encounterRate, TotalVulnerableCohort)
                    #
                    #        if(BroodYear <= D$LastBY){
                    #            SublegalShakerMortalities[PSCFishery, age, BroodYear] <- TempCatch * D$IMdf$SublegalIMRate[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery] * encounterRate
                    #            SublegalDropoffMortalities[PSCFishery, age, BroodYear] <- TempCatch * D$IMdf$DropoffRate[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery== PSCFishery] * encounterRate
                    #            
                    #            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2979 SL dropoff", ShakerMethod, PSCFishery, Age, BroodYear, "dropoff", SublegalDropoffMortalities(PSCFishery, Age, BroodYear), "catch", TempCatch, "dropOffRate", DropoffRate(CalYr, PSCFishery), "encRte", encounterRate, "relRatio", RelRatio(BroodYear))
                    #            #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2980 SL shaker", ShakerMethod, PSCFishery, Age, BroodYear, "shakers", SublegalShakerMortalities(PSCFishery, Age, BroodYear), "catch", TempCatch, "SublegalIMRat", SublegalIMRate(CalYr, PSCFishery), "encRte", encounterRate, "relRatio", RelRatio(BroodYear))
                    #            #'external shaker algorithm needs more development, as is it will produce too many shakers at the older ages
                    #            if(D$IMdf$RetentionMethod[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery] == 1){
                    #                #'handles external shaker estimates
                    #                sink("warningshake.log",append =TRUE) 
                    #                cat("External Shaker algorithm is not valid.  Do not trust output.\n")
                    #                sink()
                    #                SublegalShakerMortalities[PSCFishery, age, BroodYear] <- ((D$IMdf$SubLegalShakerEst[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery] *
                    #                                                                         D$LandedCatch[PSCFishery, age, BroodYear]) /
                    #                                                                         D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery]) * 
                    #                                                                         D$IMdf$SublegalIMRate[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery]
                    #                SublegalDropoffMortalities[PSCFishery, age, BroodYear] <- ((D$IMdf$SubLegalShakerEst[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery] * 
                    #                                                                        D$LandedCatch[PSCFishery, age, BroodYear] / 
                    #                                                                        D$IMdf$LandedCatchEst[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery]) *
                    #                                                                        D$IMdf$DropoffRate[D$IMdf$CalendarYear==CalYr & D$IMdf$PSCFishery==PSCFishery]
                    #            }
                    #            #'total (PreTerm and terminal) sublegal shakers BroodYear age
                    #            #'add up sublegal shakers for all fisheries and add in sublegal dropoffs
                    #            TempSublegalShakers[ BroodYear, age] <- TempSublegalShakers[ BroodYear, age] + SublegalShakerMortalities[PSCFishery, age, BroodYear]
                    #            
                    #            # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And Age = traceThisAge Then 
                    #            if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear == M$traceThisYear & age == M$traceThisAge){
                    #                #WriteLine(debug_matShakersID, "2990 TempSublegalShakers", BroodYear, Age, PSCFishery, TempSublegalShakers(BroodYear, Age), TempSublegalShakers(BroodYear, Age), SublegalShakerMortalities(PSCFishery, Age, BroodYear), TempCatch, SublegalIMRate(CalYr, PSCFishery), encounterRate)
                    #                sink("debug_matShakersID.log",append =TRUE) 
                    #                cat("2990 TempSublegalShakers", BroodYear, Age, PSCFishery, TempSublegalShakers[BroodYear, age], TempSublegalShakers[BroodYear, age], SublegalShakerMortalities[PSCFishery, age, BroodYear, TempCatch, SublegalIMRate[CalYr, PSCFishery], encounterRate,"\n")
                    #                sink()
                    #            }
                    #            #'totalLegalDropoffs moved here to compare against Coshak12, remove when done testing
                    #            TempLegalDropoffs[BroodYear, age] <- TempLegalDropoffs[BroodYear, age] + LegalDropoffMortality[PSCFishery, age, BroodYear]
                    #            #'TempAgeShak(BroodYear, age) = TempAgeShak(BroodYear, age) + SublegalShakerMortalities(PSCFishery, age, BroodYear) + SublegalDropoffMortalities(PSCFishery, age, BroodYear) + LegalDropoffMortality(PSCFishery, age, BroodYear)
                    #            
                    #            if(D$terminal[PSCFishery, age-D$OceanStartAge+1]){
                    #                TempTerminalShakers[BroodYear, age] <- TempTerminalShakers[BroodYear, age] + SublegalShakerMortalities[PSCFishery, age, BroodYear]
                    #                TempTerminalShakerDropoffs[BroodYear, age] <- TempTerminalShakerDropoffs[BroodYear, age] + SublegalDropoffMortalities[PSCFishery, age, BroodYear]
                    #                
                    #                if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod & BroodYear == M$traceThisYear & age == M$traceThisAge ){
                    #                    sink("debug_matShakersID.log",append =TRUE) 
                    #                    cat("2999 term shakers", BroodYear, Age, PSCFishery, TempSublegalShakers[BroodYear, age], TempTerminalShakers[BroodYear, age], SublegalShakerMortalities[PSCFishery, age, BroodYear], "calYr", CalYr, "catch", TempCatch, "endRte", encounterRate, "shakerRate", SublegalIMRate[CalYr, PSCFishery], "dropoffRate", DropoffRate[CalYr, PSCFishery],"\n")
                    #                    sink()
                    #                }
                    #                #'moved to ShakerMethod1 to compare against Coshak12, remove from ShakerMethod1 when done testing
                    #                TempTerminalLegalDropoffs[BroodYear, age] <- TempTerminalLegalDropoffs[BroodYear, age] + LegalDropoffMortality[PSCFishery, age, BroodYear]
                    #                #If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_terminalCatchID, "3011 ShakerMethod`", BroodYear, Age, PSCFishery, "TerminalLegalDropoffs", TempTerminalLegalDropoffs(BroodYear, Age), "dropoff", LegalDropoffMortality(PSCFishery, Age, BroodYear))
                    #            }
                    #        } #end if 'BroodYear <= LastBY
                    #    } #endif D$IMdf$CNRMethod[D$IMdf$CalendarYear==yr_& D$IMdf$PSCFishery==PSCFishery]!=3
                    # If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 3011", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnChrt before next age")
                    #}






    for(i in 1:4){
       
        for(j in 1:5){
            if(j>2){
                print(paste("j is", j))
                break

            }
           
            print(paste("j is", j))
        }
        if(i>2){
               
                break

        }
        print(paste("i is", i))
    }
       




  
    #original VB code
    #=============================================================
        #    'calculates the sublegal shakers
        #    Dim BroodYear As Integer
        #    Dim yr_ As Integer
        #    Dim isOK As Boolean
        #    Dim lastYear As Integer
        #    Dim NonVulnerableCohort() As Single
        #    Dim cohrt, encounterRate As Single
        #    Dim TempCatch, TotalVulnerableCohort As Single
        #    Dim TempAgeShak(LastBY, MaxAge) As Single
        #    ReDim CalendarYearShakers(LastCalendarYear, NumberPSCFisheries)
        #    ReDim SublegalShakerMortalities(NumberPSCFisheries, MaxAge, LastBY)
        #    ReDim SublegalDropoffMortalities(NumberPSCFisheries, MaxAge, LastBY)
        #    ReDim ExtraLegalShakerMortality(NumberPSCFisheries, MaxAge, LastBY)
        #    ReDim ExtraLegalShakerDropoffs(NumberPSCFisheries, MaxAge, LastBY)
        #    Dim TempLegalDropoffs(LastBY, MaxAge) As Single
        #    Dim TempTerminalShakers(LastBY, MaxAge) As Single
        #    Dim TempTerminalShakerDropoffs(LastBY, MaxAge) As Single
        #    Dim TempTerminalLegalDropoffs(LastBY, MaxAge) As Single
        #    Dim TempSublegalShakerDropoffs(LastBY, MaxAge) As Single
        #    Dim TempSublegalShakers(LastBY, MaxAge) As Single
        #
        #    If isReplicateCohShak = True Then
        #      lastYear = LastIMDataYear
        #    Else
        #      lastYear = LastCalendarYear
        #    End If
        #    For PSCFishery As Integer = 1 To NumberPSCFisheries - 1 'excludes escapement
        #      For CalYr As Integer = FirstBY + OceanStartAge To LastCalendarYear 'LastBY + LastAge(LastBY)
        #        ReDim NonVulnerableCohort(MaxAge)
        #        TotalVulnerableCohort = 0
        #
        #        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2878", BroodYear, PSCFishery, TotalVulnerableCohort, "totVulnChrt should be zero")
        #
        #        For Age = OceanStartAge To MaxAge
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2880", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnChrt start of age loop")
        #
        #          'derive either brood and calendar year from age as appropriate
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2882 shakerMethod1", CalYr - Age, Age, CalYr, LastIMDataYear, FirstBY, LastBY, TotalVulnerableCohort)
        #          If ShakerMethod = "C" Then 'CalendarYr
        #            BroodYear = CalYr - Age
        #            yr_ = CalYr
        #          Else 'Brood Year method
        #            BroodYear = CalYr - OceanStartAge
        #            yr_ = BroodYear + Age
        #          End If
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2890 shakerMethod1", ShakerMethod, CalYr, Age, BroodYear, LastIMDataYear, "oceanstartage", OceanStartAge, "ShakerMethod", ShakerMethod, "BY", BroodYear, "age", Age, TotalVulnerableCohort)
        #          If BroodYear + Age > LastIMDataYear Then GoTo NxtAge
        #          If ShakerMethod = "C" Then
        #            If BroodYear < FirstBY Then Exit For
        #            If BroodYear > LastBY Then GoTo NxtAge
        #            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2895 shakerMethod1", ShakerMethod, BroodYear, Age, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), TotalVulnerableCohort)
        #            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "2896 shakerMethod1", ShakerMethod, CalYr, BroodYear, Age, PSCFishery, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), "lastBY", LastBY, TotalVulnerableCohort)
        #            If MissingBroodYearFlag(BroodYear) = True Then GoTo NxtAge
        #          ElseIf ShakerMethod = "B" Then
        #            If BroodYear > LastBY Then GoTo SkipYR
        #            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2900 shakerMethod1", ShakerMethod, BroodYear, Age, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), TotalVulnerableCohort)
        #            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "2901 shakerMethod1", ShakerMethod, CalYr, BroodYear, Age, PSCFishery, MissingBroodYearFlag(BroodYear), CompleteBYFlag(BroodYear), "lastBY", LastBY)
        #            If MissingBroodYearFlag(BroodYear) = True Then GoTo SkipYR
        #          End If
        #
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2904", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnchrt before cohrt=")
        #
        #          If CompleteBYFlag(BroodYear) = True Then 'cohorts estimated for complete broods
        #            If terminal(PSCFishery, Age) = False Then
        #              'PreTerm cohrt after natural mortality
        #              cohrt = Cohort(BroodYear, Age) * SurvivalRate(Age)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_CohortID, "2908 com BY term F", BroodYear, Age, PSCFishery, cohrt, Cohort(BroodYear, Age), SurvivalRate(Age))
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2908 com BY term F", BroodYear, Age, PSCFishery, cohrt, Cohort(BroodYear, Age), SurvivalRate(Age))
        #            Else
        #              'total terminal landed catch + terminal shakers + terminal CNR + escapement
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalCatchID, "2911 ShakMeth1 call TotalTerminalcatcg_Age", ShakerMethod, BroodYear, Age)
        #
        #              cohrt = TotalTerminalCatch_Age(BroodYear, Age, pass) + Escape(BroodYear, Age)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_CohortID, "2914 com BY term T", BroodYear, Age, PSCFishery, "cohort", cohrt, "totalTerminalCatch_Age", TotalTerminalCatch_Age(BroodYear, Age, pass), "Escape", Escape(BroodYear, Age))
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2914 com BY term T", BroodYear, Age, PSCFishery, "cohort", cohrt, "totalTerminalCatch_Age", TotalTerminalCatch_Age(BroodYear, Age, pass), "Escape", Escape(BroodYear, Age))
        #            End If
        #          Else     'cohorts estimated for incomplete broods
        #            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2917 shakerMethod1", BroodYear, ShakerMethod, "1", Age, LastAge(BroodYear), TotalVulnerableCohort)
        #            If ShakerMethod = "C" Or Age <= LastAge(BroodYear) Then
        #              cohrt = CalcEstmCohrt2(BroodYear, Age)
        #              'If isTraceCalc = True and shakermethod = traceThisShakerMethod And BroodYear>= traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_Cohort_IncompleteBroodID, "2921 incom BY shakCalcFg 1",ShakerMethod, BroodYear, Age, PSCFishery, cohrt, CalcEstmCohrt2(BroodYear, Age))
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_Cohort_IncompleteBroodID, "2921 ShakerMethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2921 ShakerMethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt)
        #            Else
        #              cohrt = CalcEstmCohrt(BroodYear, Age)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_Cohort_IncompleteBroodID, "2924 Shakermethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt, CalcEstmCohrt(BroodYear, Age))
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2924 Shakermethod1", ShakerMethod, BroodYear, Age, PSCFishery, cohrt, CalcEstmCohrt(BroodYear, Age))
        #            End If
        #            'for all terminal fishery ages
        #            If terminal(PSCFishery, Age) = True Then
        #              cohrt = cohrt * (1 - AveragePreTermER(Age)) * AverageMatRate(Age)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery And Age = traceThisAge Then WriteLine(debug_CohortID, "2929 incom BY term", BroodYear, Age, PSCFishery, cohrt, AveragePreTermER(Age), AverageMatRate(Age))
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2929 incom BY term", BroodYear, Age, PSCFishery, cohrt, AveragePreTermER(Age), AverageMatRate(Age))
        #            End If
        #          End If 'CompleteBYFlag(BroodYear) = True
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2932", BroodYear, Age, PSCFishery, TotalVulnerableCohort, "totVulnChrt before summation")
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CohortID, "line 2932", BroodYear, Age, PSCFishery, TotalVulnerableCohort, "totVulnChrt before summation")
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "line 2932", BroodYear, Age, PSCFishery, TotalVulnerableCohort, "totVulnChrt before summation")
        #          NonVulnerableCohort(Age) = cohrt * PNV(BroodYear + Age, PSCFishery, Age)
        #          TotalVulnerableCohort = TotalVulnerableCohort + cohrt * (PV(BroodYear + Age, PSCFishery, Age))
        #
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2937 nonVuln", ShakerMethod, BroodYear, Age, PSCFishery, NonVulnerableCohort(Age), cohrt, PNV(BroodYear + Age, PSCFishery, Age))
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 2938 totVuln", ShakerMethod, BroodYear, Age, PSCFishery, TotalVulnerableCohort, cohrt, PV(BroodYear + Age, PSCFishery, Age))
        #          'If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "line 2937 nonVuln", ShakerMethod, BroodYear, Age, PSCFishery, NonVulnerableCohort(Age), cohrt, PNV(BroodYear + Age, PSCFishery, Age))
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "line 2938 totVuln", ShakerMethod, BroodYear, Age, PSCFishery, TotalVulnerableCohort, cohrt, PV(BroodYear + Age, PSCFishery, Age))
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_CohortID, "line 2939 nonVuln", BroodYear, Age, PSCFishery, NonVulnerableCohort(Age), cohrt, PNV(BroodYear + Age, PSCFishery, Age))
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And CalYr = 1983 Then WriteLine(debug_CohortID, "line 2940 totVuln", CalYr, BroodYear, Age, PSCFishery, TotalVulnerableCohort, cohrt, PV(BroodYear + Age, PSCFishery, Age), cohrt * PV(BroodYear + Age, PSCFishery, Age))
        #NxtAge:
        #        Next Age
        #        If ShakerMethod = "C" Then 'calendar year method
        #          TempCatch = CalendarYearLandedCatch(CalYr, PSCFishery)
        #        Else 'brood year method
        #          TempCatch = TotalLandedCatch_ByFishery(BroodYear, PSCFishery)
        #        End If
        #        For Age = OceanStartAge To MaxAge
        #          'derive either brood and calendar year from age as appropriate
        #          If ShakerMethod = "C" Then 'CalendarYr
        #            BroodYear = CalYr - Age
        #            yr_ = CalYr
        #          Else 'Brood Year method
        #            BroodYear = CalYr - OceanStartAge
        #            yr_ = BroodYear + Age
        #            If isReplicateCohShak = True And CompleteBYFlag(BroodYear) = True Then
        #              yr_ = CalYr
        #            End If
        #          End If 'ShakerMethod = "C"
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_calcEstmCohrtID, "2960 calyr", yr_, "age", Age, "brood", BroodYear, "ShakerMethod", ShakerMethod, TotalVulnerableCohort)
        #          'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2961", "fishery", PSCFishery, "age", Age, "brood", BroodYear, "FirstBY", FirstBY, "ShakerMethod", ShakerMethod, "yr_", yr_, "LastIMDataYear", LastIMDataYear)
        #          If BroodYear < FirstBY Or yr_ > LastIMDataYear Then Exit For
        #          'If isTraceCalc = True and shakermethod = traceThisShakerMethod And PSCFishery = traceThisFishery Then WriteLine(debug_ShakerID, "2963", "fishery", PSCFishery, "age", Age, "brood", BroodYear, "yr_", yr_, "CNRMethod", CNRMethod(yr_, PSCFishery), "lastby", LastBY, "ShakerMethod", ShakerMethod)
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "2969", yr_, PSCFishery, CNRMethod(yr_, PSCFishery), TotalVulnerableCohort)
        #          If CNRMethod(yr_, PSCFishery) <> 3 Then
        #            If TotalVulnerableCohort <> 0 Then
        #              encounterRate = 0.00001
        #              If NonVulnerableCohort(Age) > 0 And TotalVulnerableCohort > 0 Then
        #                encounterRate = NonVulnerableCohort(Age) / TotalVulnerableCohort
        #              End If
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2970 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age) / TotalVulnerableCohort, NonVulnerableCohort(Age), TotalVulnerableCohort)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_CohortID, "line 2971 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age), TotalVulnerableCohort)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_matShakersID, "line 2972 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age), TotalVulnerableCohort)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2970 encounterrate", BroodYear, Age, PSCFishery, encounterRate, NonVulnerableCohort(Age) / TotalVulnerableCohort, NonVulnerableCohort(Age), TotalVulnerableCohort)
        #            Else
        #              encounterRate = 0
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2973 encounterRate & TotalVulnerableCohort=0", BroodYear, Age, PSCFishery, encounterRate, TotalVulnerableCohort)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2973 encounterrate =0", BroodYear, Age, PSCFishery, encounterRate, TotalVulnerableCohort)
        #            End If
        #
        #            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_EncounterRateID, "line 2973", BroodYear, PSCFishery, Age, TotalVulnerableCohort, " totVuln after goto NxtAge and If CNRMethod(yr_, PSCFishery) <> 3")
        #            If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "2973", ShakerMethod, PSCFishery, Age, BroodYear, encounterRate, TotalVulnerableCohort)
        #
        #            If BroodYear <= LastBY Then
        #              SublegalShakerMortalities(PSCFishery, Age, BroodYear) = TempCatch * SublegalIMRate(CalYr, PSCFishery) * encounterRate
        #              SublegalDropoffMortalities(PSCFishery, Age, BroodYear) = TempCatch * DropoffRate(CalYr, PSCFishery) * encounterRate
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2979 SL dropoff", ShakerMethod, PSCFishery, Age, BroodYear, "dropoff", SublegalDropoffMortalities(PSCFishery, Age, BroodYear), "catch", TempCatch, "dropOffRate", DropoffRate(CalYr, PSCFishery), "encRte", encounterRate, "relRatio", RelRatio(BroodYear))
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_ShakerID, "line 2980 SL shaker", ShakerMethod, PSCFishery, Age, BroodYear, "shakers", SublegalShakerMortalities(PSCFishery, Age, BroodYear), "catch", TempCatch, "SublegalIMRat", SublegalIMRate(CalYr, PSCFishery), "encRte", encounterRate, "relRatio", RelRatio(BroodYear))
        #              'external shaker algorithm needs more development, as is it will produce too many shakers at the older ages
        #              If RetentionMethod(CalYr, PSCFishery) = 1 Then 'handles external shaker estimates
        #                MsgBox("External Shaker algorithm is not valid.  Do not trust output.")
        #                SublegalShakerMortalities(PSCFishery, Age, BroodYear) = ((SubLegalShakerEst(CalYr, PSCFishery) * LandedCatch(PSCFishery, Age, BroodYear)) / LandedCatchEst(CalYr, PSCFishery)) * SublegalIMRate(CalYr, PSCFishery)
        #                SublegalDropoffMortalities(PSCFishery, Age, BroodYear) = ((SubLegalShakerEst(CalYr, PSCFishery) * LandedCatch(PSCFishery, Age, BroodYear)) / LandedCatchEst(CalYr, PSCFishery)) * DropoffRate(CalYr, PSCFishery)
        #              End If
        #              'total (PreTerm and terminal) sublegal shakers BroodYear age
        #              'add up sublegal shakers for all fisheries and add in sublegal dropoffs
        #              TempSublegalShakers(BroodYear, Age) = TempSublegalShakers(BroodYear, Age) + SublegalShakerMortalities(PSCFishery, Age, BroodYear)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And Age = traceThisAge Then WriteLine(debug_matShakersID, "2990 TempSublegalShakers", BroodYear, Age, PSCFishery, TempSublegalShakers(BroodYear, Age), TempSublegalShakers(BroodYear, Age), SublegalShakerMortalities(PSCFishery, Age, BroodYear), TempCatch, SublegalIMRate(CalYr, PSCFishery), encounterRate)
        #              TempSublegalShakerDropoffs(BroodYear, Age) = TempSublegalShakerDropoffs(BroodYear, Age) + SublegalDropoffMortalities(PSCFishery, Age, BroodYear)
        #              'totalLegalDropoffs moved here to compare against Coshak12, remove when done testing
        #              TempLegalDropoffs(BroodYear, Age) = TempLegalDropoffs(BroodYear, Age) + LegalDropoffMortality(PSCFishery, Age, BroodYear)
        #              'TempAgeShak(BroodYear, age) = TempAgeShak(BroodYear, age) + SublegalShakerMortalities(PSCFishery, age, BroodYear) + SublegalDropoffMortalities(PSCFishery, age, BroodYear) + LegalDropoffMortality(PSCFishery, age, BroodYear)
        #              If terminal(PSCFishery, Age) = True Then
        #                TempTerminalShakers(BroodYear, Age) = TempTerminalShakers(BroodYear, Age) + SublegalShakerMortalities(PSCFishery, Age, BroodYear)
        #                TempTerminalShakerDropoffs(BroodYear, Age) = TempTerminalShakerDropoffs(BroodYear, Age) + SublegalDropoffMortalities(PSCFishery, Age, BroodYear)
        #
        #                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And Age = traceThisAge Then WriteLine(debug_matShakersID, "2999 term shakers", BroodYear, Age, PSCFishery, TempSublegalShakers(BroodYear, Age), TempTerminalShakers(BroodYear, Age), SublegalShakerMortalities(PSCFishery, Age, BroodYear), "calYr", CalYr, "catch", TempCatch, "endRte", encounterRate, "shakerRate", SublegalIMRate(CalYr, PSCFishery), "dropoffRate", DropoffRate(CalYr, PSCFishery))
        #                'moved to ShakerMethod1 to compare against Coshak12, remove from ShakerMethod1 when done testing
        #                TempTerminalLegalDropoffs(BroodYear, Age) = TempTerminalLegalDropoffs(BroodYear, Age) + LegalDropoffMortality(PSCFishery, Age, BroodYear)
        #                If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear Then WriteLine(debug_terminalCatchID, "3011 ShakerMethod`", BroodYear, Age, PSCFishery, "TerminalLegalDropoffs", TempTerminalLegalDropoffs(BroodYear, Age), "dropoff", LegalDropoffMortality(PSCFishery, Age, BroodYear))
        #              End If
        #            End If  'BroodYear <= LastBY
        #          End If 'CNRMethod(yr_, PSCFishery) <> 3
        #          If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 3011", BroodYear, Age, PSCFishery, TotalVulnerableCohort, " totVulnChrt before next age")
        #        Next Age
        #SkipYR:
        #        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And BroodYear = traceThisYear And PSCFishery = traceThisFishery Then WriteLine(debug_EncounterRateID, "line 3011", BroodYear, PSCFishery, TotalVulnerableCohort, "totVulnChrt before next yr")
        #      Next CalYr
        #    Next PSCFishery
        #    'code below is necessary outside of loop above so that function TotalCatch_Age works correctly
        #    For BroodYear = FirstBY To LastBY
        #      For Age = OceanStartAge To MaxAge
        #        'total (PreTerm and terminal) sublegal shakers by age
        #        'add up sublegal shakers for all fisheries and add in sublegal dropoffs
        #        TotalSublegalShakers(BroodYear, Age) = TempSublegalShakers(BroodYear, Age)
        #        TotalSublegalShakerDropoffs(BroodYear, Age) = TempSublegalShakerDropoffs(BroodYear, Age)
        #        'totalLegalDropoffs moved here to compare against Coshak12, remove when done testing
        #        TotalLegalDropoffs(BroodYear, Age) = TempLegalDropoffs(BroodYear, Age)
        #        TotalTerminalShakers(BroodYear, Age) = TempTerminalShakers(BroodYear, Age)
        #        TotalTerminalShakerDropoffs(BroodYear, Age) = TempTerminalShakerDropoffs(BroodYear, Age)
        #        'moved to ShakerMethod1 to compare against Coshak12, remove from ShakerMethod1 when done testing
        #        TotalTerminalLegalDropoffs(BroodYear, Age) = TempTerminalLegalDropoffs(BroodYear, Age)
        #      Next Age
        #    Next BroodYear
        #
        #    'calculate shakers for each calendar year and fishery
        #    'old code errantly added legal dropoffs to shakers so I have added them for now to allow testing against old code output
        #    '************legal dropoffs should be removed from here for final code
        #    For CalYr As Integer = FirstBY + OceanStartAge To LastCalendarYear
        #      For PSCFishery As Integer = 1 To NumberPSCFisheries
        #        isOK = True
        #        If PSCFisheryGear(PSCFishery) = "STRAY" Then isOK = False
        #        If PSCFisheryGear(PSCFishery) = "ESC" Then isOK = False
        #        If isOK = True Then
        #          For Age = OceanStartAge To MaxAge
        #            If CalYr - Age >= FirstBY And CalYr - Age <= LastBY Then
        #              CalendarYearShakers(CalYr, PSCFishery) += SublegalShakerMortalities(PSCFishery, Age, CalYr - Age) + SublegalDropoffMortalities(PSCFishery, Age, CalYr - Age) + LegalDropoffMortality(PSCFishery, Age, CalYr - Age)
        #              If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And CalYr - Age >= traceThisYear And PSCFishery = 8 Then WriteLine(debug_CalYrShakersID, "3038 CalYrShakers", CalYr, PSCFishery, Age, "accum by fishery", CalendarYearShakers(CalYr, PSCFishery), "shaker+dropoff", (SublegalShakerMortalities(PSCFishery, Age, CalYr - Age) + SublegalDropoffMortalities(PSCFishery, Age, CalYr - Age) + LegalDropoffMortality(PSCFishery, Age, CalYr - Age)), "shaker", SublegalShakerMortalities(PSCFishery, Age, CalYr - Age), "SL dropoff", SublegalDropoffMortalities(PSCFishery, Age, CalYr - Age), "L dropoff", LegalDropoffMortality(PSCFishery, Age, CalYr - Age))
        #            End If
        #          Next Age
        #        End If
        #      Next PSCFishery
        #    Next CalYr
  
}




