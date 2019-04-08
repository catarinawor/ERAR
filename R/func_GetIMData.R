#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


#' @title GetIMData
#'
#' @description  Get incidental mortality data based on Calendar years and PNV Region (whatever that is).
#' 
#' 
#'
#' @param M A list passed to MainSub
#' @param D A list containing the outputs of GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates 
#'
#' @details This function was directly translated from the VB code, and as is it is extermely unneficient in R. 
#' Think about linearizing all the outputs as this program evolves
#'
#' @return D: A list 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' @importFrom tidyr spread
#' 
#' 
GetIMData <- function( D,M ){

    # ReDimension Incidental Mortality variables
    #Coshak PSL file has 1 more year of data than recoveries
    
    PNV <- list() #array(NA, dim = c(M$LastCalendarYear + 1, M$NumberPSCFisheries, D$MaxAge))
    PV <- list()  #array(NA, dim = c(M$LastCalendarYear + 1, M$NumberPSCFisheries, D$MaxAge))
    
    
   
    #Read in data from ERA_IMInputs table based on the PNVRegion of the stock,  Terminal fisheries are specific to a PNVRegion, Preterminal fisheries data is from PNVRegion 1 ("All") 
    #only get values <= LastCalendarYear unless when emulating Coshak
    #    Dim CalendarYear, PSCFishery As Integer
   # if(!isReplicateCohShak){
    
   # dta <- RODBC::odbcConnectAccess2007(M$datbse)      


    ERASQL <- paste0( "SELECT ERA_IMInputs.* FROM ERA_IMInputs WHERE CalendarYear <= ", M$LastCalendarYear, " and (PNVRegion = 1 or PNVRegion = ", D$PNVRegion,") Order By PSCFishery, CalendarYear" )
              
    #read from data base
    
    df1 <- sqlQuery( M$chnl , query = ERASQL )

    summary(df1)


    PSCFishery <- df1$PSCFishery
    CalendarYear <- df1$CalendarYear

    SublegalIMRate <- tidyr::spread(df1[,c("SublegalIMRate","CalendarYear","PSCFishery")],key=PSCFishery,value=SublegalIMRate)
    
    LegalIMRate <- tidyr::spread(df1[,c("LegalIMRate","CalendarYear","PSCFishery")],key=PSCFishery,value=LegalIMRate)

    DropoffRate <- tidyr::spread(df1[,c("DropoffRate","CalendarYear","PSCFishery")],key=PSCFishery,value= DropoffRate)
    
    df1$ExtraLegalIMRate[is.na(df1$ExtraLegalIMRate)] <- 0
    ExtraLegalIMRate <-  tidyr::spread(df1[,c("ExtraLegalIMRate","CalendarYear","PSCFishery")],key=PSCFishery,value=ExtraLegalIMRate)

    CNRMethod <-  tidyr::spread(df1[,c("CNRMethod","CalendarYear","PSCFishery")],key=PSCFishery,value=CNRMethod )
   
    df1$RetentionMethod[is.na(df1$RetentionMethod)] <- 0  
    RetentionMethod <- tidyr::spread(df1[,c("RetentionMethod","CalendarYear","PSCFishery")],key=PSCFishery,value= RetentionMethod )

    df1$CNRLegalEncounterEst[is.na(df1$CNRLegalEncounterEst)] <- 0
    CNRLegalEncounterEst <- tidyr::spread(df1[,c("CNRLegalEncounterEst","CalendarYear","PSCFishery")],key=PSCFishery,value= CNRLegalEncounterEst )

    df1$CNRSubLegalEncounterEst[is.na(df1$CNRSubLegalEncounterEst)] <- 0 
    CNRSubLegalEncounterEst <- df1$CNRSubLegalEncounterEst

    df1$CNRExtraLegalEncounterEst[is.na(df1$CNRExtraLegalEncounterEst)]<- 0 
    CNRExtraLegalEncounterEst <- tidyr::spread(df1[,c("CNRExtraLegalEncounterEst","CalendarYear","PSCFishery")],key=PSCFishery,value= CNRExtraLegalEncounterEst )

    df1$LandedCatchEst[is.na(df1$LandedCatchEst)] <- 0
    LandedCatchEst <-  tidyr::spread(df1[,c("LandedCatchEst","CalendarYear","PSCFishery")],key=PSCFishery,value=LandedCatchEst )

    df1$LegalShakerEst[is.na(df1$LegalShakerEst)] <- 0
    LegalShakerEst <-  tidyr::spread(df1[,c("LegalShakerEst","CalendarYear","PSCFishery")],key=PSCFishery,value= LegalShakerEst )

    df1$SubLegalShakerEst[is.na(df1$SubLegalShakerEst)] <- 0
    SubLegalShakerEst <-  tidyr::spread(df1[,c("SubLegalShakerEst","CalendarYear","PSCFishery")],key=PSCFishery,value=SubLegalShakerEst )

    df1$ExtraLegalShakerEst[is.na(df1$ExtraLegalShakerEst)] <-0
    ExtraLegalShakerEst <-  tidyr::spread(df1[,c("ExtraLegalShakerEst","CalendarYear","PSCFishery")],key=PSCFishery,value= ExtraLegalShakerEst )

    df1$LegalSelectivityFactor[is.na(df1$LegalSelectivityFactor)] <-0
    LegalSelectivityFactor <-  tidyr::spread(df1[,c("LegalSelectivityFactor","CalendarYear","PSCFishery")],key=PSCFishery,value=LegalSelectivityFactor )

    df1$SublegalSelectivityFactor[is.na(df1$SublegalSelectivityFactor)] <- 0
    SublegalSelectivityFactor <-  tidyr::spread(df1[,c("SublegalSelectivityFactor","CalendarYear","PSCFishery")],key=PSCFishery,value=SublegalSelectivityFactor )

    df1$ExtraLegalSelectivityFactor[is.na(df1$ExtraLegalSelectivityFactor)] <- 0
    ExtraLegalSelectivityFactor <-  tidyr::spread(df1[,c("ExtraLegalSelectivityFactor","CalendarYear","PSCFishery")],key=PSCFishery,value=ExtraLegalSelectivityFactor )

    df1$MonitoredLegalCatch[is.na(df1$MonitoredLegalCatch)] <- 0
    MonitoredLegalCatch <-  tidyr::spread(df1[,c("MonitoredLegalCatch","CalendarYear","PSCFishery")],key=PSCFishery,value=  MonitoredLegalCatch )

    df1$MonitoredLegalReleases[is.na(df1$MonitoredLegalReleases)] <- 0
    MonitoredLegalReleases <-  tidyr::spread(df1[,c("MonitoredLegalReleases","CalendarYear","PSCFishery")],key=PSCFishery,value= MonitoredLegalReleases )

    df1$SeasonUnits[is.na(df1$SeasonUnits)] <- 0
    SeasonUnits <-  tidyr::spread(df1[,c("SeasonUnits","CalendarYear","PSCFishery")],key=PSCFishery,value= SeasonUnits )

    df1$SeasonLength[is.na(df1$SeasonLength)] <- 0
    SeasonLength <-  tidyr::spread(df1[,c("SeasonLength","CalendarYear","PSCFishery")],key=PSCFishery,value= SeasonLength  )

    df1$CNRSeasonLength[is.na(df1$CNRSeasonLength)] <- 0
    CNRSeasonLength <-  tidyr::spread(df1[,c("CNRSeasonLength","CalendarYear","PSCFishery")],key=PSCFishery,value= CNRSeasonLength  )

    df1$RetentionEffort[is.na(df1$RetentionEffort)] <- 0
    RetentionEffort <-  tidyr::spread(df1[,c("RetentionEffort","CalendarYear","PSCFishery")],key=PSCFishery,value=RetentionEffort )

    df1$CNREffort[is.na(df1$CNREffort)] <- 0
    CNREffort <-  tidyr::spread(df1[,c("CNREffort","CalendarYear","PSCFishery")],key=PSCFishery,value= CNREffort )

    df1$Reavailability[is.na(df1$Reavailability)] <- 0
    Reavailability <- tidyr::spread(df1[,c("Reavailability","CalendarYear","PSCFishery")],key=PSCFishery,value=Reavailability )



    PNV[[1]] <- tidyr::spread(df1[,c("Age2PNV","CalendarYear","PSCFishery")],key=PSCFishery,value=Age2PNV )
    PNV[[2]] <- tidyr::spread(df1[,c("Age3PNV","CalendarYear","PSCFishery")],key=PSCFishery,value=Age3PNV )
    PNV[[3]] <- tidyr::spread(df1[,c("Age4PNV","CalendarYear","PSCFishery")],key=PSCFishery,value=Age4PNV )
    PNV[[4]] <- tidyr::spread(df1[,c("Age5PNV","CalendarYear","PSCFishery")],key=PSCFishery,value=Age5PNV )

    PV[[1]]<- 1 - PNV[[1]]
    PV[[2]]<- 1 - PNV[[2]]
    PV[[3]]<- 1 - PNV[[3]]
    PV[[4]]<- 1 - PNV[[4]]


    LastIMDataYear <- max(CalendarYear) 


    # 'Assign Age 5 PNV to Age 6 for stocks that go from age 2 to age 6, necessary because we only have PNVs for four ages
    if(D$OceanStartAge ==2 & D$MaxAge==6){
        PNV[[5]]<-PNV[[4]]
        PV[[5]]<-PV[[4]]

    }

   

    ERASQL2 = paste0("SELECT PSCFishery,Age,SizeClass,AvgQ FROM ERA_CatchabilityCoefficients WHERE ERAStock = '", D$CurrentStock ,"' AND ShakerMethod = '" ,M$ShakerMethod ,"'")
    df2 <- sqlQuery( M$chnl , query = ERASQL2 )
     GetIMDataErr <- 0

    if(nrow(df2)==0){
        sink("../logs/GetIMData.log", append=TRUE)
        cat("Catchability coefficients for ",D$CurrentStock, " is missing in the ERA_CatchabilityCoefficients table.  Program is going to stop.\n")
        sink()
        GetIMDataErr <- 1
    }
    #       

    df2select_L<- df2[df2$Age<D$MaxAge&df2$SizeClass=="L",c("PSCFishery", "Age", "AvgQ")]   
    LegalCatchabilityCoefficient <- tidyr::spread(df2select_L,key=PSCFishery,value=AvgQ)

    df2select_S<- df2[df2$Age<D$MaxAge&df2$SizeClass!="L",c("PSCFishery", "Age", "AvgQ")]   
    SubLegalCatchabilityCoefficient <- tidyr::spread(df2select_L,key=PSCFishery,value=AvgQ)

    return(list(IMdf=df1,catchabilitydf=df2,
        GetIMDataErr=GetIMDataErr))

    #return( list(IM_PSCFishery=PSCFishery,
    #    IM_CalendarYear=CalendarYear,
    #    SublegalIMRate=SublegalIMRate,
    #    LegalIMRate=LegalIMRate,
    #    DropoffRate=DropoffRate,
    #    ExtraLegalIMRate=ExtraLegalIMRate,
    #    CNRMethod=CNRMethod,
    #    RetentionMethod=RetentionMethod,
    #    CNRLegalEncounterEst=CNRLegalEncounterEst,
    #    CNRSubLegalEncounterEst=CNRSubLegalEncounterEst,
    #    CNRExtraLegalEncounterEst=CNRExtraLegalEncounterEst,
    #    LandedCatchEst=LandedCatchEst,
    #    LegalShakerEst=LegalShakerEst,
    #    SubLegalShakerEst=SubLegalShakerEst,
    #    ExtraLegalShakerEst=ExtraLegalShakerEst,
    #    LegalSelectivityFactor=LegalSelectivityFactor,
    #    SublegalSelectivityFactor=SublegalSelectivityFactor,
    #    ExtraLegalSelectivityFactor=ExtraLegalSelectivityFactor,
    #    MonitoredLegalCatch=MonitoredLegalCatch,
    #    MonitoredLegalReleases=MonitoredLegalReleases,
    #    SeasonUnits=SeasonUnits,
    #    SeasonLength=SeasonLength,
    #    CNRSeasonLength=CNRSeasonLength,
    #    RetentionEffort=RetentionEffort,
    #    CNREffort=CNREffort,
    #    Reavailability=Reavailability,
    #    PNV= PNV,
    #    PV=PV,
    #    LegalCatchabilityCoefficient=LegalCatchabilityCoefficient,
    #    SubLegalCatchabilityCoeificient=SubLegalCatchabilityCoeificient,
    #    GetIMDataErr=GetIMDataErr
    #    ))





  


    #Original VB code
    #===================================================================================
    #'ReDimension Incidental Mortality variables
    #    'Coshak PSL file has 1 more year of data than recoveries
    #    ReDim PNV(LastCalendarYear + 1, NumberPSCFisheries, MaxAge)
    #    ReDim PV(LastCalendarYear + 1, NumberPSCFisheries, MaxAge)
    #    ReDim LegalCatchabilityCoeifficient(MaxAge, NumberPSCFisheries)
    #    ReDim SubLegalCatchabilityCoeifficient(MaxAge, NumberPSCFisheries)
    #    ReDim CNRMethod(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim RetentionMethod(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim CNRLegalEncounterEst(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim CNRSubLegalEncounterEst(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim CNRExtraLegalEncounterEst(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim LandedCatchEst(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim LegalShakerEst(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim SubLegalShakerEst(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim ExtraLegalShakerEst(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim MonitoredLegalCatch(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim MonitoredLegalReleases(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim SeasonUnits(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim SeasonLength(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim CNRSeasonLength(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim RetentionEffort(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim CNREffort(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim Reavailability(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim SublegalIMRate(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim LegalIMRate(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim ExtraLegalIMRate(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim DropoffRate(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim LegalSelectivityFactor(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim SublegalSelectivityFactor(LastCalendarYear + 1, NumberPSCFisheries)
    #    ReDim ExtraLegalSelectivityFactor(LastCalendarYear + 1, NumberPSCFisheries)
    #
    #    LastIMDataYear = 0
    #
    #    'Read in data from ERA_IMInputs table based on the PNVRegion of the stock,  Terminal fisheries are specific to a PNVRegion, Preterminal fisheries data is from PNVRegion 1 ("All") 
    #    'only get values <= LastCalendarYear unless when emulating Coshak
    #    Dim CalendarYear, PSCFishery As Integer
    #    'If isReplicateCohShak = False Then
    #    ERASQL = "SELECT ERA_IMInputs.* FROM ERA_IMInputs WHERE CalendarYear <= " & LastCalendarYear & " and (PNVRegion = 1 or PNVRegion = " & PNVRegion & ") Order By PSCFishery, CalendarYear"
    #    'Else
    #    'ERASQL = "SELECT ERA_IMInputs.* FROM ERA_IMInputs WHERE (PNVRegion = 1 or PNVRegion = " & PNVRegion & ") Order By PSCFishery, CalendarYear"
    #    'End If
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    Do While CISDataReader.Read()
    #        PSCFishery = CISDataReader(0)
    #        CalendarYear = CISDataReader(3)
    #        SublegalIMRate(CalendarYear, PSCFishery) = CISDataReader(4)
    #        LegalIMRate(CalendarYear, PSCFishery) = CISDataReader(5)
    #        DropoffRate(CalendarYear, PSCFishery) = CISDataReader(6)
    #        ExtraLegalIMRate(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(7)), CISDataReader(7), 0)
    #        CNRMethod(CalendarYear, PSCFishery) = CISDataReader(8)
    #        RetentionMethod(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(9)), CISDataReader(9), 0)
    #        CNRLegalEncounterEst(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(13)), CISDataReader(13), 0)
    #        CNRSubLegalEncounterEst(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(14)), CISDataReader(14), 0)
    #        CNRExtraLegalEncounterEst(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(15)), CISDataReader(15), 0)
    #        LandedCatchEst(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(16)), CISDataReader(16), 0)
    #        LegalShakerEst(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(17)), CISDataReader(17), 0)
    #        SubLegalShakerEst(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(18)), CISDataReader(18), 0)
    #        ExtraLegalShakerEst(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(19)), CISDataReader(19), 0)
    #        LegalSelectivityFactor(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(20)), CISDataReader(20), 0)
    #        SublegalSelectivityFactor(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(21)), CISDataReader(21), 0)
    #        ExtraLegalSelectivityFactor(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(22)), CISDataReader(22), 0)
    #        MonitoredLegalCatch(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(23)), CISDataReader(23), 0)
    #        MonitoredLegalReleases(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(24)), CISDataReader(24), 0)
    #        SeasonUnits(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(9)), CISDataReader(9), 0)
    #        SeasonLength(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(26)), CISDataReader(26), 0)
    #        CNRSeasonLength(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(27)), CISDataReader(27), 0)
    #        RetentionEffort(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(28)), CISDataReader(28), 0)
    #        CNREffort(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(29)), CISDataReader(29), 0)
    #        Reavailability(CalendarYear, PSCFishery) = If(IsNumeric(CISDataReader(30)), CISDataReader(30), 0)
    #        PNV(CalendarYear, PSCFishery, OceanStartAge) = If(IsNumeric(CISDataReader(31)), CISDataReader(31), 0)
    #        PNV(CalendarYear, PSCFishery, OceanStartAge + 1) = If(IsNumeric(CISDataReader(32)), CISDataReader(32), 0)
    #        PNV(CalendarYear, PSCFishery, OceanStartAge + 2) = If(IsNumeric(CISDataReader(33)), CISDataReader(33), 0)
    #        PNV(CalendarYear, PSCFishery, OceanStartAge + 3) = If(IsNumeric(CISDataReader(34)), CISDataReader(34), 0)
    #        PV(CalendarYear, PSCFishery, OceanStartAge) = 1 - PNV(CalendarYear, PSCFishery, OceanStartAge)
    #        PV(CalendarYear, PSCFishery, OceanStartAge + 1) = 1 - PNV(CalendarYear, PSCFishery, OceanStartAge + 1)
    #        PV(CalendarYear, PSCFishery, OceanStartAge + 2) = 1 - PNV(CalendarYear, PSCFishery, OceanStartAge + 2)
    #        PV(CalendarYear, PSCFishery, OceanStartAge + 3) = 1 - PNV(CalendarYear, PSCFishery, OceanStartAge + 3)
    #        If CalendarYear > LastIMDataYear Then LastIMDataYear = CalendarYear
    #    Loop
    #    CISDataReader.Close()
    #    'Assign Age 5 PNV to Age 6 for stocks that go from age 2 to age 6, necessary because we only have PNVs for four ages
    #    If OceanStartAge = 2 And MaxAge = 6 Then
    #        For CY As Integer = 1960 To LastCalendarYear 'set to 1960 to speed up the loop
    #            For Fish As Integer = 1 To NumberPSCFisheries
    #                PNV(CY, Fish, MaxAge) = PNV(CY, Fish, OceanStartAge + 3)
    #                PV(CY, Fish, MaxAge) = 1 - PNV(CY, Fish, MaxAge)
    #            Next Fish
    #        Next CY
    #    End If
    #    'Read in data from catchability coefficients
    #    'ERASQL = "SELECT PSCFishery,Age,SizeClass,AvgQ FROM ERA_CatchabilityCoefficients INNER JOIN PSCtoModelFisheryMapping ON ERA_CatchabilityCoefficients.ModelFishery = PSCtoModelFisheryMapping.ModelFishery WHERE ERAStock = '" & CurrentStock & "' AND ShakerMethod = '" & ShakerMethod & "'"
    #    ERASQL = "SELECT PSCFishery,Age,SizeClass,AvgQ FROM ERA_CatchabilityCoefficients WHERE ERAStock = '" & CurrentStock & "' AND ShakerMethod = '" & ShakerMethod & "'"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    If CISDataReader.HasRows Then
    #        Do While CISDataReader.Read()
    #            If Not CISDataReader(1) > MaxAge Then
    #                PSCFishery = CISDataReader(0)
    #                If CISDataReader(2) = "L" Then
    #                    LegalCatchabilityCoeifficient(CISDataReader(1), PSCFishery) = CISDataReader(3)
    #                    'If isTraceCalc = True and shakermethod = traceThisShakerMethod and ShakerMethod = "B" Then WriteLine(debug_CNRNoDirID, "1771 get IMdata", LegalCatchabilityCoeifficient(CISDataReader(1), PSCFishery), CISDataReader(1), PSCFishery)
    #                Else
    #                    SubLegalCatchabilityCoeifficient(CISDataReader(1), PSCFishery) = CISDataReader(3)
    #                End If
    #            End If
    #        Loop
    #        CISDataReader.Close()
    #    Else
    #        'exit sub
    #        MsgBox("Catchability coefficients for " & CurrentStock & " is missing in the ERA_CatchabilityCoefficients table.  Program is going to stop.")
    #        End
    #    End If





}




