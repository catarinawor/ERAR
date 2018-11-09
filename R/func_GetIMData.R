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
#' @description  
#' 
#' 
#'
#' @param M A list passed to MainSub
#'
#' @details
#'
#' @return D: A list 
#' 
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
GetIMData <- function(){

    # ReDimension Incidental Mortality variables
    #Coshak PSL file has 1 more year of data than recoveries
    
    PNV <- array(NA, dim = c(LastCalendarYear + 1, NumberPSCFisheries, MaxAge))
    PV <- array(NA, dim = c(LastCalendarYear + 1, NumberPSCFisheries, MaxAge))
    LegalCatchabilityCoeifficient < -matrix(NA, nrow = MaxAge, ncol = NumberPSCFisheries)
    SubLegalCatchabilityCoeifficient <- matrix(NA, nrow = MaxAge, ncol = NumberPSCFisheries)
    CNRMethod <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    RetentionMethod <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    CNRLegalEncounterEst <- matrix(NA,nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    CNRSubLegalEncounterEst <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    CNRExtraLegalEncounterEst <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    LandedCatchEst < -matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    LegalShakerEst <-matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    SubLegalShakerEst <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    ExtraLegalShakerEst <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    MonitoredLegalCatch <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    MonitoredLegalReleases < -matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    SeasonUnits <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    SeasonLength <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    CNRSeasonLength <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    CNRSeasonLength <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    RetentionEffort <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    CNREffort <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    Reavailability <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    SublegalIMRate <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    LegalIMRate <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    ExtraLegalIMRate <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    DropoffRate <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    LegalSelectivityFactor <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    SublegalSelectivityFactor <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    ExtraLegalSelectivityFactor <- matrix(NA, nrow = LastCalendarYear + 1, ncol = NumberPSCFisheries)
    
    LastIMDataYear = 0
    #Read in data from ERA_IMInputs table based on the PNVRegion of the stock,  Terminal fisheries are specific to a PNVRegion, Preterminal fisheries data is from PNVRegion 1 ("All") 
    #only get values <= LastCalendarYear unless when emulating Coshak
    #    Dim CalendarYear, PSCFishery As Integer
   # if(!isReplicateCohShak){
    
    ERASQL = "SELECT ERA_IMInputs.* FROM ERA_IMInputs WHERE CalendarYear <= " & LastCalendarYear & " and (PNVRegion = 1 or PNVRegion = " & PNVRegion & ") Order By PSCFishery, CalendarYear" 
    #read from data base
    
    for (i in 1:length(CISDataReader[1])){
        PSCFishery <- CISDataReader[1]
        CalendarYear <- CISDataReader[4]
        SublegalIMRate[CalendarYear, PSCFishery] <-  CISDataReader[5]
        LegalIMRate[CalendarYear, PSCFishery] <-  CISDataReader[6]
        DropoffRate[CalendarYear, PSCFishery] = CISDataReader[7]
        
        if(is.numeric(CISDataReader[8])){
            ExtraLegalIMRate[CalendarYear, PSCFishery] <- CISDataReader[8]
        }else{
            ExtraLegalIMRate[CalendarYear, PSCFishery] <- 0
        }
        
        CNRMethod[CalendarYear, PSCFishery] <- CISDataReader[9]
        
        if(is.numeric(CISDataReader[10])){
            RetentionMethod[CalendarYear, PSCFishery] <- CISDataReader[10]
        }else{
            RetentionMethod[CalendarYear, PSCFishery] <- 0
        }
        
        if(is.numeric(CISDataReader[14])){
            NRLegalEncounterEst[CalendarYear, PSCFishery] <- CISDataReader[14]
        }else{
            NRLegalEncounterEst[CalendarYear, PSCFishery] <- 0
        }
        
        if(is.numeric(CISDataReader[15])){
            CNRSubLegalEncounterEst[CalendarYear, PSCFishery] <- CISDataReader[15]
        }else{
           CNRSubLegalEncounterEst[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[16])){
           CNRExtraLegalEncounterEst[CalendarYear, PSCFishery] <- CISDataReader[16]
        }else{
           CNRExtraLegalEncounterEst[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[17])){
           LandedCatchEst[CalendarYear, PSCFishery] <- CISDataReader[17]
        }else{
           LandedCatchEst[CalendarYear, PSCFishery] <- 0
        }
            
        if(is.numeric(CISDataReader[18])){
           LegalShakerEst[CalendarYear, PSCFishery] <- CISDataReader[18]
        }else{
           LegalShakerEst[CalendarYear, PSCFishery] <- 0
        }
    #        
        if(is.numeric(CISDataReader[19])){
           SubLegalShakerEst[CalendarYear, PSCFishery] <- CISDataReader[19]
        }else{
           SubLegalShakerEst[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[20])){
           ExtraLegalShakerEst[CalendarYear, PSCFishery] <- CISDataReader[20]
        }else{
           ExtraLegalShakerEst[CalendarYear, PSCFishery] <- 0
        }
            
        if(is.numeric(CISDataReader[21])){
           LegalSelectivityFactor[CalendarYear, PSCFishery] <- CISDataReader[21]
        }else{
           LegalSelectivityFactor[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[22])){
           SublegalSelectivityFactor[CalendarYear, PSCFishery] <- CISDataReader[22]
        }else{
           SublegalSelectivityFactor[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[23])){
           ExtraLegalSelectivityFactor[CalendarYear, PSCFishery] <- CISDataReader[23]
        }else{
           ExtraLegalSelectivityFactor[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[24])){
           MonitoredLegalCatch[CalendarYear, PSCFishery] <- CISDataReader[24]
        }else{
           MonitoredLegalCatch[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[25 ])){
           MonitoredLegalReleases[CalendarYear, PSCFishery] <- CISDataReader[25]
        }else{
           MonitoredLegalReleases[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[10])){
           SeasonUnits[CalendarYear, PSCFishery] <- CISDataReader[10]
        }else{
           SeasonUnits[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[27])){
           SeasonLength[CalendarYear, PSCFishery] <- CISDataReader[27]
        }else{
          SeasonLength[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[28])){
           CNRSeasonLength[CalendarYear, PSCFishery] <- CISDataReader[28]
        }else{
          CNRSeasonLength[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[29])){
            RetentionEffort[CalendarYear, PSCFishery] <- CISDataReader[29]
        }else{
            RetentionEffort[CalendarYear, PSCFishery] <- 0
        }
   
        if(is.numeric(CISDataReader[30])){
            CNREffort[CalendarYear, PSCFishery] <- CISDataReader[30]
        }else{
            CNREffort[CalendarYear, PSCFishery] <- 0
        }

        if(is.numeric(CISDataReader[31])){
            Reavailability[CalendarYear, PSCFishery] <- CISDataReader[31]
        }else{
            Reavailability[CalendarYear, PSCFishery] <- 0
        }
    
        if(is.numeric(CISDataReader[32])){
            PNV[CalendarYear, PSCFishery, OceanStartAge] <- CISDataReader[32]
        }else{
             PNV[CalendarYear, PSCFishery, OceanStartAge] <- 0
        } 

        if(is.numeric(CISDataReader[33])){
            PNV[CalendarYear, PSCFishery, OceanStartAge + 1] <- CISDataReader[33]
        }else{
             PNV[CalendarYear, PSCFishery, OceanStartAge + 1] <- 0
        } 

        if(is.numeric(CISDataReader[34])){
            PNV[CalendarYear, PSCFishery, OceanStartAge + 2] <- CISDataReader[34]
        }else{
             PNV[CalendarYear, PSCFishery, OceanStartAge + 2] <- 0
        } 

        if(is.numeric(CISDataReader[35])){
            PNV[CalendarYear, PSCFishery, OceanStartAge + 3] <- CISDataReader[35]
        }else{
             PNV[CalendarYear, PSCFishery, OceanStartAge + 3] <- 0
        } 

 
        PV[CalendarYear, PSCFishery, OceanStartAge] = 1 - PNV[CalendarYear, PSCFishery, OceanStartAge]
        PV[CalendarYear, PSCFishery, OceanStartAge + 1] = 1 - PNV[CalendarYear, PSCFishery, OceanStartAge + 1]
        PV[CalendarYear, PSCFishery, OceanStartAge + 2] = 1 - PNV[CalendarYear, PSCFishery, OceanStartAge + 2]
        PV[CalendarYear, PSCFishery, OceanStartAge + 3] = 1 - PNV[CalendarYear, PSCFishery, OceanStartAge + 3]
        if(CalendarYear > LastIMDataYear){LastIMDataYear <- CalendarYear}

    }
    #'Assign Age 5 PNV to Age 6 for stocks that go from age 2 to age 6, necessary because we only have PNVs for four ages
    if(OceanStartAge == 2 & MaxAge == 6){
        #'set to 1960 to speed up the loop
        for(CY in  1960:LastCalendarYear){ 
            for( Fish  in 1:NumberPSCFisheries){
                
                PNV[CY, Fish, MaxAge] = PNV[CY, Fish, OceanStartAge + 3]
                PV[CY, Fish, MaxAge] = 1 - PNV[CY, Fish, MaxAge]

            }
        } 
    } 

    #Read in data from catchability coefficients
    #ERASQL = "SELECT PSCFishery,Age,SizeClass,AvgQ FROM ERA_CatchabilityCoefficients INNER JOIN PSCtoModelFisheryMapping ON ERA_CatchabilityCoefficients.ModelFishery = PSCtoModelFisheryMapping.ModelFishery WHERE ERAStock = '" & CurrentStock & "' AND ShakerMethod = '" & ShakerMethod & "'"
    ERASQL = "SELECT PSCFishery,Age,SizeClass,AvgQ FROM ERA_CatchabilityCoefficients WHERE ERAStock = '" & CurrentStock & "' AND ShakerMethod = '" & ShakerMethod & "'"
    #read from database
    #CISDataReader
    
    if(nrow(CISDataReader)>0) {
        for(i in 1:nrow(CISDataReader)){


            if(!CISDataReader[2] < MaxAge){
                PSCFishery = CISDataReader[1]
            
                if(CISDataReader[3] == "L"){
                    LegalCatchabilityCoeifficient[CISDataReader[2], PSCFishery] <- CISDataReader[4]
                } else{
                    SubLegalCatchabilityCoeifficient[CISDataReader[2], PSCFishery] <- CISDataReader[4]
                }
            } 
        }
    }else{
        
        print(paste("Catchability coefficients for " , CurrentStock , " is missing in the ERA_CatchabilityCoefficients table.  Program is going to stop."))
    }
  
}




