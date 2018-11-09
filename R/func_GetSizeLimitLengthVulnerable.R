#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


#' @title GetSizeLimitLengthVulnerable
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
GetSizeLimitLengthVulnerable <- function(){

    #'get minimum vulnerable lengths and size limits for PSC fisheries
    MinSizeLimit<- matrix(NA,nrow=LastCalendarYear, ncol=NumberPSCFisheries)
    MaxSizeLimit<- matrix(NA,nrow=LastCalendarYear, ncol= NumberPSCFisheries)
    MinSizeVulnerable<- matrix(NA,nrow=LastCalendarYear, ncol=NumberPSCFisheries)
    SizeLimitType <-matrix(NA,nrow=LastCalendarYear, ncol=NumberPSCFisheries)
    ERASQL = "SELECT CalendarYear, PSCFishery, MinSizeVulnerable, MinSizeLimit, MaxSizeLimit FROM ERA_IMInputs WHERE TimePeriod ='" & TimePeriod & "'"
    #read from database
    
    for (i in 1:length(CISDataReader) ){
        CY <- CISDataReader[1]
        PSCFishery <-  CISDataReader[2]
        if(is.na(CISDataReader[3])){
            MinSizeVulnerable[CY, PSCFishery] <- 0
        }else{
            MinSizeVulnerable[CY, PSCFishery] <- CISDataReader[3]
        }

        if(is.na(CISDataReader[4])){
            MinSizeLimit[CY, PSCFishery] <- 0
        }else{
            MinSizeLimit[CY, PSCFishery] <- CISDataReader[4]
        }

        if(is.na(CISDataReader[5])){
            #note: if maxsizelimir= 0 no fish are caught
            MaxSizeLimit[CY, PSCFishery] <- 0
        }else{
            MaxSizeLimit[CY, PSCFishery] <- CISDataReader[5]
        }

        if(MinSizeLimit[CY, PSCFishery] > 0 & MaxSizeLimit[CY, PSCFishery] > 0){
             SizeLimitType[CY, PSCFishery] = "SLOT"
        } else if(MinSizeLimit[CY, PSCFishery] > 0 & MaxSizeLimit[CY, PSCFishery] == 0){
            SizeLimitType[CY, PSCFishery] = "MINIMUM"
        }else if(MinSizeLimit[CY, PSCFishery] == 0&MaxSizeLimit[CY, PSCFishery] > 0){
            SizeLimitType[CY, PSCFishery] = "MAXIMUM"
        }else{
            SizeLimitType[CY, PSCFishery] = "NONE"
        }
              
          
    }
        
  
}




