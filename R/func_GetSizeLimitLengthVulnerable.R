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
#' @param D A list that includes the output of GetMeanLength
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
GetSizeLimitLengthVulnerable <- function(D,M){


    dta <- RODBC::odbcConnectAccess2007(M$datbse)         

    ERASQL <-paste0("SELECT CalendarYear, PSCFishery, MinSizeVulnerable, MinSizeLimit, MaxSizeLimit FROM ERA_IMInputs WHERE TimePeriod ='", D$TimePeriod, "'")

    df1 <- sqlQuery( dta , query = ERASQL )

    head(df1)

    CY <-df1$CalendarYear 
    PSCFishery <- df1$PSCFishery
    
      

    df1$MinSizeVulnerable[is.na(df1$MinSizeVulnerable)] <- 0
    MinSizeVulnerable <-  tidyr::spread(df1[,c("MinSizeVulnerable","CalendarYear","PSCFishery")],key=PSCFishery,value= MinSizeVulnerable )

    df1$MinSizeLimit[is.na(df1$MinSizeLimit)] <- 0
    MinSizeLimit <-  tidyr::spread(df1[,c("MinSizeLimit","CalendarYear","PSCFishery")],key=PSCFishery,value=MinSizeLimit)

    df1$MaxSizeLimit[is.na(df1$MaxSizeLimit)] <- 0
    MaxSizeLimit <-  tidyr::spread(df1[,c("MaxSizeLimit","CalendarYear","PSCFishery")],key=PSCFishery,value= MaxSizeLimit )


    df1$SizeLimitType<-rep("NONE",nrow(df1))
    df1$SizeLimitType[df1$MinSizeLimit>0&df1$MaxSizeLimit>0]<-"SLOT"
    df1$SizeLimitType[df1$MinSizeLimit>0&df1$MaxSizeLimit==0]<-"MINIMUM"
    df1$SizeLimitType[df1$MinSizeLimit>0&df1$MaxSizeLimit==0]<-"MAXIMUM"

    SizeLimitType <-  tidyr::spread(df1[,c("SizeLimitType","CalendarYear","PSCFishery")],key=PSCFishery,value=SizeLimitType )


    return(list(SizeLimitCY=CY,
        SizeLimitPSCFishery=PSCFishery,
        MinSizeVulnerable=MinSizeVulnerable,
        MinSizeLimit=MinSizeLimit,
        MaxSizeLimit=MaxSizeLimit,
        SizeLimitType=SizeLimitType))
    

    #original VB code
    #=========================================================================
    # 'get minimum vulnerable lengths and size limits for PSC fisheries
    #    ReDim MinSizeLimit(LastCalendarYear, NumberPSCFisheries)
    #    ReDim MaxSizeLimit(LastCalendarYear, NumberPSCFisheries)
    #    ReDim MinSizeVulnerable(LastCalendarYear, NumberPSCFisheries)
    #    ReDim SizeLimitType(LastCalendarYear, NumberPSCFisheries)
    #    ERASQL = "SELECT CalendarYear, PSCFishery, MinSizeVulnerable, MinSizeLimit, MaxSizeLimit FROM ERA_IMInputs WHERE TimePeriod ='" & TimePeriod & "'"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    Do While CISDataReader.Read()
    #        Dim CY As Integer = CISDataReader(0)
    #        Dim PSCFishery As Integer = CISDataReader(1)
    #        MinSizeVulnerable(CY, PSCFishery) = If(IsDBNull(CISDataReader(2)), 0, CISDataReader(2))
    #        MinSizeLimit(CY, PSCFishery) = If(IsDBNull(CISDataReader(3)), 0, CISDataReader(3))
    #        MaxSizeLimit(CY, PSCFishery) = If(IsDBNull(CISDataReader(4)), 0, CISDataReader(4))
    #        If MinSizeLimit(CY, PSCFishery) > 0 And MaxSizeLimit(CY, PSCFishery) > 0 Then
    #            SizeLimitType(CY, PSCFishery) = "SLOT"
    #        ElseIf MinSizeLimit(CY, PSCFishery) > 0 And MaxSizeLimit(CY, PSCFishery) = 0 Then
    #            SizeLimitType(CY, PSCFishery) = "MINIMUM"
    #        ElseIf MinSizeLimit(CY, PSCFishery) = 0 And MaxSizeLimit(CY, PSCFishery) > 0 Then
    #            SizeLimitType(CY, PSCFishery) = "MAXIMUM"
    #        Else
    #            SizeLimitType(CY, PSCFishery) = "NONE"
    #        End If
    #    Loop
    #    CISDataReader.Close()
        
  
}




