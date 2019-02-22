
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor


#source(utils.R)



#' @title CreatePNV
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
#' @return  A list containing the internally calculated PNV and PV
#' 

#'
#' @examples
#' 
#' 
CreatePNV <- function(M,D){

    #--------------------------------------------------------------
    #  Purpose:  Calculates stock,age,fishery,year specific PNV values using ND function and input values.
    #  Externals: Function ND()
    #--------------------------------------------------------------
    
    PV <- list()
    PNV <- list()

    stk_age<-D$OceanStartAge:D$MaxAge

    for(age in 1:(length(stk_age))){

        #tmpPNV<-matrix(NA, ncol=ncol(D$PNV[[1]]),nrow=nrow(D$PNV[[1]]))
        #tmpPV<-matrix(NA, ncol=ncol(D$PV[[1]]),nrow=nrow(D$PV[[1]]))
        nCalYr<-length(unique(D$IMdf$CalendarYear))
        nPSCFisheries<-length(unique(D$IMdf$PSCFishery))

        CalYrs<-(unique(D$IMdf$CalendarYear))
        PSCFisheries<-(unique(D$IMdf$PSCFishery))

        tmpPNV<-matrix(NA, ncol= nPSCFisheries+1,nrow=nCalYr)
        tmpPV<-matrix(NA, ncol= nPSCFisheries+1,nrow=nCalYr)


        for(CalYr in 1:nCalYr){

            tmpPNV[CalYr,1]<-unique(D$IMdf$CalendarYear)[CalYr]
             tmpPV[CalYr,1]<-unique(D$IMdf$CalendarYear)[CalYr]

            for(PSCfishery in 1:nPSCFisheries+1){

                head(SizeLimitLengthVulnerabledf)
                head(MeanLengthdf)

                minSize_tmp<-D$SizeLimitLengthVulnerabledf$MinSizeLimit[D$SizeLimitLengthVulnerabledf$CalendarYear== CalYrs[CalYr]&
                                                                        D$SizeLimitLengthVulnerabledf$PSCFishery==PSCfisheries[PSCfishery]]
                maxSize_tmp<-D$SizeLimitLengthVulnerabledf$MaxSizeLimit[D$SizeLimitLengthVulnerabledf$CalendarYear== CalYrs[CalYr]&
                                                                        D$SizeLimitLengthVulnerabledf$PSCFishery==PSCfisheries[PSCfishery]]

                minSizeVul_tmp<-D$SizeLimitLengthVulnerabledf$MinSizeVulnerable[D$SizeLimitLengthVulnerabledf$CalendarYear== CalYrs[CalYr]&
                                                                        D$SizeLimitLengthVulnerabledf$PSCFishery==PSCfisheries[PSCfishery]]


                meanSize_tmp<-D$MeanLengthdf$MeanLength[D$MeanLengthdf$CalendarYear== CalYrs[CalYr]&
                                                        D$MeanLengthdf$Age==stk_age[age]]

                sdSize_tmp<-D$MeanLengthdf$StandardDeviation[D$MeanLengthdf$CalendarYear== CalYrs[CalYr]&
                                                        D$MeanLengthdf$Age==stk_age[age]]

                SizeLimitType_tmp <-  D$SizeLimitLengthVulnerabledf$SizeLimitType[D$SizeLimitLengthVulnerabledf$CalendarYear== CalYrs[CalYr]&
                                                                D$SizeLimitLengthVulnerabledf$PSCFishery==PSCFisheries[PSCfishery]]       

            #SizeLimitLengthVulnerabledf$MinSizeLimit[SizeLimitLengthVulnerabledf$CalendarYear== CalYrs[CalYr]&
            #                                                        SizeLimitLengthVulnerabledf$PSCFishery==PSCFisheries[PSCfishery]]
            #  SizeLimitLengthVulnerabledf$MaxSizeLimit[D$SizeLimitLengthVulnerabledf$CalendarYear== CalYrs[CalYr]&
            #                                                            D$SizeLimitLengthVulnerabledf$PSCFishery==PSCFisheries[PSCfishery]]

            #    meanSize_tmp<-MeanLengthdf$MeanLength[D$MeanLengthdf$CalendarYear== CalYrs[CalYr]&
            #                                            D$MeanLengthdf$Age==stk_age[age]]       ]

            #    SizeLimitLengthVulnerabledf$SizeLimitType[SizeLimitLengthVulnerabledf$CalendarYear== CalYrs[CalYr]&
            #                                                    SizeLimitLengthVulnerabledf$PSCFishery==PSCFisheries[PSCfishery]]


                if(SizeLimitType_tmp=="SLOT"){
                
                    tmpPNV[CalYr,PSCfishery] <- pnorm(minSize_tmp, mean = meanSize_tmp, sd=sdSize_tmp)-
                                            pnorm( minSizeVul_tmp, mean = meanSize_tmp, sd=sdSize_tmp) + 
                                            (1 - pnorm( maxSize_tmp, mean = meanSize_tmp, sd=sdSize_tmp))
                                    
                    tmpPV[CalYr, PSCfishery] <- pnorm(maxSize_tmp, mean= meanSize_tmp, sd=sdSize_tmp) - 
                                                     pnorm(minSize_tmp, mean=meanSize_tmp, sd=sdSize_tmp)
                
                }else if(SizeLimitType_tmp=="MINIMUM"){
                    
                    tmpPNV[CalYr,PSCfishery] <- pnorm(minSize_tmp, mean=meanSize_tmp, sd=sdSize_tmp) -
                                             pnorm(minSizeVul_tmp, mean=meanSize_tmp, sd=sdSize_tmp)
                    tmpPV[CalYr,PSCfishery] <- 1 - (pnorm(minSize_tmp, mean=meanSize_tmp, sd=sdSize_tmp))                      

                }else if(SizeLimitType_tmp=="MAXIMUM"){
                   
                    tmpPNV[CalYr, PSCfishery] <- 1 - pnorm(maxSize_tmp, mean=meanSize_tmp, sd=sdSize_tmp)
                    tmpPV[CalYr, PSCfishery] <- pnorm(maxSize_tmp, mean=meanSize_tmp, sd=sdSize_tmp)

                }else if(SizeLimitType_tmp=="NONE"){
                        sink("../logs/CreatePNV.log")
                        cat(paste("There is no data to compute an age ", (D$OceanStartAge:D$MaxAge)[age] , "PNV for Fishery " , unique(D$IM_PSCFishery)[PSCfishery] , " in ", unique(D$IM_CalendarYear)[CalYr]),"\n")
                        sink()
                }

            }
        }
        PNV<-c(PNV,tmpPNV)
        PV<-c(PNV,tmpPV)

    }



    return(list(PNV=PNV,PV=PV))




    #original VB code
    #'--------------------------------------------------------------
    #    '  Purpose:  Calculates stock,age,fishery,year specific PNV values using ND function and input values.
    #    '  Externals: Function ND()
    #    '--------------------------------------------------------------
    #    ReDim PNV(LastCalendarYear, NumberPSCFisheries, MaxAge)
    #    ReDim PV(LastCalendarYear, NumberPSCFisheries, MaxAge)
    #    Dim age, PSCfishery As Integer
    #    For CalYr As Integer = FirstBY + OceanStartAge To LastCalendarYear
    #        For age = OceanStartAge To MaxAge
    #            For PSCfishery = 1 To NumberPSCFisheries
    #                Select Case SizeLimitType(CalYr, PSCfishery)
    #                    Case "SLOT"
    #                        PNV(CalYr, PSCfishery, age) = (ND(MinSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr)) - ND(MinSizeVulnerable(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr))) + (1 - ND(MaxSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr)))
    #                        PV(CalYr, PSCfishery, age) = ND(MaxSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr)) - ND(MinSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr))
    #                    Case "MINIMUM"
    #                        PNV(CalYr, PSCfishery, age) = ND(MinSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr)) - ND(MinSizeVulnerable(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr))
    #                        PV(CalYr, PSCfishery, age) = 1 - ND(MinSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr))
    #                    Case "MAXIMUM"
    #                        PNV(CalYr, PSCfishery, age) = 1 - ND(MaxSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr))
    #                        PV(CalYr, PSCfishery, age) = ND(MaxSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr))
    #                    Case "NONE"
    #                        MsgBox("There is no data to compute an age " & age & " PNV for Fishery " & PSCfishery & " in " & CalYr)
    #                End Select
    #                If PNV(CalYr, PSCfishery, age) < 0 Then
    #                    PNV(CalYr, PSCfishery, age) = 0
    #                End If
    #            Next PSCfishery
    #        Next age
    #    Next CalYr
    #    'print PNV values to log file
    #    intFileNum1 = FreeFile()
    #    FileOpen(intFileNum1, "PNVvalues.log", OpenMode.Output)
    #    Print(intFileNum1, "Year,", "PSCfishery,", "Stock,", "Limit Type,", "MinLen,", "MaxLen,", "MinVulnLen,")
    #    For age = OceanStartAge To MaxAge
    #        Print(intFileNum1, "Age " & age & " MeanLen,")
    #    Next age
    #    For age = OceanStartAge To MaxAge
    #        Print(intFileNum1, "Age " & age & " SD,")
    #    Next age
    #    For age = OceanStartAge To MaxAge
    #        If age < MaxAge Then
    #            Print(intFileNum1, "Age " & age & " PNV,")
    #        Else
    #            Print(intFileNum1, "Age " & age & " PNV")
    #        End If
    #    Next age
    #    PrintLine(intFileNum1)
    #    For CalYr As Integer = FirstBY + OceanStartAge To LastCalendarYear
    #        For PSCfishery = 1 To NumberPSCFisheries
    #            Print(intFileNum1, CalYr + 1900, ",", PSCfishery, ",", SizeLimitType(CalYr, PSCfishery), ",", MinSizeLimit(CalYr, PSCfishery), ",", MaxSizeLimit(CalYr, PSCfishery), ",", MinSizeVulnerable(CalYr, PSCfishery), ",")
    #            For age = OceanStartAge To MaxAge
    #                Print(intFileNum1, meanLength(age, CalYr), ",")
    #            Next age
    #            For age = OceanStartAge To MaxAge
    #                Print(intFileNum1, lengthstDev(age, CalYr), ",")
    #            Next age
    #            For age = OceanStartAge To MaxAge
    #                If age < MaxAge Then
    #                    Print(intFileNum1, PNV(CalYr, PSCfishery, age), ",")
    #                Else
    #                    Print(intFileNum1, PNV(CalYr, PSCfishery, age))
    #                End If
    #            Next age
    #            PrintLine(intFileNum1)
    #        Next PSCfishery
    #    Next CalYr
    #    FileClose(intFileNum1)
   ##     
  
}




