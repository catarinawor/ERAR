
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor


#source(utils.R)


#CreatePNV <- function(M){

    #--------------------------------------------------------------
    #  Purpose:  Calculates stock,age,fishery,year specific PNV values using ND function and input values.
    #  Externals: Function ND()
    #--------------------------------------------------------------
    
    #PNV<-array(NA,dim=c(LastCalendarYear, NumberPSCFisheries, MaxAge))
    #PV<-array(NA,dim=c(LastCalendarYear, NumberPSCFisheries, MaxAge))
    
    #Dim age, PSCfishery As Integer
     
    #for (CalYr in (FirstBY + OceanStartAge):LastCalendarYear) {
    #    for (age in OceanStartAge:MaxAge) {
    #        for (PSCfishery in 1:NumberPSCFisheries) {
    #            switch(SizeLimitType[CalYr, PSCfishery],
    #                "SLOT" = {
    #                            PNV[CalYr, PSCfishery, age] = pnorm(MinSizeLimit[CalYr, PSCfishery], meanLength[age, CalYr], lengthstDev[age, CalYr]) - pnorm(MinSizeVulnerable[CalYr, PSCfishery], meanLength[age, CalYr], lengthstDev[age, CalYr])) + (1 - ND(MaxSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr)))
    #                            PV[CalYr, PSCfishery, age] = ND(MaxSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr)) - ND(MinSizeLimit(CalYr, PSCfishery), meanLength(age, CalYr), lengthstDev(age, CalYr))
    #                        },
    #                )
    #        }
    #    }
    #}
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
    #End Sub
   ##     
  
#}




