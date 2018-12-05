
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor


#source(utils.R)



#' @title SetTerminalFishery
#'
#' @description Populates a matrix that is PSCfishe ry x Age, with flags indicating if fish is caught at a terminal fishery
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
SetTerminalFishery <- function(D,M){

    dta <- RODBC::odbcConnectAccess2007(M$datbse)         

    ERASQL = "SELECT * FROM ERA_PSCFishery WHERE PSCTerminal=Yes"

    df2<- sqlQuery( dta , query = ERASQL )

    terminal<-matrix(FALSE,nrow=length(unique(D$IM_PSCFishery)),ncol=length(D$OceanStartAge:D$MaxAge))

     PSCFisheryNumber<-df2[,1]
     terminal[PSCFisheryNumber,]<-TRUE


     terminal[which(M$PSCFisheryGear=="NET"),D$TermNetSwitchAge:D$MaxAge]<-TRUE

     return(terminal)

   

    #original VB code
    #=============================================================
    #'Set Terminal flag for PSCFisheries
    #    ERASQL = "SELECT * FROM ERA_PSCFishery WHERE PSCTerminal=Yes"
    #    ERACommand = New OleDbCommand(ERASQL, CISDBConnection)
    #    CISDataReader = ERACommand.ExecuteReader()
    #    Do While CISDataReader.Read()
    #        Dim PSCFisheryNumber As Integer
    #        PSCFisheryNumber = CISDataReader(0)
    #        For age As Integer = OceanStartAge To MaxAge
    #            terminal(PSCFisheryNumber, age) = True
    #        Next
    #    Loop
    #    CISDataReader.Close()
    #    'set terminal flags for older ages in net fisheries
    #    For PSCFishery As Integer = 1 To NumberPSCFisheries
    #        If PSCFisheryGear(PSCFishery) = "NET" Then
    #            For age As Integer = TermNetSwitchAge To MaxAge
    #                terminal(PSCFishery, age) = True
    #            Next age
    #        End If
    #        If isTraceCalc = True And ShakerMethod = traceThisShakerMethod Then WriteLine(debug_terminalFlagID, "1166 isterminal", PSCFishery, PSCFisheryName(PSCFishery), terminal(PSCFishery, OceanStartAge), terminal(PSCFishery, OceanStartAge + 1), terminal(PSCFishery, OceanStartAge + 2), terminal(PSCFishery, OceanStartAge + 3))
    #    Next PSCFishery
  
}




