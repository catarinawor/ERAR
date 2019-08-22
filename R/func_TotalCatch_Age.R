
#ERA function TotalCatch_Age()
#Translated from VB ERA CIS code
#January 2019
#Author: Catarina Wor








#' @title TotalCatch_Age
#'
#' @description  This function  aggregates catches for all fisheries to catch at age. 
#'
#' @param D A list with ERA stock specific values
#' @param M A list with ERA settings
#' @param BY integer indicatin brood year
#' @param age integer indicating age
#' @param pass integer indicating pass of ERA.
#'
#' @details
#'
#' @return Tuall mortality for a given brood year and age.
#' 
#' @export
#'
#' @examples
#' 
#' 
TotalCatch_Age <- function(D, M, BY, age){



    # 'return total Mortalities for all ages in a brood year
    if(D$pass == 0){
        #first time through is only landed catch, add legal dropoffs when code is finalized
        TotalCatch_Age <- D$TotalLandedCatch[BY, age]

    }else{

         TotalCatch_Age <- D$TotalLandedCatch[BY, age] + 
                           D$TotalLegalDropoffs[BY, age] + 
                           D$TotalSublegalShakers[BY, age] + 
                           D$TotalSublegalShakerDropoffs[BY, age] + 
                           D$TotalCNRLegal[BY, age] + 
                           D$TotalCNRLegalDropoffs[BY, age] + 
                           D$TotalCNRSubLegal[BY, age] + 
                           D$TotalCNRSubLegalDropoffsl[BY, age]
        if(BY>= M$traceThisYear & age == M$traceThisAge ){

            sink("../logs/debug_totCatAgeID.log", append=TRUE)
            cat(paste("line 2578 totalcatch_age", BY, age, TotalCatch_Age, D$TotalLandedCatch[BY, age], 
                D$TotalLegalDropoffs[BY, age], D$TotalSublegalShakers[BY, age], D$TotalSublegalShakerDropoffs[BY, age],
                D$TotalCNRLegal[BY, age], D$TotalCNRLegalDropoffs[BY, age], D$TotalCNRSubLegal[BY, age],
                D$TotalCNRSubLegalDropoffsl[BY, age], "pass", pass))
            sink()

        }
    }

    return(TotalCatch_Age)

    # Version of the ERA used
    #Function TotalCatch_Age(ByRef BroodYear As Integer, ByRef Age As Integer, ByRef pass As Integer) As Single
    #    'return total Mortalities for all ages in a brood year
    #    If pass = 0 Then 'first time through is only landed catch, add legal dropoffs when code is finalized
    #     TotalCatch_Age = TotalLandedCatch(BroodYear, Age) '+ TotalLegalDropoffs(BroodYear, Age)
    #    'If BroodYear>= traceThisYear Then WriteLine(debug_CohortID, "line 2575 totalcatch_age", BroodYear, Age, TotalCatch_Age, TotalLandedCatch(BroodYear, Age))
    #    Else
    #        TotalCatch_Age = TotalLandedCatch(BroodYear, Age) + TotalLegalDropoffs(BroodYear, Age) + TotalSublegalShakers(BroodYear, Age) + TotalSublegalShakerDropoffs(BroodYear, Age) + TotalCNRLegal(BroodYear, Age) + TotalCNRLegalDropoffs(BroodYear, Age) + TotalCNRSubLegal(BroodYear, Age) + TotalCNRSubLegalDropoffs(BroodYear, Age)
    #        If BroodYear >= traceThisYear And Age = traceThisAge Then WriteLine(debug_totCatAgeID, "line 2578 totalcatch_age", BroodYear, Age, TotalCatch_Age, TotalLandedCatch(BroodYear, Age), TotalLegalDropoffs(BroodYear, Age), TotalSublegalShakers(BroodYear, Age), TotalSublegalShakerDropoffs(BroodYear, Age), TotalCNRLegal(BroodYear, Age), TotalCNRLegalDropoffs(BroodYear, Age), TotalCNRSubLegal(BroodYear, Age), TotalCNRSubLegalDropoffs(BroodYear, Age), "pass", pass)
    #    End If
    #End Function


}









#' @title TotalTerminalCatch_Age
#'
#' @description  This function  aggregates catches for all fisheries to catch at age. 
#'
#' @param D A list with ERA stock specific values
#' @param M A list with ERA settings
#' @param BY integer indicatin brood year
#' @param age integer indicating age
#' @param pass integer indicating pass of ERA.
#'
#' @details
#'
#' @return Tuall mortality for a given brood year and age.
#' 
#' @export
#'
#' @examples
#' 
#' 
TotalTerminalCatch_Age <- function(D, M, BY, age){



    # 'return total Mortalities for all ages in a brood year
    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod){

        sink("../logs/debug_terminalCatchID.log", append=TRUE)
        cat(paste("3149 TotalTerminalCatch_Age before summation",
            M$ShakerMethod, BY, age, "sum all ages",TotalTerminalCatch_Age ))
        sink()
    }

    if(D$pass == 0){
        #'first time through is only landed catch, add legal dropoffs when code is finalized
        TotalTerminalCatch_Age <- D$TotalTerminalLandedCatch[BY,age]
    }else{
        TotalTerminalCatch_Age <- D$TotalTerminalLandedCatch[BY,age] + 
                                  D$TotalTerminalLegalDropoffs[BY,age] +
                                  D$TotalTerminalShakers[BY,age] +
                                  D$TotalTerminalShakerDropoffs[BY,age] +
                                  D$TotalTerminalCNRLegal[BY,age] +
                                  D$TotalTerminalCNRLegalDropoffs[BY,age] +
                                  D$TotalTerminalCNRSubLegal[BY,age] +
                                  D$TotalTerminalCNRSubLegalDropoffs[BY,age]

    }
    if(M$isTraceCalc & M$ShakerMethod == M$traceThisShakerMethod){
        sink("../logs/debug_terminalCatchID.log", append=TRUE)
        cat(paste("3150 TotalTerminalCatch_Age after summation",
            M$ShakerMethod, BY, age, "sum all ages", TotalTerminalCatch_Age, "age specific",
            D$TotalTerminalLandedCatch[BY,age], "pass", pass, 
            D$TotalTerminalLegalDropoffs[BY,age] + D$TotalTerminalShakers[BY,age] + D$TotalTerminalShakerDropoffs[BY,age],
            D$TotalTerminalLegalDropoffs[BY,age], D$TotalTerminalShakers[BY,age], D$TotalTerminalShakerDropoffs[BY,age], 
            D$TotalTerminalCNRLegal[BY,age], D$TotalTerminalCNRLegalDropoffs[BY,age],  
            D$TotalTerminalCNRSubLegal[BY,age] +  D$TotalTerminalCNRSubLegalDropoffs[BY,age]))
        sink()
    }

    return(TotalTerminalCatch_Age)

    # Version of the ERA used
    #Function TotalTerminalCatch_Age(ByRef BroodYear As Integer, ByRef Age As Integer, ByRef pass As Integer) As Single
    #   'return total terminal mortalities for all ages in a brood year
    #   If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And ShakerMethod = traceThisShakerMethod Then
    #       WriteLine(debug_terminalCatchID, "3149 TotalTerminalCatch_Age before summation", ShakerMethod, BroodYear, Age, "sum all ages", TotalTerminalCatch_Age)
    #   End If
    #   If pass = 0 Then 'first time through is only landed catch, add legal dropoffs when code is finalized
    #       TotalTerminalCatch_Age = TotalTerminalLandedCatch(BroodYear, Age) '+ TotalTerminalLegalDropoffs(BroodYear, Age)
    #   Else
    #       TotalTerminalCatch_Age = TotalTerminalLandedCatch(BroodYear, Age) + TotalTerminalLegalDropoffs(BroodYear, Age) + TotalTerminalShakers(BroodYear, Age) + TotalTerminalShakerDropoffs(BroodYear, Age) + TotalTerminalCNRLegal(BroodYear, Age) + TotalTerminalCNRLegalDropoffs(BroodYear, Age) + TotalTerminalCNRSubLegal(BroodYear, Age) + TotalTerminalCNRSubLegalDropoffs(BroodYear, Age)
    #   End If
    #   If isTraceCalc = True And ShakerMethod = traceThisShakerMethod And ShakerMethod = traceThisShakerMethod Then
    #       WriteLine(debug_terminalCatchID, "3150 TotalTerminalCatch_Age after summation", ShakerMethod, BroodYear, Age, "sum all ages", TotalTerminalCatch_Age, "age specific", TotalTerminalLandedCatch(BroodYear, Age), "pass", pass, TotalTerminalLegalDropoffs(BroodYear, Age) + TotalTerminalShakers(BroodYear, Age) + TotalTerminalShakerDropoffs(BroodYear, Age), TotalTerminalLegalDropoffs(BroodYear, Age), TotalTerminalShakers(BroodYear, Age), TotalTerminalShakerDropoffs(BroodYear, Age), TotalTerminalCNRLegal(BroodYear, Age), TotalTerminalCNRLegalDropoffs(BroodYear, Age), TotalTerminalCNRSubLegal(BroodYear, Age) + TotalTerminalCNRSubLegalDropoffs(BroodYear, Age))
    #   End If
    #End Function


}


