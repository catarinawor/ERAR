#=======================================================
#ERA function CheckIMData
#Translated from VB ERA CIS code
#February 2019
#Author: Catarina Wor
#=======================================================






#' @title CheckIMData
#'
#' @description   
#'  
#' @param M A list passed to StartCohortAnalysis_Click
#' 
#' @param D  A list containing stock specific info
#'
#' @details 
#'
#' @return 
#' 
#' @export
#'
#' @examples
#' 
#' 
CheckIMData <- function(D,M){

	allBY <- D$FirstBY:D$LastBY
    allCY <- (D$FirstBY + D$OceanStartAge):M$LastCalendarYear
    allAge <- D$OceanStartAge:D$MaxAge

	for(BYind in 1:length(allBY)){
		for(PSCFishery in 1:M$NumberPSCFisheries){
			if(M$PSCFisheryName[PSCFishery]=="ESCAPEMENT" | M$PSCFisheryName[PSCFishery] == "XCA ESC STRAY" | M$PSCFisheryName[PSCFishery] == "XUS ESC STRAY" ){
				next
			}
			
			for(age in D$OceanStartAge:LastAge[BYind]){
				CalYr <- allBY[BYind] + age

				if(length(D$IMdf$CNRMethod[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery])>0){
					if(D$IMdf$CNRMethod[D$IMdf$CalendarYear==CalYr&D$IMdf$PSCFishery==PSCFishery]==0){
						sink("../logs/debug_CNRMethod.log", append=TRUE)
                		cat(paste("You need to add data to the ERA_IMInputs table for all fisheries in year",
                		CalYr , "before program will work.  Else if NO CNR then CNRMethod 0 should be 9. ERROR \n"))
                		sink() 
                	 	imd<-list(IMerror=1)
					}else{
						#Necessary IM data is loaded, do nothing
						imd<-NULL
					}
				}
			}
		}
	}

	return(imd)

	#    'check to see if IM data is loaded for all calendar years needed for the analysis
	#    Dim CalYr As Integer
	#    For BroodYear As Integer = FirstBY To LastBY
	#      For PSCFishery As Integer = 1 To NumberPSCFisheries
	#        If PSCFisheryName(PSCFishery) = "ESCAPEMENT" Or PSCFisheryName(PSCFishery) = "XCA ESC STRAY" Or PSCFisheryName(PSCFishery) = "XUS ESC STRAY" Then GoTo SkipFishery
	#        For age As Integer = OceanStartAge To LastAge(BroodYear)
	#          CalYr = BroodYear + age
	#          Select Case CNRMethod(CalYr, PSCFishery)
	#            Case 0
	#              MsgBox("You need to add data to the ERA_IMInputs table for all fisheries in year " & CalYr & " before program will work.  Else if NO CNR then CNRMethod 0 should be 9.", , "ERROR")
	#              End
	#            Case Else
	#              'Necessary IM data is loaded, do nothing
	#          End Select
	#        Next age
	#SkipFishery:
	#      Next PSCFishery
	#    Next BroodYear

}
