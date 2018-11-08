#=======================================================
#ERA function GetFirstAndLastBY()
#Translated from VB ERA CIS code
#October 2018
#Author: Catarina Wor
#=======================================================

#=======================================================


#source(utils.R)


RedimensionArrays <- function(){

	#Redimension variables with correct number of BroodYear,Age, and Fishery dimensions

	AverageMatRate <- numeric(length = MaxAge)
 	AverageAEQ <- numeric(length = MaxAge)
    CompleteBYFlag <- logical(length = LastBY)
 	MissingBroodYearFlag <-logical(length = LastBY)
 	CWTRelease <- numeric(length = LastBY)
 	TotalRelease <- numeric(length = LastBY)
 	RelRatio <- numeric(length=LastBY)
 	Escape <- matrix(NA,nrow = LastBY, ncol=MaxAge)
 	EscStray_CA <- matrix(NA, nrow = LastBY, ncol = MaxAge)
 	EscStray_US <- matrix(NA,nrow = LastBY, ncol =MaxAge)
 	TotalMortalities <- array(NA,dim = c(NumberPSCFisheries, MaxAge, LastBY))
	TotalMortalityHarvestRate <- array(NA, dim = c(NumberPSCFisheries, MaxAge, LastBY))
 	CohortAfterNaturalMortality <- matrix(NA, nrow=LastBY, ncol=MaxAge)

 	TerminalRun <- matrix(NA,nrow = LastBY, ncol = MaxAge)
 	terminal <- matrix(NA, nrow = NumberPSCFisheries, ncol = MaxAge)
	SurvivalRate <- numeric(length = MaxAge + 1)
  	LastAge <- numeric(length = LastBY)
 	Cohort <- matrix(NA, nrow = LastBY, ncol = MaxAge + 1)
	MatRate <- matrix(NA, nrow = LastBY,ncol = MaxAge)
 	AEQ <- matrix(NA, nrow = LastBY, ncol = MaxAge + 1)
 	#landed catch variables
 	if(!isReplicateCohShak){
 		LandedCatch <- array(NA, dim = c(NumberPSCFisheries, 7, LastBY))
 	}else{
 		LandedCatch <- array(NA, dim = c(NumberPSCFisheries, 7, LastBY))
 	}
 	LegalDropoffMortality <-array(NA, dim = c(NumberPSCFisheries, 7, LastBY))
    TotalTerminalLandedCatch <- matrix(NA, nrow = LastBY, ncol = MaxAge)
 	TotalLandedCatch <- matrix(NA, nrow = LastBY, ncol = MaxAge)
 	TotalLandedCatch_ByFishery <- matrix(NA, nrow = LastBY, ncol =NumberPSCFisheries)
 	LandedCatchHarvestRate <- array(NA, dim = c(NumberPSCFisheries, MaxAge, LastBY))
 	AEQPreTermLandedCatch <- matrix(NA, nrow = LastBY, ncol= MaxAge)
 	AEQLandedCatchPreTermER <- matrix(NA, nrow = LastBY, ncol = MaxAge)
	LandedCatchTermER  <- matrix(NA, nrow = LastBY, ncol = MaxAge)
	AEQLandedCatchTotER <- matrix(NA, nrow = LastBY, ncol = MaxAge)
	TotalMortTermER <- matrix(NA, nrow = LastBY, ncol = MaxAge)
 	AEQTotalMortTotER <- matrix(NA, nrow = LastBY, ncol = MaxAge)
 	AEQTotalMortPreTermER <- matrix(NA, nrow= LastBY, ncol= MaxAge)
	PreTermIncidentalMortalities <- matrix(NA, nrow = LastBY, ncol = MaxAge)
 	AEQPreTermTotalMorts <- matrix(NA, nrow= LastBY, ncol = MaxAge)
 	AEQLandedCatchTotalRun <- numeric(length = LastBY)
	AEQTotalMortTotalRun <- numeric(length = LastBY)
	LandedCatchTerminalRun <- numeric(length = LastBY)
 	TotalMortTerminalRun <- numeric(length = LastBY)
 	AEQPreTermLandedCatchAllAges <- numeric(length = LastBY)
 	AEQPreTermTotalMortsAllAges <- numeric(length = LastBY)
 	TerminalLandedCatchAllAges <- numeric(length = LastBY)
	TerminalTotalMortsAllAges <- numeric(length = LastBY)
 	AEQLandedCatchAllAgesPreTermER <- numeric(length = LastBY)
 	AEQTotalMortAllAgesPreTermER <- numeric(length = LastBY)
 	LandedCatchAllAgesTerminalER <- numeric(length = LastBY)
 	TotalMortAllAgesTerminalER <- numeric(length = LastBY)
 	AEQLandedCatchAllAgesTotalER <- numeric(length = LastBY)
 	AEQTotalMortsAllAgesTotalER <- numeric(length = LastBY)
 	#shaker variables
 	TotalTerminalLegalDropoffs <- matrix(NA, nrow=LastBY, ncol=MaxAge)
 	TotalTerminalShakers <- matrix(NA, nrow=LastBY, ncol=MaxAge)
 	TotalTerminalShakerDropoffs <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalLegalDropoffs <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalSublegalShakerDropoffs<- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalSublegalShakers<- matrix(NA, nrow=LastBY, ncol=MaxAge)
 	#cnr variables
 	TotalCNR<- matrix(NA, nrow = LastBY, ncol=MaxAge)
 	TotalTerminalCNR <- matrix(NA, nrow=LastBY, ncol=MaxAge)
 	LegalCNRMortality <- array(NA, dim = c(NumberPSCFisheries, MaxAge, LastBY))
 	SubLegalCNRMortality <- array(NA,dim = c(NumberPSCFisheries, MaxAge, LastBY))
	LegalCNRDropoffs <- array(NA, dim = c(NumberPSCFisheries, MaxAge, LastBY))
	SubLegalCNRDropoffs <- array(NA, dim = c(NumberPSCFisheries, MaxAge, LastBY))
 	TotalCNRLegal <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalCNRLegalDropoffs <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalCNRSubLegal <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalCNRSubLegalDropoffs <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalTerminalCNRLegal <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalTerminalCNRLegalDropoffs <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalTerminalCNRSubLegal <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	TotalTerminalCNRSubLegalDropoffs <- matrix(NA, nrow=LastBY, ncol=MaxAge)
	ExtraLegalCNRMortality <- array(NA, dim=c(NumberPSCFisheries, MaxAge, LastBY))
	ExtraLegalCNRDropoffs <- array(NA,dim=c(NumberPSCFisheries, MaxAge, LastBY))
 	AdultInterDamSurvivalRate <- numeric(LastBY + MaxAge)
 	JackInterDamSurvivalRate <- numeric(LastBY + MaxAge)

 #Sub RedimensionArrays()
 #       'Redimension variables with correct number of BroodYear,Age, and Fishery dimensions
 #       ReDim AverageMatRate(MaxAge)
 #       ReDim AverageAEQ(MaxAge)
 #       ReDim CompleteBYFlag(LastBY)
 #       ReDim MissingBroodYearFlag(LastBY)
 #       ReDim CWTRelease(LastBY)
 #       ReDim TotalRelease(LastBY)
 #       ReDim RelRatio(LastBY)
 #       ReDim Escape(LastBY, MaxAge)
 #       ReDim EscStray_CA(LastBY, MaxAge)
 #       ReDim EscStray_US(LastBY, MaxAge)
 #       ReDim TotalMortalities(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim TotalMortalityHarvestRate(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim CohortAfterNaturalMortality(LastBY, MaxAge)
 #       ReDim TerminalRun(LastBY, MaxAge)
 #       ReDim terminal(NumberPSCFisheries, MaxAge)
 #       ReDim SurvivalRate(MaxAge + 1)
 #       ReDim LastAge(LastBY)
 #       ReDim Cohort(LastBY, MaxAge + 1)
 #       ReDim MatRate(LastBY, MaxAge)
 #       ReDim AEQ(LastBY, MaxAge + 1)
 #       'landed catch variables
 #       If isReplicateCohShak = False Then
 #           ReDim LandedCatch(NumberPSCFisheries, 7, LastBY)
 #       Else
 #           ReDim LandedCatch(NumberERAFisheries, 7, LastBY)
 #       End If

 #       ReDim LegalDropoffMortality(NumberPSCFisheries, 7, LastBY)
 #       ReDim TotalTerminalLandedCatch(LastBY, MaxAge)
 #       ReDim TotalLandedCatch(LastBY, MaxAge)
 #       ReDim TotalLandedCatch_ByFishery(LastBY, NumberPSCFisheries)
 #       ReDim LandedCatchHarvestRate(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim AEQPreTermLandedCatch(LastBY, MaxAge)
 #       ReDim AEQLandedCatchPreTermER(LastBY, MaxAge)
 #       ReDim LandedCatchTermER(LastBY, MaxAge)
 #       ReDim AEQLandedCatchTotER(LastBY, MaxAge)
 #       ReDim TotalMortTermER(LastBY, MaxAge)
 #       ReDim AEQTotalMortTotER(LastBY, MaxAge)
 #       ReDim AEQTotalMortPreTermER(LastBY, MaxAge)
 #       ReDim PreTermIncidentalMortalities(LastBY, MaxAge)
 #       ReDim AEQPreTermTotalMorts(LastBY, MaxAge)
 #       ReDim AEQLandedCatchTotalRun(LastBY)
 #       ReDim AEQTotalMortTotalRun(LastBY)
 #       ReDim LandedCatchTerminalRun(LastBY)
 #       ReDim TotalMortTerminalRun(LastBY)
 #       ReDim AEQPreTermLandedCatchAllAges(LastBY)
 #       ReDim AEQPreTermTotalMortsAllAges(LastBY)
 #       ReDim TerminalLandedCatchAllAges(LastBY)
 #       ReDim TerminalTotalMortsAllAges(LastBY)
 #       ReDim AEQLandedCatchAllAgesPreTermER(LastBY)
 #       ReDim AEQTotalMortAllAgesPreTermER(LastBY)
 #       ReDim LandedCatchAllAgesTerminalER(LastBY)
 #       ReDim TotalMortAllAgesTerminalER(LastBY)
 #       ReDim AEQLandedCatchAllAgesTotalER(LastBY)
 #       ReDim AEQTotalMortsAllAgesTotalER(LastBY)
 #       'shaker variables
 #       ReDim TotalTerminalLegalDropoffs(LastBY, MaxAge)
 #       ReDim TotalTerminalShakers(LastBY, MaxAge)
 #       ReDim TotalTerminalShakerDropoffs(LastBY, MaxAge)
 #       ReDim TotalLegalDropoffs(LastBY, MaxAge)
 #       ReDim TotalSublegalShakerDropoffs(LastBY, MaxAge)
 #       ReDim TotalSublegalShakers(LastBY, MaxAge)
 #       'cnr variables
 #       ReDim TotalCNR(LastBY, MaxAge)
 #       ReDim TotalTerminalCNR(LastBY, MaxAge)
 #       ReDim LegalCNRMortality(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim SubLegalCNRMortality(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim LegalCNRDropoffs(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim SubLegalCNRDropoffs(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim TotalCNRLegal(LastBY, MaxAge)
 #       ReDim TotalCNRLegalDropoffs(LastBY, MaxAge)
 #       ReDim TotalCNRSubLegal(LastBY, MaxAge)
 #       ReDim TotalCNRSubLegalDropoffs(LastBY, MaxAge)
 #       ReDim TotalTerminalCNRLegal(LastBY, MaxAge)
 #       ReDim TotalTerminalCNRLegalDropoffs(LastBY, MaxAge)
 #       ReDim TotalTerminalCNRSubLegal(LastBY, MaxAge)
 #       ReDim TotalTerminalCNRSubLegalDropoffs(LastBY, MaxAge)
 #       ReDim ExtraLegalCNRMortality(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim ExtraLegalCNRDropoffs(NumberPSCFisheries, MaxAge, LastBY)
 #       ReDim AdultInterDamSurvivalRate(LastBY + MaxAge)
 #       ReDim JackInterDamSurvivalRate(LastBY + MaxAge)
 #   End Sub
}




