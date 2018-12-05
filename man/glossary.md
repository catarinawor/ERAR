# Variable definition for the ERA CIS code

Objects are split in three groups: subs (or functions), variables and flags. All objects are listed in alphabetic order within the two groups. uncertain definitions are marked with *IDK*

## Subs/functions

**Age:** ages associated with antural mortlity rates

**GetCASStocks:** function that Get the array of CASStocks corresponding to the current ERAStock (e.g. ANB,ACI,ALP,ADM,AHC for ERA stock AKS).

**GetFirstAndLastBY:** Get first and last brood years for the CASStocks corresponding to the current ERAstock.

**GetIMData:** 

**GetInterDamSurvival:** This function retrieves the inter dam survival for a specific ERA stock, if inter dam survival data  does not exist (i.e. no dams), then sets the interdam survival to 1

**GetMaxReleaseSize:**  Returns the maximum amount (counts) amount of releases for each stock.

**GetSuperStockData:** Function that reads in the data from the SuperStock table in the database.

**GetSurvivalRates:** Retrieves Natural mortality rate from the database and the associated vector of ages.

**GetTaggedReleaseByBrood:** Recovers the information about hatchery releases by brood year - BroodYear CWTRelease TotalRelease.

**GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates:**

**MainSub:** Main routine that calls most of the data reading and calculations subroutines.

**NaturalMortalityRate:** What it says. For each ERA stock

**SetTerminalFishery:** Populates a matrix that is PSCfishe ry x Age, with flags indicating if fish is caught at a terminal fishery.

## Variables


**AdultInterDamSurvivalRate:** What is says. by calendar year

**AverageMatRate:** vector of length 4 that includes either maturity rate values or NA for the internally calculated maturity rates.

**CalendarYear:** Calendar year for which there is incidental mortality data


**CASStockString:** vector containing three letter identifiers of CAS Stocks for a specific ERAStock. Many CAS stocks can correspond to a single ERAStock (e.g.CAS stocks ANB,ACI,ALP,ADM,AHC for ERA stock AKS)

**CNREffort:** Catch non release effort

**CNRLegalEncounterEst:** Catch non release legal encounter estimate. Numeric ,CalendarYear x PSCFishery matrix.

**CNRMethod:** Catch non retention method, numrical, either 9, 1, 2, or 3 ("HAR"). CalendarYear x PSCFishery matrix.

**CNRSeasonLength:**  Catch non retention season length?

**CNRSubLegalEncounterEst:** Catch non release sub legal encounter estimate. Numeric ,CalendarYear x PSCFishery matrix.

**CurrentStock:** holder of the three letter ERA stock code from ERAStockArray

**DropoffRate:** Dropoff Incidental mortality rate. CalendarYear x PSCFishery matrix. CalendarYear x PSCFishery matrix.

**ERAStock:** Counter for loop over all ERA stocks pa

**ERAStockArray:** Vector containing the 3-letter code that specifies the ERA stock identifier. Read from table CASStock in CIS. 

**ExtraLegalIMRate:** ExtraLegal incidental mortality rate (Is this larger than the legal size, for the slot limit case?). CalendarYear x PSCFishery matrix.

**ExtraLegalSelectivityFactor:** Extra legal selectivity factor *IDK*. CalendarYear x PSCFishery matrix.

**ExtraLegalShakerEst:** Extra legal shaker estimate. CalendarYear x PSCFishery matrix.

**FirstBY:** First brood year for a given CASStockString


**InterDamSurvival_CalendarYear:** Calendar year for the interdamSurvival data. 

**JackInterDamSurvivalRate:** What is says. by calendar year

**LandedCatchEst:** Landed catch estimate.CalendarYear x PSCFishery matrix.

**LegalShakerEst:** Legal Shaker estimate. CalendarYear x PSCFishery matrix.

**LastAvailableBY:** Last available brood year for a given CASStockString

**LastBY:** Last Brood Year, either equal to LastAvailableBY or LastPossibleBY depending on which one is lower. 

**LastPossibleBY:** LastCalendarYear - OceanStartAge


**LastCalendarYear:** Variable that stores the last calendar year in the database. It is initially set by the user, but then gets adjusted in the Get_IMData function.

**LegalCatchabilityCoefficient:** Legal Catchability Coefficient. PSCFishery xAge.

**LegalIMRate:** Legal incidental mortality rate. CalendarYear x PSCFishery matrix.

**LegalSelectivityFactor:** Legal selectivity factor *IDK* numerical: 0.34, 0.20, 0.00, or 1.00. CalendarYear x PSCFishery matrix.

**MaxAge:** Maximum age assigned to a given Superstock. 

**MaxCalendarYear:** variable used to store the last calendar year set initially by the  user. 

**MeanMatType:** Variable indicating what method is used to calculate average maturity rates. Options are: "ArithmeticMean" and  or other (defaults to geometric mean).

**MonitoredLegalCatch:** that.  CalendarYear x PSCFishery matrix.

**MonitoredLegalReleases:** that.  CalendarYear x PSCFishery matrix.

**OceanStartAge:** The first age in which a given Superstock appears in the fishery.

**PNV:** Proportion non vulnerable  CalendarYear x PSCFishery x Age array

**PNVRegion:** 5? *IDK* 

**PSCFishery:** List of Integeres indicating PSC fisheries, current CIS data incluse 78 fisheries. 

**PV:** Proportion  vulnerable,  CalendarYear x PSCFishery x Age array

**Reavailability:** *IDK* one of these: 0  78   1  92 125  60 for "HAR"

**RetentionEffort:** *IDK* What are the units for this? all 0s. CalendarYear x PSCFishery matrix.

**RetentionMethod:** Either NA or 0, CalendarYear x PSCFishery matrix.


**SeasonLength:** Numerical (high numbers) CalendarYear x PSCFishery matrix. 

**SeasonUnits:** Either NA or 0 for harrison, CalendarYear x PSCFishery matrix.

**SubLegalCatchabilityCoeificient:** Sub legal catchability. PSCFishery xAge.

**SublegalIMRate:** Sub legal incidental mortality rate. CalendarYear x PSCFishery matrix.

**SublegalSelectivityFactor:** *IDK* either 1 or 0. CalendarYear x PSCFishery marix. 

**SubLegalShakerEst:** Sub Legal shaker estimate. CalendarYear x PSCFishery marix.

**SuperStock:** 3-4 letter code indicating Superstock identifier 

**SurvivalRate:** Survival rate at age for the CASStockString

**terminal:** Matrix with flags indicating is a fishery is terminal or not.

**TermNetSwitchAge:** Any age older than this age will be marked in the "terminal" flag. *IDK* I am not sure but I think the terminal flag will be used to indicate that aging should not occur for fish caught in certain net fisheries.  

**TimeStep:** *IDK* Not functional and set to 1 in the current CIS code. Maybe time steps within a year? 

**TimePeriod:** *IDK* Not functional and set to 1 in the current CIS code. Maybe periods larger than year? 

**youngestAge:** Youngest age for a given set of CASStockString



## Flags

**ArithmeticMeanFlag:** Flag indicating if Arithmetic Average Maturity Rates is used. Alternative is geometric mean. Note: In the R code this flag is given by the M$MeanMatType variable.

**isreplicate2016URBdata:** Flag used to make CIS match CAS, since some data was missing in the 2016 CAS database but was present in the C files. - This flag will probabaly go away once the review of the CIS prgram is complete. 


**isTraceCalc:** *IDK* Some sort of verbose flag?
            
**isTraceByBroodYr:** *IDK* Some sort of verbose flag?

**isTraceByCalendarYr:** *IDK* Some sort of verbose flag?
           
**traceThisShakerMethod:** *IDK* Some sort of verbose flag?

**ShakCalcFlg:** Flag indicating which shaker method should be used. 'C' is the default.

**isCombineAge2And3:** indicates whether age 2 and 3 should be combined. Combination only applies if Oceanstartage is 3

**WithinBYWeightFlag:** Flag used to calculate Landed catch and escapenet method

