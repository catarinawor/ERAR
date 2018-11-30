# Variable definition for the ERA CIS code

Objects are split in three groups: subs (or functions), variables and flags. All objects are listed in alphabetic order within the two groups. uncertain definitions are marked with *IDK*

## Subs/functions

**GetCASStocks:** function that Get the array of CASStocks corresponding to the current ERAStock (e.g. ANB,ACI,ALP,ADM,AHC for ERA stock AKS).

**GetFirstAndLastBY:** Get first and last brood years for the CASStocks corresponding to the current ERAstock.

**GetIMData:**

**GetInterDamSurvival:**

**GetMaxReleaseSize:**  Returns the maximum amount (counts) amount of releases for each stock.

**GetSuperStockData:** Function that reads in the data from the SuperStock table in the database.

**GetSurvivalRates:**

**GetTaggedReleaseByBrood:** Recovers the information about hatchery releases by brood year - BroodYear CWTRelease TotalRelease.

**GetWithinBYWeightFlagAndPNVRegionAndAvgMatRates:**


**MainSub:** Main routine that calls most of the data reading and calculations subroutines.

## Variables

**CASStockString:** vector containing three letter identifiers of CAS Stocks for a specific ERAStock.


**CurrentStock:** holder of the three letter ERA stock code from ERAStockArray

**ERAStock:** Counter for loop over all ERA stocks pa

**ERAStockArray:** Vector containing the 3-letter code that specifies the ERA stock identifier. Read from table CASStock in CIS. 

**FirstBY:** First brood year for a given CASStockString 

**LastAvailableBY:** Last available brood year for a given CASStockString

**LastBY:** Last Brood Year, either equal to LastAvailableBY or LastPossibleBY depending on which one is lower. 

**LastPossibleBY:** LastCalendarYear - OceanStartAge


**LastCalendarYear:** Variable that stores the last calendar year in the database. It is initially set by the user, but then gets adjusted in the Get_IMData function

**MaxAge:** Maximum age assigned to a given Superstock. 

**MaxCalendarYear:** variable used to store the last calendar year set initially by the user. 

**MeanMatType:** Variable indicating what method is used to calculate average maturity rates. Options are: "ArithmeticMean" and  or other (defaults to geometric mean).

**OceanStartAge:** The first age in which a given Superstock appears in the fishery.

**SuperStock:** 3-4 letter code indicating Superstock identifier 

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

