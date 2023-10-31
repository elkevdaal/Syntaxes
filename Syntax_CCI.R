# Author: Elke van Daal
# Date: 31-10-2023
# Subject: Syntax Comprehensive Complication Index (CCI) (validated with cci Calculator)

### Explanation: 
### The CCI syntax enables the calculation of the overall morbidity 
### of a patient after any surgical intervention based on 
### the complication grading by the Clavien-Dindo Classification. 
### The CCI reflects the gravity of this overall complication burden on 
### the patient on a scale from 0 (no complication) to 100 (death). 

cci <- function( X = NULL ) {
  
  ## **************************
  ## *** STEP 1: INPUT DATA ***
  ## **************************
  
  if((!(is.data.frame(X) | is.matrix(X))) | (ncol(X)!=7) )
    stop("X must be a data.frame (or matrix) with 7 columns")
  
  X <- as.data.frame(lapply(as.data.frame(X), as.integer))
  names(X) <- 
    c("cd_1", "cd_2", "cd_3a", "cd_3b", "cd_4a", "cd_4b", "cd_5")
  
  ## ****************************
  ## *** STEP 2: Define constants 
  ## ****************************
  
  wc1 <- 300
  wc2 <- 1750
  wc3a <- 2750
  wc3b <- 4550
  wc4a <- 7200
  wc4b <- 8550
  
  ## ***********************
  ## *** STEP 3: CCI formula
  ## ***********************
  
 cci <- ifelse(X$cd_5 > 0, 100, sqrt(
   X$cd_1 * wc1 + 
     X$cd_2 * wc2 + 
     X$cd_3a * wc3a + 
     X$cd_3b * wc3b + 
     X$cd_4a * wc4a +
     X$cd_4b * wc4b)/2)
 
 ## **********************************
 ## *** STEP 4: safe output and return
 ## **********************************
 
 cci_scores <- as.data.frame(cci)
 assign("cci_scores", cci_scores, envir = .GlobalEnv)
 return(X)
 
} 

### Exit ###

