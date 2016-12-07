# Outputting outcomes 
best<- function(state, outcome,num){
    
    ocdata0<-read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    
    if (outcome =="heart attack"){
         outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome =="heart failure"){
        outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
    } else if (outcome =="pneumonia"){
        outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
    } else {stop("invalid outcome")}
    
    
    
    if (any(ocdata0$State==state )){}
    else {stop("invalid state")}
    suppressWarnings(ocdata0[,outcome]<-as.numeric(paste(ocdata0[,outcome]))) ##changing class of outcome data
    ocdata<-ocdata0[complete.cases(ocdata0[,outcome]),]
    ocdata1<-ocdata[ocdata[,"State"]==state,]##subsetting the state
    ocdata2<-ocdata1[order(ocdata1[,outcome],ocdata1[,"Hospital.Name"], na.last = TRUE), c("Hospital.Name", outcome)]
    ocdata2[1,1]
}

## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
## "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 

##best("NY","heartattack")

##best("TX","heart attack")

##best("TX","heart failure")

##best("MD","heart attack")

##best("MD","pneumonia")
