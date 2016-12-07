rankhospital<- function(state, outcome,num){
    
##Reading in data
    ocdata0<-read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
##Validating outcome 
    if (outcome =="heart attack"){
         outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome =="heart failure"){
        outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
    } else if (outcome =="pneumonia"){
        outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
    } else {stop("invalid outcome")}
    
##Validating state    
    if (any(ocdata0$State==state )){}
    else {stop("invalid state")}

##Validating num
    if (is.numeric(num)&num>sum(ocdata0[,"State"]==state)){print(NA)}
    if (is.numeric(num)&num>sum(ocdata0[,"State"]==state)){stop()}
    
    
##Changing class of outcome data
    suppressWarnings(ocdata0[,outcome]<-as.numeric(paste(ocdata0[,outcome]))) 
    
##Cleaning Data
    ocdata<-ocdata0[complete.cases(ocdata0[,outcome]),]    
    
##Subsetting the state
    ocdata1<-ocdata[ocdata[,"State"]==state,]
    
##Building New Data.Frame
    ocdata2<-ocdata1[order(ocdata1[,outcome],ocdata1[,"Hospital.Name"], na.last = TRUE), c("Hospital.Name", outcome)]

##Defining num 
    if (num=="best") {num<-1}
    if (num=="worst") {num<-length(ocdata2[,2])}
    
##Printing Results    
    ocdata2[num,1]
}

## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
## "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 

##rankhospital("TX","heart failure", 4)

##rankhospital("MD","heart attack", "worst")

##rankhospital("MN","heart failure", 5000)


