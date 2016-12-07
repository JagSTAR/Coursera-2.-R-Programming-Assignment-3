rankall<- function(outcome,num = "best"){
    
##Reading in data
    ocdata0<-read.csv("outcome-of-care-measures.csv",)
    
##Validating outcome 
    if (outcome =="heart attack"){
        outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome =="heart failure"){
        outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
    } else if (outcome =="pneumonia"){
        outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
    } else {stop("invalid outcome")}
    
##Defining num 
    if (num=="best") {num<-1}

##Changing class of outcome data
    suppressWarnings(ocdata0[,outcome]<-as.numeric(paste(ocdata0[,outcome]))) 
    
##Cleaning Data
    ocdata<-ocdata0[complete.cases(ocdata0[,outcome]),]    
    
##Splitting the data
    ocdata1<-split(ocdata,ocdata[,"State"])
    
##Reordering the data
    ocdata2<-lapply(ocdata1, function(x){x<-x[order(x[,outcome],x[,"Hospital.Name"], na.last = TRUE),]})

##Subsetting rank lists
    ocdataranked<-lapply(ocdata2,function(y){
        if (num=="worst"){num<-length(y[,1])} 
        y[num,c("Hospital.Name","State")]
        })

##Rebuilding the data frame and printing results
    do.call(rbind,ocdataranked)

}

## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
## "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 

##head(rankall("heart attack", 20), 10)

##tail(rankall("pneumonia", "worst"), 3)

##tail(rankall("heart failure"), 10)

