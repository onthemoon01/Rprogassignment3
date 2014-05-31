
best<-function(state, outcome){
        setwd("/Users/aguirre/Desktop/Computer science/Coursera R programming/assignment/rprog_data_ProgAssignment3-data")
        fullTable<-read.csv("outcome-of-care-measures.csv",colClasse='character', na.strings="Not Available")
        totalState<-as.vector(fullTable[,7])
        uniqueState<-unique(totalState)
        if (state %in% uniqueState==FALSE){
                stop("invalid state")
        }
        if (outcome!="heart attack" && outcome!="heart failure" && outcome!="pneumonia"){
                stop("invalid outcome")
        }
        sapply(fullTable[,c(11,17,23)], as.numeric)
        smallTable<-(fullTable)[,c(2,7,11,17,23)]
        smallTable[,3:5]<-sapply(smallTable[,3:5],as.numeric)
        colnames(smallTable)<-c("Name", "State", "heart attack", "heart failure", "pneumonia")
        dataOutcome<-subset(smallTable,State==state, select=c("Name",outcome, "State"))
        dataOutcome2<-dataOutcome[order(dataOutcome[outcome],dataOutcome["Name"]),]
        result<-dataOutcome2[1,1]
        result
}
        
best("NY","pneumonia")



