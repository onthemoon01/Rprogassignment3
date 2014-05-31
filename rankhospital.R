rankhospital<-function(state, outcome, num){
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
        fullTable[,c(11,17,23)]<-sapply(fullTable[,c(11,17,23)], as.numeric)
        smallTable<-(fullTable)[,c(2,7,11,17,23)]
        smallTable[,3:5]<-sapply(smallTable[,3:5],as.numeric)
        colnames(smallTable)<-c("Name", "State", "heart attack", "heart failure", "pneumonia")
        dataOutcome<-subset(smallTable,State==state, select=c("Name",outcome, "State"))
        dataOutcome2<-dataOutcome[order(dataOutcome[outcome],dataOutcome["Name"],na.last=NA),]
        x<-length(dataOutcome2[,1])
        if (num=="best") {result<-dataOutcome2[1,1]}
        if (num=="worst"){result<-dataOutcome2[x,1]}
        if (num != "best" && num != "worst") {result<-dataOutcome2[num,1]}
        result     
}
