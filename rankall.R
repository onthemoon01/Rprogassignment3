rankall<-function(outcome, num){
        fullTable<-read.csv("outcome-of-care-measures.csv",colClasse='character', na.strings="Not Available")
        totalState<-as.vector(fullTable[,7])
        uniqueState<-unique(totalState)
        uniqueState<-uniqueState[order(uniqueState)]
        fullTable[,c(11,17,23)]<-sapply(fullTable[,c(11,17,23)], as.numeric)
        smallTable<-(fullTable)[,c(2,7,11,17,23)]
        smallTable[,3:5]<-sapply(smallTable[,3:5],as.numeric)
        colnames(smallTable)<-c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        y<-length(uniqueState)
        result<-data.frame(NULL)
        for (i in 1:y){
                State<-uniqueState[i]
                dataOutcome<-subset(smallTable,state==State, select=c("hospital",outcome, "state"))
                dataOutcome<-subset(smallTable,state==State, select=c("hospital",outcome, "state"))
                dataOutcome2<-dataOutcome[order(dataOutcome[outcome],dataOutcome["hospital"],na.last=NA),]
                x<-length(dataOutcome2[,1])
                if (num=="best") {result<-rbind(result,dataOutcome2[1,c(1,3)])}
                if (num=="worst"){result<-rbind(result,dataOutcome2[x,c(1,3)])}
                if (num != "best" && num != "worst") {result<-rbind(result,dataOutcome2[num,c(1,3)])}
                result
        }
        result
}       
