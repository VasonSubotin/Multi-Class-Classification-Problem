
DeteidexofNas<-function(data){ 
        #detect the idecies (df_Ind) and names (df) of colums where NA's are of more than 80 precent
        df<-c()
        df_Ind<-c()
        for (i in 1:dim(data)[2]) {
                if ((length(which(!is.na(data[,i])))/length(data[,i]))<0.8)
                {df<-cbind(df,names(data)[i])
                 df_Ind<-cbind(df_Ind,i)
                }  
        }  
        
        return(df_Ind)
}

ChangeClassforMatrix<-function(data,StartPos,EndPosition,NameOfTheClassToChangeTo){ 
        for (i in StartPos:EndPosition) {
                if (NameOfTheClassToChangeTo=="numeric") {data[,i]<-as.numeric(data[,i])}
                if (NameOfTheClassToChangeTo=="factor") {data[,i]<-as.factor(data[,i])}
        }  
        return(data)
}


data.cleaning<-function(mydata,number.sampled.data){
        
        #removed variables containing more than 80% of NA's
        pp<-DeteidexofNas(mydata)
        ppst1<-mydata[,-pp]
        
        #No Zeroes Variance Variable
        nzv <- nearZeroVar(ppst1)                   
        ppst2<-ppst1[,-nzv]
        
        #No Zeroes, complete samples
        ppst3<-subset(ppst2,ppst2$pitch_forearm!=0) 
        ppst4<-ppst3[,-c(2,5)]                     
        
        #No Characters
        temp=dim(ppst4)[2]
        ppst5<-ChangeClassforMatrix(ppst4,1,temp-1,"numeric")       
        
        #make all columns as numerical
        temp0=dim(ppst5)[2]
        ppst5[,temp0]<-as.factor(ppst5[,temp0])           
        
        #make the last columns as factor
        temp1=dim(ppst5)[2]
        ppst6<-preProcess(ppst5[,-temp1],method=c("scale","center")) #Scale and Center
        ppst7<-predict(ppst6,ppst5[,-temp1])
        
        #Scale and Center
        ppst7[, "classe"] <- ppst5$classe
        
        
        dd1<-ppst7[sample(nrow(ppst7),number.sampled.data),]
        
        return(dd1) #No Correaltion on the data 
        
        
}

CVoneVsAll<-function(data,onefactor,nameFactor,K,EndValueofNumerofVariable,StartValueofNumerofVariable=1){
        
        TrainAcuracy<-vector()
        TestAcuracy_ts<-vector()
        
        TempMatrix<-cbind(data,onefactor)
        EndCol<-dim(TempMatrix)[2]
        EndDataCol<-EndValueofNumerofVariable
        colnames(TempMatrix)[EndCol]<-nameFactor
        
        modelFormula<-AddNamesOfColl(data,StartValueofNumerofVariable,EndDataCol)
        UpdateFit<-paste(colnames(TempMatrix)[EndCol],"~",modelFormula)
        
        flds <- createFolds(TempMatrix[,EndCol], k = K, list = TRUE, returnTrain = T) #make K folds for corss valdation set 
        
        #preform cross validation for range of diffferent K folds starting from 1 to K
        for (i in 1:K) { 
                
                #make training set
                training<-TempMatrix[flds[[i]],] 
                
                #make testing set
                testing<- TempMatrix[-flds[[i]],] 
                
                #fit the glm model for data set, one dummy column and K folds 
                glm.fit<-glm(UpdateFit,data=training,family=binomial) 
                
                # predition on training set
                glm.probs_x<-predict(glm.fit,newdata=training,type="response")
                
                # making probabilities as binary outcome 
                glm.pred_Train_x<-ifelse(glm.probs_x>0.5,"1","0") 
                
                TrainTable<-table(glm.pred_Train_x,training[,EndCol])
                TrainAcuracy<-cbind(TrainAcuracy,(TrainTable[1,1]+TrainTable[2,2])/sum(TrainTable))
                
                # predition on testing set
                # making probabilities as binary outcome 
                glm.probs_ts<-predict(glm.fit,type="response",newdata=testing) 
                glm.pred_ts<-ifelse(glm.probs_ts>0.5,"1","0")                
                
                
                TestTable<-table(glm.pred_ts,testing[,EndCol])
                # comperaing testing factor with predicted on testing set using table function
                #print(TestTable[2,2])
                TestAcuracy_ts<-cbind(TestAcuracy_ts,(TestTable[1,1]+TestTable[2,2])/sum(TestTable)) 
                
        }
        CVoneVsAll<-list(TrainAcuracy<-mean(TrainAcuracy),TestAcuracy_ts<-mean(TestAcuracy_ts))
        return(CVoneVsAll)
        
}


CossValidation<-function(data, factors, K){ 
        
        # K- cross validation with glm starting from fold=2 to K.K will be sent to CVoneVsAll function
        TrainlistNames<-list()
        TestlistNames<-list()
        
        TrainError<-vector()
        TestError<-vector()
        
        NumbrOfVaibale<-dim(data)[2]        
        DimFactors<-dim(factors)[2]
        x=seq(from=2,to=K,by=1) # x vector of K's
        
        for (d in 2:K){ # do cross validation for range of folds numbers  K  starting from 2
                
                TrainErrortemp<-vector()
                TestErrortemp<-vector()
                TempMatrix<-data.frame() 
                
                for (i in 1:DimFactors){ 
                        # do cross validation for each of the dummy factors  column
                        
                        TempMatrix<-cbind(data,factors[i]) #merge data and factor column "A" (i=1), B(i=2),..) 
                        TrainErrortemp<-cbind(TrainErrortemp,CVoneVsAll(data,factors[i],names(factors)[i],d,NumbrOfVaibale)[[1]] )
                        #train Error for every of Factors colmun for "A" (i=1), "B" (i=2),..) 
                        
                        
                        TestErrortemp<-cbind(TestErrortemp,CVoneVsAll(data,factors[i],names(factors)[i],d,NumbrOfVaibale)[[2]])
                        #test Error for every of Factors colmun for "A" (i=1), "B" (i=2),..) 
                        
                }
                
                TrainlistNames<-c(TrainlistNames,TrainErrortemp)
                TestlistNames<-c(TestlistNames,TestErrortemp)
                
                TrainError<- cbind(TrainError,mean(TrainErrortemp)) #get the mean value all train errors of "A" (i=1), "B" (i=2),..
                TestError<- cbind(TestError,mean(TestErrortemp)) #get the mean value all test errors of "A" (i=1), "B" (i=2),..
                
        }
        
        
        return(list(TrainError= TrainError,TestError=TestError,x=x))
}


data.correlation<-function(processed.data.nofactors){
        recived.cor<-cor(processed.data.nofactors)
        recived.cor[lower.tri(recived.cor)] <- 0
        diag(recived.cor)<-0
        recived.variables<-which(recived.cor>=0.9,arr.ind=T,useNames=T) 
        temp.unique.columns<-c(recived.variables[,1],recived.variables[,2])
        temp.unique.val<-unique(temp.unique.columns)
        temp.data.frame<-(processed.data.nofactors[,c(temp.unique.val)])
        par(mfrow=c(1,2),pin=c(2,2))
        
        #pairs(temp.data.frame[,1:3])
        
        temp<-temp.data.frame
        
        plot(temp[,1],temp[,2],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),)
        par(new=T)
        plot(temp[,1],temp[,3],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),col="blue")
        par(new=F)
        
        plot(temp[,2],temp[,3],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),col="red")
        par(new=T)
        plot(temp[,2],temp[,5],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),col="green")
        
        # par(new=T)
        # plot(temp[,7],temp[,13],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),col="darkblue")
        return(list(temp.data.frame=temp.data.frame,recived.variables=recived.variables))
}

AddNamesOfColl<-function(dat,start,end){ 
        tem<-as.character()
        namesOfVaribales<-names(x = dat)
        for (i in start:(end-1)) {tem<-paste(tem,namesOfVaribales[i],"+")}
        tem<-paste(tem,namesOfVaribales[end])
        return((tem))
}


#corelated.var<-data.correlation(dummy.num.data[[1]])
#corelated.var[[2]]

my.dummy.variable<-function(data,IndexColToBeDummy){ #make dummy variables from one factor column  
        
        class.name<-sapply(data,class)
        index.classVar<-which(class.name=="factor") #index of factor columns
        temp.vector<-data[,index.classVar] #get the factor colum and assing it to vector
        if (index.classVar!=IndexColToBeDummy) stop("type correct column to be dummied")          
        
        UniqueLevels<-unique(data[,index.classVar]) #unique levels(velues) in factor colunm
        data<-data[,-index.classVar]
        tem<-data.frame()
        tem1<-data.frame()
        tem2<-data.frame()
        
        for (i in 1:length(UniqueLevels)) {  #create a dummy varibale matrix
                rr<-temp.vector==UniqueLevels[i]
                rr1<-as.numeric(rr)
                #rr1<-as.factor(rr1)
                tem<-rbind(tem,rr1)
        }
        tem1<-t(tem)
        tem2<-data.frame(tem1)
        names(tem2)<- UniqueLevels #assingin names for colums
        data<-data[,-index.classVar]
        #done<-cbind(data,tem,deparse.level=0)
        
        return(list(Num.data=data,DummyVariables=tem2))
}

Error_vs_NumberofVariables<-function(cc1,sample.number){
        #make a final graph for 
        #cc1-cleaned data with one multilcass factor column variables at the end. 
        #called CossValidation function(prepresented above)
        temp.ectorOfColums<-vector()                                
        EndDummyCol<-dim(cc1)[2]
        
        dd1<-cc1[sample(nrow(cc1), sample.number),]
        dd1[,EndDummyCol]<-as.factor(dd1[,EndDummyCol])
        
        #get a random sample of data
        dd2<-my.dummy.variable(dd1,EndDummyCol)
        dd2_data<-dd2[[1]]
        dd2_factors<-dd2[[2]]
        dd2_factors<-ChangeClassforMatrix(dd2_factors,1,5,"factor") #make all dummy-class varibales as factors
        
        temp.numberts<-vector()
        temp.numbertr<-vector()
        x=seq(from=15,to=dim(dd2_data)[2],by=2)        		  
        
        for (i in x)  {						#for loop for testing error as function of the number of variables (represented by vector x) 
                
                temp.VectorOfColums<-dd2_data[1:i]
                tr<-CossValidation(temp.VectorOfColums,dd2_factors,3) #10 folds cross validation 
                EndValTs<-length(tr$TestError)				#getting the mean for TestError 
                temp.numberts<-cbind(temp.numberts,max(tr$TestError))
                EndValTr<-length(tr$TrainError)
                temp.numbertr<-cbind(temp.numbertr,min(tr$TrainError))  #getting the mean for TrainError 
        }
        s<-list(TrainError=temp.numbertr,TestEror=temp.numberts,x=x)
        
        
        #return(list(TrainError=temp.numbertr,TestEror=temp.numberts))
        return(s)
}

pca.data.cleaning<-function(raw.data,number.sampled.data){
        
        mydata<-raw.data
        #removed variables containing more than 80% of NA's
        pp<-DeteidexofNas(mydata)
        ppst1<-mydata[,-pp]
        
        #No Zeroes Variance Variable
        nzv <- nearZeroVar(ppst1)                   
        ppst2<-ppst1[,-nzv]
        
        #No Zeroes, complete cases
        ppst3<-subset(ppst2,ppst2$pitch_forearm!=0) 
        ppst4<-ppst3[,-c(2,5)]                     
        
        #No Characters
        temp=dim(ppst4)[2]
        ppst5<-ChangeClassforMatrix(ppst4,1,temp-1,"numeric")       
        
        end.col<-dim(ppst5)[2]
        pca<-preProcess(ppst5[,-end.col],method="pca")
        #pca <- prcomp(ppst5[,-end.col],center = TRUE, scale. = TRUE)
        transformedData <- predict(pca, ppst5[,-end.col])
        transformedData<-as.data.frame(transformedData)
        set.seed(1)
        transformedData[, "classe"] <- ppst5$classe
        dd1<-transformedData[sample(nrow(transformedData),number.sampled.data),]     
        
        return(dd1) #No Correaltion on the data 
        
}


correlation.pca.data.cleaning<-function(ppst7) {
        endcol<-dim(ppst7)[2]
        ppst8_1<-abs(cor(ppst7[,-endcol]))                              
        ppst8_2<-findCorrelation(ppst8_1,cutoff = 0.9)
        ppst8<-ppst7[,-ppst8_2]
        return(ppst8)  
}

make.correlation.plots<-function(cleaned.data) {
        cleaned.data<-data.cleaning(raw.data)
        nocorrelated.var.data<-correlation.pca.data.cleaning(cleaned.data)
        sample.test1<-Error_vs_NumberofVariables(nocorrelated.var.data,6000)
        x<-sample.test1[[3]]
        return(list(x=x,test.error=1-sample.test1[[2]]))
        
}



make.pca.plots<-function(raw.data){
        pca.data.temp<-pca.data.cleaning(raw.data,6000)
        sample.test2<-Error_vs_NumberofVariables(pca.data.temp,6000)
        x<-sample.test2[[3]]
        return(list(x=x,test.error=1-sample.test2[[2]]))
}


ebat<-function(raw.data,cleaned.data) {
        
        cor<-make.correlation.plots(cleaned.data)
        
        plot(cor[[1]],cor[[2]],type="b",col="green",xlim=c(20,56),
             ylim=c(0.04,0.12),xlab="The Number of Values",ylab="Test Error",
        )
        
        par(new=T)
        
        pca<-make.pca.plots(raw.data)
        
        plot(pca[[1]],pca[[2]],type="b",col="blue",xlim=c(20,56),
             ylim=c(0.04,0.12),xlab="The Number of Values",ylab="Test Error")
        par(new=T)
        
        #plot(accomplished.plot[[1]],accomplished.plot[[2]],type="b",col="red",xlim=c(20,56), ylim=c(0.04,0.12),axes=F,ann=F)
        
        text(c(42,42,42),c(0.11,0.105,0.1),labels=c("With PCA","Control","With Correlation"),col=c("blue","red","green"),adj=c(0,0,0))        
        
        return(list(psa=pca,cor=cor))
        
}





variables.vs.cases<-function() { #three option of error_vs_number_of_variables
        #curve for different nunber of cases 
        par(mfrow=c(1,1))
        sample.test1<-Error_vs_NumberofVariables(cleaned.data,500)
        x<-sample.test1[[3]]
        plot(x,1-sample.test1[[2]],type="b",col="blue",xlim=c(19,56),ylim=c(0.04,0.12),xlab="The Number Of Variables",ylab="Test Error",main="Variables is The Tunning Parameter",cex.lab=1, cex.axis=1,cex=1,cex.main=1 )
        
        par(new=TRUE)
        sample.test2<-Error_vs_NumberofVariables(cleaned.data,500)
        plot(x,1-sample.test2[[2]],col="red",cex=1,axes=F,ann=F,type="b",xlim=c(19,56),ylim=c(0.04,0.12))
        
        par(new=TRUE)
        sample.test3<-Error_vs_NumberofVariables(cleaned.data,500)
        plot(x,1-sample.test3[[2]],col="green",cex=1,lwd=2,axes=F,ann=F,type="b",xlim=c(19,56),ylim=c(0.04,0.12))
        
        text(c(47,47,47),c(0.115,0.108,0.101),labels=c("500 samples","1000 samples","5000 samples"),col=c("blue","red","green"), adj=c(0,0,0))
        
        par(new=F)
        return(list(x,1-sample.test3[[1]]))
}

plot.data.correlation<-function(temp.data.frame){#corelated.var[[1]],corelated.var[[3]]
        temp<-temp.data.frame
        par(mfrow=c(1,2),pin=c(1.5,1.5))
        plot(temp[,1],temp[,2],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),main="Correlation",xlab="roll_belt",ylab="yaw_belt,total_accel_belt")
        
        
        par(new=T)
        plot(temp[,1],temp[,3],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),col="blue",ann=FALSE,axes=FALSE)
        par(new=F)
        
        plot(temp[,2],temp[,3],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),col="red",main="Correlation",xlab="yaw_belt",ylab="total_accel_belt,accel_belt_y")
        par(new=T)
        plot(temp[,2],temp[,5],xlim=c(-2.5,2.5),ylim=c(-2.5,2.5),col="green",ann=FALSE,axes=FALSE)
}


data.correlation<-function(processed.data.nofactors){
        recived.cor<-cor(processed.data.nofactors)
        recived.cor[lower.tri(recived.cor)] <- 0
        diag(recived.cor)<-0
        recived.variables<-which(recived.cor>=0.9,arr.ind=T,useNames=T) 
        temp.unique.columns<-c(recived.variables[,1],recived.variables[,2])
        temp.unique.val<-unique(temp.unique.columns)
        temp.data.frame<-(processed.data.nofactors[,c(temp.unique.val)])
        
        temp<-temp.data.frame
        return(list(temp.data.frame=temp.data.frame,recived.variables=recived.variables))
}

svmtest<-function(cleaned.data,new.raw.data){
        mydata<-new.raw.data
        require(e1071)
        #removed variables containing more than 80% of NA's
        pp<-DeteidexofNas(mydata)
        ppst1<-mydata[,-pp]
        
        #No Zeroes Variance Variable
        nzv <- nearZeroVar(ppst1)                   
        ppst2<-ppst1[,-nzv]
        
        ppst4<-ppst2[,-c(2,5)]  
        
        #No Characters
        temp=dim(ppst4)[2]
        ppst5<-ChangeClassforMatrix(ppst4,1,temp-1,"numeric")       
        
        #make all columns as numerical
        temp0=dim(ppst5)[2]
        ppst5[,temp0]<-as.factor(ppst5[,temp0])           
        
        #make the last columns as factor
        temp1=dim(ppst5)[2]
        ppst6<-preProcess(ppst5[,-temp1],method=c("scale","center")) #Scale and Center
        ppst7<-predict(ppst6,ppst5[,-temp1])
        
        end.col<-dim(cleaned.data)[2]
        model <- svm(cleaned.data$classe~., cleaned.data[,-end.col] )
        
        train<- predict(model, newdata=cleaned.data)
        res<-predict( model, newdata=ppst7)
        
        conf.mat<-confusionMatrix(train,cleaned.data$classe)
        return(list(res=res,conf.mat=conf.mat,model=model,ppst7=as.data.frame(ppst7)))
}

predictions<-function(data,dummyvar,new.data,Cv_Value){
        #how Error depens on the number of samples?
        #glm: fitting of data with factor vector dummyvar containing n colums of factors for multi-class classification problem.
        #Cv_Value, presentage of data suppose to be Training set, a "p" number in createDataPartition
        dd1<-data #[sample(nrow(data), 6000),]
        
        dummyVarlenght<-dim(dummyvar)[2]
        dataEndVal<-dim(dd1)[2]
        modelFormula<-AddNamesOfColl(dd1,1,dataEndVal)
        TrainAcuracy<-vector()
        TestAcuracy_ts<-vector()
        our.table<-matrix(nrow=5,ncol=20)
        
        for (i in 1:dummyVarlenght) {
                TempMatrix<-cbind(dd1,dummyvar[i])
                indx<-dim(TempMatrix)[2]
                dataEndVal1<-dim(TempMatrix)[2]
                
                inTrain<-createDataPartition(y=TempMatrix[,indx],p=Cv_Value,list=FALSE)
                training<-TempMatrix[inTrain,]
                testing<-TempMatrix[-inTrain,]
                
                UpdateFit<-paste(names(training[dataEndVal1]),"~",modelFormula)
                glm.fit<-glm(UpdateFit,data=training,family=binomial)
                glm.training<-glm(UpdateFit,data=training,family=binomial)
                
                glm.summary<-glm.fit
                
                glm.probs_x<-predict(glm.fit,newdata=training,type="response")
                glm.pred_Train_x<-ifelse(glm.probs_x>0.5,"1","0")
                CFglm.pred_Train_x<-confusionMatrix(glm.pred_Train_x,training[,indx])
                TrainAcuracy<-cbind(TrainAcuracy,CFglm.pred_Train_x[[3]][[1]])
                
                glm.probs_ts<-predict(glm.training,type="response",newdata=testing)
                glm.pred_ts<-ifelse(glm.probs_ts>0.5,"1","0") 
                CFglm.pred_ts<-confusionMatrix(glm.pred_ts,testing[,indx]) 
                TestAcuracy_ts<-cbind(TestAcuracy_ts,CFglm.pred_ts[[3]][[1]])
                
                glm.probs_ts<-predict(glm.training,type="response",newdata=new.data)
                glm.pred_ts<-ifelse(glm.probs_ts>0.5,paste(names(dummyvar)[i]),"0")
                
                our.table[i,]<-(glm.probs_ts)
                
                
        }
        our.table1<-t(our.table)
        our.table1<as.data.frame(our.table1)
        colnames(our.table1)<-names(dummyvar)
        ap<-apply(our.table1,1,max)
        tt<-(our.table1==ap)
        tem.vec<-vector()
        
        endcol<-dim(tt)[1]
        for (i in 1:endcol){
                ind<-which.max(our.table1[i,])
                
                our.table1[i,names(ind)]<-names(ind)
                tem.vec<-cbind(tem.vec,names(ind))
        }
        
        
        
        return(list(our.table1=our.table1,tem.vec=as.character(tem.vec)))
        
        
}
