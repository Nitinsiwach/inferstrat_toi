library(data.table)
library(caret)
setwd('C:/Users/Goutam Ghosh/Desktop/Times of India - Analysis/Codes run/Presentation/Offline Predictor/use this/finals_mar26/finals_mar26/lets do it/Last modifications')

source('buildModel_hdr.R')

check=read.csv2('adamcorrectfileoctoberedited.csv',sep=",")

saveRDS(check,'adamcorrectoctober.RDS')
master= readRDS('adamcorrectoctober.RDS')
master=data.table(master)
#write.csv(master,"adamcorrect.csv")
oldmaster=master

nn= names(master)
nn= gsub(" ", "", nn)
nn= gsub("\\(", "", nn)
nn= gsub("\\)", "", nn)
# nn= gsub("/", "", nn)
names(master)= nn

rm(nn)

dontuse.cols= c("Runid", "IssueID", "IssueDate", "Edition", "Main.Suppl",
                "Products", "BookID", "Production.Start", "Production.End",
                "numFails", "Gross.Counter", "Waste", "Change.Over.Time..Sec.",
                "isTrain","startHour","endHour","Twin.Folder","X","Change.Over.Type","BWPages","ColorPages")

targs= c('Total.Run.time..Mnts.', 'Total.Downtime', 'Number.of.stoppages', 'W.b.s','Avg.Speed')

master[, targ:= Avg.Speed]
oldcols= c("X1.2.Flap","X1.2.Flap.with.Glue")
newcols= c("halfFlap","halfFlapwithGlue")
setnames(master, oldcols, newcols)

summary(master$IssueDate)
master$isTrain=1
train.dt= master[isTrain==1, -c(dontuse.cols, targs), with=F]
test.dt= master[isTrain==1, -c(dontuse.cols, targs), with=F]

#if (!identical(train.dt$targ, train.dt$Avg.Speed))
#{
#avgspd= train.dt[, list(avgSpeed= mean(Avg.Speed)), by= c("Folder", "Product.Type")]
#train.dt= merge(train.dt, avgspd, by= c("Folder", "Product.Type"), all.x= T)
# test.dt= merge(test.dt, avgspd, by= c("Folder", "Product.Type"), all.x= T)
#}

#train.dt[, Avg.Speed:= NULL]
#test.dt[, Avg.Speed:= NULL]


#nzv= nearZeroVar(train.dt, saveMetrics=T)
#nzv$varname= rownames(nzv)
#nzv= data.table(nzv)
#forcedvar=c("GNP_Jacket_comb","GLUED.HALF.FLAP")
#dropvars= nzv[zeroVar==T | percentUnique < 0.05]$varname

#train.dt= train.dt[, -dropvars, with=F]
#test.dt= test.dt[, -dropvars, with=F]
str(train.dt)

# library(doMC)
# registerDoMC(2)

# ctrl= trainControl(method= "repeatedcv", number= 5, repeats= 3)
ctrl= trainControl(method= "none")

ret= buildGBM(depth = 4, trees = 250, nodesize = 10)

# ret= buildRF(varsToSample = 8, numtrees = 500, nodewt = 10)

mymodel= ret[[1]]; vartable= ret[[2]]
head(vartable)
print(vartable)

# an example of variable reduction
vars.i.want = vartable[importance > 0.5]$varname
train.dt= train.dt[, c(vars.i.want, 'targ'), with=F]   #and now go back and rerun the model


saveRDS(mymodel, 'finalAdamModelavgspeed17.RDS')
saveRDS(vartable, 'finalModelVarImportancesavgspeed17.RDS')



finalcols= c("Runid", "IssueID", "IssueDate", "Edition", "Products", "BookID", 
             "Book.Type", "No.Of.Books", "Machine", "Folder", "Print.Order","Avg.Speed",
             "TotalPages", "Production.Start", "Production.End", "Product.Type",
             "Total.Run.time..Mnts.", "Predicted.avgspeed", "Prediction.Error.Speed")

test= scoreWithModel(master[isTrain==1], "finalOutput_testsetoctaftrgnpall.csv")
master$GNP.Jacket=1
Testgnp1= scoreWithModel(master[isTrain==1], "finalOutput_gnp1.csv")
master$GNP.Jacket=0
Testgnp0= scoreWithModel(master[isTrain==1], "finalOutput_gnp0.csv")
master$GNP.Jacket=oldmaster$GNP.Jacket

#CHECKING THE TESTING

check=read.csv2('inputtogautam.csv',sep=",")
saveRDS(check,'Inputtogautam.RDS')
master= readRDS('Inputtogautam.RDS')
master=data.table(master)

test= scoreWithModel(master, "testresults5.csv")

#train= scoreWithModel(master[isTrain==1], "finalOutput_trainset1aftrrcorr1.csv")





train = findBasicOutliers(train, kdev= 2)
test = findBasicOutliers(test, kdev= 2)

calcRMSE(train$Total.Run.time..Mnts., train$Print.Order/train$Predicted.avgspeed*60)
calcRMSE(test$Total.Run.time..Mnts., test$Print.Order/test$Predicted.avgspeed*60)

calcRMSE(train[outlier.tag == 0]$Total.Run.time..Mnts., 
         train[outlier.tag == 0]$Predicted.Run.time.Mnts)

calcRMSE(test[outlier.tag == 0]$Total.Run.time..Mnts., 
         test[outlier.tag == 0]$Predicted.Run.time.Mnts)
