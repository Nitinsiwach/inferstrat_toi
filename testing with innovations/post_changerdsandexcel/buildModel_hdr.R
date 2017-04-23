

calcRMSE= function(actualcol, predcol)
{
  rmse= sqrt(mean((actualcol - predcol)^2))
  return(rmse)
}

calcRsq= function(actualcol, predcol)
{
  total.sum.sq= sum((actualcol - mean(actualcol))^2)
  err.sum.sq= sum((actualcol - predcol)^2)
  rsq= (total.sum.sq - err.sum.sq)/total.sum.sq
  return(rsq)
}



extractVariableImportance = function(md, charcols)
{
  vv= data.frame(varImp(md)$importance)
  vv$importance= vv$Overall
  vv$colname= rownames(vv)
  
  varTable= data.table(vv[,c('colname','importance')])
  varTable[, varname:= colname]
  
  # recollecting variables taht got scattered through one-hot encoding in caret
  for (cc in charcols) { varTable[ grepl(cc, colname), varname:= cc] }
  
  varTable = varTable[, list(importance= sum(importance)), by= 'varname']
  varTable= varTable[order(-importance)]
  
  return(varTable)
}



buildRF = function(varsToSample, numtrees= 500, nodewt= 5) 
{
  set.seed(8765)
  rfGrid= expand.grid(mtry= varsToSample)
  
  rf= train(targ ~ ., 
            data= train.dt,
            method= "rf",
            trControl= ctrl,
            tuneGrid= rfGrid,
            ntree= numtrees,
            nodesize= nodewt,
            importance= T)
  
  varTable = extractVariableImportance(rf, charcols= names(train.dt)[sapply(train.dt, "class") %in% c("character", "factor")])
  
  train.preds= predict(rf, train.dt)
  test.preds= predict(rf, test.dt)
  
  print(paste("RMSE on train=", calcRMSE(train.dt$targ, train.preds) ))
  print(paste("R-squared on train=", calcRsq(train.dt$targ, train.preds) ))
  
  print(paste("RMSE on test=", calcRMSE(test.dt$targ, test.preds) ))
  print(paste("R-squared on test=", calcRsq(test.dt$targ, test.preds) ))
  
  return(list(rf, varTable))
}


buildGBM = function (depth, trees, nodesize, step = 0.1, model_data)
{
  set.seed(8765)
  gbmGrid= expand.grid(interaction.depth = depth, 
                       n.trees = trees, 
                       shrinkage = step,
                       n.minobsinnode = nodesize)
  
  gbm= train(targ~ ., 
             data= model_data,
             method= "gbm",
             trControl= ctrl,
             tuneGrid= gbmGrid,
             verbose= F)
  
  varTable = extractVariableImportance(gbm, charcols= names(train.dt)[sapply(train.dt, "class") %in% c("character", "factor")])
  
  train.preds= predict(gbm, train.dt)
  test.preds= predict(gbm, test.dt)
  
  print(paste("RMSE on train=", calcRMSE(train.dt$targ, train.preds) ))
  print(paste("R-squared on train=", calcRsq(train.dt$targ, train.preds) ))
  
  print(paste("RMSE on test=", calcRMSE(test.dt$targ, test.preds) ))
  print(paste("R-squared on test=", calcRsq(test.dt$targ, test.preds) ))
  
  return(list(gbm, varTable))
}



scoreWithModel = function(scoredata, outfile, scorecols= finalcols)
{
  #scoredata= merge(scoredata, avgspd, by= c("Folder", "Product.Type"), all.x=T)
  
  scoredata$Predicted.avgspeed = predict(mymodel, scoredata)
  scoredata[, Prediction.Error.Speed:= (Predicted.avgspeed - Avg.Speed)]
  scoredata= scoredata[, , with=F]
  
  write.csv(scoredata, outfile, row.names=F)
  return(scoredata)
}