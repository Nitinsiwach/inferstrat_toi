#install.packages(c("plyr", "caret"))
#install.packages("gbm")
require("plyr")
require("caret") 
require("gbm")
require("RMySQL")
require("lubridate")
avgmod <- readRDS("finalAdamModelavgspeed11.RDS")



 
#read from csv
#file <- read.csv("Testing File.csv")
#file$Avg.Speed <- predict(avgmod, file)


#read from db
mydb = dbConnect(MySQL(), user='root', password='t@i1234', dbname='toi_data_entry', host='10.101.1.199')
file = dbSendQuery(mydb, "select * from input")
file = fetch(file, n = -1)
file <- file[,!(colnames(file) %in% c("Estimated_production_start", "Estimated_production_end", "lag"))]
file$rowindex <- 1:nrow(file)
todb_file <- file







colnames(file) <- gsub("_",".", colnames(file))
file$Folder <- as.factor(gsub("\"", "", file$Folder))
#file <- file[-1]









file$Predicted.avgspeed <- predict(avgmod, file)


#populate Calculated.Run.time
file$`Calculated.Run.time` <- (file$Print.Order/file$Predicted.avgspeed)*60


todb_file$Calculated_Run_time <- file$Calculated.Run.time
todb_file$Predicted_avgspeed <- file$Predicted.avgspeed
#writing the pred.avgspeed to input. changing entire file for the timebeing
#dbWriteTable(mydb, name='input', value= file, overwrite = T)


#set lag
lag <- 15




#colnames(file)[which(colnames(file) == "X.1")] <- "`Calculated.Run.time`"
file$`Calculated.Run.time` <- round(file$`Calculated.Run.time`, digits = 0)
file[file == ""] <- NA
file$LPRS.time <- as.POSIXct(as.character(file$LPRS.time), format = "%H:%M")
file$Production.Start <- as.POSIXct(as.character(file$Production.Start), format = "%H:%M")
file <- file[order(file$IssueDate),]
output <- data.frame(matrix(ncol = ncol(file), nrow = 0))
colnames(output) <- colnames(file)
for (j in unique(file$Folder)) {
  print (j)
  for (k in unique(file$IssueDate)) {
   # print (k)
    #browser()
    file.Folder <- file[which(file$Folder == j),]
    file.Folder <- file.Folder[which(as.character(file.Folder$IssueDate) == k),]
    
    
    
    
    if (nrow(file.Folder) == 0) { print ("NA")} else {
    file.Folder$sortindexprodstart <- as.POSIXct(substr(as.character(file.Folder$Production.Start - 12*60*60), start = 12, stop = 19), format = "%H:%M:%S")
    file.Folder$sortindexlprs <- as.POSIXct(substr(as.character(file.Folder$LPRS.time - 12*60*60), start = 12, stop = 19), format = "%H:%M:%S")
    
    #for (l in 1:nrow(file.Folder)){
    #if (is.na(file.Folder$Production.Start[l])) {file.Folder$sortindex[l] <- file.Folder$sortindexlprs[l]} else {
     # file.Folder$sortindex[l] <- file.Folder$sortindexprodstart[l]}
    
    
  file.Folder <- file.Folder[order(file.Folder$sortindexprodstart, file.Folder$sortindexlprs),]
  
  #file.Folder <- file.Folder[order(file.Folder$IssueDate,file.Folder$sortindex),]
  #file.Folder$groupindex <- ddply(file.Folder, .(LPRS.time), summarize, order(Production.Start))[,2]
  #file.Folder$row.num <- 1:nrow(file.Folder)
  #file.Folder1 <- ddply(file.Folder, .(LPRS.time), function(x) x[order(x$groupindex),])
  
  drop <- which(colnames(file.Folder) == "sortindexlprs")
    file.Folder <- file.Folder[,-c(drop,drop-1)]
    #test <- test+1
    
    #test2 <- test2 + 1
  if (is.na(file.Folder$Production.Start[1])) {
    file.Folder$start.time <- file.Folder$LPRS.time + lag*60} else { 
      file.Folder$start.time <- file.Folder$Production.Start}
  file.Folder$end.time <- file.Folder$start.time + file.Folder$`Calculated.Run.time`*60
  file.Folder$lag <- 0
  if (nrow(file.Folder) > 1) {
    for (i in 2:nrow(file.Folder)) {
      if (!is.na(file.Folder$Production.Start[i])) {
        file.Folder[i,"start.time"] <- file.Folder$Production.Start[i]
        file.Folder[i, "end.time"] <- file.Folder$Production.Start[i] + file.Folder[i,"Calculated.Run.time"]*60
      } else { 
        end.mark <- as.POSIXct(substr(as.character(file.Folder$end.time[i-1] - 12*60*60), start = 12, stop = 19), format = "%H:%M:%S")
        start.mark <- as.POSIXct(substr(as.character(file.Folder$LPRS.time[i] - 12*60*60), start = 12, stop = 19), format = "%H:%M:%S")
        fill.value <- max(end.mark, start.mark) 
        fill.value <- as.POSIXct(substr(as.character(fill.value+12*60*60 + 15*60), 12,19), format = "%H:%M:%S")
        file.Folder[i,"start.time"] <- fill.value
        file.Folder[i, "end.time"] <- file.Folder[i,"start.time"] + file.Folder[i,"Calculated.Run.time"]*60
        file.Folder[i, "lag"] <- file.Folder[i, "start.time"] - file.Folder[i-1, "end.time"]
        
        
      }
    }
  }
  #file.Folder <- file.Folder[order(file.Folder$IssueDate,file.Folder$sortindex),]
  if (nrow(file.Folder) > 1) {
     for (r in 2:nrow(file.Folder)) { 
       #print("checkerror")
    start <- substr(as.character(file.Folder$start.time[r] - 12*60*60), start = 12, stop = 19)
    end <- substr(as.character(file.Folder$end.time[r-1] - 12*60*60), start = 12, stop = 19)
    start.hours <- substr(start, 1,2)
    start.minutes <- substr(start, 4,5)
     end.hours <- substr(end, 1,2) 
    end.minutes <- substr(end, 4,5)
     #x <- seconds_to_period(seconds(file.Folder$start.time[r]) - seconds(file.Folder$end.time[r-1]))
    if (start.minutes >= end.minutes){
   file.Folder$lag[r] <- paste(as.integer(start.hours)-as.integer(end.hours), "Hours", as.integer(start.minutes)-as.integer(end.minutes), "Minutes", sep = " ")} else {
     file.Folder$lag[r] <- paste(as.integer(start.hours)-as.integer(end.hours) - 1, "Hours", 60 + as.integer(start.minutes)-as.integer(end.minutes), "Minutes", sep = " ") 
   }
    #if (hm(as_datetime(file.Folder$lag[r])) > hours(5)) {file.Folder$lag[r] <- NA}
    
     } 
  } else {file.Folder$lag[1] <- 0 }
  output <- rbind(output, file.Folder)
  }
  }
}
output$Estimated.production.end <- output$end.time
#output <- output[,-which(colnames(output) == "sortindex")]
#write.csv(output[,-which(colnames(output) == "end.time")], "testout2.csv")
output.max.Folder <- output
output.max.Folder$end.time <- output.max.Folder$end.time - 12*60*60
output.max.Folder$end.time <- strftime(output.max.Folder$end.time, format = "%H:%M:%S")
maxfolder.night <- ddply(output.max.Folder, .(IssueDate), summarize, Value = max(end.time, na.rm = T))

output.max.Folder$key <- paste(output.max.Folder$IssueDate, output.max.Folder$Folder)

maxfolder.folder <- ddply(output.max.Folder, .(key), summarize, Value = max(end.time, na.rm = T))

maxfolder.folder[,2] <- strftime((as.POSIXct(as.character(maxfolder.folder[,2]), format = "%H:%M:%S") + 12*60*60), format = "%H:%M:%S")
maxfolder.night[,2] <- strftime((as.POSIXct(as.character(maxfolder.night[,2]), format = "%H:%M:%S") + 12*60*60), format = "%H:%M:%S")
maxfolder.folder$date <- substr(maxfolder.folder$key, 1,10)
maxfolder.folder$folder <- substr(maxfolder.folder$key, 11,18)
maxfolder.folder <- maxfolder.folder[c(3,4,2)]
folderlist<-c("Press 1", "Press 2", "Press 3", "Press 4", "Press 5")
datelist <- maxfolder.night$IssueDate
maxfolder.folder.columns.a <- do.call(paste0, expand.grid(datelist, folderlist))
maxfolder.folder.columns <- data.frame(date=substr(maxfolder.folder.columns.a,1,10), folder = substr(maxfolder.folder.columns.a,11,17))
#check all are TRUE

maxfolder.folder.columns$key <- gsub(" ","",paste(maxfolder.folder.columns$date, maxfolder.folder.columns$folder))
maxfolder.folder$key <- gsub(" ","",paste(maxfolder.folder$date, maxfolder.folder$folder))
maxfolder.folder.columns$Value <- "NA"
for (i in 1:nrow(maxfolder.folder.columns)){
  #print (i)
  k <- which(maxfolder.folder.columns$key[i] == maxfolder.folder$key)
    if (!(length(k) == 0)) {
      maxfolder.folder.columns$Value[i] <- maxfolder.folder$Value[k]} else {
      maxfolder.folder.columns$Value[i] <- "NA"}
}
maxfolder.folder.columns <- maxfolder.folder.columns[order(maxfolder.folder.columns$date),]
maxfolder.folder.columns <- maxfolder.folder.columns[,-3]
colnames(maxfolder.folder.columns) <- c("Issue_Date", "Folder" ,"Time")
colnames(maxfolder.night) <- c("Issue_Date", "Time" )


#Inserting estimated_production_end and Estimated_production_start to todb_file
#todb_file$mkey <- paste(todb_file$IssueDate, gsub("\"","",todb_file$Folder), todb_file$Edition,sep="")



mergeset1 <- data.frame(InputId = output$InputId, Predicted_avgspeed = output$Predicted.avgspeed,
                        Calculated_Run_time = output$Calculated.Run.time,
                        Estimated_production_start = output$start.time,
                        Estimated_production_end = output$Estimated.production.end
                        )
mergeset1$Estimated_production_start <- substr(as.character(mergeset1$Estimated_production_start), 12,16)
mergeset1$Estimated_production_end <- substr(as.character(mergeset1$Estimated_production_end), 12,16)
#output$Actual_Production_Finish <- substr(as.character(output$Actual_Production_Finish), 1, 5)
#output$Production_Start <- substr(as.character(output$Production_Start), 1, 5)
dbWriteTable(mydb, name='mergeset', value =mergeset1, overwrite=T, row.names = F)

dbSendQuery(mydb, "UPDATE input t1 
              INNER JOIN mergeset t2 
            ON t1.InputId = t2.InputId
            SET t1.Predicted_avgspeed = t2.Predicted_avgspeed,
            t1.Calculated_Run_time = t2.Calculated_Run_time,
            t1.Estimated_production_start = t2.Estimated_production_start, 
            t1.Estimated_production_end = t2.Estimated_production_end ")




#for writing entire data table
#mergeset1$sortnumber <- 1:nrow(todb_file)
#todb_file <- merge(todb_file, mergeset1, by = "mkey")
#todb_file<- todb_file[order(todb_file$sortnumber),]
#todb_file <- todb_file[,-which(colnames(todb_file) == "sortnumber")]
#todb_file$Estimated_production_end <- todb_file1$Estimated.production.end1
#todb_file$Estimated_production_start <- todb_file1$Estimated_production_start
#todb_file <- todb_file[,-(which(colnames(todb_file)=="mkey"))]
#todb_file$Estimated_production_end <- substr(as.character(todb_file$Estimated_production_end), 12,17)
#todb_file <- todb_file[order(todb_file$IssueDate, todb_file$Folder, todb_file$Production_Start),]

#write to db
dbWriteTable(mydb, name='prediction_table', value=maxfolder.folder.columns, overwrite = T, row.names = F)
#dbWriteTable(mydb, name='mergeset', value=mergeset1, overwrite=T, row.names = F)
dbWriteTable(mydb, name='prediction_table_night', value=maxfolder.night, overwrite = T, row.names = F)
#dbwritetable(mydb, name = 'mergeset', value = mergeset1)


#write.csv(maxfolder.folder.columns, file = "Maxfolder.csv")
#write.csv(maxfolder.night, file = "Nightfolder.csv")
#write.csv(todb_file, file = "testout3.csv")


#Disconnect all db connections
#dbDisconnectAll <- function(){
  #ile <- length(dbListConnections(MySQL())  )
  #lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
 # cat(sprintf("%s connection(s) closed.\n", ile))
#}

#dbDisconnectAll()
#checkfile
#check <- data.frame(output$IssueDate,output$Folder ,output$LPRS.time, prodstart = output$Production.Start, output$Calculated.Run.time, output$start.time,output$Estimated.production.end)
