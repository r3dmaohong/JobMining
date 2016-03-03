##簡化廠商職務大蒐秘輸出表格

rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放 

path<-"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki"
#讀取多個csv至df

##工作說明 分行業
data_path<-paste0(path,"\\分行業別output\\工作說明")
setwd(data_path)
csv_list = list.files(pattern="*對應結果.csv")
#myfiles = lapply(temp, read.delim)
for(i in 1:length(csv_list)){
  temp = read.csv(csv_list[i],stringsAsFactors=F)
  temp = temp[,2]
  temp = unique(temp)
  temp = temp[!is.na(temp)]
  temp = cbind(substr(csv_list[i],1,unlist(gregexpr(pattern =' -',csv_list[i]))[1]-1),substr(csv_list[i],unlist(gregexpr(pattern ='- ',csv_list[i]))[1]+2,unlist(gregexpr(pattern ='工作說明',csv_list[i]))[1]-1),temp)
  colnames(temp) = c('職務名稱','行業別','工作說明')
  temp = temp[order(temp[,3]),]
  write.csv(temp,paste0('簡化\\',gsub('對應','簡化',csv_list[i])),row.names=F)
  print(paste0('工作說明 分行業',i/length(csv_list)*100,' %'))
}

##工作說明 整體
data_path<-paste0(path,"\\分行業別output\\工作說明\\整體")
setwd(data_path)
csv_list = list.files(pattern="*對應結果.csv")
#myfiles = lapply(temp, read.delim)
for(i in 1:length(csv_list)){
  temp = read.csv(csv_list[i],stringsAsFactors=F)
  temp = temp[,2]
  temp = unique(temp)
  temp = temp[!is.na(temp)]
  temp = cbind(substr(csv_list[i],1,unlist(gregexpr(pattern ='工作說明',csv_list[i]))[1]-1),'不分行業',temp)
  colnames(temp) = c('職務名稱','行業別','工作說明')
  temp = temp[order(temp[,3]),]
  write.csv(temp,paste0('簡化\\',gsub('對應','簡化',csv_list[i])),row.names=F)
  print(paste0('工作說明 不分行業',i/length(csv_list)*100,' %'))
}

##附加條件 分行業
data_path<-paste0(path,"\\分行業別output\\附加條件")
setwd(data_path)
csv_list = list.files(pattern="*對應結果.csv")
#myfiles = lapply(temp, read.delim)
for(i in 1:length(csv_list)){
  temp = read.csv(csv_list[i],stringsAsFactors=F)
  temp = temp[,2]
  temp = unique(temp)
  temp = temp[!is.na(temp)]
  temp = cbind(substr(csv_list[i],1,unlist(gregexpr(pattern =' -',csv_list[i]))[1]-1),substr(csv_list[i],unlist(gregexpr(pattern ='- ',csv_list[i]))[1]+2,unlist(gregexpr(pattern ='附加條件',csv_list[i]))[1]-1),temp)
  colnames(temp) = c('職務名稱','行業別','附加條件')
  temp = temp[order(temp[,3]),]
  write.csv(temp,paste0('簡化\\',gsub('對應','簡化',csv_list[i])),row.names=F)
  print(paste0('附加條件 分行業',i/length(csv_list)*100,' %'))
}

##附加條件 整體
data_path<-paste0(path,"\\分行業別output\\附加條件\\整體")
setwd(data_path)
csv_list = list.files(pattern="*對應結果.csv")
#myfiles = lapply(temp, read.delim)
for(i in 1:length(csv_list)){
  temp = read.csv(csv_list[i],stringsAsFactors=F)
  temp = temp[,2]
  temp = unique(temp)
  temp = temp[!is.na(temp)]
  temp = cbind(substr(csv_list[i],1,unlist(gregexpr(pattern ='附加條件',csv_list[i]))[1]-1),'不分行業',temp)
  colnames(temp) = c('職務名稱','行業別','附加條件')
  temp = temp[order(temp[,3]),]
  write.csv(temp,paste0('簡化\\',gsub('對應','簡化',csv_list[i])),row.names=F)
  print(paste0('附加條件 不分行業',i/length(csv_list)*100,' %'))
}


##輸出總表

##工作說明 分行業
data_path<-paste0(path,"\\分行業別output\\工作說明\\簡化")
setwd(data_path)
csv_list = list.files(pattern="*簡化結果.csv")
myfiles = {}
for(i in 1:length(csv_list)){
  tryCatch({
    temp = read.csv(csv_list[i],stringsAsFactors=F)
    myfiles = rbind(myfiles,temp)
    print(paste0('整合檔案第一部分',i/length(csv_list)*100,'%'))
  }, error = function(e) {
    print(paste0(csv_list[i],'錯誤'))
  })

}
##工作說明 整體
data_path<-paste0(path,"\\分行業別output\\工作說明\\整體\\簡化")
setwd(data_path)
csv_list = list.files(pattern="*簡化結果.csv")
for(i in 1:length(csv_list)){
  tryCatch({
    temp = read.csv(csv_list[i],stringsAsFactors=F)
    myfiles = rbind(myfiles,temp)
    print(paste0('整合檔案第二部分',i/length(csv_list)*100,'%'))
  }, error = function(e) {
    print(paste0(csv_list[i],'錯誤'))
  })
  
}
write.csv(myfiles,'D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\工作說明總表.csv',row.names=F)


##輸出總表

##附加條件 分行業
data_path<-paste0(path,"\\分行業別output\\附加條件\\簡化")
setwd(data_path)
csv_list = list.files(pattern="*簡化結果.csv")
myfiles = {}
for(i in 1:length(csv_list)){
  tryCatch({
    temp = read.csv(csv_list[i],stringsAsFactors=F)
    myfiles = rbind(myfiles,temp)
    print(paste0('整合檔案第一部分',i/length(csv_list)*100,'%'))
  }, error = function(e) {
    print(paste0(csv_list[i],'錯誤'))
  })
  
}
##附加條件 整體
data_path<-paste0(path,"\\分行業別output\\附加條件\\整體\\簡化")
setwd(data_path)
csv_list = list.files(pattern="*簡化結果.csv")
for(i in 1:length(csv_list)){
  tryCatch({
    temp = read.csv(csv_list[i],stringsAsFactors=F)
    myfiles = rbind(myfiles,temp)
    print(paste0('整合檔案第二部分',i/length(csv_list)*100,'%'))
  }, error = function(e) {
    print(paste0(csv_list[i],'錯誤'))
  })
  
}
write.csv(myfiles,'D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\附加條件總表.csv',row.names=F)
