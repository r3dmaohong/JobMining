##處理簡化表格文字內容

rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放 

options(stringsAsFactors = FALSE)
path<-"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki"

##去除前後標點除了引號
trim_punc <- function (x){
  #gsub("^[[:punct:]]+|[[:punct:]]+$", "", x)
  gsub("([()（）])|^[[:punct:]]+|[[:punct:]]+$", "\\1", x)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

job_d <- read.csv('D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\工作說明總表.csv')

##excel內 - 開頭會有問題
#job_d$工作說明 = gsub('-','*',job_d$工作說明)
job_d$工作說明 = trim_punc(job_d$工作說明)
job_d$工作說明 = trim(job_d$工作說明)
job_d$工作說明 = gsub(',','，',job_d$工作說明)

if(F){
  ##比較字串，只有一個字有差就踢掉其一
  uni_job_list = unique(job_d[,c('職務名稱','行業別')])
  
  for(i in 1:nrow(uni_job_list)){
    split_tmp = strsplit(job_d$工作說明[which(job_d$職務名稱==uni_job_list$職務名稱[i] & job_d$行業別==uni_job_list$行業別[i])],'')
    
  }
}


write.csv(job_d,'D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\工作說明處理後總表.csv',row.names=F)
