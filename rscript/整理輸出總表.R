##處理簡化表格文字內容

rm(list = ls())
gc() 

options(stringsAsFactors = FALSE)
setwd("JobMining")

##Returns string without leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
##Something like trim, but will remove punctuations except quotation marks : () （）
trim_punc <- function (x){
  #gsub("^[[:punct:]]+|[[:punct:]]+$", "", x)
  gsub("([()（）])|^[[:punct:]]+|[[:punct:]]+$", "\\1", x)
}

job_d <- read.csv(".\\分行業別output\\工作說明總表.csv")

##Leading minus sign will cause problem in Excel.
##job_d$工作說明 <- gsub("-","*",job_d$工作說明)
job_d$工作說明 <- trim_punc(job_d$工作說明)
job_d$工作說明 <- trim(job_d$工作說明)
job_d$工作說明 <- gsub(",", "，", job_d$工作說明)

if(F){
  ##Comparing strings.
  ##If both of it just make difference in one char, remove one of the string...
  ##比較字串，只有一個字有差就踢掉其一
  uni_job_list <- unique(job_d[,c("職務名稱","行業別")])
  
  for(i in 1:nrow(uni_job_list)){
    split_tmp <- strsplit(job_d$工作說明[which(job_d$職務名稱==uni_job_list$職務名稱[i] & job_d$行業別==uni_job_list$行業別[i])], "")  
  }
}

write.csv(job_d, ".\\分行業別output\\工作說明處理後總表.csv", row.names=F)

##Fuzzy matching
job_d_list <- job_d[,1:2]
job_d_list <- unique(job_d_list)

for(i in 1:nrow(job_d_list)){
  tmp            <- job_d[which(job_d[,1]==job_d_list[i,1] & job_d[,2]==job_d_list[i,2]), ]
  wordlist       <- expand.grid(words = tmp[,3], ref = tmp[,3], stringsAsFactors = FALSE)
  fuzzy_matching <- wordlist %>% mutate(match_score = jarowinkler(words, ref))
  
  ##Why using tostring and as numeric? 
  ##Because sometimes the comparing is wronge
  fuzzy_matching <- fuzzy_matching[which(fuzzy_matching$match_score >=0.9 & fuzzy_matching$match_score < 1), ]
  ##Maybe 0.9 is to high...
  fuzzy_matching <- fuzzy_matching[which(fuzzy_matching[,1]!=fuzzy_matching[,2]), ]
  ##fuzzy_matching[order(fuzzy_matching$match_score),]
  if(nrow(fuzzy_matching)>0){
    #apply(fuzzy_matching, 1, function(x) print(paste0(x[1]," <==> ",x[2])))
    
    #export_df = rbind(export_df,fuzzy_matching)
    #sink("D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\整體工作說明fuzzymatch紀錄.txt",append=TRUE)                      
    #apply(fuzzy_matching, 1, function(x) print(paste0(x[1]," <==> ",x[2])))
    #sink()
        
    get_min_nchar <- apply(fuzzy_matching, 1, function(x){
      if(nchar(x[1]) > nchar(x[2])){
        return(x[2])
      }else{
        return(x[1])
      }
    })
    get_min_nchar <- unique(as.vector(get_min_nchar))
    job_d         <- setdiff(job_d,tmp[which(get_min_nchar %in% tmp$工作說明), ])
    #tmp[which(get_min_nchar %in% job_d$工作說明),]
    #job_d = job_d[which(job_d[,1]==job_d_list[i,1] & job_d[,2]==job_d_list[i,2] & !get_min_nchar %in% job_d$工作說明),]
  }
  print(paste0(i/nrow(job_d_list)*100, "%"))
}

##write.csv(export_df,"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\整體工作說明fuzzymatch紀錄.csv",row.names=F)
nrow(job_d)
job_d$工作說明 <- gsub("[*]","-", job_d$工作說明)
write.csv(job_d, ".\\分行業別output\\整體工作說明fuzzymatch後整理結果.csv",row.names=F)
