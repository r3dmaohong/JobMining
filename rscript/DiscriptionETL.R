####################
####################
##1-1 Data Transform
####################
####################

rm(list = ls())
gc()
options(stringsAsFactors = FALSE)
setwd("JobMining")

##對應=>簡化
##type: 工作說明 or 附加條件
##"total = T" => 整體
##"total = F" => 分行業
OutputSimplify <- function(type, total = F){
  FNamePlus<- ifelse(total, "\\整體", "")
  ##Read mutiple csv files to one DF
  csv_list <- list.files(paste0(".\\分行業別output\\", type, FNamePlus), pattern="*對應結果.csv")
  #myfiles <- lapply(temp, read.delim)
  for(i in 1:length(csv_list)){
    temp <- read.csv(csv_list[i], stringsAsFactors=F)
    temp <- temp[, 2]
    temp <- unique(temp)
    temp <- temp[!is.na(temp)]
    temp <- cbind(substr(csv_list[i], 1, unlist(gregexpr(pattern = " -", csv_list[i]))[1]-1), substr(csv_list[i], unlist(gregexpr(pattern = "- ", csv_list[i]))[1]+2, unlist(gregexpr(pattern = type, csv_list[i]))[1]-1), temp)
    colnames(temp) <- c("職務名稱", "行業別", type)
    temp <- temp[order(temp[, 3]), ]
    write.csv(temp, paste0(".\\分行業別output\\", type, FNamePlus, "簡化\\", gsub("對應", "簡化", csv_list[i])), row.names=F)
    cat("\r", type, ifelse(total, "不分行業", "分行業"), i/length(csv_list)*100, " %", rep(" ", 50))
  }
  cat("\n")
}

##Job discriptions with industry
OutputSimplify("工作說明", total=F)
##Job discriptions without industry
OutputSimplify("工作說明", total=T)

##Export total form
CombinationExport <- function(type){
  csv_list <- list.files(paste0(".\\分行業別output\\", type, "\\簡化"), pattern="*簡化結果.csv")
  csv_list <- c(csv_list, list.files(paste0(".\\分行業別output\\", type, "\\整體\\簡化"), pattern="*簡化結果.csv"))
  myfiles <- {}
  for(i in 1:length(csv_list)){
    tryCatch({
      temp    <- read.csv(csv_list[i], stringsAsFactors=F)
      myfiles <- rbind(myfiles, temp)
      cat("\r", type, "整合檔案 ", i/length(csv_list)*100, "%", rep(" ", 50))
    }, error = function(e) {
      print(paste0(csv_list[i], "錯誤"))
    })
  }
  myfiles <- unique(myfiles)
  write.csv(myfiles, paste0(".\\分行業別output\\", type, "總表.csv"), row.names=F)
}

##Job Discriptions with and without industry
CombinationExport("工作說明")

####################
####################
##2-1 Data Cleaning
####################
####################

if(T){
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
}

####################
####################
##2-2 Data Cleaning
####################
####################

if(T){
  library(RecordLinkage)
  library(dplyr)
  library(jiebaR)
  cutter = worker()
  
  ##Custom trim function
  trim <- function (x){
    gsub("^[　���P＞､δ//s]+|[　���Pδ､//s]+$", "",gsub("^\\s+|\\s+$", "", x))
  } 
  ##triming star symbols
  trim_star <- function (x) gsub("^[*]+|[*]+$", "", x)
  ##Trimming punctuations except quotation marks : () （）
  trim_punc <- function (x){
    #gsub("^[[:punct:]]+|[[:punct:]]+$", "", x)
    gsub("([()（）])|^[[:punct:]]+|[[:punct:]]+$", "\\1", x)
  }
  ##Custom trim with mixed factors
  trim_mix <- function(x){
    x <- gsub("^\\)+|\\(+$", "", x)
    ##Sometimes have to trim multiple times...
    x <- gsub("∼", " ", x)
    x <- gsub("^[0-9０-９a-zA-Z一-十四√]{1,2}[:、､，,• ．)）-＞]", "", x)
    x <- gsub("^[0-9０-９a-zA-Z一-十四√]{1,2}[.]", "", x)
    x <-  gsub("^[0-9０-９a-zA-Z一-十四√]{1,2}-", "", x)
    ##New discovery!! Numbers used by Chinese is functional.
    ##But the word "四" has failed.
    x <- gsub("^[(（][0-9０-９a-zA-Z一-十四ㄧ]{1,2}[)）]", "", x)
    x <- gsub("^◎", "", x)
    
    if(grepl("^[(（＜]+", x) & grepl("[)）＞]+$", x)){
      x <- gsub("^[(（＜]+|[)）＞]+$", "", x)
    }
    x <- gsub("^[0-9０-９]{1,2}[-][0-9０-９]{1,2}", "", x)
    
    if(grepl("\\)", x) & grepl("\\(", x) & length(unlist(gregexpr("\\)", x)))==1 & length(unlist(gregexpr("\\(", x)))==1){
      if(unlist(gregexpr("\\)", x)) < unlist(gregexpr("\\(", x))){
        x <- "" ##It will be removed at the final stage...
      }
    }
    if(grepl("[0-9][0-9]:[0-9][0-9]", x)){
      x <- ""
    }
    ##Remove Date
    if(grepl("[0-9][/][0-9]", x)){
      x <- ""
    }
    ##》 but no 《, or 《 but no 》
    if(grepl("《", x) & !grepl("》", x)){
      x <- unlist(strsplit(x, "《"))[1]
    }else if(!grepl("《", x) & grepl("》", x)){
      x <- unlist(strsplit(x, "》"))[length(unlist(strsplit(x, "》")))]
    }else{
      ##
    }
    # Their's problem
    #if(length(nchar(unlist(lapply(strsplit(x,","),trim)))[which(nchar(unlist(lapply(strsplit(x,","),trim)))==1)])>0){
    #  x = unlist(lapply(strsplit(x,","),trim))[which(nchar(unlist(lapply(strsplit(x,","),trim)))==max(nchar(unlist(lapply(strsplit(x,","),trim)))))][1]
    #}
    return(x)
  }
  trim_du <- function(x){
    ##Removed when occur multiple times?
    #if(length(unlist(gregexpr(pattern ="[0-9０-９a-zA-Z一-十四√]+[:、，,-• ．)）.]",x)))>1 | length(unlist(gregexpr(pattern ="[0-9０-９a-zA-Z]+[.]",x)))>1){
    ##if(grepl("[0-9０-９a-zA-Z一-十四√]+[:、，,-• ．)）.＞]",x)){
    if(grepl("[0-9０-９a-zA-Z一-十四√]{1,2}[:•．＞]", x) | grepl("[0-9０-９a-zA-Z一-十四√]{1,2}-", x) | grepl("[0-9０-９a-zA-Z一-十四√]{1,2}[.]", x)){
      x <- ""
    }else{
      #x <- x
    }
    return(x)
  }
  ##Using jiebaR to remove words?
  remove_head_num_jiebar <- function(x){
    if(grepl("^[0-9]", x)){
      if(substr(x,2,2) %in% c("年", "萬", "噸", "頓", "歲", "小")){
      }else if(substr(x,2,2) %in% c("具")){
        x <- gsub("^[0-9]", "", x)
      }else{
        tmp = cutter <= x
        if(nchar(tmp[2]) != 1){
          x <- gsub("^[0-9]", "", x)
        }
      }
    }
    return(x)
  }
  
  ##What file!?
  ##I forgot...
  ##The code below is to remove useless discription
  ##Therefore the file should be discriptionMining()"s output or so.
  #job_d  <- read.csv(file.choose())
  job_d <- read.csv(".\\分行業別output\\整體工作說明fuzzymatch後整理結果.csv", stringsAsFactors=F)
  
  job_d <- job_d[which(!grepl("[0-9]+$", job_d[,3])), ]
  #toMatch= c("口試", "簡章", "台中", "台北", "台東", "台南",  "宜蘭",  "東沙",  "花蓮",	"金門",	"南投",	"南沙",	"屏東",	"苗栗",	"桃園",	"烏坵",	"馬祖",	"高雄",	"基隆",	"雲林",	"新竹",	"嘉義",	"彰化",	"澎湖",	"臺中",	"臺北",	"臺東",	"臺南", "捷運", "嘉裕", "先生", "小姐", "？", "?", "$", "短期", "報名時間", "www", "@", "com", "面試地點", "意者", "速洽", "待優", "上班日", "連絡電話", "聯絡方式", "均薪", "保底", "創立於", "1111", "asp", "店", "熱情招募", "日薪", "我們公司",	"月休",	"待遇佳",	"底薪",	"營業時間",	"來電",	"培訓期間薪資",	"面試時間",	"有限公司",	"津貼",	"誠徵", "小時", "工作待遇", "月薪", "工作時段", "午休", "時薪", "工作地區", "工作地點", "時段", "歡迎", "履歷", "工作內容", "本公司", "目前", "你好", "大家好", "工作時間", "獎金", "地址", "上班時間", "準時開始", "計費方式")
  #for(i in 1:length(toMatch)){
  #  job_d <- job_d[which(!grepl(toMatch[i], job_d[,3],fixed=T)), ]
  #}
  
  ##Remove discriptions with Date or Time.
  ##job_d <- job_d[which(!grepl("[0-9０-９]{4}",job_d[,3])), ]
  #job_d <- job_d[which(!grepl("[0-9]點[0-9]", job_d[, 3])), ]
  #job_d <- job_d[which(!grepl("[０-９]點[０-９]",job_d[, 3])), ]
  #job_d <- job_d[which(!grepl("[0-9０-９]：[0-9０-９]", job_d[, 3])), ]
  #job_d <- job_d[which(!grepl("[０-９]：[０-９]", job_d[, 3])), ]
  #job_d <- job_d[which(!grepl("[0-9０-９]:[0-9０-９]", job_d[, 3])), ]
  #job_d <- job_d[which(!grepl("^[0-9０-９]{3}", job_d[, 3])), ]
  #job_d <- job_d[which(!grepl("[0-9０-９]{2}年", job_d[, 3])), ]
  
  job_d <- job_d[which(!(grepl("[0-9０-９]{5,}",job_d[,3]) & grepl("[a-zA-Z]",job_d[,3]))), ]
  
  toMatch = c("玉山", "信義", "吉源", "面試", "班",	"聯絡",	"金",	"日",	"休",	"手機",	"週",	"起",	"~",	"-",	"line",	"電", "元", "薪")
  for(i in 1:length(toMatch)){
    if(grepl("[a-z]", tolower(toMatch[i]))){
      job_d <- job_d[which(!(grepl("[0-9０-９]{3,}", job_d[,3]) & !grepl("0800",job_d[,3]) & grepl(tolower(toMatch[i]), tolower(job_d[,3]), fixed=T))), ]
    }else{
      job_d <- job_d[which(!(grepl("[0-9０-９]{3,}", job_d[,3]) & !grepl("0800",job_d[,3]) & grepl(toMatch[i], job_d[,3], fixed=T))), ]
    }
  }
  job_d <- job_d[which(!(grepl("[0-9０-９]{3,}", job_d[,3]) & !grepl("0800", job_d[,3]) & !grepl("[a-zA-Z]", job_d[,3]))), ]
  job_d <- job_d[which(!(grepl("[0-9０-９]{3,}", job_d[,3]) & grepl("班", job_d[,3]))), ]
  
  #tmp = job_d[which(grepl("[(（]",job_d[,3]) & !grepl("[)）]",job_d[,3])),3]
  job_d[which(grepl("[(（]", job_d[,3]) & !grepl("[)）]", job_d[,3])),3] <- 
    unlist(lapply(job_d[which(grepl("[(（]", job_d[,3]) & !grepl("[)）]", job_d[,3])), 3],function(x){
      x <- trim(substr(x, 1, unlist(gregexpr(pattern ="[(（]",x))[length(unlist(gregexpr(pattern ="[(（]",x)))] - 1))
      if(nchar(x)<=4){
        x=""
      }
      return(x)
    }))
  
  ##Fuzzy matching
  ##Job and Industry...
  job_d_list <- job_d[,1:2]
  job_d_list <- unique(job_d_list)
  
  ##Empty DF
  new_job_df = job_d[0, ]
  
  if(T){
    for(times in 1:2){
      job_d[, 3] <- trim(job_d[, 3])
      job_d[, 3] <- trim_star(job_d[, 3])
      job_d[, 3] <- trim_punc(job_d[, 3])
      job_d[, 3] <- trim_mix(job_d[, 3])
      #for(i in 1:nrow(job_d)){
      #  job_d[i, 3] <- trim_du(job_d[i, 3])
      #}
      job_d[, 3] <- unlist(lapply(job_d[, 3], trim_du))
    }
    
    ##This block of code is for "Other needs"
    #job_d <- job_d[which(!grepl(".com",job_d[, 3],fixed=T) & !grepl("電話",job_d[, 3]) & !grepl("來電",job_d[, 3]) & !grepl("font",job_d[, 3]) & !grepl("電話",job_d[, 3]) & !grepl("e-mail",job_d[, 3]) & !grepl("【",job_d[, 3]) & !grepl("★",job_d[, 3]) & !grepl("☆",job_d[, 3]) & !grepl("◆",job_d[, 3]) & !grepl("■",job_d[, 3]) & !grepl("】",job_d[, 3])), ]
    job_d <- job_d[which(!grepl(".com",job_d[, 3],fixed=T) & !grepl("font",job_d[, 3]) & !grepl("【",job_d[, 3]) & !grepl("★",job_d[, 3]) & !grepl("☆",job_d[, 3]) & !grepl("◆",job_d[, 3]) & !grepl("■",job_d[, 3]) & !grepl("】",job_d[, 3])), ]
    
    job_d <- job_d[which(job_d[, 3]!=""), ]
    
    #job_d <- job_d[which(!grepl("^[(（＜?？]+",job_d[, 3])), ]
    #job_d <- job_d[which(!grepl("[?？]+",job_d[, 3])), ]
    job_d <- job_d[which(nchar(job_d[, 3]) > 4), ]
    
    job_d[, 3] <- unlist(lapply(job_d[, 3],remove_head_num_jiebar))
    
    #job_d <- job_d[which(!grepl("^[(（]",job_d[, 3])), ]
    #job_d <- job_d[which(!grepl("^的",job_d[, 3])), ]
    #job_d <- job_d[which(!grepl("^及",job_d[, 3])), ]
    
    job_d[, 3] <- gsub("　"," ",job_d[, 3])
    job_d[, 3] <- gsub("[~]+","，",job_d[, 3])
    job_d[, 3] <- gsub("＊","、",job_d[, 3])
    
    job_d[which(grepl("計時服務員",job_d[, 3])), 3] <- "計時服務員"
  }
  
  ##job_d_list
  ##Job and Industry...
  x = 1
  for(i in 1:nrow(job_d_list)){
    tmp <- job_d[which(job_d[,1]==job_d_list[i,1] & job_d[,2]==job_d_list[i,2]), ]
    if(nrow(tmp)<6){
      job_d <- job_d[which(!(job_d[,1]==job_d_list[i,1] & job_d[,2]==job_d_list[i,2])), ]
    }else{
      ##Remove similar discriptions...
      wordlist       <- expand.grid(words = tmp[,3], ref = tmp[,3], stringsAsFactors = FALSE)
      fuzzy_matching <- wordlist %>% mutate(match_score = jarowinkler(words, ref))
      fuzzy_matching <- fuzzy_matching[order(fuzzy_matching$match_score), ]
      fuzzy_matching <- fuzzy_matching[which(fuzzy_matching[,3]<0.9999), ]
      #fuzzy_matching[,1]  
      #fuzzy_matching[,2]
      #ord_v = unique(trim(unlist(strsplit(paste(fuzzy_matching[,1],"。", fuzzy_matching[,2]),"。"))))
      
      ##Max score side as the head side...
      #unique(rev(fuzzy_matching[seq(1, nrow(fuzzy_matching), 2),1]))
      ord_v <- unique(rev(fuzzy_matching[,1]))
      
      temp = c()
      for(j in 1:length(unique(substr(ord_v,1,5)))){
        temp = c(temp, ord_v[which(unique(substr(ord_v,1,5))[j]==substr(ord_v,1,5))][1])
      }
      
      ord_v <- temp[1:20]
      ord_v <- sort(ord_v)
      ord_v <- ord_v[!is.na(ord_v)]
      
      ord_v_rm = c()
      for(check_i in 1:length(ord_v)){
        ##Extract "grepl=True" but different with the original one?
        if(length(which(grepl(ord_v[check_i],ord_v,fixed=TRUE)))==1){
          ord_v_rm <- c(ord_v_rm, ord_v[check_i])
        }
      }
      ord_v <- ord_v_rm
      
      new_job_df[x:(x+length(ord_v)-1), 1:2] <- tmp[1:(length(ord_v)), 1:2]
      new_job_df[x:(x+length(ord_v)-1), 3]   <- ord_v
      
      cat(paste0("\r", format(round(i/nrow(job_d_list)*100,3),nsmall=3), "%"))
      x <- x + length(ord_v)
    }
  }
  
  #write.csv(new_job_df,".\\分行業別output\\[edit篩選後4]整體工作說明fuzzymatch後整理結果.csv",row.names=F)
  write.csv(new_job_df,".\\分行業別output\\篩選後_工作說明總表.csv",row.names=F)
}