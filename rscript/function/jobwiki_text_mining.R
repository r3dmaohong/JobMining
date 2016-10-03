##jobwiki mining main program
library(gtools)

##Sets min amount of sample, if smaller than the value, ignore it.
min_n_sample = 100

#Returns string without leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#gc
jgc <- function(){
  gc()
  .jcall("java/lang/System", method = "gc")
}  

##Extracts data
jobDataExtraction <- function(people, total=F){
  jgc()
  ##Industry and job
  if(total){
    jobData <- as.data.frame(table(people$職務小類), stringsAsFactors=F)
  }else{
    jobData <- as.data.frame(table(people$行業與職務), stringsAsFactors=F)
  }
  high_Freq_jobData <- jobData[which(jobData$Freq>=min_n_sample), ]
  jobData           <- c(jobData$Var1)
  jobData           <- jobData[which(jobData!="NULL")]
  
  print(paste0("jobs' sample more than 100 remains ", length(jobData), " items.."))
  return(jobData)
}

##Job discription or other needs with industry...
##Type: 工作說明 or 附加條件
discriptionMining <- function(people, jobVector, type, total = F){
  min_n_sample <- ifelse(type=="工作說明", 100, 70)
  #if(total){
  #  jobVector <- job_only
  #}else{
  #  jobVector <- job
  #}
  
  for(job_i in 1:length(jobVector)){
    tryCatch({
      jgc()
      print(paste0(job_i, ". ", jobVector[job_i], " is under text mining : ", type))
      if(total){
        people_sep <- people[which(people$職務小類==job_only[job_i]),]
      }else{
        people_sep <- people[which(people$行業與職務==jobVector[job_i]),]
      }        
      setDF(people_sep)
      
      if(nrow(people_sep) > min_n_sample){
        ##computer can't support more than 15000 items of data while doing the following analysis...
        if(nrow(people_sep)>15000){
          people_sep <- people_sep[sample(1:nrow(people_sep),15000),]
        }
        
        people_sep[,type] <- gsub("\x9e", "  ", people_sep[,type])
        ##Removes english discriptions
        en_remove_index <-{}
        for(en.remove in 1:length(people_sep[,type])){
          if(length(gregexpr(pattern ="[a-z]", people_sep[,type])[[en.remove]])/nchar(people_sep[,type][en.remove])>0.5){
            en_remove_index <- c(en_remove_index,en.remove)
          }
        }
        if(toString(en_remove_index)==""){
          #No english discriptions to be removed.
        }else{
          people_sep <- people_sep[-en_remove_index,]
        }
        job_all_name = substr(jobVector[job_i], 1, unlist(gregexpr(" - ", jobVector[job_i]))-1)
        #min_nrow = mean(as.data.frame(table(people[which(people$職務小類==job_all_name)]$行業與職務))$Freq)-2*sd(as.factor(people[which(people$職務小類==job_all_name)]$行業與職務))
        
        review_text <- paste(people_sep[,type], collapse=" ")
        #review_text <- people_sep[,type] ##something went wrong
        review_text <- gsub("[\n]", "  ", review_text)
        #review_text <- unlist(strsplit(review_text, "[，,。●;；]"))
        #review_text <- unlist(strsplit(review_text, "[。●;；]"))
        #review_text <- unlist(strsplit(review_text, "  "))
        #review_text <- unlist(strsplit(review_text  , "[(][0-9][)]"))
        #review_text <- unlist(strsplit(review_text  , "[（][0-9][）]"))
        #review_text <- unlist(strsplit(review_text  , "[0-9][.]"))
        #review_text <- unlist(strsplit(review_text  , "[0-9][、]"))
        
        review_source <- VectorSource(review_text)
        d.corpus      <- Corpus(review_source)
        
        d.corpus <- tm_map(d.corpus, removePunctuation) 
        d.corpus <- tm_map(d.corpus, removeNumbers) 
        d.corpus <- tm_map(d.corpus, content_transformer(tolower))
        d.corpus <- tm_map(d.corpus, function(word) {
          gsub("[0-9]", " ", word)
        })
        
        #Sometimes this part of code went wrong
        #detach("package:Rwordseg", unload=TRUE)
        #library(Rwordseg)
        d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
        
        myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者")
        d.corpus    <- tm_map(d.corpus, removeWords, myStopWords)
        d.corpus    <- tm_map(d.corpus, removeWords, stopwords("english")) 
        d.corpus    <- tm_map(d.corpus, PlainTextDocument)
        ##Extract words with more than 2 nchar
        tdm <- TermDocumentMatrixCN(d.corpus, control = list(wordLengths = c(2, Inf)))
        
        m1           <- as.matrix(tdm)
        v            <- sort(rowSums(m1), decreasing = TRUE)
        d            <- data.frame(word = names(v), freq = v)
        d$word       <- as.character(d$word)
        d$percentage <- d$freq/nrow(people_sep)
        if(nrow(d)>100){
          d = d[1:100,]
        }        
        
        delete.word.vector <- c("null","以上","年以上","經驗","工作","公司","企業","加班","負責","配合","完成","地區","相關","與","完成","work","experience","進行","擔任","will","能力","基本","興趣","主要","具有","具備","面試","下班","上班","內容","薪資","完整","優先","自行","統一")
        for(delete.word.index in 1:length(delete.word.vector)){
          if(toString(which(d$word==delete.word.vector[delete.word.index]))!=""){
            d <- d[-which(d$word==delete.word.vector[delete.word.index]),]
          }
        }
        if(total){
          write.csv(d,paste0("分行業別output\\", type, "\\整體\\",jobVector[job_i], type, "文字Freq.csv"),row.names=F)
        }else{
          write.csv(d,paste0("分行業別output\\", type, "\\",jobVector[job_i], type, "文字Freq.csv"),row.names=F)
        }       
        #png(paste0(output_path,"\\",type,"\\",jobVector[job_i],"_",type,"wordcloud.png"), width=800,height=800)
        
        #if(length(d$freq)>=100){
        #  wordcloud(d$word[1:100], d$freq[1:100], random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
        #}else{
        #  wordcloud(d$word, d$freq, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
        #}
        #dev.off()
        jgc()
        
        ##抓出前10名字串對應至col:type
        wordlen <- ifelse(length(d$word)>=10, 10, length(d$word))
        for(i in 1:wordlen){
          word.to.handle <- d$word[i]
          people_sep[,paste0(type, "處理過")] <- people_sep[,type]
          #people_sep[,paste0(type, "處理過")] <- tolower(people_sep[,paste0(type, "處理過")])
          
          ##order by their capital and number of employees
          people_sep      <- people_sep[order(-as.numeric(people_sep$資本金額), -as.numeric(people_sep$員工人數)), ]
          job_description <- people_sep[,paste0(type, "處理過")]
          job_description <- unlist(strsplit(job_description, "[。●;；]"))
          job_description <- unlist(strsplit(job_description, "  "))
          job_description <- unlist(strsplit(job_description, "[(][0-9][)]"))
          job_description <- unlist(strsplit(job_description, "[（][0-9][）]"))
          job_description <- unlist(strsplit(job_description, "[0-9][.]"))
          job_description <- unlist(strsplit(job_description, "[0-9][、]"))
          job_description <- unlist(strsplit(job_description, "[0-9][，]"))
          holo_num = c("０","１","２","３","４","５","６","７","８","９")
          for(i in 1:length(holo_num)){
            job_description <- unlist(strsplit(job_description, paste0(holo_num[i],"．")))
          }
          
          job_description <- trim(job_description)
          if(length(unlist(gregexpr("[a-z]",word.to.handle)))/nchar(word.to.handle)>0.9){
            job.describe.df <- job_description[which(grepl(paste0("[^a-z]",word.to.handle,"[^a-z]"),tolower(job_description)))]
          }else{
            job.describe.df <- job_description[which(grepl(word.to.handle,tolower(job_description)))]
          }
          job.describe <- unique(job.describe.df)
          
          if(toString(job.describe)!=""){
            ##extract top 5 big companys' job discriptions
            job.describe      <- job.describe[1:5]
            job.describe      <- as.data.frame(job.describe)
            job.describe$word <- word.to.handle
            job.describe      <- job.describe[,c("word","job.describe")]
            ##append
            if(i==1){
              if(total){
                write.table(job.describe, paste0("分行業別output\\", type, "\\整體\\",jobVector[job_i], type, "詞彙與內容對應結果.csv"), row.names=F, col.names=TRUE, sep=",")
              }else{
                write.table(job.describe, paste0("分行業別output\\", type, "\\",jobVector[job_i], type, "詞彙與內容對應結果.csv"), row.names=F, col.names=TRUE, sep=",")
              }
             }else{
               if(total){
                 write.table(job.describe, paste0("分行業別output\\", type, "\\整體\\",jobVector[job_i], type, "詞彙與內容對應結果.csv"), row.names=F,col.names=F, sep=",", append=TRUE)
               }else{
                 write.table(job.describe, paste0("分行業別output\\", type, "\\",jobVector[job_i], type, "詞彙與內容對應結果.csv"), row.names=F,col.names=F, sep=",", append=TRUE)
               }              
            }
          }
        }
        
        cat("\r", jobVector[job_i], "complete... ", format(round(job_i/length(job)*100,2),2), "%", rep(" ", 50))
      }else{
        print(paste0(jobVector[job_i], " shortage in sample: ", nrow(people_sep), " => Not analyzed"))
      }
      }, error=function(e){
        detach("package:Rwordseg", unload=TRUE)
        library(Rwordseg)
        ##error memo
        print(paste0(job_i, " ",jobVector[job_i] ,"  ", e))
        if(total){
          sink(paste0("分行業別output\\整體", type, "錯誤訊息.txt"), append=TRUE)
        }else{
          sink(paste0("分行業別output\\分行業", type, "錯誤訊息.txt"), append=TRUE)
        }                     
        print(paste0(job_i, " ",jobVector[job_i] ,"  ", e))
        sink()
    }) 
  } 
}

##computer skills or certification
##Type: 電腦專長 or 專業憑證
specialtyMining <- function(people, job_only, type){
  #computer skills
  specialty_del <- c("Mac OS X","Windows Server 2000-2012","Windows NT","Windows Vista","windows 8","Word","Excel","PowerPoint","Outlook","Windows XP","Windows 7","lnternet Explorer","Windows 98")
  for(job_i in 1:length(job_only)){
    jgc()
    print(paste0(job_only[job_i], " ", type, " now analyzing..."))
    people_sep <- people[which(people$職務小類==job_only[job_i]),]
    setDF(people_sep)
    total_sep_people_sum   <- length(people_sep$職務小類)
    people_specialty       <- c(people_sep[, type])
    people_specialty       <- people_specialty[which(people_specialty!="NULL")]
    people_specialty       <- strsplit(people_specialty, ",")
    people_specialty       <- unlist(people_specialty)
    people_specialty       <- as.data.frame(table(people_specialty), stringsAsFactors=F)
    people_specialty       <- people_specialty[order(-people_specialty$Freq), ]
    
    if(toString(nrow(people_specialty))!=""){
      #people_specialty <- people_specialty[which(people_specialty$Freq>=nrow(people_sep)*0.4),]
      if(nrow(people_specialty)>=1){
        ##Set filters
        people_specialty$percentage <- people_specialty$Freq/total_sep_people_sum
        people_specialty            <- people_specialty[which(people_specialty$Freq>5 & people_specialty$percentage>0.015),]
        for(i in 1:length(specialty_del)){
          people_specialty <- people_specialty[which(people_specialty[,1]!=specialty_del[i]),]
        }
        if(nrow(people_specialty)>10){
          people_specialty <- people_specialty[1:10,]
        }
        colnames(people_specialty)[1] = job_only[job_i]

        if(nrow(people_specialty)>0){
          people_specialty <- t(people_specialty)
          colnames(people_specialty) = c(1:ncol(people_specialty))
          write.csv(people_specialty, paste0("分行業別output\\", type, "\\", job_only[job_i], "高頻", type, ".csv"))
        }
      }
    }
  }
  print(paste0("Export total form of ", type, "..."))
  ##Export total form
  files <- list.files(paste0(".\\分行業別output\\", type), pattern = "*.csv", full.names = T)
  #library(gtools)
  total_specialty <- data.frame(a='1', stringsAsFactors=F)
  for(i in 1:length(files)){
    temp_specialty           <- read.csv(files[i],stringsAsFactors=F)
    colnames(temp_specialty) <- c(1:ncol(temp_specialty))
    total_specialty          <- smartbind(total_specialty, temp_specialty[1,])
  }
  total_specialty_1           <- total_specialty[-1, -1]
  colnames(total_specialty_1) <- c("職務小類名稱", 1:(ncol(total_specialty_1)-1))
  for(i in 1:ncol(total_specialty_1)){
    total_specialty_1[which(is.na(total_specialty_1[,i])),i] <- ""
  }  
  write.csv(total_specialty_1, paste0(".\\分行業別output\\", type, "\\", type, "總整理表.csv"),row.names=F)
}



##apriori 電腦專長
arule_computer_skills <- function(type){
  library(arules)

  new_people <- as.data.frame(cbind(people$職缺編號, people$職務小類, people$電腦專長))
  setDF(new_people)
  new_people[which(new_people[,3]=="NULL"), 3] <- ""
  new_people2 <- new_people[which(new_people[,3]!=""), ]
  colnames(new_people2) <- c("職缺編號", "職務小類", "電腦專長")
  
  print(paste0("Original number of rows : ", nrow(new_people)))
  print(paste0("Number of rows with", type, " : ", nrow(new_people2)))
  #new_people2$職務小類 <- gsub('/','／',new_people2$職務小類)
  
  new_people2$職務小類 <- as.character(new_people2$職務小類)
  new_people2$電腦專長 <- as.character(new_people2$電腦專長)
  new_people2$職缺編號 <- as.character(new_people2$職缺編號)
  
  job_list <- unique(new_people$V2)
  
  for(job_num in 1:length(job_list)){
    job         <- toString(job_list[job_num])
    new_people3 <- new_people2[which(new_people2$職務小類==job),]
    
    computer_skills_del <- c("Mac OS X","Windows Server 2000-2012","Windows NT","Windows Vista","windows 8","Word","Excel","PowerPoint","Outlook","Windows XP","Windows 7","lnternet Explorer","Windows 98")
    
    lev <- levels(as.factor(new_people3[,"電腦專長"])) #取得factor的level，此時都還是許多單一證照字串串起來的情況，故level數會很多
    lev <- unique(unlist(strsplit(lev, ","))) #以","將各單一證照字串分開，並去除重覆
    lev <- lev [! lev %in% computer_skills_del]
    
    tranc_df = data.frame('序號'=numeric(),'證照'=character(),stringsAsFactors=F)
    
    if(length(lev)>0){
      for(i in 1:length(lev)){
        
        temp_df = data.frame('序號'=numeric(),'證照'=character(),stringsAsFactors=F)
        temp_df[1:length(new_people3$職缺編號[which(grepl(lev[i],new_people3$電腦專長, fixed=TRUE))]),1] = new_people3$職缺編號[which(grepl(lev[i],new_people3$電腦專長, fixed=TRUE))]
        temp_df[1:length(new_people3$職缺編號[which(grepl(lev[i],new_people3$電腦專長, fixed=TRUE))]),2] = lev[i]
        tranc_df = rbind(tranc_df,temp_df)
        
        
        
      }
      id_list = unique(tranc_df$序號)
      for(i in 1:length(id_list)){
        temp_df = data.frame('序號'=numeric(),'證照'=character(),stringsAsFactors=F)
        temp_df[1,1] = id_list[i]
        temp_df[1,2] = job
        tranc_df = rbind(tranc_df,temp_df)
      }
      tranc_df = tranc_df[order(tranc_df$序號),]
      tranc_df = unique(tranc_df)
      
      tranc_list = split(x=tranc_df$證照,f=tranc_df$序號)
      rules = apriori(tranc_list,parameter=list(supp=0.2,conf=0.8,maxlen=2),appearance=list(rhs=job,default="lhs"))
      inspect(head(sort(rules,by="support"),40))
      #inspect(sort(rules,by="support"))
      path_arule<-"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\電腦專長arule"
      setwd(path_arule)
      sink(paste0(job,"電腦專長.csv"))
      inspect(sort(rules,by="support"))
      sink()
    }
    
  }
  setwd(output_path)
}

##apriori for 專業憑證
arule_pro_certificate <- function(){
  library(arules)
  
  new_people = as.data.frame(cbind(people$職缺編號,people$職務小類,people$專業憑證))
  new_people[which(new_people[,3]=='NULL'),3] = ''
  new_people2 = new_people[which(new_people[,3]!=''),]
  colnames(new_people2) = c('職缺編號','職務小類','專業憑證')
  print(paste0('原資料筆數為 ', nrow(new_people),' 筆'))
  print(paste0('有填寫專業憑證資料筆數為 ', nrow(new_people2),' 筆'))
  ##new_people2$職務小類 <- gsub('/','／',new_people2$職務小類)
  
  new_people2$職務小類 = as.character(new_people2$職務小類)
  new_people2$專業憑證 = as.character(new_people2$專業憑證)
  new_people2$職缺編號 = as.character(new_people2$職缺編號)
  
  job_list = unique(new_people$V2)
  
  for(job_num in 1:length(job_list)){
    job=toString(job_list[job_num])
    new_people3 = new_people2[which(new_people2$職務小類==job),]
    
    lev <- levels(as.factor(new_people3[,"專業憑證"])) #取得factor的level，此時都還是許多單一證照字串串起來的情況，故level數會很多
    lev <- unique(unlist(strsplit(lev, ","))) #以","將各單一證照字串分開，並去除重覆
    
    tranc_df = data.frame('序號'=numeric(),'證照'=character(),stringsAsFactors=F)
    
    if(length(lev)>0){
      for(i in 1:length(lev)){
        
        temp_df = data.frame('序號'=numeric(),'證照'=character(),stringsAsFactors=F)
        temp_df[1:length(new_people3$職缺編號[which(grepl(lev[i],new_people3$專業憑證, fixed=TRUE))]),1] = new_people3$職缺編號[which(grepl(lev[i],new_people3$專業憑證, fixed=TRUE))]
        temp_df[1:length(new_people3$職缺編號[which(grepl(lev[i],new_people3$專業憑證, fixed=TRUE))]),2] = lev[i]
        tranc_df = rbind(tranc_df,temp_df)
        
        
        
      }
      id_list = unique(tranc_df$序號)
      for(i in 1:length(id_list)){
        temp_df = data.frame('序號'=numeric(),'證照'=character(),stringsAsFactors=F)
        temp_df[1,1] = id_list[i]
        temp_df[1,2] = job
        tranc_df = rbind(tranc_df,temp_df)
      }
      tranc_df = tranc_df[order(tranc_df$序號),]
      tranc_df = unique(tranc_df)
      
      tranc_list = split(x=tranc_df$證照,f=tranc_df$序號)
      rules = apriori(tranc_list,parameter=list(supp=0.2,conf=0.8,maxlen=2),appearance=list(rhs=job,default="lhs"))
      inspect(head(sort(rules,by="support"),40))
      #inspect(sort(rules,by="support"))
      path_arule<-"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\專業憑證arule"
      setwd(path_arule)
      sink(paste0(job,"專業憑證.csv"))
      inspect(sort(rules,by="support"))
      sink()
    }
    
  }
  setwd(output_path)
}

##取交集
intersect_computer_skills <- function(){
  
  output_path<-paste0(path,"\\分行業別output")
  setwd(output_path)
  sink("電腦專長交集.csv")
  unique_job_type <- unique(people$職務小類)
  for (j in 1:length(unique_job_type)) {
    tryCatch({
      job = unique_job_type[j]
      
      cs_output_path<-paste0(path,"\\分行業別output\\電腦專長arule")
      setwd(cs_output_path)
      arule_computer_skills_df = read.csv(paste0(job,'電腦專長.csv'),stringsAsFactors=F)
      dim(arule_computer_skills_df)
      arule_computer_skills_df = arule_computer_skills_df[-1,]
      arule_computer_skills_df = as.data.frame(arule_computer_skills_df,stringsAsFactors=F)
      
      arule_computer_skills_df$證照=''
      for(i in 1:nrow(arule_computer_skills_df)){
        arule_computer_skills_df$證照[i] = substr(arule_computer_skills_df$arule_computer_skills_df[i],unlist(gregexpr(pattern ='\\{',arule_computer_skills_df$arule_computer_skills_df[i]))[1]+1,unlist(gregexpr(pattern ='\\}',arule_computer_skills_df$arule_computer_skills_df[i]))[1]-1)
        
      }
      
      cs_output_path<-paste0(path,"\\分行業別output\\電腦專長")
      setwd(cs_output_path)
      computer_skills_df = read.csv(paste0(job,'高頻電腦專長.csv'),stringsAsFactors=F)
      
      
      cat(paste0(job,',',paste(intersect(arule_computer_skills_df$證照,as.character(computer_skills_df[1,])), collapse = ','),'\n'))
      
      
      
      
    }, error=function(e){
      #cat("ERROR :",conditionMessage(e), "\n")
    })
  }
  
  output_path<-paste0(path,"\\分行業別output")
  setwd(output_path)
  
  sink()
  
}

##取交集
intersect_pro_certificate <- function(){
  
  output_path<-paste0(path,"\\分行業別output")
  setwd(output_path)
  sink("專業憑證交集.csv")
  unique_job_type <- unique(people$職務小類)
  for (j in 1:length(unique_job_type)) {
    tryCatch({
      job = unique_job_type[j]
      
      cs_output_path<-paste0(path,"\\分行業別output\\專業憑證arule")
      setwd(cs_output_path)
      arule_computer_skills_df = read.csv(paste0(job,'專業憑證.csv'),stringsAsFactors=F)
      dim(arule_computer_skills_df)
      arule_computer_skills_df = arule_computer_skills_df[-1,]
      arule_computer_skills_df = as.data.frame(arule_computer_skills_df,stringsAsFactors=F)
      
      arule_computer_skills_df$證照=''
      for(i in 1:nrow(arule_computer_skills_df)){
        arule_computer_skills_df$證照[i] = substr(arule_computer_skills_df$arule_computer_skills_df[i],unlist(gregexpr(pattern ='\\{',arule_computer_skills_df$arule_computer_skills_df[i]))[1]+1,unlist(gregexpr(pattern ='\\}',arule_computer_skills_df$arule_computer_skills_df[i]))[1]-1)
        
      }
      
      cs_output_path<-paste0(path,"\\分行業別output\\專業憑證")
      setwd(cs_output_path)
      computer_skills_df = read.csv(paste0(job,'高頻專業憑證.csv'),stringsAsFactors=F)
      
      
      cat(paste0(job,',',paste(intersect(arule_computer_skills_df$證照,as.character(computer_skills_df[1,])), collapse = ','),'\n'))
      
      
      
      
    }, error=function(e){
      #cat("ERROR :",conditionMessage(e), "\n")
    })
  }
  
  output_path<-paste0(path,"\\分行業別output")
  setwd(output_path)
  
  sink()
  
}