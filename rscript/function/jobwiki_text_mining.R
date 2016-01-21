#廠商版職務大蒐秘

##設定門檻值
min_n_sample = 100

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#gc
jgc <- function()
{
  gc()
  .jcall("java/lang/System", method = "gc")
}  

##挑出要處理的資料
data_processing_job <- function(){
  jgc()
  ##行業別與職務
  job_and_undustry = as.data.frame(table(people$行業與職務),stringsAsFactors=F)
  
  high_Freq_job_industry <- job_and_undustry[which(job_and_undustry$Freq>=100),]
  job <- c(high_Freq_job_industry$Var1)
  print(paste0('不同行業別 職務小類樣本數大於100者剩 ',length(job),' 筆'))
  
  return(job)
}
##挑出要處理的資料
data_processing_job_only <- function(){
  jgc()
  
  ##純職務
  job_only = as.data.frame(table(people$職務小類),stringsAsFactors=F)
  
  high_Freq_job_only <- job_only[which(job_only$Freq>=min_n_sample),]
  job_only <- c(job_only$Var1)
  job_only <- job_only[-which(job_only=='NULL')]
  print(paste0('職務小類樣本數大於',min_n_sample,'者剩 ',length(job_only),' 筆'))
  return(job_only)
  
}

##工作說明
job_discription <-function(){
  
  
  for(job_i in 1:length(job)){
    jgc()
    print(paste0(job[job_i],' 進行工作說明文字探勘與計算中'))
    people_sep <- people[which(people$行業與職務==job[job_i]),]
    
    people_sep$工作說明 = gsub('\x9e','  ', people_sep$工作說明)
    ##去除英文工作說明
    en_remove_index <-{}
    for(en.remove in 1:length(people_sep$工作說明)){
      if(length(gregexpr(pattern ='[a-z]',people_sep$工作說明)[[en.remove]])/nchar(people_sep$工作說明[en.remove])>0.5){
        en_remove_index <- c(en_remove_index,en.remove)
      }
    }
    if(toString(en_remove_index)==''){
      
    }else{
      people_sep <- people_sep[-en_remove_index,]
    }
    job_all_name = substr(job[job_i],1,unlist(gregexpr(' - ',job[job_i]))-1)
    #min_nrow = mean(as.data.frame(table(people[which(people$職務小類==job_all_name)]$行業與職務))$Freq)-2*sd(as.factor(people[which(people$職務小類==job_all_name)]$行業與職務))
    
    if(nrow(people_sep)>min_n_sample){
      review_text <- paste(people_sep$工作說明, collapse=" ")
      #review_text =people_sep$工作說明
      review_text <- gsub("[\n]", "  ", review_text)
      #review_text = unlist(strsplit(review_text, "[，,。●;；]"))
      #review_text = unlist(strsplit(review_text, "[。●;；]"))
      #review_text = unlist(strsplit(review_text, "  "))
      #review_text = unlist(strsplit(review_text  , "[(][0-9][)]"))
      #rreview_text = unlist(strsplit(review_text  , "[（][0-9][）]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][.]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][、]"))
      
      review_source <- VectorSource(review_text)
      d.corpus <- Corpus(review_source)
      
      d.corpus <- tm_map(d.corpus, removePunctuation) 
      d.corpus <- tm_map(d.corpus, removeNumbers) 
      d.corpus <- tm_map(d.corpus, content_transformer(tolower))
      d.corpus <- tm_map(d.corpus, function(word) {
        gsub("[0-9]", " ", word)
      })
      
      
      #問題在這
      d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
      
      myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者","資料庫管理")
      d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
      d.corpus <- tm_map(d.corpus, removeWords, stopwords("english")) 
      d.corpus <- tm_map(d.corpus, PlainTextDocument)
      ##修改取出文字長度
      tdm <- TermDocumentMatrixCN(d.corpus, control = list(wordLengths = c(2, Inf)))
      
      m1 <- as.matrix(tdm)
      v <- sort(rowSums(m1), decreasing = TRUE)
      d <- data.frame(word = names(v), freq = v)
      d$word <- as.character(d$word)
      d$percentage <- d$freq/nrow(people_sep)
      
      delete.word.vector <- c('null','以上','年以上','經驗','工作','公司','企業','加班','負責','配合','完成','地區','相關','與','完成','work','experience','進行','擔任','will','能力','基本','興趣','主要','具有','具備','面試','下班','上班','內容','薪資','完整','優先','自行','統一')
      for(delete.word.index in 1:length(delete.word.vector)){
        if(toString(which(d$word==delete.word.vector[delete.word.index]))!=''){
          d <- d[-which(d$word==delete.word.vector[delete.word.index]),]
        }
        
      }
      
      write.csv(d,paste0('工作說明\\',job[job_i],'工作說明文字Freq.csv'),row.names=F)
      
      png(paste0(output_path,'\\','工作說明\\',job[job_i],'_工作說明wordcloud.png'), width=800,height=800)
      
      if(length(d$freq)>=100){
        wordcloud(d$word[1:100], d$freq[1:100], random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }else{
        wordcloud(d$word, d$freq, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }
      dev.off()
      jgc()
      
      ##抓出前10名字串對應至工作說明
      for(i in 1:10){
        word.to.handle <- d$word[i]
        people_sep$工作說明處理過 <- people_sep$工作說明
        people_sep$工作說明處理過 <- tolower(people_sep$工作說明處理過)
        
        people_sep <- people_sep[order(-as.numeric(people_sep$資本金額),-as.numeric(people_sep$員工人數)),]
        job_description <- people_sep$工作說明處理過
        job_description = unlist(strsplit(job_description, "[。●;；]"))
        job_description = unlist(strsplit(job_description, "  "))
        job_description = unlist(strsplit(job_description  , "[(][0-9][)]"))
        job_description = unlist(strsplit(job_description  , "[（][0-9][）]"))
        job_description = unlist(strsplit(job_description  , "[0-9][.]"))
        job_description = unlist(strsplit(job_description  , "[0-9][、]"))
        job_description = trim(job_description)
        
        job.describe.df <- job_description[which(grepl(word.to.handle,job_description))]
        job.describe <- unique(job.describe.df)
        
        if(toString(job.describe)!=''){
          
          ##抓1~5大的公司的描述
          job.describe <- job.describe[1:5]
          
          job.describe <- as.data.frame(job.describe)
          job.describe$word <- word.to.handle
          job.describe <- job.describe[,c('word','job.describe')]
          
          if(i==1){
            write.table(job.describe, paste0('工作說明\\',job[job_i],'工作說明詞彙與內容對應結果.csv'), row.names=F, col.names=TRUE, sep=",")
            
          }else{
            write.table(job.describe, paste0('工作說明\\',job[job_i],'工作說明詞彙與內容對應結果.csv'), row.names=F,col.names=F, sep=",", append=TRUE)
            
          }
        }
      }
      
      print(paste0(format(round(job_i/length(job)*100,2),2),'%'))
      print(paste0(job[job_i],' 計算完成'))
    }else{
      print(paste0(job[job_i],' 樣本不足',min_n_sample,'，不予計算'))
    }
    
    
  }
  
}

##附加條件
other_needs <-function(){
  #job_and_undustry = as.data.frame(table(people$行業與職務),stringsAsFactors=F)
  
  #high_Freq_job_industry <- job_and_undustry[which(job_and_undustry$Freq>=100),]
  #job <- c(high_Freq_job_industry$Var1)
  #print(paste0('不同行業別 職務小類樣本數大於100者剩 ',length(job),' 筆'))
  
  for(job_i in 1:length(job)){
    jgc()
    print(paste0(job[job_i],' 進行附加條件文字探勘與計算中'))
    people_sep <- people[which(people$行業與職務==job[job_i]),]
    
    people_sep$附加條件 = gsub('\x9e','  ', people_sep$附加條件)
    ##去除英文的附加條件
    en_remove_index <-{}
    for(en.remove in 1:length(people_sep$附加條件)){
      if(length(gregexpr(pattern ='[a-z]',people_sep$附加條件)[[en.remove]])/nchar(people_sep$附加條件[en.remove])>0.5){
        en_remove_index <- c(en_remove_index,en.remove)
      }
    }
    if(toString(en_remove_index)==''){
      
    }else{
      people_sep <- people_sep[-en_remove_index,]
    }
    
    if(nrow(people_sep)>min_n_sample){
      review_text <- paste(people_sep$附加條件, collapse=" ")
      #review_text <- people_sep$附加條件
      review_text <- gsub("[\n]", "  ", review_text)
      #review_text = unlist(strsplit(review_text, "[，,。●;；]"))
      #review_text = unlist(strsplit(review_text, "[。●;；]"))
      #review_text = unlist(strsplit(review_text, "  "))
      #review_text = unlist(strsplit(review_text  , "[(][0-9][)]"))
      #review_text = unlist(strsplit(review_text  , "[（][0-9][）]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][.]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][、]"))
      
      review_source <- VectorSource(review_text)
      d.corpus <- Corpus(review_source)
      
      d.corpus <- tm_map(d.corpus, removePunctuation) 
      d.corpus <- tm_map(d.corpus, removeNumbers) 
      d.corpus <- tm_map(d.corpus, content_transformer(tolower))
      d.corpus <- tm_map(d.corpus, function(word) {
        gsub("[0-9]", " ", word)
      })
      
      d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
      
      myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者","資料庫管理")
      d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
      d.corpus <- tm_map(d.corpus, removeWords, stopwords("english")) 
      d.corpus <- tm_map(d.corpus, PlainTextDocument)
      tdm <- TermDocumentMatrixCN(d.corpus, control = list(wordLengths = c(2, Inf)))
      
      m1 <- as.matrix(tdm)
      v <- sort(rowSums(m1), decreasing = TRUE)
      d <- data.frame(word = names(v), freq = v)
      d$word <- as.character(d$word)
      d$percentage <- d$freq/nrow(people_sep)
      
      delete.word.vector <- c('以上','年以上','經驗','工作','公司','企業','加班','負責','配合','完成','地區','相關','與','完成','work','experience','進行','擔任','will','能力','基本','興趣','主要','具有','具備','面試','下班','上班','內容','薪資','完整','優先','自行','統一')
      for(delete.word.index in 1:length(delete.word.vector)){
        if(toString(which(d$word==delete.word.vector[delete.word.index]))!=''){
          d <- d[-which(d$word==delete.word.vector[delete.word.index]),]
        }
        
      }
      
      write.csv(d,paste0('附加條件\\',job[job_i],'附加條件文字Freq.csv'),row.names=F)
      
      png(paste0(output_path,'\\','附加條件\\',job[job_i],'_附加條件wordcloud.png'), width=800,height=800)
      
      if(length(d$freq)>=100){
        wordcloud(d$word[1:100], d$freq[1:100], random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }else{
        wordcloud(d$word, d$freq, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }
      dev.off()
      jgc()
      
      ##抓出前10名字串對應至附加條件
      for(i in 1:10){
        word.to.handle <- d$word[i]
        people_sep$附加條件處理過 <- people_sep$附加條件
        people_sep$附加條件處理過 <- tolower(people_sep$附加條件處理過)
        
        people_sep <- people_sep[order(-as.numeric(people_sep$資本金額),-as.numeric(people_sep$員工人數)),]
        job_description <- people_sep$附加條件處理過
        job_description = unlist(strsplit(job_description, "[。●;；]"))
        job_description = unlist(strsplit(job_description, "  "))
        job_description = unlist(strsplit(job_description  , "[(][0-9][)]"))
        job_description = unlist(strsplit(job_description  , "[（][0-9][）]"))
        job_description = unlist(strsplit(job_description  , "[0-9][.]"))
        job_description = unlist(strsplit(job_description  , "[0-9][、]"))
        
        job_description = trim(job_description)
        job.describe.df <- job_description[which(grepl(word.to.handle,job_description))]
        job.describe <- unique(job.describe.df)
        
        if(toString(job.describe)!=''){
          
          ##抓1~5大的公司的描述
          job.describe <- job.describe[1:5]
          
          job.describe <- as.data.frame(job.describe)
          job.describe$word <- word.to.handle
          job.describe <- job.describe[,c('word','job.describe')]
          
          if(i==1){
            write.table(job.describe, paste0('附加條件\\',job[job_i],'附加條件詞彙與內容對應結果.csv'), row.names=F, col.names=TRUE, sep=",")
            
          }else{
            write.table(job.describe, paste0('附加條件\\',job[job_i],'附加條件詞彙與內容對應結果.csv'), row.names=F,col.names=F, sep=",", append=TRUE)
            
          }
        }
      }
      
      print(paste0(format(round(job_i/length(job)*100,2),2),'%'))
      print(paste0(job[job_i],' 附加條件計算完成'))
    }else{
      print(paste0(job[job_i],' 樣本不足',min_n_sample,'，不予計算'))
    }
    
    
    
  }
  
}

#電腦專長
computer_skills <- function(){
  for(job_i in 1:length(job_only)){
    jgc()
    print(paste0(job_only[job_i],' 進行電腦專長計算中'))
    people_sep <- people[which(people$職務小類==job_only[job_i]),]
    total_sep_people_sum = length(people_sep$職務小類)
    people_computer_skills <- c(people_sep$電腦專長)
    people_computer_skills <- people_computer_skills[-which(people_computer_skills=='NULL')]
    people_computer_skills <- strsplit(people_computer_skills,',')
    people_computer_skills <- unlist(people_computer_skills)
    people_computer_skills <- as.data.frame(table(people_computer_skills),stringsAsFactors=F)
    people_computer_skills <- people_computer_skills[order(-people_computer_skills$Freq),]
    
    
    computer_skills_del <- c('Mac OS X','Windows Server 2000-2012','Windows NT','Windows Vista','windows 8','Word','Excel','PowerPoint','Outlook','Windows XP','Windows 7','lnternet Explorer','Windows 98')
    
    if(toString(nrow(people_computer_skills))!=""){
      #people_computer_skills <- people_computer_skills[which(people_computer_skills$Freq>=nrow(people_sep)*0.4),]
      if(toString(nrow(people_computer_skills))!="" & nrow(people_computer_skills)>=1){
        ##設定篩選條件
        people_computer_skills$percentage = people_computer_skills$Freq/total_sep_people_sum
        people_computer_skills <- people_computer_skills[which(people_computer_skills$Freq>5 | people_computer_skills$percentage>0.015),]
        for(i in 1:length(computer_skills_del)){
          people_computer_skills = people_computer_skills[which(people_computer_skills[,1]!=computer_skills_del[i]),]
        }}
      if(toString(nrow(people_computer_skills))!="" & nrow(people_computer_skills)>=1){
        if(nrow(people_computer_skills)>10){
          people_computer_skills <- people_computer_skills[1:10,]
        }
        
        colnames(people_computer_skills)[1] = job_only[job_i]
        
        
        people_computer_skills = t(people_computer_skills)
        colnames(people_computer_skills) = c(1:ncol(people_computer_skills))
        if(nrow(people_computer_skills)>0){
          write.csv(people_computer_skills,paste0('電腦專長\\',job_only[job_i],'高頻電腦專長.csv'))
        }
        
      }
    }
  }
  print('開始抓取電腦專長總表')
  ##輸出總表
  path_com<-"C:\\Users\\abc\\Desktop\\廠商版職務大辭典\\分行業別output\\電腦專長"
  setwd(path_com)
  files = list.files(pattern="*.csv")
  library(gtools)
  total_computer_skills <- data.frame(a='1',stringsAsFactors=F)
  for(i in 1:length(files)){
    temp_computer_skills = read.csv(files[i],stringsAsFactors=F)
    colnames(temp_computer_skills) = c(1:ncol(temp_computer_skills))
    total_computer_skills = smartbind(total_computer_skills, temp_computer_skills[1,])
  }
  total_computer_skills_1 = total_computer_skills[-1,-1]
  colnames(total_computer_skills_1) = c('職務小類名稱',1:(ncol(total_computer_skills_1)-1))
  for(i in 1:ncol(total_computer_skills_1)){
    total_computer_skills_1[which(is.na(total_computer_skills_1[,i])),i] =''
  }  
  write.csv(total_computer_skills_1,'電腦專長總整理表.csv',row.names=F)
  
  path_output<-"C:\\Users\\abc\\Desktop\\廠商版職務大辭典\\分行業別output"
  setwd(path_output)
}

##專業憑證
pro_certificate <- function(){
  for(job_i in 1:length(job_only)){
    jgc()
    print(paste0(job_only[job_i],' 進行專業憑證計算中'))
    people_sep <- people[which(people$職務小類==job_only[job_i]),]
    total_sep_people_sum = length(people_sep$職務小類)
    people_pro_certificate <- c(people_sep$專業憑證)
    people_pro_certificate <- people_pro_certificate[-which(people_pro_certificate=='NULL')]
    people_pro_certificate <- strsplit(people_pro_certificate,',')
    people_pro_certificate <- unlist(people_pro_certificate)
    people_pro_certificate <- as.data.frame(table(people_pro_certificate),stringsAsFactors=F)
    people_pro_certificate <- people_pro_certificate[order(-people_pro_certificate$Freq),]
    
    
    ##超過4成廠商所要求的再抓出
    if(toString(nrow(people_pro_certificate))!=""){
      #people_pro_certificate <- people_pro_certificate[which(people_pro_certificate$Freq>=nrow(people_sep)*0.4),]
      if(toString(nrow(people_pro_certificate))!="" & nrow(people_pro_certificate)>=1){
        ##篩選條件
        people_pro_certificate$percentage = people_pro_certificate$Freq/total_sep_people_sum
        people_pro_certificate <- people_pro_certificate[which(people_pro_certificate$Freq>5 | people_pro_certificate$percentage>0.015),]
      }
      
      if(toString(nrow(people_pro_certificate))!="" & nrow(people_pro_certificate)>=1){
        if(nrow(people_pro_certificate)>10){
          people_pro_certificate <- people_pro_certificate[1:10,]
        }
        
        colnames(people_pro_certificate)[1] = job_only[job_i]
        
        people_pro_certificate = t(people_pro_certificate)
        write.csv(people_pro_certificate,paste0('專業憑證\\',job_only[job_i],'高頻專業憑證.csv'))
      }
    }
  }
  print('開始抓取專業憑證總表')
  ##輸出總表
  path_pro<-"C:\\Users\\abc\\Desktop\\廠商版職務大辭典\\分行業別output\\專業憑證"
  setwd(path_pro)
  files = list.files(pattern="*.csv")
  library(gtools)
  total_pro_certifi <- data.frame(a='1',stringsAsFactors=F)
  for(i in 1:length(files)){
    temp_pro_certifi = read.csv(files[i],stringsAsFactors=F)
    colnames(temp_pro_certifi) = c(1:ncol(temp_pro_certifi))
    total_pro_certifi = smartbind(total_pro_certifi, temp_pro_certifi[1,])
  }
  total_pro_certifi_1 = total_pro_certifi[-1,-1]
  colnames(total_pro_certifi_1) = c('職務小類名稱',1:(ncol(total_pro_certifi_1)-1))
  for(i in 1:ncol(total_pro_certifi_1)){
    total_pro_certifi_1[which(is.na(total_pro_certifi_1[,i])),i] =''
  }  
  write.csv(total_pro_certifi_1,'專業憑證總整理表.csv',row.names=F)
  
  path_output<-"C:\\Users\\abc\\Desktop\\廠商版職務大辭典\\分行業別output"
  setwd(path_output)
}

##整體工作說明
all_job_discription <-function(){
  
  for(job_i in 1:length(job_only)){
    jgc()
    print(paste0(job_only[job_i],' 進行工作說明文字探勘與計算中'))
    people_sep <- people[which(people$職務小類==job_only[job_i]),]
    
    people_sep$工作說明 = gsub('\x9e','  ', people_sep$工作說明)
    ##去除英文工作說明
    en_remove_index <-{}
    for(en.remove in 1:length(people_sep$工作說明)){
      if(length(gregexpr(pattern ='[a-z]',people_sep$工作說明)[[en.remove]])/nchar(people_sep$工作說明[en.remove])>0.5){
        en_remove_index <- c(en_remove_index,en.remove)
      }
    }
    if(toString(en_remove_index)==''){
      
    }else{
      people_sep <- people_sep[-en_remove_index,]
    }
    job_all_name = substr(job_only[job_i],1,unlist(gregexpr(' - ',job_only[job_i]))-1)
    
    if(nrow(people_sep)>min_n_sample){
      review_text <- paste(people_sep$工作說明, collapse=" ")
      #review_text =people_sep$工作說明
      review_text <- gsub("[\n]", "  ", review_text)
      #review_text = unlist(strsplit(review_text, "[，,。●;；]"))
      #review_text = unlist(strsplit(review_text, "[。●;；]"))
      #review_text = unlist(strsplit(review_text, "  "))
      #review_text = unlist(strsplit(review_text  , "[(][0-9][)]"))
      #rreview_text = unlist(strsplit(review_text  , "[（][0-9][）]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][.]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][、]"))
      
      review_source <- VectorSource(review_text)
      d.corpus <- Corpus(review_source)
      
      d.corpus <- tm_map(d.corpus, removePunctuation) 
      d.corpus <- tm_map(d.corpus, removeNumbers) 
      d.corpus <- tm_map(d.corpus, content_transformer(tolower))
      d.corpus <- tm_map(d.corpus, function(word) {
        gsub("[0-9]", " ", word)
      })
      
      
      #問題在這
      d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
      
      myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者","資料庫管理")
      d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
      d.corpus <- tm_map(d.corpus, removeWords, stopwords("english")) 
      d.corpus <- tm_map(d.corpus, PlainTextDocument)
      ##修改取出文字長度
      tdm <- TermDocumentMatrixCN(d.corpus, control = list(wordLengths = c(2, Inf)))
      
      m1 <- as.matrix(tdm)
      v <- sort(rowSums(m1), decreasing = TRUE)
      d <- data.frame(word = names(v), freq = v)
      d$word <- as.character(d$word)
      d$percentage <- d$freq/nrow(people_sep)
      
      delete.word.vector <- c('null','以上','年以上','經驗','工作','公司','企業','加班','負責','配合','完成','地區','相關','與','完成','work','experience','進行','擔任','will','能力','基本','興趣','主要','具有','具備','面試','下班','上班','內容','薪資','完整','優先','自行','統一')
      for(delete.word.index in 1:length(delete.word.vector)){
        if(toString(which(d$word==delete.word.vector[delete.word.index]))!=''){
          d <- d[-which(d$word==delete.word.vector[delete.word.index]),]
        }
        
      }
      
      write.csv(d,paste0('工作說明\\整體\\',job_only[job_i],'工作說明文字Freq.csv'),row.names=F)
      
      png(paste0(output_path,'\\','工作說明\\整體\\',job_only[job_i],'_工作說明wordcloud.png'), width=800,height=800)
      
      if(length(d$freq)>=100){
        wordcloud(d$word[1:100], d$freq[1:100], random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }else{
        wordcloud(d$word, d$freq, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }
      dev.off()
      jgc()
      
      ##抓出前10名字串對應至工作說明
      for(i in 1:10){
        word.to.handle <- d$word[i]
        people_sep$工作說明處理過 <- people_sep$工作說明
        people_sep$工作說明處理過 <- tolower(people_sep$工作說明處理過)
        
        people_sep <- people_sep[order(-as.numeric(people_sep$資本金額),-as.numeric(people_sep$員工人數)),]
        job_description <- people_sep$工作說明處理過
        job_description = unlist(strsplit(job_description, "[。●;；]"))
        job_description = unlist(strsplit(job_description, "  "))
        job_description = unlist(strsplit(job_description  , "[(][0-9][)]"))
        job_description = unlist(strsplit(job_description  , "[（][0-9][）]"))
        job_description = unlist(strsplit(job_description  , "[0-9][.]"))
        job_description = unlist(strsplit(job_description  , "[0-9][、]"))
        job_description = trim(job_description)
        
        job.describe.df <- job_description[which(grepl(word.to.handle,job_description))]
        job.describe <- unique(job.describe.df)
        
        if(toString(job.describe)!=''){
          
          ##抓1~5大的公司的描述
          job.describe <- job.describe[1:5]
          
          job.describe <- as.data.frame(job.describe)
          job.describe$word <- word.to.handle
          job.describe <- job.describe[,c('word','job.describe')]
          
          if(i==1){
            write.table(job.describe, paste0('工作說明\\整體\\',job_only[job_i],'工作說明詞彙與內容對應結果.csv'), row.names=F, col.names=TRUE, sep=",")
            
          }else{
            write.table(job.describe, paste0('工作說明\\整體\\',job_only[job_i],'工作說明詞彙與內容對應結果.csv'), row.names=F,col.names=F, sep=",", append=TRUE)
            
          }
        }
      }
      
      print(paste0(format(round(job_i/length(job)*100,2),2),'%'))
      print(paste0(job_only[job_i],' 計算完成'))
    }else{
      print(paste0(job_only[job_i],' 樣本不足',min_n_sample,'，不予計算'))
    }
    
    
  }
  
}

##整體附加條件
all_other_needs <-function(){
  
  for(job_i in 1:length(job_only)){
    jgc()
    print(paste0(job_only[job_i],' 進行附加條件文字探勘與計算中'))
    people_sep <- people[which(people$職務小類==job_only[job_i]),]
    
    people_sep$附加條件 = gsub('\x9e','  ', people_sep$附加條件)
    ##去除英文的附加條件
    en_remove_index <-{}
    for(en.remove in 1:length(people_sep$附加條件)){
      if(length(gregexpr(pattern ='[a-z]',people_sep$附加條件)[[en.remove]])/nchar(people_sep$附加條件[en.remove])>0.5){
        en_remove_index <- c(en_remove_index,en.remove)
      }
    }
    if(toString(en_remove_index)==''){
      
    }else{
      people_sep <- people_sep[-en_remove_index,]
    }
    
    if(nrow(people_sep)>min_n_sample){
      review_text <- paste(people_sep$附加條件, collapse=" ")
      #review_text <- people_sep$附加條件
      review_text <- gsub("[\n]", "  ", review_text)
      #review_text = unlist(strsplit(review_text, "[，,。●;；]"))
      #review_text = unlist(strsplit(review_text, "[。●;；]"))
      #review_text = unlist(strsplit(review_text, "  "))
      #review_text = unlist(strsplit(review_text  , "[(][0-9][)]"))
      #review_text = unlist(strsplit(review_text  , "[（][0-9][）]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][.]"))
      #review_text = unlist(strsplit(review_text  , "[0-9][、]"))
      
      review_source <- VectorSource(review_text)
      d.corpus <- Corpus(review_source)
      
      d.corpus <- tm_map(d.corpus, removePunctuation) 
      d.corpus <- tm_map(d.corpus, removeNumbers) 
      d.corpus <- tm_map(d.corpus, content_transformer(tolower))
      d.corpus <- tm_map(d.corpus, function(word) {
        gsub("[0-9]", " ", word)
      })
      
      d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
      
      myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者","資料庫管理")
      d.corpus <- tm_map(d.corpus, removeWords, myStopWords)
      d.corpus <- tm_map(d.corpus, removeWords, stopwords("english")) 
      d.corpus <- tm_map(d.corpus, PlainTextDocument)
      tdm <- TermDocumentMatrixCN(d.corpus, control = list(wordLengths = c(2, Inf)))
      
      m1 <- as.matrix(tdm)
      v <- sort(rowSums(m1), decreasing = TRUE)
      d <- data.frame(word = names(v), freq = v)
      d$word <- as.character(d$word)
      d$percentage <- d$freq/nrow(people_sep)
      
      delete.word.vector <- c('以上','年以上','經驗','工作','公司','企業','加班','負責','配合','完成','地區','相關','與','完成','work','experience','進行','擔任','will','能力','基本','興趣','主要','具有','具備','面試','下班','上班','內容','薪資','完整','優先','自行','統一')
      for(delete.word.index in 1:length(delete.word.vector)){
        if(toString(which(d$word==delete.word.vector[delete.word.index]))!=''){
          d <- d[-which(d$word==delete.word.vector[delete.word.index]),]
        }
        
      }
      
      write.csv(d,paste0('附加條件\\整體\\',job_only[job_i],'附加條件文字Freq.csv'),row.names=F)
      
      png(paste0(output_path,'\\','附加條件\\整體\\',job_only[job_i],'_附加條件wordcloud.png'), width=800,height=800)
      
      if(length(d$freq)>=100){
        wordcloud(d$word[1:100], d$freq[1:100], random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }else{
        wordcloud(d$word, d$freq, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
      }
      dev.off()
      jgc()
      
      ##抓出前10名字串對應至附加條件
      for(i in 1:10){
        word.to.handle <- d$word[i]
        people_sep$附加條件處理過 <- people_sep$附加條件
        people_sep$附加條件處理過 <- tolower(people_sep$附加條件處理過)
        
        people_sep <- people_sep[order(-as.numeric(people_sep$資本金額),-as.numeric(people_sep$員工人數)),]
        job_description <- people_sep$附加條件處理過
        job_description = unlist(strsplit(job_description, "[。●;；]"))
        job_description = unlist(strsplit(job_description, "  "))
        job_description = unlist(strsplit(job_description  , "[(][0-9][)]"))
        job_description = unlist(strsplit(job_description  , "[（][0-9][）]"))
        job_description = unlist(strsplit(job_description  , "[0-9][.]"))
        job_description = unlist(strsplit(job_description  , "[0-9][、]"))
        
        job_description = trim(job_description)
        job.describe.df <- job_description[which(grepl(word.to.handle,job_description))]
        job.describe <- unique(job.describe.df)
        
        if(toString(job.describe)!=''){
          
          ##抓1~5大的公司的描述
          job.describe <- job.describe[1:5]
          
          job.describe <- as.data.frame(job.describe)
          job.describe$word <- word.to.handle
          job.describe <- job.describe[,c('word','job.describe')]
          
          if(i==1){
            write.table(job.describe, paste0('附加條件\\整體\\',job_only[job_i],'附加條件詞彙與內容對應結果.csv'), row.names=F, col.names=TRUE, sep=",")
            
          }else{
            write.table(job.describe, paste0('附加條件\\整體\\',job_only[job_i],'附加條件詞彙與內容對應結果.csv'), row.names=F,col.names=F, sep=",", append=TRUE)
            
          }
        }
      }
      
      print(paste0(format(round(job_i/length(job)*100,2),2),'%'))
      print(paste0(job_only[job_i],' 附加條件計算完成'))
    }else{
      print(paste0(job_only[job_i],' 樣本不足',min_n_sample,'，不予計算'))
    }
    
    
    
  }
  
}

##apriori 電腦專長
arule_computer_skills <- function(){
  library(arules)
  
  new_people = as.data.frame(cbind(people$職缺編號,people$職務小類,people$電腦專長))
  new_people[which(new_people[,3]=='NULL'),3] = ''
  new_people2 = new_people[which(new_people[,3]!=''),]
  colnames(new_people2) = c('職缺編號','職務小類','電腦專長')
  print(paste0('原資料筆數為 ', nrow(new_people),' 筆'))
  print(paste0('有填寫電腦專長資料筆數為 ', nrow(new_people2),' 筆'))
  #new_people2$職務小類 <- gsub('/','／',new_people2$職務小類)
  
  new_people2$職務小類 = as.character(new_people2$職務小類)
  new_people2$電腦專長 = as.character(new_people2$電腦專長)
  new_people2$職缺編號 = as.character(new_people2$職缺編號)
  
  job_list = unique(new_people$V2)
  
  for(job_num in 1:length(job_list)){
    job=toString(job_list[job_num])
    new_people3 = new_people2[which(new_people2$職務小類==job),]
    
    computer_skills_del <- c('Mac OS X','Windows Server 2000-2012','Windows NT','Windows Vista','windows 8','Word','Excel','PowerPoint','Outlook','Windows XP','Windows 7','lnternet Explorer','Windows 98')
    lev <- levels(as.factor(new_people3[,"電腦專長"])) #取得factor的level，此時都還是許多單一證照字串串起來的情況，故level數會很多
    lev <- unique(unlist(strsplit(lev, ","))) #以","將各單一證照字串分開，並去除重覆
    lev = lev [! lev %in% computer_skills_del]
    
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
      path_arule<-"C:\\Users\\abc\\Desktop\\廠商版職務大辭典\\分行業別output\\電腦專長arule"
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
      path_arule<-"C:\\Users\\abc\\Desktop\\廠商版職務大辭典\\分行業別output\\專業憑證arule"
      setwd(path_arule)
      sink(paste0(job,"專業憑證.csv"))
      inspect(sort(rules,by="support"))
      sink()
    }
    
  }
  setwd(output_path)
}

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