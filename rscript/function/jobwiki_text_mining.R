##廠商版職務大蒐秘

##挑出要處理的資料
data_processing_job <- function(){
  ##行業別與職務
  job_and_undustry = as.data.frame(table(people$行業與職務),stringsAsFactors=F)
  
  high_Freq_job_industry <- job_and_undustry[which(job_and_undustry$Freq>=100),]
  job <- c(high_Freq_job_industry$Var1)
  print(paste0('不同行業別 職務小類樣本數大於100者剩 ',length(job),' 筆'))
  
  return(job)
}
##挑出要處理的資料
data_processing_job_only <- function(){
  
  ##純職務
  job_only = as.data.frame(table(people$職務小類),stringsAsFactors=F)
  
  high_Freq_job_only <- job_only[which(job_only$Freq>=100),]
  job_only <- c(job_only$Var1)
  job_only <- job_only[-which(job_only=='NULL')]
  print(paste0('職務小類樣本數大於100者剩 ',length(job_only),' 筆'))
  return(job_only)
  
}

##工作說明
job_discription <-function(){
  
  for(job_i in 1:length(job)){
    print(paste0(job[job_i],' 進行工作說明文字探勘與計算中'))
    people_sep <- people[which(people$行業與職務==job[job_i]),]
    
    
    review_text <- paste(people_sep$工作說明, collapse=" ")
    review_text <- gsub("[\n]", "  ", review_text)
    
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
    d$percentage <- d$freq/sum(d$freq)
    
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
    
    
    ##抓出前10名字串對應至工作說明
    for(i in 1:10){
      no.inf <- 1
      word.to.handle <- d$word[i]
      people_sep$工作說明處理過 <- people_sep$工作說明
      people_sep$工作說明處理過 <- tolower(people_sep$工作說明處理過)
      job.describe.df <- people_sep[which(grepl(word.to.handle,people_sep$工作說明處理過)),]
      job.describe.df$員工人數<- as.numeric(job.describe.df$員工人數)
      job.describe.df$資本金額<- as.numeric(job.describe.df$資本金額)
      
      job.describe <- job.describe.df[order(-job.describe.df$資本金額,-job.describe.df$員工人數),]
      job.describe <- job.describe$工作說明處理過
      
      job.describe<-gsub("[\n]", "  ", job.describe)
      job.describe <- gsub("\\。", "  ", job.describe)
      
      if(grepl('  ', paste(job.describe, collapse=""))){
        job.describe <- strsplit(job.describe,'  ')
      }
      
      job.describe <- unlist(job.describe)
      job.describe <- job.describe[which(grepl(word.to.handle,job.describe))]
      job.describe <- unique(job.describe)
      
      if(toString(job.describe)!=''){
        ##試試看剔除英文的條件
        en.remove.index <-{}
        for(en.remove in 1:length(job.describe)){
          if(length(gregexpr(pattern ='[a-z]',job.describe)[[en.remove]])/nchar(job.describe[en.remove])>0.5){
            en.remove.index <- c(en.remove.index,en.remove)
          }
        }
        if(toString(en.remove.index)==''){
          
        }else{
          job.describe <- job.describe[-en.remove.index]
        }
        
        
        ##抓1~3大的公司的描述
        job.describe <- job.describe[1:3]
        
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
  }
  
}

##附加條件
other_needs <-function(){
  #job_and_undustry = as.data.frame(table(people$行業與職務),stringsAsFactors=F)
  
  #high_Freq_job_industry <- job_and_undustry[which(job_and_undustry$Freq>=100),]
  #job <- c(high_Freq_job_industry$Var1)
  #print(paste0('不同行業別 職務小類樣本數大於100者剩 ',length(job),' 筆'))
  
  for(job_i in 1:length(job)){
    print(paste0(job[job_i],' 進行附加條件文字探勘與計算中'))
    people_sep <- people[which(people$行業與職務==job[job_i]),]
    
    
    review_text <- paste(people_sep$附加條件, collapse=" ")
    review_text <- gsub("[\n]", "  ", review_text)
    
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
    d$percentage <- d$freq/sum(d$freq)
    
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
    
    
    ##抓出前10名字串對應至附加條件
    for(i in 1:10){
      no.inf <- 1
      word.to.handle <- d$word[i]
      people_sep$附加條件處理過 <- people_sep$附加條件
      people_sep$附加條件處理過 <- tolower(people_sep$附加條件處理過)
      job.describe.df <- people_sep[which(grepl(word.to.handle,people_sep$附加條件處理過)),]
      job.describe.df$員工人數<- as.numeric(job.describe.df$員工人數)
      job.describe.df$資本金額<- as.numeric(job.describe.df$資本金額)
      
      job.describe <- job.describe.df[order(-job.describe.df$資本金額,-job.describe.df$員工人數),]
      job.describe <- job.describe$附加條件處理過
      
      job.describe<-gsub("[\n]", "  ", job.describe)
      job.describe <- gsub("\\。", "  ", job.describe)
      
      if(grepl('  ', paste(job.describe, collapse=""))){
        job.describe <- strsplit(job.describe,'  ')
      }
      
      job.describe <- unlist(job.describe)
      job.describe <- job.describe[which(grepl(word.to.handle,job.describe))]
      job.describe <- unique(job.describe)
      
      if(toString(job.describe)!=''){
        ##試試看剔除英文的條件
        en.remove.index <-{}
        for(en.remove in 1:length(job.describe)){
          if(length(gregexpr(pattern ='[a-z]',job.describe)[[en.remove]])/nchar(job.describe[en.remove])>0.5){
            en.remove.index <- c(en.remove.index,en.remove)
          }
        }
        if(toString(en.remove.index)==''){
          
        }else{
          job.describe <- job.describe[-en.remove.index]
        }
        
        
        ##抓1~3大的公司的描述
        job.describe <- job.describe[1:3]
        
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
  }
  
}

##電腦專長
conputer_skills <- function(){
  for(job_i in 1:length(job_only)){
    print(paste0(job_only[job_i],' 進行電腦專長計算中'))
    #people_sep <- people[which(people$行業與職務==job[job_i]),]
    people_sep <- people[which(people$職務小類==job_only[job_i]),]
    
    people_computer_skills <- c(people_sep$電腦專長)
    people_computer_skills <- people_computer_skills[-which(people_computer_skills=='NULL')]
    people_computer_skills <- strsplit(people_computer_skills,',')
    people_computer_skills <- unlist(people_computer_skills)
    people_computer_skills <- as.data.frame(table(people_computer_skills),stringsAsFactors=F)
    people_computer_skills <- people_computer_skills[order(-people_computer_skills$Freq),]
    ##超過4成廠商所要求的再抓出
    computer_skills_del <- c('Windows Server 2000-2012','Windows NT','Windows Vista','windows 8','Word','Excel','PowerPoint','Outlook','Windows XP','Windows 7','lnternet Explorer','Windows 98')
    
    
    
    if(toString(nrow(people_computer_skills))!=""){
      
      #people_computer_skills <- people_computer_skills[which(people_computer_skills$Freq>=nrow(people_sep)*0.4),]
      if(toString(nrow(people_computer_skills))!="" & nrow(people_computer_skills)>=1){
        for(i in 1:length(computer_skills_del)){
          people_computer_skills = people_computer_skills[which(people_computer_skills[,1]!=computer_skills_del[i]),]
        }
        if(nrow(people_computer_skills)>10){
          people_computer_skills <- people_computer_skills[1:10,]
        }
        colnames(people_computer_skills)[1] = job_only[job_i]
        write.csv(people_computer_skills,paste0('電腦專長\\',job_only[job_i],'高頻電腦專長.csv'),row.names=F)
      }
    }
  }
}

##專業憑證
pro_certificate <- function(){
  for(job_i in 1:length(job_only)){
    print(paste0(job_only[job_i],' 進行專業憑證計算中'))
    people_sep <- people[which(people$職務小類==job_only[job_i]),]
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
        if(nrow(people_pro_certificate)>10){
          people_pro_certificate <- people_pro_certificate[1:10,]
        }
        
        colnames(people_pro_certificate)[1] = job_only[job_i]
        
        people_pro_certificate = t(people_pro_certificate)
        write.csv(people_pro_certificate,paste0('專業憑證\\',job_only[job_i],'高頻專業憑證.csv'))
      }
    }
  }
}
