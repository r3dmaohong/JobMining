library(RecordLinkage)
library(dplyr)

options(stringsAsFactors = FALSE)

trim <- function (x){
  gsub("^[　]+|[　]+$", "",gsub("^\\s+|\\s+$", "", x))
} 
trim_star <- function (x) gsub("^[*]+|[*]+$", "", x)
##去除前後標點除了引號
trim_punc <- function (x){
  #gsub("^[[:punct:]]+|[[:punct:]]+$", "", x)
  gsub("([()（）])|^[[:punct:]]+|[[:punct:]]+$", "\\1", x)
}

trim_mix = function(x){
  x = gsub("^\\)+|\\(+$",'',x)
  ##trim要重複用
  ##才刪個乾淨
  
  x = gsub("～",' ',x)
  x = gsub("^[0-9０-９a-zA-Z一-十四√]+[:、，,-‧ ．)）.]",'',x)
  
  ##..國字也可以?真是驚奇的發現
  ##真怪 四沒自動處理掉
  x = gsub("^[(（][0-9０-９a-zA-Z一-十四]+[)）]",'',x)
  
  x = gsub("^◎",'',x)
  
  if(grepl('^[(（＜]+',x) & grepl('[)）＞]+$',x)){
    x = gsub("^[(（＜]+|[)）＞]+$",'',x)
  }
  x = gsub("^[0-9０-９]+[-][0-9０-９]+",'',x)
  
  if(grepl('\\)',x) & grepl('\\(',x) & length(unlist(gregexpr('\\)',x)))==1 & length(unlist(gregexpr('\\(',x)))==1){
    if(unlist(gregexpr('\\)',x)) < unlist(gregexpr('\\(',x))){
      x='' ##最後要移除
    }
  }
  
  if(grepl('[0-9][0-9]:[0-9][0-9]',x)){
    x = ''
  }
  ##有時間就移除
  if(grepl('[0-9][/][0-9]',x)){
    x = ''
  }
  return(x)
}

trim_du <- function(x){
  ##次數大於1移除?
  #if(length(unlist(gregexpr(pattern ="[0-9０-９a-zA-Z一-十四√]+[:、，,-‧ ．)）.]",x)))>1 | length(unlist(gregexpr(pattern ="[0-9０-９a-zA-Z]+[.]",x)))>1){
  if(grepl("[0-9０-９a-zA-Z一-十四√]+[:、，,-‧ ．)）.]",x)){
    x = '' 
  }else{
    x = x
  }
  return(x)
}


job_d  = read.csv(file.choose())

##fuzzy matching
job_d_list = job_d[,1:2]
job_d_list = unique(job_d_list)

new_job_df = job_d[0,]

x = 1
for(i in 1:nrow(job_d_list)){
  tmp = job_d[which(job_d[,1]==job_d_list[i,1] & job_d[,2]==job_d_list[i,2]),]
  wordlist <- expand.grid(words = tmp[,3], ref = tmp[,3], stringsAsFactors = FALSE)
  fuzzy_matching = wordlist %>% mutate(match_score = jarowinkler(words, ref))
  
  fuzzy_matching = fuzzy_matching[order(fuzzy_matching$match_score),]
  
  #fuzzy_matching[,1]  
  #fuzzy_matching[,2]
  ord_v = unique(trim(unlist(strsplit(paste(fuzzy_matching[,1],'。', fuzzy_matching[,2]),'。'))))
  
  temp = c()
  for(j in 1:length(unique(substr(ord_v,1,5)))){
    temp = c(temp, ord_v[which(unique(substr(ord_v,1,5))[j]==substr(ord_v,1,5))][1])
  }
    
  ord_v = temp[1:20]
  ord_v = sort(ord_v)
  ord_v = ord_v[!is.na(ord_v)]
  
  ord_v_rm = c()
  for(check_i in 1:length(ord_v)){
    ##找出有grepl的但是又根本身不一樣?
    if(length(which(grepl(ord_v[check_i],ord_v,fixed=TRUE)))==1){
      ord_v_rm = c(ord_v_rm, ord_v[check_i])
    }
  }
  ord_v = ord_v_rm
  
  new_job_df[x:(x+length(ord_v)-1),1:2] = tmp[1:(length(ord_v)),1:2]
  new_job_df[x:(x+length(ord_v)-1),3] = ord_v
  
  cat(paste0('\r',format(round(i/nrow(job_d_list)*100,3),nsmall=3),'%'))
  x = x + length(ord_v)
}


##
##最後提供10筆?
##該random?
##或是再一次挑不相關者?
##還是全上?

##random後蒐集常被用的
##然後把順序條上去?
tmp
##或是把相似的抓出來!?
##第一波篩過了
##那第二波是否相似反而證明這些能力是主要的?
new_job_df[,3] = trim(new_job_df[,3])
new_job_df[,3] = trim_star(new_job_df[,3])
new_job_df[,3] = trim_punc(new_job_df[,3])
new_job_df[,3] = trim_mix(new_job_df[,3])
#for(i in 1:nrow(new_job_df)){
#  new_job_df[i,3] = trim_du(new_job_df[i,3])
#}
new_job_df[,3] = unlist(lapply(new_job_df[,3],trim_du))

new_job_df[,3] = trim(new_job_df[,3])
new_job_df[,3] = trim_star(new_job_df[,3])
new_job_df[,3] = trim_punc(new_job_df[,3])
new_job_df[,3] = trim_mix(new_job_df[,3])
new_job_df[,3] = unlist(lapply(new_job_df[,3],trim_du))

##附加再用這個
#new_job_df = new_job_df[which(!grepl('.com',new_job_df[,3],fixed=T) & !grepl('電話',new_job_df[,3]) & !grepl('來電',new_job_df[,3]) & !grepl('font',new_job_df[,3]) & !grepl('電話',new_job_df[,3]) & !grepl('e-mail',new_job_df[,3]) & !grepl('【',new_job_df[,3]) & !grepl('★',new_job_df[,3]) & !grepl('☆',new_job_df[,3]) & !grepl('◆',new_job_df[,3]) & !grepl('■',new_job_df[,3]) & !grepl('】',new_job_df[,3])),]
new_job_df = new_job_df[which(!grepl('.com',new_job_df[,3],fixed=T) & !grepl('font',new_job_df[,3]) & !grepl('【',new_job_df[,3]) & !grepl('★',new_job_df[,3]) & !grepl('☆',new_job_df[,3]) & !grepl('◆',new_job_df[,3]) & !grepl('■',new_job_df[,3]) & !grepl('】',new_job_df[,3])),]

new_job_df = new_job_df[which(new_job_df[,3]!=''),]

new_job_df = new_job_df[which(!grepl('^[(（＜]+',new_job_df[,3])),]

#write.csv(new_job_df,'D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\[篩選後2]整體工作說明fuzzymatch後整理結果.csv',row.names=F)

#write.csv(new_job_df,'D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\[篩選後2]整體附加條件fuzzymatch後整理結果.csv',row.names=F)
