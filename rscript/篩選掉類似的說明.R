options(stringsAsFactors = FALSE)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

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
  
  new_job_df[x:(x+length(ord_v)-1),1:2] = tmp[1:(length(ord_v)),1:2]
  new_job_df[x:(x+length(ord_v)-1),3] = ord_v
  
  cat(paste0('\r',format(round(i/nrow(job_d_list)*100,3),nsmall=3),'%'))
  x = x + length(ord_v)
}

write.csv(new_job_df,'D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\[篩選後]整體工作說明fuzzymatch後整理結果.csv',row.names=F)
