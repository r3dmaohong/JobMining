##source('D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\rscript\\廠商版職務大辭典.R', print.eval  = TRUE)

##廠商版職務大辭典
rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory 

library(XML)
library(RCurl)
library(tm)
options(java.parameters = "-Xmx8g" )
library(tmcn)
library(Rwordseg)
library(SnowballC)
library(cluster)   
library(ggplot2) 
library(data.table) 
library(doSNOW)
library(foreach)
library(dplyr)
library(stringr)


setwd(".\\廠商版職務大蒐秘\\jobwiki")

##Part1 or Part2?

#######################
######## Part1 ########
#######################

#Read mutiple csv files and convert them to one data frame
files <- list.files("撈取資料", pattern="*.csv", full.names = TRUE)
cat("\n讀取資料中")
temp <- lapply(files, fread, sep=",")
people <- rbindlist(temp)
cat("\n讀取資料完畢")
##text mining
#path<-"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki"

#Record time
start.time <- Sys.time()

##Remove original jobwiki's content
#Code in this paragraph may be WEIRD.., but it's writen long time ago, just don't touch it currently...
jobwiki_old_discription <- read.csv("jobwiki_discription.csv", stringsAsFactors=F)
jobwiki_old_discription <- unlist(strsplit(jobwiki_old_discription$工作內容  , "[0-9][.]"))
jobwiki_old_discription <- unlist(strsplit(jobwiki_old_discription  , "[（][0-9][）]"))
jobwiki_old_discription <- unlist(strsplit(jobwiki_old_discription  , "[(][0-9][)]"))
jobwiki_old_discription <- gsub(" ", "", jobwiki_old_discription)
jobwiki_old_discription <- gsub("<br>", "",jobwiki_old_discription)
jobwiki_old_discription <- sort(jobwiki_old_discription)
jobwiki_old_discription <- gsub("^[.]", "", jobwiki_old_discription)
jobwiki_old_discription <- gsub("A.工作內容", "", jobwiki_old_discription)
jobwiki_old_discription <- gsub("A.工作內容:", "", jobwiki_old_discription)
jobwiki_old_discription <- jobwiki_old_discription[-which(jobwiki_old_discription=="")]

#people$工作說明 <- sapply(jobwiki_old_discription, function(x){
#  gsub(x, "", people$工作說明)
#})
#number of CPU cores
#cl <- makeCluster(2)
#registerDoSNOW(cl)

##Remove original jobwiki's content
#foreach is much slower...!?
for(i in 1:length(jobwiki_old_discription)){
  ##str_replace_all is faster than gsub.
  #people$工作說明 <- gsub(jobwiki_old_discription[i], "", people$工作說明)
  #people$工作說明 <- str_replace_all(people$工作說明, jobwiki_old_discription[i], "")
  people$工作說明 <- str_replace_all(people$工作說明, jobwiki_old_discription[i] %>% gsub("（", "\\（", fixed=TRUE), "")
  
  cat("\r Job discription processing... : ", (i/length(jobwiki_old_discription)*100) %>% round(., 3) %>% format(nsmall=3), "%", rep(" ", 50))
  gc()
}

job_type <- as.data.frame(table(people$職務小類), stringsAsFactors=F)
job_type <- job_type[order(rank(-job_type$Freq)), ]

industry_list <- read.csv("產業中類名稱.csv", stringsAsFactors=F)
industry_list <- industry_list[order(industry_list[,1]), ]
industry_list <- c(industry_list)
  
error_industry <- as.data.frame(table(people$行業別), stringsAsFactors=F)
  
##兩個階段處理比單一for直接處理較快
##In error_industry, names of industry are wrong...
for(i in 1:nrow(error_industry)){
  for(j in 1:length(industry_list)){
    if(grepl(industry_list[j],error_industry$Var1[i])){
      error_industry$Var2[i] = industry_list[j]
      print(paste0(error_industry$Var1[i]," ==> ",industry_list[j]))
    }
  }
  cat("\rIndustry correction - First stage: ", i, " => ", round(i/nrow(error_industry)*100,3) %>% format(., nsmall=3), " %", rep(" ", 50))
}
  
##Free up the memory 
gc()

people$產業中類 <- people$行業別

start.time <- Sys.time()
for(i in 1:nrow(error_industry)){
  people$產業中類[which(people$產業中類==error_industry$Var1[i])] <- error_industry$Var2[i]
  cat("\rIndustry correction - Second stage: ", i, " => ", round(i/nrow(error_industry)*100,3) %>% format(., nsmall=3), " %", rep(" ", 50))
}
Sys.time() - start.time

gc() 

people$職務小類   <- str_replace_all(people$職務小類, "/", "／")
people$行業與職務 <- paste0(people$職務小類, " - ", people$產業中類)

print("Industry correction process complete")

##
##Solving termdocumentmatrix error
source("rscript\\function\\error_solve_termdocumentmatrix.R", print.eval  = TRUE)
##main analysis function
source("rscript\\function\\jobwiki_text_mining.R", print.eval  = TRUE)

people$syear = NULL
people$smonth = NULL
people$統一編號 = NULL
people$公司名稱 = NULL
people$職務名稱 = NULL
people$職務中類 = NULL
people$工作地點 = NULL
people$學歷限制 = NULL
people$科系限制 = NULL

###save.image("DataProcessed")

#######################
######## Part2 ########
#######################

##load("DataProcessed")

##取出要處理的資料
job <- data_processing_job()

##工作說明
job_discription()

##附加條件
other_needs()

##取出要處理的資料
job_only <- data_processing_job_only()

##整體工作說明
all_job_discription()

##整體附加條件
all_other_needs()

#系統時間
end.time <- Sys.time()
#記錄一段程序結束執行時間
run.time <- end.time - start.time
run.time

##職務類別
##找出各職務的詞
##找出整體高頻詞
##將各職務的詞去除高頻詞 >0.5?
##再找出各職務的高頻詞 >0.5?


##電腦專長
##改先抓共同高的，把這些替除，推薦特殊的
computer_skills()

##專業憑證
pro_certificate()

##利用apriori抓出相關證照
arule_computer_skills()
arule_pro_certificate()

##將兩者抓出之證照取交集
intersect_computer_skills()
intersect_pro_certificate()

temp = read.csv('D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\專業憑證交集.csv',header = F,stringsAsFactors=F)
colnames(temp) = c('職務小類名稱',paste0('證照名稱',2:ncol(temp)))
for(i in 1:ncol(temp)){
  temp[which(is.na(temp[,i])),i] = ''
}
temp = temp[which(temp[,2]!=''),]
write.csv('專業憑證交集整理後.csv',row.names=F)

temp = read.csv('D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\分行業別output\\電腦專長交集.csv',header = F,stringsAsFactors=F)
colnames(temp) = c('職務小類名稱',paste0('證照名稱',2:ncol(temp)))
for(i in 1:ncol(temp)){
  temp[which(is.na(temp[,i])),i] = ''
}
temp = temp[which(temp[,2]!=''),]
write.csv('電腦專長交集整理後.csv',row.names=F)
#系統時間
end.time <- Sys.time()
#記錄一段程序結束執行時間
run.time <- end.time - start.time
run.time