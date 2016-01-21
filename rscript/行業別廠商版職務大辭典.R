##source('D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki\\rscript\\廠商版職務大辭典.R', print.eval  = TRUE)

##廠商版職務大辭典
rm(list = ls()) #去除工作空間中所有物件

library(XML)
library(RCurl)
library(tm)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(SnowballC)
library(cluster)   
library(ggplot2) 
library(data.table) 

gc() #記憶體釋放 

path<-"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki"
#讀取多個csv至df
data_path<-paste0(path,"\\撈取資料")
setwd(data_path)
 
files = list.files(pattern="*.csv")
print('讀取資料中')
temp <- lapply(files, fread, sep=",")
people <- rbindlist(temp)
print('讀取資料完畢')
##text mining
#path<-"D:\\abc\\wjhong\\projects\\廠商版職務大蒐秘\\jobwiki"
setwd(path)
start.time<-Sys.time()

job_type <- as.data.frame(table(people$職務小類),stringsAsFactors=F)
job_type <- job_type[order(rank(-job_type$Freq)),]

industry_list <- read.csv('產業中類名稱.csv',stringsAsFactors=F)
industry_list <- industry_list[order(industry_list[,1]),]
industry_list <- c(industry_list)

error_industry <- as.data.frame(table(people$行業別),stringsAsFactors=F)

##兩個階段處理比單一for直接處理較快
for(i in 1:nrow(error_industry)){
  for(j in 1:length(industry_list)){
    if(grepl(industry_list[j],error_industry$Var1[i])){
      error_industry$Var2[i] = industry_list[j]
      print(paste0(error_industry$Var1[i],"==>",industry_list[j]))
    }
  }
  
  print(paste0('行業別轉換階段一 第',i,'筆，',round(i/nrow(error_industry)*100,3),' %'))
}

gc() #記憶體釋放 

people$產業中類 <- people$行業別

for(i in 1:nrow(error_industry)){
  people$產業中類[which(people$產業中類==error_industry$Var1[i])]=error_industry$Var2[i]
  print(paste0('行業別轉換階段二 第',i,'筆，',round(i/nrow(error_industry)*100,3),' %'))
}
gc() #記憶體釋放 

##避免斜線輸出檔案錯誤
people$職務小類 <- gsub('/','／',people$職務小類)
people$行業與職務 = paste0(people$職務小類,' - ',people$產業中類)

print('行業別處理完成')

##
##修正termdocumentmatrix問題用
source(paste0(path,'\\rscript\\function\\error_solve_termdocumentmatrix.R'), print.eval  = TRUE)
##分析相關function
source(paste0(path,'\\rscript\\function\\jobwiki_text_mining.R'), print.eval  = TRUE)


##從這設定儲存位置好了
output_path<-paste0(path,"\\分行業別output")
setwd(output_path)






##取出要處理的資料
job <- data_processing_job()

##工作說明
job_discription()

##附加條件
other_needs()

##取出要處理的資料
job_only <- data_processing_job_only()
##電腦專長
##改先抓共同高的，把這些替除，推薦特殊的
computer_skills()

##專業憑證
pro_certificate()

##整體工作說明
all_job_discription()

##整體附加條件
all_other_needs()

##職務類別
##找出各職務的詞
##找出整體高頻詞
##將各職務的詞去除高頻詞 >0.5?
##再找出各職務的高頻詞 >0.5?

##利用apriori抓出相關證照
arule_computer_skills()
arule_pro_certificate()

##將兩者抓出之證照取交集
intersect_computer_skills()
intersect_pro_certificate()

#系統時間
end.time <- Sys.time()
#記錄一段程序結束執行時間
run.time <- end.time - start.time
run.time