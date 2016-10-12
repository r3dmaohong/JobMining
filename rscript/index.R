##source('D:\\abc\\wjhong\\projects\\JobMining\\rscript\\index.R', print.eval  = TRUE)

##JobMining of companys
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
library(stringr)


setwd(".\\JobMining")

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

#Record time
#start.time <- Sys.time()

##Remove original Jobwiki's content
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
  people$工作說明 <- str_replace_all(people$工作說明, gsub("（", "\\（", jobwiki_old_discription[i], fixed=TRUE), "")
  
  cat("\r Job discription processing... : ", (i/length(jobwiki_old_discription)*100) %>% round(., 3) %>% format(nsmall=3), "%", rep(" ", 50))
  gc()
}

job_type <- as.data.frame(table(people$職務小類), stringsAsFactors=F)
job_type <- job_type[order(rank(-job_type$Freq)), ]

industry_list <- read.csv("產業中類名稱.csv", stringsAsFactors=F)
industry_list <- industry_list[order(industry_list[,1]), ]
industry_list <- c(industry_list)
  
error_industry <- as.data.frame(table(people$行業別), stringsAsFactors=F)

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

#start.time <- Sys.time()
for(i in 1:nrow(error_industry)){
  people$產業中類[which(people$產業中類==error_industry$Var1[i])] <- error_industry$Var2[i]
  cat("\rIndustry correction - Second stage: ", i, " => ", round(i/nrow(error_industry)*100,3) %>% format(., nsmall=3), " %", rep(" ", 50))
}
#Sys.time() - start.time

gc() 

people$職務小類   <- str_replace_all(people$職務小類, "/", "／")
people$行業與職務 <- paste0(people$職務小類, " - ", people$產業中類)

print("Industry correction process complete")

##

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
detach("package:tmcn", unload=TRUE)
library(tmcn)
detach("package:Rwordseg", unload=TRUE)
library(Rwordseg)

##Main functions
##Solving termdocumentmatrix error
source("rscript\\function\\error_solve_termdocumentmatrix.R", print.eval  = TRUE)
source("rscript\\function\\JobMining_main.R", print.eval  = TRUE)

##Data Extraction
job      <- jobDataExtraction(people, total=F)
job_only <- jobDataExtraction(people, total=T)
job      <- job[!grepl("工讀生", job)]
job      <- job[str_count(job, "人力")!=1 | grepl("人事", job)]
job_only <- job_only[!grepl("工讀生", job_only)]

##Job with industry
discriptionMining(people, job, "工作說明", total=F)
##Job with no industry
discriptionMining(people, job_only, "工作說明", total=T)

##Other needs..
##Job with industry
#discriptionMining(people, job, "附加條件", total=F)
##Job with no industry
#discriptionMining(people, job_only, "附加條件", total=T)

##Computer skills and certification
specialtyAnalysis(people, job_only, "專業憑證")
specialtyAnalysis(people, job_only, "電腦專長")

##########################
##Next => DiscriptionETL.R
##########################