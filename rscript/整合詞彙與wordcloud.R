##
##Word freq to total form.
##
rm(list = ls()) 
gc() 
setwd(".\\分行業別output")

library(wordcloud)

file_names <- list.files(".\\工作說明", pattern="*工作說明文字Freq.csv")
file_names <- c(file_names, list.files(".\\工作說明\\整體", pattern="*工作說明文字Freq.csv"))

integrated_output <- data.frame("職務名稱"=character(), "行業別"=character(), "詞彙"=character(), "次數"=character(), stringsAsFactors=F)
 
for(i in 1:length(file_names)){
  ## content with industry.
  if(grepl(" - ", file_names[i])){
    tmp               <- read.csv(file_names[i], stringsAsFactors=F)
    colnames(tmp)     <- c("詞彙","次數","百分比")
    tmp$職務名稱      <- substr(file_names[i], 1, unlist(gregexpr(pattern =" - ", file_names[i]))-1)
    tmp$行業別        <- substr(file_names[i], unlist(gregexpr(pattern ="- ",file_names[i]))+2, unlist(gregexpr(pattern ="工作說明文字", file_names[i]))-1)
    tmp               <- tmp[, c("職務名稱","行業別","詞彙","次數")]
    tmp$詞彙          <- toupper(tmp$詞彙)
    integrated_output <- rbind(integrated_output, tmp)
    cat(paste0("\r", format(round(i/length(file_names)*100,2), nsmall=2), "%"))
    
    ##wordcloud
    png(paste0(".\\分行業別output\\工作說明png\\", tmp$職務名稱[1], " - ", tmp$行業別[1], "_工作說明wordcloud.png"), width=600, height=600)
    wordcloud(tmp$詞彙, tmp$次數, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
    dev.off()
  }else{
    #without industry
    tmp               <- read.csv(paste0("整體\\",file_names[i]), stringsAsFactors=F)
    colnames(tmp)     <- c("詞彙","次數","百分比")
    tmp$職務名稱      <- substr(file_names[i], 1, unlist(gregexpr(pattern ="工作說明文字", file_names[i]))-1)
    tmp$行業別        <- "整體"
    tmp               <- tmp[, c("職務名稱","行業別","詞彙","次數")]
    tmp$詞彙          <- toupper(tmp$詞彙)
    integrated_output <- rbind(integrated_output, tmp)
    cat(paste0("\r", format(round(i/length(file_names)*100,2), nsmall=2), "%"))
    
    png(paste0(".\\分行業別output\\工作說明png\\", tmp$職務名稱[1], " - ", tmp$行業別[1], "_工作說明wordcloud.png"), width=600, height=600)
    wordcloud(tmp$詞彙, tmp$次數, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
    dev.off()
  }  
}

integrated_output <- integrated_output[order(integrated_output$職務名稱, integrated_output$行業別, -integrated_output$次數), ]
write.csv(integrated_output, ".\\分行業別output\\工作說明文字Freq總表.csv", row.names=F)