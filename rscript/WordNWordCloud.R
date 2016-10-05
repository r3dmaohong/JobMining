#####################################
#####################################
##Word Frequency and Wordcloud Export
#####################################
#####################################

rm(list = ls()) 
gc() 
setwd(".\\����~�Ooutput")

library(wordcloud)

file_names <- list.files(".\\�u�@����", pattern="*�u�@������rFreq.csv")
file_names <- c(file_names, list.files(".\\�u�@����\\����", pattern="*�u�@������rFreq.csv"))

integrated_output <- data.frame("¾�ȦW��"=character(), "��~�O"=character(), "���J"=character(), "����"=character(), stringsAsFactors=F)
 
for(i in 1:length(file_names)){
  ## content with industry.
  if(grepl(" - ", file_names[i])){
    tmp               <- read.csv(file_names[i], stringsAsFactors=F)
    colnames(tmp)     <- c("���J", "����", "�ʤ���")
    tmp$¾�ȦW��      <- substr(file_names[i], 1, unlist(gregexpr(pattern =" - ", file_names[i]))-1)
    tmp$��~�O        <- substr(file_names[i], unlist(gregexpr(pattern ="- ",file_names[i]))+2, unlist(gregexpr(pattern ="�u�@������r", file_names[i]))-1)
    tmp               <- tmp[, c("¾�ȦW��", "��~�O", "���J", "����")]
    tmp$���J          <- toupper(tmp$���J)
    integrated_output <- rbind(integrated_output, tmp)
    cat(paste0("\r", format(round(i/length(file_names)*100,2), nsmall=2), "%"))
    
    ##wordcloud
    png(paste0(".\\����~�Ooutput\\�u�@����png\\", tmp$¾�ȦW��[1], " - ", tmp$��~�O[1], "_�u�@����wordcloud.png"), width=600, height=600)
    wordcloud(tmp$���J, tmp$����, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
    dev.off()
  }else{
    #without industry
    tmp               <- read.csv(paste0("����\\",file_names[i]), stringsAsFactors=F)
    colnames(tmp)     <- c("���J","����","�ʤ���")
    tmp$¾�ȦW��      <- substr(file_names[i], 1, unlist(gregexpr(pattern ="�u�@������r", file_names[i]))-1)
    tmp$��~�O        <- "����"
    tmp               <- tmp[, c("¾�ȦW��", "��~�O", "���J", "����")]
    tmp$���J          <- toupper(tmp$���J)
    integrated_output <- rbind(integrated_output, tmp)
    cat(paste0("\r", format(round(i/length(file_names)*100,2), nsmall=2), "%"))
    
    png(paste0(".\\����~�Ooutput\\�u�@����png\\", tmp$¾�ȦW��[1], " - ", tmp$��~�O[1], "_�u�@����wordcloud.png"), width=600, height=600)
    wordcloud(tmp$���J, tmp$����, random.order = F, scale=c(10, .5), colors=brewer.pal(6, "Dark2"))
    dev.off()
  }  
}

integrated_output <- integrated_output[order(integrated_output$¾�ȦW��, integrated_output$��~�O, -integrated_output$����), ]
write.csv(integrated_output, ".\\����~�Ooutput\\�u�@������rFreq�`��.csv", row.names=F)