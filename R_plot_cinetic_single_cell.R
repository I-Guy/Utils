## Author  : Ilango Guy
## CEPR
plot_cinetic <- function(table1, cluster, table2){
  p<-ggplot(table1 %>% filter(clusters == cluster) , aes(x = timepoint  , y = freq   , group = 1) ) +geom_line( linewidth = 1)
  
  p<- p + annotate("text", x = "J4", y = as.numeric(table1 %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) -5, label = head(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[5] , color ="blue")
  p<- p + annotate("text", x = "J4", y = as.numeric(table1 %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) -4, label = head(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[4] , color ="blue")
  p <- p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) -3, label = head(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[3] , color ="blue")
  p<-p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) -2, label = head(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[2] , color ="blue")
  p<- p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) -1, label =head(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[1] , color ="blue")
  p<- p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) +5, label = tail(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[1] , color ="red")
  p<- p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters ==cluster , timepoint == "J4") %>% select(freq)) +4, label = tail(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[2] , color ="red")
  p<- p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) +3, label = tail(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[3] , color ="red")
  p<- p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) +2, label = tail(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[4] , color ="red")
  p<- p + annotate("text", x = "J4", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J4") %>% select(freq)) +1, label = tail(rownames(arrange(table2 %>% filter(condition == "NI_J4") , avg_log2FC) , 5))[5] , color ="red")
  
  p<- p + annotate("text", x = "J7", y = as.numeric(table1 %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) -5, label = head(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[5] , color ="blue")
  p<- p + annotate("text", x = "J7", y = as.numeric(table1 %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) -4, label = head(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[4] , color ="blue")
  p <- p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) -3, label = head(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[3] , color ="blue")
  p<-p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) -2, label = head(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[2] , color ="blue")
  p<- p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) -1, label =head(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[1] , color ="blue")
  p<- p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) +5, label = tail(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[1] , color ="red")
  p<- p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters ==cluster , timepoint == "J7") %>% select(freq)) +4, label = tail(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[2] , color ="red")
  p<- p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) +3, label = tail(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[3] , color ="red")
  p<- p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) +2, label = tail(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[4] , color ="red")
  p<- p + annotate("text", x = "J7", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J7") %>% select(freq)) +1, label = tail(rownames(arrange(table2 %>% filter(condition == "J4_J7") , avg_log2FC) , 5))[5] , color ="red")
  
  p<- p + annotate("text", x = "J12", y = as.numeric(table1 %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) -5, label = head(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[5] , color ="blue")
  p<- p + annotate("text", x = "J12", y = as.numeric(table1 %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) -4, label = head(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[4] , color ="blue")
  p <- p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) -3, label = head(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[3] , color ="blue")
  p<-p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) -2, label = head(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[2] , color ="blue")
  p<- p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) -1, label =head(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[1] , color ="blue")
  p<- p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) +5, label = tail(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[1] , color ="red")
  p<- p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters ==cluster , timepoint == "J12") %>% select(freq)) +4, label = tail(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[2] , color ="red")
  p<- p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) +3, label = tail(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[3] , color ="red")
  p<- p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) +2, label = tail(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[4] , color ="red")
  p<- p + annotate("text", x = "J12", y = as.numeric(all_cell %>% filter(clusters == cluster , timepoint == "J12") %>% select(freq)) +1, label = tail(rownames(arrange(table2 %>% filter(condition == "J7_J12") , avg_log2FC) , 5))[5] , color ="red")
  return(p)
  
}

plot_cinetic(all_cell ,  0,  markers_0)
