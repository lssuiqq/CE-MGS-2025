library(tidyverse)
library(ggsci)
library(ggplot2)

data <- read.delim("MAG_count.txt", header = T)
head(data)
data$Group <- as.factor(data$Group) #指定因子
data$Group <- factor(data$Group, levels=c('BCL','BCM','BCH','RCL','RCM','RCH','OBS','ORS'))#设置顺序
data$Sample <- factor(data$Sample, levels=c("RS","BS","R_45_T_3","R_45_T_2","R_45_T_1","R_45_R_3","R_45_R_2","R_45_R_1","R_45_N_3","R_45_N_2","R_45_N_1","R_45_M_3","R_45_M_2","R_45_M_1","R_45_A_3","R_45_A_2","R_45_A_1","R_30_T_3","R_30_T_2","R_30_T_1","R_30_R_3","R_30_R_2","R_30_R_1","R_30_N_3","R_30_N_2","R_30_N_1","R_30_M_3","R_30_M_2","R_30_M_1","R_30_A_3","R_30_A_2","R_30_A_1","R_15_T_3","R_15_T_2","R_15_T_1","R_15_R_3","R_15_R_2","R_15_R_1","R_15_N_3","R_15_N_2","R_15_N_1","R_15_M_3","R_15_M_2","R_15_M_1","R_15_A_3","R_15_A_2","R_15_A_1","B_45_T_3","B_45_T_2","B_45_T_1","B_45_R_3","B_45_R_2","B_45_R_1","B_45_N_3","B_45_N_2","B_45_N_1","B_45_M_3","B_45_M_2","B_45_M_1","B_45_A_3","B_45_A_2","B_45_A_1","B_30_T_3","B_30_T_2","B_30_T_1","B_30_R_3","B_30_R_2","B_30_R_1","B_30_N_3","B_30_N_2","B_30_N_1","B_30_M_3","B_30_M_2","B_30_M_1","B_30_A_3","B_30_A_2","B_30_A_1","B_15_T_3","B_15_T_2","B_15_T_1","B_15_R_3","B_15_R_2","B_15_R_1","B_15_N_3","B_15_N_2","B_15_N_1","B_15_M_3","B_15_M_2","B_15_M_1","B_15_A_3","B_15_A_2","B_15_A_1"))#设置顺序

# 画图的代码不需要任何修改
#color=c('#6EA2DB','#45A8AC','#DC8F95','#3751A6','#4daf4a','#e41a1c','grey','grey')#颜色变量
ggplot(data, aes(x = Sample, y = MAG_count,color=Group)) +
  geom_segment(aes(x = Sample, 
                   xend = Sample, 
                   y = 0, 
                   yend = MAG_count,
                   color= Group)) +
  geom_point(aes(x = Sample, y = MAG_count, size = MAG_count)) + #tat='identity', (aes(),size=4)
  geom_text(aes(label=MAG_count),color="white", size=1.5) +
  #scale_y_discrete(position = 'right')+
  scale_color_manual(values = c('#6EA2DB','#45A8AC','#DC8F95','#3751A6','#4daf4a','#e41a1c','black','black'))+
  labs(color = 'Group') +
  theme_test() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.3),
        legend.position = c(0.7, 0.7),
        legend.direction = "vertical")+
  coord_flip()

