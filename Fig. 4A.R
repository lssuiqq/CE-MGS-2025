#??ȡ????
phylum_top10 <- read.delim('phylum_top15.txt', row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)

library(reshape2)
library(ggplot2)

#?????? ggplot2 ??ͼ??ʽ
phylum_top10$Taxonomy <- factor(rownames(phylum_top10), levels = rev(rownames(phylum_top10)))
phylum_top10 <- melt(phylum_top10, id = 'Taxonomy')

#???ӷ??飬???????Ǹ??????????????Ʒ???
group <- read.delim('group.txt', sep = '	', stringsAsFactors = FALSE)
names(group)[1] <- 'variable'
phylum_top10 <- merge(phylum_top10, group, by = 'variable')
phylum_top10$group <- factor(phylum_top10$group, levels=c("B_15","B_30","B_45","R_15","R_30","R_45"))

#???ƴ?????????״ͼ
p5 <- ggplot(phylum_top10, aes(variable, 100 * value, fill = Taxonomy)) +
  geom_col(position = 'stack', width = 0.9) +
  facet_wrap(~group, scales = 'free_x', ncol = 3) +
  #scale_fill_manual(values =  rev(c('#756FB3', '#D95F02', '#1B9E77', '#FFD92E', '#A6D753', '#E789C3', 'gray'))) +
  scale_fill_manual(values =  rev(c('#756FB3', '#D95F02', '#1B9E77', 
                                    '#FFD92E'))) +
  labs(x = '', y = 'Relative Abundance (%)') +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        strip.background = element_rect(color = "black"),
        strip.text = element_text(face="bold", size = 7)) +
  theme(axis.text.x = element_text(color="black", size = 5, angle = 45, hjust = 1,vjust = 1),
        axis.text.y = element_text(color="black", size = 6),
        axis.title = element_text(face="bold", size = 10), 
        legend.title = element_text(face="bold", size = 7),
        #legend.title = element_blank(),
        legend.text = element_text(size = 6.5),
        legend.key.size = unit(0.15, "inches"),
        axis.ticks = (element_line(color = "black", linewidth = 0.2)))#+
  #theme(legend.position = 'none')
  
#ggsave('5. phylum_top10.pdf', p5, width = 15, height = 7)
#ggsave('ggplot2_plot.png', p, width = 25, height = 10)
p5
