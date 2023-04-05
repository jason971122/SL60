library(vegan)
library(plyr)
library(dplyr)
library(ggrepel)
library(ggsci)



metadata <- read.delim("sample_matedata-1.txt", sep = '\t', stringsAsFactors = FALSE)
group_name <- unique(metadata$Grouping_1)
metadata$Grouping_1 <- ordered(metadata$Grouping_1,levels = group_name)


asv <- read.csv("asv-table-compare.tsv", sep = "\t", row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
asv <- asv[,-ncol(asv)]
asv_t <- t(asv)


####PCA
asv_hel <- decostand(asv_t, method = 'hellinger')
write.table(asv_hel,"All_groups_asv-table-compare_hellinger.txt",sep="\t",col.names = NA)

pca <- rda(asv_hel, scale = FALSE)
#summary(pca, scaling = 1)

pca_eig <- pca$CA$eig
pca_exp <- pca$CA$eig / sum(pca$CA$eig)
site <- as.data.frame(scores(pca, choices = 1:3, scaling = 1, display = 'site'))

site$sampleid <- rownames(site)
site <- merge(site,metadata,intersect(names(site),names(metadata)))
site$Grouping_1 <- ordered(site$Grouping_1,levels = group_name)

write.table(site, 'All_groups_PCA.txt', col.names = NA, sep = '\t', quote = FALSE)

###CA

ca <- cca(asv_t)
ca_eig <- ca$CA$eig
ca_exp <- ca$CA$eig / sum(ca$CA$eig)
site <- as.data.frame(scores(ca, choices = 1:3, scaling = 1, display = 'site'))
site$sampleid <- rownames(site)
site <- merge(site,metadata,intersect(names(site),names(metadata)))
site$Grouping_1 <- ordered(site$Grouping_1,levels = group_name)
write.table(site, 'All_groups_CA.txt', col.names = NA, sep = '\t', quote = FALSE)


###PCOA
###以Bray-Curtis为例
distance = "Bray-Curtis"
dist <- vegdist(asv_t, method = m)

pcoa <- cmdscale(dist, k = (nrow(asv_t) - 1), eig = TRUE, add = TRUE)
  pcoa_eig <- pcoa$eig
  pcoa_exp <- pcoa$eig/sum(pcoa$eig)
  site <- as.data.frame(scores(pcoa, choices = 1:3, display = 'site'))
  site$sampleid <- rownames(site)
  site <- merge(site,metadata,intersect(names(site),names(metadata)))
  write.table(site, paste(m,"/",distance,"_All_groups_PCoA.txt", sep="" ), col.names = NA, sep = '\t', quote = FALSE)

###NMDS
###以Bray-Curtis为例
distance = "Bray-Curtis"
dist <- vegdist(asv_t, method = m)

nmds <- metaMDS(dist, k = 3)
  site <- as.data.frame(nmds$points)
  site$sampleid <- rownames(site)
  site <- merge(site,metadata,intersect(names(site),names(metadata)))
  write.table(site, paste(m,"/",distance,"_All_groups_NMDS.txt", sep="" ), col.names = NA, sep = '\t', quote = FALSE)
  


##画图
colpalettes <- unique( pal_d3("category20")(20))
##点最外围连线
type = "polygon"
group1_border <- ddply(site, 'Grouping_1', function(df) df[chull(df[[2]], df[[3]]), ])
p1 <- ggplot(data = site, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Grouping_1),size = 2) +
    scale_color_manual(values = colpalettes) + 
    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
    labs(x = paste("PCA1:",round(pca_exp["PC1"]*100,2),"%",sep = ""), y =  paste("PCA2:",round(pca_exp["PC2"]*100,2),"%",sep = "")) + 
    geom_polygon(data = group1_border, aes(fill = Grouping_1, color = Grouping_1), alpha = 0.1, show.legend = FALSE) +
    scale_fill_manual(values = colpalettes )
   ggsave("All_groups_PCA_PC1_PC2.pdf", p1)
   ggsave("All_groups_PCA_PC1_PC2.png", p1)
   

   
##置信区间画圆
type = "ellipse"
p1 <- ggplot(data = site, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Grouping_1),size = 2) +
    scale_color_manual(values = colpalettes) + 
    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
    labs(x = paste("PCA1:",round(pca_exp["PC1"]*100,2),"%",sep = ""), y =  paste("PCA2:",round(pca_exp["PC2"]*100,2),"%",sep = "")) + 
    stat_ellipse(aes(fill = Grouping_1), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) +
    scale_fill_manual(values = colpalettes)
  ggsave("All_groups_PCA_PC1_PC2.pdf", p1)
  ggsave("All_groups_PCA_PC1_PC2.png", p1)
  
 ##添加每个样本的标签
p4 <- p1 + geom_text_repel(aes(label = sampleid))
ggsave("All_groups_PCA_PC1_PC2_label.pdf", p4)
ggsave("All_groups_PCA_PC1_PC2_label.png", p4)
  
