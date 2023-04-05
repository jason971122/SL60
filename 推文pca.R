library(ggplot2)
library(ggrepel)
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
# 读取数据

asv_hel <- decostand(asv_t, method = 'hellinger')
# PCA
pca_1 <- prcomp(asv_hel, scale. = TRUE)

# 提取PC1和PC2的贡献度
pca_summary <- summary(pca_1)
pca_summary_df <- data.frame(
  "PC1" = pca_1$x[,1], 
  "PC2" = pca_1$x[,2],
  "PC3" = pca_1$x[,3],
  "Contribution" = pca_summary$sdev^2 / sum(pca_summary$sdev^2)
)

pca_summary_df$sampleid <- rownames(pca_summary_df)
pca_summary_df <- merge(pca_summary_df,metadata,intersect(names(pca_summary_df),names(metadata)))
pca_summary_df$Grouping_1 <- ordered(metadata$Grouping_1,levels = group_name)

##画图
colpalettes <- unique( pal_d3("category20")(20))
##点最外围连线
type = "polygen"
group1_border <- ddply(pca_summary_df, 'Grouping_1', function(df) df[chull(df[[2]], df[[3]]), ])
p1 <- ggplot(data = pca_summary_df, aes(x = PC1, y = PC3)) +
  geom_point(aes(color = Grouping_1),size = 2) +
  scale_color_manual(values = colpalettes) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  xlab(paste0("PC1 (", round(summary(pca_1)$importance[2, 1] * 100, 1), "%)")) +
  ylab(paste0("PC3 (", round(summary(pca_1)$importance[2, 3] * 100, 1), "%)")) +
  geom_polygon(data = group1_border, aes(fill = NULL, color = Grouping_1), 
               alpha = 0.1, show.legend = FALSE, linetype = "dashed") +
  scale_fill_manual(values = colpalettes )
p1



##置信区间画圆
type = "ellipse"
p2 <- ggplot(data = pca_summary_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Grouping_1),size = 2) +
  scale_color_manual(values = colpalettes) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  xlab(paste0("PC1 (", round(summary(pca_1)$importance[2, 1] * 100, 1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_1)$importance[2, 2] * 100, 1), "%)")) +
  stat_ellipse(aes(fill = Grouping_1), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = colpalettes)
p2


##添加每个样本的标签
p4 <- p2 + geom_text_repel(aes(label = sampleid))
p4
ggsave("All_groups_PCA_PC1_PC2_label.pdf", p4)
ggsave("All_groups_PCA_PC1_PC2_label.png", p4)


library(igraph)

# 计算距离矩阵
dist_matrix <- dist(pca_summary_df[,c("PC1", "PC2")])

# 构建图并计算最小生成树
graph <- graph_from_adjacency_matrix(as.matrix(dist_matrix), mode = "undirected")
mst <- minimum.spanning.tree(graph)

# 转换为data.frame格式
edges_df <- as_data_frame(mst, what = "edges")
colnames(edges_df) <- c("from", "to", "weight")

# 绘制散点图和最小生成树
p2 <- ggplot(data = pca_summary_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Grouping_1),size = 2) +
  scale_color_manual(values = colpalettes) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  xlab(paste0("PC1 (", round(summary(pca_1)$importance[2, 1] * 100, 1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_1)$importance[2, 2] * 100, 1), "%)")) +
  stat_ellipse(aes(fill = Grouping_1), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = colpalettes) +
  geom_edge(data = edges_df, aes(x = PC1, y = PC2, xend = PC1.1, yend = PC2.1, color = weight), 
            arrow = arrow(length = unit(0.02, "npc")), show.legend = FALSE) +
  scale_color_gradient(low = "blue", high = "red", limits = range(edges_df$weight))



center <- c(mean(asv_t["sampleid_1",]), mean(asv_t["sampleid_2",]))
center_df <- data.frame(Grouping_1 = "Center", PC1 = center[1], PC2 = center[2])

# 计算每个分组的中心点坐标
center_df <- pca_summary_df %>%
  group_by(Grouping_1) %>%
  summarize(center_x = mean(PC1), center_y = mean(PC2))

# 将中心点和分组信息合并到一个数据框中
pca_summary_df <- pca_summary_df %>%
  left_join(center_df, by = "Grouping_1")

# 绘制散点图和椭圆
p2 <- ggplot(data = pca_summary_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Grouping_1), size = 2) +
  scale_color_manual(values = colpalettes) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        legend.key = element_rect(fill = 'transparent')) +
  xlab(paste0("PC1 (", round(summary(pca_1)$importance[2, 1] * 100, 1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_1)$importance[2, 2] * 100, 1), "%)")) +
  stat_ellipse(aes(fill = Grouping_1), geom = 'polygon', level = 0.95, alpha = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = colpalettes)

# 添加放射连线
p2 <- p2 +
  geom_segment(aes(x = center_x, y = center_y, xend = PC1, yend = PC2, color = Grouping_1), 
               alpha = 0.5, size = 0.5, data = pca_summary_df)

# 展示图形
p2


library(ggplot2)

ggplot(pca_summary_df, aes(x = PC1, y = PC2)) +
  geom_point(size = 3) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face="bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "none"
  )


p1 <- ggplot(data = pca_summary_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Grouping_1),size = 3) +
  scale_color_manual(values = colpalettes) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  xlab(paste0("PC1 (", round(summary(pca_1)$importance[2, 1] * 100, 1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_1)$importance[2, 2] * 100, 1), "%)"))# +
  #geom_polygon(data = group1_border, aes(fill = NULL, color = Grouping_1), 
               #alpha = 0.1, show.legend = FALSE, linetype = "dashed") +
  #scale_fill_manual(values = colpalettes )
p1

