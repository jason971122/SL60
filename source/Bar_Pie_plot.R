library(ggplot2)
library(tidyverse)
library(reshape2)

# 加载OTU数据
otu_data <- read.table("asv-table-compare.tsv", header=TRUE, sep="\t", row.names=1)
# 创建一个数据框，列出每个样品的总OTU数量
otu_data <- otu_data[,-ncol(otu_data)]
otu_data <- t(otu_data)
otu_counts <- data.frame(count = rowSums(otu_data),
                        sample = rownames(otu_data))


# 绘制总OTU计数的条形图（黑白无序）------图1
p1 <- ggplot(otu_counts, aes(x=sample, y=count)) + 
  geom_bar(stat="identity") + 
  labs(title="Total OTU counts per sample FIG.1", x="Sample", y="Total OTU count") + 
  theme_bw()
#换个方向------图2
p2 <- ggplot(otu_counts, aes(x=count, y=sample)) + 
  geom_bar(stat="identity") + 
  labs(title="Total OTU counts per sample FIG.2", x="Sample", y="Total OTU count") + 
  theme_bw()

#排序
otu_data_sorted <- otu_counts[order(otu_counts$sample, decreasing = TRUE), ]

#升序排列------图3
p3 <- ggplot(otu_data_sorted, aes(x=reorder(sample,count), y=count)) + 
  geom_bar(stat="identity") + 
  labs(title="Total OTU counts per sample FIG.3", x="Sample", y="Total OTU count") + 
  theme_bw()

#降序排列------图4
p4 <- ggplot(otu_data_sorted, aes(x=reorder(sample,count,decreasing = TRUE), y=count)) + 
  geom_bar(stat="identity") + 
  labs(title="Total OTU counts per sample  FIG.4", x="Sample", y="Total OTU count") + 
  theme_bw()

# 生成颜色向量
colors <- rainbow(length(unique(otu_data_sorted$count)))

# 绘制图形并指定颜色,颜色太多, 且图例顺序不对------图5
p5 <- ggplot(otu_data_sorted, aes(x=reorder(sample,count), y=count, fill=sample)) + 
  geom_bar(stat="identity") + 
  labs(title="Total OTU counts per sample FIG.5", x="Sample", y="Total OTU count") + 
  scale_fill_manual(values = colors) + 
  theme_bw()

#图例顺序更正------图6
otu_data_sorted$sample <- factor(otu_data_sorted$sample, levels = c(paste0("A", 1:12), paste0("B", 1:12), paste0("C", 1:12)))

p6 <- ggplot(otu_data_sorted, aes(x=reorder(sample, count), y=count, fill=sample)) + 
  geom_bar(stat="identity") + 
  labs(title="Total OTU counts per sample  FIG.6", x="Sample", y="Total OTU count") + 
  scale_fill_manual(values = colors) + 
  theme_bw()

##减少颜色，按ABC区分颜色种类
#对数据进行重新整合
otu_data_sorted_ex <- otu_data_sorted %>%
  mutate(group = case_when(
    str_detect(sample, "^A") ~ "A",
    str_detect(sample, "^B") ~ "B",
    str_detect(sample, "^C") ~ "C"
  ))

otu_data_sorted_ex$sample <- factor(otu_data_sorted_ex$sample)
otu_data_sorted_ex <- otu_data_sorted_ex %>%
  mutate(sample = factor(sample, levels = c(paste0("A", 1:12), paste0("B", 1:12), paste0("C", 1:12))))

####划分组，同时所有样本都在。图7
p7 <- ggplot(otu_data_sorted_ex, aes(x = sample, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Count") +
  ylab("Sample") +
  scale_fill_manual(values = c("#619CFF", "#EF8532", "#27AE60"), 
                    name = "Group",
                    labels = c("A", "B", "C")) +
  facet_wrap(~group, ncol = 3, scales = "free_x") +
  ggtitle("OTU Data by Group and Count  FIG.7") +
  theme_bw()

#按照分组统计 OTU 数量
#注意此时otu_data的形式实际上已经是转置后的形式
otu_counts_ex <- data.frame(A = colSums(otu_data[grepl("^A", rownames(otu_data)), ]),
                            B = colSums(otu_data[grepl("^B", rownames(otu_data)), ]),
                            C = colSums(otu_data[grepl("^C", rownames(otu_data)),]))
# 计算每列的总和
otu_counts_sum <- colSums(otu_counts_ex)

# 创建数据框
df <- data.frame(Group = c("A", "B", "C"),
                 OTU_Count = otu_counts_sum)

# 绘制柱状图
p8 <- ggplot(data = df, aes(x = Group, y = OTU_Count, fill = Group)) +
  geom_bar(stat = "identity") +
  xlab("Group") + ylab("Total OTU Count") +
  ggtitle("Total OTU Count by Group  FIG.8") +
  theme_bw()

#堆叠柱状图
p9 <- ggplot(otu_data_sorted_ex, aes(x = group, y = count, fill = sample)) +
  geom_col()+
  xlab("Count") +
  ylab("Sample") +
  ggtitle("OTU Data by Group and Count  FIG.9") +
  theme_bw()


########################################################################
########下面是扇形图########
##########################################################################################

otu_counts_pie <- otu_data_sorted %>%
  mutate(percent = count / sum(count) * 100)

p10 <- ggplot(otu_counts_pie, aes(x = "", y = percent, fill = sample)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Pie chart of OTU count  FIG.10")


# 计算每个组的OTU百分比
df$percent <- round(df$OTU_Count/sum(df$OTU_Count)*100,1)

# 绘制带百分比标签的饼图
p11 <- ggplot(data = df, aes(x = "", y = OTU_Count, fill = Group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ggtitle("Total OTU Count by Group  FIG.11") +
  scale_fill_manual(values = c("gray", "lightblue", "pink")) +
  theme_void() +
  geom_text(aes(label = paste0(percent,"%")), position = position_stack(vjust = 0.5))



#保存图片
# 创建一个包含所有要保存的图形的列表
plots <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)

# 循环遍历列表并保存每个图形
for (i in 1:length(plots)) {
  ggsave(paste0("p", i, ".png"), plots[[i]], width = 40, height = 20, units = "cm")
}
