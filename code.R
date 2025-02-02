library(readxl)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(gghalves)
library(ggrepel)
library(wesanderson)
library(ggbeeswarm)
library(RColorBrewer)
library(MASS)
library(pROC)
library(pheatmap)
library(umap)
library(eulerr)
library(cowplot)

#Figure 1
##B
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 1, skip = 1))

pdf(file = "Figure1B.pdf", width = 4, height = 4)
ggplot(data, aes(x = type, y = Number, fill = type))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Blues")+
  geom_text(aes(label = 10^Number), vjust = 1.6, color = "black",
            position = position_dodge(0.9), size=3.5)+
  labs(x = "", y = "Number")+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))
dev.off()

##C
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 2, skip = 1))

my_comparisons=list(c("CA", "EN"), c("CA", "HD"), c("EN", "HD"))
pdf(file = "Figure1C.pdf", width = 4, height = 4)
ggplot(data, aes(x = sample_type, y = identified_protein))+
  stat_boxplot(geom = "errorbar", width = 0.4) + 
  geom_beeswarm(size = 1, aes(color = sample_type), cex = 3)+
  stat_summary(fun.y = "median", geom = "crossbar", 
               mapping = aes(ymin = ..y.., ymax = ..y..),
               width = 0.8, size = 0.3, color = "black")+
  scale_color_manual(values = c("#1b6292", "#c5934e", "#A73030"))+
  labs(x = "", y = "Number of proteins")+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))+
  stat_compare_means() +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")
dev.off()

##D
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 3, skip = 1))

pdf(file = "Figure1D.pdf", width = 4, height = 3)
ggplot(data = data) +
  geom_point(data = data, aes(x = cumu_sample, y = Var1), cex = 1)+
  geom_hline(yintercept = c(1516, 1482, 1240), color = "gray")+
  annotate('text', x = 76, y = 1516,label = "1516")+
  annotate('text', x = 113, y = 1482,label = "1482")+
  annotate('text', x = 150, y = 1240,label = "1240")+
  scale_y_continuous(limits = c(1200, 1800))+
  labs(x = "Number of sample with protein observation", y = "Number of proteins")+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        # axis.text.x = element_blank(),
        panel.spacing =unit(0, 'cm'))

dev.off()

##E
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 4, skip = 1))

pdf(file = "Figure1E.pdf", width = 4, height = 4)
ggplot(data = data) +
  geom_point(data = data, aes(x = 1:150, y = protein_number, color = sample_type), cex = 1)+
  geom_point(data = data, aes(x = 1:150, y = cumulative_number, color = sample_type), cex = 1)+
  scale_color_manual(values = c("#1b6292", "#c5934e", "#A73030"))+
  labs(x = "Sample", y = "Number of proteins")+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))
dev.off()

##F
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 5, skip = 1))

pdf(file = "Figure1F.pdf", width = 6, height = 3)
p1=ggplot(data = subset(data, sample_type == "HD")) +
  geom_point(aes(x = Rank, y = Log10Abundance, color = label_color), cex = 1)+
  geom_text_repel(aes(x = Rank, y = Log10Abundance, label = label), 
                  color = "#1b6292", size = 2, nudge_x = 500, nudge_y = -0.5)+
  scale_color_manual(values = c("grey", "#1b6292"))+
  scale_x_continuous(limits = c(-80, 2150), expand = c(0, 0))+
  labs(x = "Abundance rank", y = "Log10 Abundance")+
  theme_classic()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))

p2=ggplot(data = subset(data, sample_type == "EN")) +
  geom_point(aes(x = Rank, y = Log10Abundance, color = label_color), cex = 1)+
  geom_text_repel(aes(x = Rank, y = Log10Abundance, label = label), 
                  color = "#c5934e", size = 2, nudge_x = 500, nudge_y = -0.5)+
  scale_color_manual(values = c("grey", "#c5934e"))+
  scale_x_continuous(limits = c(-80, 2200), expand = c(0, 0))+
  labs(x = "Abundance rank", y = "Log10 Abundance")+
  theme_classic()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))

p3=ggplot(data = subset(data, sample_type == "CA")) +
  geom_point(aes(x = Rank, y = Log10Abundance, color = label_color), cex = 1)+
  geom_text_repel(aes(x = Rank, y = Log10Abundance, label = label), 
                  color = "#A73030", size = 2, nudge_x = 500, nudge_y = -0.5)+
  scale_color_manual(values = c("grey", "#A73030"))+
  scale_x_continuous(limits = c(-80, 2200), expand = c(0, 0))+
  labs(x = "Abundance rank", y = "Log10 Abundance")+
  theme_classic()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))

grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

##G
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 6, skip = 1))

pdf(file = "Figure1G.pdf", width = 10, height = 3)
p1=ggplot(subset(data, type == "no"), aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  # scale_y_continuous(limits = c(0,5.3),expand = c(0,0))+
  geom_text(aes(label = Freq), vjust = 1.6, color = "black",
            position = position_dodge(0.9), size=3.5)+
  labs(x = "", y = "Number")+
  theme_classic()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))

p2=ggplot(subset(data, type == "location"), aes(x = "", y = Freq, fill = Var1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  scale_fill_manual(values = c(brewer.pal(n = 7, name = 'Set2'), wes_palette("Royal2"))) + 
  blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq/5 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = scales::percent(Freq/sum(Secretome.location$Freq)), size=5))

p3=ggplot(subset(data, type == "function"), aes(x = "", y = Freq, fill = Var1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  scale_fill_brewer(palette="Set3") + 
  blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq/5 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = scales::percent(Freq/sum(Secretome.function$Freq)), size=5))

grid.arrange(p1, p2, p3, nrow = 1, widths = c(1, 3, 2.4))
dev.off()

##H
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 7, skip = 1))

pdf(file = "Figure1H.pdf", width = 4, height = 3)
ggplot(data, aes(x = "", y = Freq, fill = Var1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  scale_fill_brewer(palette="Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq/3 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = scales::percent(Freq/sum(Freq)), size=5))

dev.off()

##I
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 8, skip = 1))

pdf(file = "Figure1I.pdf", width = 4, height = 3)
ggplot(data, aes(x = PCR, fill = cluster)) + 
  geom_histogram(aes(y = ..density..), alpha = 1, bins = 1000,
                 position = "identity") +
  scale_fill_manual(values = "#6497b1") +   #21908C
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        axis.text=element_text(size = 12), 
        axis.title = element_text(size=12), 
        legend.title = element_blank(), 
        legend.text = element_blank())
dev.off()

#Figure 2
##B
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 9, skip = 1))

pdf(file = "Figure2B.pdf", width = 9, height = 7)
p1=pheatmap(data[c("age", "BMI"), module_order],
            show_colnames = T, 
            show_rownames = T,
            border=FALSE, 
            fontsize = 12, 
            cellheight = 25,
            cellwidth = 25,
            annotation_colors = ann_colors, 
            cluster_cols = F,
            cluster_rows = F,
            color = colorRampPalette(c("navy", "white", "firebrick3"))(100))

p2=pheatmap(-log(data[1:10, module_order], 10),
            show_colnames = T, 
            show_rownames = T,
            border=FALSE, 
            fontsize = 12, 
            cellheight = 25,
            cellwidth = 25,
            annotation_colors = ann_colors, 
            cluster_cols = F,
            cluster_rows = F,
            color = colorRampPalette(c("white", "firebrick3"))(100))

plot_grid(p1$gtable, p2$gtable, ncol = 1, rel_heights = c(1, 1))

dev.off()

##C
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 10, skip = 1))

pdf(file = "Figure2C.pdf", width = 7, height = 3)
ggplot(data = data, aes(x = UMAP1, y = UMAP2, color = module))+
  geom_point(cex = 1.5)+
  coord_fixed() + 
  scale_color_manual(values = color.module)+
  scale_fill_manual(values = color.module)+
  labs(x = "UMAP1", y = "UMAP2")+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))
dev.off()

##D
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 11, skip = 1))

pdf(file = "Figure2D.pdf", width = 7, height = 3)
ggplot(data = umap_data, aes(x = UMAP1, y = UMAP2, color = module))+
  geom_point(cex = 1.5)+
  coord_fixed() + 
  scale_color_manual(values = color.module)+
  scale_fill_manual(values = color.module)+
  labs(x = "UMAP1", y = "UMAP2")+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))
dev.off()

##E
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 12, skip = 1))

pdf(file = "Figure2E.pdf", width = 7, height = 3)
ggbarplot(data, x = "Module", y = "Score",
          color = "Type", fill = "Type",
          palette = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"), 
          width = 0.5, size = 0,
          add = c("mean_se"), add.params = list(width = 0.5),
          order = unique(data$Module),
          position = position_dodge(0.6),
          xlab = "", ylab = "Module Score")+ 
  theme_classic()+ 
  geom_hline(yintercept = 0, color = "black")+ 
  stat_compare_means(aes(group = Type, label = ..p.format..), size = 3, label.y = 0.065)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))
dev.off()

#Figure 3
##A
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 13, skip = 1))

pdf(file = "Figure3A.pdf", width = 4, height = 4)
ggplot(data = data, aes(x = UMAP1, y = UMAP2, color = sample_type))+
  geom_point(cex = 1)+
  coord_fixed() + 
  scale_color_manual(values = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))+
  scale_fill_manual(values = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))+
  labs(x = "UMAP1", y = "UMAP2")+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))
dev.off()

##B
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 14, skip = 1))

up.KNN=list(CA_HD = rownames(data)[data$CA_HD_sig == "up"], 
            EN_HD = rownames(data)[data$EN_HD_sig == "up"])

p1=plot(euler(up.KNN, shape = "circle"),
        quantities = list(type = c("percent", "counts"), cex = 1), 
        labels = list(cex = 1),
        edges = list(col = "black", lex = 1),
        fills = list(fill = c("#d13728", "#e6cc84"), alpha = 0.7) 
)

down.KNN=list(CA_HD = rownames(data)[data$CA_HD_sig == "down"],
              EN_HD = rownames(data)[data$EN_HD_sig == "down"])

p2=plot(euler(down.KNN, shape = "circle"), 
        quantities = list(type = c("percent", "counts"), cex = 1), 
        labels = list(cex = 1),
        edges = list(col = "black", lex = 1), 
        fills = list(fill = c("#0b5394", "#d9ead3"), alpha = 0.7)
)

p3=ggplot(data, aes(x = CA_HD_logFC, y = EN_HD_logFC, color = sig, size = size)) + 
  geom_point() + 
  scale_size_identity() +
  geom_text_repel(aes(x = CA_HD_logFC, y = EN_HD_logFC, label = label, color = sig), size = 2) + #, nudge_x = 1, nudge_y = -0.5
  labs(x = "Log2FC (CA vs. HD)", y = "Log2FC (EN vs. HD)") + 
  scale_color_manual(values = c(down = "#0072B5", no = "grey", up = "#BC3C28")) + 
  geom_hline(yintercept = c(log2(2/3), log2(1.5)), linetype = 4) + 
  geom_vline(xintercept = c(log2(2/3), log2(1.5)),linetype = 4) +
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))

pdf(file = "Figure3B.pdf", width = 8, height = 4)
pc1=plot_grid(p1, p2, ncol = 1)
plot_grid(pc1, p3, nrow = 1)
dev.off()

##C
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 15, skip = 1))

pdf(file = "Figure3C.pdf", width = 7, height = 3)
annotation_col=data.frame(sample_type = data$sample_type, row.names = rownames(data))
ann_colors=list(sample_type = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))

bk=unique(c(seq(-1.5, 1.5, length = 100)))
p=pheatmap(t(data[, -ncol(data)]),
           show_colnames = F, 
           show_rownames = T,
           annotation_col = annotation_col, 
           border = FALSE,
           scale = 'row',
           breaks = bk,
           color = colorRampPalette(c("navy", "white", "firebrick3"))(100), 
           fontsize = 8, 
           annotation_colors = ann_colors,
           cluster_rows = T,
           cluster_cols = F,
           clustering_distance_rows = "euclidean", 
           clustering_distance_cols = "euclidean",
           clustering_method = "ward.D")
dev.off()

##D
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 16, skip = 1))

pdf(file = "Figure3D.pdf", width = 7, height = 3)
ggplot(data, aes(Description, -log10(FDR), fill = ONTOLOGY)) + 
  geom_col() +
  scale_fill_manual(values = c("#a389e6", "#ffb263", "#9bd283", "#72b2ed"))+
  theme_classic() +
  coord_flip()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        # axis.text.x = element_blank(),
        panel.spacing =unit(0, 'cm'))
dev.off()

##E
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 17, skip = 1))

up.KNN=list(CA_HD = rownames(data)[data$CA_HD_sig == "up"], 
            CA_EN = rownames(data)[data$CA_EN_sig == "up"])

p1=plot(euler(up.KNN, shape = "circle"),
        quantities = list(type = c("percent", "counts"), cex = 1), 
        labels = list(cex = 1),
        edges = list(col = "black", lex = 1),
        fills = list(fill = c("#d13728", "#e6cc84"), alpha = 0.7) 
)

down.KNN=list(CA_HD = rownames(data)[data$CA_HD_sig == "down"],
              CA_EN = rownames(data)[data$CA_EN_sig == "down"])

p2=plot(euler(down.KNN, shape = "circle"), 
        quantities = list(type = c("percent", "counts"), cex = 1), 
        labels = list(cex = 1),
        edges = list(col = "black", lex = 1), 
        fills = list(fill = c("#0b5394", "#d9ead3"), alpha = 0.7)
)

p3=ggplot(data, aes(x = CA_HD_logFC, y = CA_EN_logFC, color = sig, size = size)) + 
  geom_point() + 
  scale_size_identity() +
  geom_text_repel(aes(x = CA_HD_logFC, y = CA_EN_logFC, label = label, color = sig), size = 2) + #, nudge_x = 1, nudge_y = -0.5
  labs(x = "Log2FC (CA vs. HD)", y = "Log2FC (CA vs. EN)") + 
  scale_color_manual(values = c(down = "#0072B5", no = "grey", up = "#BC3C28")) + 
  geom_hline(yintercept = c(log2(2/3), log2(1.5)), linetype = 4) + 
  geom_vline(xintercept = c(log2(2/3), log2(1.5)),linetype = 4) +
  theme_classic()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing =unit(0, 'cm'))

pdf(file = "Figure3E.pdf", width = 8, height = 4)
pc1=plot_grid(p1, p2, ncol = 1)
plot_grid(pc1, p3, nrow = 1)
dev.off()

##F
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 18, skip = 1))

pdf(file = "Figure3F.pdf", width = 7, height = 3)
annotation_col=data.frame(sample_type = data$sample_type, row.names = rownames(data))
ann_colors=list(sample_type = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))

bk=unique(c(seq(-1.5, 1.5, length = 100)))
pheatmap(t(data[, -ncol(data)]),
         show_colnames = F, 
         show_rownames = T,
         annotation_col = annotation_col, 
         border = FALSE,
         scale = 'row',
         breaks = bk,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100), 
         fontsize = 8, 
         annotation_colors = ann_colors,
         cluster_rows = T,
         cluster_cols = F,
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D")
dev.off()

##G
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 19, skip = 1))

pdf(file = "Figure3G.pdf", width = 5, height = 4)
ggplot(data, aes(Description, -log10(FDR), fill = ONTOLOGY)) + 
  geom_col() +
  scale_fill_manual(values = c("#a389e6", "#ffb263", "#9bd283", "#72b2ed"))+
  theme_classic() +
  coord_flip()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        # axis.text.x = element_blank(),
        panel.spacing =unit(0, 'cm'))
dev.off()

##H
marker=c("FN1", "VWF", "FBXO34", "ITGA2B")
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 20, skip = 1))

my_comparisons=list(c("CA", "EN"), c("CA", "HD"), c("EN", "HD"))
plot_data_column_half = function(data, column) {
  ggplot(data, aes(x = type, y = get(column), fill = type))+
    geom_half_violin(position = position_nudge(x = 0.25), side = "r", width = 0.8, color = 'black')+
    geom_jitter(aes(fill = type), shape = 21, size = 1, width = 0.15)+
    # coord_flip()+
    scale_fill_manual(values = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))+
    theme_classic()+
    labs(x = "", y = column)+
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          panel.spacing =unit(0, 'cm'))+
    stat_compare_means() +
    stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")
}
myplots_half=lapply(marker, plot_data_column_half, data = data)

pdf(file = "Figure3H.pdf", width = 9, height = 3)
theme_set(theme_bw())
grid.arrange(myplots_half[[1]], myplots_half[[2]], myplots_half[[3]], 
             myplots_half[[4]], ncol = 4)
dev.off()

#Figure 4
##A
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 21, skip = 1))

CAHD_roc=subset(data, type != "EN")
CAHD_roc$type=factor(CAHD_roc$type, levels = c("HD", "CA"))
ENHD_roc=subset(data, type != "CA")
ENHD_roc$type=factor(ENHD_roc$type, levels = c("HD", "EN"))

pdf(file="Figure4A.pdf", width=8, height=8)
r1=plot.roc(type2 ~ scoreCAEN, data = data, col = "#377EB8", legacy.axes = T)
r2=lines.roc(type ~ scoreCA2HD, data = CAHD_roc, col = "#c1382d")
r3=lines.roc(type ~ scoreEN2HD, data = ENHD_roc, col = "#984EA3")
legend('bottom', c(paste("ESCC&EIN vs. HC", "AUC =", round(as.numeric(r1$auc),3), "(", round(ci.auc(r1)[1], 3), "-", round(ci.auc(r1)[3], 3), ")"),
                   paste("ESCC vs. HC", "AUC =", round(as.numeric(r2$auc), 3), "(", round(ci.auc(r2)[1], 3), "-", round(ci.auc(r2)[3], 3), ")"),
                   paste("EIN vs. HC", "AUC =", round(as.numeric(r3$auc),3), "(", round(ci.auc(r3)[1], 3), "-", round(ci.auc(r3)[3], 3), ")")),
       lty='solid', text.font = 1, 
       col=c("#377EB8", "#c1382d", "#984EA3"))
dev.off()

##C 
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 22, skip = 1))

annotation_col=data.frame(Real = data$type)
rownames(annotation_col)=rownames(data)
ann_colors=list(Real = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))
bk=unique(c(seq(-1, 1, length=100)))

pdf(file = "Figure4C.pdf", width=8, height=3)
p=pheatmap(t(data[, marker]),
           breaks = bk, 
           scale = 'row', 
           show_colnames = F, 
           show_rownames = T,
           annotation_col = annotation_col, 
           border=FALSE, 
           fontsize = 12, 
           cellheight = 15,
           cellwidth = 5,
           annotation_colors = ann_colors, 
           cluster_cols = F,
           cluster_rows = F,
           color = colorRampPalette(c("navy", "white", "firebrick3"))(100))
dev.off()

##D 
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 23, skip = 1))

CAHD_roc=subset(data, type != "EN")
CAHD_roc$type=factor(CAHD_roc$type, levels = c("HD", "CA"))
ENHD_roc=subset(data, type != "CA")
ENHD_roc$type=factor(ENHD_roc$type, levels = c("HD", "EN"))

pdf(file="Figure4D.pdf", width=8, height=8)
r1=plot.roc(type2 ~ scoreCAEN, data = data, col = "#377EB8", legacy.axes = T)
r2=lines.roc(type ~ scoreCA2HD, data = CAHD_roc, col = "#c1382d")
r3=lines.roc(type ~ scoreEN2HD, data = ENHD_roc, col = "#984EA3")
legend('bottom', c(paste("ESCC&EIN vs. HC", "AUC =", round(as.numeric(r1$auc),3), "(", round(ci.auc(r1)[1], 3), "-", round(ci.auc(r1)[3], 3), ")"),
                   paste("ESCC vs. HC", "AUC =", round(as.numeric(r2$auc), 3), "(", round(ci.auc(r2)[1], 3), "-", round(ci.auc(r2)[3], 3), ")"),
                   paste("EIN vs. HC", "AUC =", round(as.numeric(r3$auc),3), "(", round(ci.auc(r3)[1], 3), "-", round(ci.auc(r3)[3], 3), ")")),
       lty='solid', text.font = 1, 
       col=c("#377EB8", "#c1382d", "#984EA3"))
dev.off()

##F 
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 24, skip = 1))

annotation_col=data.frame(Real = data$type)
rownames(annotation_col)=rownames(data)
ann_colors=list(Real = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))
bk=unique(c(seq(-1, 1, length=100)))

pdf(file = "Figure4F.pdf", width=8, height=3)
p=pheatmap(t(data[, marker]),
           breaks = bk, 
           scale = 'row', 
           show_colnames = F, 
           show_rownames = T,
           annotation_col = annotation_col, 
           border=FALSE, 
           fontsize = 12, 
           cellheight = 15,
           cellwidth = 5,
           annotation_colors = ann_colors, 
           cluster_cols = F,
           cluster_rows = F,
           color = colorRampPalette(c("navy", "white", "firebrick3"))(100))
dev.off()

##G 
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 25, skip = 1))

myplots_half=lapply(marker, plot_data_column_half, data = data)
pdf(file = "Figure4G.pdf", width = 9, height = 3)
theme_set(theme_bw())
grid.arrange(myplots_half[[1]], myplots_half[[2]], myplots_half[[3]], 
             myplots_half[[4]], ncol = 4)
dev.off()

##H 
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 26, skip = 1))

CAHD_roc=subset(data, type != "EN")
CAHD_roc$type=factor(CAHD_roc$type, levels = c("HD", "CA"))
ENHD_roc=subset(data, type != "CA")
ENHD_roc$type=factor(ENHD_roc$type, levels = c("HD", "EN"))

pdf(file="Figure4H.pdf", width=8, height=8)
r1=plot.roc(type2 ~ scoreCAEN, data = data, col = "#377EB8", legacy.axes = T)
r2=lines.roc(type ~ scoreCA2HD, data = CAHD_roc, col = "#c1382d")
r3=lines.roc(type ~ scoreEN2HD, data = ENHD_roc, col = "#984EA3")
legend('bottom', c(paste("ESCC&EIN vs. HC", "AUC =", round(as.numeric(r1$auc),3), "(", round(ci.auc(r1)[1], 3), "-", round(ci.auc(r1)[3], 3), ")"),
                   paste("ESCC vs. HC", "AUC =", round(as.numeric(r2$auc), 3), "(", round(ci.auc(r2)[1], 3), "-", round(ci.auc(r2)[3], 3), ")"),
                   paste("EIN vs. HC", "AUC =", round(as.numeric(r3$auc),3), "(", round(ci.auc(r3)[1], 3), "-", round(ci.auc(r3)[3], 3), ")")),
       lty='solid', text.font = 1, 
       col=c("#377EB8", "#c1382d", "#984EA3"))
dev.off()

##J 
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 27, skip = 1))

annotation_col=data.frame(Real = data$type)
rownames(annotation_col)=rownames(data)
ann_colors=list(Real = c(HD = "#1b6292", EN = "#c5934e", CA = "#A73030"))
bk=unique(c(seq(-1, 1, length=100)))

pdf(file = "Figure4J.pdf", width=8, height=3)
p=pheatmap(t(data[, marker]),
           breaks = bk, 
           scale = 'row', 
           show_colnames = F, 
           show_rownames = T,
           annotation_col = annotation_col, 
           border=FALSE, 
           fontsize = 12, 
           cellheight = 15,
           cellwidth = 5,
           annotation_colors = ann_colors, 
           cluster_cols = F,
           cluster_rows = F,
           color = colorRampPalette(c("navy", "white", "firebrick3"))(100))
dev.off()

##K 
data=data.frame(read_excel(path ="./source_data.xlsx", sheet = 28, skip = 1))

myplots_half=lapply(marker, plot_data_column_half, data = data)
pdf(file = "Figure4K.pdf", width = 9, height = 3)
theme_set(theme_bw())
grid.arrange(myplots_half[[1]], myplots_half[[2]], myplots_half[[3]], 
             myplots_half[[4]], ncol = 4)
dev.off()
