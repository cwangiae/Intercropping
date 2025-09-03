####fig. 1 A-B####
rm(list = ls())
library(ggplot2)
library(dplyr)
library(ggpattern)
library(ggpubr)
library(cowplot)

data <- data.frame(
  Sample = rep(c("Root", "Stem", "Leaf", "Total"), each = 6),
  Treatment = rep(rep(c("MM", "IM"), each = 3), 4),
  kg_ha = c(2600, 2500, 2900, 3200, 3650, 3850,
            6250, 6300, 5950, 6600, 6000, 6100,
            2200, 2500, 2100, 3800, 3000, 3400,
            11050, 11300, 10950, 13600, 12650, 13350)
)

data$Sample <- factor(data$Sample, levels = c("Leaf", "Stem", "Root", "Total"))

data$Treatment <- factor(data$Treatment, levels = c("MM", "IM"))

colors <- c("Root" = "#8B6508", "Stem" = "#006400", "Leaf" = "#66CD00", "Total" = "#EEEE00")

sig_labels <- data %>%
  group_by(Sample) %>%
  summarise(p = t.test(kg_ha ~ Treatment)$p.value) %>%
  mutate(label = case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  ),
  y.position = data %>%
    group_by(Sample) %>%
    summarise(max_val = max(kg_ha)) %>%
    pull(max_val) + 800
  )

main_plot <- ggplot(data, aes(x = Sample, y = kg_ha,
                              fill = Sample, pattern = Treatment)) +
  geom_bar_pattern(stat = "summary", fun = "mean",
                   position = position_dodge(0.7),
                   width = 0.6,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.02,
                   pattern_key_scale_factor = 0.6,
                   color = "black",
                   linewidth = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.7), width = 0.2, color = "black") +
  geom_text(data = sig_labels,
            aes(x = Sample, y = y.position, label = label),
            inherit.aes = FALSE,
            size = 5) +
  scale_fill_manual(values = colors) +
  scale_pattern_manual(values = c("MM" = "none", "IM" = "stripe")) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 15000, 2500), limits = c(0, 15000)) +
  labs(x = NULL, y = "Biomass (kg/ha)") +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(size = 14),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.title = element_text(size = 16),    
    legend.text = element_text(size = 14),    
    legend.key.size = unit(1.2, "cm"),       
    plot.margin = margin(t = 0, r = 20, b = 5, l = 10)
  )

title_bar <- ggplot() +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 1, fill = "grey80") +
  annotate("text", x = 2.5, y = 0.5, label = "Biomass",
           size = 6, fontface = "bold", color = "black", hjust = 0.5) +
  xlim(0.5, 4.5) +
  theme_void() +
  theme(plot.margin = margin(b = 0))

final_plot <- plot_grid(title_bar, main_plot, ncol = 1, rel_heights = c(0.07, 1))

print(final_plot)

data <- data.frame(
  Sample = rep(c("Root", "Stem", "Leaf", "Total"), each = 6),
  Treatment = rep(rep(c("MM", "IM"), each = 3), 4),
  N_concentration = c(
    75.92, 79.5, 100.17, 143.52, 168.33, 194.74,
    106.25, 108.36, 84.15, 96.36, 91.2, 80.58,
    122.76, 129, 108.81, 243.8, 193.8, 194.3,
    304.93, 316.86, 293.13, 503.68, 453.33, 469.62
  )
)

data$Sample <- factor(data$Sample, levels = c("Leaf", "Stem", "Root", "Total"))
data$Treatment <- factor(data$Treatment, levels = c("MM", "IM"))

colors <- c("Root" = "#8B6508", "Stem" = "#006400", "Leaf" = "#66CD00", "Total" = "#EEEE00")

sig_labels <- data %>%
  group_by(Sample) %>%
  summarise(p = t.test(N_concentration ~ Treatment)$p.value,
            max_val = max(N_concentration)) %>%
  mutate(label = case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  ),
  y.position = max_val + max_val * 0.1
  )

main_plot1 <- ggplot(data, aes(x = Sample, y = N_concentration,
                               fill = Sample, pattern = Treatment)) +
  geom_bar_pattern(stat = "summary", fun = "mean",
                   position = position_dodge(0.7),
                   width = 0.6,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.02,
                   pattern_key_scale_factor = 0.6,
                   color = "black",
                   linewidth = 0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.7), width = 0.2, color = "black") +
  geom_text(data = sig_labels,
            aes(x = Sample, y = y.position, label = label),
            inherit.aes = FALSE,
            size = 5) +
  scale_fill_manual(values = colors) +
  scale_pattern_manual(values = c("MM" = "none", "IM" = "stripe")) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 600, 150), limits = c(0, 600)) +
  labs(x = NULL, y = "N concentration (%)") +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(size = 14),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.title = element_text(size = 16),    
    legend.text = element_text(size = 14),     
    legend.key.size = unit(1.2, "cm"),        
    plot.margin = margin(t = 0, r = 20, b = 5, l = 10)
  )

title_bar1 <- ggplot() +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 0, ymax = 1, fill = "grey80") +
  annotate("text", x = 2.5, y = 0.5, label = "Organ N concentration",
           size = 6, fontface = "bold", color = "black", hjust = 0.5) +
  xlim(0.5, 4.5) +
  theme_void() +
  theme(plot.margin = margin(b = 0))

final_plot1 <- plot_grid(title_bar1, main_plot1, ncol = 1, rel_heights = c(0.07, 1))

print(final_plot1)
library(cowplot)

combined_plot <- plot_grid(final_plot, final_plot1, nrow = 1, rel_widths = c(1, 1))

print(combined_plot)

####fig. 2A-B bacterial_shannnon####

rm(list = ls())
library(tidyverse)
library(vegan)
library(patchwork)
library(showtext)
library(ggpubr)
library(ggpattern)
library(effectsize)  
showtext_auto()

format_pval <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(sprintf("p = %.3f", p))
  }
}

seqtab1 <- read.csv("data/bacterial_shannon_belowground_ASV.csv",
                    header = TRUE, row.names = 1, check.names = FALSE)
otu_rare1 <- as.data.frame(t(rrarefy(t(seqtab1), round(min(colSums(seqtab1)), -4))))
otutab_rare_t1 <- t(otu_rare1)

Shannon_1_1 <- vegan::diversity(otutab_rare_t1, index = 'shannon', base = exp(1))

alpha_result1 <- data.frame(
  Shannon_1 = Shannon_1_1,
  Sample = rownames(otutab_rare_t1)
)

alpha_result1$Group <- rep(c("Bulk_soil_MM", "Bulk_soil_IM", 
                             "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                             "Rhizoplane_MM", "Rhizoplane_IM",
                             "Root_en_MM", "Root_en_IM"), each = 3)

alpha_result1$Group2 <- rep(c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane", "Root_en"), each = 6)
alpha_result1$Treatment <- ifelse(grepl("IM", alpha_result1$Group), "IM", "MM")

alpha_result1$Group <- factor(alpha_result1$Group, levels = unique(alpha_result1$Group))
alpha_result1$Group2 <- factor(alpha_result1$Group2, levels = c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane", "Root_en"))

seqtab2 <- read.csv("data/bacterial_shannon_aboveground_ASV.csv",
                    header = TRUE, row.names = 1, check.names = FALSE)
otu_rare2 <- as.data.frame(t(rrarefy(t(seqtab2), round(min(colSums(seqtab2)), -4))))
otutab_rare_t2 <- t(otu_rare2)

Shannon_1_2 <- vegan::diversity(otutab_rare_t2, index = 'shannon', base = exp(1))

alpha_result2 <- data.frame(
  Shannon_1 = Shannon_1_2,
  Sample = rownames(otutab_rare_t2)
)

alpha_result2$Group <- rep(c("Stem_ep_MM", "Stem_ep_IM",
                             "Stem_en_MM", "Stem_en_IM",
                             "phylloplane_MM", "phylloplane_IM",
                             "Leaf_en_MM", "Leaf_en_IM"), each = 3)

alpha_result2$Group2 <- rep(c("Stem_ep", "Stem_en", "phylloplane", "Leaf_en"), each = 6)
alpha_result2$Treatment <- ifelse(grepl("IM", alpha_result2$Group), "IM", "MM")

alpha_result2$Group <- factor(alpha_result2$Group, levels = unique(alpha_result2$Group))
alpha_result2$Group2 <- factor(alpha_result2$Group2, levels = c("Stem_ep", "Stem_en", "phylloplane", "Leaf_en"))

fill_colors <- c("Bulk_soil" = "#1874CD", 
                 "Rhizosphere_soil" = "#BFEFFF", 
                 "Rhizoplane" = "#458B00", 
                 "Root_en" = "#BCEE68",
                 "Stem_ep" = "#1874CD", 
                 "Stem_en" = "#BFEFFF", 
                 "phylloplane" = "#458B00", 
                 "Leaf_en" = "#BCEE68")

alpha_theme <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15, colour = "black"),
    axis.text.x = element_text(size = 6, hjust = 1),
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_rect(color = "#808080", linetype = 1)
  )

plot_bar <- function(data, index_col, y_label, y_limits, group_order, comp_list) {

  aov_model <- aov(as.formula(paste(index_col, "~ Group2 * Treatment")), data = data)
  aov_summary <- summary(aov_model)

  eta_sq <- eta_squared(aov_model, partial = TRUE)

  stats_text <- sprintf(
    "ANOVA:\nniche: η²=%.2f; %s\ntreatment: η²=%.2f; %s\nniche×treatment: η²=%.2f; %s",
    eta_sq$Eta2_partial[1], format_pval(aov_summary[[1]]$`Pr(>F)`[1]),
    eta_sq$Eta2_partial[2], format_pval(aov_summary[[1]]$`Pr(>F)`[2]),
    eta_sq$Eta2_partial[3], format_pval(aov_summary[[1]]$`Pr(>F)`[3])
  )

  p <- ggplot(data, aes(x = Group, y = .data[[index_col]], fill = Group2, pattern = Treatment)) +
    scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0.05))) +
    geom_bar_pattern(
      stat = "summary", fun = "mean",
      color = "black", width = 0.6,
      pattern_fill = "black", pattern_angle = 45,
      pattern_density = 0.1, pattern_spacing = 0.02,
      pattern_key_scale_factor = 0.6
    ) +
    scale_fill_manual(values = fill_colors) +
    scale_pattern_manual(values = c("MM" = "none", "IM" = "stripe")) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
    labs(y = y_label, x = NULL) +
    alpha_theme +
    theme(legend.title = element_blank()) +
    annotate(
      "text", x = Inf, y = Inf,
      label = stats_text,
      hjust = 1.1, vjust = 1.1, size = 4,
      color = "black", fontface = "bold"
    )
  
  if (!missing(comp_list)) {
    p <- p + stat_compare_means(
      comparisons = comp_list,
      method = "t.test",
      label = "p.signif",
      hide.ns = TRUE,
      tip.length = 0.01
    )
  }
  
  return(p)
}


shannon_bar1 <- plot_bar(
  alpha_result1, "Shannon_1", "Shannon", c(0, 8), levels(alpha_result1$Group),
  list(
    c("Bulk_soil_MM", "Bulk_soil_IM"),
    c("Rhizosphere_soil_MM", "Rhizosphere_soil_IM"),
    c("Rhizoplane_MM", "Rhizoplane_IM"),
    c("Root_en_MM", "Root_en_IM")
  )
)

shannon_bar2 <- plot_bar(
  alpha_result2, "Shannon_1", "Shannon", c(0, 8), levels(alpha_result2$Group),
  list(
    c("Stem_ep_MM", "Stem_ep_IM"),
    c("Stem_en_MM", "Stem_en_IM"),
    c("phylloplane_MM", "phylloplane_IM"),
    c("Leaf_en_MM", "Leaf_en_IM")
  )
)

alpha_combined1 <- shannon_bar1 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')

alpha_combined2 <- shannon_bar2 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')

combined_all <- shannon_bar2 / shannon_bar1 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')
print(combined_all)

####fig. 2C-D fungal_shannon####
seqtab1 <- read.csv("data/fungal_shannon_belowground_ASV.csv",
                    header = TRUE, row.names = 1, check.names = FALSE)
otu_rare1 <- as.data.frame(t(rrarefy(t(seqtab1), round(min(colSums(seqtab1)), -4))))
otutab_rare_t1 <- t(otu_rare1)

Shannon_1_1 <- vegan::diversity(otutab_rare_t1, index = 'shannon', base = exp(1))

alpha_result1 <- data.frame(
  Shannon_1 = Shannon_1_1,
  Sample = rownames(otutab_rare_t1)
)

alpha_result1$Group <- rep(c("Bulk_soil_MM", "Bulk_soil_IM", 
                             "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                             "Rhizoplane_MM", "Rhizoplane_IM",
                             "Root_en_MM", "Root_en_IM"), each = 3)

alpha_result1$Group2 <- rep(c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane", "Root_en"), each = 6)
alpha_result1$Treatment <- ifelse(grepl("IM", alpha_result1$Group), "IM", "MM")

alpha_result1$Group <- factor(alpha_result1$Group, levels = unique(alpha_result1$Group))
alpha_result1$Group2 <- factor(alpha_result1$Group2, levels = c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane", "Root_en"))

seqtab2 <- read.csv("data/fungal_shannon_aboveground_ASV.csv",
                    header = TRUE, row.names = 1, check.names = FALSE)
otu_rare2 <- as.data.frame(t(rrarefy(t(seqtab2), round(min(colSums(seqtab2)), -4))))
otutab_rare_t2 <- t(otu_rare2)

Shannon_1_2 <- vegan::diversity(otutab_rare_t2, index = 'shannon', base = exp(1))

alpha_result2 <- data.frame(
  Shannon_1 = Shannon_1_2,
  Sample = rownames(otutab_rare_t2)
)

alpha_result2$Group <- rep(c("Stem_ep_MM", "Stem_ep_IM",
                             "Stem_en_MM", "Stem_en_IM",
                             "phylloplane_MM", "phylloplane_IM",
                             "Leaf_en_MM", "Leaf_en_IM"), each = 3)

alpha_result2$Group2 <- rep(c("Stem_ep", "Stem_en", "phylloplane", "Leaf_en"), each = 6)
alpha_result2$Treatment <- ifelse(grepl("IM", alpha_result2$Group), "IM", "MM")

alpha_result2$Group <- factor(alpha_result2$Group, levels = unique(alpha_result2$Group))
alpha_result2$Group2 <- factor(alpha_result2$Group2, levels = c("Stem_ep", "Stem_en", "phylloplane", "Leaf_en"))


shannon_bar1 <- plot_bar(
  alpha_result1, "Shannon_1", "Shannon", c(0, 8), levels(alpha_result1$Group),
  list(
    c("Bulk_soil_MM", "Bulk_soil_IM"),
    c("Rhizosphere_soil_MM", "Rhizosphere_soil_IM"),
    c("Rhizoplane_MM", "Rhizoplane_IM"),
    c("Root_en_MM", "Root_en_IM")
  )
)

shannon_bar2 <- plot_bar(
  alpha_result2, "Shannon_1", "Shannon", c(0, 8), levels(alpha_result2$Group),
  list(
    c("Stem_ep_MM", "Stem_ep_IM"),
    c("Stem_en_MM", "Stem_en_IM"),
    c("phylloplane_MM", "phylloplane_IM"),
    c("Leaf_en_MM", "Leaf_en_IM")
  )
)

alpha_combined1 <- shannon_bar1 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')

alpha_combined2 <- shannon_bar2 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')

combined_all <- shannon_bar2 / shannon_bar1 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')
print(combined_all)


####fig. 3A-C bacterial_pcoa1####

rm(list = ls())
library(patchwork)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(vegan)
library(ggforce)

seqtab <- read.csv("data/bacterial_pcoa_aboveground_ASV.csv", 
                   header = TRUE, row.names = 1, check.names = FALSE)
seqtab <- seqtab[rowSums(seqtab) != 0, ]

set.seed(1115)
otu_rare <- as.data.frame(t(rrarefy(t(seqtab), round(min(colSums(seqtab)), -4))))

group <- data.frame(
  sample = colnames(otu_rare),
  group = rep(c("Stem_ep_MM", "Stem_ep_IM",
                "Stem_en_MM", "Stem_en_IM",
                "phylloplane_MM", "phylloplane_IM",
                "Leaf_en_MM", "Leaf_en_IM"), each = 3)
)

group$Niche <- sub("_(MM|IM)$", "", group$group)
group$Treatment <- sub("^.*_(MM|IM)$", "\\1", group$group)

adonis_result <- adonis2(t(otu_rare) ~ Niche * Treatment + Niche + Treatment, data = group, permutations = 999, by = "terms", method = "bray")

Niche_stats <- paste0("Niche: R² = ", round(adonis_result$R2[1], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[1], digits = 2))
Treat_stats <- paste0("Treatment: R² = ", round(adonis_result$R2[2], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[2], digits = 2))
Interact_stats <- paste0("Niche × Treatment: R² = ", round(adonis_result$R2[3], 2), 
                         "; p = ", format.pval(adonis_result$`Pr(>F)`[3], digits = 2))

distance <- vegdist(t(otu_rare), method = 'bray')
pcoa <- cmdscale(distance, k = 2, eig = TRUE)

plot_data <- data.frame(
  PCoA1 = pcoa$points[, 1], 
  PCoA2 = pcoa$points[, 2],
  group = group$group,
  sample = group$sample,
  Niche = group$Niche,
  Treatment = group$Treatment
)

levels_Niche <- c("Stem_ep", "Stem_en", "phylloplane", "Leaf_en")  
levels_Treatment <- c("MM", "IM") 

plot_data$Niche <- factor(plot_data$Niche, levels = levels_Niche)
plot_data$Treatment <- factor(plot_data$Treatment, levels = levels_Treatment)

plot_data <- plot_data %>% drop_na()

fill_colors <- c("Stem_ep" = "#8B5A2B", "Stem_en" = "#EECFA1", "phylloplane" = "#DB7093", "Leaf_en" = "#FFC1C1")

p_pcoa1 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(aes(fill = Niche, shape = Treatment), size = 6, stroke = 1, color = "black", alpha = 0.8) +
  labs(
    x = paste0("PCoA1 (", round(100 * pcoa$eig[1] / sum(pcoa$eig), 2), "%)"),
    y = paste0("PCoA2 (", round(100 * pcoa$eig[2] / sum(pcoa$eig), 2), "%)"),
    fill = "Niche", 
    shape = "Treatment", 
    color = "Niche"
  ) +
  annotate("text", x = -0.35, y = 0.90, label = Niche_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.83, label = Treat_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.76, label = Interact_stats, hjust = 0, size = 5) +
  
  scale_fill_manual(values = fill_colors, name = "Niche") +
  scale_color_manual(values = fill_colors, name = "Niche") +
  scale_shape_manual(values = c("IM" = 24, "MM" = 22), name = "Treatment") +
  guides(
    fill = guide_legend(override.aes = list(size = 8, shape = 21, color = "black")), 
    shape = guide_legend(override.aes = list(size = 6))  
  ) +
  
  xlim(c(-0.35, 0.7)) +
  ylim(c(-0.2, 0.9)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_rect(color = "#808080", linetype = 1)
  )

print(p_pcoa1)

library(tidyverse)
library(ggplot2)
library(ggsci)
library(vegan)
library(ggforce)
library(patchwork) 

seqtab <- read.csv("data/bacterial_pcoa_belowground_ASV.csv", 
                   header = TRUE, row.names = 1, check.names = FALSE)
seqtab <- seqtab[rowSums(seqtab) != 0, ]

set.seed(1115)
otu_rare <- as.data.frame(t(rrarefy(t(seqtab), round(min(colSums(seqtab)), -4))))

group <- data.frame(
  sample = colnames(otu_rare),
  group = rep(c("Bulk_soil_MM", "Bulk_soil_IM",
                "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                "Rhizoplane_MM", "Rhizoplane_IM",
                "Root_en_MM", "Root_en_IM"), each = 3)
)

group$Niche <- sub("_(MM|IM)$", "", group$group)
group$Treatment <- sub("^.*_(MM|IM)$", "\\1", group$group)

group$group <- factor(group$group, 
                      levels = c("Bulk_soil_MM", "Bulk_soil_IM",
                                 "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                                 "Rhizoplane_MM", "Rhizoplane_IM",
                                 "Root_en_MM", "Root_en_IM"))

adonis_result <- adonis2(t(otu_rare) ~ Niche * Treatment + Niche + Treatment, data = group, permutations = 999, by = "terms", method = "bray")

Niche_stats <- paste0("Niche: R² = ", round(adonis_result$R2[1], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[1], digits = 2))
Treat_stats <- paste0("Treatment: R² = ", round(adonis_result$R2[2], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[2], digits = 2))
Interact_stats <- paste0("Niche × Treatment: R² = ", round(adonis_result$R2[3], 2), 
                         "; p = ", format.pval(adonis_result$`Pr(>F)`[3], digits = 2))

distance <- vegdist(t(otu_rare), method = 'bray')
pcoa <- cmdscale(distance, k = 2, eig = TRUE)

plot_data <- data.frame(
  PCoA1 = pcoa$points[, 1], 
  PCoA2 = pcoa$points[, 2],
  group = group$group,
  sample = group$sample,
  Niche = group$Niche,
  Treatment = group$Treatment
)
plot_data$Niche <- factor(plot_data$Niche, levels = c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane", "Root_en"))

p_pcoa2 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(
    aes(fill = Niche, shape = Treatment), 
    size = 6,
    stroke = 1, 
    color = "black", 
    alpha = 0.8
  ) +

  labs(
    x = paste0("PCoA1 (", round(100 * pcoa$eig[1] / sum(pcoa$eig), 2), "%)"),
    y = paste0("PCoA2 (", round(100 * pcoa$eig[2] / sum(pcoa$eig), 2), "%)"),
    fill = "Niche", 
    shape = "Treatment",
    color = "Niche" 
  ) +

  annotate("text", x = -0.35, y = 0.90, label = Niche_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.83, label = Treat_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.76, label = Interact_stats, hjust = 0, size = 5) +

  scale_fill_manual(
    values = c("Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"), # 自定义颜色
    name = "Niche"
  ) +
  scale_color_manual(
    values = c("Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"), # 确保边框颜色一致
    name = "Niche"
  ) +

  scale_shape_manual(
    values = c("IM" = 24, "MM" = 22),
    name = "Treatment"
  ) +

  guides(
    fill = guide_legend(override.aes = list(size = 8, shape = 21, color = "black")), 
    shape = guide_legend(override.aes = list(size = 6)) 
  ) +
  
  xlim(c(-0.35, 0.7)) +
  ylim(c(-0.2, 0.9)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +

    theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_rect(color = "#808080", linetype = 1)
  )

print(p_pcoa2)

seqtab <- read.csv("data/bacterial_pcoa_all_ASV.csv", 
                   header = TRUE, row.names = 1, check.names = FALSE)
seqtab <- seqtab[rowSums(seqtab) != 0, ]

set.seed(1115)

otu_rare <- as.data.frame(t(rrarefy(t(seqtab), round(min(colSums(seqtab)), -4))))

group <- data.frame(
  sample = colnames(otu_rare),
  group = rep(c("Bulk_soil_MM", "Bulk_soil_IM",
                "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                "Rhizoplane_MM", "Rhizoplane_IM",
                "Root_en_MM", "Root_en_IM",
                "Stem_ep_MM", "Stem_ep_IM",
                "Stem_en_MM", "Stem_en_IM",
                "phylloplane_MM", "phylloplane_IM",
                "Leaf_en_MM", "Leaf_en_IM"), each = 3)
)

group$Niche <- sub("_(MM|IM)$", "", group$group)

group$Treatment <- sub("^.*_(MM|IM)$", "\\1", group$group)

group$group <- factor(group$group, 
                      levels = c("Stem_ep_MM", "Stem_ep_IM", 
                                 "Stem_en_MM", "Stem_en_IM", 
                                 "phylloplane_MM", "phylloplane_IM", 
                                 "Leaf_en_MM", "Leaf_en_IM",
                                 "Bulk_soil_MM","Bulk_soil_IM",
                                 "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                                 "Rhizoplane_MM","Rhizoplane_IM",
                                 "Root_en_MM","Root_en_IM"))

adonis_result <- adonis2(t(otu_rare) ~ Niche * Treatment + Niche + Treatment, data = group, permutations = 999, by = "terms", method = "bray")

Niche_stats <- paste0("Niche: R² = ", round(adonis_result$R2[1], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[1], digits = 2))
Treat_stats <- paste0("Treatment: R² = ", round(adonis_result$R2[2], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[2], digits = 2))
Interact_stats <- paste0("Niche × Treatment: R² = ", round(adonis_result$R2[3], 2), 
                         "; p = ", format.pval(adonis_result$`Pr(>F)`[3], digits = 2))

distance <- vegdist(t(otu_rare), method = 'bray')
pcoa <- cmdscale(distance, k = 2, eig = TRUE)

plot_data <- data.frame(
  PCoA1 = pcoa$points[, 1], 
  PCoA2 = pcoa$points[, 2],
  group = group$group,
  sample = group$sample,
  Niche = group$Niche,
  Treatment = group$Treatment
)
plot_data$Niche <- factor(plot_data$Niche, levels = c("Stem_ep", 
                                                      "Stem_en", 
                                                      "phylloplane", 
                                                      "Leaf_en",
                                                      "Bulk_soil",
                                                      "Rhizosphere_soil",
                                                      "Rhizoplane",
                                                      "Root_en" ))

p_pcoa3 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(
    aes(fill = Niche, shape = Treatment), 
    size = 6,
    stroke = 1,  
    color = "black",  
    alpha = 0.8
  ) +
  
  labs(
    x = paste0("PCoA1 (", round(100 * pcoa$eig[1] / sum(pcoa$eig), 2), "%)"),
    y = paste0("PCoA2 (", round(100 * pcoa$eig[2] / sum(pcoa$eig), 2), "%)"),
    fill = "Niche", 
    shape = "Treatment", 
    color = "Niche" 
  ) +
  annotate("text", x = -0.35, y = 0.90, label = Niche_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.83, label = Treat_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.76, label = Interact_stats, hjust = 0, size = 5) +
 
  scale_fill_manual(
    values = c("Stem_ep" = "#8B5A2B", "Stem_en" = "#EECFA1", "phylloplane" = "#DB7093", "Leaf_en" = "#FFC1C1","Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"), # 自定义颜色
    name = "Niche"
  ) +
  scale_color_manual(
    values = c("Stem_ep" = "#8B5A2B", "Stem_en" = "#EECFA1", "phylloplane" = "#DB7093", "Leaf_en" = "#FFC1C1","Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"),, # 确保边框颜色一致
    name = "Niche"
  ) +
  
  scale_shape_manual(
    values = c("IM" = 24, "MM" = 22),  
    name = "Treatment"
  ) +
 
  guides(
    fill = guide_legend(override.aes = list(size = 8, shape = 21, color = "black")), 
    shape = guide_legend(override.aes = list(size = 6))  
  ) +
  

  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  

  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_rect(color = "#808080", linetype = 1)
  )

print(p_pcoa3)

final_plot <- ( p_pcoa3| p_pcoa1 | p_pcoa2) 

print(final_plot)

####fig.3D-E fungal_pcoa####

rm(list = ls())
library(patchwork)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(vegan)
library(ggforce)

seqtab <- read.csv("data/fungal_pcoa_aboveground_ASV.csv", 
                   header = TRUE, row.names = 1, check.names = FALSE)
seqtab <- seqtab[rowSums(seqtab) != 0, ]

set.seed(1115)
otu_rare <- as.data.frame(t(rrarefy(t(seqtab), round(min(colSums(seqtab)), -4))))

group <- data.frame(
  sample = colnames(otu_rare),
  group = rep(c("Stem_ep_MM", "Stem_ep_IM",
                "Stem_en_MM", "Stem_en_IM",
                "phylloplane_MM", "phylloplane_IM",
                "Leaf_en_MM", "Leaf_en_IM"), each = 3)
)

group$Niche <- sub("_(MM|IM)$", "", group$group)
group$Treatment <- sub("^.*_(MM|IM)$", "\\1", group$group)

adonis_result <- adonis2(t(otu_rare) ~ Niche * Treatment + Niche + Treatment, data = group, permutations = 999, by = "terms", method = "bray")

Niche_stats <- paste0("Niche: R² = ", round(adonis_result$R2[1], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[1], digits = 2))
Treat_stats <- paste0("Treatment: R² = ", round(adonis_result$R2[2], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[2], digits = 2))
Interact_stats <- paste0("Niche × Treatment: R² = ", round(adonis_result$R2[3], 2), 
                         "; p = ", format.pval(adonis_result$`Pr(>F)`[3], digits = 2))

distance <- vegdist(t(otu_rare), method = 'bray')
pcoa <- cmdscale(distance, k = 2, eig = TRUE)

plot_data <- data.frame(
  PCoA1 = pcoa$points[, 1], 
  PCoA2 = pcoa$points[, 2],
  group = group$group,
  sample = group$sample,
  Niche = group$Niche,
  Treatment = group$Treatment
)

levels_Niche <- c("Stem_ep", "Stem_en", "phylloplane", "Leaf_en")
levels_Treatment <- c("MM", "IM") 

plot_data$Niche <- factor(plot_data$Niche, levels = levels_Niche)
plot_data$Treatment <- factor(plot_data$Treatment, levels = levels_Treatment)

plot_data <- plot_data %>% drop_na()

fill_colors <- c("Stem_ep" = "#8B5A2B", "Stem_en" = "#EECFA1", "phylloplane" = "#DB7093", "Leaf_en" = "#FFC1C1")

p_pcoa1 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(aes(fill = Niche, shape = Treatment), size = 6, stroke = 1, color = "black", alpha = 0.8) +
  labs(
    x = paste0("PCoA1 (", round(100 * pcoa$eig[1] / sum(pcoa$eig), 2), "%)"),
    y = paste0("PCoA2 (", round(100 * pcoa$eig[2] / sum(pcoa$eig), 2), "%)"),
    fill = "Niche", 
    shape = "Treatment", 
    color = "Niche"
  ) +

  annotate("text", x = -0.35, y = 0.90, label = Niche_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.83, label = Treat_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.76, label = Interact_stats, hjust = 0, size = 5) +
  
  scale_fill_manual(values = fill_colors, name = "Niche") +
  scale_color_manual(values = fill_colors, name = "Niche") +
  scale_shape_manual(values = c("IM" = 24, "MM" = 22), name = "Treatment") +
  guides(
    fill = guide_legend(override.aes = list(size = 8, shape = 21, color = "black")), 
    shape = guide_legend(override.aes = list(size = 6))  
  ) +
  
  xlim(c(-0.35, 0.7)) +
  ylim(c(-0.2, 0.9)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +

  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_rect(color = "#808080", linetype = 1)
  )

print(p_pcoa1)

library(tidyverse)
library(ggplot2)
library(ggsci)
library(vegan)
library(ggforce)
library(patchwork) 


seqtab <- read.csv("data/fungal_pcoa_belowground_ASV.csv", 
                   header = TRUE, row.names = 1, check.names = FALSE)
seqtab <- seqtab[rowSums(seqtab) != 0, ]

set.seed(1115)
otu_rare <- as.data.frame(t(rrarefy(t(seqtab), round(min(colSums(seqtab)), -4))))

group <- data.frame(
  sample = colnames(otu_rare),
  group = rep(c("Bulk_soil_MM", "Bulk_soil_IM",
                "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                "Rhizoplane_MM", "Rhizoplane_IM",
                "Root_en_MM", "Root_en_IM"), each = 3)
)

group$Niche <- sub("_(MM|IM)$", "", group$group)
group$Treatment <- sub("^.*_(MM|IM)$", "\\1", group$group)

group$group <- factor(group$group, 
                      levels = c("Bulk_soil_MM", "Bulk_soil_IM",
                                 "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                                 "Rhizoplane_MM", "Rhizoplane_IM",
                                 "Root_en_MM", "Root_en_IM"))

adonis_result <- adonis2(t(otu_rare) ~ Niche * Treatment + Niche + Treatment, data = group, permutations = 999, by = "terms", method = "bray")

Niche_stats <- paste0("Niche: R² = ", round(adonis_result$R2[1], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[1], digits = 2))
Treat_stats <- paste0("Treatment: R² = ", round(adonis_result$R2[2], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[2], digits = 2))
Interact_stats <- paste0("Niche × Treatment: R² = ", round(adonis_result$R2[3], 2), 
                         "; p = ", format.pval(adonis_result$`Pr(>F)`[3], digits = 2))

distance <- vegdist(t(otu_rare), method = 'bray')
pcoa <- cmdscale(distance, k = 2, eig = TRUE)

plot_data <- data.frame(
  PCoA1 = pcoa$points[, 1], 
  PCoA2 = pcoa$points[, 2],
  group = group$group,
  sample = group$sample,
  Niche = group$Niche,
  Treatment = group$Treatment
)
plot_data$Niche <- factor(plot_data$Niche, levels = c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane", "Root_en"))

p_pcoa2 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(
    aes(fill = Niche, shape = Treatment), 
    size = 6,
    stroke = 1, 
    color = "black",  
    alpha = 0.8
  ) +
  
  labs(
    x = paste0("PCoA1 (", round(100 * pcoa$eig[1] / sum(pcoa$eig), 2), "%)"),
    y = paste0("PCoA2 (", round(100 * pcoa$eig[2] / sum(pcoa$eig), 2), "%)"),
    fill = "Niche", 
    shape = "Treatment", 
    color = "Niche" 
  ) +
  
  annotate("text", x = -0.35, y = 0.90, label = Niche_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.83, label = Treat_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.76, label = Interact_stats, hjust = 0, size = 5) +
  
  scale_fill_manual(
    values = c("Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"), # 自定义颜色
    name = "Niche"
  ) +
  scale_color_manual(
    values = c("Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"), # 确保边框颜色一致
    name = "Niche"
  ) +
 
  scale_shape_manual(
    values = c("IM" = 24, "MM" = 22), 
    name = "Treatment"
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(size = 8, shape = 21, color = "black")),
    shape = guide_legend(override.aes = list(size = 6))  
  ) +
  
  xlim(c(-0.35, 0.7)) +
  ylim(c(-0.2, 0.9)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
 
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_rect(color = "#808080", linetype = 1)
  )


print(p_pcoa2)

seqtab <- read.csv("data/fungal_pcoa_all_ASV.csv", 
                   header = TRUE, row.names = 1, check.names = FALSE)
seqtab <- seqtab[rowSums(seqtab) != 0, ]

set.seed(1115)
otu_rare <- as.data.frame(t(rrarefy(t(seqtab), round(min(colSums(seqtab)), -4))))

group <- data.frame(
  sample = colnames(otu_rare),
  group = rep(c("Bulk_soil_MM", "Bulk_soil_IM",
                "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                "Rhizoplane_MM", "Rhizoplane_IM",
                "Root_en_MM", "Root_en_IM",
                "Stem_ep_MM", "Stem_ep_IM",
                "Stem_en_MM", "Stem_en_IM",
                "phylloplane_MM", "phylloplane_IM",
                "Leaf_en_MM", "Leaf_en_IM"), each = 3)
)

group$Niche <- sub("_(MM|IM)$", "", group$group)

group$Treatment <- sub("^.*_(MM|IM)$", "\\1", group$group)

group$group <- factor(group$group, 
                      levels = c("Stem_ep_MM", "Stem_ep_IM", 
                                 "Stem_en_MM", "Stem_en_IM", 
                                 "phylloplane_MM", "phylloplane_IM", 
                                 "Leaf_en_MM", "Leaf_en_IM",
                                 "Bulk_soil_MM","Bulk_soil_IM",
                                 "Rhizosphere_soil_MM", "Rhizosphere_soil_IM",
                                 "Rhizoplane_MM","Rhizoplane_IM",
                                 "Root_en_MM","Root_en_IM"))

adonis_result <- adonis2(t(otu_rare) ~ Niche * Treatment + Niche + Treatment, data = group, permutations = 999, by = "terms", method = "bray")

Niche_stats <- paste0("Niche: R² = ", round(adonis_result$R2[1], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[1], digits = 2))
Treat_stats <- paste0("Treatment: R² = ", round(adonis_result$R2[2], 2), 
                      "; p = ", format.pval(adonis_result$`Pr(>F)`[2], digits = 2))
Interact_stats <- paste0("Niche × Treatment: R² = ", round(adonis_result$R2[3], 2), 
                         "; p = ", format.pval(adonis_result$`Pr(>F)`[3], digits = 2))

distance <- vegdist(t(otu_rare), method = 'bray')

pcoa <- cmdscale(distance, k = 2, eig = TRUE)

plot_data <- data.frame(
  PCoA1 = pcoa$points[, 1], 
  PCoA2 = pcoa$points[, 2],
  group = group$group,
  sample = group$sample,
  Niche = group$Niche,
  Treatment = group$Treatment
)
plot_data$Niche <- factor(plot_data$Niche, levels = c("Stem_ep", 
                                                      "Stem_en", 
                                                      "phylloplane", 
                                                      "Leaf_en",
                                                      "Bulk_soil",
                                                      "Rhizosphere_soil",
                                                      "Rhizoplane",
                                                      "Root_en" ))

p_pcoa3 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2)) +
  geom_point(
    aes(fill = Niche, shape = Treatment), 
    size = 6,
    stroke = 1, 
    color = "black",  
    alpha = 0.8
  ) +
  

  labs(
    x = paste0("PCoA1 (", round(100 * pcoa$eig[1] / sum(pcoa$eig), 2), "%)"),
    y = paste0("PCoA2 (", round(100 * pcoa$eig[2] / sum(pcoa$eig), 2), "%)"),
    fill = "Niche", 
    shape = "Treatment", 
    color = "Niche" 
  ) +
 
  annotate("text", x = -0.35, y = 0.90, label = Niche_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.83, label = Treat_stats, hjust = 0, size = 5) +
  annotate("text", x = -0.35, y = 0.76, label = Interact_stats, hjust = 0, size = 5) +
 
  scale_fill_manual(
    values = c("Stem_ep" = "#8B5A2B", "Stem_en" = "#EECFA1", "phylloplane" = "#DB7093", "Leaf_en" = "#FFC1C1","Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"), # 自定义颜色
    name = "Niche"
  ) +
  scale_color_manual(
    values = c("Stem_ep" = "#8B5A2B", "Stem_en" = "#EECFA1", "phylloplane" = "#DB7093", "Leaf_en" = "#FFC1C1","Bulk_soil" = "#1874CD", "Rhizosphere_soil" = "#BFEFFF", "Rhizoplane" = "#458B00", "Root_en" = "#BCEE68"),, # 确保边框颜色一致
    name = "Niche"
  ) +

  scale_shape_manual(
    values = c("IM" = 24, "MM" = 22), 
    name = "Treatment"
  ) +

  guides(
    fill = guide_legend(override.aes = list(size = 8, shape = 21, color = "black")), 
    shape = guide_legend(override.aes = list(size = 6)) 
  ) +

  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
 
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    legend.position = "right",
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_rect(color = "#808080", linetype = 1)
  )

print(p_pcoa3)

final_plot <- ( p_pcoa3| p_pcoa1 | p_pcoa2) 

print(final_plot)

####fig. 4 A-D####

rm(list=ls())
library(tidyverse)
library(RColorBrewer)
library(cowplot)

get_color_map <- function(genus_list) {
  all_unique <- unique(genus_list)
  color_palette <- colorRampPalette(brewer.pal(12, "Paired"))(length(all_unique))
  names(color_palette) <- all_unique
  return(color_palette)
}

bac_up <- read_csv("data/bacterial_genus_aboveground_top20_ASV.csv") %>%
  mutate(Genus = as.character(Genus))

bac_down <- read_csv("data/bacterial_genus_belowground_top20_ASV.csv") %>%
  mutate(Genus = as.character(Genus))

bac_all_genus <- union(bac_up$Genus, bac_down$Genus)
bac_color_map <- get_color_map(bac_all_genus)

bac_up_long <- bac_up %>%
  pivot_longer(-Genus, names_to = "SampleType", values_to = "Abundance") %>%
  group_by(SampleType) %>%
  mutate(RelativeAbundance = Abundance / sum(Abundance) * 100) %>%
  ungroup()

bac_down_long <- bac_down %>%
  pivot_longer(-Genus, names_to = "SampleType", values_to = "Abundance") %>%
  group_by(SampleType) %>%
  mutate(RelativeAbundance = Abundance / sum(Abundance) * 100) %>%
  ungroup()

bac_up_long$SampleType <- factor(bac_up_long$SampleType, levels = c("phylloplane_MM", "phylloplane_IM","Leaf_en_MM", "Leaf_en_IM", "Stem_ep_MM", "Stem_ep_IM", "Stem_en_MM","Stem_en_IM" ))
bac_up_long$Genus <- factor(bac_up_long$Genus, levels = bac_all_genus)

bac_down_long$SampleType <- factor(bac_down_long$SampleType, levels = c( "Rhizoplane_MM", "Rhizoplane_IM", "Root_en_MM", "Root_en_IM","Bulk_soil_MM", "Bulk_soil_IM", "Rhizosphere_soil_MM", "Rhizosphere_soil_IM"))
bac_down_long$Genus <- factor(bac_down_long$Genus, levels = bac_all_genus)

plot_bac_up <- ggplot(bac_up_long, aes(x = SampleType, y = RelativeAbundance, fill = Genus)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = bac_color_map) +
  labs(title = "Bacterial genus of top20 (Aboveground)", x = NULL, y = NULL) +
  theme_classic()+
  theme(axis.text.x = element_blank())

plot_bac_down <- ggplot(bac_down_long, aes(x = SampleType, y = RelativeAbundance, fill = Genus)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = bac_color_map) +
  labs(title = "Bacterial genus of top20 (Belowground)", x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text.x = element_blank())

fungi_up <- read_csv("data/fungal_genus_aboveground_top20_ASV.csv") %>%
  mutate(Genus = as.character(Genus))
fungi_down <- read_csv("data/fungal_genus_belowground_top20_ASV.csv") %>%
  mutate(Genus = as.character(Genus))

fungi_all_genus <- union(fungi_up$Genus, fungi_down$Genus)
fungi_color_map <- get_color_map(fungi_all_genus)

fungi_up_long <- fungi_up %>%
  pivot_longer(-Genus, names_to = "SampleType", values_to = "Abundance") %>%
  group_by(SampleType) %>%
  mutate(RelativeAbundance = Abundance / sum(Abundance) * 100) %>%
  ungroup()

fungi_down_long <- fungi_down %>%
  pivot_longer(-Genus, names_to = "SampleType", values_to = "Abundance") %>%
  group_by(SampleType) %>%
  mutate(RelativeAbundance = Abundance / sum(Abundance) * 100) %>%
  ungroup()

fungi_up_long$SampleType <- factor(fungi_up_long$SampleType, levels = c("phylloplane_MM", "phylloplane_IM","Leaf_en_MM", "Leaf_en_IM",   "Stem_ep_MM","Stem_ep_IM", "Stem_en_MM","Stem_en_IM"))
fungi_up_long$Genus <- factor(fungi_up_long$Genus, levels = fungi_all_genus)

fungi_down_long$SampleType <- factor(fungi_down_long$SampleType, levels = c("Rhizoplane_MM", "Rhizoplane_IM", "Root_en_MM", "Root_en_IM","Bulk_soil_MM", "Bulk_soil_IM", "Rhizosphere_soil_MM", "Rhizosphere_soil_IM"))
fungi_down_long$Genus <- factor(fungi_down_long$Genus, levels = fungi_all_genus)

plot_fungi_up <- ggplot(fungi_up_long, aes(x = SampleType, y = RelativeAbundance, fill = Genus)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = fungi_color_map) +
  labs(title = "Fungal genus of top20 (Aboveground)", x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text.x = element_blank())

plot_fungi_down <- ggplot(fungi_down_long, aes(x = SampleType, y = RelativeAbundance, fill = Genus)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = fungi_color_map) +
  labs(title = "Fungal genus of top20 (Belowground)", x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.text.x = element_blank())

row_bac <- plot_grid(plot_bac_up, plot_bac_down, labels = c("A", "B"), ncol = 2, label_size = 14, align = "v", axis = "tb")
row_fungi <- plot_grid(plot_fungi_up, plot_fungi_down, labels = c("C", "D"), ncol = 2, label_size = 14, align = "v", axis = "tb")

combined_plot <- plot_grid(row_bac, row_fungi, nrow = 2, rel_heights = c(1, 1))

combined_plot


####fig.5####

rm(list = ls())
library(tidyverse)
library(ggnewscale)
library(patchwork)  

data <- read.csv("data/B&F_biomass&shannon_pcoa1.csv")

bio_vars <- c("Root.biomass", "Stem.biomass", "Leaf.biomass", "Total.biomass",
              "Root.TN", "Stem.TN", "Leaf.TN", "Total.TN")

data <- data %>%
  mutate(
    Microbe = ifelse(grepl("^Fungi", Sample), "Fungi", "Bacteria"),
    IndexType = ifelse(grepl("shannon", Sample), "Shannon", "PCoA1"),
    ShortLabel = sub("^[^_]+_[^_]+_", "", Sample)
  )

cor_results <- data.frame()
samples <- unique(data$Sample)
for (sample in samples) {
  sample_data <- data[data$Sample == sample, ]
  x <- sample_data$index
  for (bio in bio_vars) {
    y <- sample_data[[bio]]
    if (sd(x) == 0 || sd(y) == 0 || any(is.na(x)) || any(is.na(y))) next
    test <- cor.test(x, y, method = "pearson")
    r <- test$estimate
    p <- test$p.value
    sig <- ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
    cor_results <- rbind(cor_results, data.frame(Sample = sample, Biomass = bio, Pearson_Rho = r, P_value = p, Significance = sig))
  }
}

cor_results <- cor_results %>%
  left_join(select(data, Sample, Microbe, IndexType, ShortLabel), by = "Sample") %>%
  distinct(Sample, Biomass, .keep_all = TRUE)

cor_results$Sample <- factor(cor_results$Sample, levels = unique(data$Sample))
cor_results$Biomass <- factor(cor_results$Biomass, levels = rev(bio_vars))

p <- ggplot() +

  geom_tile(data = cor_results, aes(x = Sample, y = -2, fill = Microbe), height = 0.9) +
  scale_fill_manual(values = c("Fungi" = "#9e3bdb", "Bacteria" = "#FFBBFF"),
                    name = "Microbe",
                    guide = guide_legend(title.position = "top", label.position = "right")) +
  new_scale_fill() +

  geom_tile(data = cor_results, aes(x = Sample, y = -1, fill = IndexType), height = 0.9) +
  scale_fill_manual(values = c("Shannon" = "#90D4cf", "PCoA1" = "#32A8A6"),
                    name = "Index Type",
                    guide = guide_legend(title.position = "top", label.position = "right")) +
  new_scale_fill() +
 
  geom_tile(data = cor_results, aes(x = Sample, y = Biomass, fill = Pearson_Rho), color = "#E0E0E0", linewidth = 0.4) +
  scale_fill_gradient2(low ="#6B8E23" , high = "#FFB90F", midpoint = 0, limit = c(-1, 1),
                       name = "Pearson Rho",
                       guide = guide_colorbar(title.position = "top", barwidth = 12, barheight = 0.5)) +
  
  geom_text(data = cor_results, aes(x = Sample, y = Biomass, label = Significance), size = 5) +
  
  scale_y_discrete(position = "right") +
  scale_x_discrete(position = "bottom", labels = setNames(cor_results$ShortLabel, cor_results$Sample)) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, face = "bold", size = 18),
    axis.text.y.right = element_text(face = "bold", size = 13),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    
    legend.position = "top",
    legend.box = "horizontal",      
    legend.spacing.x = unit(0.5, "cm"),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 2, b = 2)
  ) +
  coord_fixed(ratio = 0.6)

print(p)

####fig.6A bacterial—tree####

rm(list=ls()) 
library(ggtree)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(cowplot)
library(patchwork) 
library(ape)
library(ggtreeExtra) 
library(grid)
library(readr) 
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

otu <- read.table("data/ASV_bacteria_DNA_noSB_taxa.csv", header = TRUE, row.names = 1, sep = ",")

yield <- read.csv("data/bio_N.csv", header = TRUE)

otu <- as.data.frame(otu)
otu <- otu[, colnames(otu) %in% yield$Sample]  
otu <- t(otu)  
otu <- as.data.frame(otu)
otu$Sample <- rownames(otu)

merged_data <- merge(yield, otu, by = "Sample")

traits <- colnames(yield)[2:ncol(yield)]  
otus <- colnames(merged_data)[!(colnames(merged_data) %in% c("Sample", traits))]

cor_results <- data.frame()

for (trait in traits) {
  for (otu_col in otus) {
    x <- merged_data[[otu_col]]
    y <- merged_data[[trait]]
    if (is.numeric(x) && is.numeric(y)) {
      test <- cor.test(x, y, method = "pearson", use = "pairwise.complete.obs") 
      cor_results <- rbind(cor_results, data.frame(
        Trait = trait,
        OTU = otu_col,
        r = test$estimate,  
        P = test$p.value
      ))
    }
  }
}

cor_results$Significance <- cut(cor_results$P,
                                breaks = c(-Inf, 0.001, 0.01, 0.05, 1),
                                labels = c("***", "**", "*", ""),
                                right = TRUE)

cor_filtered <- cor_results %>% filter(P < 0.05)

sig_asv <- unique(cor_filtered$OTU)

cor_sig_asv <- cor_results %>% filter(OTU %in% sig_asv)

write.csv(cor_results, "data/bacterial_p&r.csv", row.names = FALSE)



sig_asv_present <- sig_asv[sig_asv %in% rownames(t(otu))]
otu_sig <- t(otu)[sig_asv_present, ]
otu_sig_binary <- as.data.frame((otu_sig > 0) * 1)
write.csv(otu_sig_binary, "data/bacterial_niche.csv", row.names = TRUE)

cor_sig_asv$Trait <- factor(cor_sig_asv$Trait, levels = traits)
cor_df <- cor_sig_asv

sig_asv_df <- data.frame(ASV_ID = sig_asv) 
write.csv(sig_asv_df, file = "data/bacterial_ASV_list.csv",row.names = FALSE)

asv_list <- read.csv("data/bacterial_ASV_list.csv", header = TRUE)
taxonomy <- read.csv("data/taxonomy.csv", header = TRUE)
target_asv <- asv_list$ASV_ID
filtered_taxonomy <- taxonomy[taxonomy$ASV_ID %in% target_asv, ]
nrow(filtered_taxonomy)

head(filtered_taxonomy)
write.csv(filtered_taxonomy, "data/filtered_taxonomy.csv", row.names = FALSE)

library(Biostrings)
library(tidyverse)

significant_asvs <- read_csv("data/bacterial_ASV_list.csv") %>%
  pull(ASV_ID) 

fasta_file <- readDNAStringSet("data/ASV_reps.fasta")

matched_asvs <- fasta_file[names(fasta_file) %in% significant_asvs]

writeXStringSet(matched_asvs, "data/bacterial_sequence_ASV.fa")

library(ggtree)
library(dplyr)
library(ape)
library(ggtreeExtra)
library(ggplot2)
library(ggnewscale)

tree <- read.tree("data/otus_16s0609.treefile")

group_data <- read.csv("data/filtered_taxonomy.csv")

phylum_colors <- c(
  "p__Acidobacteriota" = "#CD0000",
  "p__Actinobacteriota" = "#377EB8",
  "p__Armatimonadota" = "#4DAF4A",
  "p__Bacteroidota" = "#984EA3",
  "p__Chloroflexi" = "#FF7F00",
  "p__Firmicutes" = "#CDCD00",
  "p__Myxococcota" = "#0000CD",
  "p__Nitrospirota" = "#8B4513",
  "p__Patescibacteria"="#FF3E96",
  "p__Proteobacteria"= "#F08080"
)

cladegroup <- group_data %>%
  filter(!is.na(Phylum), Phylum %in% names(phylum_colors)) %>%
  select(ASV_ID, Phylum) %>%
  split(.$Phylum) %>%
  lapply(function(x) x$ASV_ID)

p_tmp <- ggtree(tree, layout = "rectangular", branch.length = "none")

tip_order_df <- p_tmp$data %>%
  dplyr::filter(isTip == TRUE, !is.na(label)) %>%
  dplyr::arrange(desc(y)) %>%
  dplyr::select(label) %>%
  dplyr::rename(ASV_ID = label)


write.csv(tip_order_df,file = "data/bacterial_tree_order.csv",row.names = FALSE, quote = FALSE)
tip_order_with_phylum <- p_tmp$data %>%
  dplyr::filter(isTip == TRUE, !is.na(label)) %>%
  dplyr::arrange(desc(y)) %>%
  dplyr::select(label) %>%
  dplyr::rename(ASV_ID = label) %>%
  left_join(group_data %>% select(ASV_ID, Phylum), by = "ASV_ID") %>%
  dplyr::arrange(dplyr::desc(dplyr::row_number()))

write.csv(tip_order_with_phylum,file = "data/bacterial_tree_order_phylum.csv",row.names = FALSE,quote = FALSE)

tree2 <- groupOTU(tree, cladegroup)


p_tree <- ggtree(tree2, layout = "rectangular", branch.length = "none", aes(color = group), size = 0.6) +
  geom_tiplab(size = 3, hjust = 1, offset = 3.5) +
  scale_color_manual(values = phylum_colors, guide = "none") +
  theme(plot.margin = margin(5, 0, 5, 5))

p_tree <- ggtree(tree2, layout = "rectangular", branch.length = "none", aes(color = group), size = 0.6) +
  geom_tiplab(size = 3, hjust = 1, offset = 3) +
  scale_color_manual(values = phylum_colors, guide = "none") +
  new_scale_fill() +
  geom_fruit(
    data = tip_order_with_phylum,
    geom = geom_tile,
    mapping = aes(y = ASV_ID, fill = Phylum),
    width = 1.2,
    offset = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = phylum_colors) +
  theme(
    plot.margin = margin(5, 0, 5, 5)
  )
write.csv(tip_order_df, file = "data/tip_order_by_tree_y.csv", 
          row.names = FALSE, quote = FALSE)
print(p_tree)

####fig.6A bacterial_heatmap####
tip_order_y <- read_csv("data/bacterial_tree_order.csv", show_col_types = FALSE)[[1]]
tip_order_y <- tip_order_df$ASV_ID
cor_df <- read.csv("data/bacterial_p&r.csv", stringsAsFactors = FALSE)

cor_df <- cor_df %>% filter(OTU %in% tip_order_y)

cor_df$OTU <- factor(cor_df$OTU,  levels = rev(tip_order_y))

cor_df <- cor_df %>% arrange(OTU)






traits <- c(
  "Leaf_biomass", "Stem_biomass","Root_biomass", "Total_biomass",
  "Leaf_N_concentration","Stem_N_concentration", "Root_N_concentration", "Total_N"
)

cor_df$Trait <- factor(cor_df$Trait, levels = traits)


n_asv <- length(tip_order_y)

cor_df$y_num <- match(cor_df$OTU, rev(tip_order_y))  
p_heatmap <- ggplot(cor_df, aes(x = Trait, y = y_num, fill = r)) +
  geom_tile(color = "white", height = 1) +  
  geom_text(aes(label = Significance), color = "black", size = 5) +
  scale_fill_gradient2(
    low = "#008B45", mid = "#FFF8DC", high ="#FF7F24", midpoint = 0,
    
    name = "Pearson's r", limits = c(-0.4, 0.4)
  ) +
  scale_y_continuous(
    breaks = 1:n_asv,
    labels = rev(tip_order_y),
    expand = c(0, 0),
    position = "right"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1,size =15),
    plot.margin = margin(5, 5, 5, 0)
  )
print(p_heatmap)



library(cowplot)
legend_plot <- ggplot(tip_order_with_phylum, aes(x = 1, y = 1, fill = Phylum)) +
  geom_tile() +
  scale_fill_manual(values = phylum_colors, name = "Phylum") +
  theme_minimal() +
  theme(legend.position = "right")

phylum_legend <- get_legend(legend_plot)
main_plot <- p_tree + p_heatmap + plot_layout(ncol = 2, widths = c(0.4, 0.6))
final_plot <- plot_grid(
  
  phylum_legend,main_plot,
  ncol = 2,
  rel_widths = c( 0.12,1)
)

print(final_plot)
####fig.6A bacterial_niche####
library(tidyverse)
library(pheatmap)
library(grid) 
binary_data <- read.csv("data/bacterial_niche.csv",
                        row.names = 1, check.names = FALSE)

group_info <- read.csv("data/bacterial_group.csv")

tip_order_df <- read.csv("data/bacterial_tree_order.csv") 

sample_niche <- group_info %>% select(Sample, Niche)

binary_long <- binary_data %>%
  rownames_to_column("ASV") %>%
  pivot_longer(-ASV, names_to = "Sample", values_to = "Present") %>%
  left_join(sample_niche, by = "Sample") %>%
  mutate(Present = as.numeric(Present)) %>%
  group_by(ASV, Niche) %>%
  summarise(Present = as.numeric(any(Present == 1)), .groups = "drop")

binary_wide <- binary_long %>%
  pivot_wider(names_from = Niche, values_from = Present, values_fill = 0) %>%
  column_to_rownames("ASV")

binary_mat_unique <- as.matrix(binary_wide)

asv_order <- tip_order_df$ASV_ID
asv_order_used <- asv_order[asv_order %in% rownames(binary_mat_unique)]
binary_mat_unique <- binary_mat_unique[asv_order_used, , drop = FALSE]

niche_levels <- c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane",
                  "Root_en", "Stem_ep", "Stem_en", 
                  "phylloplane", "Leaf_en")
binary_mat_unique <- binary_mat_unique[, niche_levels[niche_levels %in% colnames(binary_mat_unique)], drop = FALSE]

ann_colors <- list(
  Niche = c("Bulk_soil" = "#66c2a5",
            "Rhizosphere_soil" = "#fc8d62",
            "Rhizoplane" = "#8da0cb",
            "Root_en" = "#e78ac3",
            "Stem_ep" = "#a6d854",
            "Stem_en" = "#ffd92f",
            "phylloplane" = "#e5c494",
            "Leaf_en" = "#b3b3b3")
)

annotation_col <- data.frame(
  Niche = colnames(binary_mat_unique),
  row.names = colnames(binary_mat_unique)
)

p <- pheatmap(
  binary_mat_unique,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  show_colnames = TRUE,
  show_rownames = TRUE,
  color = c("white", "#8da0cb"),
  annotation_col = annotation_col,
  annotation_colors = ann_colors,
  main = "Bacterial ASV Ecological Niches (Ordered by tip_order_df)",
  silent = TRUE  
)

grid.newpage()
grid.draw(p$gtable)


####fig.6B fungal_tree####

rm(list=ls()) 
library(ggtree)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(cowplot)
library(patchwork) 
library(ape)
library(ggtreeExtra) 
library(grid)
library(readr) 
library(tidyr)
library(tidyverse)

df <- read_csv("data/taxonomy_fungi.csv")  

head(df)

df_split <- df %>%

  separate(
    col = taxonomy,                   
    into = c("Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
    sep = ";",                       
    remove = FALSE,                   
    fill = "right",                     
    extra = "drop"                  
  ) %>%

  mutate(across(Domain:Species, ~ str_replace(., "^[a-z]__", ""))) %>%

  select(ASV_ID = `ASV ID`, Domain, Kingdom, Phylum, Class, Order, Family, Genus, Species)

head(df_split)

write_csv(df_split, "data/taxonomy_fungi_separate.csv")
library(dplyr)
library(ggplot2)


otu <- read.table("data/asv_fungi(1).csv", header = TRUE, row.names = 1, sep = ",")


yield <- read.csv("data/bio_N.csv", header = TRUE)

otu <- as.data.frame(otu)
otu <- otu[, colnames(otu) %in% yield$Sample]  
otu <- t(otu) 
otu <- as.data.frame(otu)
otu$Sample <- rownames(otu)

merged_data <- merge(yield, otu, by = "Sample")

traits <- colnames(yield)[2:ncol(yield)]  
otus <- colnames(merged_data)[!(colnames(merged_data) %in% c("Sample", traits))]

cor_results <- data.frame()

for (trait in traits) {
  for (otu_col in otus) {
    x <- merged_data[[otu_col]]
    y <- merged_data[[trait]]
    if (is.numeric(x) && is.numeric(y)) {
      test <- cor.test(x, y, method = "pearson", use = "pairwise.complete.obs")  
      cor_results <- rbind(cor_results, data.frame(
        Trait = trait,
        OTU = otu_col,
        r = test$estimate,
        P = test$p.value
      ))
    }
  }
}

cor_results$Significance <- cut(cor_results$P,
                                breaks = c(-Inf, 0.001, 0.01, 0.05, 1),
                                labels = c("***", "**", "*", ""),
                                right = TRUE)


cor_filtered <- cor_results %>% filter(P < 0.05)


sig_asv <- unique(cor_filtered$OTU)


cor_sig_asv <- cor_results %>% filter(OTU %in% sig_asv)


write.csv(cor_results, "data/fungal_p&r.csv", row.names = FALSE)


sig_asv_present <- sig_asv[sig_asv %in% rownames(t(otu))]
otu_sig <- t(otu)[sig_asv_present, ]
otu_sig_binary <- as.data.frame((otu_sig > 0) * 1)
write.csv(otu_sig_binary, "data/fungal_niche.csv", row.names = TRUE)

cor_sig_asv$Trait <- factor(cor_sig_asv$Trait, levels = traits)
cor_df <- cor_sig_asv




sig_asv_df <- data.frame(ASV_ID = sig_asv)
write.csv(sig_asv_df, 
          file = "data/fungal_ASV_list.csv",
          row.names = FALSE)



asv_list <- read.csv("data/fungal_ASV_list.csv", header = TRUE)

taxonomy <- read.csv("data/taxonomy_fungi_separate.csv", header = TRUE)

target_asv <- asv_list$ASV_ID

filtered_taxonomy <- taxonomy[taxonomy$ASV_ID %in% target_asv, ]
nrow(filtered_taxonomy)

head(filtered_taxonomy)
write.csv(filtered_taxonomy, "data/filtered_taxonomy.csv", row.names = FALSE)

library(Biostrings)
library(tidyverse)

significant_asvs <- read_csv("data/fungal_ASV_list.csv") %>%
  pull(ASV_ID)  

fasta_file <- readDNAStringSet("data/ASV_fungi_0721.fa")

matched_asvs <- fasta_file[names(fasta_file) %in% significant_asvs]

writeXStringSet(matched_asvs, 
                "data/fungal_ASV.fa")

library(ggtree)
library(dplyr)
library(ape)
library(ggtreeExtra)
library(ggplot2)
library(ggnewscale)

tree <- read.tree("data/otus_ITS.treefile")
group_data <- read.csv("data/filtered_taxonomy.csv")

phylum_colors <- c(
  "Ascomycota" = "#F08080",
  "Basidiomycota" = "#377EB8",
  "Chytridiomycota" = "#4DAF4A",
  "Mortierellomycota" = "#984EA3",
  "Rozellomycota" = "#FF7F00",
  "unclassified" = "#CDCD00"
)

cladegroup <- group_data %>%
  filter(!is.na(Phylum), Phylum %in% names(phylum_colors)) %>%
  select(ASV_ID, Phylum) %>%
  split(.$Phylum) %>%
  lapply(function(x) x$ASV_ID)

p_tmp <- ggtree(tree, layout = "rectangular", branch.length = "none")

tip_order_df <- p_tmp$data %>%
  dplyr::filter(isTip == TRUE, !is.na(label)) %>%
  dplyr::arrange(desc(y)) %>%
  dplyr::select(label) %>%
  dplyr::rename(ASV_ID = label)


write.csv(tip_order_df,
          file = "data/fungal_tree_order.csv",
          row.names = FALSE,
          quote = FALSE)
tip_order_with_phylum <- p_tmp$data %>%
  dplyr::filter(isTip == TRUE, !is.na(label)) %>%
  dplyr::arrange(desc(y)) %>%
  dplyr::select(label) %>%
  dplyr::rename(ASV_ID = label) %>%
  left_join(group_data %>% select(ASV_ID, Phylum), by = "ASV_ID") %>%
  dplyr::arrange(dplyr::desc(dplyr::row_number()))

write.csv(tip_order_with_phylum,
          file = "data/fungal_tree_order_phylum.csv",
          row.names = FALSE,
          quote = FALSE)

tree2 <- groupOTU(tree, cladegroup)



p_tree <- ggtree(tree2, layout = "rectangular", branch.length = "none", aes(color = group), size = 0.6) +
  geom_tiplab(size = 3, hjust = 1, offset = 3.5) +
  scale_color_manual(values = phylum_colors, guide = "none") +
  theme(plot.margin = margin(5, 0, 5, 5))
print(p_tree)

p_tree <- ggtree(tree2, layout = "rectangular", branch.length = "none", aes(color = group), size = 0.6) +
  geom_tiplab(size = 3, hjust = 1, offset = 3) +
  scale_color_manual(values = phylum_colors, guide = "none") +
  new_scale_fill() +
  geom_fruit(
    data = tip_order_with_phylum,
    geom = geom_tile,
    mapping = aes(y = ASV_ID, fill = Phylum),
    width = 1.2,
    offset = 0.3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = phylum_colors) +
  theme(
    plot.margin = margin(5, 0, 5, 5)
  )

print(p_tree)

write.csv(tip_order_df, file = "data/tip_order_by_tree_y.csv", 
          row.names = FALSE, quote = FALSE)

####fig.6B fungal_heatmap####

tip_order_y <- read_csv("data/fungal_tree_order.csv", show_col_types = FALSE)[[1]]
tip_order_y <- tip_order_df$ASV_ID

cor_df <- read.csv("data/fungal_p&r.csv", stringsAsFactors = FALSE)

cor_df <- cor_df %>% filter(OTU %in% tip_order_y)

cor_df$OTU <- factor(cor_df$OTU,  levels = rev(tip_order_y))


cor_df <- cor_df %>% arrange(OTU)






traits <- c(
  "Leaf_biomass", "Stem_biomass","Root_biomass", "Total_biomass",
  "Leaf_N_concentration","Stem_N_concentration", "Root_N_concentration", "Total_N"
)

cor_df$Trait <- factor(cor_df$Trait, levels = traits)


n_asv <- length(tip_order_y)


cor_df$y_num <- match(cor_df$OTU, rev(tip_order_y))  
p_heatmap <- ggplot(cor_df, aes(x = Trait, y = y_num, fill = r)) +
  geom_tile(color = "white", height = 1) +  
  geom_text(aes(label = Significance), color = "black", size = 6) +
  scale_fill_gradient2(
    low = "#008B45", mid = "#FFF8DC", high ="#FF7F24", midpoint = 0,
    name = "Pearson's r"
  ) +
  scale_y_continuous(
    breaks = 1:n_asv,
    labels = rev(tip_order_y),
    expand = c(0, 0),
    position = "right"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1,size=15),
    plot.margin = margin(5, 5, 5, 0)
  )

print(p_heatmap)
ggsave("data/heatmap_fungal.pdf", p_heatmap, width = 6, height = 12)





library(cowplot)

legend_plot <- ggplot(tip_order_with_phylum, aes(x = 1, y = 1, fill = Phylum)) +
  geom_tile() +
  scale_fill_manual(values = phylum_colors, name = "Phylum") +
  theme_minimal() +
  theme(legend.position = "right")


phylum_legend <- get_legend(legend_plot)

main_plot <- p_tree + p_heatmap + plot_layout(ncol = 2, widths = c(0.4, 0.6))

final_plot <- plot_grid(
  
  phylum_legend,main_plot,
  ncol = 2,
  rel_widths = c( 0.12,1) 
)

print(final_plot)


####fig.6B fungal_niche####

library(tidyverse)
library(reshape2)
library(pheatmap)
library(ggplot2)
library(ggtree)
library(patchwork) 


binary_data <- read.csv("data/fungal_niche.csv", row.names = 1, check.names = FALSE)
group_info <- read.csv("data/fungal_group.csv")


cor_sig_asv <- tip_order_df$ASV_ID


binary_data <- binary_data[cor_sig_asv, , drop = FALSE]  


binary_mat <- as.matrix(binary_data)


group_info$Niche <- factor(group_info$Niche, 
                           levels = c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane",
                                      "Root_en", "Stem_ep", "Stem_en", 
                                      "phylloplane", "Leaf_en"))

group_info$Site <- factor(group_info$Site, 
                          levels = c("soil", "Root", "stem", "Leaf"))


group_info <- group_info %>% 
  arrange(Niche, Site)  


binary_mat <- binary_mat[, group_info$Sample]


annotation_col <- data.frame(
  Niche = group_info$Niche,
  Site = group_info$Site,
  row.names = group_info$Sample
)



library(tidyverse)
library(pheatmap)


binary_data <- read.csv("data/fungal_niche.csv", 
                        row.names = 1, check.names = FALSE)
group_info <- read.csv("data/fungal_group.csv")
tip_order_df <- read.csv("data/fungal_tree_order.csv") 


sample_niche <- group_info %>% select(Sample, Niche)

binary_long <- binary_data %>%
  rownames_to_column("ASV") %>%
  pivot_longer(-ASV, names_to = "Sample", values_to = "Present") %>%
  left_join(sample_niche, by = "Sample") %>%
  mutate(Present = as.numeric(Present)) %>%
  group_by(ASV, Niche) %>%
  summarise(Present = as.numeric(any(Present == 1)), .groups = "drop")

binary_wide <- binary_long %>%
  pivot_wider(names_from = Niche, values_from = Present, values_fill = 0) %>%
  column_to_rownames("ASV")

binary_mat_unique <- as.matrix(binary_wide)


asv_order <- tip_order_df$ASV_ID
asv_order_used <- asv_order[asv_order %in% rownames(binary_mat_unique)]
binary_mat_unique <- binary_mat_unique[asv_order_used, , drop = FALSE]

niche_levels <- c("Bulk_soil", "Rhizosphere_soil", "Rhizoplane",
                  "Root_en", "Stem_ep", "Stem_en", 
                  "phylloplane", "Leaf_en")
binary_mat_unique <- binary_mat_unique[, niche_levels[niche_levels %in% colnames(binary_mat_unique)], drop = FALSE]

ann_colors <- list(
  Niche = c("Bulk_soil" = "#66c2a5",
            "Rhizosphere_soil" = "#fc8d62",
            "Rhizoplane" = "#8da0cb",
            "Root_en" = "#e78ac3",
            "Stem_ep" = "#a6d854",
            "Stem_en" = "#ffd92f",
            "phylloplane" = "#e5c494",
            "Leaf_en" = "#b3b3b3")
)

annotation_col <- data.frame(
  Niche = colnames(binary_mat_unique),
  row.names = colnames(binary_mat_unique)
)

pheatmap(
  binary_mat_unique,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  show_colnames = TRUE,
  show_rownames = TRUE,
  color = c("white", "#8da0cb"),
  annotation_col = annotation_col,
  annotation_colors = ann_colors,
  main = "ASV Ecological Niches (Ordered by tip_order_df)",
  filename = "data/Niche_deduplicated_ordered.pdf", width = 4.5,   
  height = 15 
)
