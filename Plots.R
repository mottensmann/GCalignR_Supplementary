## Ordiplot
library(ggplot2)
## run to code chunk in R_code
a <- ggplot(data = scent_nmds,aes(MDS1,MDS2,color = colony)) +
    geom_point() + 
    theme_void() + 
    scale_color_manual(values = c("blue","red")) +
    theme(panel.background = element_rect(colour = "black", size = 1.25,
            fill = NA), aspect.ratio = 1, legend.position = "none") 
ggplot2::ggsave(a, filename = "Figures/Fig7.tiff",width = 5,height = 5,units = "in",dpi = 300, device = "tiff")

## Heatmap
library(ggplot2)
library(GCalignR)
## run to code chunk
a <- gc_heatmap(aligned_peak_data,type = "binary",
                threshold = 0.05,
                main_title = "",
                label = "none",
                show_legend = F)
a <- a + theme(plot.background = element_rect(fill = "white"),
               axis.title.x = element_text(size = 12),
               axis.title.y = element_text(size = 12))

ggplot2::ggsave(a,filename = "Figures/Figure6.tiff",width = 6,height = 4,units = "in",dpi = 300, device = "tiff")

## diagnostic plots
library(GCalignR)
# 1500 x 500
plot_1 <- plot(x = aligned_peak_data, which_plot ="peak_num",cex.lab = 1.5,cex.names = 0.8)

# 500 x 500
plot_2 <- plot(x = aligned_peak_data, which_plot ="shifts",cex.lab = 1.5,cex.names = 0.8)

plot_3 <- plot(x = aligned_peak_data, which_plot ="variation",cex.lab = 1.5,cex.names = 0.8)

plot_4 <- plot(x = aligned_peak_data, which_plot ="peaks_s",cex.lab = 1.5,cex.names = 0.8)

## Simualtion 
# 1000x1000px
with(errors_bbim,scatter3D(
    x = p2p,
    y = p2m,
    z = error, 
    pch = 19,
    size = 2,
    theta = 30,
    phi = 0,
    ticktype = "detailed",
    xlab = "",
    ylab = "",
    zlab = "",
    bty = "g",
    colkey = FALSE,
    cex = 1.5,
    cex.axis = 1,
    zlim = c(0,0.2)))

with(errors_beph,scatter3D(
    x = p2p,
    y = p2m,
    z = error, 
    pch = 19,
    size = 2,
    theta = 30,
    phi = 0,
    ticktype = "detailed",
    xlab = "",
    ylab = "",
    zlab = "",
    bty = "g",
    colkey = FALSE,
    cex = 1.5,
    cex.axis = 1,
    zlim = c(0,0.2)))

with(errors_bfla,scatter3D(
    x = p2p,
    y = p2m,
    z = error, 
    pch = 19,
    size = 2,
    theta = 30,
    phi = 0,
    ticktype = "detailed",
    xlab = "",
    ylab = "",
    zlab = "",
    bty = "g",
    colkey = FALSE,
    cex = 1.5,
    cex.axis = 1,
    zlim = c(0,0.2)))
