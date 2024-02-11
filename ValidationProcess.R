library(ggplot2)
library(ggpmisc)
library(ggrepel)
library(dplyr)
library(quantmod)
library(gridExtra)
library(grid)


# ggplot of T3,T4 and T16   
#import brightness temperature data of channel 3, 4 and 16 within five different period
Data1 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34.csv", sep=",", header=T)
Data2 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_2.csv", sep=",", header=T)
Data3 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_3.csv", sep=",", header=T)
Data4 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_AFTER.csv", sep=",", header=T)
Data5 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_1.csv", sep=",", header=T)
#define theme parameters for ggplot
cols <- c("Ch3L" = "red", "Ch4L" = "blue", "Ch16L" = "green", 
          "Ch3W" = "purple", "Ch4W" = "cyan", "Ch16W" = "orange")
color <- c("red","blue","green","purple","cyan","orange")
line <- c(1, 2, 3, 4, 5, 6)
windowsFonts(A = windowsFont("Palatino Linotype"))
#ggplot for five scenarios
p1 <- ggplot(Data1, aes(x = Data1$Index, y = Data1$L3, group = 1))+
    geom_line(aes(y = Data1$L3, colour="Ch3L"), size = 1)+
    geom_line(aes(y = Data1$L4, colour="Ch4L"), size = 1)+
    geom_line(aes(y = Data1$L16, colour="Ch16L"), size = 1)+
    geom_line(aes(y = Data1$W3, colour="Ch3W"), size = 1)+
    geom_line(aes(y = Data1$W4, colour="Ch4W"), size = 1)+
    geom_line(aes(y = Data1$W16, colour="Ch16W"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/18/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")

p2 <- ggplot(Data2, aes(x = Data2$Index, y = Data2$L3, group = 1))+
    geom_line(aes(y = Data2$L3, colour="Ch3L"), size = 1)+
    geom_line(aes(y = Data2$L4, colour="Ch4L"), size = 1)+
    geom_line(aes(y = Data2$L16, colour="Ch16L"), size = 1)+
    geom_line(aes(y = Data2$W3, colour="Ch3W"), size = 1)+
    geom_line(aes(y = Data2$W4, colour="Ch4W"), size = 1)+
    geom_line(aes(y = Data2$W16, colour="Ch16W"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/19/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")

p3 <- ggplot(Data3, aes(x = Data3$Index, y = Data3$L3, group = 1))+
    geom_line(aes(y = Data3$L3, colour="Ch3L"), size = 1)+
    geom_line(aes(y = Data3$L4, colour="Ch4L"), size = 1)+
    geom_line(aes(y = Data3$L16, colour="Ch16L"), size = 1)+
    geom_line(aes(y = Data3$W3, colour="Ch3W"), size = 1)+
    geom_line(aes(y = Data3$W4, colour="Ch4W"), size = 1)+
    geom_line(aes(y = Data3$W16, colour="Ch16W"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/23/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")

p4 <- ggplot(Data4, aes(x = Data4$Index, y = Data5$L3, group = 1))+
    geom_line(aes(y = Data4$L3, colour="Ch3L"), size = 1)+
    geom_line(aes(y = Data4$L4, colour="Ch4L"), size = 1)+
    geom_line(aes(y = Data4$L16, colour="Ch16L"), size = 1)+
    geom_line(aes(y = Data4$W3, colour="Ch3W"), size = 1)+
    geom_line(aes(y = Data4$W4, colour="Ch4W"), size = 1)+
    geom_line(aes(y = Data4$W16, colour="Ch16W"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/30/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")

p5 <- ggplot(Data5, aes(x = Data5$Index, y = Data5$L3, group = 1))+
    geom_line(aes(y = Data5$L3, colour="Ch3L"), size = 1)+
    geom_line(aes(y = Data5$L4, colour="Ch4L"), size = 1)+
    geom_line(aes(y = Data5$L16, colour="Ch16L"), size = 1)+
    geom_line(aes(y = Data5$W3, colour="Ch3W"), size = 1)+
    geom_line(aes(y = Data5$W4, colour="Ch4W"), size = 1)+
    geom_line(aes(y = Data5$W16, colour="Ch16W"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="9/3/2017 Feature Sample Index", y="BT (K)", 
          color  = "Legend", linetype = "Legend", shape = "Legend")

#legend setup
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + guides(colour = guide_legend(nrow = 1))+ theme(legend.position = c(0.5,0.5),legend.direction="horizontal"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
}
#output
out = grid_arrange_shared_legend(p1, p2, p3, p4, p5, nrow = 3, ncol = 2)
ggsave("E:/temp/NewIdea/paper/Major revision/Graphs/Figure3.jpg", out, height=7, width=10, dpi=1200)
# ggplot of T3, T4 difference
#import brightness temperature difference between channel 3 and 4 within five different period
Data11 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_Diff.csv", sep=",", header=T)
Data22 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_2_Diff.csv", sep=",", header=T)
Data33 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_3_Diff.csv", sep=",", header=T)
Data44 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_AFTER_Diff.csv", sep=",", header=T)
Data55 <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\T34_1_Diff.csv", sep=",", header=T)
#define theme parameters for ggplot
cols <- c("Ch(4-3)W" = "blue", "Ch(4-3)L" = "red")
color <- c("blue","red")
line <- c(1, 2)

#ggplot for five scenarios
p11 <- ggplot(Data11, aes(x = Data11$Index, y = Data11$Wdiff, group = 1))+
    geom_line(aes(y = Data11$Wdiff, colour="Ch(4-3)W"), size = 1)+
    geom_line(aes(y = Data11$Ldiff, colour="Ch(4-3)L"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/18/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")

p22 <- ggplot(Data22, aes(x = Data22$Index, y = Data22$Wdiff, group = 1))+
    geom_line(aes(y = Data22$Wdiff, colour="Ch(4-3)W"), size = 1)+
    geom_line(aes(y = Data22$Ldiff, colour="Ch(4-3)L"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/19/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")

p33 <- ggplot(Data33, aes(x = Data33$Index, y = Data33$Wdiff, group = 1))+
    geom_line(aes(y = Data33$Wdiff, colour="Ch(4-3)W"), size = 1)+
    geom_line(aes(y = Data33$Ldiff, colour="Ch(4-3)L"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/23/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")


p44 <- ggplot(Data44, aes(x = Data44$Index, y = Data44$Wdiff, group = 1))+
    geom_line(aes(y = Data44$Wdiff, colour="Ch(4-3)W"), size = 1)+
    geom_line(aes(y = Data44$Ldiff, colour="Ch(4-3)L"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="8/30/2017 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")

p55 <- ggplot(Data55, aes(x = Data55$Index, y = Data55$Wdiff, group = 1))+
    geom_line(aes(y = Data55$Wdiff, colour="Ch(4-3)W"), size = 1)+
    geom_line(aes(y = Data55$Ldiff, colour="Ch(4-3)L"), size = 1)+
    theme(text=element_text(family="A"))+
    labs(x="9/3/2018 Feature Sample Index", y="BT (K)", 
         color  = "Legend", linetype = "Legend", shape = "Legend")
#output
out2 = grid_arrange_shared_legend(p11, p22, p33, p44, p55, nrow = 3, ncol = 2)
ggsave("E:/temp/NewIdea/paper/Major revision/Graphs/Figure4.jpg", out2, height=7, width=10, dpi=1200)

#linear model of CERA SSH and DWF result
#import DWF and CERA SSH datasets
Data<-read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\Final.csv", sep=",", header=T)
head(Data)
#develop a linear regression model for these two datasets
reg <- lm(Data$PDWF~Data$PSSH,data=Data) 
arrow = arrow(angle=10)
#legend setup
equation = function(x) {
    lm_coef <- list(a = round(coef(x)[1], digits = 2),
                    b = round(coef(x)[2], digits = 2),
                    r2 = round(summary(x)$r.squared, digits = 2));
    lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
    as.character(as.expression(lm_eq));                 
}

#plot linear model
ggplot(Data, aes(x = Data$PDWF, y = Data$PSSH))+
    geom_point()+
    geom_smooth(method=lm, se=FALSE, colour = "black")+
    stat_poly_eq(aes(label = equation(reg)), 
                 label.x.npc = 0.2, label.y.npc = 0.6,
                 formula = reg, parse = TRUE, size = 5)+
    xlab("DWF (%)") + ylab("CERA SSH (%)")+ theme_bw()+
    theme(text=element_text(family="A"))+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(), 
          axis.line.x = element_line(), 
          axis.line.y = element_line())
#export linear model plot
ggsave("E:/temp/NewIdea/paper/Major revision/Graphs/Figure 9.jpg", height=5, width=5, dpi=1200)
summary(reg)
#Pearson's correlation
cor(Data$PDWF, Data$PSSH, method = c("pearson"))


#T-test
Data <- read.table("E:\\temp\\NewIdea\\paper\\Paper\\MajorRevisionData\\Test.csv", sep=",", header=T)

t.test(Data$Before,Data$After,data=Data,paired=TRUE)
