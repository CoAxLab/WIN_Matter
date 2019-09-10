library("pander")
library("tidyverse")
library("knitr")
library("car")
library("reshape2")
library("ggplot2")
library('gridExtra')
library("plyr")
PC1_less<- read_csv('/data/dataDB/WIN_Matter/analysis/data/PC1_lesser_sigOnly_fibers_grubbs.csv')
PC1_less<- mutate(PC1_less, tx_code=factor(tx_code, 
                                           levels=1:3, 
                                           labels=c("DIET","MOD-PA","HIGH-PA")))



PC1_great<- read_csv('/data/dataDB/WIN_Matter/analysis/data/PC1_greater_sigOnly_fibers_grubbs.csv')
PC1_great<- mutate(PC1_great, tx_code=factor(tx_code, 
                                             levels=1:3, 
                                             labels=c("DIET","MOD-PA","HIGH-PA")))

PC2_great<- read_csv('/data/dataDB/WIN_Matter/analysis/data/PC2_greater_sigOnly_fibers_grubbs.csv')
PC2_great<- mutate(PC2_great, tx_code=factor(tx_code, 
                                             levels=1:3, 
                                             labels=c("DIET","MOD-PA","HIGH-PA")))

PC2_less<- read_csv('/data/dataDB/WIN_Matter/analysis/data/PC2_lesser_sigOnly_fibers_grubbs.csv')
PC2_less<- mutate(PC2_less, tx_code=factor(tx_code, 
                                           levels=1:3, 
                                           labels=c("DIET","MOD-PA","HIGH-PA")))

# Create Boxplot
par(mar=c(2,2,2,2))
par(mfrow=c(2,2)) #2 rows 2 col
boxplot(fiberMean ~ tx_code, data = PC1_great,
        xlab = "Group", ylab = expression(paste(Delta,'MeanFiber'['post-pre'])),
        main = "PC1(+)")
#dev.copy(png, '~/Google Drive/WIN_Matter/analysis/output/images/pc1_great_tx_code_collapsed.jpg')
#dev.off()
# Create Boxplot
#par(mar=c(1,1,1,1))
boxplot(fiberMean ~ tx_code, data = PC1_less,
        xlab = "Group", ylab = expression(paste(Delta,'MeanFiber'['post-pre'])),
        main = "PC1(-)", size=4)
#dev.copy(png, 'Downloads/win/pc1_less_tx_code_collapsed.jpg')
#dev.off() 


# Create Boxplot
boxplot(fiberMean ~ tx_code, data = PC2_great,
        xlab = "Group", ylab = expression(paste(Delta,'MeanFiber'['post-pre'])),
        main = "PC2(+)")
#dev.copy(png, '~/Google Drive/WIN_Matter/analysis/output/images/pc2_great_tx_code_collapsed.jpg')
#dev.off()
# Create Boxplot
boxplot(fiberMean ~ tx_code, data = PC2_less,
        xlab = "Group", ylab = expression(paste(Delta,'MeanFiber'['post-pre'])),
        main = "PC2(-)")
dev.copy(png, 'Downloads/win/pc_tx_code_collapsed.jpg')
dev.off()




