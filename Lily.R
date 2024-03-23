# read in data
Winterswijk_LA <- read.csv(file = "DataThesis.csv",
                           header = T,
                           sep = ";")
# change rownames

rownames(Winterswijk_LA) <- Winterswijk_LA[,1]
Winterswijk_LA <- Winterswijk_LA[,-1]

# remove standards
Winterswijk_WS<-Winterswijk_LA[!startsWith(rownames(Winterswijk_LA), "N6"),]

#make all variables numericals
Winterswijk_WS[,5:17]<-apply(Winterswijk_WS[,5:17],2,function(x) gsub("^<","\\1",x))
Winterswijk_WS[,5:17]<-apply(Winterswijk_WS[,5:17],2,as.numeric)

#calculate concentrations
Winterswijk_WS$Ca<-Winterswijk_WS$Ca43+Winterswijk_WS$Ca44
Winterswijk_WS$Sr<-Winterswijk_WS$Sr87+Winterswijk_WS$Sr88
Winterswijk_WS$Ba<-Winterswijk_WS$Ba137+Winterswijk_WS$Ba138
Winterswijk_WS<-Winterswijk_WS[,-c(5,6,12,13,15,16)]

# calculate ratios of elements
Winterswijk_WS$Sr.Ca<-Winterswijk_WS$Sr/Winterswijk_WS$Ca
Winterswijk_WS$Ba.Ca<-Winterswijk_WS$Ba/Winterswijk_WS$Ca

# make variables factors
Winterswijk_WS$Genus<-as.factor(Winterswijk_WS$Genus)
Winterswijk_WS$Layer<-as.factor(Winterswijk_WS$Layer)
Winterswijk_WS$Specimen<-as.factor(Winterswijk_WS$Specimen)

library(RColorBrewer)

Winterswijk_Complete <- Winterswijk_WS
Winterswijk_Complete$Genus <- droplevels(Winterswijk_Complete$Genus)

#### Scatterplot of all data ####

# Asign taxa names to colors
Genus_colors <- setNames(brewer.pal(length(levels(Winterswijk_Complete$Genus)), "RdBu"), levels(Winterswijk_Complete$Genus))

plot(Winterswijk_Complete$Sr.Ca,Winterswijk_Complete$Ba.Ca,
     xlab="Sr/Ca", ylab="Ba/Ca",
     col=Genus_colors[Winterswijk_Complete$Genus],
     pch=c(16, 17, 21, 22, 23)[Winterswijk_Complete$Genus],
     bg=Genus_colors[Winterswijk_Complete$Genus])

text(Winterswijk_Complete$Sr.Ca,
     Winterswijk_Complete$Ba.Ca,
     labels = Winterswijk_Complete$Specimen,
     pos = 4,
     cex=0.6)

legend(x="topright",
       title = "Genera",
       legend=c("Birgeria","Colobodus","Gyrolepis","Nothosaurus","Saurichthys"),
       col=Genus_colors,
       pch=c(16, 17, 21, 22, 23),
       pt.bg = Genus_colors,
       cex=0.7,
       pt.cex = 0.8)
title("Samples by genus",
      font.main=4, 
      adj=1, 
      cex.main=1)

#### Scatterplot by layers #### 

Winterswijk_Complete$Layer <- droplevels(Winterswijk_Complete$Layer)

# Asign taxa names to colors
Layer_colors <- setNames(brewer.pal(length(levels(Winterswijk_Complete$Layer)), "RdBu"), levels(Winterswijk_Complete$Layer))

plot(Winterswijk_Complete$Sr.Ca,Winterswijk_Complete$Ba.Ca,
     xlab="Sr/Ca", ylab="Ba/Ca",
     col=Layer_colors[Winterswijk_Complete$Layer],
     pch=c(16, 17, 21, 22)[Winterswijk_Complete$Layer],
     bg=Layer_colors[Winterswijk_Complete$Layer])

text(Winterswijk_Complete$Sr.Ca,
     Winterswijk_Complete$Ba.Ca,
     labels = Winterswijk_Complete$Genus,
     pos = 4,
     cex=0.4)

legend(x="topleft",
       title = "Layer",
       legend=c("Layer 4","Layer 14","Layer 36","Layer 46"), 
       col=Layer_colors,
       pch=c(16, 17, 21, 22),
       pt.bg = Layer_colors,
       cex=0.7,
       pt.cex = 0.8)
title("Genera represented by layer",
      font.main=4, adj=1, cex.main=1)

#### Plot genera without Birgeria ####

# This naming scheme may get confusing

Winterswijk_Complete <- Winterswijk_Complete[which(Winterswijk_Complete$Specimen != 5),]
Winterswijk_Complete$Specimen <- droplevels(Winterswijk_Complete$Specimen)

plot(Winterswijk_Complete$Sr.Ca,Winterswijk_Complete$Ba.Ca,
     xlab="Sr/Ca", 
     ylab="Ba/Ca",
     col=Genus_colors[Winterswijk_Complete$Genus],
     pch=c(16, 17, 21, 22, 23)[Winterswijk_Complete$Genus],
     bg=Genus_colors[Winterswijk_Complete$Genus])

text(Winterswijk_Complete$Sr.Ca,
     Winterswijk_Complete$Ba.Ca,
     labels = Winterswijk_Complete$Specimen,
     pos = 4,
     cex=0.6)

legend(x="topright",
       title = "Genera",
       legend=c("Birgeria","Colobodus","Gyrolepis","Nothosaurus","Saurichthys"),
       col=Genus_colors,
       pch=c(16, 17, 21, 22, 23),
       pt.bg = Genus_colors,
       cex=0.7,
       pt.cex = 0.8)
title("Samples by genus",
      font.main=4, 
      adj=1, 
      cex.main=1)

#### Plot genera without Birgeria and the odd Colobodus ####

# This naming scheme may get confusing

Winterswijk_Complete <- Winterswijk_Complete[which(Winterswijk_Complete$Specimen != 5),]
Winterswijk_Complete$Specimen <- droplevels(Winterswijk_Complete$Specimen)
Winterswijk_Complete$Genus <- droplevels(Winterswijk_Complete$Genus)

plot(Winterswijk_Complete$Sr.Ca,Winterswijk_Complete$Ba.Ca,
     xlab="Sr/Ca", 
     ylab="Ba/Ca",
     ylim = c(0, 0.0007),
     col=Genus_colors[Winterswijk_Complete$Genus],
     pch=c(16, 17, 21, 22, 23)[Winterswijk_Complete$Genus],
     bg=Genus_colors[Winterswijk_Complete$Genus])

text(Winterswijk_Complete$Sr.Ca,
     Winterswijk_Complete$Ba.Ca,
     labels = Winterswijk_Complete$Specimen,
     pos = 4,
     cex=0.6)

legend(x="topright",
       title = "Genera",
       legend=c("Birgeria","Colobodus","Gyrolepis","Nothosaurus","Saurichthys"),
       col=Genus_colors,
       pch=c(16, 17, 21, 22, 23),
       pt.bg = Genus_colors,
       cex=0.7,
       pt.cex = 0.8)
title("Samples by genus",
      font.main=4, 
      adj=1, 
      cex.main=1)

#### Check if all is displayed correctly ####

boxplot(Sr.Ca ~ Genus,
        data = Winterswijk_Complete)
# This shows that L4colobodus2-20 is also probably measured incorrectly
