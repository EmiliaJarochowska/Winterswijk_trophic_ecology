# Read in the matrix with detection limits

dl <- read.csv(file = "data/original/250404_Lily_detectionlimit.csv",
               header = T,
               sep = ";")
rownames(dl) <- dl$Element
dl <- dl[,-1]

# When '<' is used, take the value after '<' as the value in the cell - questionable choice!
dl[,1:57]<-apply(dl[,1:57],2,function(x) gsub("^<","\\1",x))
dl[,1:57]<-apply(dl[,1:57],2,as.numeric)

measurements <- read.csv(file = "data/original/250404_Lily_data.csv",
                header = T,
                sep = ";")
rownames(measurements) <- measurements$Element
measurements <- measurements[,-1]
# When '<' is used, take the value after '<' as the value in the cell - questionable choice!
measurements[,1:57]<-apply(measurements[,1:57],2,function(x) gsub("^<","\\1",x))
measurements[,1:57]<-apply(measurements[,1:57],2,as.numeric)

source("code/helper_functions/detection_limit.R")

concentrations <- detection_limit(measurements_matrix = measurements, dl_matrix = dl)
