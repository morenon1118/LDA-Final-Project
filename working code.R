back <- read.table("/Users/Tervs/Downloads/backpain.txt")
colnames(back) <- c("id", 
                   "trt", 
                   "vrs.1", 
                   "vas.1", 
                   "anx.1", 
                   "alrt.1", 
                   "time.trt.1", 
                   "vrs.2", 
                   "vas.2", 
                   "anx.2", 
                   "alrt.2", 
                   "time.trt.2", 
                   "vrs.3", 
                   "vas.3", 
                   "anx.3", 
                   "alrt.3", 
                   "time.trt.3", 
                   "vrs.4", 
                   "vas.4", 
                   "anx.4", 
                   "alrt.4", 
                   "time.trt.4")


### 1. EDA/Data Restructuring ###

#Separate responses, reshape each into long

#Verbal Analog Scale (percentage)
vas <- back[, c(1, 2, 4, 9, 14, 19)]
vas.l <- reshape(vas, 
                 varying = c("vas.1", "vas.2", "vas.3", "vas.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")

vrs <- back[, c(1, 2, 3, 8, 13, 18)]
vrs.l <- reshape(vrs, 
                 varying = c("vrs.1", "vrs.2", "vrs.3", "vrs.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")

anxiety <- back[, c(1, 2, 5, 10, 15, 20)]
anx.l <- reshape(anxiety, 
                 varying = c("anx.1", "anx.2", "anx.3", "anx.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")

alert <- back[, c(1, 2, 6, 11, 16, 21)]
alert.l <- reshape(alert, 
                 varying = c("alrt.1", "alrt.2", "alrt.3", "alrt.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")

#