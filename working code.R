library(nlme)
library(geepack)
library(lme4)
library(ordinal)
library(mclogit)
library(dplyr)

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
vrs.1 <- vrs.l[vrs.l$trt == 1, ]
vrs.2 <- vrs.l[vrs.l$trt == 2, ]

vrs.1 %>% filter(visit == 1 | visit == 2 | visit == 3 | visit == 4) %>% group_by(visit) %>% summarise(mean = mean(score))
vrs.2 %>% filter(visit == 1 | visit == 2 | visit == 3 | visit == 4) %>% group_by(visit) %>% summarise(mean = mean(score))

vas.1 <- vas.l[vas.l$trt == 1, ]
vas.2 <- vas.l[vas.l$trt == 2, ]

vas.1 %>% filter(visit == 1 | visit == 2 | visit == 3 | visit == 4) %>% group_by(visit) %>% summarise(mean = mean(score))
vas.2 %>% filter(visit == 1 | visit == 2 | visit == 3 | visit == 4) %>% group_by(visit) %>% summarise(mean = mean(score))

#Separate responses, reshape each into long

#Verbal Analog Scale (percentage)
vas <- back[, c(1, 2, 4, 9, 14, 19)]
vas.l <- reshape(vas, 
                 varying = c("vas.1", "vas.2", "vas.3", "vas.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")
vas.l$trt <- as.factor(vas.l$trt)
vas.l$visit <- as.factor(vas.l$visit)

vrs <- back[, c(1, 2, 3, 8, 13, 18)]
vrs.l <- reshape(vrs, 
                 varying = c("vrs.1", "vrs.2", "vrs.3", "vrs.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")
vrs.l$trt <- as.factor(vrs.l$trt)
vrs.l$visit <- as.factor(vrs.l$visit)

anxiety <- back[, c(1, 2, 5, 10, 15, 20)]
anx.l <- reshape(anxiety, 
                 varying = c("anx.1", "anx.2", "anx.3", "anx.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")
anx.l$trt <- as.factor(anx.l$trt)
anx.l$visit <- as.factor(anx.l$visit)

alert <- back[, c(1, 2, 6, 11, 16, 21)]
alert.l <- reshape(alert, 
                 varying = c("alrt.1", "alrt.2", "alrt.3", "alrt.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")
alert.l$trt <- as.factor(alert.l$trt)
alert.l$visit <- as.factor(alert.l$visit)

# 2. Tackling Missing Data  

# patient 8 missing responses from second visit - NON-MONOTONE DROPOUT
### For 8, if this patient does not differ from the other patients in any way, safe to "drop" patient. 
### Without background of patients, cannot truly tell if patient 8 differs, but lets assume they do not so we can drop.

# patient 20 missing vas, anx, and alrt response from first visit
# because we want to model both vas and vrs, let's deem 20 a non-monotone dropout as well.
back <- back[-c(8, 20), ]

vas <- back[, c(1, 2, 4, 9, 14, 19)]
vas.l <- reshape(vas, 
                 varying = c("vas.1", "vas.2", "vas.3", "vas.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")
#vas.l$id <- as.factor(vas.l$id)
vas.l$trt <- as.factor(vas.l$trt)
vas.l$visit <- as.factor(vas.l$visit)

vrs <- back[, c(1, 2, 3, 8, 13, 18)]
vrs.l <- reshape(vrs, 
                 varying = c("vrs.1", "vrs.2", "vrs.3", "vrs.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")
vrs.l$id <- as.factor(vrs.l$id)
vrs.l$trt <- as.factor(vrs.l$trt)
#vrs.l$visit <- as.factor(vrs.l$visit)

anxiety <- back[, c(1, 2, 5, 10, 15, 20)]
anx.l <- reshape(anxiety, 
                 varying = c("anx.1", "anx.2", "anx.3", "anx.4"), 
                 direction = "long", 
                 idvar = c("id", "trt"), 
                 v.names = "score", 
                 timevar = "visit")
anx.l$id <- as.factor(anx.l$id)
anx.l$trt <- as.factor(anx.l$trt)
anx.l$visit <- as.factor(anx.l$visit)

alert <- back[, c(1, 2, 6, 11, 16, 21)]
alert.l <- reshape(alert, 
                   varying = c("alrt.1", "alrt.2", "alrt.3", "alrt.4"), 
                   direction = "long", 
                   idvar = c("id", "trt"), 
                   v.names = "score", 
                   timevar = "visit")
alert.l$id <- as.factor(alert.l$id)
alert.l$trt <- as.factor(alert.l$trt)
alert.l$visit <- as.factor(alert.l$visit)

# ptient 15 missing anx response from second visit
# patient 21 missing anx and alrt response from first visit

#I want to focus on the pain responses, but anxiety response could yield interesting findings for the future. 

# 3. GEE Marginal Model  
#Multinominal logistic regression for descrete, non-binary, non-count response
attach(vas.l)
summary(lme(score ~ trt + visit, 
            random = ~ visit | id))


#Ordinal model for pain rating response from 1-5, assessing the changes in the odds of a better(lower) pain rating 
attach(vrs.l)
vrs.l$visit <- as.numeric(vrs.l$visit)
summary(ordgee(ordered(score) ~ trt + visit + trt:visit, 
              id = id, waves = visit, corstr = "independence"))

# In summary, our missing data being classified as MNAR yields GEE models invalid. GLMM Mixed effect models may be more suitable. 

# 4. GLMM Mixed Effect Model  

attach(vas.l)
summary(lme(score ~ trt + visit, 
            random = ~ visit | id))

#Let's model VRS with a GLMM for ordinal response data:
attach(vrs.l)
summary(clmm2(ordered(score) ~ trt + visit + trt:visit, 
             data = vrs.l,
             random = id,
             nAGQ = 50, 
             Hess = T))
# 5. Comparison of Models
# 6. Sample size and power discussion
mean(back$time.trt.4-back$time.trt.3)
mean(back$time.trt.3-back$time.trt.2)
mean(back$time.trt.2-back$time.trt.1)


# 7. Experimental Design discussion