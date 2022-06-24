# Supplemental Code for “Chimpanzees communicate to coordinate a cultural practice"
# Zoë Goldsborough, Anne Marijke Schel & Edwin J.C. van Leeuwen (2022)

# packages needed
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(brms)
library(gridExtra)

# open dataset with coded videos
GHC <- read.csv("Handclasp Datacoding MD_postIRR.csv", header = TRUE,  sep =",", row.names=NULL, stringsAsFactors = TRUE)
GHC$Date <- as.Date(GHC$Date, format = "%d/%m/%Y")

##### PROPORTION TABLE ######
### SIDE VIEW ###

### PH/MC
GHC_sidePM <- droplevels.data.frame(subset(GHC,GHC$View == "Side" & (GHC$Condition == "PH" |
                                                                GHC$Condition == "MC")))

aggregate(data = GHC_sidePM, BoutID ~ Condition, function(x) length(unique(x)))

prop_side <- as.data.frame(as.matrix(ftable(GHC_sidePM$Behavior, GHC_sidePM$Condition)))
# remove behaviors that occurred less than <5 times and raise (because coded in every bout)
prop_side <- prop_side[c("Elbow hold", "Elbow touch", "Groom face", "Groom hand/arm", "Groom other", 
                       "Hand Grab", "Hand touch", "Head move", "Head touch", "Hold", "Nosewipe",
                        "Self scratch", "Torso"),]

# calculate rates
prop_side$PHrate <- round(prop_side$PH/94, 2) # 94 is nr of bouts
prop_side$MCrate <- round(prop_side$MC/94, 2)
prop_side <- tibble::rownames_to_column(prop_side, var = "Behavior")

### Initation regular  grooming bouts (IRG)
GHC_sideIRG <- droplevels.data.frame(subset(GHC, GHC$View == "Side" & GHC$Condition == "IRG"))
# only include bouts of dyads that also have PH/MC
GHC_sideIRG$Match <- GHC_sideIRG$Dyad %in% GHC_sidePM$Dyad
GHC_sideIRG_2 <- droplevels.data.frame(subset(GHC_sideIRG, GHC_sideIRG$Match == "TRUE"))

# nr of bouts
length(unique(GHC_sideIRG_2$BoutID))

prop_sideIRG <- as.data.frame(as.matrix(ftable(GHC_sideIRG_2$Behavior, GHC_sideIRG_2$Condition)))
prop_sideIRG$IRGrate <- round(prop_sideIRG$IRG/17, 2)
prop_sideIRG <- tibble::rownames_to_column(prop_sideIRG, var = "Behavior")

prop_sideFull <- left_join(prop_side, prop_sideIRG, by = "Behavior")
# replace NA with 0
prop_sideFull[is.na(prop_sideFull)] <- 0
# change order of columns for easy reading
prop_sideFull[, c(1,3,4,2,5,6,7)]

### SIDE AND BACK VIEW ###
### PH/MC
GHC_totalPM <- droplevels.data.frame(subset(GHC, GHC$Condition == "PH" | GHC$Condition == "MC"))
aggregate(data = GHC_totalPM, BoutID ~ Condition, function(x) length(unique(x)))

prop_total <- as.data.frame(as.matrix(ftable(GHC_totalPM$Behavior, GHC_totalPM$Condition)))
# remove behaviors that occurred less than <5 times and raise (because coded in every bout)
prop_total <- prop_total[c("Elbow hold", "Elbow touch", "Groom face", "Groom hand/arm", "Groom other", 
                   "Hand Grab", "Hand touch", "Head move", "Head touch", "Hold", "Nosewipe",
                   "Self scratch", "Torso"),]
# calculate rates
prop_total$PHrate <- round(prop_total$PH/133, 2) # 133 is nr of bouts
prop_total$MCrate <- round(prop_total$MC/133, 2)
prop_total <- tibble::rownames_to_column(prop_total, var = "Behavior")

### Initation regular  grooming bouts (IRG)
# only include bouts of dyads that also have PH/MC
GHC_totalIRG <- droplevels.data.frame(subset(GHC, GHC$Condition == "IRG"))
GHC_totalIRG$Match <- GHC_totalIRG$Dyad %in% GHC_totalPM$Dyad
GHC_totalIRG_2 <- droplevels.data.frame(subset(GHC_totalIRG, GHC_totalIRG$Match == "TRUE"))

# nr of bouts
length(unique(GHC_totalIRG_2$BoutID))

prop_totalIRG <- as.data.frame(as.matrix(ftable(GHC_totalIRG_2$Behavior, GHC_totalIRG_2$Condition)))
prop_totalIRG$IRGrate <- round(prop_totalIRG$IRG/23, 2)
prop_totalIRG <- tibble::rownames_to_column(prop_totalIRG, var = "Behavior")

prop_totalFull <- left_join(prop_total, prop_totalIRG, by = "Behavior")
# replace NA with 0
prop_totalFull[is.na(prop_totalFull)] <- 0
# change order of columns
prop_totalFull[, c(1,3,4,2,5,6,7)]

##### PH/MC COMPARISON #####
### SIDE ###
GHC_sideP <- subset(GHC_sidePM, GHC_sidePM$Condition == "PH")
freqPH <- as.data.frame(as.matrix(ftable(GHC_sideP$BoutID, GHC_sideP$Behavior)))
freqPH <- tibble::rownames_to_column(freqPH, "BoutID")

GHC_sideM <- subset(GHC_sidePM, GHC_sidePM$Condition == "MC")
freqMC <- as.data.frame(as.matrix(ftable(GHC_sideM$BoutID, GHC_sideM$Behavior)))
freqMC <- tibble::rownames_to_column(freqMC, "BoutID")
freqMC2 <- freqMC  
  
colnames(freqMC2) <- c("BoutID", "BentMC", "ElbowholdMC", "ElbowtouchMC", "GroomarmpitMC", "GroomfaceMC", "GroomHAMC",
                  "GroomotherMC", "HandgrabMC", "HandtouchMC", "HeadmoveMC", "HeadshakeMC", "HeadtouchMC", "HoldMC",
                  "LeanMC", "MoveArmpitMC", "NosewipeMC", "RaiseMC", "SelfscratchMC", "TorsoMC", "TouchotherMC")

freqW <- left_join(freqMC2, freqPH, "BoutID")

freqW$BoutID <- as.integer(freqW$BoutID)
ModelD <- left_join(GHC_sidePM, freqW, by = "BoutID")
ModelDI <- droplevels.data.frame(ModelD[!duplicated(ModelD[c("BoutID")]),])

# wilcox test on those with freq > 5 (self-scratch, nose-wipe and torso excluded for theoretical reasons, see ms)
wilcox.test(ModelDI$ElbowholdMC, ModelDI$`Elbow hold`, paired = TRUE)
wilcox.test(ModelDI$ElbowtouchMC, ModelDI$`Elbow touch`, paired = TRUE)
wilcox.test(ModelDI$HandgrabMC, ModelDI$`Hand Grab`, paired = TRUE)
wilcox.test(ModelDI$HandtouchMC, ModelDI$`Hand touch`, paired = TRUE)
wilcox.test(ModelDI$HeadmoveMC, ModelDI$`Head move`, paired = TRUE)
wilcox.test(ModelDI$HoldMC, ModelDI$Hold, paired = TRUE)
wilcox.test(ModelDI$HeadtouchMC, ModelDI$`Head touch`, paired = TRUE)

#enter p values from these analyses manually
p_side <-  c(0.0001057, 0.0002105, 0.03689, 0.01966,0.004478, 0.0003626, 0.008457)
round(p.adjust(p_side, method = "holm"), digits =3)

### SIDE AND BACK ###
GHC_totalP <- subset(GHC, GHC$Condition == "PH")
freqtotalPH <- as.data.frame(as.matrix(ftable(GHC_totalP$BoutID, GHC_totalP$Behavior)))
freqtotalPH <- tibble::rownames_to_column(freqtotalPH, "BoutID")

GHC_totalM <- subset(GHC, GHC$Condition == "MC")
freqtotalMC <- as.data.frame(as.matrix(ftable(GHC_totalM$BoutID, GHC_totalM$Behavior)))
freqtotalMC <- tibble::rownames_to_column(freqtotalMC, "BoutID")
freqtotalMC_2 <- freqtotalMC 

colnames(freqtotalMC_2) <- c("BoutID", "BentMC", "ElbowholdMC", "ElbowtouchMC", "GroomarmpitMC", "GroomfaceMC", "GroomHAMC",
                       "GroomotherMC", "HandgrabMC", "HandtouchMC", "HeadmoveMC", "HeadshakeMC", "HeadtouchMC", "HoldMC",
                       "LeanMC", "MoveArmpitMC", "NosewipeMC", "RaiseMC", "SelfscratchMC", "TorsoMC", "TouchotherMC")

freqtotalW <- left_join(freqtotalMC_2, freqtotalPH, "BoutID")

freqtotalW$BoutID <- as.integer(freqtotalW$BoutID)
ModelD_total <- left_join(GHC_totalPM, freqtotalW, by = "BoutID")
ModelDI_total <- droplevels.data.frame(ModelD_total[!duplicated(ModelD_total[c("BoutID")]),])
ModelDI_total <- ModelDI_total[ModelDI_total$Condition == "PH",]

# wilcox test on those with freq > 5 (self-scratch, nose-wipe and torso excluded for theoretical reasons, see ms)
wilcox.test(ModelDI_total$ElbowholdMC, ModelDI_total$`Elbow hold`, paired = TRUE)
wilcox.test(ModelDI_total$ElbowtouchMC, ModelDI_total$`Elbow touch`, paired = TRUE)
wilcox.test(ModelDI_total$HandgrabMC, ModelDI_total$`Hand Grab`, paired = TRUE)
wilcox.test(ModelDI_total$HandtouchMC, ModelDI_total$`Hand touch`, paired = TRUE)
wilcox.test(ModelDI_total$HeadmoveMC, ModelDI_total$`Head move`, paired = TRUE)
wilcox.test(ModelDI_total$HoldMC, ModelDI_total$Hold, paired = TRUE)
wilcox.test(ModelDI_total$HeadtouchMC, ModelDI_total$`Head touch`, paired = TRUE)

#enter p values from these analyses manually
p_SB <-  c(4.548e-06, 7.17e-05, 0.005962, 0.01966, 0.0003832, 0.0001227, 0.001158)
round(p.adjust(p_SB, method = "holm"), digits =3)

## some information on the samples
# amount of dyads and unique individuals per sample
length(unique(ModelDI$Dyad))
length(unique(ModelDI_total$Dyad))

# amount of bouts an individual is involved in (to show skew of sample)
md <- merge(as.data.frame(ftable(ModelDI$Initiator)), as.data.frame(ftable(ModelDI$Partner)), all = TRUE, by = "Var1")
md[is.na(md)] <- 0
md$Total <- md$Freq.x + md$Freq.y
mean(md$Total)

# amount of bouts an individual is involved in (to show skew of sample)
mdt <- merge(as.data.frame(ftable(ModelDI_total$Initiator)), as.data.frame(ftable(ModelDI_total$Partner)), all = TRUE, by = "Var1")
mdt[is.na(mdt)] <- 0
mdt$Total <- mdt$Freq.x + mdt$Freq.y
mean(mdt$Total)

#### CLASSIFY BOUTS #####
# Classify initiation strategy into Shaping, Communication or Synchrony based on which behaviors were present

### SIDE ###
ModelDI <- mutate(ModelDI, type = 
                    ifelse(((ModelDI$`Elbow hold` > 0) | (ModelDI$`Hand Grab` > 0)), "Shaping", 
                           ifelse((((ModelDI$`Self scratch` > 0) | (ModelDI$Torso > 0) | 
                                      (ModelDI$Nosewipe >0) | (ModelDI$Raise > 0)) & 
                                     ((ModelDI$`Elbow hold` == 0) & (ModelDI$`Hand Grab` == 0) & (ModelDI$`Elbow touch` == 0) &
                                        (ModelDI$Hold == 0) & (ModelDI$`Hand touch` == 0) & 
                                        (ModelDI$`Head move` == 0) & (ModelDI$`Head touch` == 0))), "Synchrony", 
                                  ifelse((((ModelDI$`Elbow touch` > 0) | (ModelDI$Hold > 0) | 
                                             (ModelDI$`Hand touch` > 0) | (ModelDI$`Head touch` > 0) | (ModelDI$`Head move` > 0)) &                                              
                                            ((ModelDI$`Elbow hold` == 0) & (ModelDI$`Hand Grab` == 0))), "Communication", "Other"))))


ftable(ModelDI$type)

### SIDE AND BACK ###
ModelDI_total <- mutate(ModelDI_total, type = 
                       ifelse(((ModelDI_total$`Elbow hold` > 0) | (ModelDI_total$`Hand Grab` > 0)), "Shaping", 
                              ifelse((((ModelDI_total$`Self scratch` > 0) | (ModelDI_total$Torso > 0) | 
                                         (ModelDI_total$Nosewipe >0) |  (ModelDI_total$Raise > 0)) & 
                                        ((ModelDI_total$`Elbow hold` == 0) & (ModelDI_total$`Hand Grab` == 0) & (ModelDI_total$`Elbow touch` == 0) &
                                           (ModelDI_total$Hold == 0) & (ModelDI_total$`Hand touch` == 0) & 
                                           (ModelDI_total$`Head move` == 0) & (ModelDI_total$`Head touch` == 0))), "Synchrony", 
                                     ifelse((((ModelDI_total$`Elbow touch` > 0) | (ModelDI_total$Hold > 0) | 
                                                (ModelDI_total$`Hand touch` > 0) | (ModelDI_total$`Head touch` > 0) | (ModelDI_total$`Head move` > 0)) &                                              
                                               ((ModelDI_total$`Elbow hold` == 0) & (ModelDI_total$`Hand Grab` == 0))), "Communication", "Other"))))


ftable(ModelDI_total$type)

##### ADDITIONAL DYAD INFORMATION ####
InfoAll <- read.csv("Information for CMS Model Handclasp.csv", header = TRUE, sep = ";")
# Kin = Whether dyad is matrilineal kin yes or no
# Kin2 = Whether dyad is mother-offspring dyad

# Days since first handclasp
InfoAll$FirstGHC <- as.Date(InfoAll$FirstGHC, format = "%d/%m/%Y")
FieldDate <- as.Date("2019-09-01", format = "%Y-%m-%d")
InfoAll$DaysEx <- (FieldDate - InfoAll$FirstGHC)

##### INITIATION STRATEGY #####
# factors influencing whether they initiate via Shaping/Communication/Synchrony

## Add dyad information to frequency dataset with type
ModelDI_2 <- left_join(ModelDI, InfoAll, "Dyad") # only side bouts
ModelDI_2all <- left_join(ModelDI_total, InfoAll, "Dyad") # side and back

# Outcome is the type of initiation, with "Shaping" as reference level
ModelDI_2$Type <- relevel(as.factor(ModelDI_2$type), ref = 2)

# Random effects will be Dyad and Initiator identity, both need to be factors
ModelDI_2$DyadF <- as.factor(ModelDI_2$Dyad)
ModelDI_2$Initiator <- as.factor(ModelDI_2$Initiator)

## Predictors
# Days of Experience (days since first handclasp) 
# want to standardize DaysEx, where 0 is the lowest value and 1 the highest
ModelDI_2$zDaysEx <- (as.numeric(ModelDI_2$DaysEx)-min(as.numeric(ModelDI_2$DaysEx)))/(max(as.numeric(ModelDI_2$DaysEx))-min(as.numeric(ModelDI_2$DaysEx)))
# Whether Dyad is a mother-offspring Dyad yes or no
ModelDI_2$Kin2F <- as.factor(ModelDI_2$Kin2)

# strip dataframe down to essential information
ModelDI_2s <- ModelDI_2[,c("DyadF", "Initiator", "Type", "View", "zDaysEx", "Kin2F")]
head(ModelDI_2)

## Model: Type  depending on Days of Experience and Mother-Offspring dyad yes/no, with Dyad and Initiator ID as random effects
# Type ~ zDaysEx + Kin2F + (1|DyadF) + (1|Initiator)

# Set priors to weakly regularizing prior
# normal(0,1) for estimates and exponential(1) for standard deviations
bm_prior <- c(prior(normal(0,1), class = b, dpar = muCommunication), 
               prior(normal(0,1), class = b, dpar = muSynchrony), 
               prior(exponential(1), class = sd, dpar = muCommunication), 
               prior(exponential(1), class = sd, dpar = muSynchrony))

# prior predictive simulation
bm_p <- brm(Type ~ zDaysEx + Kin2F + (1|DyadF) + (1|Initiator), data=ModelDI_2s, family="categorical", chains=4, cores = 4, 
             warmup = 1500, iter = 3000, control = list(adapt_delta = 0.99), prior = bm_prior, sample_prior = "only", seed = 1222)
# to save output
# saveRDS(bm_p, file = "bm_p.rds")
# to read output
# bm_p <- readRDS("bm_p.rds")

# visualize priors
prior_summary(bm_p) 
pp_check(bm_p, ndraws = 100)
summary(bm_p)
plot(bm_p) # see that they all are centered on 0, so normal distributions

# Compare prior to posterior to see if model estimated a difference
# Run model to get posterior
bm <- brm(Type ~ zDaysEx + Kin2F + (1|DyadF) + (1|Initiator), data=ModelDI_2s, family="categorical", chains=4, cores = 4, 
             warmup = 5000, iter = 10000, control = list(adapt_delta = 0.99), prior = bm_prior, seed = 1222)
# to save output
# saveRDS(bm, file = "bm.rds")
# to read output
# bm <- readRDS("bm.rds")

summary(bm)

# checking model
VarCorr(bm)
cov2cor(vcov(bm))
pairs(bm)
pp_check(bm, type = "dens_overlay", ndraws = 100)

# hypothesis testing
h1 <- hypothesis(bm, c("muCommunication_zDaysEx > 0", "muCommunication_zDaysEx <0" ))
print(h1,digits = 3)
h2 <- hypothesis(bm, c("muCommunication_Kin2FYes < 0", "muCommunication_Kin2FYes > 0"))
print(h2, digits = 3)

# Plotting
# posterior distributions
plot(bm)
# to export, uncomment the png or setEPS command and the dev.off command
# png("Brm_posterior.png", width = 8, height = 9, units = 'in', res = 300)
# setEPS(postscript(file = "Brm_posterior.eps", width = 8, height = 9))
brms::mcmc_plot(bm, type = "areas", prob = 0.8, prob_outer = 0.99) + labs(title = "Posterior distributions", subtitle = "with medians and 80% intervals") +
  ggplot2::scale_y_discrete(labels = c("b_Communication_Intercept", "b_Synchrony_Intercept", "b_Communication_DaysExperience", "b_Communication_Mother-Offspring(Yes)",
                                       "b_Synchrony_DaysExperience", "b_Synchrony_Mother-Offspring(Yes)", "sd_Dyad_Communication_Intercept", "sd_Initiator_Communication_Intercept",
                                       "sd_Dyad_Synchrony_Intercept", "sd_Initiator_Synchrony_Intercept"))
# dev.off()

# conditional effects
Exp_bm <- conditional_effects(bm, effects = "zDaysEx", categorical = TRUE, prob = 0.95)
Kin_bm <- conditional_effects(bm, effects = "Kin2F", categorical = TRUE, prob = 0.95)

p1 <- plot(Exp_bm, plot = FALSE)[[1]] +
       theme_bw() + theme(text = element_text(size = 20)) + labs(x = "Days Experience (Standardized)", y = "Probability of Initiation Type") + ggtitle("A.")

p2 <- plot(Kin_bm, plot = FALSE)[[1]] +
  theme_bw()+ theme(text = element_text(size = 20))  + labs(x = "Mother-Offspring Dyad", y = "Probability of Initiation Type") + ggtitle("B.") 

# to export, uncomment the png or setEPS command and the dev.off command
# png("Brm_effects.png", width = 14, height = 7, units = 'in', res = 300)
#setEPS(postscript(file = "Brm_effects.eps", width = 14, height = 7))
grid.arrange(p1, p2, nrow = 1)
#dev.off()

##### SUNBURSTS #####

require(sunburstR)
require(TraMineR)
require(d3r)
require(tibble)
require(colorBlindness)

cols <- c("#e0e2e3",  # handclasp
           "#990F0F", # elbowhold
           "#00BB00", # elbow touch
           "#E57E7E", # handgrab
           "#008600", # handtouch
           "#009292", # headmove
           "#005000", # headtouch
           "#BBFFBB", # hold
           "#0F6B99", # nosewipe
           "#51A3CC", # self scratch
           "#B2E5FF") # torso

#grey (handclasp), red (elbowhold), dark green (elbowtouch), pink (handgrab)
#green (handtouch), blue (headmove), middle green (headtouch), lightgreen (hold),
#lightblue (nosewipe), very dark blue (selfscratch), lightest blue (torso)

# check for colorblindness compatibility
displayAllColors(cols, color = "white")

#function to be able to suppress the NA's
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}

#make sure it doesn't round to the nearest 10
custom.message = "function (d) {
  root = d;
  while (root.parent) {
    root = root.parent
  }
  p = (100*d.value/root.value).toPrecision(3);
  msg = p+' %<br/>'+d.value+' of '+root.value;
  return msg;
}"

### SUNBURST FIG 2A
sun <- read.csv("Sunburst 2A_postIRR.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

#make variable for when initiator matches ID1
sun$In <- ifelse(sun$Initiator == sun$ID1, 1, 0)
#subset
in_sun <- subset(sun, sun$In == 1) 

# How many bouts and how many dyads
length(unique(in_sun$BoutID))
length(unique(in_sun$Dyad))

md_sun <- in_sun[, c("Dyad", "BoutID")]
md_sun <- md_sun[!duplicated(md_sun$BoutID),]
md_sun2 <- cbind(md_sun, colsplit(md_sun$Dyad, "-", names = c("ID1", "ID2")))

md_sunt <- merge(x = as.data.frame(ftable(md_sun2$ID1)), y = as.data.frame(ftable(md_sun2$ID2)), all = TRUE, by = "Var1")
md_sunt[is.na(md_sunt)] <- 0
md_sunt$Total <- md_sunt$Freq.x + md_sunt$Freq.y
mean(md_sunt$Total)

#steps to make sunburst
in_sun$nr <- ave(in_sun$BoutID, in_sun$BoutID, FUN = seq_along)
in_sun$BoutID <- as.factor(in_sun$BoutID)

in_sunC <- dcast(in_sun, BoutID ~ nr, value.var = "Behavior")

in_sunC$First <- in_sunC$`1`
in_sunC$Second <- in_sunC$`2`
in_sunC$Third <- in_sunC$`3`
in_sunC$Fourth <- in_sunC$`4`

in_sunC$sequence <- with(in_sunC, paste5(First, Second, Third, Fourth, "handclasp", sep = "-", na.rm = T))

sunburst(data.frame(table(in_sunC$sequence)), colors = cols, explanation = custom.message)

### FLEXIBILITY OF INITIATION

# subset individuals who initiated more than one bout
in_sun$In_index <- as.integer(as.factor(in_sun$Initiator))
Initiators <- as.data.frame(sort(unique(in_sun$Initiator)))
Initiators$ID <- as.factor(Initiators$`sort(unique(in_sun$Initiator))`)
Initiators$ID_index <- as.integer(Initiators$ID)
Initiators$UniqueBout <- NA

for (i in 1:max(Initiators$ID_index)) {
  Initiators[i,4] = length(unique(in_sun$BoutID[in_sun$In_index == i]))
} 

in_repeat <- Initiators[Initiators$UniqueBout > 1,]
# select these individuals from larger initiation dataset
in_sun$Repeat <-  in_sun$In_index %in% in_repeat$ID_index
in_sunR <- subset(in_sun, in_sun$Repeat == TRUE)
# subset only the behaviors that were specific to GHC initiation (so prevalent in PH)
in_sunRcomm <- subset(in_sunR, in_sunR$Behavior == "elbowtouch" | in_sunR$Behavior == "elbowhold" | in_sunR$Behavior == "handtouch" | in_sunR$Behavior == "handgrab" | 
                                  in_sunR$Behavior == "hold" | in_sunR$Behavior == "headmove" | in_sunR$Behavior == "headtouch")

# only look at first behavior each individual did
in_sunRcomm2 <- in_sunRcomm[!duplicated(in_sunRcomm$BoutID),]
# overview of initiators and which behavior they began initiation with
ftable(in_sunRcomm2$Initiator, in_sunRcomm2$Behavior)

# binomial test
# 14 out of 15 individuals show variation in starting behavior
binom.test(x=14, n=15)
