# Supplemental Code for “Chimpanzees communicate to coordinate a cultural practice"
# Zoë Goldsborough, Anne Marijke Schel & Edwin J.C. van Leeuwen (2021)

#setwd("~/Research/Projects/Grooming Handclasp (2019-)/GHC-initiation")

library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)

# open dataset with coded videos
GHC <- read.csv("Handclasp Datacoding MD_postIRR.csv", header = TRUE,  sep =";", row.names=NULL, stringsAsFactors = TRUE)
GHC$Date <- as.Date(GHC$Date, format = "%d/%m/%Y")

##### PROPORTION TABLE ######
### SIDE VIEW ###

### PH/MC
GHC_sidePM <- subset(GHC,GHC$View == "Side" & (GHC$Condition == "PH" |
                                                                GHC$Condition == "MC"))
GHC_sidePM <- droplevels.data.frame(GHC_sidePM)
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
# only include bouts of dyads that also have PH/MC
GHC_sideIRG <- subset(GHC, GHC$View == "Side" & GHC$Condition == "IRG")
GHC_sideIRG <- droplevels.data.frame(GHC_sideIRG)
GHC_sideIRG$Match <- GHC_sideIRG$Dyad %in% GHC_sidePM$Dyad
GHC_sideIRG_2 <- subset(GHC_sideIRG, GHC_sideIRG$Match == "TRUE")
GHC_sideIRG_2 <- droplevels.data.frame(GHC_sideIRG_2)

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
GHC_totalPM <- subset(GHC, GHC$Condition == "PH" | GHC$Condition == "MC")
GHC_totalPM <- droplevels.data.frame(GHC_totalPM)
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
GHC_totalIRG <- subset(GHC, GHC$Condition == "IRG")
GHC_totalIRG <- droplevels.data.frame(GHC_totalIRG)
GHC_totalIRG$Match <- GHC_totalIRG$Dyad %in% GHC_totalPM$Dyad
GHC_totalIRG_2 <- subset(GHC_totalIRG, GHC_totalIRG$Match == "TRUE")
GHC_totalIRG_2 <- droplevels.data.frame(GHC_totalIRG_2)

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
#### SIDE
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
ModelDI <- ModelD[!duplicated(ModelD[c("BoutID")]),]
ModelDI <- droplevels.data.frame(ModelDI)

# wilcox test on those with freq > 5 (and not in more than 15% of IRG)
wilcox.test(ModelDI$ElbowholdMC, ModelDI$`Elbow hold`, paired = TRUE)
wilcox.test(ModelDI$ElbowtouchMC, ModelDI$`Elbow touch`, paired = TRUE)
wilcox.test(ModelDI$HandgrabMC, ModelDI$`Hand Grab`, paired = TRUE)
wilcox.test(ModelDI$HandtouchMC, ModelDI$`Hand touch`, paired = TRUE)
wilcox.test(ModelDI$HeadmoveMC, ModelDI$`Head move`, paired = TRUE)
wilcox.test(ModelDI$HoldMC, ModelDI$Hold, paired = TRUE)

#enter p values from these analyses manually
p_side <-  c(0.0001057, 0.0002105, 0.03689, 0.01966,0.004478, 0.0003626)
round(p.adjust(p_side, method = "holm"), digits =3)

#### SIDE AND BACK
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
ModelD_total <- left_join(GHC, freqtotalW, by = "BoutID")
ModelDI_total <- ModelD_total[!duplicated(ModelD_total[c("BoutID")]),]
ModelDI_total <- droplevels.data.frame(ModelDI_total)

# wilcox test on those with freq > 5 (and not in more than 15% of IRG)
wilcox.test(ModelDI_total$ElbowholdMC, ModelDI_total$`Elbow hold`, paired = TRUE)
wilcox.test(ModelDI_total$ElbowtouchMC, ModelDI_total$`Elbow touch`, paired = TRUE)
wilcox.test(ModelDI_total$HandgrabMC, ModelDI_total$`Hand Grab`, paired = TRUE)
wilcox.test(ModelDI_total$HandtouchMC, ModelDI_total$`Hand touch`, paired = TRUE)
wilcox.test(ModelDI_total$HeadmoveMC, ModelDI_total$`Head move`, paired = TRUE)
wilcox.test(ModelDI_total$HoldMC, ModelDI_total$Hold, paired = TRUE)

#enter p values from these analyses manually
p_SB <-  c(2.702e-06, 7.17e-05, 0.005962, 0.01966, 0.0003832, 0.0001227)
round(p.adjust(p_SB, method = "holm"), digits =3)

#### CLASSIFY BOUTS #####
# Side
ModelDI <- mutate(ModelDI, type = 
                    ifelse(((ModelDI$`Elbow hold` > 0) | (ModelDI$`Hand Grab` > 0)), "Moulding", 
                           ifelse((((ModelDI$`Self scratch` > 0) | (ModelDI$Torso > 0) | 
                                      (ModelDI$Nosewipe >0) | (ModelDI$`Head touch` > 0) | (ModelDI$Raise > 0)) & 
                                     ((ModelDI$`Elbow hold` == 0) & (ModelDI$`Hand Grab` == 0) & (ModelDI$`Elbow touch` == 0) &
                                        (ModelDI$Hold == 0) & (ModelDI$`Hand touch` == 0) & 
                                        (ModelDI$`Head move` == 0))), "Synchrony", 
                                  ifelse((((ModelDI$`Elbow touch` > 0) | (ModelDI$Hold > 0) | 
                                             (ModelDI$`Hand touch` > 0) | (ModelDI$`Head move` > 0)) &                                              
                                            ((ModelDI$`Elbow hold` == 0) & (ModelDI$`Hand Grab` == 0))), "Communication", "Other"))))


ftable(ModelDI$type)

# Back
ModelDI_total <- mutate(ModelDI_total, type = 
                       ifelse(((ModelDI_total$`Elbow hold` > 0) | (ModelDI_total$`Hand Grab` > 0)), "Moulding", 
                              ifelse((((ModelDI_total$`Self scratch` > 0) | (ModelDI_total$Torso > 0) | 
                                         (ModelDI_total$Nosewipe >0) | (ModelDI_total$`Head touch` > 0) | (ModelDI_total$Raise > 0)) & 
                                        ((ModelDI_total$`Elbow hold` == 0) & (ModelDI_total$`Hand Grab` == 0) & (ModelDI_total$`Elbow touch` == 0) &
                                           (ModelDI_total$Hold == 0) & (ModelDI_total$`Hand touch` == 0) & 
                                           (ModelDI_total$`Head move` == 0))), "Synchrony", 
                                     ifelse((((ModelDI_total$`Elbow touch` > 0) | (ModelDI_total$Hold > 0) | 
                                                (ModelDI_total$`Hand touch` > 0) | (ModelDI_total$`Head move` > 0)) &                                              
                                               ((ModelDI_total$`Elbow hold` == 0) & (ModelDI_total$`Hand Grab` == 0))), "Communication", "Other"))))


ftable(ModelDI_total$type)

##### SUNBURSTS #####

require(sunburstR)
require(TraMineR)
require(d3r)
require(tibble)

cols <- c("#e0e2e3",
           "#a33b20",
           "#1a4829",
           "#fb9a99",
           "#399e5a",
           "#5abcb9",
           "#335c81",
           "#c2eabd",
           "#1f78b4",
           "#003049",
           "#9dcded")

#grey (handclasp), red (elbowhold), dark green (elbowtouch), pink (handgrab)
#green (handtouch), turquoise (headmove), blue (headtouch), lightgreen (hold),
#lightblue (nosewipe), very dark blue (selfscratch), lightest blue (torso)

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
sun <- read.csv("Sunburst 2A_postIRR.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

#make variable for when initiator matches ID1
sun$In <- ifelse(sun$Initiator == sun$ID1, 1, 0)
#subset
in_sun <- subset(sun, sun$In == 1) 

# How many bouts and how many dyads
length(unique(in_sun$BoutID))
length(unique(in_sun$Dyad))

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
                                  in_sunR$Behavior == "hold" | in_sunR$Behavior == "headmove")

# only look at first behavior each individual did
in_sunRcomm2 <- in_sunRcomm[!duplicated(in_sunRcomm$BoutID),]
# overview of initiators and which behavior they began initiation with
ftable(in_sunRcomm2$Initiator, in_sunRcomm2$Behavior)

# binomial test
# 14 out of 15 individuals show variation in starting behavior
binom.test(x=14, n=15)