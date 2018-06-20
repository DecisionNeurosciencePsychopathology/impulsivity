## Michelle's impulsivity analyses

##install packages on new machine
install.packages(c("readr", "lme4", "lmerTest", "ggplot2", "dplyr", "tidyr", "tibble", "Hmisc",
                   "nnet", "reshape2", "emmeans", "factoextra", "compareGroups", "effects", "VIM", "mice", "multcompView",
                   "readxl", "lsmeans", "corrplot", "stringi", "psych", "stringr"))

#setwd("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/")

#wd for desktop comp @ home
#setwd("/home/bluebird/Desktop")

#wd for UPMC desktop
setwd("C:/Users/perryma/Desktop")

#wd for Michelle laptop
#setwd("C:/Users/Michelle/Desktop")

library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
# library(ggbiplot)
library(corrplot)
library(emmeans)
library(factoextra)
# library(ggfortify)
library(compareGroups)
# library(RColorBrewer)
# library(MASS)
library(effects)
library(readr)
library(VIM)
library(mice)
library(multcompView)
library(readxl)
library(lsmeans)
library(psych)
library(corrplot)
library(stringi)
library(stringr)

#  read in data
#df <- read_excel("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

# Michelle desktop @ home
#df <- read_excel("/home/bluebird/Desktop/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

#Michelle UPMC desktop
df <- read_excel("C:/Users/perryma/Desktop/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

#Michelle laptop
#df <- read_excel("Impulsivity.updated.01-11-18.xlsx")

View(df)

df$ln_k <- log(df$MONEY)
# NB: there are some non-discounters in this database
df$ln_k_excluding_nondiscounters <- df$ln_k
nondiscounters <- df$ln_k < -10
df$ln_k_excluding_nondiscounters[nondiscounters] <- NA


df$GROUP12467 <- as.factor(df$GROUP12467)
df$GROUP1245 <- as.factor(df$GROUP1245)

# check missing data
missing_ind_chars = aggr(
  df,
  col = mdc(1:2),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(df),
  cex.axis = .7,
  gap = 3,
  ylab = c("Proportion of missingness", "Missingness Pattern")
)

# we don't have age at first attempt, but Michelle will add it to the df and calculate a new variable as follows:

df$group_early <- as.character(df$COMMENT)
df$group_early[df$group_early=="CONTROL"] <- "Non-psychiatric controls"
df$group_early[df$group_early=="DEPRESSION"] <- "Non-suicidal depressed"
df$group_early[df$group_early=="IDEATOR"] <- "Suicide ideators"
df$group_early[df$group_early=="DEPRESSION-IDEATOR"] <- "Suicide ideators"
df$group_early[df$`AGE AT FIRST ATTEMPT`<60] <- "Early-onset attempters"
df$group_early[df$`AGE AT FIRST ATTEMPT`>59] <- "Late-onset attempters"
df$group_early <- as.factor(df$group_early)
df$group_early = factor(df$group_early, levels(df$group_early)[c(3,4,5,1,2)])

test <- df[,c(8,44,45)]
View(test)

df <- df[,c(1:7,9:45,8)]

# recode the variable names
df$UPPS_negU <- df$`UPPSP NEG URGENCY`
df$UPPS_posU <- df$`UPPSP POS URGENCY`
df$UPPS_premed <- df$`UPPSP LACK OF PREMED`
df$UPPS_persev <- df$`UPPSP LACK OF PERSEV`

df$age_first_att <- df$`AGE AT FIRST ATTEMPT`

# just a prototype
chars <- as.data.frame(df[, c(5,10:16,23)])
c1 <-
  compareGroups(
    chars,
    y = df$group_early,
    bivar = TRUE,
    include.miss = FALSE
  )
t1 <-
  createTable(
    c1,
    hide = c(sex = "FEMALE", list(race = c(
      "WHITE", "ASIAN PACIFIC"
    ))),
    hide.no = 0,
    digits = 0,
    show.n = TRUE
  )
export2html(t1, "imp_chars_by_group.html")

# Michelle to check all histograms for herself
par(mfrow=c(3,3))
hist(df$SPSI_ICSSUB, breaks=8)
hist(df$BIS_COGNIT)
hist(df$BIS_MOTOR, breaks=6)
hist(df$BIS_NONPLAN, breaks=6)
hist(df$`UPPSP NEG URGENCY`, breaks=6)
hist(df$`UPPSP POS URGENCY`, breaks=4)
hist(df$`UPPSP LACK OF PREMED`, breaks=6)
hist(df$`UPPSP LACK OF PERSEV`, breaks=8)
hist(df$ln_k_excluding_nondiscounters, breaks=6)

boxplot(df$SPSI_ICSSUB, main = "SPSI_ICSSUB")
boxplot(df$BIS_COGNIT, main = "BIS_COGNIT")
boxplot(df$BIS_MOTOR, main = "BIS_MOTOR")
boxplot(df$BIS_NONPLAN, main = "BIS_NONPLAN")
boxplot(df$'UPPSP NEG URGENCY', main = "UPPSP NEG")
boxplot(df$'UPPSP POS URGENCY', main = "UPPSP POS")
boxplot(df$'UPPSP LACK OF PREMED', main = "UPPSP LACK PREMED")
boxplot(df$`UPPSP LACK OF PERSEV`, main = "UPPSP LACK PERSEV")
boxplot(df$ln_k_excluding_nondiscounters, main = "lnK")

# correlations across impulsivity measures

chars <- as.data.frame(df[, c(26:30,32:38,42, 63)])
#head(just_rois)
# cormat <- cor(na.omit(chars))
# pdf("trait correlations.pdf", width=14, height=14)
cors <- corr.test(chars, use = "pairwise",method="pearson", alpha=.05)


par(mfrow=c(1,1))
pdf("impulsivity k correlations.pdf", width=14, height=14)
corrplot(cors$r, cl.lim=c(-1,1),
         method = "shade", tl.cex = 1, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,  
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.01, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
dev.off()

# impulsivity vars by group
chars <- as.data.frame(df[, c(26:30,32:35,42:43)])
c2 <-
  compareGroups(
    chars,
    y = df$group_early,
    bivar = TRUE,
    include.miss = FALSE
  )
t2 <-
  createTable(
    c2,
    # hide = c(sex = "FEMALE", list(race = c(
    #   "WHITE", "ASIAN PACIFIC"
    # ))),
    hide.no = 0,
    digits = 1,
    show.n = TRUE,
    show.p.mul = TRUE
  )
export2html(t2, "imp_measures_by_group.html")

# build a linear model
df$age <- df$`BASELINE AGE`
df$sex <- df$`GENDER TEXT`

m1 <- lm(SPSI_ICSSUB ~ age + EDUCATION + sex + group_early, data = df)
summary(m1)
em1 <- emmeans(m1,"group_early")
plot(em1, horiz = F)
CLD <- cld(em1)

m2 <- lm(BIS_NONPLAN ~ age + EDUCATION + sex + group_early, data = df)
summary(m2)
em2 <- emmeans(m2,"group_early")
plot(em2, horiz = F)
cld(em2)

m3 <- lm(BIS_COGNIT ~ age + EDUCATION + sex + group_early, data = df)
summary(m3)
em3 <- emmeans(m3,"group_early")
plot(em3, horiz = F)
cld(em3)

m4 <- lm(BIS_COGNIT ~ age + EDUCATION + sex + group_early, data = df)
summary(m4)
em4 <- emmeans(m4,"group_early")
plot(em4, horiz = F)
cld(em4)

m5 <- lm(`UPPSP POS URGENCY` ~ age + EDUCATION + sex + group_early, data = df)
summary(m5)
em5 <- emmeans(m5,"group_early")
plot(em5, horiz = F)
cld(em5)

m6 <- lm(`UPPSP NEG URGENCY` ~ age + EDUCATION + sex + group_early, data = df)
summary(m6)
em6 <- emmeans(m6,"group_early")
plot(em6, horiz = F)
cld(em6)

m7 <- lm(`UPPSP LACK OF PERSEV` ~ age + EDUCATION + sex + group_early, data = df)
summary(m7)
em7 <- emmeans(m7,"group_early")
plot(em7, horiz = F)
cld(em7)

m8 <- lm(`UPPSP LACK OF PREMED` ~ age + EDUCATION + sex + group_early, data = df)
summary(m8)
em8 <- emmeans(m8,"group_early")
plot(em8, horiz = F, comparisons = T)
cld(em8)

m9 <- lm(ln_k ~ age + EDUCATION + sex + group_early, data = df)
summary(m9)
em9 <- emmeans(m9,"group_early")
plot(em9, horiz = F, comparisons = T)
cld(em9)

# could try MANOVA
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance


#value
# also without discounting
imp <-  df[, c(26:29,32:35)]
#val_rois <- val_rois[,-grep("ACC",names(val_rois))]
cors <- corr.test(imp, use = "pairwise",method="pearson", alpha=.05)


pdf("impulsivity correlations.pdf", width=14, height=14)
corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.05, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
dev.off()


imp.pca = prcomp(na.omit(imp),scale = TRUE)
# imp_pcas <- get_pca_ind(imp.pca)
summary(imp.pca)
plot(imp.pca,type = 'l')


autoplot(imp.pca, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# save factor scores
# find IDs with nothing missing
ids <-  na.omit(df[, c(1,26:29,32:35,42)])
ids <- ids[,1]
df$impPC1 <- NA
df$impPC2 <- NA

test <- imp.pca$x[,1]

df$impPC1[is.element(df$ID, ids$ID)]<- imp.pca$x[,1]
df$impPC2[is.element(df$ID, ids$ID)]<- imp.pca$x[,2]

test <-  df[, c(26:29,32:35,42,48:49)]
#val_rois <- val_rois[,-grep("ACC",names(val_rois))]
cors <- corr.test(test, use = "pairwise",method="pearson", alpha=.05)

# rerun linear model on first PC
m10 <- lm(impPC1 ~ age + EDUCATION + sex + group_early, data = df)
summary(m10)
em10 <- emmeans(m10,"group_early")
plot(em10, horiz = F, comparisons = T)
cld(em10)

# by attempt lethality
m11 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467, data = df)
summary(m11)
em11 <- emmeans(m11,"GROUP12467")
plot(em11, horiz = F, comparisons = T)
cld(em11)

# do lethality and age of onset explain unique variance?
# answer: neither explains too much variance within attempters
m12 <- lm(impPC1 ~ age + EDUCATION + sex + group_early, data = df[df$GROUP1245==5,])
summary(m12)
m13 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467, data = df[df$GROUP1245==5,])
summary(m13)
anova(m12,m13)
m14 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467 + group_early, data = df[df$GROUP1245==5,])
summary(m14)
anova(m12,m13,m14)

# age of onset by lethality, obviously a relationship, partly obscured by greater # attempts in early-onset
ggplot(df[df$GROUP1245==5,], aes(x = `AGE AT FIRST ATTEMPT`,  y = `MAX LETHALITY`, color = sex, shape = `RACE TEXT`, linetype = `RACE TEXT`)) + geom_jitter() + geom_smooth(method = "gam")

# and what about impulsivity vs.continuous age at first attempt?
ggplot(df[df$GROUP1245==5,], aes(x = `AGE AT FIRST ATTEMPT`,  y = impPC1, color = sex, linetype = `RACE TEXT`)) + geom_jitter() + geom_smooth(method = "gam")
ggplot(df[df$GROUP1245==5,], aes(x = `AGE AT FIRST ATTEMPT`,  y = impPC1)) + geom_jitter() + geom_smooth(method = "gam")


# lethality vs. impulsivity -- very weak relationship, would not emphasize
ggplot(df[df$GROUP1245==5,], aes(x = `MAX LETHALITY`,  y = impPC1, color = sex)) + geom_jitter() + geom_smooth(method = "gam")

# recode the variable names

# manova

imps <- as.data.frame(df[, c(26:29,32:35)])
summary(man1 <- manova(cbind(SPSI_ICSSUB, BIS_COGNIT, BIS_MOTOR, BIS_NONPLAN, UPPS_posU, UPPS_negU, UPPS_premed, UPPS_persev) ~ group_early + sex + age, data = df))
# look at lethality for single trait measures

# re-score delay discounting in case there is an error.  Low correlations suspicious.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5042866/
#rescore values UPMC desktop
money2 <- read_excel("C:/Users/perryma/Desktop/impulsivity_delaydiscounting/mcq_rescore/subjvalues_k.xlsx")

#rescore values michelle laptop
#money2 <- read_excel("C:/Users/Michelle/Desktop/MCQ_rescore/subjvalues_k.xlsx")
money2dat <- money2[, c("ID","CDATE", "QuestionNumber", "Q")]
money2dat$Q[money2dat$Q == 1] <- 2
money2dat$Q[money2dat$Q == 0] <- 1
## Long to wide

#Change questionnumber to character string
as.character(money2dat$QuestionNumber)

#reshape to fit MCQsyntax
m2d_dates <- dcast(money2dat, ID + CDATE ~ QuestionNumber, value.var = "Q")

#make unique identifier by date taken MCQ
m2d_dates$subjID <- make.names(m2d_dates$ID,unique=T)

#df compatible with syntax
MCQdata <- m2d_dates[, c(33, 3:29)]
MCQdata$subjID <- stri_replace_all_fixed(MCQdata$subjID,"X","")
MCQdata$subjID <- stri_replace_all_fixed(MCQdata$subjID,".1","_2")
#rename column headers
colnames(MCQdata) <- paste("MCQ", colnames(MCQdata), sep = "")

#aaaaaaand undo the subjID MCQ add
names(MCQdata)[names(MCQdata) == "MCQsubjID"] <- "subjID"

# UPMC desktop load lookup tables
lookup1 <- read.table("C:/Users/perryma/Desktop/MCQ_rescore/lookup1MCQ.txt", header = TRUE)
lookup2 <- read.table("C:/Users/perryma/Desktop/MCQ_rescore/lookup2MCQ.txt", header = TRUE)
lookup3 <- read.table("C:/Users/perryma/Desktop/MCQ_rescore/lookup3MCQ.txt", header = TRUE)

# Michelle laptop load lookup tables
#lookup1 <- read.table("C:/Users/Michelle/Desktop/MCQ_rescore/lookup1MCQ.txt", header = TRUE)
#lookup2 <- read.table("C:/Users/Michelle/Desktop/MCQ_rescore/lookup2MCQ.txt", header = TRUE)
#lookup3 <- read.table("C:/Users/Michelle/Desktop/MCQ_rescore/lookup3MCQ.txt", header = TRUE)

#Calculate unique value for each sequence of responses
MCQdata$MCQ13 <- MCQdata$MCQ13*1
MCQdata$MCQ20 <- MCQdata$MCQ20*2
MCQdata$MCQ26 <- MCQdata$MCQ26*4
MCQdata$MCQ22 <- MCQdata$MCQ22*8
MCQdata$MCQ3 <- MCQdata$MCQ3*16
MCQdata$MCQ18 <- MCQdata$MCQ18*32
MCQdata$MCQ5 <- MCQdata$MCQ5*64
MCQdata$MCQ7 <- MCQdata$MCQ7*128
MCQdata$MCQ11 <- MCQdata$MCQ11*256
MCQdata$SmlSeq <- with (MCQdata, MCQ13+MCQ20+MCQ26+MCQ22+MCQ3+MCQ18+MCQ5+MCQ7+MCQ11-510)
MCQdata$MCQ1 <- MCQdata$MCQ1*1
MCQdata$MCQ6 <- MCQdata$MCQ6*2
MCQdata$MCQ24 <- MCQdata$MCQ24*4
MCQdata$MCQ16 <- MCQdata$MCQ16*8
MCQdata$MCQ10 <- MCQdata$MCQ10*16
MCQdata$MCQ21 <- MCQdata$MCQ21*32
MCQdata$MCQ14 <- MCQdata$MCQ14*64
MCQdata$MCQ8 <- MCQdata$MCQ8*128
MCQdata$MCQ27 <- MCQdata$MCQ27*256
MCQdata$MedSeq <- with (MCQdata, MCQ1+MCQ6+MCQ24+MCQ16+MCQ10+MCQ21+MCQ14+MCQ8+MCQ27-510)

MCQdata$MCQ9 <- MCQdata$MCQ9*1
MCQdata$MCQ17 <- MCQdata$MCQ17*2
MCQdata$MCQ12 <- MCQdata$MCQ12*4
MCQdata$MCQ15 <- MCQdata$MCQ15*8
MCQdata$MCQ2 <- MCQdata$MCQ2*16
MCQdata$MCQ25 <- MCQdata$MCQ25*32
MCQdata$MCQ23 <- MCQdata$MCQ23*64
MCQdata$MCQ19 <- MCQdata$MCQ19*128
MCQdata$MCQ4 <- MCQdata$MCQ4*256
MCQdata$LrgSeq <- with (MCQdata, MCQ9+MCQ17+MCQ12+MCQ15+MCQ2+MCQ25+MCQ23+MCQ19+MCQ4-510)

#Remove unwanted columns
MCQdata[2:28] <- list(NULL)

#Maintain row order
MCQdata$id <- 1:nrow(MCQdata)

#Merge in MCQindices from lookup table
MCQdata <- (merge(lookup1, MCQdata, by = 'SmlSeq'))
MCQdata <- (merge(lookup2, MCQdata, by = 'MedSeq'))
MCQdata <- (merge(lookup3, MCQdata, by = 'LrgSeq'))

#Return to the original order of rows
MCQdata <- MCQdata[order(MCQdata$id),]
head(MCQdata)

#Arrange columns in ideal order
MCQdata <- MCQdata[c(13,9,10,11,12,5,6,7,8,1,2,3,4)]

#Save MCQ indices to a text file
write.table(MCQdata, file="C:/Users/perryma/Desktop/MCQ_rescore/MCQindices.txt", row.names=FALSE)

## need to fix for NA values in primary rescoring? 
## two NA in 211147 2009-01-22 #16 and #18 BUT completed MCQ 2x, has full dataset ~4 months later


## exclude low consistency pts? Find % of pts and ask Alex

#add new K values to main df
df$money_rescore <- MCQdata$MedK[match(df$ID,MCQdata$subjID)]
df$MedCons <- MCQdata$MedCons[match(df$ID,MCQdata$subjID)]
df$ln_k_rescore <- log(df$money_rescore)
df$ln_k_rescore <- log(df$money_rescore)
df$ln_k_compare <- ((df$ln_k_rescore-df$ln_k)/df$ln_k)*100
money_compare <- df[, c("MONEY", "ln_k", "money_rescore", "ln_k_rescore")]

#rerun LM for new K
m15 <- lm(ln_k_rescore ~ age + EDUCATION + sex + group_early, data = df)
summary(m15)
em15 <- emmeans(m15,"group_early")
plot(em15, horiz = F, comparisons = T)
cld(em15)

#remove low consistency folks (there are 6 below 70%;47 below 80%)
df$ln_k_consistent_liberal <- df$ln_k_rescore
df$ln_k_consistent_conservative <- df$ln_k_rescore
inconsistent_folks_liberal <- df$MedCons < 0.7
inconsistent_folks_conservative <- df$MedCons < 0.8
#check counts format: sum(z, na.rm=TRUE)
df$ln_k_consistent_liberal[df$MedCons<0.75] <- NA
df$ln_k_consistent_conservative[df$MedCons<0.8] <- NA

m16 <- lm(ln_k_consistent_liberal ~ age + EDUCATION + sex + group_early, data = df)
summary(m16)
em16 <- emmeans(m16,"group_early")
plot(em16, horiz = F, comparisons = T)
cld(em16)

m17 <- lm(ln_k_consistent_conservative ~ age + EDUCATION + sex + group_early, data = df)
summary(m17)
em17 <- emmeans(m17,"group_early")
plot(em17, horiz = F, comparisons = T)
cld(em17)

#remove nondiscounters?
df$ln_k_consistent_liberal_nonnd <- df$ln_k_consistent_liberal
nondiscounters2 <- df$ln_k_consistent_liberal < -10
df$ln_k_consistent_liberal_nonnd[nondiscounters] <- NA

df$ln_k_consistent_cons_nonnd <- df$ln_k_consistent_conservative
nondiscounters3 <- df$ln_k_consistent_conservative < -10
df$ln_k_consistent_cons_nonnd[nondiscounters] <- NA

m18 <- lm(ln_k_consistent_liberal_nonnd ~ age + EDUCATION + sex + group_early, data = df)
summary(m18)
em18 <- emmeans(m18,"group_early")
lm18 <- lsmeans(m18,"group_early")

plot(em18, horiz = F, comparisons = T)
cld(em18)
cld(lm18)
#m18 shows significance for early onset & ide after controlling for consistency & nondiscounters

m19 <- lm(ln_k_consistent_cons_nonnd ~ age + EDUCATION + sex + group_early, data = df, col = "red")
summary(m19)
em19 <- emmeans(m19,"group_early")
plot(em19, horiz = F, comparisons = T)
cld(em19)
#m19 doesn't show anything too interesting, possibly not enough data to get clear results

hist(df$ln_k_consistent_cons_nonnd, breaks = 6)
hist(df$ln_k_consistent_liberal_nonnd, breaks = 6)

##rerun graphs for presentation
par(mfrow=c(3,3))
hist(df$SPSI_ICSSUB, breaks=8)
hist(df$BIS_COGNIT)
hist(df$BIS_MOTOR, breaks=6)
hist(df$BIS_NONPLAN, breaks=6)
hist(df$`UPPSP NEG URGENCY`, breaks=6)
hist(df$`UPPSP POS URGENCY`, breaks=4)
hist(df$`UPPSP LACK OF PREMED`, breaks=6)
hist(df$`UPPSP LACK OF PERSEV`, breaks=8)
hist(df$ln_k_consistent_liberal_nonnd, breaks=6)

boxplot(df$SPSI_ICSSUB, main = "SPSI_ICSSUB")
boxplot(df$BIS_COGNIT, main = "BIS_COGNIT")
boxplot(df$BIS_MOTOR, main = "BIS_MOTOR")
boxplot(df$BIS_NONPLAN, main = "BIS_NONPLAN")
boxplot(df$'UPPSP NEG URGENCY', main = "UPPSP NEG")
boxplot(df$'UPPSP POS URGENCY', main = "UPPSP POS")
boxplot(df$'UPPSP LACK OF PREMED', main = "UPPSP LACK PREMED")
boxplot(df$'UPPSP LACK OF PERSEV', main = "UPPSP LACK PERSEV")
boxplot(df$ln_k_consistent_liberal_nonnd, main = "lnK consist")


plot(em1, horiz=F, comparisons = T, main = "SPSI_ICSSUB", col = 'lemonchiffon4')
plot(em2, horiz=F, comparisons = T, main = "BIS_COGNIT", col = "lavender")
plot(em3, horiz=F, comparisons = T, main = "BIS_MOTOR", col = "lavenderblush1")
plot(em4, horiz=F, comparisons = T, main = "BIS_NONPLAN", col = "lavenderblush3")
plot(em5, horiz=F, comparisons = T, main = "UPPSP NEG", col = "honeydew4")
plot(em6, horiz=F, comparisons = T, main = "UPPSP POS", col = "honeydew3")
plot(em7, horiz=F, comparisons = T, main = "UPPSP LACK PREMED", col = "honeydew2")
plot(em8, horiz=F, comparisons = T, main = "UPPSP LACK PERSEV", col = "honeydew")
plot(em18, horiz=F, comparisons = T, main = "lnK CONSIST", col = "seagreen")

plot(lm18, horiz=F, comparisons = T, main = "lnK CONSIST", col = "seagreen")

CLD_lm18 = cld(lm18,
                      alpha=0.05,
                      Letters=letters,
                      adjust="tukey")
CLD_lm18$.group=gsub(" ", "", CLD_lm18$.group)

pd = position_dodge(.8)

p1 <- ggplot(CLD_lm18,
       aes(
         x     = group_early,
         y     = lsmean,
         color = group_early,
         label = .group
       )) +
  #  facet_wrap( ~ group_early) +
  
  geom_point(shape  = 15,
             size   = 4,
             #             colour = c("grey40", "grey60", "#CCCC00", "#FF9900", "#FF6600"),
             position = pd) +
  
  geom_errorbar(
    aes(ymin  =  lower.CL,
        ymax  =  upper.CL),
    width =  0.2,
    size  =  0.7,
    #    colour = c("grey40", "grey60", "#CCCC00", "#FF9900", "#FF6600"),
    position = pd
  ) +
  theme_bw() +
  theme(plot.title = element_text(size=20)) +
  scale_x_discrete(labels=c("HC","DC","SI","eoSA","loSA")) +
  theme(
    #axis.title.x=element_blank(),
    #axis.title.y = text(size=16), 
    axis.title.x=element_text(size=16),
    axis.title.y=element_text(size=16),
    axis.text.x=element_text(size = 12),
    axis.text.y=element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size=14),
    plot.title = element_text(size = 18)) +
  #theme(legend.text = element_text(colour="black", size = 16)) +
  #theme(legend.title = element_text(colour="black", size = 16, face = "bold")) +
  scale_fill_discrete(labels=c("HC: healthy controls",
                               "DC: depressed controls",
                               "SI: suicidal ideators",
                               "eoSA: early-onset attempters",
                               "laSA: late-onset attempters")) +
  # theme(axis.title.y=element_text(size=14),
  #       axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  
  ylab("Z-scores of NEO-FFI") +
  xlab("study groups") +
  ggtitle("Five factors: overall group differences") +
  #         subtitle = "Linear regression model controlling for age and gender") +
  # labs(
  #   caption  = paste0(
  #     "\n",
  #     "Boxes indicate least square means.\n",
  #     "Error bars indicate the 95% ",
  #     "confidence interval of the least squares means. \n",
  #     "Means sharing a letter are ",
  #     "not significantly different ",
  #     "(Tukey-adjusted pairwise comparisons).\n",
  #     "Early-onset attempts are defined by age at first attempt 50 or less."
#   ),
#   hjust = 0.5
# ) +
geom_text(nudge_x = 0.2,
          nudge_y = -0.2,
          color   = "black") 
#dev.off()

#multiple plots together
library(grid)
library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,
             layout_matrix = matrix(c(1,2,3,4,5,5), ncol=3, byrow=TRUE))




