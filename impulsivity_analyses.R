## Michelle's impulsivity analyses

##install packages on new machine
install.packages(c("readr", "lme4", "lmerTest", "ggplot2", "dplyr", "tidyr", "tibble", "Hmisc",
                   "nnet", "reshape2", "emmeans", "factoextra", "compareGroups", "effects", "VIM", "mice", "multcompView",
                   "readxl", "lsmeans", "corrplot", "stringi", "psych"))

#setwd("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/")

#wd for desktop comp @ home
#setwd("/home/bluebird/Desktop")

#wd for UPMC desktop
setwd("C:/Users/perryma/Box/skinner/projects_analyses/impulsivity_delaydiscounting")

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

#  read in data
#df <- read_excel("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

# Michelle desktop @ home
#df <- read_excel("/home/bluebird/Desktop/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

#Michelle UPMC desktop
df <- read_excel("impulsivity.updated.01-11-18.xlsx")
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

# correlations across impulsivity measures

chars <- as.data.frame(df[, c(26:30,32:38)])
#head(just_rois)
# cormat <- cor(na.omit(chars))
# pdf("trait correlations.pdf", width=14, height=14)
cors <- corr.test(chars, use = "pairwise",method="pearson", alpha=.05)

# Michelle to check all histograms for herself

hist(df$SPSI_ICSSUB, breaks=8)
hist(df$BIS_COGNIT)
hist(df$BIS_MOTOR, breaks=6)
hist(df$BIS_NONPLAN, breaks=6)
hist(df$`UPPSP NEG URGENCY`, breaks=6)
hist(df$`UPPSP POS URGENCY`, breaks=4)
hist(df$`UPPSP LACK OF PREMED`, breaks=6)
hist(df$`UPPSP LACK OF PERSEV`, breaks=8)
hist(df$ln_k_excluding_nondiscounters, breaks=6)

par(mfrow=c(1,1))
corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.05, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
# dev.off()

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
em <- emmeans(m1,"group_early")
plot(em, horiz = F)
CLD <- cld(em)

m2 <- lm(BIS_NONPLAN ~ age + EDUCATION + sex + group_early, data = df)
summary(m2)
em <- emmeans(m2,"group_early")
plot(em, horiz = F)
cld(em)

m3 <- lm(BIS_COGNIT ~ age + EDUCATION + sex + group_early, data = df)
summary(m3)
em <- emmeans(m3,"group_early")
plot(em, horiz = F)
cld(em)

m4 <- lm(BIS_COGNIT ~ age + EDUCATION + sex + group_early, data = df)
summary(m4)
em <- emmeans(m4,"group_early")
plot(em, horiz = F)
cld(em)

m5 <- lm(`UPPSP POS URGENCY` ~ age + EDUCATION + sex + group_early, data = df)
summary(m5)
em <- emmeans(m5,"group_early")
plot(em, horiz = F)
cld(em)

m6 <- lm(`UPPSP NEG URGENCY` ~ age + EDUCATION + sex + group_early, data = df)
summary(m6)
em <- emmeans(m6,"group_early")
plot(em, horiz = F)
cld(em)

m7 <- lm(`UPPSP LACK OF PERSEV` ~ age + EDUCATION + sex + group_early, data = df)
summary(m7)
em <- emmeans(m7,"group_early")
plot(em, horiz = F)
cld(em)

m8 <- lm(`UPPSP LACK OF PREMED` ~ age + EDUCATION + sex + group_early, data = df)
summary(m8)
em <- emmeans(m8,"group_early")
plot(em, horiz = F, comparisons = T)
cld(em)

m9 <- lm(ln_k ~ age + EDUCATION + sex + group_early, data = df)
summary(m9)
em <- emmeans(m9,"group_early")
plot(em, horiz = F, comparisons = T)
cld(em)

# could try MANOVA
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance


#value
# also without discounting
imp <-  df[, c(26:29,32:35)]
#val_rois <- val_rois[,-grep("ACC",names(val_rois))]
cors <- corr.test(imp, use = "pairwise",method="pearson", alpha=.05)

# Michelle to check all histograms for herself

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
em <- emmeans(m10,"group_early")
plot(em, horiz = F, comparisons = T)
cld(em)

# by attempt lethality
m11 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467, data = df)
summary(m11)
em <- emmeans(m11,"GROUP12467")
plot(em, horiz = F, comparisons = T)
cld(em)

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

