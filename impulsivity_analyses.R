## Michelle's impulsivity analyses

##install packages on new machine

#install.packages(c("readr", "lme4", "lmerTest", "ggplot2", "dplyr", "tidyr", "tibble", "Hmisc",
#                   "nnet", "reshape2", "emmeans", "factoextra", "compareGroups", "effects", "VIM", "mice", "multcompView",
#                   "readxl", "lsmeans", "corrplot", "stringi", "psych", "stringr", "ggfortify"))

#setwd("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/")

#wd for desktop comp @ home
#setwd("/home/bluebird/Desktop"),

#wd for UPMC desktop
setwd("C:/Users/perryma/Desktop")

#wd for Michelle laptop
#setwd("C:/Users/Michelle/Desktop")
library(stringi)
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
#library(ggbiplot)
library(corrplot)
library(emmeans)
library(factoextra)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(MASS)
library(effects)
library(readr)
library(VIM)
library(mice)
library(multcompView)
library(readxl)
library(lsmeans)
library(psych)
library(corrplot)
library(stringr)

#  read in data
#df <- readxl::read_excel("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

# Michelle desktop @ home
#df <- readxl::read_excel("/home/bluebird/Desktop/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

#Michelle UPMC desktop
#df <- readxl::read_excel("C:/Users/perryma/Desktop/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")
df <- readxl::read_excel(("C:/Users/perryma/Documents/GitHub/impulsivity/impl400.xlsx"))

#Michelle laptop
#df <- readxl::read_excel("Impulsivity.updated.01-11-18.xlsx")
names(df)
#View(df)

df$MONEY <- (df$MoneyS*df$MoneyM*df$MoneyL)^(1/3)
df$ln_k <- log(df$MONEY)
# NB: there are some non-discounters in this database
df$ln_k_excluding_nondiscounters <- df$ln_k
nondiscounters <- df$ln_k < -10
df$ln_k_excluding_nondiscounters[nondiscounters] <- NA


df$GROUP12467 <- as.factor(df$GROUP12467)
df$GROUP1245 <- as.factor(df$GROUP1245)
df$DEBT <- as.factor(df$DEBT)
df$DEBT_TROUBLE <- as.factor(df$DEBT_TROUBLE)
df$'MAX LETHALITY' <- as.numeric(df$'MAX LETHALITY')
summary(df)

df_sup <- readxl::read_excel(("C:/Users/perryma/Documents/GitHub/impulsivity/suppl_imp.xlsx"))


df_tot <- left_join(df, df_sup, by=c("ID"))
summary(df_tot)
df_tot$ID <- as.factor(df_tot$ID)


# check missing data
missing_ind_chars = VIM::aggr(
  df_tot,
  col = mice::mdc(1:2),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(df_tot),
  cex.axis = .7,
  gap = 3,
  ylab = c("Proportion of missingness", "Missingness Pattern")
)

# we don't have age at first attempt, but Michelle will add it to the df_tot and calculate a new variable as follows:

#early- and late-onset grouping

#df_tot$group_early <- as.character(df_tot$COMMENT)
#df_tot$group_early <- df_tot$GROUP1245
df_tot$group_early <- factor(df_tot$GROUP1245, levels=c(levels(df_tot$GROUP1245), "10", "11"))

##df_tot$group_early[df_tot$group_early=="CONTROL"] <- "Non-psychiatric controls"
#df_tot$group_early[df_tot$group_early=="DEPRESSION"] <- "Non-suicidal depressed"
#df_tot$group_early[df_tot$group_early=="IDEATOR"] <- "Suicide ideators"
#df_tot$group_early[df_tot$group_early=="DEPRESSION-IDEATOR"] <- "Suicide ideators"
#df_tot$group_early[df_tot$group_early=="IDEATOR-ATTEMPTER"] <- "Suicide ideators"
df_tot$group_early[df_tot$`AGE AT FIRST ATTEMPT`<57] <- "10"
df_tot$group_early[df_tot$`AGE AT FIRST ATTEMPT`>=57] <- "11"
df_tot$group_early[df_tot$ID=='210002'] <- "11"
df_tot$group_early[df_tot$ID=='114886'] <- "11"
#df_tot$group_early[df_tot$`AGE AT FIRST ATTEMPT`<56] <- "Early-onset attempters"
#df_tot$group_early[df_tot$`AGE AT FIRST ATTEMPT`>=56] <- "Late-onset attempters"
#df_tot$group_early[df_tot$`AGE AT FIRST ATTEMPT`<60] <- "Early-onset attempters"
#df_tot$group_early[df_tot$`AGE AT FIRST ATTEMPT`>=60] <- "Late-onset attempters"
df_tot$group_early <- factor(df_tot$group_early)
table(df_tot$group_early)
median(df_tot$'AGE AT FIRST ATTEMPT', na.rm = TRUE)
median(df_tot_censored$'AGE AT FIRST ATTEMPT', na.rm = TRUE)

#low and high planing grouping
df_tot$group_planning <- factor(df_tot$GROUP1245, levels=c(levels(df_tot$GROUP1245), "8", "9"))

df_tot$group_planning[df_tot$`BL WORST INTENT PLANNING`<=8] <- "8"
df_tot$group_planning[df_tot$`BL WORST INTENT PLANNING`>8] <- "9"
df_tot$group_planning[df_tot$group_planning == '5'] <- NA
df_tot$group_planning <- factor(df_tot$group_planning)
table(df_tot$group_planning)


names(df_tot)[names(df_tot)=="HOUSEHOLD INCOME"] <- "Income_tot"

#test <- df_tot[,c(8,44,45)]
#View(test)


# recode the variable names
df_tot$UPPS_negU <- df_tot$`UPPSP NEG URGENCY`
df_tot$UPPS_posU <- df_tot$`UPPSP POS URGENCY`
df_tot$UPPS_premed <- df_tot$`UPPSP LACK OF PREMED`
df_tot$UPPS_persev <- df_tot$`UPPSP LACK OF PERSEV`
df_tot$age_first_att <- df_tot$`AGE AT FIRST ATTEMPT`

#####need to specify compareGroups::compareGroups:: package or won't run
# just a prototype
chars <- as.data.frame(df_tot[, c(5,10:16,23)])
c1 <-
  compareGroups::compareGroups(
    chars,
    y = df_tot$group_early,
    bivar = TRUE,
    include.miss = FALSE
  )
t1 <-
  compareGroups::createTable(
    c1,
    hide = c(sex = "FEMALE", list(race = c(
      "WHITE", "ASIAN PACIFIC"
    ))),
    hide.no = 0,
    digits = 0,
    show.n = TRUE
  )
compareGroups::export2html(t1, "imp_chars_by_group.html")

# Michelle to check all histograms for herself
par(mfrow=c(3,3))
hist(df_tot$SPSI_ICSSUB, breaks=8)
hist(df_tot$BIS_COGNIT)
hist(df_tot$BIS_MOTOR, breaks=6)
hist(df_tot$BIS_NONPLAN, breaks=6)
hist(df_tot$`UPPSP NEG URGENCY`, breaks=6)
hist(df_tot$`UPPSP POS URGENCY`, breaks=4)
hist(df_tot$`UPPSP LACK OF PREMED`, breaks=6)
hist(df_tot$`UPPSP LACK OF PERSEV`, breaks=8)
hist(df_tot$ln_k_excluding_nondiscounters, breaks=6)

boxplot(df_tot$SPSI_ICSSUB, main = "SPSI_ICSSUB")
boxplot(df_tot$BIS_COGNIT, main = "BIS_COGNIT")
boxplot(df_tot$BIS_MOTOR, main = "BIS_MOTOR")
boxplot(df_tot$BIS_NONPLAN, main = "BIS_NONPLAN")
boxplot(df_tot$'UPPSP NEG URGENCY', main = "UPPSP NEG")
boxplot(df_tot$'UPPSP POS URGENCY', main = "UPPSP POS")
boxplot(df_tot$'UPPSP LACK OF PREMED', main = "UPPSP LACK PREMED")
boxplot(df_tot$`UPPSP LACK OF PERSEV`, main = "UPPSP LACK PERSEV")
boxplot(df_tot$ln_k_excluding_nondiscounters, main = "lnK")

# correlations across impulsivity measures
names(df_tot)
df_tot$attempts_tot <- df_tot$`TOTAL BASELINE ATTEMPTS` + df_tot$`TOTAL FOLLOWUP ATTEMPTS` 
chars <- as.data.frame(df_tot[, c(65, 39:42, 32:35)])
#head(just_rois)
# cormat <- cor(na.omit(chars))
# pdf_tot("trait correlations.pdf_tot", width=14, height=14)
cors <- psych::corr.test(chars, use = "pairwise",method="pearson", alpha=.05)

par(mfrow=c(1,1))
#pdf_tot("impulsivity k correlations.pdf_tot", width=14, height=14)
corrplot::corrplot(cors$r, cl.lim=c(-1,1),
         method = "shade", tl.cex = 1, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,  
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.01, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
#dev.off()

# impulsivity vars by group
chars2 <- as.data.frame(df_tot[, c(26:30,32:35,42:43)])
c2 <-
  compareGroups::compareGroups(
    chars2,
    y = df_tot$group_early,
    bivar = TRUE,
    include.miss = FALSE
  )
t2 <-
  compareGroups::createTable(
    c2,
    # hide = c(sex = "FEMALE", list(race = c(
    #   "WHITE", "ASIAN PACIFIC"
    # ))),
    hide.no = 0,
    digits = 1,
    show.n = TRUE,
    show.p.mul = TRUE
  )
compareGroups::export2html(t2, "imp_measures_by_group.html")

# build a linear model
emmeans::emm_options(graphics.engine = "lattice")
df_tot$age <- df_tot$`BASELINE AGE`
df_tot$sex <- df_tot$`GENDER TEXT`
df_tot$group_basic <- df_tot$GROUP1245
df_tot$MAX_LETHALITY <- as.numeric(df_tot$`MAX LETHALITY`)
df_tot$TOTAL_ATTEMPTS <- df_tot$'TOTAL BASELINE ATTEMPTS'+ df_tot$'TOTAL FOLLOWUP ATTEMPTS'
df_tot_att <- df_tot[df_tot$GROUP1245 == '5',]


m0 <- lm(UPPS_persev ~ age + EDUCATION + sex + scale(`AGE AT FIRST ATTEMPT`) + I(scale(`AGE AT FIRST ATTEMPT`)^2), data = df_tot_att)
summary(m0)
#... to be continued

#models with groups
df_tot_censored <- df_tot
df_tot_censored$group_early[df_tot_censored$`AGE AT FIRST ATTEMPT` < 20]<- NA

m1 <- lm(SPSI_ICSSUB ~ age + EDUCATION + sex + group_early, data = df_tot)
m1sum <- summary(m1)
em1 <- emmeans::emmeans(m1,"group_early")
plot(em1, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
spsicld <- emmeans::cld(em1)

m1cens <- lm(SPSI_ICSSUB ~ `BASELINE AGE` + EDUCATION + `GENDER TEXT` + group_early, data = df_tot_censored)
m1censsum <- summary(m1cens)
em1cens <- emmeans::emmeans(m1cens,"group_early")
plot(em1cens, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
spsicld_cens <- emmeans::cld(em1cens)


m1nb <- glm.nb(SPSI_ICSSUB ~ age + EDUCATION + sex + group_early, data = df_tot)
m1nbsum <- summary(m1nb)
em1nb <- emmeans::emmeans(m1nb,"group_early")
plot(em1nb, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
spsicld_nb <- emmeans::cld(em1nb)

m1p <- lm(SPSI_ICSSUB ~ age + EDUCATION + sex + group_planning, data = df_tot)
m1psum <- summary(m1p)
em1p <- emmeans::emmeans(m1p,"group_planning")
plot(em1p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
spsicld_p <- emmeans::cld(em1p)

m2 <- lm(BIS_NONPLAN ~ age + EDUCATION + sex + group_early, data = df_tot)
m2sum <- summary(m2)
em2 <- emmeans::emmeans(m2,"group_early")
plot(em2, horiz = F, comparisons = T, main = "BIS_NONPLAN")
nonplancld <- emmeans::cld(em2)

m2p <- lm(BIS_NONPLAN ~ age + EDUCATION + sex + group_planning, data = df_tot)
m2psum <- summary(m2p)
em2p <- emmeans::emmeans(m2p,"group_planning")
plot(em2p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
nonplancld_p <- emmeans::cld(em2p)

m2cens <- lm(BIS_NONPLAN ~ age + EDUCATION + sex + group_early, data = df_tot_censored)
m2censsum <- summary(m2cens)
em2cens <- emmeans::emmeans(m2cens,"group_early")
plot(em2cens, horiz = F, comparisons = T, main = "BIS_NONPLAN")
nonplancld_cens <- emmeans::cld(em2cens)

m3 <- lm(BIS_COGNIT ~ age + EDUCATION + sex + group_early, data = df_tot)
m3sum <- summary(m3)
em3 <- emmeans::emmeans(m3,"group_early")
plot(em3, horiz = F, comparisons = T, main = "BIS_COGNIT")
cognitcld <- emmeans::cld(em3)

m3p <- lm(BIS_COGNIT ~ age + EDUCATION + sex + group_planning, data = df_tot)
m3psum <- summary(m3p)
em3p <- emmeans::emmeans(m3p,"group_planning")
plot(em3p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
cognitcld_p <- emmeans::cld(em3p)

m4 <- lm(BIS_MOTOR ~ age + EDUCATION + sex + group_early, data = df_tot)
m4sum <- summary(m4)
em4 <- emmeans::emmeans(m4,"group_early")
plot(em4, horiz = F, comparisons = T, main = "BIS_MOTOR")
em4cld <- emmeans::cld(em4)

m4p <- lm(BIS_MOTOR ~ age + EDUCATION + sex + group_planning, data = df_tot)
m4psum <- summary(m4p)
em4p <- emmeans::emmeans(m4p,"group_planning")
plot(em4p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
em4cld_p <- emmeans::cld(em4p)


m5 <- lm(`UPPSP POS URGENCY` ~ age + EDUCATION + sex + group_early, data = df_tot)
m5sum <- summary(m5)
em5 <- emmeans::emmeans(m5,"group_early")
plot(em5, horiz = F, comparisons = T, main = "UPPSP_POS")
posurgcld <- emmeans::cld(em5)

m5nb <- glm.nb(`UPPSP POS URGENCY` ~ age + EDUCATION + sex + group_early, data = df_tot)
m5nbsum <- summary(m5nb)
em5nb <- emmeans::emmeans(m5nb,"group_early")
plot(em5nb, horiz = F, comparisons = T, main = "UPPSP_POS")
posurgcld_nb <- emmeans::cld(em5nb)

m5p <- lm(`UPPSP POS URGENCY` ~ age + EDUCATION + sex + group_planning, data = df_tot)
m5psum <- summary(m5p)
em5p <- emmeans::emmeans(m5p,"group_planning")
plot(em5p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
posurgcld_p <- emmeans::cld(em5p)

m6 <- lm(`UPPSP NEG URGENCY` ~ age + EDUCATION + sex + group_early, data = df_tot)
m6sum <- summary(m6)
em6 <- emmeans::emmeans(m6,"group_early")
plot(em6, horiz = F, comparisons = T, main = "UPPSP_NEG")
negurgcld <- emmeans::cld(em6)

m6p <- lm(`UPPSP NEG URGENCY` ~ age + EDUCATION + sex + group_planning, data = df_tot)
m6psum <- summary(m6p)
em6p <- emmeans::emmeans(m6p,"group_planning")
plot(em6p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
negurgcld_p <- emmeans::cld(em6p)


m7 <- lm(`UPPSP LACK OF PERSEV` ~ age + EDUCATION + sex + group_early, data = df_tot)
m7sum <- summary(m7)
em7 <- emmeans::emmeans(m7,"group_early")
plot(em7, horiz = F, comparisons = T, main = "UPPSP_LPERS")
lackperscld <- emmeans::cld(em7)

m7p <- lm(`UPPSP LACK OF PERSEV` ~ age + EDUCATION + sex + group_planning, data = df_tot)
m7psum <- summary(m7p)
em7p <- emmeans::emmeans(m7p,"group_planning")
plot(em7p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
lackperscld_p <- emmeans::cld(em7p)



m8 <- lm(`UPPSP LACK OF PREMED` ~ age + EDUCATION + sex + group_early, data = df_tot)
m8sum <- summary(m8)
em8 <- emmeans::emmeans(m8,"group_early")
plot(em8, horiz = F, comparisons = T, main = "UPPSP_LPREM")
lackpremcld <- emmeans::cld(em8)

m8cens <- lm(`UPPSP LACK OF PREMED` ~ `BASELINE AGE` + EDUCATION + `GENDER TEXT` + group_early, data = df_tot_censored)
m8censsum <- summary(m8cens)
em8cens <- emmeans::emmeans(m8cens,"group_early")
plot(em8cens, horiz = F, comparisons = T, main = "BIS_NONPLAN")
nonplancld_cens <- emmeans::cld(em8cens)


m8p <- lm(`UPPSP LACK OF PREMED` ~ age + EDUCATION + sex + group_planning, data = df_tot)
m8psum <- summary(m8p)
em8p <- emmeans::emmeans(m8p,"group_planning")
plot(em8p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
lackpremcld_p <- emmeans::cld(em8p)


m9 <- lm(ln_k ~ age + EDUCATION + sex + group_early, data = df_tot)
m9sum <- summary(m9)
em9 <- emmeans::emmeans(m9,"group_early")
plot(em9, horiz = F, comparisons = T, main = "ln_K")
lnkcld <- emmeans::cld(em9)


m9p <- lm(`ln_k` ~ age + EDUCATION + sex + group_planning, data = df_tot)
m9psum <- summary(m9p)
em9p <- emmeans::emmeans(m9p,"group_planning")
plot(em9p, horiz = F, comparisons = T, main = "SPSI_ICCSUB")
lnkcld_p <- emmeans::cld(em9p)


# could try MANOVA
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance


#value
# also without discounting

imp <-  df_tot[, c(26:29,32:35)]
impk <-  df_tot[, c(26:29,32:35, 42:43)]

#val_rois <- val_rois[,-grep("ACC",names(val_rois))]
cors <- psych::corr.test(imp, use = "pairwise",method="pearson", alpha=.05)


pdf_tot("impulsivity correlations.pdf_tot", width=14, height=14)

corrplot::corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.05, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
dev.off()

par(mfrow=c(1,1))
imp.pca <- prcomp(na.omit(imp),scale = TRUE)
impk.pca <- prcomp(na.omit(impk),scale = TRUE)
# imp_pcas <- get_pca_ind(imp.pca)

summary(imp.pca)
plot(imp.pca,type = 'l', main = "imp.pca")

summary(impk.pca)
plot(impk.pca, type = 'l', main = "impk.pca")


ggplot2::autoplot(imp.pca, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

ggplot2::autoplot(impk.pca, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



# save factor scores
# find IDs with nothing missing
ids <-  na.omit(df_tot[, c(1,26:29,32:35)])
ids <- ids[,1]
df_tot$impPC1 <- NA
df_tot$impPC2 <- NA

test <- imp.pca$x[,1]

df_tot$impPC1[is.element(df_tot$ID, ids$ID)]<- imp.pca$x[,1]
df_tot$impPC2[is.element(df_tot$ID, ids$ID)]<- imp.pca$x[,2]

test <-  df_tot[, c(26:29,32:35)]
#val_rois <- val_rois[,-grep("ACC",names(val_rois))]
cors <- psych::corr.test(test, use = "pairwise",method="pearson", alpha=.05)

# rerun linear model on first PC
m10 <- lm(impPC1 ~ age + EDUCATION + sex + group_early, data = df_tot)
summary(m10)
em10 <- emmeans::emmeans(m10,"group_early")
plot(em10, horiz = F, comparisons = T, main = "PC1 by Group")
emmeans::cld(em10)

# by attempt lethality
m11 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467, data = df_tot)
summary(m11)
em11 <- emmeans::emmeans(m11,"GROUP12467")
plot(em11, horiz = F, comparisons = T, main = "PC1 by Lethality")
emmeans::cld(em11)

# do lethality and age of onset explain unique variance?
# answer: neither explains too much variance within attempters
m12 <- lm(impPC1 ~ age + EDUCATION + sex + group_early, data = df_tot[df_tot$GROUP1245==5,])
summary(m12)
m13 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467, data = df_tot[df_tot$GROUP1245==5,])
summary(m13)
anova(m12,m13)
m14 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467 + group_early, data = df_tot[df_tot$GROUP1245==5,])
summary(m14)
anova(m12,m13,m14)

# age of onset by lethality, obviously a relationship, partly obscured by greater # attempts in early-onset
ggplot2::ggplot(df_tot[df_tot$GROUP1245==5,], ggplot2::aes(x = `AGE AT FIRST ATTEMPT`,  y = `MAX LETHALITY`, color = sex, shape = `RACE TEXT`, linetype = `RACE TEXT`)) + ggplot2::geom_jitter() + ggplot2::geom_smooth(method = "gam")

# and what about impulsivity vs.continuous age at first attempt?
ggplot2::ggplot(df_tot[df_tot$GROUP1245==5,], ggplot2::aes(x = `AGE AT FIRST ATTEMPT`,  y = impPC1, color = sex, linetype = `RACE TEXT`)) + ggplot2::geom_jitter() + ggplot2::geom_smooth(method = "gam")
ggplot2::ggplot(df_tot[df_tot$GROUP1245==5,], ggplot2::aes(x = `AGE AT FIRST ATTEMPT`,  y = impPC1)) + ggplot2::geom_jitter() + ggplot2::geom_smooth(method = "gam")


# lethality vs. impulsivity -- very weak relationship, would not emphasize
ggplot2::ggplot(df_tot[df_tot$GROUP1245==5,], ggplot2::aes(x = `MAX LETHALITY`,  y = impPC1, color = sex)) + ggplot2::geom_jitter() + ggplot2::geom_smooth(method = "gam")

# recode the variable names

# manova

imps <- as.data.frame(df_tot[, c(26:29,32:35)])
summary(man1 <- manova(cbind(SPSI_ICSSUB, BIS_COGNIT, BIS_MOTOR, BIS_NONPLAN, UPPS_posU, UPPS_negU, UPPS_premed, UPPS_persev) ~ group_early + sex + age, data = df_tot))

## look at lethality for single trait measures
#summary(man1 <- manova(cbind(SPSI_ICSSUB, BIS_COGNIT, BIS_MOTOR, BIS_NONPLAN, UPPS_posU, UPPS_negU, UPPS_premed, UPPS_persev) ~ group_early + sex + age, data = df_tot))
#m12 <- lm(impPC1 ~ age + EDUCATION + sex + group_early, data = df_tot[df_tot$GROUP1245==5,])
#summary(m12)
#m13 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467, data = df_tot[df_tot$GROUP1245==5,])
#summary(m13)
#anova(m12,m13)
#m14 <- lm(impPC1 ~ age + EDUCATION + sex + GROUP12467 + group_early, data = df_tot[df_tot$GROUP1245==5,])
#summary(m14)
#anova(m12,m13,m14)
# re-score delay discounting in case there is an error.  Low correlations suspicious.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5042866/
#rescore values UPMC desktop
money2 <- readxl::read_excel("C:/Users/perryma/Desktop/impulsivity_delaydiscounting/mcq_rescore/subjvalues_k.xlsx")

#rescore values michelle laptop
#money2 <- readxl::read_excel("C:/Users/Michelle/Desktop/MCQ_rescore/subjvalues_k.xlsx")
money2dat <- money2[, c("ID","CDATE", "QuestionNumber", "Q")]
money2dat$Q[money2dat$Q == 1] <- 2
money2dat$Q[money2dat$Q == 0] <- 1
## Long to wide

#Change questionnumber to character string
as.character(money2dat$QuestionNumber)

#reshape to fit MCQsyntax
m2d_dates <- reshape2::dcast(money2dat, ID + CDATE ~ QuestionNumber, value.var = "Q")

#make unique identifier by date taken MCQ
m2d_dates$subjID <- make.names(m2d_dates$ID,unique=T)

#df_tot compatible with syntax
MCQdata <- m2d_dates[, c(33, 3:29)]
MCQdata$subjID <- stringi::stri_replace_all_fixed(MCQdata$subjID,"X","")
MCQdata$subjID <- stringi::stri_replace_all_fixed(MCQdata$subjID,".1","_2")
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
#write.table(MCQdata, file="C:/Users/Michelle/Desktop/MCQ_rescore/MCQindices.txt")
## need to fix for NA values in primary rescoring? 
## two NA in 211147 2009-01-22 #16 and #18 BUT completed MCQ 2x, has full dataset ~4 months later


## exclude low consistency pts? Find % of pts and ask Alex

#only med k values ##should eventually discard
df_tot$money_rescore <- MCQdata$MedK[match(df_tot$ID,MCQdata$subjID)]
df_tot$MedCons <- MCQdata$MedCons[match(df_tot$ID,MCQdata$subjID)]
df_tot$ln_k_rescore <- log(df_tot$money_rescore)


#add new K values to main df_tot
df_tot$lnkMed <- MCQdata$MedK[match(df_tot$ID,MCQdata$subjID)]
df_tot$MedConsMED <- MCQdata$MedCons[match(df_tot$ID,MCQdata$subjID)]
df_tot$lnkLar <- MCQdata$LrgK[match(df_tot$ID,MCQdata$subjID)]
df_tot$MedConsLRG <- MCQdata$LrgCons[match(df_tot$ID,MCQdata$subjID)]
df_tot$lnkSmall <- MCQdata$SmlK[match(df_tot$ID,MCQdata$subjID)]
df_tot$MedConsSML <- MCQdata$SmlCons[match(df_tot$ID,MCQdata$subjID)]
df_tot$geomMean_K <- (df_tot$lnkMed*df_tot$lnkLar*df_tot$lnkSmall)^(1/3)
df_tot$geomMean_consist <- (df_tot$MedConsLRG*df_tot$MedConsMED*df_tot$MedConsSML)^(1/3)
df_tot$ln_k_rescore_geom <- log(df_tot$geomMean_K)

ldf_tot = reshape2::melt(df_tot,
           na.rm = FALSE,
           measure.vars = c("lnkSmall", "lnkMed", "lnkLar"), value.name = 'ln_k_long')

lm1 <- lme4::lmer(ln_k_long ~ 'name of factor variable in ldf_tot'*group_early + age + EDUCATION + sex + Income_tot + (1|ID), data = ldf_tot)


##any difference?
t.test(df_tot$ln_k, df_tot$ln_k_rescore, alternative = "two.sided", var.equal = FALSE)

#rerun LM for new K

m15 <- lm(ln_k_rescore ~ age + EDUCATION + sex + group_early, data = df_tot)
summary(m15)
em15 <- emmeans::emmeans(m15,"group_early")
plot(em15, horiz = F, comparisons = T, main = "ln_K_rescore" )
emmeans::cld(em15)

m15b <- lm(ln_k_rescore_geom ~ age + EDUCATION + sex + group_early, data = df_tot)
summary(m15b)
em15b <- emmeans::emmeans(m15b,"group_early")
plot(em15b, horiz = F, comparisons = T, main = "ln_K_rescore_geom" )
emmeans::cld(em15b)

m15c <- lm(ln_k_rescore_geom ~ age + EDUCATION + sex + group_early + Income_tot, data = df_tot)
summary(m15c)
em15c <- emmeans::emmeans(m15c,"group_early")
plot(em15c, horiz = F, comparisons = T, main = "ln_K_rescore_geom_income" )
emmeans::cld(em15c)

##medium reward size only
#remove low consistency folks (there are 6 below 70%;47 below 80%)
#df_tot$ln_k_consistent_liberal <- df_tot$ln_k_rescore
#df_tot$ln_k_consistent_conservative <- df_tot$ln_k_rescore
#inconsistent_folks_liberal <- df_tot$MedCons < 0.7
#inconsistent_folks_conservative <- df_tot$MedCons < 0.8
#check counts format: sum(z, na.rm=TRUE)
#df_tot$ln_k_consistent_liberal[df_tot$MedCons<0.7] <- NA
#df_tot$ln_k_consistent_conservative[df_tot$MedCons<0.8] <- NA


# orig med k only -- will eventually discard remove low consistency folks (there are 6 below 70%;47 below 80%)
#df_tot$ln_k_consistent_liberal <- df_tot$ln_k_rescore
#df_tot$ln_k_consistent_conservative <- df_tot$ln_k_rescore
#inconsistent_folks_liberal <- df_tot$MedCons < 0.7
#inconsistent_folks_conservative <- df_tot$MedCons < 0.8
#check counts format: sum(z, na.rm=TRUE)
#df_tot$ln_k_consistent_liberal[df_tot$MedCons<0.7] <- NA
#df_tot$ln_k_consistent_conservative[df_tot$MedCons<0.8] <- NA

df_tot$ln_k_consistent_liberal <- df_tot$ln_k_rescore_geom
df_tot$ln_k_consistent_conservative <- df_tot$ln_k_rescore_geom
inconsistent_folks_liberal <- df_tot$geomMean_consist < 0.7
inconsistent_folks_conservative <- df_tot$geomMean_consist < 0.8
#check counts format: sum(z, na.rm=TRUE)
df_tot$ln_k_consistent_liberal[df_tot$geomMean_consist<0.7] <- NA
df_tot$ln_k_consistent_conservative[df_tot$geomMean_consist<0.8] <- NA

m16 <- lm(ln_k_consistent_liberal ~ age + EDUCATION + sex + group_early, data = df_tot)
summary(m16)
em16 <- emmeans::emmeans(m16,"group_early")
plot(em16, horiz = F, comparisons = T, main = "ln_K_consislib")
emmeans::cld(em16)

m16b <- lm(ln_k_consistent_liberal ~ age + EDUCATION + sex + group_early + Income_tot, data = df_tot)
summary(m16b)
em16b <- emmeans::emmeans(m16b,"group_early")
plot(em16b, horiz = F, comparisons = T, main = "ln_K_consislib")
emmeans::cld(em16b)

m17 <- lm(ln_k_consistent_conservative ~ age + EDUCATION + sex + group_early, data = df_tot)
summary(m17)
em17 <- emmeans::emmeans(m17,"group_early")
plot(em17, horiz = F, comparisons = T, main = "ln_K_consiscon")
emmeans::cld(em17)

m17b <- lm(ln_k_consistent_conservative ~ age + EDUCATION + sex + group_early + Income_tot, data = df_tot)
summary(m17b)
em17b <- emmeans::emmeans(m17b,"group_early")
plot(em17b, horiz = F, comparisons = T, main = "ln_K_consiscon_income")
emmeans::cld(em17b)

#remove nondiscounters?
df_tot$ln_k_consistent_liberal_nonnd <- df_tot$ln_k_consistent_liberal
nondiscounters2 <- df_tot$ln_k_consistent_liberal < -10
df_tot$ln_k_consistent_liberal_nonnd[nondiscounters] <- NA

df_tot$ln_k_consistent_cons_nonnd <- df_tot$ln_k_consistent_conservative
nondiscounters3 <- df_tot$ln_k_consistent_conservative < -10
df_tot$ln_k_consistent_cons_nonnd[nondiscounters] <- NA

m18 <- lm(ln_k_consistent_liberal_nonnd ~ age + EDUCATION + sex + group_early, data = df_tot)
summary(m18)
em18 <- emmeans::emmeans(m18,"group_early")
lm18 <- lsmeans::lsmeans(m18,"group_early")

plot(em18, horiz = F, comparisons = T, main = "ln_k_consislib_nondis")
emmeans::cld(em18)
emmeans::cld(lm18)
#m18 shows significance for early onset & ide after controlling for consistency & nondiscounters, not betweeen
#groups but as coefficients (?)


m18b <- lm(ln_k_consistent_liberal_nonnd ~ age + EDUCATION + sex + group_early + Income_tot, data = df_tot)
summary(m18b)
em18b <- emmeans::emmeans(m18b,"group_early")
lm18b <- lsmeans::lsmeans(m18b,"group_early")

plot(em18b, horiz = F, comparisons = T, main = "ln_k_consislib_nondis_income")
emmeans::cld(em18b)
emmeans::cld(lm18b)

m19 <- lm(ln_k_consistent_cons_nonnd ~ age + EDUCATION + sex + group_early, data = df_tot)
summary(m19)
em19 <- emmeans::emmeans(m19,"group_early")
plot(em19, horiz = F, comparisons = T, color = 'red', main = "ln_K_consiscon_nondis")
emmeans::cld(em19)
#m19 doesn't show anything too interesting, possibly not enough data to get clear results

m19b <- lm(ln_k_consistent_cons_nonnd ~ age + EDUCATION + sex + group_early + Income_tot, data = df_tot)
summary(m19b)
em19b <- emmeans::emmeans(m19b,"group_early")
plot(em19b, horiz = F, comparisons = T, color = 'red', main = "ln_K_consiscon_nondis")
emmeans::cld(em19b)

hist(df_tot$ln_k_consistent_cons_nonnd, breaks = 6)
hist(df_tot$ln_k_consistent_liberal_nonnd, breaks = 6)


#emmeans::cld_lm18 = emmeans::cld(lm18,
#                      alpha=0.05,
#                     Letters=letters,
#                      adjust="tukey")
#emmeans::cld_lm18$.group=gsub(" ", "", emmeans::cld_lm18$.group)

#pd = position_dodge(.8)

#p1 <- ggplot(emmeans::cld_lm18,
 #      aes(
  #       x     = group_early,
   #      y     = lsmean,
    #     color = group_early,
     #    label = .group
      # )) +
  #  facet_wrap( ~ group_early) +
  
  #geom_point(shape  = 15,
   #          size   = 4,
             #             colour = c("grey40", "grey60", "#CCCC00", "#FF9900", "#FF6600"),
    #         position = pd) +
  
  #geom_errorbar(
   # aes(ymin  =  lower.CL,
    #    ymax  =  upper.CL),
  #  width =  0.2,
   # size  =  0.7,
    #    colour = c("grey40", "grey60", "#CCCC00", "#FF9900", "#FF6600"),
    #position = pd
  #) +
#  theme_bw() +
 # theme(plot.title = element_text(size=20)) +
  #scale_x_discrete(labels=c("HC","DC","SI","eoSA","loSA")) +
  #theme(
    #axis.title.x=element_blank(),
    #axis.title.y = text(size=16), 
#    axis.title.x=element_text(size=16),
#    axis.title.y=element_text(size=16),
#    axis.text.x=element_text(size = 12),
#    axis.text.y=element_text(size = 12),
#    legend.title = element_blank(),
##    legend.text = element_text(size = 12),
#    strip.text.x = element_text(size=14),
#    plot.title = element_text(size = 18)) +
  #theme(legend.text = element_text(colour="black", size = 16)) +
  #theme(legend.title = element_text(colour="black", size = 16, face = "bold")) +
#  scale_fill_discrete(labels=c("HC: healthy controls",
                              # "DC: depressed controls",
                              # "SI: suicidal ideators",
                              # "eoSA: early-onset attempters",
                              # "laSA: late-onset attempters")) +
  # theme(axis.title.y=element_text(size=14),
  #       axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  
#  ylab("Z-scores of NEO-FFI") +
#  xlab("study groups") +
#  ggtitle("Five factors: overall group differences") +
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
#geom_text(nudge_x = 0.2,
          #nudge_y = -0.2,
          #color   = "black") 
#dev.off()

#multiple plots together
#library(grid)
#library(gridExtra)
#grid.arrange(p1,p2,p3,p4,p5,
            # layout_matrix = matrix(c(1,2,3,4,5,5), ncol=3, byrow=TRUE))

# impulsivity vars by group; univariate cors...
chars3 <- as.data.frame(df_tot[, c(26:30,32:35,42:43, 57, 66:70)])
c3 <-
  compareGroups::compareGroups(
    chars3,
    y = df_tot$group_early,
    bivar = TRUE,
    include.miss = FALSE
  )
t3 <-
  compareGroups::createTable(
    c3,
    # hide = c(sex = "FEMALE", list(race = c(
    #   "WHITE", "ASIAN PACIFIC"
    # ))),
    hide.no = 0,
    digits = 1,
    show.n = TRUE,
    show.p.mul = TRUE
  )
compareGroups::export2html(t3, "imp_measures_by_group_pluskrescores.html")


# rerun correlations across impulsivity measures/income/etc
chars4 <- as.data.frame(df_tot[, c(26:30,32:35,42, 43, 66:70, 21, 25, 23, 19, 15, 51)])
#head(just_rois)
# cormat <- cor(na.omit(chars))
# pdf_tot("trait correlations.pdf_tot", width=14, height=14)
cors3 <- psych::corr.test(chars4, use = "pairwise",method="pearson", alpha=.01)


par(mfrow=c(1,1))
bigcor <- corrplot::corrplot(cors3$r, cl.lim=c(-1,1),
         method = "shade", tl.cex = 1, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,  
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors3$p, sig.level=0.01, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")


#go crazy with histograms and boxplots
par(mfrow=c(3,4))
hist(df_tot$SPSI_ICSSUB, breaks=8, main = "SPSI_ICCSUB")
hist(df_tot$BIS_COGNIT, main = "BIS_COGNIT")
hist(df_tot$BIS_MOTOR, breaks=6, main = "BIS_MOTOR")
hist(df_tot$BIS_NONPLAN, breaks=6, main = "BIS_NONPLAN")
hist(df_tot$`UPPSP NEG URGENCY`, breaks=6, main = "UPPSP NEG")
hist(df_tot$`UPPSP POS URGENCY`, breaks=4, main =  "UPPSP POS")
hist(df_tot$`UPPSP LACK OF PREMED`, breaks=6, main = "UPPSP LPREM")
hist(df_tot$`UPPSP LACK OF PERSEV`, breaks=8, main = "UPPSP LPERS")
hist(df_tot$'ln_k', breaks = 6, main = "ln_K")
hist(df_tot$ln_k_excluding_nondiscounters, breaks=6, main = "lnK_ex_nondis")
hist(df_tot$ln_k_consistent_cons_nonnd, breaks = 6, main = "lnk_ccnd")
hist(df_tot$ln_k_consistent_liberal_nonnd, breaks = 6, main = "lnk_clnd")

boxplot(df_tot$SPSI_ICSSUB, main = "SPSI_ICSSUB")
boxplot(df_tot$BIS_COGNIT, main = "BIS_COGNIT")
boxplot(df_tot$BIS_MOTOR, main = "BIS_MOTOR")
boxplot(df_tot$BIS_NONPLAN, main = "BIS_NONPLAN")
boxplot(df_tot$'UPPSP NEG URGENCY', main = "UPPSP NEG")
boxplot(df_tot$'UPPSP POS URGENCY', main = "UPPSP POS")
boxplot(df_tot$'UPPSP LACK OF PREMED', main = "UPPSP LACK PREMED")
boxplot(df_tot$`UPPSP LACK OF PERSEV`, main = "UPPSP LACK PERSEV")
boxplot(df_tot$ln_k, main = "ln_k")
boxplot(df_tot$ln_k_excluding_nondiscounters, main = "lnK_ex_nondis")
boxplot(df_tot$ln_k_consistent_cons_nonnd, main = "lnk_ccnd")
boxplot(df_tot$ln_k_consistent_liberal_nonnd, main = "lnk_clnd")

##will need to change SES debt yes/no questions some people answered no then no (0 then 0)

df_tot_att <- df_tot[df_tot$GROUP1245 == '5',]
g1 <- ggplot(df_tot_att, aes(age_first_att, SPSI_ICSSUB)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "SPSI")

g2 <- ggplot(df_tot_att, aes(age_first_att, BIS_NONPLAN)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "SPSI")

g3 <- ggplot(df_tot_att, aes(age_first_att, BIS_MOTOR)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "SPSI")

g4 <- ggplot(df_tot_att, aes(age_first_att, BIS_COGNIT)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "SPSI")

g5 <- ggplot(df_tot_att, aes(age_first_att, BIS_TOTMEAN)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "SPSI")

df_tot$UPPSP_NEG_URGENCY <- df_tot$'UPPSP NEG URGENCY'
df_tot$UPPSP_POS_URGENCY <- df_tot$'UPPSP POS URGENCY'
df_tot$UPPSP_LACK_OF_PREMED <- df_tot$'UPPSP LACK OF PREMED'
df_tot$UPPSP_LACK_OF_PERSEV <- df_tot$'UPPSP LACK OF PERSEV'
df_tot_att <- df_tot[df_tot$GROUP1245 == '5',]
g6 <- ggplot(df_tot_att, aes(age_first_att, UPPSP_NEG_URGENCY)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "UPPSP_NEG_URGENCY")

g6l <- ggplot(df_tot_att, aes('BL WORST INTENT PLANNING', UPPSP_NEG_URGENCY)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="PLANNING", y = "UPPSP_NEG_URGENCY")


g7 <- ggplot(df_tot_att, aes(age_first_att, UPPSP_POS_URGENCY)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "UPPSP_POS_URGENCY")

g8 <- ggplot(df_tot_att, aes(age_first_att, UPPSP_LACK_OF_PREMED)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "SPSI")

g9 <- ggplot(df_tot_att, aes(age_first_att, UPPSP_LACK_OF_PERSEV)) + geom_smooth(method = 'loess', span = 2, se = T) + geom_jitter() +
  labs(x="Age at 1st attempt", y = "SPSI")
