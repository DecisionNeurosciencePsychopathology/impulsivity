## Michelle's impulsivity analyses

setwd("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/")
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
# library(corrplot)
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
#  read in data
df <- read_excel("~/Box Sync/skinner/projects_analyses/impulsivity_delaydiscounting/Impulsivity.updated.01-11-18.xlsx")

View(df)

df$ln_k <- log(df$MONEY)
# NB: there are some non-discounters in this database
df$ln_k_excluding_nondiscounters <- df$ln_k
nondiscounters <- df$ln_k < -10
df$ln_k_excluding_nondiscounters[nondiscounters] <- NA
hist(df$ln_k_excluding_nondiscounters)

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
cormat <- cor(na.omit(chars))
# pdf("trait correlations.pdf", width=14, height=14)
cors <- corr.test(chars, use = "pairwise",method="pearson", alpha=.05)

# Michelle to check all histograms for herself


corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
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

