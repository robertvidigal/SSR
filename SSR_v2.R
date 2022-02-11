# ----------
# SSR BRAZIL - Feb 11
# ----------
ssr<-haven::read_sav("~/Dropbox/PhD/ARTIGOS Trabalhando/SSR/fssr/data/FSSR1.sav")
require(psych)
names(ssr)

ssr$ONDA <- as.factor(ssr$ONDA)
ssr$tratamento <- as.factor(ssr$tratamento)

# Tratamento 1: frame increasing investment in public health and education (silent).
# Tratamento 2: frame reducing expenditure on pensions (salient).

# Survey Mode (1 = telephone, 0 = online)
ssr$mode<-ifelse(ssr$ONDA==1, 1,
                 ifelse(ssr$ONDA==2,0,
                        ifelse(ssr$ONDA==3,0,1))) # 1 = telephone 0 = online
table(ssr$mode)
table(ssr$op_previdência)
table(ssr$op_previdência_AFAVOR)
describe(ssr$bolsonaro) # Higher values indicate more negative views.

ssr$antibolso<-ifelse(ssr$bolsonaro>2,1,0)
ssr$probolso<-ifelse(ssr$bolsonaro<3,1,0)

antibolso<-subset(ssr, bolsonaro>2) # 2224
probolso<-subset(ssr, bolsonaro<3) # 2130 

table(ssr$ONDA)
w1<-subset(ssr, ONDA==1) # 905
w2<-subset(ssr, ONDA==2) # 1456
w3<-subset(ssr, ONDA==3) # 775
w4<-subset(ssr, ONDA==4) # 1636

table(w1$antibolso)
table(w2$antibolso)
table(w3$antibolso)
table(w4$antibolso)

w1c <-subset(w1, tratamento==0) # 297
w1t1<-subset(w1, tratamento==1) # 307
w1t2<-subset(w1, tratamento==2) # 301

w2c <-subset(w2, tratamento==0) # 505
w2t1<-subset(w2, tratamento==1) # 473
w2t2<-subset(w2, tratamento==2) # 478

w3c <-subset(w3, tratamento==0) # 262
w3t1<-subset(w3, tratamento==1) # 261
w3t2<-subset(w3, tratamento==2) # 252

w4c <-subset(w4, tratamento==0) # 545
w4t1<-subset(w4, tratamento==1) # 553
w4t2<-subset(w4, tratamento==2) # 538

# GENDER EFFECTS BY WAVE
summary(glm(op_previdência_AFAVOR~tratamento+mulher+ensino, family="binomial", data=w1)) 
summary(glm(op_previdência_AFAVOR~tratamento+mulher, family="binomial", data=w2))
summary(glm(op_previdência_AFAVOR~tratamento+mulher+ensino, family="binomial", data=w3))
summary(glm(op_previdência_AFAVOR~tratamento+mulher+ensino, family="binomial", data=w4))

# GENDER SUBSETS
male<-subset(ssr, mulher==0)
female<-subset(ssr, mulher==1)

summary(glm(op_previdência_AFAVOR~tratamento+mode+antibolso, family="binomial", data=male)) # T1 effect
summary(glm(op_previdência_AFAVOR~tratamento+mode+antibolso, family="binomial", data=female)) # T1 larger effect

# W1
summary(glm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade+
              tratamento*mulher+tratamento*antibolso+ensino, family="binomial", data=w1)) 
# ANTIBOLSO neg and t2 sig neg

# W2
# WAVE2 no education, thus not included in the model.
summary(glm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade+
              tratamento*mulher+tratamento*antibolso, family="binomial", data=w2)) 
# ANTIBOLSO and WOMEN neg and t2 sig neg

# W3
summary(glm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade+
              tratamento*mulher+tratamento*antibolso+ensino, family="binomial", data=w3)) # 
# ANTIBOLSO neg and AGE pos
# no int, t2 is NS, and t1 is SIG

# W4
summary(glm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade+
              tratamento*mulher+tratamento*antibolso+ensino, family="binomial", data=w4)) # 
# ANTIBOLSO E WOMEN neg
# effects on the women*t1 interaction

# ---------------------------
# MODELO TABLE 2
# ---------------------------
model1<-lm(op_previdência_AFAVOR~tratamento+bolsonaro+mulher+idade+mode+
             tratamento*mulher+tratamento*mode+tratamento*antibolso, family="binomial", data=ssr)
summary(model1) # run as lm() for fit stats

stargazer::stargazer(model1, type="text")

# ----------------------
# A MULTILEVEL MODELS
# ----------------------
# 2nd-level: Survey Mode (FIXED EFFECTS)
require(plm)
fixed1<-plm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade, 
             index=c("mode"), data=ssr, model="within") # FE
summary(fixed1) # Controlling for time-invariant variables (FE).
fixef(fixed1) 

# 2nd-level: Survey Mode (FIXED EFFECTS)
fixed2<-plm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade, 
            index=c("ONDA"), data=ssr, model="within") # FE
summary(fixed2) # Controlling for time-invariant variables (FE).
fixef(fixed2) 

### with EDUCATION
model2<-lm(op_previdência_AFAVOR~tratamento+bolsonaro+mulher+idade+mode+ensino+
             tratamento*mulher+tratamento*mode+tratamento*antibolso+
             tratamento*ensino+tratamento*ensino*mulher, family="binomial", data=ssr)
summary(model2) # run as lm() for fit stats


# ----------------------
# MEAN LEVELS
# ----------------------
require(psych)
describe(w1c$op_previdência_AFAVOR) ### 60%
describe(w1t1$op_previdência_AFAVOR) # 69%
describe(w1t2$op_previdência_AFAVOR) # 44%

describe(w2c$op_previdência_AFAVOR) ### 48%
describe(w2t1$op_previdência_AFAVOR) # 54%
describe(w2t2$op_previdência_AFAVOR) # 37%

describe(w3c$op_previdência_AFAVOR) ### 36%
describe(w3t1$op_previdência_AFAVOR) # 41%
describe(w3t2$op_previdência_AFAVOR) # 35%

describe(w4c$op_previdência_AFAVOR) ### 48%
describe(w4t1$op_previdência_AFAVOR) # 60%
describe(w4t2$op_previdência_AFAVOR) # 46%

# ----------------------
### PAIRWISE COMPARISONS
# ----------------------

# W1
w1ct1<-t.test(w1c$op_previdência_AFAVOR,  w1t1$op_previdência_AFAVOR) # sig
w1ct2<-t.test(w1c$op_previdência_AFAVOR,  w1t2$op_previdência_AFAVOR) # sig
w1t1t2<-t.test(w1t1$op_previdência_AFAVOR, w1t2$op_previdência_AFAVOR) # sig .01
summary(glm(op_previdência_AFAVOR~tratamento+bolsonaro+mulher+idade, family="binomial", data=w1))
# Both frames different from control and each other.

# W2
w2ct1<-t.test(w2c$op_previdência_AFAVOR,  w2t1$op_previdência_AFAVOR) # NS
w2ct2<-t.test(w2c$op_previdência_AFAVOR,  w2t2$op_previdência_AFAVOR) # sig 
w2t1t2<-t.test(w2t1$op_previdência_AFAVOR, w2t2$op_previdência_AFAVOR) # sig
summary(glm(op_previdência_AFAVOR~tratamento+bolsonaro+mulher+idade, family="binomial", data=w2))
# Only Silent Frame not different from control

# W3
w3ct1<-t.test(w3c$op_previdência_AFAVOR,  w3t1$op_previdência_AFAVOR) # NS
w3ct2<-t.test(w3c$op_previdência_AFAVOR,  w3t2$op_previdência_AFAVOR) # NS
w3t1t2<-t.test(w3t1$op_previdência_AFAVOR, w3t2$op_previdência_AFAVOR) # NS
summary(glm(op_previdência_AFAVOR~tratamento+bolsonaro+mulher+idade, family="binomial", data=w3))
# None significant

# W4
w4ct1<-t.test(w4c$op_previdência_AFAVOR,  w4t1$op_previdência_AFAVOR) # sig
w4ct2<-t.test(w4c$op_previdência_AFAVOR,  w4t2$op_previdência_AFAVOR) # NS 
w4t1t2<-t.test(w4t1$op_previdência_AFAVOR, w4t2$op_previdência_AFAVOR) # sig
summary(glm(op_previdência_AFAVOR~tratamento+bolsonaro+mulher+idade, family="binomial", data=w3))
# Silent Frame different but Salient not different from control anymore

require(dplyr)
### # Experimental Results
tab1 = ssr %>% 
  group_by(ONDA, tratamento, op_previdência_AFAVOR) %>%
  summarise(Freq = n()) %>%
  mutate(Perc = round(Freq/sum(Freq),2)) %>%
  dplyr::filter(op_previdência_AFAVOR == 1)
tab0 = ssr %>% 
  group_by(ONDA, tratamento) %>%
  summarise(N = n()) %>%
  mutate(lower95 = qbeta(0.025, (N/2)+1, N-(N/2)+1)) %>%
  mutate(upper95 = qbeta(0.975, (N/2+1), N-(N/2)+1)) %>%
  mutate(IC95 = (upper95 - lower95)/2) %>%
  mutate(lower88 = qbeta(0.060, (N/2)+1, N-(N/2)+1)) %>%
  mutate(upper88 = qbeta(0.940, (N/2+1), N-(N/2)+1)) %>%
  mutate(IC88 = (upper88 - lower88)/2) %>%
  select(ONDA, tratamento, IC95, IC88)

tab1; tab0

tab2 <- left_join(tab1, tab0) %>% 
  mutate(min95 = Perc-IC95, 
         max95 = Perc+IC95,
         min88 = Perc-IC88,
         max88 = Perc+IC88)
levels(tab2$ONDA) <- c("Survey 1 \n (post-election)", "Survey 2 \n (inauguration)", "Survey 3 \n (bill introduced)", "Survey 4 \n (bill approval)")


levels(tab2$tratamento) <- c("Control",  "Increase Investiment \n (Silent Frame)", "Reduce Expenditure \n (Salient Frame)")
tab2$tratamento
# Over time the salient frame loses significance.
# Silent only significant in the last wave.

#  Plot: Means differences (FIGURE 3)
require(ggplot2)
jpeg(filename="~/Dropbox/PhD/ARTIGOS Trabalhando/SSR/meansplot.png", units="in", width = 5, height = 5, res=300)
ggplot(tab2, aes(x=ONDA, y=Perc, group=tratamento, shape=tratamento, color=tratamento))+ 
  theme_light() +
  geom_point(size=2, position=position_dodge(.15)) + 
  geom_errorbar(aes(ymin=Perc-IC95, ymax=Perc+IC95), width=0, position=position_dodge(.15)) +
  geom_errorbar(aes(ymin=Perc-IC88, ymax=Perc+IC88), width=0, size=1, position=position_dodge(.15)) +
  scale_y_continuous(limits = c(.0,.80), 
                     labels = scales::percent) +
  scale_color_manual(values = c("#7e7f80","#a8a9ab", "#000000")) +
  labs(x="", y="% Support Social Security Reform") + 
  theme(legend.position = "top", panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank(), axis.text.x = element_text(face='bold', size=10))
dev.off()

#################
### EFFECT SIZES
#################
# We can also calculate r person (effect size).
# The value of t is stored in our model as a variable called statistic[[1]] 
# and the degrees of freedom are stored as parameter[[1]].
# According to Cohen (1988, 1992 Statistics for social and behavioral sciences), 
# the effect size is LOW if r varies around 0.1, 
# MEDIUM if r varies around 0.3, 
# and LARGE if r varies more than 0.5.
ind.t.test<-t.test(ideology ~ homem, databr)
t<-ind.t.test$statistic[[1]]
df<-ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df)) # Rosenthal, 1991; Rosnow & Rosenthal, 2005
round(r, 3) # small effect size

# W1 vs W2 vs W3 vs W4 Effect Size
# T1 vs CONTROL (SALIENT)
round(sqrt(w1ct1$statistic[[1]]^2/(w1ct1$statistic[[1]]^2+w1ct1$parameter[[1]])),3) # .096 W1
round(sqrt(w2ct1$statistic[[1]]^2/(w2ct1$statistic[[1]]^2+w2ct1$parameter[[1]])),3) # .06 W2
round(sqrt(w3ct1$statistic[[1]]^2/(w3ct1$statistic[[1]]^2+w3ct1$parameter[[1]])),3) # .057 W3
round(sqrt(w4ct1$statistic[[1]]^2/(w4ct1$statistic[[1]]^2+w4ct1$parameter[[1]])),3) # .127 W4
# T2 vs CONTROL (SILENT)
round(sqrt(w1ct2$statistic[[1]]^2/(w1ct2$statistic[[1]]^2+w1ct2$parameter[[1]])),3) # .164 W1
round(sqrt(w2ct2$statistic[[1]]^2/(w2ct2$statistic[[1]]^2+w2ct2$parameter[[1]])),3) # .104W2
round(sqrt(w3ct2$statistic[[1]]^2/(w3ct2$statistic[[1]]^2+w3ct2$parameter[[1]])),3) # .006 W3
round(sqrt(w4ct2$statistic[[1]]^2/(w4ct2$statistic[[1]]^2+w4ct2$parameter[[1]])),3) # .014 W4
# T1 vs T2
round(sqrt(w1t1t2$statistic[[1]]^2/(w1t1t2$statistic[[1]]^2+w1t1t2$parameter[[1]])),3) # .258 W1
round(sqrt(w2t1t2$statistic[[1]]^2/(w2t1t2$statistic[[1]]^2+w2t1t2$parameter[[1]])),3) # .163 W2
round(sqrt(w3t1t2$statistic[[1]]^2/(w3t1t2$statistic[[1]]^2+w3t1t2$parameter[[1]])),3) # .062 W3
round(sqrt(w4t1t2$statistic[[1]]^2/(w4t1t2$statistic[[1]]^2+w4t1t2$parameter[[1]])),3) # .142 W4

# ---------------------------
# 3-way INTERACTION MODEL
# ---------------------------
# ANTIBOLSO MODEL
summary(glm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade+mode+
              mode*tratamento+
              tratamento*antibolso+
              mode*tratamento*antibolso, family="binomial", data=ssr)) 
# No Effects on the triple interaction.

# GENDER MODEL
# ANTIBOLSO MODEL
summary(glm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade+mode+
              mode*tratamento+
              tratamento*antibolso+
              mode*tratamento*antibolso, family="binomial", data=ssr)) # No Effects on the triple interaction.
# No Effects on the triple interaction.

# ---------------------------
### APPENDIX
# ---------------------------
# FULL MODEL
fullmodel<-glm(op_previdência_AFAVOR~tratamento+antibolso+mulher+idade+ONDA+
                 ONDA*tratamento+
                 tratamento*mulher+
                 tratamento*antibolso+
                 ONDA*tratamento*mulher+
                 ONDA*tratamento*antibolso, family="binomial", data=ssr)
summary(fullmodel) # run as lm() for fit stats
1 - (4759.8/6035.7) # pseudo-R^2

# Waves 2 and 4 show triple int effects with gender and treatment type.
# (no education due to wave2 other we lose data)
# running the model with education (without W2) does not change the results.

#  The triple interactions are borderline significant and negative 
# for waves 2 and 4 with the T2 treatment (ps=.054 and .046).  

modelfull2<-glm(op_previdência_AFAVOR~tratamento+bolsonaro+mulher+idade+ONDA+
                  tratamento*mulher+
                  tratamento*ONDA+
                  ONDA*mulher+
                  mulher*tratamento*ONDA, family="binomial", data=ssr)
summary(modelfull2)


#### #### #### #### 
#### PRESS RESULTS #
#### #### #### #### 
source("~/Dropbox/PhD/ARTIGOS Trabalhando/SSR/PRESS_v1.R")
#
pressdf <- readxl::read_excel("~/Downloads/pressdf1819.xlsx")
names(pressdf)

pressdf %>% group_by(data) %>% summarise(n())
pressdf %>% group_by(periodo) %>% summarise(n())

table(pressdf$periodo) # 1-4 Surveys, 0 is not in the study window
pressdf2<-pressdf %>% group_by(periodo) %>% group_by(formato) %>% filter(periodo!=0)

pressdf2$wave<-as.numeric(pressdf2$periodo)

table(pressdf2$assunto)
table(pressdf2$veiculo); table(pressdf2$cod_veic)
table(pressdf2$caderno)
table(pressdf2$tiragem) # ?
table(pressdf2$formato)
#table(pressdf$`link-fabrica`) # href

frontpages<-subset(pressdf2, formato=="Chamada de capa" | 
                     formato=="Chamada de capa - Caderno" | 
                     formato=="Chamada de Capa - Principal")
330/2864 # 11.5% front page overall

reportagens<-subset(pressdf2, formato=="Reportagem")
1216/2864 # 42% reports overall

# S1-S4 Front Pages
frontpages %>% group_by(periodo) %>% summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100)

# S1-S4 Front Reports
reportagens %>% group_by(periodo) %>% summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100)

# How many articles per day on average?
dMean

### FIGURE 2
presstab<-data.frame(
  "wave"=c(rep(c("Survey 1", "Survey 2", "Survey 3", "Survey 4", "Total"), 6)),
  "topic"=c(rep(1, 5), rep(2, 5), rep(3, 5),
            rep(4, 5), rep(5, 5), rep(6, 5)),
  "meanp"=c(5.0, 6.6,	14.5,	3.3, 29.4,
            4.8, 4.8, 8.5, 4.1, 22.2,
            3.2, 6.5, 9.9, 0.0,  19.6,
            0, 0, 13.3, 3.4, 16.7,
            5.1, 4.5, 0, 0, 9.6,
            0, 0, 0, 2.6, 2.6))
  #"totaltopic"=c(rep(29.4, 4), rep(22.2, 4), rep(19.6, 4), rep(16.7, 4), rep(9.6, 4), rep(2.6, 4)),
  #"totalwave"=c(rep(c(18.1, 22.4, 46.2, 13.3, 100), 6))

presstab$topic<-factor(presstab$topic, 
                       levels=c(1:6), 
                       labels=c("Political Debate", "Macroeconomic Impact", "New SSR Rules & Reasoning", "Legislative Process", 
                                               "Government Deliberation", "Reforms at State & Municipal Levels"))


jpeg(filename="~/Dropbox/PhD/ARTIGOS Trabalhando/SSR/fig2.png", units="in", width = 8, height = 4, res=300)

ggplot(presstab, aes(x=wave, y=meanp, fill=topic)) + geom_bar(stat="identity", colour="black", width=.5) + theme_minimal() +
  scale_x_discrete(labels = c("Survey 1 \n (Dec 2018)", "Survey 2 \n (Jan 2019)", "Survey 3 \n (Mar 2019)", "Survey 4 \n (Dec 2019)", "Total")) +
  ylab('% total by survey') +
  theme(legend.title = element_blank()) +
  theme(legend.position = "right") +
  theme(legend.text = element_text(size=10)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text=element_text(color="black", size=12)) +
  theme(axis.title.y=element_text(size=14),   axis.text.x = element_text(size=10)) +
  scale_fill_manual(values=gray.colors(6, start = 0, end = 1))
dev.off()

###############
# Text-analysis
###############
#table(pressdf$titulo)
#table(pressdf$texto)

### QUICK TOPIC ANALYSIS

# PLOT WITH PERCENTAGES ON IT
#require(ggrepel)
#ggplot(tab2, aes(x=ONDA, y=Perc, group = tratamento, shape=tratamento, color=tratamento))+ 
#  theme_light() +
#  geom_errorbar(aes(ymin=Perc-IC95, ymax=Perc+IC95), width=0, position=position_dodge(.15)) +
#  geom_errorbar(aes(ymin=Perc-IC88, ymax=Perc+IC88), width=0, size=1, position=position_dodge(.15)) +
#  scale_y_continuous(limits = c(.20,.80), 
#                     labels = scales::percent) +
#  geom_point(size=2, position=position_dodge(.15)) + 
#  scale_color_manual(values = c("#7e7f80","#000000", "#a8a9ab")) +
#  
#  labs(x="", y="% Approving Pension Reform") + 
#  theme(legend.position = "top", panel.border = element_blank(), panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
#  geom_text_repel(
#    aes(label = paste0(Perc*100,"%")), color = 'black',
#    size = 2, max.iter = 5000, nudge_x=-.15,
#    min.segment.length = 11.95) +
#  theme(legend.title = element_blank())