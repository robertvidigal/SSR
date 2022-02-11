# Figure 1

library(ggplot2)
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(lubridate)
library(ggthemes)
library(dplyr) 

basecompleta <- readxl::read_excel("~/Dropbox/PhD/ARTIGOS Trabalhando/SSR/fssr/data/dados imprensa.xlsx")

df <- basecompleta[c(3,4)]
colnames(df)[2] <- "wave"
table(df$wave)

df$wave <- as.integer(df$wave) 
table(df$wave)

#criando a tabela de frequência por data
df2 <- df %>% 
  filter(wave > 0) %>%
  select(data, wave) %>%
  group_by(data) %>%
  summarize(qtde = n())

#juntando a informação de cada onda
df2 <- left_join(df2, unique(df), by = "data")

df2$wave <- case_when(
  df2$wave == 1 ~ "Survey 1",
  df2$wave == 2 ~ "Survey 2",
  df2$wave == 3 ~ "Survey 3",
  df2$wave == 4 ~ "Survey 4",
  TRUE ~ as.character(df2$wave)
)

table(df2$wave)
names(df2)

#criando as médias por período(wave) para plotar no gráfico
dMean <- df2 %>%
  group_by(wave) %>%
  summarise(MN = mean(qtde))
# content analysis results

#criando o gráfico
png(filename="~/Dropbox/PhD/ARTIGOS Trabalhando/SSR/contentplot.png", width = 700, height = 250)
df2 %>%
  mutate(data = as.Date(data)) %>%
  ggplot(aes(x=data, y=qtde)) +
  theme_light() +
  geom_line() + 
  xlab("") + ylab("Number of News Articles") + 
  theme(axis.text.x = element_text(face='bold', angle = 75, size = 10, hjust = 1),
        axis.title.y = element_text(size=14),
        strip.text = element_text(size=14)) +
  geom_hline(data = dMean, aes(yintercept = MN),linetype="dashed", color = "gray", size=1)+
  facet_wrap(~ wave, scales = "free_x", dir = "h", ncol = 4) +  
  scale_x_date(breaks=date_breaks("3 days"),
                minor_breaks = NULL,
                expand = c(0, 0),
                labels=date_format("%d/%b/%y"))
dev.off()
