---
  title: "IB177_final_project"
author: "gut_group"
date: "2023-09-26"
output: html_document
---

# load in the packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(broom)

#install.packages("palmerpenguins") 
library(palmerpenguins) 


# clean the data
mtdt<- read_csv("/Users/zehuazhou/Desktop/ib177/ib177_meta_data.csv") %>% 
  filter(measured == "yes") %>%
  filter(generation != "none") 
# change things to log formed
log_df<- mtdt %>% mutate(num_gen = as.numeric(gsub("[^0-9]", "", generation))) %>%
  mutate(type=as.factor(type)) %>%
  mutate(num_gen=as.factor(num_gen)) %>%
  mutate(log_SL=log(standard_length)) %>%
  mutate(log_GL=log(gut_length)) %>%
  mutate(log_GW=log(gut_width))
#mutate(condition=as.factor(condition))
p4 <-ggplot(log_df, aes(x = log_SL, y = log_GL,color=type)) +
  geom_point(aes(colour=factor(type), size=5, alpha=0.7)) +
  geom_smooth(method = "lm", se = FALSE,fullrange=TRUE, linewidth=2) +
  labs(x = "log_SL", y = "log_GL")
legend_title <- "Species"

p5<-p4 + scale_color_manual(legend_title,labels=c("generalist","lepidophagy","molluscivore"),values=c("#999999", "#E69F00", "#56B4E9")) + 
  xlab("Logged Standard Length (mm)") + ylab("Logged Gut Length in log (mm)") +
  theme(legend.position= "none")+ggtitle("gut length")+theme(plot.title = element_text(size=22)) 
options(repr.plot.width = 14, repr.plot.height = 8)

p5
p6 <-ggplot(log_df, aes(x = log_SL, y = log_GW,color=type)) +
  geom_point(aes(colour=factor(type), size=5, alpha=0.7)) +
  geom_smooth(method = "lm", se = FALSE, fullrange=TRUE, linewidth=2) +
  labs(x = "log_SL", y = "log_GW")

p7<- p6 + scale_color_manual(legend_title,labels=c("generalist","lepidophagy","molluscivore"),values=c("#999999", "#E69F00", "#56B4E9")) + 
  guides(size=FALSE,alpha=FALSE) +  xlab("Logged Standard Length (mm)") + ylab("Logged Gut Length in log (mm)")+
  ggtitle("gut width")+theme(plot.title = element_text(size=22))
p7
# Install patchwork package
#install.packages("patchwork")               
library("patchwork")      

p8<-p5+p7
ggsave(filename  = 'ib177_plots.png', plot =p8, width = 20, height = 10, units = 'cm', pointsize = 20)


model_sp<-glm(log_GL~type+log_SL, data = log_df)
summary(model_sp)
model_plain<-glm(log_GL~log_SL, data = log_df)
summary(model_plain)
model_sp_interact<-glm(log_GL~type*log_SL, data = log_df)
summary(model_sp_interact)

AIC(model_sp,model_plain,model_sp_interact)


