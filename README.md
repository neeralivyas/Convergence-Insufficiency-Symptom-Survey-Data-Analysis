# Convergence Insufficiency Symptom Survey Data Analysis

# I characterized the symptom profile of adolescent concussion patients using the convergence insufficiency symptom survey (CISS) and explored if CISS symptom reporting is impacted by visual function, sex, time since concussion (TSC), and method of administration.

library(dplyr)
library(stringr)
library(openxlsx)
library(purrr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(gmodels)
library(eulerr)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(writexl)
library(car)
library(scales)

### read in data ###
all.data.df <- read.csv(file = "CISS_Master.csv", header = TRUE)
View(all.data.df)

data.df <- all.data.df[,c(1:34)]

Concussed.df <- subset(all.data.df, Concussed == 1)
View(Concussed.df)

Concussed.df$npc_fail <- ifelse(Concussed.df$NPC > 7, 1, 0)

Concussed.df <- Concussed.df %>%
  mutate(Worse_Eye_AA = ifelse(AA_OS_D < AA_OD_D,
                            AA_OS_D, AA_OD_D))

Concussed.df <- Concussed.df %>% mutate(Phase = ifelse(Concussed.df$TSC > 84, "Chronic", "Subacute"))

View(Concussed.df)

Control.df <- subset(all.data.df, Concussed == 2)
View(Control.df)

View(data.df)

Concussed.df$abnormal.vf <- ifelse(Concussed.df$npc_fail == 1 | Concussed.df$AA_FAIL == 1, "YES", "NO")
Concussed.df$abnormal.vf <- as.factor(Concussed.df$abnormal.vf)

all.data.df$abnormal.vf <- ifelse(all.data.df$npc_fail == 1 | all.data.df$AA_FAIL == 1, "YES", 
                                  ifelse(!(all.data.df$npc_fail == 1 | all.data.df$AA_FAIL == 1), "NO", NA))

NPC_AA <- data.frame(table(Concussed.df$NPC, Concussed.df$AA_FAIL))
View(NPC_AA)

table(Concussed.df$abnormal.vf)
View(NPC)

## symptoms ## 

often.always.fun <- function(question.vec){
  helper.fun <- function(x){
    if( x == 3 | x == 4){
      1
    }else{
      0
    }
  }
  sapply(question.vec, helper.fun)
}

Concussed.df <- Concussed.df %>% mutate (Tired_Eyes = often.always.fun(Q1))%>%
  mutate (Eye_Discomfort = often.always.fun(Q2)) %>%
  mutate (Headaches = often.always.fun(Q3)) %>%
  mutate (Sleepy = often.always.fun(Q4)) %>%
  mutate (Lose_Concentration = often.always.fun(Q5)) %>%
  mutate (Trouble_Remembering = often.always.fun(Q6)) %>%
  mutate (Double_Vision = often.always.fun(Q7)) %>%
  mutate (Words_Move = often.always.fun(Q8)) %>%
  mutate (Read_Slowly = often.always.fun(Q9)) %>% 
  mutate (Eyes_Hurt = often.always.fun(Q10)) %>%
  mutate (Eyes_Sore = often.always.fun(Q11)) %>%
  mutate (Eyes_Pulling = often.always.fun(Q12)) %>%
  mutate (Words_Blurring = often.always.fun(Q13)) %>%
  mutate (Lose_Place = often.always.fun(Q14)) %>%
  mutate (Reread = often.always.fun(Q15))

Control.df <- Control.df %>% mutate (Tired_Eyes = often.always.fun(Q1))%>%
  mutate (Eye_Discomfort = often.always.fun(Q2)) %>%
  mutate (Headaches = often.always.fun(Q3)) %>%
  mutate (Sleepy = often.always.fun(Q4)) %>%
  mutate (Lose_Concentration = often.always.fun(Q5)) %>%
  mutate (Trouble_Remembering = often.always.fun(Q6)) %>%
  mutate (Double_Vision = often.always.fun(Q7)) %>%
  mutate (Words_Move = often.always.fun(Q8)) %>%
  mutate (Read_Slowly = often.always.fun(Q9)) %>% 
  mutate (Eyes_Hurt = often.always.fun(Q10)) %>%
  mutate (Eyes_Sore = often.always.fun(Q11)) %>%
  mutate (Eyes_Pulling = often.always.fun(Q12)) %>%
  mutate (Words_Blurring = often.always.fun(Q13)) %>%
  mutate (Lose_Place = often.always.fun(Q14)) %>%
  mutate (Reread = often.always.fun(Q15))
#### TOTAL ####

total.symptoms <- Concussed.df[,c(38:52)]
View(total.symptoms)

percent.fun <- function(vec, x){
  
  helper.fun <- function(y, x){
    y/x
  }
  mapply(helper.fun, vec, x)
}


sum.total <- (as.data.frame(mapply(sum,total.symptoms)))

View(total.symptoms)
View(sum.total)


sum.total$Question <- c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                        "Lose Concentration", "Trouble Remembering", "Double Vision", "Words Moving",
                        "Reading Slowly","Eyes Hurt", "Eyes Sore", "Eyes Pulling",
                        "Words Blurring", "Lose Place", "Re-Read Words")
colnames(sum.total) <- c("N", "Question")
sum.total <- sum.total %>% mutate(Frequency = percent.fun(N, 210))

sum.total

sum.total<- sum.total %>% arrange(desc(N))
sum.total$Question <- factor(sum.total$Question, levels = c(sum.total$Question))

subscale.fx <- function(symptom.vec){
  helper.function <- function(x){
    if(x == "Lose Concentration"| x== "Trouble Remembering"|
       x== "Reading Slowly"| x== "Lose Place"| x== "Re-Read Words"){
      "CISS-P"
    }else if(x== "Words Blurring"| x == "Double Vision"| x== "Words Moving"){
      "CISS-V"
    }else{
      "CISS-S"
    }
  }
  sapply(symptom.vec, helper.function)
}

sum.total <- sum.total %>% mutate(Subscale = subscale.fx(Question))

sum.total$Subscale <- factor(sum.total$Subscale, levels = c("CISS-S", "CISS-P", "CISS-V"))

View(sum.total)

sum.total_CISSS <- subset(sum.total, Subscale == "CISS-S")
sum.total_CISSP <- subset(sum.total, Subscale == "CISS-P")
sum.total_CISSV <- subset(sum.total, Subscale == "CISS-V")

quantile(sum.total_CISSS$Frequency)
quantile(sum.total_CISSP$Frequency)
quantile(sum.total_CISSV$Frequency)

quantile(c(51.7, 45.7, 48.3, 47.4, 35.5))
quantile(c(42.2, 35.3, 37.9, 33.6, 34.5, 25, 14.7))
quantile(c(34.5, 38.8, 19.8))

total.ggp <- ggplot(sum.total, aes(x = reorder(Question, -Frequency), y = Frequency, fill = Subscale)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 0.5),
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm")
  ) +
  labs(x = "Symptoms", y = "Frequency") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_fill_manual(values = c("darkgreen", "darkblue", "darkred"))
total.ggp


Total_Frequencies <- ggsave("Total Symptom Frequency.png",
       scale = 1,
       width = 6.5,
       height = 4,
       units = c("in"))


############# Frequencies for Abnormal Visual Function

abnormal_visual_function <- subset(Concussed.df, npc_fail == 1 | AA_FAIL == 1)


## symptoms ## 
## if answered 3 or 4, mark as 1, otherwise mark as 0 ##

## abnormal vs normal
often.always.fun <- function(question.vec){
  helper.fun <- function(x){
    if( x == 3 | x == 4){
      1
    }else{
      0
    }
  }
  sapply(question.vec, helper.fun)
}

## changing from Q1, Q2, etc to descriptors of what they actually are ##
abnormal_visual_function <- abnormal_visual_function %>% mutate (Tired_Eyes = often.always.fun(Q1))%>%
  mutate (Eye_Discomfort = often.always.fun(Q2)) %>%
  mutate (Headaches = often.always.fun(Q3)) %>%
  mutate (Sleepy = often.always.fun(Q4)) %>%
  mutate (Lose_Concentration = often.always.fun(Q5)) %>%
  mutate (Trouble_Remembering = often.always.fun(Q6)) %>%
  mutate (Double_Vision = often.always.fun(Q7)) %>%
  mutate (Words_Move = often.always.fun(Q8)) %>%
  mutate (Read_Slowly = often.always.fun(Q9)) %>% 
  mutate (Eyes_Hurt = often.always.fun(Q10)) %>%
  mutate (Eyes_Sore = often.always.fun(Q11)) %>%
  mutate (Eyes_Pulling = often.always.fun(Q12)) %>%
  mutate (Words_Blurring = often.always.fun(Q13)) %>%
  mutate (Lose_Place = often.always.fun(Q14)) %>%
  mutate (Reread = often.always.fun(Q15))


#### Abnormal Visual Function Symptom Frequency####

total.abnormal.symptoms <- abnormal_visual_function[,c(35:49)]
View(total.abnormal.symptoms)


percent.fun <- function(vec, x){
  
  helper.fun <- function(y, x){
    y/x
  }
  mapply(helper.fun, vec, x)
}


sum.abnormal.total <- (as.data.frame(mapply(sum,total.abnormal.symptoms)))


sum.abnormal.total$Question <- c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                        "Lose Concentration", "Trouble Remembering", "Double Vision", "Words Moving",
                        "Reading Slowly","Eyes Hurt", "Eyes Sore", "Eyes Pulling",
                        "Words Blurring", "Lose Place", "Re-Read Words")
colnames(sum.abnormal.total) <- c("N", "Question", "Frequency", "Subscale")
sum.abnormal.total <- sum.abnormal.total %>% mutate(Frequency = percent.fun(N, 164))

sum.abnormal.total$Question <- factor(sum.abnormal.total$Question, levels = c(sum.abnormal.total$Question))

### Separating based on subscales ###
subscale.fx <- function(symptom.vec){
  helper.function <- function(x){
    if(x == "Lose Concentration"| x== "Trouble Remembering"|
       x== "Reading Slowly"| x== "Lose Place"| x== "Re-Read Words"){
      "CISS-P"
    }else if(x== "Words Blurring"| x == "Double Vision"| x== "Words Moving"){
      "CISS-V"
    }else{
      "CISS-S"
    }
  }
  sapply(symptom.vec, helper.function)
}

sum.abnormal.total <- sum.abnormal.total %>% mutate(Subscale = subscale.fx(Question))

sum.abnormal.total$Subscale <- factor(sum.abnormal.total$Subscale, levels = c("CISS-S", "CISS-P", "CISS-V"))


sum.abnormal.total

sum.abnormal.total$Question <- reorder(sum.abnormal.total$Question, -sum.abnormal.total$Frequency)

total.abnormal.ggp <- ggplot(sum.abnormal.total, aes(x = reorder(Question, -Frequency), y = Frequency, fill = Subscale)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 0.5),
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(0.5, "cm")
  ) +
  labs(x = "Symptoms", y = "Frequency") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_fill_manual(values = c("darkgreen", "darkblue", "darkred"))
total.abnormal.ggp


ggsave("Total Abnormal Symptom Frequency.png",
       scale = 1,
       width = 6.5,
       height = 4,
       units = c("in"))

#### Normal Visual Function Symptom Frequency####

normal_visual_function <- subset(Concussed.df, !(npc_fail == 1 | AA_FAIL == 1))
View(normal_visual_function)


## symptoms ## 
## if answered 3 or 4, mark as 1, otherwise mark as 0 ##
often.always.fun <- function(question.vec){
  helper.fun <- function(x){
    if( x == 3 | x == 4){
      1
    }else{
      0
    }
  }
  sapply(question.vec, helper.fun)
}

## changing from Q1, Q2, etc to descriptors of what they actually are ##
normal_visual_function <- normal_visual_function %>% mutate (Tired_Eyes = often.always.fun(Q1))%>%
  mutate (Eye_Discomfort = often.always.fun(Q2)) %>%
  mutate (Headaches = often.always.fun(Q3)) %>%
  mutate (Sleepy = often.always.fun(Q4)) %>%
  mutate (Lose_Concentration = often.always.fun(Q5)) %>%
  mutate (Trouble_Remembering = often.always.fun(Q6)) %>%
  mutate (Double_Vision = often.always.fun(Q7)) %>%
  mutate (Words_Move = often.always.fun(Q8)) %>%
  mutate (Read_Slowly = often.always.fun(Q9)) %>% 
  mutate (Eyes_Hurt = often.always.fun(Q10)) %>%
  mutate (Eyes_Sore = often.always.fun(Q11)) %>%
  mutate (Eyes_Pulling = often.always.fun(Q12)) %>%
  mutate (Words_Blurring = often.always.fun(Q13)) %>%
  mutate (Lose_Place = often.always.fun(Q14)) %>%
  mutate (Reread = often.always.fun(Q15))



total.normal.symptoms <- normal_visual_function[,c(35:49)]
View(total.normal.symptoms)


percent.fun <- function(vec, x){
  
  helper.fun <- function(y, x){
    y/x
  }
  mapply(helper.fun, vec, x)
}


sum.total.normal <- (as.data.frame(mapply(sum,total.normal.symptoms)))


sum.total.normal$Question <- c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                        "Lose Concentration", "Trouble Remembering", "Double Vision", "Words Moving",
                        "Reading Slowly","Eyes Hurt", "Eyes Sore", "Eyes Pulling",
                        "Words Blurring", "Lose Place", "Re-Read Words")
colnames(sum.total.normal) <- c("N", "Question")
sum.total.normal <- sum.total.normal %>% mutate(Frequency = percent.fun(N, 46))

sum.total.normal$Question <- factor(sum.total.normal$Question, levels = c(sum.total.normal$Question))

### Separating based on subscales ###
subscale.fx <- function(symptom.vec){
  helper.function <- function(x){
    if(x == "Lose Concentration"| x== "Trouble Remembering"|
       x== "Reading Slowly"| x== "Lose Place"| x== "Re-Read Words"){
      "CISS-P"
    }else if(x== "Words Blurring"| x == "Double Vision"| x== "Words Moving"){
      "CISS-V"
    }else{
      "CISS-S"
    }
  }
  sapply(symptom.vec, helper.function)
}

sum.total.normal <- sum.total.normal %>% mutate(Subscale = subscale.fx(Question))

sum.total.normal$Subscale <- factor(sum.total$Subscale, levels = c("CISS-S", "CISS-P", "CISS-V"))


sum.total.normal

sum.total.normal$Question <- reorder(sum.total.normal$Question, -sum.total.normal$Frequency)

total.normal.ggp <- ggplot(sum.total.normal, aes(x = reorder(Question, -Frequency), y = Frequency, fill = Subscale)) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 0.5),
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "none",
    legend.key.size = unit(0.5, "cm")
  ) +
  labs(x = "Symptoms", y = "Frequency") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_fill_manual(values = c("darkgreen", "darkblue", "darkred"))
total.normal.ggp


ggsave("Total Normal Symptom Frequency.png",
       scale = 1,
       width = 6.5,
       height = 4,
       units = c("in"))


#### TEMPORALITY ####
########## Creating a new column for subacute <= 12 weeks vs chronic > 12 weeks to 1 year
Concussed.df <- Concussed.df %>% mutate(Phase = ifelse(Concussed.df$TSC > 84, "Chronic", "Subacute"))

View(Concussed.df)

subacute.df <- Concussed.df %>% filter(Phase == "Subacute")
View(subacute.df)
sub.symptoms <- subacute.df[,c(35:49)]
View(sub.symptoms)
chronic.df <- Concussed.df %>% filter(Phase == "Chronic")
chronic.symptoms <- chronic.df[,c(35:49)]
View(chronic.df)

###### Subacute vs Chronic

TSC.df <- data.frame(
  Response = c(
    subacute.df$CISS_total_15,
    chronic.df$CISS_total_15),
  Type = rep(c("Sub-acute", "Chronic"),times = c(91, 119)))

View(TSC.df)

View(subacute.df)
View(chronic.df)

TSC_graph <- ggplot(TSC.df, aes(x = Type, y = Response, fill = Type)) +
  geom_boxplot(size = 1.25, outlier.shape = NA) +
  scale_fill_manual(values = c("white","darkorange3")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, color = "black")
  )

TSC_graph

TSC <- ggsave("Subacute vs Chronic.png",
              scale = 1,
              width = 1.75,
              height = 1.75,
              units = c("in"))

#### SEX ####

table(Concussed.df$Gender)
girl.df <- Concussed.df %>% filter(Gender == 2)
View(girl.df)
girl.symptoms <- girl.df[,c(35:49)]
boy.df <- Concussed.df %>% filter(Gender == 1)
boy.symptoms <- boy.df[,c(35:49)]

##### Sex Graphs

Sex.total <- data.frame(
  Response = c(
    girl.df$CISS_total_15,
    boy.df$CISS_total_15),
  Type = rep(c("Female", "Male"),times = c(142, 68)))

Sex_Graph <- ggplot(Sex.total, aes(x = Type, y = Response, fill = Type)) +
  geom_boxplot(size = 1.25, outlier.shape = NA) +
  scale_fill_manual(values = c("green4","green3")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, color = "black")
  )
Sex_Graph
Sex_Total <- ggsave("Sex.png",
                    scale = 1,
                    width = 3,
                    height = 3,
                    units = c("in"))



#### CLINIC TYPE ####

table(Concussed.df$Clinic_type)

MDCC.df <- Concussed.df %>% filter(Clinic_type == 1)
View(MDCC.df)
MDCC.symptoms <- MDCC.df[,c(35:49)]

table(MDCC.df$Phase)
table(Concussed.df$Phase)

Ref.df <- Concussed.df %>% filter(Clinic_type == 2)
View(Ref.df)
Ref.symptoms <- Ref.df[,c(35:49)]

table(Ref.df$Phase)

###### Clinic Type Graph

Clinic.df <- data.frame(
  Total_Score = c(
    MDCC.df$CISS_total_15,
    Ref.df$CISS_total_15),
  Type = rep(c("MDCC", "Referred"),times = c(106, 104)))

View(Clinic.df)

Clinic_graph <- ggplot(Clinic.df, aes(x = Type, y = Total_Score, fill = Type)) +
  geom_boxplot(size = 1.25) +
  scale_fill_manual(values = c("slategray","darkgreen")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, color = "black")
  )

Clinic_graph

Clinic_Type <- ggsave("Clinic Type.png",
                      scale = 1,
                      width = 3,
                      height = 3,
                      units = c("in"))

#### mean subscale scores ####

# (19) 1"Tired Eyes", (20)2"Eye Discomfort", (21)3"Headaches", (22)4"Sleepy",
# (23)5"Lose Concentration", (24)6"Trouble Remembering", (25)7"Double Vision", (26)8"Words Moving",
# (27)9"Reading Slowly",(28)10"Eyes Hurt", (29)11"Eyes Sore", (30)12"Eyes Pulling",
# (31)13"Words Blurring", (32)14"Lose Place", (33)15"Re-Read Words")

### CISS-V ### Words blurring, double vision, words moving
CISSV.df <- Concussed.df[,c(31,25,26)]
CISSV.df <- CISSV.df %>% rowwise() %>% mutate(CISSV.total = mean(Q13,Q7,Q8))
View(CISSV.df)

### CISS-P ### "Lose Concentration", "Trouble Remembering","Reading Slowly", "Lose Place", "Re-Read Words"
CISSP.df <- Concussed.df[,c(23,24,27,32,33)]
CISSP.df <- CISSP.df %>% rowwise() %>% mutate(CISSP.total = sum(Q5,Q6,Q9,Q14,Q15))


### CISS-S ### "Sleepy" "Tired Eyes", "Eye Discomfort", "Headaches","Eyes Hurt", "Eyes Sore", "Eyes Pulling"
CISSS.df <- Concussed.df[,c(19,20,21,22,28,29,30)]
CISSS.df <- CISSS.df %>% rowwise() %>% mutate(CISSS.total = sum(Q1,Q2,Q3,Q4,Q10, Q11,Q12))


### Subscales Median

Subscales.df <- data.frame(
  Response = c(
    CISSS.df$Q1,
    CISSS.df$Q2,
    CISSS.df$Q3,
    CISSS.df$Q4,
    CISSS.df$Q10,
    CISSS.df$Q11,
    CISSS.df$Q12,
    CISSP.df$Q5,
    CISSP.df$Q6,
    CISSP.df$Q9,
    CISSP.df$Q14,
    CISSP.df$Q15,
    CISSV.df$Q7,
    CISSV.df$Q8,
    CISSV.df$Q13),
  Subscale = rep(c("CISS-S", "CISS-P", "CISS-V"),times = c(1470, 1050, 630)))


Subscales_graph <- ggplot(Subscales.df, aes(x = Subscale, y = Response, fill = Subscale)) +
  geom_boxplot(size = 1.25, outlier.shape = NA) +
  scale_fill_manual(values = c("lightblue1", "lightblue3", "lightblue4")) +
  theme_bw() +
  labs(x = "", y = "Values") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, color = "black")
  )

Subscales_graph


Subscales <- ggsave("Subscales Concussed.png",
       scale = 1,
       width = 3,
       height = 3,
       units = c("in"))


########### Visual Function
VF.df <- data.frame(
  Response = c(
    abnormal_visual_function$CISS_total_15,
    normal_visual_function$CISS_total_15),
  Type = rep(c("Abnormal", "Normal"),times = c(153, 57)))

VF_graph <- ggplot(VF.df, aes(x = Type, y = Response, fill = Type)) +
  geom_boxplot(size = 1.25, outlier.shape = NA) +
  scale_fill_manual(values = c("goldenrod3","goldenrod1")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 8, color = "black")
  )
VF_graph
Visual_Function <- ggsave("Visual Function.png",
       scale = 1,
       width = 1.75,
       height = 1.75,
       units = c("in"))



############# Frequency Bar Graphs ----
####### Controls



Control_type_of_symptoms.df <- data.frame(
  Patient = 1:29,
  Question_1 = Control.df$Q1,
  Question_2 = Control.df$Q2,
  Question_3 = Control.df$Q3,
  Question_4 = Control.df$Q4,
  Question_5 = Control.df$Q5,
  Question_6 = Control.df$Q6,
  Question_7 = Control.df$Q7,
  Question_8 = Control.df$Q8,
  Question_9 = Control.df$Q9,
  Question_10 = Control.df$Q10,
  Question_11 = Control.df$Q11,
  Question_12 = Control.df$Q12,
  Question_13 = Control.df$Q13,
  Question_14 = Control.df$Q14,
  Question_15 = Control.df$Q15)

Control_type_of_symptoms_longer <- pivot_longer(Control_type_of_symptoms.df,
                                                cols = starts_with("Question_"),
                                                names_to = "Question",
                                                values_to = "Frequency")
desired_order_control_symptoms <- c(
  "Question_1", "Question_2", "Question_3", "Question_4", "Question_5", "Question_6", "Question_7", "Question_8",
  "Question_9", "Question_10", "Question_11", "Question_12", "Question_13", "Question_14", "Question_15"
)

Control_type_of_symptoms_longer$Question <- factor(
  Control_type_of_symptoms_longer$Question,
  levels = desired_order_control_symptoms
)

View(Control_type_of_symptoms_longer)


gnbu_palette <- brewer.pal(n = 5, name = "GnBu")


Control_symptoms_frequency_graph <- ggplot(Control_type_of_symptoms_longer, aes(x = Question, fill = factor(Frequency)))+
  geom_bar()+
  labs(
    x = "Questions",
    y = "N",
    title = "",
    fill = "Symptom Frequency"
  ) +
  scale_x_discrete(labels = c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                              "Lose Concentration", "Trouble Remembering", "Double Vision", "Words Moving",
                              "Reading Slowly","Eyes Hurt", "Eyes Sore", "Eyes Pulling",
                              "Words Blurring", "Lose Place", "Re-Read Words"))+
  scale_fill_manual(
    values = c("0"= gnbu_palette[1], "1"= gnbu_palette[2], "2" = gnbu_palette[3], "3" = gnbu_palette[4], "4" = gnbu_palette[5]),
    name = "Symptom Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

Control_symptoms_frequency_graph  

Control_symptoms_frequency_graph <- ggsave("Control Symptoms Frequency.png",
                                               scale = 1,
                                               width = 6,
                                               height = 7,
                                               units = c("in"))

####### Concussed

Concussed_type_of_symptoms.df <- data.frame(
  Patient = 1:210,
  Question_1 = Concussed.df$Q1,
  Question_2 = Concussed.df$Q2,
  Question_3 = Concussed.df$Q3,
  Question_4 = Concussed.df$Q4,
  Question_5 = Concussed.df$Q5,
  Question_6 = Concussed.df$Q6,
  Question_7 = Concussed.df$Q7,
  Question_8 = Concussed.df$Q8,
  Question_9 = Concussed.df$Q9,
  Question_10 = Concussed.df$Q10,
  Question_11 = Concussed.df$Q11,
  Question_12 = Concussed.df$Q12,
  Question_13 = Concussed.df$Q13,
  Question_14 = Concussed.df$Q14,
  Question_15 = Concussed.df$Q15)

Concussed_type_of_symptoms_longer <- pivot_longer(Concussed_type_of_symptoms.df,
                                                cols = starts_with("Question_"),
                                                names_to = "Question",
                                                values_to = "Frequency")
desired_order_concussed_symptoms <- c(
  "Question_1", "Question_2", "Question_3", "Question_4", "Question_5", "Question_6", "Question_7", "Question_8",
  "Question_9", "Question_10", "Question_11", "Question_12", "Question_13", "Question_14", "Question_15"
)

Concussed_type_of_symptoms_longer$Question <- factor(
  Concussed_type_of_symptoms_longer$Question,
  levels = desired_order_concussed_symptoms
)

Concussed_type_of_symptoms_longer$Frequency <- 
  as.factor(Concussed_type_of_symptoms_longer$Frequency)

data.class(Concussed_type_of_symptoms_longer$Frequency)

Concussed_type_of_symptoms_longer <- Concussed_type_of_symptoms_longer %>%
  group_by(Question) %>%
  mutate(Percentage = (sum(as.numeric(as.character(Frequency)))) / sum(as.numeric(as.character(Concussed_type_of_symptoms_longer$Frequency))) * 100)

View(Concussed_type_of_symptoms_longer)

gnbu_palette <- brewer.pal(n = 5, name = "GnBu")


Concussed_symptoms_frequency_graph <- ggplot(Concussed_type_of_symptoms_longer, aes(x = Question, fill = factor(Frequency)))+
  geom_bar()+
  labs(
    x = "Questions",
    y = "%",
    title = "",
    fill = "Symptom Frequency"
  ) +
  scale_x_discrete(labels = c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                              "Lose Concentration", "Trouble Remembering", "Double Vision", "Words Moving",
                              "Reading Slowly","Eyes Hurt", "Eyes Sore", "Eyes Pulling",
                              "Words Blurring", "Lose Place", "Re-Read Words"))+
  scale_fill_manual(
    values = c("0"= gnbu_palette[1], "1"= gnbu_palette[2], "2" = gnbu_palette[3], "3" = gnbu_palette[4], "4" = gnbu_palette[5]),
    name = "Symptom Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

Concussed_symptoms_frequency_graph 

Concussed_symptoms_frequency_graph <- ggsave("Concussed Symptoms Frequency.png",
                                           scale = 1,
                                           width = 6,
                                           height = 7,
                                           units = c("in"))


######## Abnormal VF - Visual Symptoms

abnormal_VF_type_of_symptoms.df <- data.frame(
  Question_7 = abnormal_visual_function$Q7,
  Question_8 = abnormal_visual_function$Q8,
  Question_13 = abnormal_visual_function$Q13)

abnormal_VF_type_of_symptoms_longer <- pivot_longer(abnormal_VF_type_of_symptoms.df,
                                                  cols = starts_with("Question_"),
                                                  names_to = "Question",
                                                  values_to = "Frequency")
desired_order_abnormal_VF_symptoms <- c(
"Question_7", "Question_8",
  "Question_13"
)

abnormal_VF_type_of_symptoms_longer$Question <- factor(
  abnormal_VF_type_of_symptoms_longer$Question,
  levels = desired_order_abnormal_VF_symptoms
)

View(abnormal_VF_type_of_symptoms_longer)

gnbu_palette <- brewer.pal(n = 5, name = "GnBu")


abnormal_VF_symptoms_frequency_graph <- ggplot(abnormal_VF_type_of_symptoms_longer, aes(x = Question, fill = factor(Frequency))) +
  geom_bar() +
  labs(
    x = "",
    y = "",
    title = "",
    fill = "Symptom Frequency"
  ) +
  scale_x_discrete(labels = c("Double Vision", "Words Moving", "Words Blurring")) +
  scale_fill_manual(
    values = c("0" = gnbu_palette[1], "1" = gnbu_palette[2], "2" = gnbu_palette[3], "3" = gnbu_palette[4], "4" = gnbu_palette[5]),
    name = "Symptom Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.text = element_text(size = 20),
    legend.key.size = unit(1, 'cm'),
    legend.title = element_text(size = 20),
    legend.position = "none"
  )

abnormal_VF_symptoms_frequency_graph 


abnormal_VF_symptoms_frequency_graph <- ggsave("Abnormal VF Visual Symptoms.png",
                                        scale = 1,
                                        width = 3.5,
                                        height = 2.5,
                                        units = c("in"))

######## Normal VF - Vision Symptoms


normal_VF_type_of_symptoms.df <- data.frame(
  Patient = 1:57,
  Question_7 = normal_visual_function$Q7,
  Question_8 = normal_visual_function$Q8,
  Question_13 = normal_visual_function$Q13)

normal_VF_type_of_symptoms_longer <- pivot_longer(normal_VF_type_of_symptoms.df,
                                                    cols = starts_with("Question_"),
                                                    names_to = "Question",
                                                    values_to = "Frequency")
desired_order_normal_VF_symptoms <- c(
 "Question_7", "Question_8",
 "Question_13", "Question_14", "Question_15"
)

normal_VF_type_of_symptoms_longer$Question <- factor(
  normal_VF_type_of_symptoms_longer$Question,
  levels = desired_order_normal_VF_symptoms
)

View(normal_VF_type_of_symptoms_longer)

gnbu_palette <- brewer.pal(n = 5, name = "GnBu")


normal_VF_symptoms_frequency_graph <- ggplot(normal_VF_type_of_symptoms_longer, aes(x = Question, fill = factor(Frequency)))+
  geom_bar()+
  labs(
    x = "",
    y = "N",
    title = "",
    fill = "Symptom Frequency"
  ) +
  scale_x_discrete(labels = c("Double Vision", "Words Moving",
                              "Words Blurring"))+
  scale_fill_manual(
    values = c("0"= gnbu_palette[1], "1"= gnbu_palette[2], "2" = gnbu_palette[3], "3" = gnbu_palette[4], "4" = gnbu_palette[5]),
    name = "Symptom Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y.left = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 20),
        legend.position = "none")

normal_VF_symptoms_frequency_graph 


normal_VF_symptoms_frequency_graph <- ggsave("Normal VF Visual Symptoms.png",
                                               scale = 1,
                                               width = 3.5,
                                               height = 2.5,
                                               units = c("in"))


######## Figure 1 - Total CISS scores in Controls vs Concussed
###### Clinic Type

Concussed_Control_Total.df <- data.frame(
  Total_Score = c(
    Concussed.df$CISS_total_15,
    Control.df$CISS_total_15),
  Type = rep(c("Concussed", "Control"),times = c(210, 29)))

View(Concussed_Control_Total.df)

Concussed_Control_Total_graph <- ggplot(Concussed_Control_Total.df, aes(x = Type, y = Total_Score, fill = Type)) +
  geom_boxplot(size = 1.25) +
  scale_fill_manual(values = c("coral","darkolivegreen3")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, color = "black")
  )

Concussed_Control_Total_graph

Concussed_Control_Total_graph <- ggsave("Control vs Concussed Total Scores.png",
                      scale = 1,
                      width = 2.65,
                      height = 3.5,
                      units = c("in"))

####### Subscores for concussed and controls

CISSP_all_patients.df <- all.data.df[,c(23,24,27,32,33)]
CISSS_all_patients.df <- all.data.df[,c(19,20,21,22,28,29,30)]
CISSV_all_patients.df <- all.data.df[,c(31,25,26)]

all.data.df$Concussed <- ifelse(all.data.df$Concussed == 1, "Concussed", "Control")
all.data.df <- all.data.df %>% rowwise() %>% mutate(CISSS_total_normalized = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12)/7)
all.data.df <- all.data.df %>% rowwise() %>% mutate(CISSP_total_normalized = sum(Q5, Q6, Q9, Q14, Q15)/5)
all.data.df <- all.data.df %>% rowwise() %>% mutate(CISSV_total_normalized = sum(Q7, Q8, Q13)/3)

Subscales_all_patients.df <- data.frame(
  Response = c(
   all.data.df$CISSP_total_normalized,
   all.data.df$CISSS_total_normalized,
   all.data.df$CISSV_total_normalized),
  Subscales = rep(c("CISS-P", "CISS-S", "CISS-V"),times = c(239, 239, 239)),
  Group = factor(rep(all.data.df$Concussed)))

desired_order_subscales_all <- c(
  "CISS-P", "CISS-S", "CISS-V"
)

Subscales_all_patients.df$Subscales <- factor(
  Subscales_all_patients.df$Subscales,
  levels = desired_order_subscales_all
)

View(Subscales_all_patients.df)

####### Concussed vs Controls Subscale Graph

Subscales_all_patients_graph <- ggplot(Subscales_all_patients.df, aes(x = Group, y = Response, fill = Subscales)) +
  geom_boxplot(size = 1.25, outlier.shape = NA) +
  scale_fill_manual(values = c("darkmagenta", "darkolivegreen", "blue4")) +
  theme_bw() +
  labs(x = "", y = "Values") +
  facet_wrap(~ Subscales)+
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    
    panel.grid.minor = element_blank(),    
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 10, color = "black")
  )

Subscales_all_patients_graph


Subscales_all_patients_graph <- ggsave("Subscales All Patients.png",
                    scale = 1,
                    width = 4.55,
                    height = 3.5,
                    units = c("in"))

######## Normal vs Abnormal Sub-scores ----
CISSP.df <- Concussed.df[,c(23,24,27,32,33)]
CISSS.df <- Concussed.df[,c(19,20,21,22,28,29,30)]
CISSV.df <- Concussed.df[,c(31,25,26)]

View(Concussed.df)

table(Concussed.df$abnormal.vf)

Concussed.df$abnormal.vf <- ifelse(Concussed.df$abnormal.vf == "YES", "Abnormal Visual Function", "Normal Visual Function")


Subscales_Concussed.df <- data.frame(
  Response = c(
    CISSS.df$Q1,
    CISSS.df$Q2,
    CISSS.df$Q3,
    CISSS.df$Q4,
    CISSS.df$Q10,
    CISSS.df$Q11,
    CISSS.df$Q12,
    CISSP.df$Q5,
    CISSP.df$Q6,
    CISSP.df$Q9,
    CISSP.df$Q14,
    CISSP.df$Q15,
    CISSV.df$Q7,
    CISSV.df$Q8,
    CISSV.df$Q13),
  Subscales = rep(c("CISS-S", "CISS-P", "CISS-V"),times = c(1470, 1050, 630)),
  Group = factor(rep(Concussed.df$abnormal.vf)))

View(Subscales_Concussed.df)


Subscales_Concussed_graph <- ggplot(Subscales_Concussed.df, aes(x = Subscales, y = Response, fill = Subscales)) +
  geom_boxplot(size = 1.25, outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid1", "cadetblue", "deeppink3")) +
  theme_bw() +
  labs(x = "", y = "Values") +
  facet_wrap(~ Group) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    # Remove major grid lines
    panel.grid.minor = element_blank(),    # Remove minor grid lines
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 15, color = "black")
  )

Subscales_Concussed_graph


Subscales_Concussed_graph <- ggsave("Subscales Concussed.png",
                                       scale = 1,
                                       width = 6,
                                       height = 6,
                                       units = c("in"))

###### subscales for abnormal vs normal with normalized subscales ----


Subscales_Concussed.df <- data.frame(
  Response = c(
    Concussed.df$CISSS_total_normalized,
    Concussed.df$CISSP_total_normalized,
    Concussed.df$CISSV_total_normalized),
  Subscales = rep(c("CISS-S", "CISS-P", "CISS-V"),times = c(210, 210, 210)),
  Group = factor(rep(Concussed.df$abnormal.vf)))

desired_order_subscales <- c(
  "CISS-S", "CISS-P", "CISS-V"
)

Subscales_Concussed.df$Subscales <- factor(
  Subscales_Concussed.df$Subscales,
  levels = desired_order_subscales
)
  


Subscales_Concussed_Normalized_graph <- ggplot(Subscales_Concussed.df, aes(x = Subscales, y = Response, fill = Subscales)) +
  geom_boxplot(size = 1.25, outlier.shape = NA) +
  scale_fill_manual(values = c("darkorchid1", "cadetblue", "deeppink3")) +
  theme_bw() +
  labs(x = "", y = "Values") +
  facet_wrap(~ Group) +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 20),
    legend.text.align = 0.5,
    legend.key.size = unit(1, "cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),    
    panel.grid.minor = element_blank(),   
    axis.ticks.y = element_blank(),
    axis.text.y.left = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.text.x.bottom = element_text(size = 15, color = "black")
  )

Subscales_Concussed_Normalized_graph
Subscales_Concussed_Normalized_graph <- ggsave("Abnormal vs Normal VF subscales.png",
                                    scale = 1,
                                    width = 10,
                                    height = 6,
                                    units = c("in"))

######## Normal vs abnormal Somatic symptoms ----
#### Normal VF

normal_VF_type_of_symptoms.df <- data.frame(
  Patient = 1:57,
  Question_1 = normal_visual_function$Q1,
  Question_2 = normal_visual_function$Q2,
  Question_3 = normal_visual_function$Q3,
  Question_4 = normal_visual_function$Q4,
  Question_10 = normal_visual_function$Q10,
  Question_11 = normal_visual_function$Q11,
  Question_12 = normal_visual_function$Q12)

normal_VF_type_of_symptoms_longer <- pivot_longer(normal_VF_type_of_symptoms.df,
                                                  cols = starts_with("Question_"),
                                                  names_to = "Question",
                                                  values_to = "Frequency")
desired_order_normal_VF_symptoms <- c(
  "Question_1", "Question_2",
  "Question_3", "Question_4", "Question_10",
  "Question_11", "Question_12"
)

normal_VF_type_of_symptoms_longer$Question <- factor(
  normal_VF_type_of_symptoms_longer$Question,
  levels = desired_order_normal_VF_symptoms
)

View(normal_VF_type_of_symptoms_longer)

gnbu_palette <- brewer.pal(n = 5, name = "GnBu")


normal_VF_somatic_symptoms_frequency_graph <- ggplot(normal_VF_type_of_symptoms_longer, aes(x = Question, fill = factor(Frequency)))+
  geom_bar()+
  labs(
    x = "",
    y = "N",
    title = "",
    fill = "Symptom Frequency"
  ) +
  scale_x_discrete(labels = c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                              "Eyes Hurt", "Eyes Sore", "Eyes Pulling"))+
  scale_fill_manual(
    values = c("0"= gnbu_palette[1], "1"= gnbu_palette[2], "2" = gnbu_palette[3], "3" = gnbu_palette[4], "4" = gnbu_palette[5]),
    name = "Symptom Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y.left = element_text(size = 20),
        axis.title.y = element_text(size = 15),
        axis.text.x.bottom = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 20),
        legend.position = "bottom")

normal_VF_somatic_symptoms_frequency_graph 
normal_VF_somatic_symptoms_frequency_graph <- ggsave("Normal VF Somatic Symptoms.png",
                                                       scale = 1,
                                                       width = 5,
                                                       height = 6,
                                                       units = c("in"))

######### abnormal vF
abnormal_VF_somatic_symptoms.df <- data.frame(
  Patient = 1:153,
  Question_1 = abnormal_visual_function$Q1,
  Question_2 = abnormal_visual_function$Q2,
  Question_3 = abnormal_visual_function$Q3,
  Question_4 = abnormal_visual_function$Q4,
  Question_10 = abnormal_visual_function$Q10,
  Question_11 = abnormal_visual_function$Q11,
  Question_12 = abnormal_visual_function$Q12)

abnormal_VF_somatic_symptoms_longer <- pivot_longer(abnormal_VF_somatic_symptoms.df,
                                                  cols = starts_with("Question_"),
                                                  names_to = "Question",
                                                  values_to = "Frequency")
desired_order_abnormal_VF_somatic_symptoms <- c(
  "Question_1", "Question_2",
  "Question_3", "Question_4", "Question_10",
  "Question_11", "Question_12"
)

abnormal_VF_somatic_symptoms_longer$Question <- factor(
  abnormal_VF_somatic_symptoms_longer$Question,
  levels = desired_order_abnormal_VF_somatic_symptoms
)

View(abnormal_VF_somatic_symptoms_longer)

gnbu_palette <- brewer.pal(n = 5, name = "GnBu")


abnormal_VF_somatic_symptoms_frequency_graph <- ggplot(abnormal_VF_somatic_symptoms_longer, aes(x = Question, fill = factor(Frequency)))+
  geom_bar()+
  labs(
    x = "",
    y = "N",
    title = "",
    fill = "Symptom Frequency"
  ) +
  scale_x_discrete(labels = c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                              "Eyes Hurt", "Eyes Sore", "Eyes Pulling"))+
  scale_fill_manual(
    values = c("0"= gnbu_palette[1], "1"= gnbu_palette[2], "2" = gnbu_palette[3], "3" = gnbu_palette[4], "4" = gnbu_palette[5]),
    name = "Symptom Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y.left = element_text(size = 20),
        axis.title.y = element_text(size = 15),
        axis.text.x.bottom = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 20),
        legend.position = "bottom")

abnormal_VF_somatic_symptoms_frequency_graph
abnormal_VF_somatic_symptoms_frequency_graph <- ggsave("Abnormal VF Somatic Symptoms.png",
                                             scale = 1,
                                             width = 5,
                                             height = 6,
                                             units = c("in"))
### Abnormal Visual Function ###

Concussed.df$abnormal.vf <- ifelse(Concussed.df$npc_fail == 1 | Concussed.df$AA_FAIL == 1, "YES", 
                                   ifelse(!(Concussed.df$npc_fail == 1 | Concussed.df$AA_FAIL == 1), "NO", NA))
Concussed.df$abnormal.vf <- as.factor(Concussed.df$abnormal.vf)

all.data.df$abnormal.vf <- ifelse(all.data.df$npc_fail == 1 | all.data.df$AA_FAIL == 1, "YES", 
                                   ifelse(!(all.data.df$npc_fail == 1 | all.data.df$AA_FAIL == 1), "NO", NA))

all.data.df$abnormal.vf <- as.factor(all.data.df$abnormal.vf)

table(Concussed.df$abnormal.vf)

abnormal_visual_function <- Concussed.df %>% filter(npc_fail == 1 | AA_FAIL == 1)
  View(abnormal_visual_function)

mean(abnormal_visual_function$CISS_total_15)
sd(abnormal_visual_function$CISS_total_15)

normal_visual_function <- Concussed.df %>% filter(!(npc_fail == 1 | AA_FAIL == 1))
View(normal_visual_function)

mean(normal_visual_function$CISS_total_15)
sd(normal_visual_function$CISS_total_15)

####### Total Medians and IQR ----

quantile(abnormal_visual_function$CISS_total_15)
quantile(normal_visual_function$CISS_total_15)

###### Medians for Subgroups ----
##### CISSS
quantile(abnormal_visual_function$CISSS_total_normalized)
quantile(normal_visual_function$CISSS_total_normalized)

wilcox.test(abnormal_visual_function$CISSS_total_normalized, normal_visual_function$CISSS_total_normalized)

####### CISSP
quantile(abnormal_visual_function$CISSP_total_normalized)
quantile(normal_visual_function$CISSP_total_normalized)

wilcox.test(abnormal_visual_function$CISSP_total_normalized, normal_visual_function$CISSP_total_normalized)

#### CISSV
quantile(abnormal_visual_function$CISSV_total_normalized)
quantile(normal_visual_function$CISSV_total_normalized)

wilcox.test(abnormal_visual_function$CISSV_total_normalized, normal_visual_function$CISSV_total_normalized)


wilcox.test(abnormal_visual_function$CISS_total_15, normal_visual_function$CISS_total_15)

                                                              #### Mean-Likert Rating for Subscale Differences ####
##abnormal and normal in general

CISSS.abnormal.df <- data.frame(abnormal_visual_function %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
total_sum_CISSS.abnormal <- sum(unlist(CISSS.abnormal.df))
print(total_sum_CISSS.abnormal)
total_sum_CISSS.abnormal_weight <- total_sum_CISSS.abnormal/7
total_sum_CISSS.abnormal_weight
Normalized_CISSS.abnormal <- total_sum_CISSS.abnormal_weight/164
Normalized_CISSS.abnormal


CISSS.abnormal.df_sd <- sd(unlist(CISSS.abnormal.df))
CISSS.abnormal.df_sd

###Performance/Cognitive
CISSC.abnormal.df <- data.frame(abnormal_visual_function %>% select(Q5, Q6, Q9, Q14, Q15))
CISSC.abnormal.df
total_sum_CISSC.abnormal <- sum(unlist(CISSC.abnormal.df))
View(total_sum_CISSC.abnormal)
total_sum_CISSC.abnormal_weight <- total_sum_CISSC.abnormal/5
total_sum_CISSC.abnormal_weight
Normalized_CISSC.abnormal <- total_sum_CISSC.abnormal_weight/164
Normalized_CISSC.abnormal

CISSC.abnormal.df_sd <- sd(unlist(CISSC.abnormal.df))
CISSC.abnormal.df_sd

###Visual
CISSV.abnormal.df <- data.frame(abnormal_visual_function %>% select(Q7, Q8, Q13))
abnormal_visual_function <- abnormal_visual_function %>% rowwise() %>% mutate(CISSV_total = sum(Q7,Q8,Q13)/3)

View(CISSV.abnormal.df)


total_sum_CISSV.abnormal <- sum(unlist(CISSV.abnormal.df))

total_CISSV.abnormal <- unlist(CISSV.abnormal.df)
View(total_CISSV.abnormal)
print(total_sum_CISSV.abnormal)
total_sum_CISSV.abnormal_weight <- total_sum_CISSV.abnormal/3
total_sum_CISSV.abnormal_weight
Normalized_CISSV.abnormal <- total_sum_CISSV.abnormal_weight/164
Normalized_CISSV.abnormal

CISSV.abnormal.df_sd <- sd(unlist(CISSV.abnormal.df))
CISSV.abnormal.df_sd


### Mean Likert Rating Subscales for normal visual function ###

CISSS.normal.vf <- data.frame(normal_visual_function %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
total_sum_CISSS.normal.vf <- sum(unlist(CISSS.normal.vf))
print(total_sum_CISSS.normal.vf)
total_sum_CISSS.normal.vf_weight <- total_sum_CISSS.normal.vf/7
total_sum_CISSS.normal.vf_weight
Normalized_CISSS.normal.vf <- total_sum_CISSS.normal.vf_weight/46
Normalized_CISSS.normal.vf

CISSS.normal.vf_sd <- sd(unlist(CISSS.normal.vf))
CISSS.normal.vf_sd

###Performance/Cognitive
CISSC.normal.vf <- data.frame(normal_visual_function %>% select(Q5, Q6, Q9, Q14, Q15))
CISSC.normal.vf
total_sum_CISSC.normal.vf <- sum(unlist(CISSC.normal.vf))
print(total_sum_CISSC.normal.vf)
total_sum_CISSC.normal.vf_weight <- total_sum_CISSC.normal.vf/5
total_sum_CISSC.normal.vf_weight
Normalized_CISSC.normal.vf <- total_sum_CISSC.normal.vf_weight/46
Normalized_CISSC.normal.vf

CISSC.normal.vf_sd <- sd(unlist(CISSC.normal.vf))
CISSC.normal.vf_sd

###### Vision 
CISSV.normal.vf <- data.frame(normal_visual_function %>% select(Q7, Q8, Q13))
CISSV.normal.vf

total_sum_CISSV.normal.vf <- sum(unlist(CISSV.normal.vf))
print(total_sum_CISSV.normal.vf)
total_sum_CISSV.normal.vf_weight <- total_sum_CISSV.normal.vf/3
total_sum_CISSV.normal.vf_weight
Normalized_CISSV.normal.vf <- total_sum_CISSV.normal.vf_weight/46
Normalized_CISSV.normal.vf

CISSV.normal.vf_sd <- sd(unlist(CISSV.normal.vf))
CISSV.normal.vf_sd

### Are normal visual function mean likert rating subscales different than abnormal visual function symptoms ###
wilcox.test((unlist(CISSS.abnormal.df)), (unlist(CISSS.normal.vf)))
wilcox.test((unlist(CISSC.abnormal.df)), (unlist(CISSC.normal.vf)))
wilcox.test((unlist(CISSV.normal.vf)), (unlist(CISSV.abnormal.df)))

###Frequencies for Often or always for abnormal###

total.symptoms.abnormal <- abnormal_visual_function[,c(35:49)]
View(total.symptoms.abnormal)


percent.fun <- function(vec, x){
  
  helper.fun <- function(y, x){
    y/x
  }
  mapply(helper.fun, vec, x)
}

sum.total.abnormal <- (as.data.frame(mapply(sum,total.symptoms.abnormal)))


sum.total.abnormal$Question <- c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                                 "Lose Concentration", "Trouble Remembering", "Double Vision", "Words Moving",
                                 "Reading Slowly","Eyes Hurt", "Eyes Sore", "Eyes Pulling",
                                 "Words Blurring", "Lose Place", "Re-Read Words")
colnames(sum.total.abnormal) <- c("N", "Question")
sum.total.abnormal <- sum.total.abnormal %>% mutate(Frequency = percent.fun(N, 164))

sum.total.abnormal

sum.total.abnormal$Question <- factor(sum.total.abnormal$Question, levels = c(sum.total.abnormal$Question))

### Separating based on subscales ###
subscale.fx <- function(symptom.vec){
  helper.function <- function(x){
    if(x == "Lose Concentration"| x== "Trouble Remembering"|
       x== "Reading Slowly"| x== "Lose Place"| x== "Re-Read Words"){
      "CISS-C"
    }else if(x== "Words Blurring"| x == "Double Vision"| x== "Words Moving"){
      "CISS-V"
    }else{
      "CISS-S"
    }
  }
  sapply(symptom.vec, helper.function)
}

sum.total.abnormal <- sum.total.abnormal %>% mutate(Subscale = subscale.fx(Question))

sum.total.abnormal$Subscale <- factor(sum.total.abnormal$Subscale, levels = c("CISS-S", "CISS-C", "CISS-V"))

sum.total.abnormal


#### Frequencies for fairly often or always for normal visual function #####
total.symptoms.normal <- normal_visual_function[,c(35:49)]
View(total.symptoms.normal)


percent.fun <- function(vec, x){
  
  helper.fun <- function(y, x){
    y/x
  }
  mapply(helper.fun, vec, x)
}

sum.total.normal <- (as.data.frame(mapply(sum,total.symptoms.normal)))


sum.total.normal$Question <- c("Tired Eyes", "Eye Discomfort", "Headaches", "Sleepy",
                               "Lose Concentration", "Trouble Remembering", "Double Vision", "Words Moving",
                               "Reading Slowly","Eyes Hurt", "Eyes Sore", "Eyes Pulling",
                               "Words Blurring", "Lose Place", "Re-Read Words")
colnames(sum.total.normal) <- c("N", "Question")
sum.total.normal <- sum.total.normal %>% mutate(Frequency = percent.fun(N, 46))

sum.total.normal

sum.total.normal$Question <- factor(sum.total.normal$Question, levels = c(sum.total.normal$Question))

### Separating based on subscales ###
subscale.fx <- function(symptom.vec){
  helper.function <- function(x){
    if(x == "Lose Concentration"| x== "Trouble Remembering"|
       x== "Reading Slowly"| x== "Lose Place"| x== "Re-Read Words"){
      "CISS-C"
    }else if(x== "Words Blurring"| x == "Double Vision"| x== "Words Moving"){
      "CISS-V"
    }else{
      "CISS-S"
    }
  }
  sapply(symptom.vec, helper.function)
}

sum.total.normal <- sum.total.normal %>% mutate(Subscale = subscale.fx(Question))

sum.total.normal$Subscale <- factor(sum.total.normal$Subscale, levels = c("CISS-S", "CISS-C", "CISS-V"))

sum.total.normal

##### Q3 Headaches

chisq.test(Concussed.df$abnormal.vf, Concussed.df$Headaches)

###### Q11 Eyes Sore

chisq.test(Concussed.df$abnormal.vf, Concussed.df$Eyes_Sore)

####### Did specific variables have differences within abnormal and normal for CISS Total ####

### Temporality
abnormal_visual_function.chronic <- abnormal_visual_function %>% filter(Temporality == 2)
abnormal_visual_function.subacute <- abnormal_visual_function %>% filter(Temporality == 1)
normal_visual_function.chronic <- normal_visual_function %>% filter(Temporality == 2)
normal_visual_function.subacute <- normal_visual_function %>% filter(Temporality == 1)

wilcox.test(abnormal_visual_function.chronic$CISS_total_15, 
            abnormal_visual_function.subacute$CISS_total_15)


#### Subacute and Chronic did not vary for abnormal and normal VF


### Gender

abnormal_visual_function.girl <- abnormal_visual_function %>% filter(Gender == 2)
abnormal_visual_function.boy <- abnormal_visual_function %>% filter(Gender == 1)
normal_visual_function.girl <- normal_visual_function %>% filter(Gender == 2)
normal_visual_function.boy <- normal_visual_function %>% filter(Gender == 1)

wilcox.test(abnormal_visual_function.girl$CISS_total_15,
            abnormal_visual_function.boy$CISS_total_15)


#### Clinic Type ####

abnormal_visual_function.mdcc <- abnormal_visual_function %>% filter(Clinic_type == 1)
abnormal_visual_function.ref <- abnormal_visual_function %>% filter(Clinic_type == 2)

wilcox.test(abnormal_visual_function.mdcc$CISS_total_15,
            abnormal_visual_function.ref$CISS_total_15)

mean(abnormal_visual_function.subacute$CISS_total_15)
sd(abnormal_visual_function.subacute$CISS_total_15)
mean(abnormal_visual_function.chronic$CISS_total_15)
sd(abnormal_visual_function.chronic$CISS_total_15)

#### Looking at symptom reporting for different mechanisms of concussion for abnormal vs normal 

############# NEEDS TO BE FIXED or deleted

Sport_Con <- Concussed.df %>% filter(Type_con == 1)
View(Sport_Con)

CISSS.sport.df <- data.frame(Sport_Con %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
total_sum_CISSS.df <- sum(unlist(CISSS.df))

total_sum_CISSS.df_weight <- total_sum_CISSS.df/7
total_sum_CISSS.df_weight
Normalized_CISSS.df <- total_sum_CISSS.normal.vf_weight/46
Normalized_CISSS.df

CISSS.normal.vf_sd <- sd(unlist(CISSS.normal.vf))
CISSS.normal.vf_sd

###Performance/Cognitive
CISSC.normal.vf <- data.frame(normal_visual_function %>% select(Q5, Q6, Q9, Q14, Q15))
CISSC.normal.vf
total_sum_CISSC.normal.vf <- sum(unlist(CISSC.normal.vf))
print(total_sum_CISSC.normal.vf)
total_sum_CISSC.normal.vf_weight <- total_sum_CISSC.normal.vf/5
total_sum_CISSC.normal.vf_weight
Normalized_CISSC.normal.vf <- total_sum_CISSC.normal.vf_weight/46
Normalized_CISSC.normal.vf

CISSC.normal.vf_sd <- sd(unlist(CISSC.normal.vf))
CISSC.normal.vf_sd

###### Vision 
CISSV.normal.vf <- data.frame(normal_visual_function %>% select(Q7, Q8, Q13))
CISSV.normal.vf

total_sum_CISSV.normal.vf <- sum(unlist(CISSV.normal.vf))
print(total_sum_CISSV.normal.vf)
total_sum_CISSV.normal.vf_weight <- total_sum_CISSV.normal.vf/3
total_sum_CISSV.normal.vf_weight
Normalized_CISSV.normal.vf <- total_sum_CISSV.normal.vf_weight/46
Normalized_CISSV.normal.vf

CISSV.normal.vf_sd <- sd(unlist(CISSV.normal.vf))
CISSV.normal.vf_sd

wilcox.test((unlist(CISSS.abnormal.df)), (unlist(CISSS.normal.vf)))$conf.int
t.test((unlist(CISSV.abnormal.df)), (unlist(CISSV.normal.vf)))$conf.int


#### Normalized Differences between Abnormal Subacute vs Chronic ####

CISSS.subacute.abnormal.df <- data.frame(abnormal_visual_function.subacute %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
total_sum_CISSS.subacute.abnormal <- sum(unlist(CISSS.subacute.abnormal.df))
print(total_sum_CISSS.subacute.abnormal)
total_sum_CISSS.subacute.abnormal_weight <- total_sum_CISSS.subacute.abnormal/7
total_sum_CISSS.subacute.abnormal_weight
Normalized_CISSS.subacute.abnormal <- total_sum_CISSS.subacute.abnormal_weight/103
Normalized_CISSS.subacute.abnormal

View(abnormal_visual_function.subacute)

CISSS.abnormal.subacute.df_sd <- sd(unlist(CISSS.subacute.abnormal.df))
CISSS.abnormal.subacute.df_sd

###Performance/Cognitive
CISSC.subacute.abnormal.df <- data.frame(abnormal_visual_function.subacute %>% select(Q5, Q6, Q9, Q14, Q15))
CISSC.subacute.abnormal.df
total_sum_CISSC.subacute.abnormal <- sum(unlist(CISSC.subacute.abnormal.df))
print(total_sum_CISSC.subacute.abnormal)
total_sum_CISSC.subacute.abnormal_weight <- total_sum_CISSC.subacute.abnormal/5
total_sum_CISSC.subacute.abnormal_weight
Normalized_CISSC.subacute.abnormal <- total_sum_CISSC.subacute.abnormal_weight/103
Normalized_CISSC.subacute.abnormal

CISSC.subacute.abnormal.df_sd <- sd(unlist(CISSC.subacute.abnormal.df))
CISSC.subacute.abnormal.df_sd

###Visual
CISSV.subacute.abnormal.df <- data.frame(abnormal_visual_function.subacute %>% select(Q7, Q8, Q13))
CISSV.subacute.abnormal.df

total_sum_CISSV.subacute.abnormal <- sum(unlist(CISSV.subacute.abnormal.df))
print(total_sum_CISSV.subacute.abnormal)
total_sum_CISSV.subacute.abnormal_weight <- total_sum_CISSV.subacute.abnormal/3
total_sum_CISSV.subacute.abnormal_weight
Normalized_CISSV.subacute.abnormal <- total_sum_CISSV.subacute.abnormal_weight/103
Normalized_CISSV.subacute.abnormal

CISSV.abnormal.df_sd <- sd(unlist(CISSV.abnormal.df))
CISSV.abnormal.df_sd

####### Chronic

CISSS.chronic.abnormal <- data.frame(abnormal_visual_function.chronic %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
total_sum_CISSS.chronic <- sum(unlist(CISSS.chronic.abnormal))
print(total_sum_CISSS.chronic)
total_sum_CISSS.chronic_weight <- total_sum_CISSS.chronic/7
total_sum_CISSS.chronic_weight
Normalized_CISSS.chronic.abnormal <- total_sum_CISSS.chronic_weight/61
Normalized_CISSS.chronic.abnormal

CISSS.chronic.abnormal_sd <- sd(unlist(CISSS.chronic.abnormal))
CISSS.chronic.abnormal_sd

###Performance/Cognitive
CISSC.chronic <- data.frame(abnormal_visual_function.chronic %>% select(Q5, Q6, Q9, Q14, Q15))
CISSC.chronic
total_sum_CISSC.chronic <- sum(unlist(CISSC.chronic))
print(total_sum_CISSC.chronic)
total_sum_CISSC.chronic_weight <- total_sum_CISSC.chronic/5
total_sum_CISSC.chronic_weight
Normalized_CISSC.chronic.abnormal <- total_sum_CISSC.chronic_weight/61
Normalized_CISSC.chronic.abnormal

CISSC.chronic.abnormal_sd <- sd(unlist(CISSC.chronic))
CISSC.chronic.abnormal_sd

###### Vision 
CISSV.chronic <- data.frame(abnormal_visual_function.chronic %>% select(Q7, Q8, Q13))
CISSV.chronic

total_sum_CISSV.chronic <- sum(unlist(CISSV.chronic))
print(total_sum_CISSV.chronic)
total_sum_CISSV.chronic_weight <- total_sum_CISSV.chronic/3
total_sum_CISSV.chronic_weight
Normalized_CISSV.chronic.abnormal <- total_sum_CISSV.chronic_weight/61
Normalized_CISSV.chronic.abnormal

CISSV.chronic.abnormal_sd <- sd(unlist(CISSV.chronic))
CISSV.chronic.abnormal_sd

### Are chronic and subacute subscale symptoms different ###
wilcox.test((unlist(CISSS.subacute.abnormal.df)), (unlist(CISSS.chronic.abnormal)))
wilcox.test((unlist(CISSC.subacute.abnormal.df)), (unlist(CISSC.chronic)))
wilcox.test((unlist(CISSV.subacute.abnormal.df)), (unlist(CISSV.chronic)))

##### Vision Symptoms are different for sub-acute and chronic

### Are chronic and subacute subscale symptoms different ###
CISSS.mdcc <- data.frame(abnormal_visual_function.mdcc %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
CISSC.mdcc <- data.frame(abnormal_visual_function.mdcc %>% select(Q5, Q6, Q9, Q14, Q15))
CISSV.mdcc <- data.frame(abnormal_visual_function.mdcc %>% select(Q7, Q8, Q13))

CISSS.ref <- data.frame(abnormal_visual_function.ref %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
CISSC.ref <- data.frame(abnormal_visual_function.ref %>% select(Q5, Q6, Q9, Q14, Q15))
CISSV.ref <- data.frame(abnormal_visual_function.ref %>% select (Q7, Q8, Q13))

wilcox.test((unlist(CISSS.mdcc)), (unlist(CISSS.ref)))
wilcox.test((unlist(CISSC.mdcc)), (unlist(CISSC.ref)))
wilcox.test((unlist(CISSV.mdcc)), (unlist(CISSV.ref)))

#### All are Different #####

####### Sub-scales for Abnormal Visual Function Girls/Boys ########
####### Girl

CISSS.girl <- data.frame(abnormal_visual_function.girl %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
total_sum_CISSS.girl <- sum(unlist(CISSS.girl))
print(total_sum_CISSS.girl)
total_sum_CISSS.girl_weight <- total_sum_CISSS.girl/7
total_sum_CISSS.girl_weight
Normalized_CISSS.girl <- total_sum_CISSS.girl_weight/113
Normalized_CISSS.girl

CISSS.girl_sd <- sd(unlist(CISSS.girl))
CISSS.girl_sd

###Performance/Cognitive
CISSC.girl <- data.frame(abnormal_visual_function.girl %>% select(Q5, Q6, Q9, Q14, Q15))
CISSC.girl
total_sum_CISSC.girl <- sum(unlist(CISSC.girl))
print(total_sum_CISSC.girl)
total_sum_CISSC.girl_weight <- total_sum_CISSC.girl/5
total_sum_CISSC.girl_weight
Normalized_CISSC.girl <- total_sum_CISSC.girl_weight/113
Normalized_CISSC.girl

CISSC.girl_sd <- sd(unlist(CISSC.girl))
CISSC.girl_sd

###### Vision 
CISSV.girl <- data.frame(abnormal_visual_function.girl %>% select(Q7, Q8, Q13))
CISSV.girl

total_sum_CISSV.girl <- sum(unlist(CISSV.girl))
print(total_sum_CISSV.girl)
total_sum_CISSV.girl_weight <- total_sum_CISSV.girl/3
total_sum_CISSV.girl_weight
Normalized_CISSV.girl <- total_sum_CISSV.girl_weight/113
Normalized_CISSV.girl

CISSV.girl_sd <- sd(unlist(CISSV.girl))
CISSV.girl_sd

########## Boy
CISSS.boy <- data.frame(abnormal_visual_function.boy %>% select(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
total_sum_CISSS.boy <- sum(unlist(CISSS.boy))
print(total_sum_CISSS.boy)
total_sum_CISSS.boy_weight <- total_sum_CISSS.boy/7
total_sum_CISSS.boy_weight
Normalized_CISSS.boy <- total_sum_CISSS.boy_weight/51
Normalized_CISSS.boy

CISSS.boy_sd <- sd(unlist(CISSS.boy))
CISSS.boy_sd

###Performance/Cognitive
CISSC.boy <- data.frame(abnormal_visual_function.boy %>% select(Q5, Q6, Q9, Q14, Q15))
CISSC.boy
total_sum_CISSC.boy <- sum(unlist(CISSC.boy))
print(total_sum_CISSC.boy)
total_sum_CISSC.boy_weight <- total_sum_CISSC.boy/5
total_sum_CISSC.boy_weight
Normalized_CISSC.boy <- total_sum_CISSC.boy_weight/51
Normalized_CISSC.boy

CISSC.boy_sd <- sd(unlist(CISSC.boy))
CISSC.boy_sd

###### Vision 
CISSV.boy <- data.frame(abnormal_visual_function.boy %>% select(Q7, Q8, Q13))
CISSV.boy

total_sum_CISSV.boy <- sum(unlist(CISSV.boy))
print(total_sum_CISSV.boy)
total_sum_CISSV.boy_weight <- total_sum_CISSV.boy/3
total_sum_CISSV.boy_weight
Normalized_CISSV.boy <- total_sum_CISSV.boy_weight/51
Normalized_CISSV.boy

CISSV.boy_sd <- sd(unlist(CISSV.boy))
CISSV.boy_sd

### Are chronic and subacute subscale symptoms different ###
wilcox.test((unlist(CISSS.girl)), (unlist(CISSS.boy)))
wilcox.test((unlist(CISSC.girl)), (unlist(CISSC.boy)))
wilcox.test((unlist(CISSV.girl)), (unlist(CISSV.boy)))
#### Cognitive symptoms are different

Concussed.df <- Concussed.df %>% mutate(abnormal.vf = npc_fail == 1 | AA_FAIL == 1)
View(Concussed.df)

############################################################### Subscales (Total Score per subscale)

### abnormal and normal in general, first starting with abnormal
##Somatic

#### Adding total Scores for each subscale
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(Somatic.total = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(Performance.total = sum(Q5,Q6,Q9,Q14,Q15))
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(Visual.total = sum(Q7,Q8,Q13))
View(Concussed.df)

abnormal_visual_function <- abnormal_visual_function %>% rowwise() %>% mutate(Somatic.total = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
abnormal_visual_function <- abnormal_visual_function %>% rowwise() %>% mutate(Performance.total = sum(Q5,Q6,Q9,Q14,Q15))
abnormal_visual_function <- abnormal_visual_function %>% rowwise() %>% mutate(Visual.total = sum(Q7,Q8,Q13))
View(abnormal_visual_function)

normal_visual_function <- normal_visual_function %>% rowwise() %>% mutate(Somatic.total = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12))
normal_visual_function <- normal_visual_function %>% rowwise() %>% mutate(Performance.total = sum(Q5,Q6,Q9,Q14,Q15))
normal_visual_function <- normal_visual_function %>% rowwise() %>% mutate(Visual.total = sum(Q7,Q8,Q13))
View(normal_visual_function)

####### Are they different?

wilcox.test(abnormal_visual_function$Somatic.total, normal_visual_function$Somatic.total)
wilcox.test(abnormal_visual_function$Performance.total, normal_visual_function$Performance.total)
wilcox.test(abnormal_visual_function$Visual.total, normal_visual_function$Visual.total)
##### Visual Symptoms are different




#### ANOVA with Bonferroni Corrections to look at Differences between Normal and Abnormal VF Group Differences

abnormal.subscales.all <- data.frame(abnormal_visual_function$Somatic.total, 
                                     abnormal_visual_function$Performance.total,
                                     abnormal_visual_function$Visual.total)
normal.subscales.all <- data.frame(normal_visual_function$Somatic.total,
                                   normal_visual_function$Performance.total,
                                   normal_visual_function$Visual.total)

subscales.all <- data.frame(Concussed.df$Somatic.total, Concussed.df$Performance.total,
                            Concussed.df$Visual.total)

View(subscales.all)
View(abnormal.subscales.all)
View(normal.subscales.all)

anova.somatic <- aov(Concussed.df$Somatic.total ~ Concussed.df$abnormal.vf * Concussed.df$Temporality *
                       Concussed.df$Gender * Concussed.df$Clinic_type * Concussed.df$Type_con,
                           data = Concussed.df)
summary(anova.somatic)

anova.performance <- aov(Concussed.df$Performance.total ~ Concussed.df$Temporality *
                           Concussed.df$Gender * Concussed.df$Clinic_type * Concussed.df$Type_con,
                         data = Concussed.df)
summary(anova.performance)

anova.visual <- aov(Concussed.df$Visual.total ~ Concussed.df$Temporality *
                      Concussed.df$Gender * Concussed.df$Clinic_type * Concussed.df$Type_con,
                    data = Concussed.df)
summary(anova.visual)

#### Visual Symptoms are Significant


####### Mean Likert Ratings/Normalized for Abnormal and Normal ANOVA ---> same as subscale total differences

Concussed.df <- Concussed.df %>% rowwise() %>% mutate(Somatic.normalized = (sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12))/7)
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(Performance.normalized = (sum(Q5,Q6,Q9,Q14,Q15))/5)
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(Visual.normalized = (sum(Q7,Q8,Q13))/3)

View(Concussed.df)

anova.somatic.normalized <- aov(Concussed.df$Somatic.normalized ~ Concussed.df$abnormal.vf,
                                data = Concussed.df)
summary(anova.somatic.normalized)
anova.perfomance.normalized <- aov(Concussed.df$Performance.normalized ~ Concussed.df$abnormal.vf,
                                   data = Concussed.df)
summary(anova.perfomance.normalized)
anova.visual.normalized <- aov(Concussed.df$Visual.normalized ~ Concussed.df$abnormal.vf,
                               data = Concussed.df)
summary(anova.visual.normalized)



abnormal_visual_function$Type_con <- factor(abnormal_visual_function$Type_con)

anova.abnormal <- aov(
                  abnormal_visual_function$CISS_total_15 
                  ~ abnormal_visual_function$Gender* 
                  abnormal_visual_function$Temporality* 
                  abnormal_visual_function$Clinic_type*
                  abnormal_visual_function$Age*
                  abnormal_visual_function$Type_con,
                  data = abnormal_visual_function)

summary(anova.abnormal)

pairwise.t.test(abnormal_visual_function$CISS_total_15, abnormal_visual_function$Clinic_type,
                p.adjust.method = "bonferroni")


###### Within abnormal - clinic type continues to be significant but all others are not significant

normal_visual_function$Type_con <- factor(normal_visual_function$Type_con)

anova.normal <- aov(
                    normal_visual_function$CISS_total_15 
                    ~ normal_visual_function$Gender* 
                      normal_visual_function$Temporality* 
                      normal_visual_function$Clinic_type*
                      normal_visual_function$Type_con*
                      normal_visual_function$Age,
                    data = normal_visual_function)
summary(anova.normal)

###### Within normal - Type/Mechanism of concussion is almost significant all others are not significant

################################## ANOVA's for Abnormal Sub-scales

Somatic.anova <- aov(
                      abnormal_visual_function$Somatic.total 
                    ~ abnormal_visual_function$Gender * 
                      abnormal_visual_function$Temporality * 
                      abnormal_visual_function$Clinic_type * 
                      abnormal_visual_function$Type_con * 
                      abnormal_visual_function$Age,
                     data = abnormal_visual_function)
summary(Somatic.anova)


Performance.anova <- aov(
                      abnormal_visual_function$Performance.total 
                      ~ abnormal_visual_function$Gender*
                      abnormal_visual_function$Temporality* 
                      abnormal_visual_function$Clinic_type* 
                      abnormal_visual_function$Type_con*
                      abnormal_visual_function$Age,
                         data = abnormal_visual_function)
summary(Performance.anova)

Visual.anova <- aov(
                  abnormal_visual_function$Visual.total 
                  ~ abnormal_visual_function$Gender* 
                  abnormal_visual_function$Temporality* 
                  abnormal_visual_function$Clinic_type* 
                  abnormal_visual_function$Type_con*
                  abnormal_visual_function$Age,
                    data = abnormal_visual_function)

summary(Visual.anova)


#################################### ANOVA's for Normal Sub-Scales

Somatic.normal.anova <- aov(
                        normal_visual_function$Somatic.total 
                        ~ normal_visual_function$Gender*
                        normal_visual_function$Temporality*
                        normal_visual_function$Clinic_type*
                        normal_visual_function$Type_con*
                        normal_visual_function$Age,
                        data = normal_visual_function)

summary(Somatic.normal.anova)

Performance.normal.anova <- aov(
                            normal_visual_function$Performance.total
                            ~ normal_visual_function$Gender*
                            normal_visual_function$Temporality*
                            normal_visual_function$Clinic_type*
                            normal_visual_function$Type_con*
                            normal_visual_function$Age,
                            data = normal_visual_function)


summary(Performance.normal.anova)

Visual.normal.anova <- aov(
                      normal_visual_function$Visual.total
                      ~ normal_visual_function$Gender*
                      normal_visual_function$Temporality*
                      normal_visual_function$Clinic_type*
                      normal_visual_function$Type_con*
                      normal_visual_function$Age,
                      data = normal_visual_function)

summary(Visual.normal.anova)







######### Kruskal-Wallis

kruskal.test(Concussed.df$CISSS_total_normalized, 
             Concussed.df$CISSP_total_normalized,
             Concussed.df$CISSV_total_normalized)



wilcox.test(Concussed.df$CISSS_total_normalized, Concussed.df$CISSP_total_normalized)
wilcox.test(Concussed.df$CISSS_total_normalized, Concussed.df$CISSV_total_normalized)
wilcox.test(Concussed.df$CISSP_total_normalized, Concussed.df$CISSV_total_normalized)

summary(Concussed.df$CISSS_total_normalized)
summary(Concussed.df$CISSP_total_normalized)
summary(Concussed.df$CISSV_total_normalized)


Normalized_Subscores <- data.frame(Scores = c(Concussed.df$CISSS_total_normalized,
                                              Concussed.df$CISSP_total_normalized,
                                              Concussed.df$CISSV_total_normalized),
                                   Sub_Scores = rep(c("Somatic", "Performance", "Visual"), each = 210))
                                   
                                   
ggplot(Normalized_Subscores, aes(x = Sub_Scores, y = Scores)) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  scale_fill_manual(values = c("lightpink", "lightblue1", "lightgreen")) +
  geom_point(position = position_jitter(0.2), size = 1)


View(MDCC.df)
mean(MDCC.df$CISS_total_15)
sd(MDCC.df$CISS_total_15)
mean(Ref.df$CISS_total_15)

mean(abnormal_visual_function$CISS_total_15)
sd(abnormal_visual_function$CISS_total_15)
mean(normal_visual_function$CISS_total_15)


MDCC.df <- MDCC.df %>% rowwise() %>% mutate(CISSS_total_normalized = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12)/7)
Ref.df <- Ref.df %>% rowwise() %>% mutate(CISSS_total_normalized = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12)/7)


mean(MDCC.df$CISSS_total_normalized)
sd(MDCC.df$CISSS_total_normalized)
mean(Ref.df$CISSS_total_normalized)

mean(Concussed.df$CISS_total_15)
sd(Concussed.df$CISS_total_15)

mean(Concussed.df$CISSS_total_normalized)
sd(Concussed.df$CISSS_total_normalized)

mean(Concussed.df$CISSP_total_normalized)
sd(Concussed.df$CISSP_total_normalized)

mean(Concussed.df$CISSV_total_normalized)
sd(Concussed.df$CISSV_total_normalized)

table(Concussed.df$Gender)

mean(Concussed.df$Age)
sd(Concussed.df$Age)


View(Ref.df)

mean(Ref.df$CISSV_total_normalized)
sd(Ref.df$CISSV_total_normalized)

mean(MDCC.df$CISSV_total_normalized)
sd(MDCC.df$CISSV_total_normalized)


mean(subacute.df$CISSV_total_normalized)
sd(subacute.df$CISSV_total_normalized)

mean(chronic.df$CISSV_total_normalized)
sd(chronic.df$CISSV_total_normalized)

mean(abnormal_visual_function$CISSV_total_normalized)
sd(abnormal_visual_function$CISSV_total_normalized)


mean(normal_visual_function$CISSV_total_normalized)
sd(normal_visual_function$CISSV_total_normalized)

mean(MDCC.df$CISSS_total_normalized)
sd(MDCC.df$CISSS_total_normalized)

table(Concussed.df$Gender)

summary(Concussed.df$Age)


###### Different for each question (abnormal vs normal) ---> not for Total Score
wilcox.test(abnormal_visual_function$CISS_total_15, normal_visual_function$CISS_total_15)

wilcox.test(abnormal_visual_function$Q1, normal_visual_function$Q1)
wilcox.test(abnormal_visual_function$Q2, normal_visual_function$Q2)
wilcox.test(abnormal_visual_function$Q3, normal_visual_function$Q3) ### significant
wilcox.test(abnormal_visual_function$Q4, normal_visual_function$Q4)
wilcox.test(abnormal_visual_function$Q5, normal_visual_function$Q5)
wilcox.test(abnormal_visual_function$Q6, normal_visual_function$Q6)
wilcox.test(abnormal_visual_function$Q7, normal_visual_function$Q7) 
wilcox.test(abnormal_visual_function$Q8, normal_visual_function$Q8)
wilcox.test(abnormal_visual_function$Q9, normal_visual_function$Q9)
wilcox.test(abnormal_visual_function$Q10, normal_visual_function$Q10)
wilcox.test(abnormal_visual_function$Q11, normal_visual_function$Q11)
wilcox.test(abnormal_visual_function$Q12, normal_visual_function$Q12)
wilcox.test(abnormal_visual_function$Q13, normal_visual_function$Q13) #### significant
wilcox.test(abnormal_visual_function$Q14, normal_visual_function$Q14)
wilcox.test(abnormal_visual_function$Q15, normal_visual_function$Q15)



###### Categorical Data for Abnormal/Normal Visual Function Patients

chisq.test(Concussed.df$abnormal.vf, Concussed.df$Gender, correct = T) ## not significant
chisq.test(Concussed.df$abnormal.vf, Concussed.df$Temporality, correct = T) ## not significant
chisq.test(Concussed.df$abnormal.vf, Concussed.df$Clinic_type, correct = T) ## significant
table(Concussed.df$abnormal.vf, Concussed.df$Clinic_type) ### More patients 
prop.table(table(Concussed.df$abnormal.vf, Concussed.df$Clinic_type))*100
table(Concussed.df$Clinic_type) ### more referred patients with abnormal visual function

chisq.test(Concussed.df$abnormal.vf, Concussed.df$Age_Group, correct = T) ## Significant


quantile(abnormal_visual_function$CISS_total_15)
quantile(normal_visual_function$CISS_total_15)

quantile(Concussed.df$TSC)

View(all.data.df)

########## Normalized subscales data ----

quantile(abnormal_visual_function$CISSS_total_normalized)
quantile(abnormal_visual_function$CISSP_total_normalized)
quantile(abnormal_visual_function$CISSV_total_normalized)

quantile(normal_visual_function$CISSS_total_normalized)
quantile(normal_visual_function$CISSP_total_normalized)
quantile(normal_visual_function$CISSV_total_normalized)

View(all.data.df)


### Checking variables
#### all data
data.class(all.data.df$Age)
all.data.df$Concussed <- as.factor(all.data.df$Concussed)
data.class(all.data.df$Concussed)
all.data.df$Concussed <- relevel(all.data.df$Concussed, ref=2) ## reference category is Control
data.class(all.data.df$Gender)
all.data.df$Gender <- as.factor(all.data.df$Gender)
all.data.df$Gender <- relevel(all.data.df$Gender, ref = 1) ### reference category is Boy

### Concussed set references -----
Concussed.df$Gender <- as.factor(Concussed.df$Gender)
Concussed.df$Gender <- relevel(Concussed.df$Gender, ref = 1) ### reference is boy
data.class(Concussed.df$Gender)

Concussed.df$Phase <- as.factor(Concussed.df$Phase)
data.class(Concussed.df$Phase)
Concussed.df$Phase <- relevel(Concussed.df$Phase, ref = "Chronic") ## reference category is chronic
table(Concussed.df$Phase)

table(Concussed.df$Phase)

Concussed.df$Clinic_type <- as.factor(Concussed.df$Clinic_type)
Concussed.df$Clinic_type <- relevel(Concussed.df$Clinic_type, ref = 2) ## reference category is referred

table(Concussed.df$Clinic_type)
data.class(Concussed.df$Clinic_type)

data.class(Concussed.df$abnormal.vf)
Concussed.df$abnormal.vf <- as.factor(Concussed.df$abnormal.vf)
table(Concussed.df$abnormal.vf)

Concussed.df$abnormal.vf <- ifelse(Concussed.df$abnormal.vf == "YES", "Abnormal", "Normal")
Concussed.df$abnormal.vf <- as.factor(Concussed.df$abnormal.vf)
Concussed.df$abnormal.vf <- relevel(Concussed.df$abnormal.vf, ref = "Normal")
table(Concussed.df$abnormal.vf)

data.class(Concussed.df$Age)
data.class(Concussed.df$TSC)

### Abnormal VF
data.class(abnormal_visual_function$Gender)
abnormal_visual_function$Gender <- relevel(abnormal_visual_function$Gender, ref = 1)
  
data.class(abnormal_visual_function$Phase)
abnormal_visual_function$Phase <- relevel(abnormal_visual_function$Phase, ref = "Chronic")

data.class(abnormal_visual_function$Clinic_type)
abnormal_visual_function$Clinic_type <- relevel(abnormal_visual_function$Clinic_type, ref = 2)

data.class(abnormal_visual_function$Age)
data.class(abnormal_visual_function$TSC)

### Normal VF
data.class(normal_visual_function$Gender)
normal_visual_function$Gender<- relevel(normal_visual_function$Gender, ref = 1)

data.class(normal_visual_function$Phase)
normal_visual_function$Phase <- relevel(normal_visual_function$Phase, ref = "Chronic")

data.class(normal_visual_function$Clinic_type)
normal_visual_function$Clinic_type <- relevel(normal_visual_function$Clinic_type, ref = 2)

data.class(normal_visual_function$Age)
data.class(normal_visual_function$TSC)



######### Regressions

##### Comparison between controls and concussed with multiple regression analysis
Controls_vs_Concussed <- lm(CISS_total_15 ~ Gender + Concussed + Age, data = all.data.df)
summary(Controls_vs_Concussed)
confint(Controls_vs_Concussed)

View(all.data.df)

View(Concussed.df) 
### Temporality - 1 = subacute

############### Regression for all concussed ----
View(Concussed.df)
model <- lm(dependent_variable ~ independent_variable_1 + independent_variable_2 + ..., data = your_data_frame)
View(Concussed.df)
Symptoms_multiple_regression <- lm(CISS_total_15 ~ abnormal.vf + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(Symptoms_multiple_regression)
confint(Symptoms_multiple_regression)
plot(Symptoms_multiple_regression)

########## Checking for multicollinearity

vif_concussed_all <- car::vif(Symptoms_multiple_regression)
vif_concussed_all

##### all were around 1 meaning that there is no correlation



####### The way I did it for abstract


Symptoms_multiple_regression_abstract <- lm(CISS_total_15 ~ abnormal.vf + Clinic_type + Gender + Temporality, data = Concussed.df)
        confint(Symptoms_multiple_regression_abstract)

summary(Symptoms_multiple_regression_abstract)
nrow(Concussed.df)

View(Concussed.df)

nrow(abnormal_visual_function)
nrow(normal_visual_function)


plot(Concusssed.df$Age, Concussed.df$CISS_total_15)

##### Linear regression with just the exposure (visual function) 
Total_CISS_just_VF <- lm(CISS_total_15 ~ abnormal.vf, data = Concussed.df)
summary(Total_CISS_just_VF)

#### Regression with just visual function by subscale
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(CISSS_total_normalized = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12)/7)

CISSS_just_VF <- lm(CISSS_total_normalized ~ abnormal.vf, data = Concussed.df)
summary(CISSS_just_VF)

CISSP_just_VF <- lm(CISSP_total_normalized ~ abnormal.vf, data = Concussed.df)
summary(CISSP_just_VF)

CISSV_just_VF <- lm(CISSV_total_normalized ~ abnormal.vf, data = Concussed.df)
summary(CISSV_just_VF)

######### Running multiple regressions instead of Mann Whitney U tests since Abnormal VS Normal effect on CISS scores came up significantly different
######### with a regression

Abnormal_multiple_regression <- lm(CISS_total_15 ~ Gender + Clinic_type + Temporality + Age, data = abnormal_visual_function)
summary(Abnormal_multiple_regression)
confint(Abnormal_multiple_regression)

###### Normal Visual Function Model

normal_visual_function_regression <- lm(CISS_total_15 ~ Gender + Clinic_type + Temporality + Age, data = normal_visual_function)
summary(normal_visual_function_regression)
confint(normal_visual_function_regression)

########### Model for CISSV - subacute and chronic
abnormal_visual_function <- abnormal_visual_function %>% rowwise() %>% mutate(CISSV_total_normalized = sum(Q7,Q8,Q13)/3)
View(abnormal_visual_function)

CISSV_normalized_model <- lm(CISSV_total_normalized ~ Gender + Clinic_type + Temporality, data = abnormal_visual_function)
summary(CISSV_normalized_model)

###### Model for CISS-S scores
abnormal_visual_function <- abnormal_visual_function %>% rowwise() %>% mutate(CISSS_total_normalized = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12)/7)

CISSS_normalized_model <- lm(CISSS_total_normalized ~ Age + Gender + Clinic_type + Phase, data = abnormal_visual_function)
summary(CISSS_normalized_model)
confint(CISSS_normalized_model)

########## not different for gender as said in abstract
abnormal_visual_function <- abnormal_visual_function %>% rowwise() %>% mutate(CISSC_total_normalized = sum(Q5, Q6, Q9, Q14, Q15)/5)

CISSC_normalized_model <- lm(CISSC_total_normalized ~ Gender + Clinic_type + Temporality, data = abnormal_visual_function)

summary(CISSC_normalized_model)

####### Model for total sub-scores
CISSS_total_model <- lm(Somatic.total ~ abnormal.vf + Clinic_type + Temporality + Gender + Age, data = Concussed.df)
summary(CISSS_total_model)
######### only clinic type significant

CISSV_total_model <- lm(Visual.total ~ abnormal.vf + Clinic_type + Temporality + Gender, data = Concussed.df)
summary(CISSV_total_model)
########## clinic type, temporality, and abnormal vf

CISSC_total_model <- lm(Performance.total ~ abnormal.vf + Clinic_type + Temporality + Gender, data = Concussed.df)
summary(CISSC_total_model)
######### nothing significant

#################### CISS normalized subscores within total concussed group ----- 
View(Concussed.df)

mean(abnormal_visual_function$CISS_total_15)
mean(normal_visual_function$CISS_total_15)

Concussed.df <- Concussed.df %>% rowwise() %>% mutate(CISSS_total_normalized = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12)/7)

Control.df <- Control.df %>% rowwise() %>% mutate(CISSS_total_normalized = sum(Q1, Q2, Q3, Q4, Q10, Q11, Q12)/7)

CISSS_total_normalized_concussed <- lm(CISSS_total_normalized ~ abnormal.vf + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(CISSS_total_normalized_concussed)
confint(CISSS_total_normalized_concussed)

######### checking for multicollienarity
vif(CISSS_total_normalized_concussed)

##### With CISSS subscores abnormal vf, clinic type, and age were significant

plot(Concussed.df$Age, Concussed.df$CISSS_total_normalized)

confint(CISSS_total_normalized_concussed)

########## CISS-V normalized subscores
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(CISSV_total_normalized = sum(Q7,Q8,Q13)/3)

Control.df <- Control.df %>% rowwise() %>% mutate(CISSV_total_normalized = sum(Q7,Q8,Q13)/3)

CISSV_total_normalized_concussed <- lm(CISSV_total_normalized ~ abnormal.vf + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(CISSV_total_normalized_concussed)

confint(CISSV_total_normalized_concussed)

##### check for collinearity
vif(CISSV_total_normalized_concussed)


###### CISS-P normalized subscores
Concussed.df <- Concussed.df %>% rowwise() %>% mutate(CISSP_total_normalized = sum(Q5, Q6, Q9, Q14, Q15)/5)
Control.df <- Control.df %>% rowwise() %>% mutate(CISSP_total_normalized = sum(Q5, Q6, Q9, Q14, Q15)/5)


CISSP_total_normalized_concussed <- lm(CISSP_total_normalized ~ abnormal.vf + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(CISSP_total_normalized_concussed)

confint(CISSP_total_normalized_concussed)

##### Age is significant

View(Concussed.df)

### check for collinearity
vif(CISSP_total_normalized_concussed)

##### Regressions for Clinic Type
MDCC_total <- lm(CISS_total_15 ~ abnormal.vf + Gender + Phase + Age, data = MDCC.df)
summary(MDCC_total)


Ref_total <- lm(CISS_total_15 ~ abnormal.vf + Gender + Phase + Age, data = Ref.df)
summary(Ref_total)


############# Regression with NPC as dependent variable and Age, CISS Score, Gender, Phase as independent -----
NPC <- lm(NPC ~ CISS_total_15 + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(NPC)
confint(NPC)

######## checking for multicollinearity
vif(NPC)

###### NPC as dependent variable for just CISS-V
NPC_CISSV <- lm(NPC ~ CISSV_total_normalized + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(NPC_CISSV)
confint(NPC_CISSV)

####### checking for multicollinearity
vif(NPC_CISSV)

##### NPC as dependent variable for just CISS-S
NPC_CISSS <- lm(NPC ~ CISSS_total_normalized + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(NPC_CISSS)

#### NPC as dependent variable for just CISS-P
NPC_CISSP <- lm(NPC ~ CISSP_total_normalized + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(NPC_CISSP)

############ Regression with AA as dependent variable ----
AA <- lm(Worse_Eye_AA ~ CISS_total_15 + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(AA)
confint(AA)

###### multicollinearity
vif(AA)

####### AA as dependent variable for just CISSV
AA_CISSV <- lm(Worse_Eye_AA ~ CISSV_total_normalized + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(AA_CISSV)
confint(AA_CISSV)

##### multicollinearity
vif(AA_CISSV)

####### AA as dependent variable for just CISSS
AA_CISSS <- lm(Worse_Eye_AA ~ CISSS_total_normalized + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(AA_CISSS)
confint(AA_CISSS)

####### AA as dependent variable for just CISSP
AA_CISSP <- lm(Worse_Eye_AA ~ CISSP_total_normalized + Clinic_type + Gender + Phase + Age, data = Concussed.df)
summary(AA_CISSP)
confint(AA_CISSP)
