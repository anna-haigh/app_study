# Learning with identification apps and dichotomous keys in undergraduate forestry education in the PNW
# Authors: Dawn Anzinger and Anna E. Haigh
# R Script and Data Analysis: Anna E. Haigh

library(tidyverse)

# Identification Accuracy
TreeApps<-read.csv("TreeApps.csv")
head(TreeApps)
Genus<-(TreeApps$X.Genus)
Species<-(TreeApps$X.Species)
App<-(TreeApps$App)
Div<-(TreeApps$Division)
Seed<-(as.factor(TreeApps$Flower.Fruit.Cone))


# Confidence Interval Function
CI<-function(object){
  mean<-mean(object)
  n<-length(object)
  stdev<-sd(object)
  se<- stdev / sqrt(n)
  alpha<-0.10
  df<-n-1
  t_score<-qt(p = alpha/2, df=df, lower.tail=FALSE)
  error <- t_score*se
  return(error)
}

# Means and 90% CI for whole data set
mean <- TreeApps %>%
  group_by(App) %>%
  summarise(
    n = n(),
    mean_species = mean(X.Species, na.rm = TRUE),
    mean_genus = mean (X.Genus, na.rm = TRUE),
    ci_species = CI(X.Species),
    ci_genus = CI(X.Genus),
    .groups = 'drop'
  )
print(mean)

# Means and SE by division 
mean_data <- TreeApps %>%
  filter(!is.na(Division), !is.na(App)) %>%  
  group_by(App, Division) %>%
  mutate(App = factor(App, levels = c("Key","vTree","Seek","TreesPNW"))) %>%
  summarise(
    n = n(),  
    mean_species = mean(X.Species, na.rm = TRUE),
    mean_genus = mean(X.Genus, na.rm = TRUE),
    se_species = sd(X.Species, na.rm = TRUE) / sqrt(sum(!is.na(X.Species))),
    se_genus = sd(X.Genus, na.rm = TRUE) / sqrt(sum(!is.na(X.Genus))),
    .groups = 'drop'
  )
print(mean_data)

# Split dataset by division
conifer<-subset(TreeApps, Div=="Pinophyta")
hwood<-subset(TreeApps, Div=="Magnoliophyta")


# t-tests to compare accuracy between dichotomous key and each app
t.test(Species[App=="Key"],Species[App=="TreesPNW"], alternative="less")
t.test(Genus[App=="Key"],Genus[App=="TreesPNW"], alternative="less")

t.test(Species[App=="Key"],Species[App=="Seek"], alternative="less")
t.test(Genus[App=="Key"],Genus[App=="Seek"], alternative="less")

t.test(Species[App=="Key"],Species[App=="vTree"], alternative="less")
t.test(Genus[App=="Key"],Genus[App=="vTree"], alternative="less")


# t-tests to compare accuracy by division
t.test(Species[Div=="Pinophyta"],Species[Div=="Magnoliophyta"])
t.test(Genus[Div=="Pinophyta"],Genus[Div=="Magnoliophyta"])

t.test(conifer$X.Species[conifer$Flower.Fruit.Cone=="1"],conifer$X.Species[conifer$Flower.Fruit.Cone=="0"])
t.test(hwood$X.Species[hwood$Flower.Fruit.Cone=="1"],hwood$X.Species[hwood$Flower.Fruit.Cone=="0"])
t.test(hwood$X.Genus[hwood$Flower.Fruit.Cone=="1"],hwood$X.Genus[hwood$Flower.Fruit.Cone=="0"])


#App specific subsets
Seek<-subset(TreeApps, App=="Seek")
TreesPNW<-subset(TreeApps, App=="TreesPNW")
vTree<-subset(TreeApps, App=="vTree")

t.test(Seek$X.Species[Seek$Flower.Fruit.Cone=="0"],Seek$X.Species[Seek$Flower.Fruit.Cone=="1"])
t.test(TreesPNW$X.Species[TreesPNW$Flower.Fruit.Cone=="0"],TreesPNW$X.Species[TreesPNW$Flower.Fruit.Cone=="1"])


boxplot(Species~App, main="Percent Correct Species by App")
boxplot(Genus~App, main="Percent Correct Genus by App")


t.test(Species[Seed=="0"],Species[Seed=="1"],data=TreeApps)


# Labels for plots
app_labels<- c(
  "Key" = "Key",
  "Seek" = "IR", 
  "TreesPNW" = "FG", 
  "vTree"="MK")

# Plots for genus and species accuracy by division
plot1 <- ggplot(
  data = mean_data, 
  aes(x = App, y = mean_species, fill = Division)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_species - 1.645 * se_species, 
        ymax = mean_species + 1.645 * se_species), 
    position = position_dodge(width = 0.9),
    width = 0.3, 
    linewidth = 0.8
  ) +
  labs(
    title = "Correct Species Identification by Learning Tool",
    x = "Learning Tool",
    y = "Mean % Correct Species ID",
    fill = "Division"
  ) +
  scale_fill_manual(
    values = c("Pinophyta" = "slategray1", "Magnoliophyta" = "orangered3")
  ) +
  scale_x_discrete(labels = app_labels) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  )
plot1

plot2 <- ggplot(
  data = mean_data, 
  aes(x = App, y = mean_genus, fill = Division)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_genus - 1.645 * se_genus, 
        ymax = mean_genus + 1.645 * se_genus), 
    position = position_dodge(width = 0.9),
    width = 0.3, 
    linewidth = 0.8
  ) +
  labs(
    title = "Correct Genus Identification by Learning Tool",
    x = "Learning Tool",
    y = "Mean % Correct Genus ID",
    fill = "Division"
  ) +
  scale_fill_manual(
    values = c("Pinophyta" = "slategray1", "Magnoliophyta" = "orangered3")
  ) +
  scale_x_discrete(labels = app_labels) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  )
plot2

# Hardwood identification accuracy by reproductive structure presence
View(hwood)
mean_cone <- TreeApps %>%
  filter(!is.na(Flower.Fruit.Cone), !is.na(App)) %>%  
  group_by(App, Flower.Fruit.Cone) %>%
  summarise(
    n = n(),  
    mean_species = mean(X.Species, na.rm = TRUE),
    mean_genus = mean(X.Genus, na.rm = TRUE),
    se_species = sd(X.Species, na.rm = TRUE) / sqrt(sum(!is.na(X.Species))),
    se_genus = sd(X.Genus, na.rm = TRUE) / sqrt(sum(!is.na(X.Genus))),
    .groups = 'drop'
  )
print(mean_cone)

plot3 <- ggplot(
  data = mean_cone, 
  aes(x = App, y = mean_genus, fill = as.factor(Flower.Fruit.Cone))
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_genus - 1.645 * se_genus, 
        ymax = mean_genus + 1.645 * se_genus), 
    position = position_dodge(width = 0.9),
    width = 0.3, 
    linewidth = 0.8
  ) +
  labs(
    title = "Correct Genus Identification by Learning Tool",
    x = "Learning Tool",
    y = "Mean % Correct Genus ID",
    fill = "Flower, Fruit, Cone"
  ) +
  scale_fill_manual(
    values = c("0" = "gray", "1" = "navy")
  ) +
  scale_x_discrete(labels = app_labels) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  )
plot3

plot4 <- ggplot(
  data = mean_cone, 
  aes(x = App, y = mean_species, fill = as.factor(Flower.Fruit.Cone))
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_species - 1.645 * se_species, 
        ymax = mean_species + 1.645 * se_species), 
    position = position_dodge(width = 0.9),
    width = 0.3, 
    linewidth = 0.8
  ) +
  labs(
    title = "Correct Species Identification by Learning Tool",
    x = "Learning Tool",
    y = "Mean % Correct Species ID",
    fill = "Flower, Fruit, Cone"
  ) +
  scale_fill_manual(
    values = c("0" = "gray", "1" = "navy")
  ) +
  scale_x_discrete(labels = app_labels) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  )
plot4


##################################################################
# Survey Data

survey<-read.csv("Survey_data.csv") %>%
  filter(!grepl("PlantNet",App)) #Remove Plant@Net for analysis

# Table of mean values and 90% CIs
mean_sur <- survey %>%
  group_by(App) %>%
  mutate(App = factor(App, levels = c("Key","vTree","Seek","TreesPNW"))) %>%
  mutate(Category = factor(Category, levels = c("Observation","Recognize","Confidence"))) %>%
  summarise(
    n = n(),  
    mean_response = mean(Response, na.rm = TRUE)*100,
    ci_response = CI(Response)*100,
    .groups = 'drop'
  )
print(mean_sur)

# Table of mean values and 90% CIs by category
mean_surv <- survey %>%
  group_by(App, Category) %>%
  mutate(App = factor(App, levels = c("Key","vTree","Seek","TreesPNW"))) %>%
  mutate(Category = factor(Category, levels = c("Observation","Recognize","Confidence"))) %>%
  summarise(
    n = n(),  
    mean_response = mean(Response, na.rm = TRUE)*100,
    ci_response = CI(Response)*100,
    .groups = 'drop'
  )
print(mean_surv)

# Assign labels
survey_labels <-c(
  "Confidence"="Self-efficacy",
  "Observation" = "Observation",
  "Recognize"="Recognition"
)

# Split by question category
confidence<- subset(survey, Category=="Confidence")
obs<- subset(survey, Category=="Observation")
recognize<-subset(survey, Category=="Recognize")

# Mean data and SE for plotting
mean_s <- survey %>%
  filter(!grepl("PlantNet",App)) %>%
  filter(!is.na(Category), !is.na(App)) %>%
  group_by(App, Category) %>%
  mutate(App = factor(App, levels = c("Key","vTree","Seek","TreesPNW"))) %>%
  mutate(Category = factor(Category, levels = c("Observation","Recognize","Confidence"))) %>%
  summarise(
    n = n(),  
    mean_response = mean(Response, na.rm = TRUE)*100,
    se_response = sd(Response, na.rm = TRUE) / sqrt(sum(!is.na(Response)))*100,
    .groups = 'drop'
  )
print(mean_s)


plot5 <- ggplot(
  data = mean_s,
  aes(x = App, y = mean_response, fill = Category)
) + 
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = mean_response - 1.645 * se_response, 
        ymax = mean_response + 1.645 * se_response), 
    position = position_dodge(width = 0.9),
    width = 0.3, 
    linewidth = 0.8
  ) +
  labs(
    title = "Mean Percent Agreeable Response by Category",
    x = "Learning Tool",
    y = "Mean Percent Agree",
    fill = "Category"
  ) +
  scale_fill_manual(
    values = c("Confidence" = "dodgerblue3", "Observation" = "firebrick2", "Recognize" = "wheat"), labels=survey_labels
  ) +
  scale_x_discrete(labels = app_labels) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
plot5


# ANOVA comparing responses between apps on each category
con_aov<- aov(Response ~ App, data=confidence)
anova(con_aov)

obs_aov<- aov(Response ~ App, data=obs)
anova(obs_aov)

rec_aov<- aov(Response ~ App, data=recognize)
anova(rec_aov)

sur_aov<- aov(Response ~ App, data = survey)
anova(sur_aov)

# t-tests comparing keys and apps
t.test(confidence$Response[confidence$App=="Key"],confidence$Response[confidence$App=="TreesPNW"])
t.test(confidence$Response[confidence$App=="Key"],confidence$Response[confidence$App=="Seek"])

t.test(survey$Response[survey$App=="Key"],survey$Response[survey$App=="TreesPNW"], alternative="greater")


# t-test for bark question
View(recognize)
bark<-survey[-c(1:474,554:1501,1581:2054),]
View(bark)

t.test(bark$Response[bark$App=="Key"],bark$Response[bark$App=="TreesPNW"])


# Filter data sets to include only app responses
app <- survey%>%
  filter(!grepl("Key",App))
app_obs <- obs%>%
  filter(!grepl("Key",App))
app_rec <- recognize%>%
  filter(!grepl("Key",App))
app_con <- confidence%>%
  filter(!grepl("Key",App))

View(app_obs)

# Means and CIs for all apps together split by question type (no key)
mean_app <- app %>%
  group_by(Category) %>%
  summarise(
    n = n(),  
    mean_response = mean(Response, na.rm = TRUE)*100,
    ci_response = CI(Response)*100,
    .groups = 'drop'
  )
print(mean_app)

# ANOVA comparing app response by category
appobs_aov <- aov(Response ~ App, data=app_obs)
anova(appobs_aov)

apprec_aov <- aov(Response ~ App, data=app_rec)
anova(apprec_aov)

appcon_aov <- aov(Response ~ App, data=app_con)
anova(appcon_aov)

app_aov <- aov(Response ~ App, data=app)
anova(app_aov)
