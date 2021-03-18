#Packages
library(tidyverse)
library(lme4)
library(ggpubr)
library(gridExtra)
library(dplyr)
library(car)
library(stringr)
library(sjPlot)
library(MASS)

#Functions
capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

##### Load in data ########
dataSet <- read_csv("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Data/kiri_whole_data_set_with_who_z_scores_z.csv") ###data with z scores added from kiri_who.R

####How I joined the original CGM and diet datasets together:
# dataSet1 <- read_csv("kiri_together_updated_bev_scores_cgm_removed_to_redo.csv")
# CGMData <- read_csv("cgm_data_whole_glyculator.csv")
# 
# dataSet <- full_join(dataSet1, CGMData, by=c("id", "timepoint", "group"))

#write.table(dataSet,paste("kiri_together_cgm_redone.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, col.names=TRUE)

##View
head(dataSet); tail(dataSet); summary(dataSet); str(dataSet); colnames(dataSet); dim(dataSet);

# dataSet$arm <- factor(ifelse(dataSet$group %in% "1", "Control",
#                      ifelse(dataSet$group %in% "2","Intervention",NA)),levels=c("Control","Intervention"))

##another way of doing this using dplyr
dataSet <- dataSet %>%
dplyr::mutate(am = factor(group, levels = c(1,2), labels = c("Control", "Intervention")))

######New variables creation####
dataSet$age_calculated <- dataSet$age_in_months/12
  
dataSet$bmi <- dataSet$weight_avg/(dataSet$height_avg/100)^2

dataSet$bmi_category <- ifelse(dataSet$age_in_months > 228 & dataSet$bmi < 18.5,"Underweight",
                             ifelse(dataSet$age_in_months > 228 & dataSet$bmi < 25,"Normal",
                                    ifelse(dataSet$age_in_months > 228 & dataSet$bmi < 30,"Overweight",
                                           ifelse(dataSet$age_in_months > 228 & dataSet$bmi < 100,"Obese",
                                                  ifelse(dataSet$zbfa < -1,"Underweight",
                                                         ifelse(dataSet$zbfa < 1,"Normal",
                                                                ifelse(dataSet$zbfa < 2,"Overweight",
                                                                       ifelse(dataSet$zbfa < 100,"Obese")
                                                                )
                                                         )
                                                  )
                                           )
                                    )
                             )
)


categories_diabetes <- c(0, 5.70, 6.50, 100)
dataSet$diabetes_category <- cut(dataSet$eA1c_whole, categories_diabetes,  labels = c(1, 2, 3), include.lowest=T, right = FALSE)

dataSet$pct_calories_total_sugar <- ((dataSet$`Total Sugars g`*4)/dataSet$`Energy kcal`)*100

dataSet$pct_calories_added_sugar <- ((dataSet$`Added Sugars by Total Sugars g`*4)/dataSet$`Energy kcal`)*100

dataSet$water_servings_per_day <- dataSet$water_servings/7

dataSet$tioka_servings_per_day <- dataSet$tioka_servings/7

dataSet$toddy_servings_per_day <- dataSet$toddy_servings/7

dataSet$tang_servings_per_day <- dataSet$tang_servings/7

dataSet$ice_block_servings_per_day <- dataSet$ice_block_servings/7

dataSet$soda_servings_per_day <- dataSet$soda_servings/7

dataSet$juice_servings_per_day <- dataSet$juice_servings/7

dataSet$fruit_drink_servings_per_day <- dataSet$fruit_drink_servings/7

dataSet$coffee_tea_servings_per_day <- dataSet$coffee_tea_servings/7

dataSet$bmi_percentile <- round(pnorm(dataSet$zbfa)*100,0)

dataSet$sugary_beverages_per_day <- dataSet$tioka_servings_per_day + dataSet$toddy_servings_per_day + dataSet$tang_servings_per_day + dataSet$tang_servings_per_day + dataSet$ice_block_servings_per_day + dataSet$soda_servings_per_day + dataSet$juice_servings_per_day + dataSet$fruit_drink_servings_per_day 

names(dataSet) <- gsub("\\[|\\]|\\*|\\/|\\%|\\>|\\)|\\(", "", names(dataSet))


cols.num <- c(3:546) #CHECK DIMENSIONS using colnames. 
dataSet[cols.num] <- sapply(dataSet[cols.num],as.numeric)
sapply(dataSet, class)

outcomeList <- names(dataSet)[61:530] #CHECK DIMENSIONS using colnames.Don't include BMI because it confuses the loop (no bmi data at timepoint 2) 

outcomesNamesList <- c("Outcomes");
alloutcomesList <- outcomeList;

coList <- numeric(0)
pValList <- numeric(0)
pre_cont_mean_list <- numeric(0)
pre_cont_sd_list <- numeric(0)
pre_cont_n_list <- numeric(0)
post_cont_mean_list <- numeric(0)
post_cont_sd_list <- numeric(0)
post_cont_n_list <- numeric(0)
pre_int_mean_list <- numeric(0)
pre_int_sd_list <- numeric(0)
pre_int_n_list <- numeric(0)
post_int_mean_list <- numeric(0)
post_int_sd_list <- numeric(0)
post_int_n_list <- numeric(0)

# SUBSET to remove the 1 participant with type 2 diabetes
# dataSet <- subset(dataSet, diabetes_category != 3)

# SUBSET to remove over 18s
dataSet <- subset(dataSet, age_in_months < 228)

dataSet$bmi_category <- ifelse(dataSet$zbfa < -1,"Underweight",
                               ifelse(dataSet$zbfa < 1,"Normal",
                                      ifelse(dataSet$zbfa < 2,"Overweight",
                                             ifelse(dataSet$zbfa < 100,"Obese")
                                                                  )
                                                           )
                                                    )

pdf("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Plots/plots.pdf", onefile = TRUE) #if you want one big pdf

for(i in seq(1:length(outcomeList)))
{
  print(outcomeList[i])
  
  thisDataInstance <- na.omit(data.frame(id=dataSet$id,
                                         Timepoint=dataSet$timepoint,
                                         outcome=dataSet[,names(dataSet) %in% outcomeList[i]],
                                         Group=dataSet$group,check.names = FALSE)) #####Check names FALSE required because otherwise will replace spaces with a dot and then two lists will not match up.
  
  names(thisDataInstance)[names(thisDataInstance) %in% outcomeList[i]] <- "outcome"
  
  thisDataInstance$outcome=capOutlier(thisDataInstance$outcome) ##Caps outliers using function at top. Basically makes anything outside 1.5*IQR into 5th/95th percentile
  
  Int <- thisDataInstance$Timepoint * thisDataInstance$Group
  
  reg = lm(outcome ~ Group + Timepoint + Int, data = thisDataInstance)
  
  summary(reg)
  
  coefficient <- summary(reg)$coefficient[4,1]
  pVal <- summary(reg)$coefficient[4,4]


  grouped_dataSet <- thisDataInstance %>%
    group_by(Group, Timepoint)
  
  summary <- dplyr::summarise(grouped_dataSet, mean = mean(outcome, na.rm=T), sd = sd(outcome, na.rm=T), n = n())
  
  pre_cont_mean <- summary$mean[1]
  pre_cont_sd <- summary$sd[1]
  pre_cont_n <- summary$n[1]
  post_cont_mean <- summary$mean[2]
  post_cont_sd <- summary$sd[2]
  post_cont_n <- summary$n[2]
  pre_int_mean <- summary$mean[3]
  pre_int_sd <- summary$sd[3]
  pre_int_n <- summary$n[3]
  post_int_mean <- summary$mean[4]
  post_int_sd <- summary$sd[4]
  post_int_n <- summary$n[4]
  
  grouped_dataSet_for_plot <- thisDataInstance %>%
    group_by(Group, Timepoint) %>%
    dplyr::summarise(outcome = mean(outcome, na.rm=T))
  
  title<-paste("Estimate = ", format(coefficient,digits=3),", p-value=", format.pval(summary(reg)$coefficient[4,4],digits=3), sep="")
  
  plots <- ggplot(thisDataInstance, aes(x = Timepoint, y = outcome, color = factor(Group))) +
    geom_line(aes(group = id), alpha = .3) +
    geom_line(data = grouped_dataSet_for_plot, alpha = .8, size = 3) +
    theme_bw() +
    labs(
      title = title,
      x = "Timepoint",
      y = outcomeList[i],
      color = NULL) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = c(1,2), labels = c("Pre-", "Post-")) +
    scale_color_discrete(name="Group",
                         breaks=c("1", "2"),
                         labels=c("Control", "Intervention"))
  
  print(plots) #if you want one big pdf
  
  pdf(paste0("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Plots/",outcomeList[i], ".pdf"))
  print(plots)
  dev.off()
  
  coList[[length(coList)+1]] <- coefficient
  pValList[[length(pValList)+1]] <- pVal
  pre_cont_mean_list[[length(pre_cont_mean_list)+1]] <- pre_cont_mean
  pre_cont_sd_list[[length(pre_cont_sd_list)+1]] <- pre_cont_sd
  pre_cont_n_list[[length(pre_cont_n_list)+1]] <- pre_cont_n
  post_cont_mean_list[[length(post_cont_mean_list)+1]] <- post_cont_mean
  post_cont_sd_list[[length(post_cont_sd_list)+1]] <- post_cont_sd
  post_cont_n_list[[length(post_cont_n_list)+1]] <- post_cont_n
  pre_int_mean_list[[length(pre_int_mean_list)+1]] <- pre_int_mean
  pre_int_sd_list[[length(pre_int_sd_list)+1]] <- pre_int_sd
  pre_int_n_list[[length(pre_int_n_list)+1]] <- pre_int_n
  post_int_mean_list[[length(post_int_mean_list)+1]] <- post_int_mean
  post_int_sd_list[[length(post_int_sd_list)+1]] <- post_int_sd
  post_int_n_list[[length(post_int_n_list)+1]] <- post_int_n
  
}

alloutcomesList <- cbind(alloutcomesList, coList, pValList, pre_cont_mean_list, pre_cont_sd_list, pre_cont_n_list, post_cont_mean_list,  post_cont_sd_list, post_cont_n_list, pre_int_mean_list, pre_int_sd_list, pre_int_n_list, post_int_mean_list, post_int_sd_list, post_int_n_list);
outcomesNamesList <- cbind(outcomesNamesList,paste("coefficient"),paste("pval"),paste('pre_cont_mean'),paste('pre_cont_sd'),paste ('pre_cont_n'),paste('post_cont_mean'),paste('post_cont_sd'),paste('post_cont_n'), paste('pre_int_mean'),paste('pre_int_sd'),paste('pre_int_n'), paste('post_int_mean'),paste('post_int_sd'),paste('post_int_n'))

dev.off() #if you want one big pdf

alloutcomesList<- data.frame(alloutcomesList)
names(alloutcomesList) <- outcomesNamesList
write.table(alloutcomesList,paste("/Users/JasminePlows/Documents/GitHub/continuous_glucose_monitoring_Kiritimati/Output/kiri_output.txt",sep=""),quote=FALSE, sep="\t",append=FALSE, row.names=FALSE, col.names=TRUE)




######Example for just checking one variable
thisDataInstance <- na.omit(data.frame(id=dataSet$id,
                                      Timepoint=dataSet$timepoint,
                                      outcome=dataSet$pct_calories_added_sugar, ####CHANGE THIS ONE ONLY
                                      Group=dataSet$group,check.names = FALSE))

thisDataInstance$outcome=capOutlier(thisDataInstance$outcome)

Int <- thisDataInstance$Timepoint * thisDataInstance$Group

reg = lm(outcome ~ Group + Timepoint + Int, data = thisDataInstance)

summary(reg)

gvlma::gvlma(reg)

###BOX-COX transformation for skewed data below

Box = boxcox(thisDataInstance$outcome ~ 1, # Transform outcome as a single vector
             lambda = seq(-6,6,0.1) # Try values -6 to 6 by 0.1
)

Cox = data.frame(Box$x, Box$y) # Create a data frame with the results

Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y

Cox2[1,] # Display the lambda with the greatest log likelihood



lambda = Cox2[1, "Box.x"]                 # Extract that lambda

thisDataInstance$transformed = (thisDataInstance$outcome ^ lambda - 1)/lambda   # Transform the original data


reg = lm(transformed ~ Group + Timepoint + Int, data = thisDataInstance)

summary(reg)

gvlma::gvlma(reg)


# 
####Continuous variables GROUPED
grouped_dataSet <- thisDataInstance %>%
group_by(Group, Timepoint)

summary <- summarise(grouped_dataSet, mean = mean(outcome, na.rm=T), sd = sd(outcome, na.rm=T), n = n())

summary

ggplot(grouped_dataSet, aes(x = outcome)) +
  geom_histogram(color = "grey30", fill = "white", bins = 100) +
  facet_grid(Group ~ Timepoint, scales = "free_x")
# 
# ####Continuous variables TOTAL SAMPLE
total_dataSet <- thisDataInstance %>%
  group_by(Timepoint)

total_summary <- summarise(total_dataSet, mean = mean(outcome, na.rm=T), sd = sd(outcome, na.rm=T))

total_summary
# 
# ##To find out frequencies
grouped_dataSet <- thisDataInstance %>%
  group_by(Group, Timepoint) %>%
  count(outcome) %>%
  mutate(prop=prop.table(n))
grouped_dataSet

# 
# #frequencies in total sample
total_dataSet <- thisDataInstance %>%
  group_by(Timepoint) %>%
  count(outcome) %>%
  mutate(prop=prop.table(n))
total_dataSet

# test <- dplyr::select(dataSet, "id", "group", "timepoint", "sex", "age", "dob", "age_calculated", "age_in_months", "height_avg", "weight_avg", "cbmi", "bmi", "zwfa", "zhfa", "zbfa", "bmi_percentile", "bmi_category") #ID8 should have diabetes
# View(test)
# 
# test <- dplyr::select(dataSet, "id", "group", "timepoint", "approx_tioka_days", "approx_tioka_amt", "tioka_servings", "tioka_servings_per_day")
# View(test)
# 
# dataSet$dob
