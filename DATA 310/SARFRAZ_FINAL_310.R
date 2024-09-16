load('C:/Users/hussainsarfraz/Desktop/DATA 310/ANES20Datafor310.Rdata')

#Packages
library(survey)
library(stargazer)
library(sandwich)
library(tidyverse)
library(textutils)

### defining independent and dependent variables ###
##Biden Feeling Thermometer - DEPENDENT VARIABLE
anes$biden.ft <- anes$V201151

anes$biden.ft[anes$biden.ft %in% c(-9,-4,998)] <- NA
summary(anes$biden.ft)

##BLM Movement Feeling Thermometer - INDEPENDENT VARIABLE
anes$blm.ft <- anes$V202174 

anes$blm.ft[anes$blm.ft %in% c(-9,-7,-6,-5,-4,998,999)] <- NA
summary(anes$blm.ft)
### creating new variables for difference of means 
#blm.ft
anes$blm.ft.category <- NA
anes$blm.ft.category[anes$blm.ft>=50] <- 1
anes$blm.ft.category[anes$blm.ft<50] <- 0

#blm.support
anes$blm.support<- NA
anes$blm.support[anes$blm.ft>=50] <- 1
anes$blm.support[is.na(anes$blm.support)] <- 0
# unique(anes$blm.support)

#blm.notsupport
anes$blm.notsupport<- NA
anes$blm.notsupport[anes$blm.ft<50] <- 1
anes$blm.notsupport[is.na(anes$blm.notsupport)] <- 0
# unique(anes$blm.support)

##Trump Feeling Thermometer - INDEPENDENT VARIABLE
anes$trump.ft <- anes$V201152

anes$trump.ft[anes$trump.ft %in% c(-9,-7,-6,-5,-4,998,999)] <- NA
summary(anes$trump.ft)

##Respondent liberal/conservative placement - INDEPENDENT VARIABLE
anes$respondent.LC.placement <- anes$V201200 

anes$respondent.LC.placement[anes$respondent.LC.placement %in% c(-9,-8,99)] <- NA
summary(anes$respondent.LC.placement)
#table(anes$respondent.LC.placement)

anes %>%
  filter(respondent.LC.placement == 4) %>%
  nrow() #1818 rows are being removed, converted to NA

### creating new variables for difference of means 
anes$liberal<- NA
anes$liberal[anes$respondent.LC.placement %in% c(1,2,3)] <- 1
anes$liberal[!(anes$respondent.LC.placement %in% c(1,2,3))] <- 0

anes$moderate<- NA
anes$moderate[anes$respondent.LC.placement %in% c(4)] <- 1
anes$moderate[!(anes$respondent.LC.placement %in% c(4))] <- 0

anes$conservative<- NA
anes$conservative[anes$respondent.LC.placement %in% c(5,6,7)] <- 1
anes$conservative[!(anes$respondent.LC.placement %in% c(5,6,7))] <- 0

##Respondent race/ethnicity - INDEPENDENT VARIABLE
anes$respondent.race <- anes$V201549x

anes$respondent.race[anes$respondent.race %in% c(-9,-8)] <- NA
summary(anes$respondent.race)
#table(anes$respondent.race)
### creating new variables for difference of means 
#res.race.category

anes$res.race.category <- NA
anes$res.race.category[anes$respondent.race %in% c(1)] <- 1
anes$res.race.category[anes$respondent.race %in% c(2,3,4,5,6)] <- 0
#anes$res.race.category[!(anes$respondent.race %in% c(1))] <- 0

unique(anes$res.race.category)
table(anes$res.race.category)

#res.white
anes$res.white <- NA
anes$res.white[anes$respondent.race %in% c(1)] <- 1
anes$res.white[is.na(anes$res.white)] <- 0

#res.nonwhite
anes$res.nonwhite <- NA
anes$res.nonwhite[anes$respondent.race %in% c(2,3,4,5,6)] <- 1
anes$res.nonwhite[is.na(anes$res.nonwhite)] <- 0

##Opinion on best way to deal with urban unrest and rioting - INDEPENDENT VARIABLE
anes$urban.unrest.opinion <- anes$V201429

anes$urban.unrest.opinion[anes$urban.unrest.opinion %in% c(-9,-8,99)] <- NA
summary(anes$urban.unrest.opinion)
#table(anes$urban.unrest.opinion)

##Opinion on building wall border with Mexico - INDEPENDENT VARIABLE
anes$mexico.border.opinion <- anes$V201424

anes$mexico.border.opinion[anes$mexico.border.opinion %in% c(-9,-8)] <- NA
summary(anes$mexico.border.opinion)
#table(anes$mexico.border.opinion)

##Opinion on U.S gov. policy towards unauthorized immigrants - INDEPENDENT VARIABLE
anes$us.immigrant.opinion <- anes$V201417

anes$us.immigrant.opinion[anes$us.immigrant.opinion %in% c(-9,-8)] <- NA
summary(anes$us.immigrant.opinion)
#table(anes$us.immigrant.opinion)

##Respondent age - INDEPENDENT VARIABLE
anes$age <- anes$V201507x

anes$age[anes$age==-9] <- NA
summary(anes$age)
#table(anes$age)

#Weight
anes$weight<- anes$V200010a

#Restrict to variables we care about
anes <- anes[,63:81]

#Create weight object
anes.w <- svydesign(ids = ~1,
                    data=anes,
                    weights = anes$weight)

#######################################################################

# ###Describing Main Variables - You need to run this in R-Markdown for it to work###
# 
# stargazer(as.data.frame(anes),
#           type = 'html',
#           title = "Legal Financial Obligations (LFOs) in Alabama Court Cases for African Americans",
#           covariate.labels = c('Remaining LFOs','LFOs Assesed','LFOs Paid to Date'),
#           digits = 2)
# 
# stargazer(as.data.frame(anes))
# 
# ###Plotting the Main Variables###
# 
# #biden and blm feeling thermometer
# anes %>% 
#   select(biden.ft, blm.ft) %>%
#   gather(key = 'which.ft', value = 'values') %>%
#   ggplot() + 
#   geom_density(aes(x=values,color=which.ft,fill=which.ft),alpha=0.3) +
#   scale_fill_manual(values = c( "#1b98e0","red"),
#                     name = "Color Legend",
#                     labels = c("Biden Feeling Thermometer", "BLM Feeling Thermometer")) +
#   scale_color_manual(values = c( "#1b98e0","red"),
#                     name = "Color Legend",
#                     labels = c("Biden Feeling Thermometer", "BLM Feeling Thermometer"))
# 
# #conservative-liberal placement
# anes %>%
#   ggplot()+
#   geom_bar(aes(x=respondent.LC.placement)) +
#   labs(title="Respondents Self-Placement of being Liberal-Conservative",  
#        x="Liberal-Conservative Scale", 
#        y = "Number of Respondents") +
#   
#   xlab(paste("\n<<< More Liberal", 
#              spaces(50), "More Conservative >>>")) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         plot.title = element_text(size = 10))
# 
# #respondent race
# anes %>%
#   ggplot()+
#   geom_bar(aes(x=respondent.LC.placement)) +
#   labs(title="Respondents Self-Placement of being Liberal-Conservative",  
#        x="Liberal-Conservative Scale", 
#        y = "Number of Respondents") +
#   
#   xlab(paste("\n<<< More Liberal", 
#              spaces(50), "More Conservative >>>")) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         plot.title = element_text(size = 10))
# 
# #respondent race
# anes %>%
#   ggplot()+
#   geom_bar(aes(x=respondent.race)) +
#   labs(title="Respondents Self-Identified Race",  
#        x="Respondents Race", 
#        y = "Number of Respondents") +
#   scale_x_continuous(breaks=c(1,2,3,4,5,6),
#                      labels = c('White','Black','Hispanic','Asian',' Native American','Mix')) + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         plot.title = element_text(size = 10))
# 
# #urban unrest opinion
# anes %>%
#   ggplot()+
#   geom_bar(aes(x=urban.unrest.opinion)) +
#   labs(title="Respondents Opinion on how to deal with Urban Unrest",  
#        x="Opinion on Urban Unrest", 
#        y = "Number of Respondents") +
#   scale_x_continuous(breaks=c(1,2,3,4,5,6,7)) + 
#   xlab(paste("\n<<< Address the Problems", 
#              spaces(50), "Use Force >>>")) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         plot.title = element_text(size = 10))
# 
# #Respondnet Opinion on building Mexico Border
# anes %>%
#   ggplot()+
#   geom_bar(aes(x=mexico.border.opinion)) +
#   labs(title="Respondents Opinion on Building Mexico Border",  
#        x="Respondents Opinion", 
#        y = "Number of Respondents") +
#   scale_x_continuous(breaks=c(1,2,3),
#                      labels = c('Favor','Oppose','Neither Favor nor Oppose')) + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(size = 8),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         plot.title = element_text(size = 10))
# 
# #Respondent Opinion in Unauthorized Immigrants
# anes %>%
#   ggplot()+
#   geom_bar(aes(x=us.immigrant.opinion)) +
#   labs(title="Respondents Opinion on how to deal with Unauthorized Immigrants",  
#        x="Opinion on how to deal with Unauthorized Immigrants", 
#        y = "Number of Respondents") +
#   scale_x_continuous(breaks=c(1,2,3,4)) + 
#   xlab(paste("\n<<< Send Immigrants Back", 
#              spaces(50), "Accept Immigrants >>>")) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         plot.title = element_text(size = 10))
# 
# #age 
# 
# 
# anes %>% 
#   ggplot() + 
#   geom_density(aes(x=age),alpha=0.3) +
#   scale_fill_manual(values = c( "#1b98e0","red"),
#                     name = "Color Legend",
#                     labels = c("Biden Feeling Thermometer", "BLM Feeling Thermometer")) +
#   scale_color_manual(values = c( "#1b98e0","red"),
#                      name = "Color Legend",
#                      labels = c("Biden Feeling Thermometer", "BLM Feeling Thermometer")) +
#   xlab("Feeling Thermometer Score") +
#   ylab('Density') +
#   ggtitle("Joe Biden and Black Lives Matter (BLM) Feeling Thermometers") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         plot.title = element_text(size = 10))
##########################################################################################
##########################################################################################
# ### creating new variables for difference of means ###
# #blm.ft
# anes$blm.ft.category <- NA
# anes$blm.ft.category[anes$blm.ft>=50] <- 1
# anes$blm.ft.category[anes$blm.ft<50] <- 0
# 
# unique(anes$blm.ft.category)
# table(anes$blm.ft.category)

#respondent.LC.placement

# anes %>%
#   filter(respondent.LC.placement == 4) %>%
#   nrow() #1818 rows are being removed, converted to NA
# 
# anes$res.LC.category <- NA
# anes$res.LC.category[anes$respondent.LC.placement %in% c(1,2,3)] <- 1
# anes$res.LC.category[anes$respondent.LC.placement %in% c(4)] <- NA
# anes$res.LC.category[anes$respondent.LC.placement %in% c(5,6,7)] <- 0
# 
# unique(anes$res.LC.category)
# table(anes$res.LC.category)

# #respondent.race
# 
# anes$res.race.category <- NA
# anes$res.race.category[anes$respondent.race %in% c(1)] <- 1
# anes$res.race.category[anes$respondent.race %in% c(2,3,4,5,6)] <- 0
# 
# unique(anes$res.race.category)
# table(anes$res.race.category)

# anes.w <- svydesign(ids = ~1,
#                     data=anes,
#                     weights = anes$weight)


#---------------------------- 1

#biden.ft,blm.ft.category
t1 <- svyttest(biden.ft ~ blm.ft.category,anes.w)
t1

#Among conservatives (biden.ft,res.LC.category,blm.ft.category)
anes.w.r1 <- svydesign(ids = ~1,
                      data=anes[anes$conservative==1,],
                      weights = anes$weight[anes$conservative==1])
tc1 <- svyttest(biden.ft ~ blm.ft.category, anes.w.r1)
tc1

#Among liberals (biden.ft,res.LC.category,blm.ft.category)
anes.w.l1 <- svydesign(ids = ~1,
                      data=anes[anes$liberal==1,],
                      weights = anes$weight[anes$liberal==1])
tl1 <- svyttest(biden.ft ~ blm.ft.category, anes.w.l1)
tl1

#---------------------------- 2
#biden.ft,res.race.category
t2 <- svyttest(biden.ft ~ res.race.category, anes.w)
t2

#Among conservatives (biden.ft,res.LC.category,res.race.category)
anes.w.r2 <- svydesign(ids = ~1,
                      data=anes[anes$conservative==1,],
                      weights = anes$weight[anes$conservative==1])
tc2 <- svyttest(biden.ft ~ res.race.category, anes.w.r2)
tc2

#Among liberals (biden.ft,res.LC.category,res.race.category)
anes.w.l2 <- svydesign(ids = ~1,
                      data=anes[anes$liberal==1,],
                      weights = anes$weight[anes$liberal==1])

tl2 <- svyttest(biden.ft ~ res.race.category, anes.w.l2)
tl2

#---------------------------- 3
#biden.ft,blm.ft.category
t3 <- svyttest(biden.ft ~ blm.ft.category, anes.w)
t3

#Among conservatives (biden.ft, res.LC.category&res.race.category, blm.ft.category) - CONSERVATIVE, WHITE
anes.w.r3LW <- svydesign(ids = ~1,
                      data=anes[anes$conservative==1&anes$res.white==1,],
                      weights = anes$weight[anes$conservative==1&anes$res.white==1])
tc3LW <- svyttest(biden.ft ~ blm.ft.category, anes.w.r3LW)
tc3LW

#Among conservatives (biden.ft, res.LC.category&res.race.category, blm.ft.category) - CONSERVATIVE, NON-WHITE
anes.w.l3LW <- svydesign(ids = ~1,
                      data=anes[anes$conservative==1&anes$res.nonwhite==1,],
                      weights = anes$weight[anes$conservative==1&anes$res.nonwhite==1])
tl3LW <- svyttest(biden.ft ~ blm.ft.category, anes.w.l3LW)
tl3LW

#Among liberals (biden.ft, res.LC.category&res.race.category, blm.ft.category) - LIBERAL, WHITE
anes.w.l..3LW <- svydesign(ids = ~1,
                      data=anes[anes$liberal==1&anes$res.white==1,],
                      weights = anes$weight[anes$liberal==1&anes$res.white==1])

tl..3LW <- svyttest(biden.ft ~ blm.ft.category, anes.w.l..3LW)
tl..3LW

#Among liberals (biden.ft, res.LC.category&res.race.category, blm.ft.category) - LIBERAL, NON-WHITE
anes.w.l..3LW <- svydesign(ids = ~1,
                      data=anes[anes$liberal==1&anes$res.nonwhite==1,],
                      weights = anes$weight[anes$liberal==1&anes$res.nonwhite==1])

tl..3LW <- svyttest(biden.ft ~ blm.ft.category, anes.w.l..3LW)
tl..3LW



#regression
summary(lm(biden.ft ~ blm.ft + respondent.LC.placement + respondent.race + urban.unrest.opinion + mexico.border.opinion + us.immigrant.opinion + age,
           data = anes,
           weight = anes$weight))