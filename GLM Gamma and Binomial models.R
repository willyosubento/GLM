#data 1
yes<-c(14,32,11,12)
No<-c(93,81,52,43)
tot<-yes+No
Race<-gl(2,2,length = 4,labels = c("white","Black"))
Drug_use<-gl(2,1,length = 4,labels = c("yes","No"))
first.glm<-glm(yes/tot~Race*Drug_use,family = binomial,weights = tot)
summary(first.glm)
#============================================================================================================================
#Cross tabulation
library(gtsummary)
tbl_binom_model<- first.glm %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  add_glance_table(everything())%>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
tbl_binom_model

#=================================================================================================================================
#data2
conc<-c(5,10,15,20,30,40,60,80,100,5,10,15,20,30,40,60,80,100)
time<-c(118,58,42,35,27,25,21,19,18,69,35,26,21,18,16,13,12,12)
Lot<-factor(c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2),levels = c(1,2))
data_plasma<-data.frame(conc,time,Lot)
fit.gam.inv<-glm(time~Lot*log(conc),data = data_plasma,family = Gamma)
summary(fit.gam.inv)
#=======================================================================================================================================
#Cross tabulation
library(gtsummary)
tbl_Gamma_model<- fit.gam.inv %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  add_glance_table(everything())%>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
tbl_Gamma_model

