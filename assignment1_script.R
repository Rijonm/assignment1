
#load Data from DataSet refression_train_loan.csv
loanData <- read.csv("/Users/rijon/Google Drive/02_Master/2.Semester/Data Science/project/Assignment1/regression_train_loan.csv") 


#Selektion der gwünschten Features
loanData2 <- data.frame(loanData$loan_amnt, loanData$term, loanData$emp_length, loanData$home_ownership, 
                        loanData$annual_inc, loanData$verification_status, loanData$purpose, 
                        loanData$dti, loanData$delinq_2yrs, loanData$open_acc, loanData$pub_rec, 
                        loanData$revol_bal, loanData$revol_util, loanData$application_type, loanData$acc_now_delinq, 
                        loanData$tot_coll_amt, loanData$tot_cur_bal, loanData$total_rev_hi_lim, loanData$int_rate)


# Outliers entfernen TBD Mauritius
boxplot(loanData2$loanData.loan_amnt) #OK

boxplot(loanData2$loanData.int_rate)
boxplot(loanData2$loanData.annual_inc)
boxplot(boxplot(dti_outlier))
dti_outlier <- loanData2$loanData.dti[!loanData2$loanData.dti %in% boxplot.stats(loanData2$loanData.dti)$out]
boxplot(dti_outlier)
boxplot(loanData2$loanData.delinq_2yrs)
boxplot(loanData2$loanData.open_acc)
boxplot(loanData2$loanData.pub_rec)
boxplot(loanData2$loanData.revol_bal)
boxplot(loanData2$loanData.revol_util)
boxplot(loanData2$loanData.application_type)
boxplot(loanData2$loanData.dti_joint)
boxplot(loanData2$loanData.acc_now_delinq)
boxplot(loanData2$loanData.tot_coll_amt)
boxplot(loanData2$loanData.tot_cur_bal)

#Fehlende Werte ersetzen
loanData2$loanData.delinq_2yrs <- ifelse(is.na(loanData2$loanData.delinq_2yrs), median(loanData2$loanData.delinq_2yrs, na.rm=TRUE), loanData2$loanData.delinq_2yrs)
loanData2$loanData.open_acc <- ifelse(is.na(loanData2$loanData.open_acc), median(loanData2$loanData.open_acc, na.rm=TRUE), loanData2$loanData.open_acc)
loanData2$loanData.pub_rec <- ifelse(is.na(loanData2$loanData.pub_rec), median(loanData2$loanData.pub_rec, na.rm=TRUE), loanData2$loanData.pub_rec)
loanData2$loanData.revol_util <- ifelse(is.na(loanData2$loanData.revol_util), median(loanData2$loanData.revol_util, na.rm=TRUE), loanData2$loanData.revol_util)
loanData2$loanData.acc_now_delinq <- ifelse(is.na(loanData2$loanData.acc_now_delinq), median(loanData2$loanData.acc_now_delinq, na.rm=TRUE), loanData2$loanData.acc_now_delinq)
loanData2$loanData.tot_coll_amt <- ifelse(is.na(loanData2$loanData.tot_coll_amt), median(loanData2$loanData.tot_coll_amt, na.rm=TRUE), loanData2$loanData.tot_coll_amt)
loanData2$loanData.tot_cur_bal <- ifelse(is.na(loanData2$loanData.tot_cur_bal), median(loanData2$loanData.tot_cur_bal, na.rm=TRUE), loanData2$loanData.tot_cur_bal)
loanData2$loanData.total_rev_hi_lim <- ifelse(is.na(loanData2$loanData.total_rev_hi_lim), median(loanData2$loanData.total_rev_hi_lim, na.rm=TRUE), loanData2$loanData.total_rev_hi_lim)

#Binarizieren der Werte
df_term <- data.frame(model.matrix(~ loanData2$loanData.term-1)) # term binariziert
df_home_ownership <- data.frame(model.matrix(~ loanData2$loanData.home_ownership-1)) # home_ownership binariziert.
df_purpose <- data.frame(model.matrix(~ loanData2$loanData.purpose-1)) # purpose binariziert
df_application_type <- data.frame(model.matrix(~ loanData2$loanData.application_type-1)) # application_type binariziert


#Definitives DataFrame fürs Entwickeln des Modells
loanDataDef <- data.frame(loanData2$loanData.loan_amnt, loanData2$loanData.emp_length, loanData2$loanData.annual_inc, loanData2$loanData.verification_status, 
                        loanData2$loanData.dti, loanData2$loanData.delinq_2yrs, loanData2$loanData.open_acc, 
                        loanData2$loanData.pub_rec, loanData2$loanData.revol_bal, loanData2$loanData.revol_util, loanData2$loanData.acc_now_delinq, 
                        loanData2$loanData.tot_coll_amt, loanData2$loanData.tot_cur_bal, loanData2$loanData.total_rev_hi_lim, 
                        df_term$loanData2.loanData.term.36.months, df_term$loanData2.loanData.term.60.months, 
                        df_purpose$loanData2.loanData.purposecar, df_purpose$loanData2.loanData.purposecredit_card, df_purpose$loanData2.loanData.purposedebt_consolidation, df_purpose$loanData2.loanData.purposeeducational, df_purpose$loanData2.loanData.purposehome_improvement,
                        df_purpose$loanData2.loanData.purposehouse, df_purpose$loanData2.loanData.purposemajor_purchase, df_purpose$loanData2.loanData.purposemedical, df_purpose$loanData2.loanData.purposemoving, df_purpose$loanData2.loanData.purposeother, df_purpose$loanData2.loanData.purposerenewable_energy,
                        df_purpose$loanData2.loanData.purposesmall_business, df_purpose$loanData2.loanData.purposevacation, df_purpose$loanData2.loanData.purposewedding,
                        df_home_ownership$loanData2.loanData.home_ownershipANY, df_home_ownership$loanData2.loanData.home_ownershipMORTGAGE, df_home_ownership$loanData2.loanData.home_ownershipNONE, df_home_ownership$loanData2.loanData.home_ownershipOTHER, 
                        df_home_ownership$loanData2.loanData.home_ownershipOWN, df_home_ownership$loanData2.loanData.home_ownershipRENT,
                        df_application_type$loanData2.loanData.application_typeINDIVIDUAL, df_application_type$loanData2.loanData.application_typeJOINT,
                        loanData2$loanData.int_rate)

loanDataDef2 <- data.frame(loanData2$loanData.loan_amnt, loanData2$loanData.annual_inc, 
                          loanData2$loanData.dti, loanData2$loanData.delinq_2yrs, loanData2$loanData.open_acc, 
                          loanData2$loanData.pub_rec, loanData2$loanData.revol_bal, loanData2$loanData.revol_util, loanData2$loanData.acc_now_delinq, 
                          loanData2$loanData.tot_coll_amt, loanData2$loanData.tot_cur_bal, loanData2$loanData.total_rev_hi_lim, 
                          df_term$loanData2.loanData.term.36.months, df_term$loanData2.loanData.term.60.months, 
                          df_purpose$loanData2.loanData.purposecar, df_purpose$loanData2.loanData.purposecredit_card, df_purpose$loanData2.loanData.purposedebt_consolidation, df_purpose$loanData2.loanData.purposeeducational, df_purpose$loanData2.loanData.purposehome_improvement,
                          df_purpose$loanData2.loanData.purposehouse, df_purpose$loanData2.loanData.purposemajor_purchase, df_purpose$loanData2.loanData.purposemedical, df_purpose$loanData2.loanData.purposemoving, df_purpose$loanData2.loanData.purposeother, df_purpose$loanData2.loanData.purposerenewable_energy,
                          df_purpose$loanData2.loanData.purposesmall_business, df_purpose$loanData2.loanData.purposevacation, df_purpose$loanData2.loanData.purposewedding,
                          df_home_ownership$loanData2.loanData.home_ownershipANY, df_home_ownership$loanData2.loanData.home_ownershipMORTGAGE, df_home_ownership$loanData2.loanData.home_ownershipNONE, df_home_ownership$loanData2.loanData.home_ownershipOTHER, 
                          df_home_ownership$loanData2.loanData.home_ownershipOWN, df_home_ownership$loanData2.loanData.home_ownershipRENT,
                          df_application_type$loanData2.loanData.application_typeINDIVIDUAL, df_application_type$loanData2.loanData.application_typeJOINT,
                          loanData2$loanData.int_rate)




#------Sonstiges
#ToBe removed
verification_status_joint #entfernt
dti_joint #entfernt
#zeigt levels, in diesem Fall " 36 months" " 60 months"
levels(loanData2$loanData.term) 
#Test ob alle NA's entfernt wurden
colSums(is.na(loanData2))



