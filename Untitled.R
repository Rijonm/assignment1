


loanData2$loanData.delinq_2yrs <- ifelse(is.na(loanData2$loanData.delinq_2yrs), median(loanData2$loanData.delinq_2yrs, na.rm=TRUE), loanData2$loanData.delinq_2yrs)
loanData2$loanData.open_acc <- ifelse(is.na(loanData2$loanData.open_acc), median(loanData2$loanData.open_acc, na.rm=TRUE), loanData2$loanData.open_acc)
loanData2$loanData.pub_rec <- ifelse(is.na(loanData2$loanData.pub_rec), median(loanData2$loanData.pub_rec, na.rm=TRUE), loanData2$loanData.pub_rec)
loanData2$loanData.revol_util <- ifelse(is.na(loanData2$loanData.revol_util), median(loanData2$loanData.revol_util, na.rm=TRUE), loanData2$loanData.revol_util)
loanData2$loanData.acc_now_delinq <- ifelse(is.na(loanData2$loanData.acc_now_delinq), median(loanData2$loanData.acc_now_delinq, na.rm=TRUE), loanData2$loanData.acc_now_delinq)
loanData2$loanData.tot_coll_amt <- ifelse(is.na(loanData2$loanData.tot_coll_amt), median(loanData2$loanData.tot_coll_amt, na.rm=TRUE), loanData2$loanData.tot_coll_amt)
loanData2$loanData.tot_cur_bal <- ifelse(is.na(loanData2$loanData.tot_cur_bal), median(loanData2$loanData.tot_cur_bal, na.rm=TRUE), loanData2$loanData.tot_cur_bal)
loanData2$loanData.total_rev_hi_lim <- ifelse(is.na(loanData2$loanData.total_rev_hi_lim), median(loanData2$loanData.total_rev_hi_lim, na.rm=TRUE), loanData2$loanData.total_rev_hi_lim)