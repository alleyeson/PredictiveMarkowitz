source(file = '/Users/allisonemono/Desktop/R-Sessions/Multivariate_Outlier_Detection.R')

source(file = '/Users/allisonemono/Desktop/R-Sessions/ProdMarkFunctions.R')
source(file = '/Users/allisonemono/Desktop/R-Sessions/PredMarkoFuncwRecursion.R')

####
set.seed(123)

### get data to use
market_index <- c('SPY', 'QQQ')
TT_split_data_obj <- train_test_split_f(num_days = 300, num_stocks = 251,freq = 'daily', train_prop = 0.7, stocks = NULL)
get_market_rep_obj<- train_test_split_f(num_days = 300, num_stocks = 2,freq = 'daily', train_prop = 0.7, stocks = market_index )


### split data into train test 
pre_test_prop <- 0.7
N_train <- nrow(TT_split_data_obj$Train)
pre_test_n <- round(pre_test_prop*N_train)

stock_list_df<- data.frame(StockTickers = colnames(TT_split_data_obj$Train[1:2,-1]))
print("Start Date: ")
TT_split_data_obj$Train[1,1]

print("end date")
TT_split_data_obj$Test[nrow(TT_split_data_obj$Test),1]

data_train_check_m<- TT_split_data_obj$Train[0:pre_test_n,]
#outlier_cleaned_ls<- multivariate_clipper.f(data_input_m = data_train_check_m[,],boot_size = 1000, percentile_cutoff = .95)

data_pre_test_check_m <- TT_split_data_obj$Train[(pre_test_n+1):N_train,]
data_test_check_m <- TT_split_data_obj$Test[,-1]

data_train_market_m <-get_market_rep_obj$Train[0:pre_test_n,]
data_pre_test_market_m <- TT_split_data_obj$Train[(pre_test_n+1):N_train,]
data_train_market_m$Market <- rowMeans(data_train_market_m[,market_index])

data_test_market_m <- get_market_rep_obj$Test[,-1]
data_test_market_m$Market <- rowMeans(data_test_market_m[,market_index])

###

###fit algorithms 

tilda <- 0.0000001#00000000000000000000000000000000
alpha <- 0.0000001
theta <- 0.0#0.001 ## setting that tend to work 
target_returns <- 1
#TT_split_data_obj
head(TT_split_data_obj$Train)
test_num_stocks = 50 


## Fit pred mark recursive 
x_test_ls<- recur_pred_marko_ols_wrt_marko_f(
  R_m = TT_split_data_obj$Train[,-1], alpha = alpha, tilda = tilda, 
  lag_back = 10, lag_forward = 38, target_mu = target_returns, 
  shorts = FALSE, threshold_cap = 0.3,
  is_recursive = T, recurse_stop=3
  )


################
# OLS pred marko 
x_ls<- pred_marko_ols_wrt_marko_f(
  R_m = TT_split_data_obj$Train[,-1], alpha = alpha, tilda = tilda, 
  lag_back = 10, lag_forward = 38, target_mu = target_returns, 
  shorts = FALSE, threshold_cap = 0.3)

## pred markpp summes
pred_mark_meaned_v<- pred_marko_revamped_f(X = TT_split_data_obj$Train,tilda = tilda,alpha = alpha)
pred_mark_summed_v<- pred_marko_revamped_proj_m_sumed_f(X = TT_split_data_obj$Train,
                                                        targ_mu = target_returns,
                                                        tilda = tilda,alpha = alpha)
pred_mark_shock_guard_v<- pred_marko_w_shock_guard_revamp_f(X = TT_split_data_obj$Train, 
                                                            tilda = tilda, alpha = alpha, theta = theta,
                                                            targ_mu = target_returns,shorts = FALSE,threshold_cap = 0.3)

## Base marko and equal weight port
pred_mark_base<- Marko_w_minVar.f(data_m = TT_split_data_obj$Train[,-1],target_returns = target_returns)$weights
d<- dim(TT_split_data_obj$Train[,-1])[2]
d_v <- rep(1/d, d)


## test models 
ret_mark_shock_guard_v <- as.matrix(data_test_check_m)%*%pred_mark_shock_guard_v$weights
ret_mark_summed_v <- as.matrix(data_test_check_m)%*%pred_mark_summed_v$weights

ret_mark_ols_v<- as.matrix(data_test_check_m)%*%x_ls$weights
ret_mark_meaned_v<- as.matrix(data_test_check_m)%*%pred_mark_meaned_v
ols_recusive_test_v <- as.matrix(data_test_check_m)%*%x_test_ls$weights

#Regular marko and equal weight port
ret_mark_base_v <- as.matrix(data_test_check_m)%*%pred_mark_base
equal_port_v <- as.matrix(data_test_check_m)%*%d_v


## Visualise results 
data_check_returns_df <- cbind.data.frame(PM_SGuard = cumsum(ret_mark_shock_guard_v),
                                          PM_Summed =  cumsum(ret_mark_summed_v), 
                                          #PM_Meaned = cumsum(ret_mark_meaned_v),
                                          M_Base = cumsum(ret_mark_base_v),
                                          Equal = cumsum(equal_port_v),
                                          MarkOls = cumsum(ret_mark_ols_v),
                                          RecursiveOls = cumsum(ols_recusive_test_v), 
                                          #Actual = cumsum(rowSums(TT_split_data_obj$TestData[,-1])),
                                          TimePoint = 1:length(ret_mark_shock_guard_v))

cbind.data.frame(PM_SGuard = mean(ret_mark_shock_guard_v),
                 PM_Summed =  mean(ret_mark_summed_v), 
                 M_Base = mean(ret_mark_base_v),
                 Equal = mean(equal_port_v),
                 RecursiveOls = mean(ols_recusive_test_v), 
                 MarkOls = mean(ret_mark_ols_v),
                 #Actual = cumsum(rowSums(TT_split_data_obj$TestData[,-1])),
                 TimePoint = 1#:length(ret_mark_shock_guard_v)
)

data_melted_plot_df <- reshape2::melt(data_check_returns_df,id = 'TimePoint')

ggplotly(
  ggplot(data = data_melted_plot_df, 
         aes(x = TimePoint, y = value, colour = variable))+
    geom_line()+
    scale_y_continuous(labels = percent)+ 
    ggtitle('Cumulative Returns Over Time') + 
    ylab("Cumulative Returns")
)

############################################
### Cumulative returns with varying lag (look back)
####

varying_lag_v <- seq(5,38,1)
get_x_m <- matrix(NA, nrow = d, ncol = length(varying_lag_v))

for(i in 1:length(varying_lag_v)){
  x_mark_ols_ls<- pred_marko_ols_wrt_marko_f(
    R_m = TT_split_data_obj$Train[,-1], alpha = alpha, tilda = tilda, 
    lag_back = varying_lag_v[i], lag_forward = 10, target_mu = target_returns, 
    shorts = FALSE, threshold_cap = 0.5)
  
  get_x_m[,i] <- x_mark_ols_ls$weights
  
}

#str_c(c('lag',1),sep = "_", collapse = "")
lag_v <- c()
for(i in varying_lag_v){
  lag_v <- c(lag_v,str_c(c('lag',i),sep = "_", collapse = ""))
}

ret_per_lag <- as.matrix(data_test_check_m)%*%get_x_m
ret_per_lag <- apply(ret_per_lag, 2, cumsum)
dim(ret_per_lag)

colnames(ret_per_lag)<- lag_v
data_varying_lag_melted_df <- cbind.data.frame(TimePoint = c(1:nrow(data_test_check_m)), ret_per_lag)
data_varying_lag_melted_df <- reshape2::melt(data_varying_lag_melted_df,id = 'TimePoint')


ggplotly(
  ggplot(data = data_varying_lag_melted_df, 
         aes(x = TimePoint, y = value, colour = variable))+
    geom_line()+
    scale_y_continuous(labels = percent)+ 
    ggtitle('Cumulative Returns Over Time By Backward Lag') + 
    ylab("Cumulative Returns")
)


############################################################################
#################
## vary coefs to see how much impact they have 
#seq_v <- seq(0.01,1,0.01)
##

seq_v <- seq(-2,2,0.1)
varied_plot_df<- varied_alpha_theta_plot_f(seq_v, training_data_m = TT_split_data_obj$Train[,-1], test_data_m = data_test_check_m, 
                          alpha = alpha, theta = tilda)

plot_check_df <- varied_plot_df

ggplotly(
  ggplot(data = plot_check_df, 
         aes(x = Seq, y = PeriodReturn, color = VariedVar))+
    geom_point()+
    ggtitle('Varying Var vs Period Total Returns')+
    scale_y_continuous(labels = percent_format())
)


###############################################################
### Cumulative return with varied alpha, theta

ret_per_varied_m_ols_m <- as.matrix(data_test_check_m)%*%get_port_varied_m_ols_m
ret_per_varied_total_m <- apply(ret_per_varied_m_ols_m, 2, cumsum)


colnames(ret_per_varied_total_m)<- varied_alpha_tilda_v
data_varying_alpha_tilda_melted_df <- cbind.data.frame(TimePoint = c(1:nrow(data_test_check_m)), ret_per_varied_total_m)
data_varying_alpha_tilda_melted_df <- reshape2::melt(data_varying_alpha_tilda_melted_df,id = 'TimePoint')


ggplotly(
  ggplot(data = data_varying_alpha_tilda_melted_df, 
         aes(x = TimePoint, y = value, colour = variable))+
    geom_line()+
    scale_y_continuous(labels = percent)+ 
    ggtitle('Cumulative Returns Over Time By Varying Alpha') + 
    ylab("Cumulative Returns")
)


#plot(seq_v,ret_pred_ols_wrt_marko_agg_v) 

ggplot(data = cbind.data.frame(Tilda = seq_v, PeriodReturn = ret_pred_ols_wrt_marko_agg_v), 
        aes(x = Tilda, y = PeriodReturn))+
  geom_point()+
  ggtitle('Varying Tilda vs Period Total Returns')+
  scale_y_continuous(labels = percent_format())



plot(seq_v,ret_check_pred_mark_shock_guard_v) 
plot(seq_v,ret_check_pred_mark_summed_v) 


mark_base<- Marko_w_minVar.f(data_m = TT_split_data_obj$Train[,-1],target_returns = target_returns)$weights
ret_mark_base_v <- as.matrix(data_test_check_m)%*%mark_base
sum(ret_mark_base_v)

mean(ret_mark_base_v)/sd(ret_mark_base_v)

sum(ret_mark_base_v)

##################################################################################################################################################################################################################
#### Test from First Pred Marko Report Ends here ######################################################################
##################################################################################################################################################################################################################



###########################################
## create quad fit signal 

#desired signals 
# market correlation, higher than market returns tendency, high returns 
#data_train_check_m
stock_n_market_data_m <- merge(data_train_check_m, data_train_market_m[c('ref.date', 'Market')], on = 'ref.date')
corr_v<- cor(stock_n_market_data_m[,-1])[,c('Market')]
corr_v <- corr_v[0:(length(corr_v) - 1)]
stock_v <- c(names(corr_v))
avg_returns_v <- colMeans(stock_n_market_data_m[,stock_v])
kurt_v <- kurtosis(stock_n_market_data_m[,stock_v])

corr_m <- cor(data_train_check_m[,stock_v])
cov_m <- cov(data_train_check_m[,stock_v])

pct_above_zero<- colMeans(stock_n_market_data_m[,stock_v]>0 )
pct_above_market <- colMeans(stock_n_market_data_m[,stock_v]> stock_n_market_data_m[,'Market'] )
pct_above_market_below_mean_v <- colMeans(
    stock_n_market_data_m[,stock_v]> stock_n_market_data_m[,'Market'] & 
      stock_n_market_data_m[,stock_v] <=avg_returns_v
    )

pct_above_zero_below_mean_v <- colMeans(
  stock_n_market_data_m[,stock_v]> 0 & 
    stock_n_market_data_m[,stock_v] <=avg_returns_v
)

pct_below_mean_v <- colMeans(
    stock_n_market_data_m[,stock_v] <=avg_returns_v
)

pct_below_zero_v <- colMeans(
  stock_n_market_data_m[,stock_v] <=0
)

cov_m_avg_returns_v<- cov_m%*%avg_returns_v
cor_m_avg_returns_v<- corr_m%*%avg_returns_v

pct_cum_sum_above_zero <- colMeans(cumsum(stock_n_market_data_m[,stock_v]) > 0 )
cov_m <- stock_n_market_data_m[,stock_v]

pre_test_avg_returns_v <- colMeans(data_pre_test_check_m[,stock_v])
row.names(kurt_v)<- NULL

stock_signals_df<- (cbind.data.frame(
  pre_test_avg_returns_v,
  avg_returns_v,
  pct_above_market_below_mean_v,
  pct_above_zero,
  pct_above_market,
  pct_cum_sum_above_zero, 
  corr_v, 
  pct_above_zero_below_mean_v,
  pct_below_mean_v,
  pct_below_zero_v, 
  kurt_v = c(as.numeric(kurt_v)),
  cov_m_avg_returns_v,
  cor_m_avg_returns_v
  ))


cor(stock_signals_df)

rownames(pct_below_zero_v)

## should be looking for stock which do bad now but were once good 
## stock who tend to be lower than their expected returns

## enter into model 



#plot(cummean(stock_n_market_data_m[,c('AVDL')]), type = 'l')



########################################
## now test m times on different stocks everytime 
m <- 100
d_stocks <- 150
collect_average_returns<- matrix(NA, nrow = m, ncol = 5)
collect_total_returns<- matrix(NA, nrow = m, ncol = 5)
collect_sharpe_ratio <- matrix(NA, nrow = m, ncol = 5)

ret_mark_shock_guard_obj<-get_portfolio_summary_f(data_returns=data_test_check_m, portfolio=pred_mark_shock_guard_v$weights, do_col_match_mult = F)
#ret_mark_shock_guard_obj
colnames(collect_average_returns) = colnames(collect_total_returns) =colnames(collect_sharpe_ratio) <- c("TimePoint",
                                                                                                         "PredMarko",
                                                                                                         "PredMarkowSGuard",
                                                                                                         "BaseMarko", 
                                                                                                         "EqualPort")
collect_average_returns[,"TimePoint"] = 
  collect_total_returns[,"TimePoint"] = 
    collect_sharpe_ratio[,"TimePoint"] <- 1:m

for(i in 1:m){
  TT_split_data_obj <- train_test_split_f(num_days = 100, num_stocks = d_stocks,freq = 'daily', train_prop = 0.7, stocks = NULL)
  
  data_train_check_m<- TT_split_data_obj$Train
  data_test_check_m <- TT_split_data_obj$Test[,-1]
  
  pred_mark_meaned_v<- pred_marko_revamped_f(X = TT_split_data_obj$Train,tilda = tilda,alpha = alpha)
  pred_mark_summed_v<- pred_marko_revamped_proj_m_sumed_f(X = TT_split_data_obj$Train,
                                                          targ_mu = target_returns,
                                                          tilda = tilda,alpha = alpha)
  pred_mark_shock_guard_v<- pred_marko_w_shock_guard_revamp_f(X = TT_split_data_obj$Train, 
                                                              tilda = tilda, alpha = alpha, theta = theta,
                                                              targ_mu = target_returns,shorts = FALSE,threshold_cap = 0.3)
  
  pred_mark_base<- Marko_w_minVar.f(data_m = TT_split_data_obj$Train[,-1],target_returns = target_returns)$weights
  #print(cbind(pred_mark_summed_v$weights,pred_mark_shock_guard_v$weights,pred_mark_base))
  
  
  ret_mark_shock_guard_v <- as.matrix(data_test_check_m)%*%pred_mark_shock_guard_v$weights
  ret_mark_summed_v <- as.matrix(data_test_check_m)%*%pred_mark_summed_v$weights
  ret_mark_base_v <- as.matrix(data_test_check_m)%*%pred_mark_base
  d <- length(pred_mark_base)
  eq_port_v <- rep(1/d, d)
  ret_equal_base_v <- as.matrix(data_test_check_m)%*%eq_port_v
  #rownames(ret_equal_base_v)<- rownames(pred_mark_base)
  #length(pred_mark_base)  
  #length(c(rep(1/d, d)))
  
  mu_ret_mark_shock_guard <- mean(ret_mark_shock_guard_v)
  mu_ret_mark_summed <- mean(ret_mark_summed_v)
  mu_ret_mark_base <- mean(ret_mark_base_v)
  mu_ret_equal_base <- mean(ret_equal_base_v)
  
  
  
  ret_mark_shock_guard_obj<-get_portfolio_summary_f(data_returns=data_test_check_m, portfolio=pred_mark_shock_guard_v$weights, do_col_match_mult = F)
  ret_mark_summed_obj<-get_portfolio_summary_f(data_returns=data_test_check_m, portfolio=pred_mark_summed_v$weights, do_col_match_mult = F)
  ret_mark_base_obj<-get_portfolio_summary_f(data_returns=data_test_check_m, portfolio=pred_mark_base, do_col_match_mult = F)
  ret_equal_base_obj<-get_portfolio_summary_f(data_returns=data_test_check_m, portfolio=eq_port_v, do_col_match_mult = F)
  #ret_equal_base_v
  
  #print(ret_mark_shock_guard_obj$Mean)
  
  collect_average_returns[i,c("PredMarko",
                             "PredMarkowSGuard",
                             "BaseMarko", 
                             "EqualPort")]<- c(
                               ret_mark_summed_obj$Mean,ret_mark_shock_guard_obj$Mean,
                               ret_mark_base_obj$Mean,ret_equal_base_obj$Mean
                               )
  
  collect_total_returns[i,c("PredMarko",
                             "PredMarkowSGuard",
                             "BaseMarko", 
                            "EqualPort")]<- c(
                              sum(ret_mark_summed_obj$PortReturns),
                              sum(ret_mark_shock_guard_obj$PortReturns),
                              sum(ret_mark_base_obj$PortReturns), 
                              sum(ret_equal_base_obj$PortReturns)
                                              )
  collect_sharpe_ratio[i,c("PredMarko",
                             "PredMarkowSGuard",
                             "BaseMarko", 
                           "EqualPort")]<- c(ret_mark_summed_obj$Sharpe,ret_mark_shock_guard_obj$Sharpe,
                                             ret_mark_base_obj$Sharpe, 
                                             ret_equal_base_obj$Sharpe
                                             )
  
  
}

colMeans(collect_total_returns)
colMeans(collect_sharpe_ratio)

colMeans(collect_average_returns[,-1], na.rm = T)
colMeans(collect_total_returns[,-1],na.rm = T)
colMeans(collect_sharpe_ratio[,-1],na.rm = T)

get_max_ports_v <- c()

for(i in 1:nrow(collect_sharpe_ratio)){
  if(collect_total_returns[i,'PredMarko'] < 2 & collect_total_returns[i,'PredMarkowSGuard'] < 2){
    port_i_values_v <- collect_average_returns[i,-1]
    max_port <- which.max(port_i_values_v)
    get_max_ports_v <- c(get_max_ports_v,names(collect_average_returns[i,-1])[max_port] ) 
  }
  
  
}

sum(table(get_max_ports_v))

nrow(collect_sharpe_ratio)
data_melt_for_graph_df <- reshape::melt(data = as.data.frame(collect_total_returns), id = 'TimePoint')
head(data_melt_for_graph_df)

ggplotly(
  ggplot(data = data_melt_for_graph_df[data_melt_for_graph_df[,c('value')]<10,],
         aes(x = value, fill = variable))+
    geom_histogram()
)

wilcox.test(collect_average_returns[,'PredMarkowSGuard'] - collect_average_returns[,'BaseMarko'])

data_check_returns_df <- cbind.data.frame(PM_SGuard = cumsum(ret_mark_shock_guard_v),
                                          PM_Summed =  cumsum(ret_mark_summed_v), 
                                          M_Base = cumsum(ret_mark_base_v),
                                          #Actual = cumsum(rowSums(TT_split_data_obj$TestData[,-1])),
                                          TimePoint = 1:length(ret_mark_shock_guard_v))

pdf_data_check_returns_df <- cbind.data.frame(PM_SGuard = (ret_mark_shock_guard_v),
                                              PM_Summed =  (ret_mark_summed_v), 
                                              M_Base = (ret_mark_base_v),
                                              #Actual = cumsum(rowSums(TT_split_data_obj$TestData[,-1])),
                                              TimePoint = 1:length(ret_mark_shock_guard_v))

colnames(pdf_data_check_returns_df) <- c("PM Shock Guarded", "PM Summed", "PM Base", "TimePoint")
sample_index <- sample(x = 1:nrow(pdf_data_check_returns_df), size = 10000, replace = T)
data_booted_df <- pdf_data_check_returns_df[sample_index,c("PM Shock Guarded", "PM Summed", "PM Base")]

data_booted_df <- cbind.data.frame(data_booted_df,TimePoint = 1:nrow(data_booted_df))
head(data_booted_df)
data_melt_for_graph_df <- reshape::melt(data = data_booted_df, id = 'TimePoint')
head(data_melt_for_graph_df)

ggplotly(
  ggplot(data = data_melt_for_graph_df,
         aes(x = value, fill = variable))+
    geom_histogram()
)



ggplot(data = data_melt_for_graph_df,
       aes(x = value, fill = variable))+
  geom_histogram(bins = 50)






