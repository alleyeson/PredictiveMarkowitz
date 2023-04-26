
#### Functions 
### Multivariate outlier detection 

#Mahalanobis distance

get_stock_sysmbol_n_sector_info.f <- function(){
  #get all stock symbols in known universe
  library(tidyquant)
  library("quantmod")
  if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
  library(BatchGetSymbols)
  library(scales)
  library(dplyr)
  library(TTR)
  
  #get_all_stocks<- GetSP500Stocks()
  # get_all_stocks <- df.SP500[,]
  #get_all_stocks <- stockSymbols()
  get_all_stocks<- read.csv("nasdaq_screener_1610640639648.csv")
  names_to_change<- names(get_all_stocks)
  names(get_all_stocks)[names_to_change=='Symbol'] <- 'Tickers'
  names(get_all_stocks)[names_to_change=='Name'] <- 'Company'
  names(get_all_stocks)[names_to_change=='Sector'] <- 'GICS.Sector'
  
  return(get_all_stocks)
  
}

stockSymbols<- function(){
  get_all_stocks<- read.csv("nasdaq_screener_1610640639648.csv")
  return(get_all_stocks)
}

train_test_split_f<- function(num_days, num_stocks = 50,freq = 'daily', train_prop = 0.7, stocks = NULL){
  #capital_p= Capital
  get_all_stocks <- stockSymbols()
  names_to_change<- names(get_all_stocks)
  names(get_all_stocks)[names_to_change=='Symbol'] <- 'Tickers'
  names(get_all_stocks)[names_to_change=='Name'] <- 'Company'
  names(get_all_stocks)[names_to_change=='Sector'] <- 'GICS.Sector'
  #get_all_stocks<- get_all_stocks[!is.na(get_all_stocks$MarketCap),]
  # df.SP500 <- GetSP500Stocks()
  
  ## set date 
  start = Sys.Date()-num_days
  end = Sys.Date()
  frequency = freq
  
  ## get random num_stocks stocks (d essentially)
  #print(head(get_all_stocks))
  if(!is.null(stocks)){
    stock_samp3_v<- which(get_all_stocks$Tickers%in% stocks)
    print(get_all_stocks[stock_samp3_v,1])
    }else {
    print(length(get_all_stocks$Tickers))
    stock_samp3_v <- sample(1:length(get_all_stocks$Tickers),num_stocks,replace = F)
  }
  data_outSample3 = BatchGetSymbols(tickers=get_all_stocks[stock_samp3_v,1], first.date=start, last.date=end,
                                    freq.data=frequency, cache.folder=file.path(tempdir(),
                                                                                'BGS_Cache'))
  
  choosen_ticker_v<- unique( data_outSample3$df.tickers[,"ticker"])
  
  data_ret_only <- na.omit(data_outSample3$df.tickers)
  data_ret_only <- data_ret_only[,c("ref.date","ticker","ret.adjusted.prices", "price.open")]
  
  ## now get shaped data 
  ## get return only
  ret_data <- data_ret_only %>%
    reshape2::dcast(
      formula = ref.date ~ ticker,
      fun.aggregate = mean,
      value.var = "ret.adjusted.prices"
    )
  ret_data <- na.omit(ret_data)
  
  ## now get price only 
  price_data <- data_ret_only %>%
    reshape2::dcast(
      formula = ref.date ~ ticker
      , value.var = "price.open"
    )
  price_data <- na.omit(price_data)
  
  t_n <- round(nrow(ret_data)*0.7)
  d <- dim(ret_data)[2]
  
  input_ret_data_m <- ret_data[1:t_n,]
  output_ret_data_m <- ret_data[-c(1:t_n),]
  
  data_in_out_split_l<- list(Train = input_ret_data_m,
                             Test = output_ret_data_m,
                             Price = price_data)
  
  return(data_in_out_split_l)
}

get_Mahalanobis_Distance_per_observation.f<- function(data_input, mu_input_v, sig_input_m){
  
  if(is.Date(data_input[,1]) == TRUE){
    stop("Please remove date column")
  }
  assertthat::are_equal(is.Date(data_input[,1]),F)
  D_v<- NULL  
  
  for(i in 1:nrow(data_input)){
    Diff_v <- (data_input[i,] - mu_input_v)
    D_p <- as.matrix(Diff_v)%*%ginv(sig_input_m)%*%t(Diff_v)
    D_v[i]<- sqrt(D_p)
  }
  
  return(D_v)
}


multivariate_clipper.f<- function(data_input_m,boot_size = 1000, percentile_cutoff = .95){
  data_dum_input_m <- na.omit(data_input_m)
  
  if(is.Date(data_input_m[,1])){
    data_dum_input_m<- data_dum_input_m[,-1]
  }else{data_dum_input_m <- data_dum_input_m}
  
  data_scaled_input_m <- scale(data_dum_input_m)
  
  if(nrow(data_scaled_input_m)>ncol(data_scaled_input_m)){
    boot_index <- sample(x = 1:nrow(data_scaled_input_m), size = boot_size, replace = T)
    data_scaled_input_m <- data_scaled_input_m[boot_index,]
    data_dum_input_m <- data_dum_input_m[boot_index,]
  }
  N<- nrow(data_scaled_input_m)
  head(data_scaled_input_m)
  mu_v <- colMeans(data_scaled_input_m)
  sig_m <- cov((data_scaled_input_m))
  
  # Write cleaning data function 
  print("Codes Reach here")
  collect_distance_alt <- try(mahalanobis(x = data_scaled_input_m, center = mu_v, cov = sig_m))
  if(('try-error'%in% class(collect_distance_alt)) ){
    
    collect_distance_alt <- NULL 
    
  }else{collect_distance_alt <- mahalanobis(x = data_scaled_input_m, center = mu_v, cov = sig_m)}
  print("Codes passed here")
  #alpha <- 1 - percentile_cutoff
  cutoff <- qchisq(p = percentile_cutoff, df = N)
  if(!is.null(collect_distance_alt)){
    index_keep <- collect_distance_alt<=cutoff
  }
  
  index_keep<- rep(TRUE, nrow(data_dum_input_m))
  data_out_clipped_m<- data_dum_input_m[index_keep,]
  
  ## non boot strapped cleaned 
  data_dum_input_m <- na.omit(data_input_m)
  
  if(is.Date(data_input_m[,1])){
    data_dum_input_m<- data_dum_input_m[,-1]
  }else{data_dum_input_m <- data_dum_input_m}
  
  
  
  ## non boot strapped cleaned 
  data_dum_input_m <- na.omit(data_input_m)
  
  if(is.Date(data_input_m[,1])){
    data_dum_input_m<- data_dum_input_m[,-1]
  }else{data_dum_input_m <- data_dum_input_m}
  
  # print("Scaling input for non boot clipped data")
  # head(data_dum_input_m)
  data_non_boot_scaled_input_m <- scale(data_dum_input_m)
  
  # print("Got herere")
  # head(data_non_boot_scaled_input_m)
  mean_v <- colMeans(data_non_boot_scaled_input_m)
  cov_m <- cov(data_non_boot_scaled_input_m)
  
  #collect_non_boot_distance <- mahalanobis(x = data_non_boot_scaled_input_m, center = mean_v, cov = cov_m)
  collect_non_boot_distance <- try(mahalanobis(x = data_non_boot_scaled_input_m, center = mu_v, cov = sig_m))
  if(!('try-error'%in% class(collect_distance_alt)) ){
    
    collect_non_boot_distance <- mahalanobis(x = data_non_boot_scaled_input_m, center = mu_v, cov = sig_m)
    
  }else{collect_non_boot_distance <- NULL}
  
  cutoff <- qchisq(p = percentile_cutoff, df = N)
  if(!is.null(collect_non_boot_distance)){
    index_keep <- collect_non_boot_distance<=cutoff
  }
  
  data_non_boot_cleaned_m <- data_dum_input_m[index_keep,]
  
  ## get parameters as well 
  ## boot strap get parameters 
  boot_index <- sample(x = 1:nrow(data_out_clipped_m), size = boot_size, replace = T)
  mu_boot_v <- colMeans(data_out_clipped_m[boot_index,])
  
  boot_index <- sample(x = 1:nrow(data_out_clipped_m), size = boot_size, replace = T)
  sig_boot_m <- cov(data_out_clipped_m[boot_index,])
  
  return(list(Data_Cleaned = data_out_clipped_m,
              MeanCleaned = mu_boot_v,
              CovMatrixCleaned = sig_boot_m,
              Mahalanobis_dist = collect_distance_alt,
              Date_NonBoot_Cleaned_m = data_non_boot_cleaned_m)
  )
}

####
Marko_w_minVar.f<- function(data_m,target_returns = NULL,shorts = FALSE,threshold_cap = 0.30){
  library(MASS)
  
  Sig_inv_m <- ginv(cov(data_m))
  mu_v <- colMeans(data_m)
  d<- dim(Sig_inv_m)[2]
  ones_v <- rep(1,d)
  
  lam <-as.numeric(t(ones_v)%*%Sig_inv_m%*%ones_v )
  x <- (1/lam)*Sig_inv_m%*%(ones_v)
  
  if(!is.null(target_returns)){
    ones_M_ones_p<- t(ones_v)%*%Sig_inv_m%*%ones_v
    mu_M_mu_p<- mu_v%*%Sig_inv_m%*%mu_v
    ones_M_mu_p<- ones_v%*%Sig_inv_m%*%mu_v
    lagrang_v <- c(1,target_returns)
    lagranian_m<- matrix(NA, nrow = 2,2)
    lagranian_m[1,1]<-  ones_M_ones_p
    lagranian_m[2,2]<-  mu_M_mu_p
    lagranian_m[1,2] = lagranian_m[2,1] <-  ones_M_mu_p
    inv_lagranian_m<- ginv(lagranian_m)
    
    larg_coeffs<- inv_lagranian_m%*%lagrang_v
    x<- Sig_inv_m%*%(larg_coeffs[1]*ones_v + larg_coeffs[2]*mu_v)
  }
  
  if(shorts == FALSE){
    x <- capital_redistribution_lazy_f(x,threshold_cap)
  }
  
  rownames(x) <- names(data_m)
  colnames(x) <- "Portion"
  
  return(list(weights = x))
}


## testing simulteneous soltuion of Least squares and Markowitz 
## abstracting port 
quad_solve_prod.f<- function(Min_m, M_targ_m = NULL, vect_targets_v = NULL,
                             v_attatch_v = NULL,alpha = NULL, tilda = NULL, theta = NULL,
                             shorts = FALSE, threshold_cap = 0.3){
  
  ## defensive coding
  if(is.null(rownames(Min_m))){
    stop("Matrix must be named by row and column")
  }else if( (is.null(rownames(M_targ_m)) & is.null(colnames(M_targ_m))) & !is.null(vect_targets_v) ){
    stop("Target vector must be named. Turn into dX1 matrix and make row names match that of M matrix")
  }
  
  ## get shapes and stuff
  d_shape<- dim(Min_m)
  d<- dim(Min_m)[1]
  x_v <- NULL
  
  ## more defensive coding 
  if(d != dim(M_targ_m)[1]){
    stop("dimension of matrix of vector constaints in M_targ_m need to match that of Min_m")
  }
  
  ## get inverse
  M_inverse_m <- ginv(Min_m)
  
  ##more defensinve coding
  if(!is.null(vect_targets_v)){
    if(is.null(M_targ_m)){
      stop("Please specify target")
    }
  }
  
  if(is.null(M_targ_m) & is.null(vect_targets_v)){
    stop("need at least one target value and vector")
  }
  
  ## solve constraint side of things
  lagr_m <- t(M_targ_m)%*%M_inverse_m%*%(M_targ_m)
  lagr_inverse_m <- ginv(lagr_m)
  larg_conts_v <- lagr_inverse_m%*%vect_targets_v
  
  #vect_m_for_solution_m <- M_targ_m
  ## now get vectors scaled by lagrange multiplier 
  
  vect_scaled_sum_v<- as.matrix(0, nrow = d) 
  if(!is.null(v_attatch_v)){
    vect_scaled_sum_v <- alpha*v_attatch_v
  }
  
  for (larg_conts in 1:length(larg_conts_v)){
    vect_scaled_sum_v<- vect_scaled_sum_v+larg_conts_v[larg_conts]*M_targ_m[,larg_conts]
  }
  x_v <- M_inverse_m%*%(vect_scaled_sum_v)
  
  if(shorts == FALSE){
    x_v <- capital_redistribution_lazy_f(x_v,threshold_cap)
  }
  
  rownames(x_v)<- colnames(Min_m)
  colnames(x_v)<- "Portion"
  
  return(list(weights = x_v))
}

## Solution 1 
pred_marko_revamped_f<- function(X,tilda,alpha,shorts = FALSE,threshold_cap = 0.3){
  dum_data <- X
  if(is.Date(X[,1])){
    dum_data <- X[,-1]
  }
  
  d_shape <-dim(dum_data)
  n <- d_shape[1]
  d <- d_shape[2]
  ones_v <- rep(1,d_shape[2])
  sig_m <- cov(dum_data)
  
  O_m <- as.matrix(dum_data[-nrow(dum_data),])
  y_v <- as.matrix(rowSums(dum_data[-1,]))
  O_mu_v <- colMeans(O_m)
  Oy_m <- O_m *as.numeric(y_v)
  Oy_mu_v <- colMeans(Oy_m)
  
  projection_m <- (O_mu_v)%*%t(O_mu_v) ## alternatively, may need to do outer prod before aggregation 
  
  projection_m<- projection_m/max(projection_m)
  #proj_guard_m<- proj_guard_m/max(proj_guard_m)
  sig_m <- sig_m/max(sig_m)
  
  mid_m <- ginv((1/n)*tilda*sig_m + alpha*projection_m)
  
  lambda_denum <-  ones_v%*%mid_m%*%ones_v
  lamdba_numer <- 1 - alpha* as.numeric(ones_v%*%mid_m%*%Oy_mu_v)
  
  lamdba_p <- lamdba_numer/lambda_denum
  
  x_v <- mid_m%*% ( alpha*Oy_mu_v + lamdba_p*ones_v)
  
  if(shorts == FALSE){
    x_v <- capital_redistribution_lazy_f(x_v,threshold_cap)
  }
  
  
  return(x_v)
  
}

pred_marko_revamped_proj_m_sumed_f<- function(X,tilda,alpha,targ_mu = NULL,shorts = FALSE,threshold_cap = 0.3){
  dum_data <- X
  if(is.Date(X[,1])){
    dum_data <- X[,-1]
  }
  
  d_shape <-dim(dum_data)
  n <- d_shape[1]
  d <- d_shape[2]
  
  
  ones_v <- rep(1,d_shape[2])
  sig_m <- cov(dum_data)
  mu_v <- colMeans(dum_data)
  
  O_m <- as.matrix(dum_data[-nrow(dum_data),])
  y_v <- as.matrix(rowSums(dum_data[-1,]))
  O_mu_v <- colMeans(O_m)
  Oy_m <- O_m *as.numeric(y_v)
  Oy_mu_v <- colMeans(Oy_m)
  
  projection_m <-  (O_m[1,])%*%t(O_m[1,]) ## alternatively, may need to do outer prod before aggregation 
  for(i in 2:nrow(O_m)){
    projection_m <- projection_m+(O_m[i,])%*%t(O_m[i,])
  }
  projection_m<- (1/n)*projection_m
  
  projection_m<- projection_m/max(projection_m)
  #proj_guard_m<- proj_guard_m/max(proj_guard_m)
  sig_m <- sig_m/max(sig_m)
  
  mid_m <- ginv((1/n)*tilda*sig_m + alpha*projection_m)
  
  lambda_denum <-  ones_v%*%mid_m%*%ones_v
  lamdba_numer <- 1 - alpha* as.numeric(ones_v%*%mid_m%*%Oy_mu_v)
  
  lamdba_p <- lamdba_numer/lambda_denum
  
  x_v <- mid_m%*% ( alpha*Oy_mu_v + lamdba_p*ones_v)
  
  if(shorts == FALSE){
    x_v <- capital_redistribution_lazy_f(x_v,threshold_cap)
  }
  
  if(!is.null(targ_mu)){
    rownames(mid_m) = colnames(mid_m) <- rownames(sig_m)
    constrain_vectors_m <- cbind.data.frame(ones_v,mu_v)
    constrain_vectors_m <- as.matrix(constrain_vectors_m)
    vect_targs_v <- c(1,targ_mu)
    x_v<- quad_solve_prod.f(Min_m = mid_m, M_targ_m = constrain_vectors_m, vect_targets_v = vect_targs_v,
                            v_attatch_v = Oy_mu_v,alpha = alpha, tilda = tilda, theta = NULL,
                            shorts = shorts, threshold_cap = threshold_cap)
  }
  
  
  return(x_v)
  
}

pred_marko_w_shock_guard_revamp_f <- function(X, tilda, alpha, theta,targ_mu = NULL,shorts = FALSE,threshold_cap = 0.3){
  dum_data <- X
  if(is.Date(X[,1])){
    dum_data <- X[,-1]
  }
  
  d_shape <-dim(dum_data)
  n <- d_shape[1]
  d <- d_shape[2]
  
  ones_v <- rep(1,d)
  sig_m <- cov(dum_data)
  
  mu_v <- colMeans(dum_data)
  
  O_m <- as.matrix(dum_data[-nrow(dum_data),])
  y_v <- as.matrix(rowSums(dum_data[-1,]))
  O_mu_v <- colMeans(O_m)
  Oy_m <- O_m *as.numeric(y_v)
  Oy_mu_v <- colMeans(Oy_m)
  
  Market_m<- rowSums(O_m)
  marktek_mu_tup_v <- mean(Market_m) * colMeans(O_m)
  Market_tup_m <- Market_m*O_m 
  Market_tup_sum_m <- colSums(Market_tup_m) - marktek_mu_tup_v
  Market_tup_targ_sum_v <- colMeans(Market_tup_m) - marktek_mu_tup_v
  
  projection_m <-  (O_m[1,])%*%t(O_m[1,]) ## alternatively, may need to do outer prod before aggregation 
  proj_guard_m <- 2*(1/(n**2))*(Market_tup_sum_m)%*%t(Market_tup_sum_m) #(Market_tup_m[1,])%*%t(Market_tup_m[1,])
  
  for(i in 2:nrow(O_m)){
    projection_m <- projection_m+(O_m[i,])%*%t(O_m[i,])
    #proj_guard_m
  }
  projection_m<- (1/n)*projection_m
  
  #scale matrices
  projection_m<- projection_m/max(projection_m)
  proj_guard_m<- proj_guard_m/max(proj_guard_m)
  sig_m <- sig_m/max(sig_m)
  
  #print(sig_m)
  mid_m <- ginv((1/n)*tilda*theta*sig_m + alpha*theta*projection_m + alpha*tilda*proj_guard_m) ## may need to scale these (all values lie between -1 to 1)
  
  lambda_denum <-  ones_v%*%mid_m%*%ones_v
  lamdba_numer <- 1 - alpha* as.numeric(ones_v%*%mid_m%*%Oy_mu_v)
  
  lamdba_p <- lamdba_numer/lambda_denum
  
  x_v <- mid_m%*% ( alpha*Oy_mu_v + lamdba_p*ones_v)
  
  if(shorts == FALSE){
    x_v <- capital_redistribution_lazy_f(x_v,threshold_cap)
  }
  
  if(!is.null(targ_mu)){
    rownames(mid_m) = colnames(mid_m) <- rownames(sig_m)
    constrain_vectors_m <- cbind.data.frame(ones_v,mu_v,Market_tup_targ_sum_v)
    constrain_vectors_m <- as.matrix(constrain_vectors_m)
    vect_targs_v <- c(1,targ_mu,0) # target_market Cov set to zero (could lead to zero vector)
    x_v<- quad_solve_prod.f(Min_m = mid_m, M_targ_m = constrain_vectors_m, vect_targets_v = vect_targs_v,
                            v_attatch_v = Oy_mu_v,alpha = alpha, tilda = tilda, theta = NULL,
                            shorts = shorts, threshold_cap = threshold_cap)
  }
  
  return(x_v)
  
}

pred_marko_ols_wrt_marko_helper_f<- function(
    R_now_m, R_t, M_i, mu_v, 
    x_t_v = NULL, mu_t_v, alpha, tilda, 
    target_mu, shorts = FALSE, threshold_cap = 0.3){
  alpha <- alpha**2
  tilda <- tilda**2
  
  
  Psi_m <- ginv(tilda*M_i + M_i%*%M_i*alpha)
  Psi_m <- as.matrix(Psi_m)
  d <- dim(Psi_m)[1]
  
  if(is.null(x_t_v)){
    x_t_v<- as.matrix(rep(1/d,d))
  }
  
  
  ## get lamdba and zeta 
  ones_v <- as.matrix(rep(1,d))
  mu_t_v <- as.matrix(mu_t_v)
  m11 <- t(ones_v)%*%Psi_m%*%ones_v
  m12 <- t(ones_v) %*%Psi_m%*%mu_t_v
  m22 <- t(mu_t_v) %*%Psi_m%*%mu_t_v
  
  
  Psi_M_R_v <- alpha*Psi_m%*%M_i%*%x_t_v
  right_handside_v <- c(1 - t(ones_v)%*%Psi_M_R_v, target_mu - t(mu_t_v)%*%Psi_M_R_v)
  
  larg_m <- matrix(NA, nrow= 2, ncol = 2)
  larg_m[1,2] = larg_m[2,1] = m12
  larg_m[1,1] = m11
  larg_m[2,2] = m22
  
  lambda_zeta_v <- ginv(larg_m)%*%right_handside_v
  
  x_right_v <- Psi_M_R_v + lambda_zeta_v[1]*ones_v + lambda_zeta_v[2]*mu_t_v
  
  x_v <- Psi_m%*%x_right_v 
  if(shorts == FALSE){
    x_v <- capital_redistribution_lazy_f(x_v,threshold_cap)
  }
  
  return(list(weights = x_v))
  
}

pred_marko_ols_wrt_marko_f <- function(
    R_m, alpha, tilda, 
    lag_back, lag_forward, target_mu, 
    shorts = FALSE, threshold_cap = 0.3,
    is_recursive = FALSE, recurse_stop = 3){
  
  R_m <- as.matrix(R_m)
  N <- nrow(R_m)
  port_m <- matrix(data = NA, nrow = dim(R_m)[2], ncol = length((lag_back-1):(N - (lag_forward)) )) 
  i = (lag_back+1)
  
  for(i in (lag_back+1):(N - (lag_forward+1)) ){
    ##get indices 
    start <- i - lag_back
    end <- i+(lag_forward-1)
    
    ####
    print("shape of input ")
    print(dim(R_m))
    
    print("current end ")
    print(end)
    
    print("current start ")
    print(start)
    
    print("current i ")
    print(i)
    
    ## get return matrices 
    R_now_m <- R_m[start:i,]
    
    R_t <- R_m[i:end, ]
    #R_t <- R_t[!is.na(R_t[,1]),]
    
    ## return vectors 
    mu_t_v <- colMeans(R_t)
    mu_v <- colMeans(R_now_m)
    
    #
    M_i <- cov(R_now_m)
    M_i <- M_i/max(M_i)
    
    R_now_m<- R_now_m/max(R_now_m)
    R_t <- R_t/max(R_t)
    
    if(is_recursive){
      #if(end>N ){end <- N}
      #if(start>end | i-1 >= end ){return(NULL)}
      
      print("shape of input ")
      print(dim(R_m))
      
      print("current end ")
      print(end)
      
      print("current start ")
      print(start)
      
      print("current i ")
      print(i)
      
      if(
        recurse_stop ==0 | 
        end>=N | 
        start>=end | 
        i-1 >= end |
        end - start <= 3
        ){
        
        x_t_v <- Marko_w_minVar.f(
          data_m = R_t,
          target_returns = target_mu,
          shorts = shorts,
          threshold_cap = threshold_cap)$weights
        
        return(x_t_v)
      }
      
      recurse_stop <- recurse_stop-1
      x_t_v <- pred_marko_ols_wrt_marko_f(
        R_t, alpha, tilda, 
        lag_back-1, lag_forward-1, target_mu, 
        shorts = FALSE, threshold_cap = 0.3,
        is_recursive = TRUE, recurse_stop = recurse_stop
      )$weights
      
    }else{
      x_t_v <- Marko_w_minVar.f(data_m = R_t,target_returns = target_mu,shorts = shorts,threshold_cap = threshold_cap)$weights
    } 
    
    x_i_v <- pred_marko_ols_wrt_marko_helper_f(
      R_now_m = R_now_m, R_t = R_t, M_i = M_i, 
      mu_v = mu_v, x_t_v = x_t_v, mu_t_v = mu_t_v, 
      alpha = alpha, tilda = tilda, target_mu = target_mu, 
      shorts = shorts, threshold_cap = threshold_cap
    )
    
    port_m[,i - lag_back ]<- x_i_v$weights
  }
  
  port_m<- port_m[,which(colMeans(is.na(port_m)) <= 0)]
  
  return(list(weights = rowMeans(port_m), ports = port_m))
  
}



### varying alpha, theta and both 
varied_alpha_theta_helper_f <- function(seq_v, training_data_m, test_data_m, other_var,
                                  varied_var = 'Alpha', lag_forward = 38, lag_back = 5, 
                                  target_mu = 1, shorts = FALSE, threshold_cap = 0.3
                                  ){
  
  d <- dim(training_data_m[,])[2]
  get_port_varied_m_ols_m <- matrix(NA, nrow = d, ncol = length(seq_v))
  
  for(i in 1:length(seq_v)){
    
    if (seq_v[i]==0){
      print(i)
      next
    }
    
    if(varied_var == 'Alpha'){
      
      pred_ols_wrt_marko_ls<- pred_marko_ols_wrt_marko_f(
        R_m = training_data_m[,], alpha = seq_v[i], tilda = other_var, 
        lag_back = lag_back, lag_forward = lag_forward, target_mu = target_mu, 
        shorts = shorts, threshold_cap = threshold_cap)
      get_port_varied_m_ols_m[,i] <- pred_ols_wrt_marko_ls$weights
    }
    else if(varied_var == 'Theta'){
      
      pred_ols_wrt_marko_ls<- pred_marko_ols_wrt_marko_f(
        R_m = training_data_m[,], alpha = other_var, tilda = seq_v[i], 
        lag_back = lag_back, lag_forward = lag_forward, target_mu = target_mu, 
        shorts = shorts, threshold_cap = threshold_cap)
      get_port_varied_m_ols_m[,i] <- pred_ols_wrt_marko_ls$weights
      
    }
    else{
      pred_ols_wrt_marko_ls<- pred_marko_ols_wrt_marko_f(
        R_m = training_data_m[,], alpha = seq_v[i], tilda = seq_v[i], 
        lag_back = lag_back, lag_forward = lag_forward, target_mu = target_mu, 
        shorts = shorts, threshold_cap = threshold_cap)
      get_port_varied_m_ols_m[,i] <- pred_ols_wrt_marko_ls$weights
    }
    
  } 
  
  period_return_v <- as.matrix(test_data_m[,]) %*% get_port_varied_m_ols_m
  period_return_v<- apply(period_return_v, 2, sum)
  helper_plot_df <- cbind.data.frame(VariedVar = rep(varied_var, length(seq_v)),
                                     Seq = seq_v,
                                     PeriodReturn = period_return_v)
  
  return(helper_plot_df)
  
}


varied_alpha_theta_plot_f <- function(seq_v, training_data_m, test_data_m, alpha, theta,
                                      lag_forward = 38, lag_back = 5, 
                                      target_mu = 1, shorts = FALSE, threshold_cap = 0.3
                                      ){
  varied_var_v <- c('Alpha', 'Theta', 'Both')
  list_df <- list()
  alpha_varied_df<- varied_alpha_theta_helper_f(seq_v, training_data_m, test_data_m, other_var = theta,
                              varied_var = varied_var_v[1], lag_forward = 38, lag_back = 5, 
                              target_mu = 1, shorts = FALSE, threshold_cap = 0.3
                              )
  list_df[[1]] <- alpha_varied_df
  
  for(j in 2:length(varied_var_v)){
    list_df[[j]] <- varied_alpha_theta_helper_f(seq_v, training_data_m, test_data_m, other_var = alpha,
                                                varied_var = varied_var_v[j], lag_forward = 38, lag_back = 5, 
                                                target_mu = 1, shorts = FALSE, threshold_cap = 0.3
                                                )
  }


plot_check_df <- alpha_varied_df

for(i in 2:length(varied_var_v)){
  plot_check_df <- rbind.data.frame(plot_check_df,list_df[[i]])
}

ggplotly(
  ggplot(data = plot_check_df, 
         aes(x = Seq, y = PeriodReturn, color = VariedVar))+
    geom_point()+
    ggtitle('Varying Var vs Period Total Returns')+
    scale_y_continuous(labels = percent_format())
)

return(plot_check_df)

}
