make_block <- function(data, cols, delays, 
                       lib = c(1, NROW(data)), 
                       diff_col = rep(FALSE, length(cols)))
{   # Takes an input matirx or data frame and creates a block of lag coordinates
    # to use with rEDM.
    # INPUTS:
    #   data - matrix, array, or data.frame with time series variables arranged in columns
    #   cols - vector indices or names of the columns of 'data' to use for each column of block
    #   delays - vector with same length as cols specifying the time displacement for that column
    #   diff_col - vector of logical on whether to apply first differencing to this lag coordinate variable
    #
    # OUTPUT:
    #   block - array with length(cols) of columns, and NROW(data) rows
    
    lib <- matrix(lib,ncol = 2)
    data <- as.matrix(data)
    
    ncol <- length(cols)
    nrow <- dim(data)[1]
    block <- array(NA,dim = c(nrow,ncol))
    colnames(block) <- 1:ncol
    
    for (i in 1:ncol)
    {
        I <- 1:nrow
        I_delay <- intersect(I,I+delays[i])
        block[I_delay-delays[i],i] <- data[I_delay,cols[i]]
        if (delays[i] < 0){
            # remove data points that fall at start of lib segments
            block[lib[,1] - (0:(delays[i]+1)),i] <- NA
            colnames(block)[i] <- paste(colnames(data)[cols[i]],'_t-',abs(delays[i]),sep="")  
        } else if (delays[i] > 0) {
            # remove data points that fall at end of lib segments
            block[lib[,2] - (0:(delays[i]-1)),i] <- NA
            colnames(block)[i] <- paste(colnames(data)[cols[i]],'_t+',abs(delays[i]),sep="")  
        } else {
            colnames(block)[i] <- paste(colnames(data)[cols[i]],'_t',sep="")
        }
        
        if (diff_col[i]){
            block[,i] <- c(NA,diff(block[,i]))
        }
    } # i
    
    
    return(block)
}



simplex_deltas <- function(time_series, lib = c(1, NROW(na.omit(time_series))-max(E)+0), pred = lib,
                           E = 1:10, tau = 1, tp = 1, num_neighbors = "e+1",...){
    #yixugai  lib = c(1, NROW(na.omit(time_series))-max(E)+1)
    #   This function is used to apply univeriate simplex projection where raw data are
    #   used to construct the lag coordinates for the embedding, i.e. x(t), x(t-1), ...
    #   but the variable that is predicted is the first difference, deltax(t + tp) =
    #   x(t + tp) - x(t + tp -1) rather than the value x(t + tp).
    #   
    #   Inputs mimic 'rEDM' function 's_map', '...' arguments passed to block_lnlp
    #   Outputs contains stats evaluated on the first-difference values and
    #   stats evaluated back calculated raw values from the predicted first-differences.
    
    
    #time_series<-data.frame(I=time_series)
    block <- make_block(time_series,rep(1,max(E)+1),c(0,0:(-max(E)+1)),diff_col=c(TRUE,rep(FALSE,max(E))))%>%
        as.data.frame()
    block = na.omit( block )#wo jia de
    L_columns <- lapply(E,function(x) 1+(1:x))
    for(y in 1:(max(E)+1)){
        colnames(block)[y]<-paste0(  "I", y)
    }
    
    out <- list(delta_stats=NULL,delta_preds=NULL,raw_stats=NULL,raw_preds=NULL)
    
    out$delta_stats <- block_lnlp(block, lib = lib, pred = pred,
                                  method = "simplex", tp = 1, num_neighbors = "e+1",
                                  columns =  names(block)[ L_columns[[1]]], target_column = 1, stats_only = FALSE,
                                  first_column_time = FALSE,theta = NULL, save_smap_coefficients = FALSE)$stats
    
    for(ax in 2:max(E)){
        out$delta_stats1 <- block_lnlp(block, lib = lib, pred = pred,
                                       method = "simplex", tp = 1, num_neighbors = "e+1",
                                       columns =  names(block)[ L_columns[[ax]]], target_column = 1, stats_only = FALSE,
                                       first_column_time = FALSE,theta = NULL, save_smap_coefficients = FALSE)$stats
        
        out$delta_stats <-do.call(rbind,
                                  list(out$delta_stats,out$delta_stats1))
    }
    
    
    
    out$delta_preds  <- block_lnlp(block, lib = lib, pred = pred,
                                   method = "simplex", tp = 1, num_neighbors = "e+1",
                                   columns =  names(block)[ L_columns[[1]]], target_column = 1, stats_only = FALSE,
                                   first_column_time = FALSE,theta = NULL, save_smap_coefficients = FALSE)$model_output
    
    for(ax in 2:max(E)){
        out$delta_preds1 <- block_lnlp(block, lib = lib, pred = pred,
                                       method = "simplex", tp = 1, num_neighbors = "e+1",
                                       columns =  names(block)[ L_columns[[ax]]], target_column = 1, stats_only = FALSE,
                                       first_column_time = FALSE,theta = NULL, save_smap_coefficients = FALSE)$model_output
        
        out$delta_preds  <-do.call(rbind,
                                   list(out$delta_preds ,out$delta_preds1))
    }
    
    
    
    #### COMPUTE STATS ON BACK TRANSFORMED
    # model_output is a list
   # out$raw_preds <- lapply(out$delta_preds, function(df) {
    #    df[,c('Observations','Predictions')] <- df[,c('Observations','Predictions')] +  rep(time_series[c(-1:-(max(E)-2))],times =max(E))
     #   return(df)
    #} )
    out$raw_preds <- out$delta_preds[,c("Observations", "Predictions")]+ rep(time_series[c(-1:-(max(E)-1))],times =max(E))
    
    
    out$raw_stats <- out$delta_stats
    
    #temp_stats <- do.call(rbind,lapply(out$raw_preds, function(df) compute_stats(as.vector(na.omit(df$Observations)),as.vector(na.omit(df$Predictions)))))
    aax<-length(out$raw_preds$Observations)/max(E)
    temp_stats <- compute_stats(as.vector(na.omit(out$raw_preds$Observations[1:aax])),as.vector(na.omit(out$raw_preds$Predictions[1:aax])))
    
    for(bbx in 2:max(E)){
        temp_stats1 <- compute_stats(as.vector(na.omit(out$raw_preds$Observations[(bbx*aax+1-aax):(bbx*aax)])),as.vector(na.omit(out$raw_preds$Predictions[(bbx*aax+1-aax):(bbx*aax)])))
        temp_stats <-do.call(rbind,
                                   list(temp_stats ,temp_stats1))  
        
    }
    
    out$raw_stats[6:11] <- temp_stats
    
    temp_stats <- do.call(rbind, lapply(out$raw_preds, function(df) {
        # I <- is.finite(df$pred)
        # compute_stats(df$obs[I-1],df$obs)
        compute_stats(lag(time_series),time_series)
    })
    )
    
    out$raw_stats[12:16] <- temp_stats[,1:5]
    
    # add in tau column to match rEDM::s_map
    out$raw_stats <- out$raw_stats %>%
        mutate(tau = 1) %>%
        select(E,tau,everything())
    out$raw_stats <-out$raw_stats[,-3]
    out$delta_stats <- out$delta_stats %>%
        mutate(tau = 1) %>%
        select(E,tau,everything())
    out$delta_stats <-out$delta_stats[,-3]
    return(out)
}

s_map_deltas <- function(time_series, lib = c(1, NROW(na.omit(time_series))-max(E)+0), pred = lib,
                         theta=c(0, 1e-04, 3e-04, 0.001,
                                 0.003, 0.01, 0.03, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8),
                         E = 1, tau = 1, tp = 1, num_neighbors = 0, ...){
    
    #   This function is used to apply univeriate S-map analysis where raw data are
    #   used to construct the lag coordinates for the embedding, i.e. x(t), x(t-1), ...
    #   but the variable that is predicted is the first difference, deltax(t + tp) =
    #   x(t + tp) - x(t + tp -1) rather than the value x(t + tp).
    #   
    #   Inputs mimic 'rEDM' function 's_map', '...' arguments passed to block_lnlp
    #   Outputs contains stats evaluated on the first-difference values and
    #   stats evaluated back calculated raw values from the predicted first-differences.
    
    
    block <- make_block(time_series,rep(1,max(E)+1),c(0,0:(-max(E)+1)),diff_col=c(TRUE,rep(FALSE,max(E))))%>%
        as.data.frame()
    block = na.omit( block )#wo jia de
    L_columns <- lapply(E,function(x) 1+(1:x))
    for(y in 1:(max(E)+1)){
        colnames(block)[y]<-paste0(  "I", y)
    }
    
   
    out <- list(delta_stats=NULL,delta_preds=NULL,raw_stats=NULL,raw_preds=NULL)
    
    out$delta_stats <- block_lnlp(block, lib = lib, pred = pred,
                                  method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                  theta = theta[1],
                                  columns = names(block)[ L_columns[[1]]], target_column = 1, stats_only = FALSE,
                                  first_column_time = FALSE,save_smap_coefficients = FALSE)$stats 
    if(length(E)!=1){
    for(ax in 2:max(E)){
        out$delta_stats1 <- block_lnlp(block, lib = lib, pred = pred,
                                       method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                       theta = theta[1],
                                       columns = names(block)[ L_columns[[ax]]], target_column = 1, stats_only = FALSE,
                                       first_column_time = FALSE,save_smap_coefficients = FALSE)$stats 
        
        out$delta_stats <-do.call(rbind,
                                  list(out$delta_stats,out$delta_stats1))
    }
    }
    
    
    out$delta_preds <- block_lnlp(block, lib = lib, pred = pred,
                                  method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                  theta = theta[1],
                                  columns = names(block)[ L_columns[[1]]], target_column = 1, stats_only = FALSE,
                                  first_column_time = FALSE,save_smap_coefficients = FALSE)$model_output[[1]] 
    if(length(E)!=1){
    for(ax in 2:max(E)){
        out$delta_preds1 <- block_lnlp(block, lib = lib, pred = pred,
                                       method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                       theta = theta[1],
                                       columns = names(block)[ L_columns[[ax]]], target_column = 1, stats_only = FALSE,
                                       first_column_time = FALSE,save_smap_coefficients = FALSE)$model_output[[1]] 
        
        out$delta_preds <-do.call(rbind,
                                  list(out$delta_preds,out$delta_preds1))
    }
    }
    #out$delta_stats <- do.call(rbind,lapply(out.raw,function(df) cbind(data.frame(E=length(strsplit(df$embedding,',')[[1]]),df$params[2:4],df$stats))))
   # out$delta_stats<-out.raw$stats
    #out$delta_preds <- lapply(out.raw,function(df) df$model_output)
    
    #out$delta_preds <-out.raw$model_output
    #### COMPUTE STATS ON BACK TRANSFORMED
    # model_output is a list
    #out$raw_preds <- lapply(out$delta_preds, function(df) {
     #   df[,c('Observations','Predictions')] <- df[,c('Observations','Predictions')] + rep(time_series[c(-1:-(max(E)-2))],times =max(E))
      #  return(df)
    #} )
    if( max(E)==1){
        out$raw_preds <- out$delta_preds[,c("Observations", "Predictions")]+ rep(time_series,times =max(E))
    }else{out$raw_preds <- out$delta_preds[,c("Observations", "Predictions")]+ rep(time_series[c(-1:-(max(E)-1))],times =max(E))
                }
    
    
    out$raw_stats <- out$delta_stats
    
    #temp_stats <- do.call(rbind,lapply(out$raw_preds, function(df) compute_stats(df$Observations,df$Predictions)))
    #temp_stats <- do.call(rbind,lapply(out$raw_preds, function(df) compute_stats(as.vector(na.omit(out$raw_preds$Observations)),as.vector(na.omit(df$Predictions)))))
    if(length(E)!=1){
    aax<-length(out$raw_preds$Observations)/max(E)
    temp_stats <- compute_stats(as.vector(na.omit(out$raw_preds$Observations[1:aax])),as.vector(na.omit(out$raw_preds$Predictions[1:aax])))
   
    for(bbx in 2:max(E)){
        temp_stats1 <- compute_stats(as.vector(na.omit(out$raw_preds$Observations[(bbx*aax+1-aax):(bbx*aax)])),as.vector(na.omit(out$raw_preds$Predictions[(bbx*aax+1-aax):(bbx*aax)])))
        temp_stats <-do.call(rbind,
                             list(temp_stats ,temp_stats1))  
        
    }
    }else{
        temp_stats <- compute_stats(as.vector(na.omit(out$raw_preds$Observations)),as.vector(na.omit(out$raw_preds$Predictions)))
        
    }
    
    
    if(length(E)!=1){
    max(E)
    aaa<-c(1:max(E))
    ccc<-aaa
    for(bbb in 2:max(E)){
        ccc[bbb]=aaa[bbb]+sum(aaa[1:(bbb-1)])
    }
    
    
    out$raw_stats<-out$raw_stats[ccc,]
    }else{
        out$raw_stats<-out$raw_stats[1,]
    }
    
    
    
    out$raw_stats[6:11] <- temp_stats
    
    temp_stats <- do.call(rbind, lapply(out$raw_preds, function(df) {
        # I <- is.finite(df$pred)
        # compute_stats(df$obs[I-1],df$obs)
        compute_stats(lag(time_series),time_series)
    })
    )
    if(length(E)!=1){
    out$raw_stats[12:16] <- temp_stats[,1:5]
    }else{
        out$raw_stats[12:16] <- temp_stats[,1:5][1,]
    }
    
    #
    #
    #
    
    for( thetax in 2:length(theta)){
        out1 <- list(delta_stats=NULL,delta_preds=NULL,raw_stats=NULL,raw_preds=NULL)
        
        out1$delta_stats <- block_lnlp(block, lib = lib, pred = pred,
                                       method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                       theta = theta[thetax],
                                       columns = names(block)[ L_columns[[1]]], target_column = 1, stats_only = FALSE,
                                       first_column_time = FALSE,save_smap_coefficients = FALSE)$stats 
        if(length(E)!=1){
            for(ax in 2:max(E)){
                out1$delta_stats1 <- block_lnlp(block, lib = lib, pred = pred,
                                                method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                                theta = theta[thetax],
                                                columns = names(block)[ L_columns[[ax]]], target_column = 1, stats_only = FALSE,
                                                first_column_time = FALSE,save_smap_coefficients = FALSE)$stats 
                
                out1$delta_stats <-do.call(rbind,
                                           list(out1$delta_stats,out1$delta_stats1))
            }
        }
        
        
        out1$delta_preds <- block_lnlp(block, lib = lib, pred = pred,
                                       method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                       theta = theta[thetax],
                                       columns = names(block)[ L_columns[[1]]], target_column = 1, stats_only = FALSE,
                                       first_column_time = FALSE,save_smap_coefficients = FALSE)$model_output[[1]] 
        if(length(E)!=1){
            for(ax in 2:max(E)){
                out1$delta_preds1 <- block_lnlp(block, lib = lib, pred = pred,
                                                method = "s-map", tp = 1, num_neighbors = num_neighbors,
                                                theta = theta[thetax],
                                                columns = names(block)[ L_columns[[ax]]], target_column = 1, stats_only = FALSE,
                                                first_column_time = FALSE,save_smap_coefficients = FALSE)$model_output[[1]] 
                
                out1$delta_preds <-do.call(rbind,
                                           list(out1$delta_preds,out1$delta_preds1))
            }
        }
        #out$delta_stats <- do.call(rbind,lapply(out.raw,function(df) cbind(data.frame(E=length(strsplit(df$embedding,',')[[1]]),df$params[2:4],df$stats))))
        # out$delta_stats<-out.raw$stats
        #out$delta_preds <- lapply(out.raw,function(df) df$model_output)
        
        #out$delta_preds <-out.raw$model_output
        #### COMPUTE STATS ON BACK TRANSFORMED
        # model_output is a list
        #out$raw_preds <- lapply(out$delta_preds, function(df) {
        #   df[,c('Observations','Predictions')] <- df[,c('Observations','Predictions')] + rep(time_series[c(-1:-(max(E)-2))],times =max(E))
        #  return(df)
        #} )
        # <- [,c("Observations", "Predictions")]+ rep(time_series[c(-1:-(max(E)-2))],times =max(E))
        if( max(E)==1){
          out1$raw_preds <-out1$delta_preds[,c("Observations", "Predictions")]+ rep(time_series,times =max(E))
        }else{out1$raw_preds <- out1$delta_preds[,c("Observations", "Predictions")]+ rep(time_series[c(-1:-(max(E)-1))],times =max(E))
        }
        
        
        out1$raw_stats <- out1$delta_stats
        
        #temp_stats <- do.call(rbind,lapply(out$raw_preds, function(df) compute_stats(df$Observations,df$Predictions)))
        #temp_stats <- do.call(rbind,lapply(out$raw_preds, function(df) compute_stats(as.vector(na.omit(out$raw_preds$Observations)),as.vector(na.omit(df$Predictions)))))
        if(length(E)!=1){
            aax<-length(out1$raw_preds$Observations)/max(E)
            temp_stats <- compute_stats(as.vector(na.omit(out1$raw_preds$Observations[1:aax])),as.vector(na.omit(out1$raw_preds$Predictions[1:aax])))
            
            for(bbx in 2:max(E)){
                temp_stats1 <- compute_stats(as.vector(na.omit(out1$raw_preds$Observations[(bbx*aax+1-aax):(bbx*aax)])),as.vector(na.omit(out1$raw_preds$Predictions[(bbx*aax+1-aax):(bbx*aax)])))
                temp_stats <-do.call(rbind,
                                     list(temp_stats ,temp_stats1))  
                
            }
        }else{
            temp_stats <- compute_stats(as.vector(na.omit(out1$raw_preds$Observations)),as.vector(na.omit(out1$raw_preds$Predictions)))
            
        }
        
        
        if(length(E)!=1){
            max(E)
            aaa<-c(1:max(E))
            ccc<-aaa
            for(bbb in 2:max(E)){
                ccc[bbb]=aaa[bbb]+sum(aaa[1:(bbb-1)])
            }
            
            
            out1$raw_stats<-out1$raw_stats[ccc,]
        }else{
            out1$raw_stats<-out1$raw_stats[1,]
        }
        
        
        
        out1$raw_stats[6:11] <- temp_stats
        
        temp_stats <- do.call(rbind, lapply(out1$raw_preds, function(df) {
            # I <- is.finite(df$pred)
            # compute_stats(df$obs[I-1],df$obs)
            compute_stats(lag(time_series),time_series)
        })
        )
        if(length(E)!=1){
            out1$raw_stats[12:16] <- temp_stats[,1:5]
        }else{
            out1$raw_stats[12:16] <- temp_stats[,1:5][1,]
        }
        
        
        out$raw_stats<-do.call(rbind,
                               list(out$raw_stats,out1$raw_stats))
        out$delta_stats<-do.call(rbind,
                                 list( out$delta_stats, out1$delta_stats))
        
        
    }
    
    
    #
        # add in tau column to match rEDM::s_map
    out$raw_stats <- out$raw_stats %>%
        mutate(tau = 1) %>%
        select(E,tau,everything())
    out$raw_stats <-out$raw_stats[,-3]
    out$delta_stats <- out$delta_stats %>%
        mutate(tau = 1) %>%
        select(E,tau,everything())
    out$delta_stats <-out$delta_stats[,-3]
    return(out)
}
