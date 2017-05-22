
#library(impute)
library(e1071)

#Labeled time series files of release 3, 828 training mts and 410 testing mts 
train_mts_dir <- "/home/hamdi/GSU/Spring17/ADM/proj/After_spring/Features_expt/data/train_828_rel3_L12S24/" 
#a directory for training vector data; make before running
train_vec_data_dir <- "/home/hamdi/GSU/Spring17/ADM/proj/After_spring/Features_expt/data/" 
#metadata directory inside training vector data directory; make before running
#train_meta_data_dir <- "/home/hamdi/GSU/Spring17/ADM/proj/data/train_vec_data/meta_data/" 
#directory for 2480 unlabeled test time series
test_mts_dir <- "/home/hamdi/GSU/Spring17/ADM/proj/After_spring/Features_expt/data/test_410_rel3_L12S24/" 
#directory for the generated test vector data
test_vec_data_dir <- "/home/hamdi/GSU/Spring17/ADM/proj/After_spring/Features_expt/data/" 
#all data dir
data_dir <- "/home/hamdi/GSU/Spring17/ADM/proj/After_spring/Features_expt/data/"

setwd(train_mts_dir)

lookback <- c(12)
span <- c(24)

for (l_iter in lookback) {
  for (s_iter in span) {
    filePattern <- paste("*Prior", as.character(l_iter), "Span", as.character(s_iter), "class*", sep="")
    print(filePattern)
    fs <- list.files(pattern = filePattern)
    print(length(fs))
    
    data_mat <- matrix(data = NA, nrow = length(fs), ncol = 112) # 8 * 14 = 112 features
    bin_label_vec <- character(length(fs))
    #mul_label_vec <- character(length(fs))
    
    #setting the time series differential parameters
    if(s_iter == 6) 
      D = 5
    else if(s_iter == 12)
      D = 10
    else
      D = 20
    
    file_iter <- 1
    for (file_name in fs) {
      #sprintf("%d : %s", file_iter, file_name)
      print(file_iter)
      print(file_name)
      if(grepl("no", file_name))
      {
        bin_label_vec[file_iter] <- "N"
        #mul_label_vec[file_iter] <- "No_flare"
      }
      else
      {
        bin_label_vec[file_iter] <- "F"
        # M, X logic here
        #if(grepl("X", file_name))
          #mul_label_vec[file_iter] <- "X"
        #else
          #mul_label_vec[file_iter] <- "M"
        #
      }
      mts <- read.csv(file_name, header = T)
      event_vec <- numeric(0)
      for (i in 2:15) {
        ts <- mts[[i]]
        # ts preprocessing null removal
        ts <- ts[!is.nan(ts)]
        # 1st level z normalize --> killer ; destroys the accuracy; turned off
        #ts <- scale(ts)
        
        val1 <- mean(ts)
        val2 <- sd(ts)
        val3 <- skewness(ts)
        val4 <- kurtosis(ts)
        
        if(length(ts) > D){
        temp_ts <- numeric(length(ts) - D)
        for (j in 1:length(temp_ts)) {
          temp_ts[j] <- ts[j + D] - ts[j]
        }
        
        val5 <- mean(temp_ts)
        val6 <- sd(temp_ts)
        val7 <- skewness(temp_ts)
        val8 <- kurtosis(temp_ts)
        }
        else
        {
          val5 <- mean(ts)
          val6 <- sd(ts)
          val7 <- skewness(ts)
          val8 <- kurtosis(ts)
        }
        
        
        ts_vec <- c(val1, val2, val3, val4, val5, val6, val7, val8)
        event_vec <- c(event_vec, ts_vec)
      }
      data_mat[file_iter, ] <- event_vec
      
      file_iter <- file_iter + 1
    }
    
    
    #####Saving mean and sd of each each column of data matrix in separate metadata file; one file for for one Lookback-span constraint; 
    #purpose: for later z normalization of test data
    #metadata_mat <- matrix(data = NA, nrow = 2, ncol = 112)
    
    #for (mi in 1:112) 
     # {
      #  metadata_mat[1, mi] <- mean(data_mat[ ,mi])
       # metadata_mat[2, mi] <- sd(data_mat[ ,mi])
      #}
    
    #writeMetaFileName <- paste(train_meta_data_dir,"metaData_r123_meanAndSD_Lookback_", as.character(l_iter), "_span_", as.character(s_iter),".csv", sep="")
    
    #write.csv(metadata_mat, file = writeMetaFileName, sep = ",", row.names = FALSE, col.names = FALSE)
    
    
    #########z norm using scale function
    #scaled.data_mat <- scale(data_mat)
    ########
    #data_df <- data.frame(scaled.data_mat)
    #data_df_l <- cbind(data_df, bin_label_vec) #add mul_label_vec if you want 3 class classification

    
    #####Outlier removal #########
    #fl_ind <- data_df_l[[113]] == "F"
    #nfl_ind <- data_df_l[[113]] == "N"
    
    #replace each z value > 3 with NA's
    #for (i in 1:112)  
    #{
     # voi <- data_df_l[[i]]
      #fl_voi <- voi[fl_ind]
      #nfl_voi <- voi[nfl_ind]
      #ind_bad_fl <- abs(fl_voi) > 3 
      #fl_voi[ind_bad_fl] <- NA
      #ind_bad_nfl <- abs(nfl_voi) > 3
      #nfl_voi[ind_bad_nfl] <- NA
      #data_df_l[[i]] <- c(fl_voi, nfl_voi)
    #}
    #df_fl_to_impute <- data_df_l[fl_ind, 1:112]
    #df_nfl_to_impute <- data_df_l[nfl_ind, 1:112]
    #impute the NA values of flare vectors by 10 nearest neighbor method
    #cleaned_fls <- impute.knn(as.matrix(df_fl_to_impute), k = 10, rng.seed=362436069)
    #impute the NA values of non-flare vectors by 10 nearest neighbor method
    #cleaned_nfls <- impute.knn(as.matrix(df_nfl_to_impute), k = 10, rng.seed=362436069)
    
    #combined_df <- data.frame(rbind(cleaned_fls$data, cleaned_nfls$data))
    #combined_df_l <- cbind(combined_df, bin_label_vec) #add mul_label_vec if you want 3 class
    
    data_df <- data.frame(data_mat)
    data_df_l <- cbind(data_df, bin_label_vec)
    
    colnames(data_df_l) <- 
      c("mean_MGT", "sd_MGT", "skew_MGT", "kurt_MGT", "mean_MGTp", "sd_MGTp", "skew_MGTp", "kurt_MGTp",
        "mean_MGH", "sd_MGH", "skew_MGH", "kurt_MGH", "mean_MGHp", "sd_MGHp", "skew_MGHp", "kurt_MGHp",
        "mean_MGV", "sd_MGV", "skew_MGV", "kurt_MGV", "mean_MGVp", "sd_MGVp", "skew_MGVp", "kurt_MGVp",
        "mean_TA", "sd_TA", "skew_TA", "kurt_TA", "mean_TAp", "sd_TAp", "skew_TAp", "kurt_TAp",
        "mean_MIA", "sd_MIA", "skew_MIA", "kurt_MIA", "mean_MIAp", "sd_MIAp", "skew_MIAp", "kurt_MIAp",
        "mean_MPMFE", "sd_MPMFE", "skew_MPMFE", "kurt_MPMFE", "mean_MPMFEp", "sd_MPMFEp", "skew_MPMFEp", "kurt_MPMFEp",
        "mean_MSA", "sd_MSA", "skew_MSA", "kurt_MSA", "mean_MSAp", "sd_MSAp", "skew_MSAp", "kurt_MSAp",
        "mean_FAS", "sd_FAS", "skew_FAS", "kurt_FAS", "mean_FASp", "sd_FASp", "skew_FASp", "kurt_FASp",
        "mean_MVCD", "sd_MVCD", "skew_MVCD", "kurt_MVCD", "mean_MVCDp", "sd_MVCDp", "skew_MVCDp", "kurt_MVCDp",
        "mean_UMF", "sd_UMF", "skew_UMF", "kurt_UMF", "mean_UMFp", "sd_UMFp", "skew_UMFp", "kurt_UMFp",
        "mean_TUCA", "sd_TUCA", "skew_TUCA", "kurt_TUCA", "mean_TUCAp", "sd_TUCAp", "skew_TUCAp", "kurt_TUCAp",
        "mean_TPED", "sd_TPED", "skew_TPED", "kurt_TPED", "mean_TPEDp", "sd_TPEDp", "skew_TPEDp", "kurt_TPEDp",
        "mean_TUCH", "sd_TUCH", "skew_TUCH", "kurt_TUCH", "mean_TUCHp", "sd_TUCHp", "skew_TUCHp", "kurt_TUCHp",
        "mean_AVCH", "sd_AVCH", "skew_AVCH", "kurt_AVCH", "mean_AVCHp", "sd_AVCHp", "skew_AVCHp", "kurt_AVCHp",
        "bin_label")
    
    writeFileName <- paste(train_vec_data_dir,"8_feats_woZ_vect_bin_classes_Lookback_", as.character(l_iter), "_span_", as.character(s_iter),".csv", sep="")
    
    write.csv(data_df_l, file = writeFileName, sep = ",", col.names = colnames(data_df_l), row.names = FALSE)
    
    
  }
  
}



