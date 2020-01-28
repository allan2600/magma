library(RMySQL) 
library(caret)
library(philentropy)
library(doMC)
library(dplyr)

registerDoMC(cores = 4)



######## FUNCTIONS #########

# ////////////// DO NOT USE /////////////////////////
add_similarity_measures <- function(data_frame) {
  
  # Get similarity measures individually for location and cuisine and both
  df_all$loc_similar <- 0
  df_all$food_similar <- 0
  df_all$food_loc_similar <- 0
  
  for (row_index in 1:nrow(data_frame)) {
    print(row_index)
    biz_place_vector <- as.numeric(data_frame[row_index, 9:75])
    usr_place_vector <- as.numeric(data_frame[row_index, 180:246])
    
    biz_cuisine_vector <- as.numeric(data_frame[row_index, 76:125])
    usr_cuisine_vector <- as.numeric(data_frame[row_index, 130:179])
    
    
    location_similarity <- distance(
      rbind(biz_place_vector, usr_place_vector),
      method="cosine"
    )
    
    food_similarity <- distance(
      rbind(biz_cuisine_vector, usr_cuisine_vector),
      method="cosine"
    )
      
    food_location_similarity <- distance(
      rbind(
        c(biz_place_vector, biz_cuisine_vector),
        c(usr_place_vector, usr_cuisine_vector)
      ), 
      method="cosine"
    )
    
    data_frame$loc_similar[row_index] <- 
      ifelse(is.na(location_similarity), 0, location_similarity)
    
    data_frame$food_similar[row_index] <- 
      ifelse(is.na(food_similarity), 0, food_similarity)
    
    data_frame$food_loc_similar[row_index] <- 
      ifelse(is.na(food_location_similarity), 0, food_location_similarity)
  }
  
  return(data_frame)
}
# ///////////////// END OF DO NOT USE /////////////////////


track_clusters <- function(data_frame, MAX_CLUSTERS) {
  
  df_prior_cluster_link <- data.frame()
  
  for (df_row_index in 2:nrow(data_frame)) {
    # 4-14, 15-25, 26-36, 37-47, 48-58, 
    # 59-69, 70-80, 81-91, 92-102, 103-113
    # 114-124, 125-135, 136-146, 147-157, 158-168
    # Prepare cluster data 
    list_prior_cluster <- list()
    list_this_cluster <- list()
    
    
    for (cluster_index in 0:(MAX_CLUSTERS - 1)) {
      shift <- 11 * cluster_index
      start <- 4+shift
      end <- 14+shift
      
      #print(paste(cluster_index,start,end, sep=','))
      prior_cluster <- data_frame[df_row_index - 1, start:end]
      this_cluster <- data_frame[df_row_index, start:end]
      names(prior_cluster) <- c(
        'sentiment', 'pct_exclam', 'log10_usr_review_count', 'pct_quest', 
        'root11_review_size', 'pct_periods', 'pct_apost', 'useful_review_count', 
        'log10_biz_review_count', 'root11_item_count', 'good_review'
      )
      names(this_cluster) <- c(
        'sentiment', 'pct_exclam', 'log10_usr_review_count', 'pct_quest', 
        'root11_review_size', 'pct_periods', 'pct_apost', 'useful_review_count', 
        'log10_biz_review_count', 'root11_item_count', 'good_review'
      )

      # EDIT: Most numbers are near 0, so apply a transform to get the numbers
      # close to 0 without global normalization
      # Transform will apply to item_count and review_size
      # TRANSFORM: (value)^(1/11) - 1
      prior_cluster$root11_item_count <- prior_cluster$root11_item_count ^ (1/11) - 1
      this_cluster$root11_item_count <- this_cluster$root11_item_count ^ (1/11) - 1
      
      prior_cluster$root11_review_size <- prior_cluster$root11_review_size ^ (1/11) - 1
      this_cluster$root11_review_size <- this_cluster$root11_review_size ^ (1/11) - 1
      
      # Plus 1 is for avoiding 0 indexing; these are parallel arrays
      list_prior_cluster[[as.character(cluster_index+1)]] <- prior_cluster
      list_this_cluster[[as.character(cluster_index+1)]] <- this_cluster
    }
    
    
    # Now calculate the entire comparison
    mat_cluster_similarity <- matrix(0, nrow=MAX_CLUSTERS, ncol=MAX_CLUSTERS)
    
    for (prior_cluster_index in 1:MAX_CLUSTERS) {
      for (current_cluster_index in 1:MAX_CLUSTERS) {
        mat_cluster_similarity[prior_cluster_index, current_cluster_index] <-
          distance(
            rbind(
              list_prior_cluster[[as.character(prior_cluster_index)]], 
              list_this_cluster[[as.character(current_cluster_index)]]
            ),
            method="cosine"
          )
      }
    }
    
    # Method for finding max similarity across entire dataset, then 0 out the 
    # current cluster (column) so that it can not be reused; make sure to record
    # in the current cluster the value of the prior cluster (row) to which it is most
    # similar. It is not an efficient method, but it will work for now.

    mat_cluster_similarity_copy <- mat_cluster_similarity
    
    for (cluster_entry_index in 1:MAX_CLUSTERS) {
      matrix_index <- which.max(mat_cluster_similarity_copy)
      
      # With the use of 1-indexing, the zero case for determining mod has to 
      # be handled differently. There does not seem to be a clean equation to use
      # to avoid the special "zero" case.
      if (matrix_index %% MAX_CLUSTERS == 0) {
        row_index <- MAX_CLUSTERS
        column_index <- matrix_index %/% MAX_CLUSTERS
      }
      else {
        row_index <- matrix_index %% MAX_CLUSTERS
        column_index <- (matrix_index %/% MAX_CLUSTERS) + 1
      }
      
      similarity <- mat_cluster_similarity_copy[row_index, column_index]
      print(paste(
        df_row_index,
        cluster_entry_index, 
        matrix_index, 
        row_index, 
        column_index, 
        similarity, 
        sep=','
      ))
      
      df_prior_cluster_link <- rbind(df_prior_cluster_link, data.frame(
        "time_iter"=df_row_index,
        "cluster_entry_num"=cluster_entry_index,
        "prior_cluster"=row_index,
        "current_cluster"=column_index,
        "similarity"=similarity
      ))
      mat_cluster_similarity_copy[, column_index] <- 0
      mat_cluster_similarity_copy[row_index,] <- 0
      #print(mat_cluster_similarity_copy)
    }
  }
  
  mat_cluster_similarity_copy
  
  return (df_prior_cluster_link)
}




###### START HERE ###########

############## ACQUIRE THE DATA ###############
set.seed(2012) #.... USE THIS
#set.seed(60577).... SAVE FOR LATER
#setwd('/Volumes/USB_UBUNTU/courses/mds576-research-methods/code')
setwd('/Users/allan/Desktop')
df_all <- read.csv('mds576_merged-clusters-final_20181208.csv')

########## ADJUST VARIABLES IN DATASET ###################

# Set the "good review" cluster average to 0.70 (70% of reviews in cluster are 4+ stars)
#good_review_threshold <- 0.85
MAX_CLUSTERS <- 15
#cluster_count_threshold <- 3
#good_review_clusters <- 0
#threshold <- 0.5


# SET UP TRACKED CLUSTERS
df_cluster_tracking <- track_clusters(df_all, MAX_CLUSTERS)

# List of data frames that hold each individual cluster data across all time iterations
list_cluster_df <- list() 

# Used to track cluster numbers across time iterations based on prior similarity of vales
# This needs to be set up properly with the prior cluster for the first iteration;
# otherwise, everything is slightly off
list_cluster_link <- list()  


# Get the first time iteration across each of the 15 (MAX_CLUSTER) clusters 
for (cluster_index in 0:(MAX_CLUSTERS-1)) {
  shift <- 11 * cluster_index
  start <- 4+shift
  end <- 14+shift
  
  # First iteration will need these to be identical
  list_cluster_link[[as.character(cluster_index + 1)]] <- 
    as.character(cluster_index + 1)
  
  df_cluster_row <- df_all[1, start:end]
  names(df_cluster_row) <- c(
    'sentiment', 'pct_exclam', 'log10_usr_review_count', 'pct_quest', 
    'review_size', 'pct_periods', 'pct_apost', 'useful_review_count', 
    'log10_biz_review_count', 'item_count', 'good_review'
  )
  df_cluster_row$time_iter <- df_all[1,]$time_iter
  df_cluster_row$current_cluster <- cluster_index + 1
  df_cluster_row$prior_cluster <- -1
  df_cluster_row$cluster_similarity <- 0
  
  
  list_cluster_df[[as.character(cluster_index+1)]] <- 
    rbind(list_cluster_df[[as.character(cluster_index+1)]], df_cluster_row)
}


# PARTITION THE DATA TO EACH CLUSTER DATA FRAME BUT ADD ROWS BASED ON
# TRACKED CLUSTER SIMILARITY

# Used to go through the rows in df_cluster_tracking as time_iter increases
cluster_tracking_row_index <- 1

# The variable data_row_index is used to manage the row number (time iteration)
# within each cluster and in the original dataset;
# remember: each time iteration cosists of a block of 15 (MAX_CLUSTER) rows
for (data_row_index in 2:nrow(df_all)) {
  time_iter <- df_all[data_row_index,]$time_iter
  
  # To avoid problems with old copies, make a fresh copy of the new
  # cluster index linking
  list_clusters_update <- list_cluster_link
  

  # The cluster_number is just a simple index to help iterate through all the clusters 
  for (cluster_number in 0:(MAX_CLUSTERS-1)) {
    # As a convenience to avoid reusing the row_index, just get the current 
    # row of the df_cluster_tracking data frame
    df_cluster_tracking_row <- df_cluster_tracking[cluster_tracking_row_index,]
    
    # Keep the cluster number values as characters since this seems better suited
    # to moving in the dictionary (list) data structure; list data structure
    # uses keys and values as character, so make sure to convert
    prior_cluster_number <- as.character(df_cluster_tracking_row$prior_cluster)
    current_cluster_number <- as.character(df_cluster_tracking_row$current_cluster)
    cluster_similarity <- df_cluster_tracking_row$similarity

    # Stored current cluster in prior iteration, now need prior to trace back;
    # remember: the value is the permanent originally linked cluster that should
    # NEVER change
        source_cluster_number <- as.numeric(list_cluster_link[[prior_cluster_number]])
    
    
    # Remember, everything is 1-index
    shift <- 11 * (as.numeric(current_cluster_number) - 1)
    start <- 4+shift
    end <- 14+shift

    df_cluster_row <- df_all[data_row_index, start:end]
    names(df_cluster_row) <- c(
      'sentiment', 'pct_exclam', 'log10_usr_review_count', 'pct_quest', 
      'review_size', 'pct_periods', 'pct_apost', 'useful_review_count', 
      'log10_biz_review_count', 'item_count', 'good_review'
    )
    df_cluster_row$time_iter <- time_iter
    df_cluster_row$cluster_similarity <- cluster_similarity
    df_cluster_row$current_cluster <- as.numeric(current_cluster_number)
    df_cluster_row$prior_cluster <- as.numeric(prior_cluster_number)
    
    # 1/20/19: Just keep the prior and current cluster and pull them out later.

    list_cluster_df[[source_cluster_number]] <- 
      rbind(list_cluster_df[[source_cluster_number]], df_cluster_row)
    
    ### All changes must be stored first or else the data that has not been accessed
    ### yet will be erased 
    ## Key is current cluster, value is source cluster
    list_clusters_update[[as.character(current_cluster_number)]] <- 
      as.character(source_cluster_number)

    cluster_tracking_row_index <- cluster_tracking_row_index + 1
  }
  
  list_cluster_link <- list_clusters_update
}

### UNIFY THE DATA FRAMES ON TIME ITERATION ###

# Setup initial data 
df_merged_clusters <- list_cluster_df[['1']]
names(df_merged_clusters) <- lapply(
  names(df_merged_clusters), 
  function(name) { return(paste('c1', name, sep='_'))}
)
names(df_merged_clusters)[12] <- 'time_iter'

for (cluster_number in 2:MAX_CLUSTERS) {
  this_df <- list_cluster_df[[as.character(cluster_number)]]
  names(this_df) <- lapply(
    names(this_df), 
    function(name) { return(paste0('c', cluster_number, '_', name))}
  )
  names(this_df)[12] <- 'time_iter'
  
  df_merged_clusters <- merge(
    x = df_merged_clusters,
    y = this_df,
    by = 'time_iter'
  )
}

# 1/20/19: Remove current cluster and prior cluster attributes and calculate the 
# delta for the remaining variables excluding "
result1 <- !grepl('current_cluster', names(df_merged_clusters), fixed=TRUE)
result2 <- !grepl('prior_cluster', names(df_merged_clusters), fixed=TRUE)
df_data_for_modeling <- df_merged_clusters[result1 & result2]


# =ABS(data!U66-data!U67)/((data!U66+data!U67)/2)
#i <- 2

################# DELTA ATTTRIBUTES ###############
df_delta <- df_data_for_modeling[1,]
df_delta[1,] <- 0
df_delta[1,]$time_iter <- 1
max_cols <- ncol(df_data_for_modeling)

for (i in 2:nrow(df_data_for_modeling)) {
    prior_row <- df_data_for_modeling[i-1, 2:max_cols]
    current_row <- df_data_for_modeling[i, 2:max_cols]
    delta_row <- abs(prior_row - current_row) / ((prior_row + current_row) / 2)
    delta_row$time_iter <- i
    df_delta <- rbind(df_delta, delta_row)
    
    #df_merged_clusters[i,] <- cbind(df_merged_clusters[i,], delta_row)
}

df_delta[is.na(df_delta)] <- 0

names(df_delta)[2:length(names(df_delta))] <- lapply(
    names(df_delta)[2:length(names(df_delta))], 
    function(name) { return(paste0(name, '_delta'))}
)

df_data_for_modeling <- merge(df_data_for_modeling, df_delta)

######## END OF DELTA ATTRIBUTES ############


################# START MODELING

### USE ONLY IF PRIOR STEP IS MESSING THINGS UP
#df_data_for_modeling <- df_merged_clusters

#### 85% good review... 2 good clusters (15 * (1-.85) = 2.25)
#### 70% good review... 4 good clusters (15 * (1-.70) = 4.5)


MAX_CLUSTERS <- 15

good_review_clusters <- 0
good_review_threshold <- 0.85
cluster_count_threshold <- 3
threshold <- 0.50
lead_iterations <- 1


for (i in 1:MAX_CLUSTERS) {
    
#### GOOD
  good_review <- paste('c', i, '_good_review', sep='')
  new_name <- paste('c', i, '_good_cluster_flag', sep='')
  df_data_for_modeling[, new_name] <- 
    as.vector(
      ifelse(df_data_for_modeling[good_review] >= good_review_threshold, 1, 0)
    )
  good_review_clusters <- good_review_clusters + df_data_for_modeling[, new_name]
}



df_data_for_modeling$good_cluster_count <- good_review_clusters
table(df_data_for_modeling$good_cluster_count)
plot(
    df_data_for_modeling$time_iter,
    df_data_for_modeling$good_cluster_count,
    'l',
    main=paste0('Threshold: ', good_review_threshold * 100, '%')
)

# correlation plot before items are turned into factors
df_data_for_modeling$good_system <- 
    ifelse(df_data_for_modeling$good_cluster_count >= cluster_count_threshold, 1, 0)

df_data_for_modeling$future_good_system <-
    lead(df_data_for_modeling$good_system, lead_iterations)

M<-cor(df_data_for_modeling)
corrplot(M, method="circle")

    
####### THRESHOLD FOR GOOD CLUSTER COUNT FOR SYSTEM
df_data_for_modeling$good_system <- 
  as.factor(
    ifelse(df_data_for_modeling$good_cluster_count >= cluster_count_threshold, 1, 0)
  )
table(df_data_for_modeling$good_system)

   
levels(
    df_data_for_modeling$good_system)[
        levels(df_data_for_modeling$good_system)=='0'] <- 'bad'

levels(
    df_data_for_modeling$good_system)[
        levels(df_data_for_modeling$good_system)=='1'] <- 'good'


# Lead steps to predict
lead_iterations <- 1

df_data_for_modeling$future_good_system <-
    lead(df_data_for_modeling$good_system, lead_iterations)

plot(
    df_data_for_modeling$time_iter,
    df_data_for_modeling$future_good_system,
    'l',
    main=paste0('Threshold: ', good_review_threshold * 100, '%')
)


# Drop the last rows that are missing data due to lead function
df_data_for_modeling <- 
    df_data_for_modeling[1:(nrow(df_data_for_modeling) - lead_iterations), ]


########### CREATE SUPPLEMENTAL ATTRIBUTES

# ABS(data!U66-data!U67)/((data!U66+data!U67)/2)

# Remove attributes not needed for modeling
# good_review, current_cluster, prior_cluster, good_cluster_flag; time_iter, good_system
list_exclude_vars <- list()

for (cluster_number in 1:MAX_CLUSTERS) {
  list_this <- lapply(
    c('good_review', 'current_cluster', 'prior_cluster', 'good_cluster_flag'), 
    function(name) { return(paste0('c', cluster_number, '_', name))}
  )
  list_exclude_vars <- c(list_exclude_vars, list_this)
}

# For using future_good_system as a target, need to exclude "good system"
#list_exclude_vars <- c(list_exclude_vars, 'time_iter', 'good_cluster_count')
list_exclude_vars <- c(list_exclude_vars, 'time_iter', 'good_cluster_count', 'good_system')





# Train: 55,774 records: 8/6/08 - 7/31/16; ~75%
#df_train <- df_all[1:55774,]
#df_train <- df_train[sample(1:nrow(df_train), 3500, replace=FALSE), ]
#df_train <- add_similarity_measures(df_train)

# Naive way of splitting 
#df_train <- subset(df_data_for_modeling, time_iter < 330)


### FIXED LIST
# THRESHOLD 70%, CLUSTER_THRESH=6, MODEL_THRESH=0.5
# df_data_for_modeling <- df_data_for_modeling[(
#     names(df_data_for_modeling) %in% c(
#         'c9_item_count', 'c14_item_count', 'c1_item_count', 'c1_log10_biz_review_count',
#         'c15_item_count', 'c3_pct_exclam', 'c13_item_count', 'c5_item_count', 'c8_item_count',
#         'c5_pct_exclam', 'c2_log10_biz_review_count', 'c2_useful_review_count', 'c2_item_count',
#         'c12_cluster_similarity', 'c7_review_size', 'c10_cluster_similarity',
#         'c14_log10_biz_review_count', 'c8_sentiment', 'c5_log10_usr_review_count',
#         'c7_item_count', 'future_good_system')
# )]

# Top 10-Bottom 10 for correlation with good system
#  df_data_for_modeling <- df_data_for_modeling[(
#      names(df_data_for_modeling) %in% c(
#         'c14_item_count','c2_sentiment','c6_item_count','c2_item_count','c10_sentiment',
#         'c3_item_count','c11_item_count','c4_item_count','c13_sentiment',
#         'c14_useful_review_count','c11_log10_usr_review_count','c6_sentiment',
#         'c5_review_size','c6_pct_exclam','c2_log10_biz_review_count',
#         'c9_log10_usr_review_count','c4_log10_usr_review_count','c2_pct_periods',
#         'c2_pct_apost','c5_log10_usr_review_count', 'future_good_system')
# )]
 
# THRESHOLD 85%, CLUSTER_THRESH=3, MODEL_THRESH=0.5
df_data_for_modeling <- df_data_for_modeling[(
    names(df_data_for_modeling) %in% c(
        'c1_item_count','c1_pct_quest','c10_item_count','c11_item_count','c11_useful_review_count',
        'c12_item_count','c13_pct_periods','c14_item_count','c14_pct_apost','c14_pct_periods',
        'c15_item_count','c2_item_count','c5_item_count','c5_review_size','c5_sentiment',
        'c5_useful_review_count','c7_item_count','c7_pct_quest','c9_item_count',
        'c9_useful_review_count', 'future_good_system')
)]

# Better way to parititon data
train_index <- createDataPartition(
    df_data_for_modeling$future_good_system, 
    p = .8, 
    list = FALSE, 
    times = 1
)

df_train <- df_data_for_modeling[train_index, ]

df_train <- df_train[(
  !(names(df_data_for_modeling) %in% list_exclude_vars)
)]


# Prior data
# for (i in 1:(nrow(df_train)-1) ) {
#   df_train$row_similarity[i+1] <- distance(
#     rbind(
#       c(df_train[i,1:165]),
#       c(df_train[i+1,1:165])
#     ), 
#     method="cosine"
#   )
  
  


############## GET TOP ATTRIBUTES ##################
ctrl <- trainControl(
  method="repeatedcv"
  , repeats = 5		    # repetitions of cross-validation 
  , number = 5        # number of folds ( * 2)
  #, summaryFunction = twoClassSummary	# Use AUC to pick the best model
  , classProbs = TRUE
  , verboseIter = TRUE
)

df_model <- train(
  #good_system ~ . 
  future_good_system ~ .
  , data = df_train
  , method = "rf"
  ,trControl= ctrl
)
#summary(df_model)
print(varImp(df_model))
print(df_model)
confusionMatrix(df_model)

fancyRpartPlot(df_model$finalModel)

#print(df_model)
print(df_model$finalModel)
print(df_model$results)
print(df_model$bestTune)
confusionMatrix(df_model)

#############################




# ^^^ VARIABLE TESTING

#Train and Tune the SVM
# df_model <- train(future_good_system ~ .  , 
#                   data = df_train,
#                   method = "svmLinear",   # Radial kernel
#                   tuneLength = 9,					# 9 values of the cost function
#                   preProc = c("center","scale"),  # Center and scale data
#                   metric = "ROC",
#                   trControl = ctrl)
# 
# print(df_model)
# print(df_model$finalModel)
# print(df_model$results)
# print(df_model$bestTune)
# confusionMatrix(df_model)
#############################################


# Test: 16,902 records: 8/1/16 - 7/26/17; ~25%
#df_test <- df_all[55775:72676,]
#df_test <- df_test[sample(1:nrow(df_test), 1300, replace=FALSE), ]
#df_test <- add_similarity_measures(df_test)

# Naive way of partitioning data
#df_test <- subset(df_data_for_modeling, time_iter >= 330)


######## MANUAL LIST SELECTION ############
# Filter the list

#### FOR THRESHOLD 
data_copy <- df_data_for_modeling[(
   names(df_all) %in% c(
       'c9_item_count', 'c14_item_count', 'c1_item_count', 'c1_log10_biz_review_count', 
       'c15_item_count', 'c3_pct_exclam', 'c13_item_count', 'c5_item_count', 'c8_item_count', 
       'c5_pct_exclam', 'c2_log10_biz_review_count', 'c2_useful_review_count', 'c2_item_count',
       'c12_cluster_similarity', 'c7_review_size', 'c10_cluster_similarity', 
       'c14_log10_biz_review_count', 'c8_sentiment', 'c5_log10_usr_review_count', 
       'c7_item_count', 'future_good_system')
 )]
train_index <- createDataPartition(
    data_copy$future_good_system, 
    p = .8, 
    list = FALSE, 
    times = 1
)

df_train <- df_data_for_modeling[train_index, ]

df_train <- df_train[(
    !(names(df_data_for_modeling) %in% list_exclude_vars)
)]

ctrl <- trainControl(
    method="repeatedcv"
    , repeats = 5		    # repetitions of cross-validation 
    , number = 5        # number of folds ( * 2)
    #, summaryFunction = twoClassSummary	# Use AUC to pick the best model
    , classProbs = TRUE
    , verboseIter = TRUE
)

df_model <- train(
    #good_system ~ . 
    future_good_system ~ .
    , data = df_train
    , method = "rf"
    ,trControl= ctrl
)
#summary(df_model)
print(varImp(df_model))
print(df_model)
confusionMatrix(df_model)
############ END OF MANUAL VARIABLE LIST


# Better way to partition data
df_test <- df_data_for_modeling[-train_index, ]

df_with_predict <- predict(
  df_model, 
  newdata=df_test[, !names(df_test) %in% c('future_good_system')], 
  type="prob")


library(pROC)
# https://stackoverflow.com/questions/30366143/how-to-compute-roc-and-auc-under-roc-after-training-using-caret-in-r
result.roc <- roc(df_test$future_good_system, df_with_predict$good) # Draw ROC curve.
plot(
    result.roc, 
    print.thres="best", 
    print.thres.best.method="closest.topleft",
    xlim=c(0.5, -0.1),
    main="Good Review Threshold 85%, Good Clusters >= 3"
)
result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy

my_table <- table(
 ifelse(unlist(df_with_predict['good'])>=result.coords[[1]],'good','bad'),
 df_test$future_good_system)
print(my_table)

#my_table <- table(
#  ifelse(unlist(df_with_predict['good'])>=threshold,'good','bad'),
#  df_test$future_good_system)
#print(my_table)

confusionMatrix(my_table, mode = "prec_recall", positive='good')
chisq.test(my_table)




#result.roc <- roc(data.test$Species, result.predicted.prob$versicolor) # Draw ROC curve.




#############
superstuff <- df_merged_clusters[2:469,]
boxplot(
    df_merged_clusters$c1_cluster_similarity,
    df_merged_clusters$c2_cluster_similarity,
    df_merged_clusters$c3_cluster_similarity,
    df_merged_clusters$c4_cluster_similarity,
    df_merged_clusters$c5_cluster_similarity,
    df_merged_clusters$c6_cluster_similarity,
    df_merged_clusters$c7_cluster_similarity,
    df_merged_clusters$c8_cluster_similarity,
    df_merged_clusters$c9_cluster_similarity,
    df_merged_clusters$c10_cluster_similarity,
    df_merged_clusters$c11_cluster_similarity,
    df_merged_clusters$c12_cluster_similarity,
    df_merged_clusters$c13_cluster_similarity,
    df_merged_clusters$c14_cluster_similarity,
    df_merged_clusters$c15_cluster_similarity
)



