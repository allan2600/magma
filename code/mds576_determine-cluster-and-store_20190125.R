library(RMySQL)
library(ggplot2)
library(cluster)

# Basic setup
set.seed(2011)
setwd('/Volumes/USB_UBUNTU/courses/mds576-research-methods/code/')
#setwd('/Users/allan/Documents/elmhurst/courses/mds549-data-mining-project/')

# Get database cred
cred <- read.csv('u-p.csv')
cred$u <- as.character(cred$u)
cred$p <- as.character(cred$p)

# Connect to database
con <- dbConnect(
    MySQL(), 
    user = cred$u, 
    password= cred$p, 
    dbname='yelp_db', 
    host='localhost'
)

# Query
sql_cmd <- "select * from v_mds564_final_data_20180815"

rs <- dbSendQuery(con, sql_cmd)
df_biz <- dbFetch(rs, n=-1)

# Clean up and close
dbClearResult(rs)

# Remove business_id and name
drop_vars <- names(df_biz) %in% c('review_id', 'business_id', 'user_id','review_date', 'stars', 'useful_review_count',
                                  'review_text_length', 'good_review', 'year_wk')
#df_biz_for_cluster <- df_biz[!drop_vars]

# Scale the data columns to be zero mean 
# and unit variance.
# The output of scale() is a matrix.
#

# Number of clusters
clust_cnt <- 15

#min_year_wk <- min(df_biz$year_wk)
min_year_wk <- 200834
max_year_wk <- max(df_biz$year_wk)

time_iter <- 1
this_year_wk <- min_year_wk
end_year_wk <- (((this_year_wk %/% 100) + ((this_year_wk %% 100) + 4) %/% 53) * 100) + (((this_year_wk %% 100) + 4) %% 53)



while (end_year_wk < max_year_wk) {
    print(paste(time_iter, this_year_wk, end_year_wk))
    
    # Set the date range
    new_df <- df_biz[(df_biz$year_wk >= this_year_wk & df_biz$year_wk < end_year_wk),]
    pmatrix <- scale(new_df[!drop_vars])
    
    # optionally, store the centers and 
    # standard deviations of the original data,
    # so you can "unscale" it later.
    pcenter <- attr(pmatrix, "scaled:center")  
    pscale <- attr(pmatrix, "scaled:scale")
    
    nrow(new_df)
    summary(new_df)
    #   Create the distance matrix.
    d <- dist(pmatrix, method="euclidean") 
    #d <- distance(pmatrix, method="jaccard") 
    
    #   Do the clustering. 
    pfit <- hclust(d, method="ward.D")   
    
    #   Plot the dendrogram.
    #plot(as.phylo(pfit), type='fan', use.labels=TRUE, labels=substr(df_biz$name, 1, 10))
    
    
    
    #plot(pfit, main="Review 3-Wk Performance: 15 Clusters") #, labels=substr(enc2utf8(df_biz$user_id),1,10))
    #rect.hclust(pfit, k=clust_cnt)
    
    
    groups <- cutree(pfit, k=clust_cnt)
    
    pfit_dendogram <- as.dendrogram(pfit)
    the_cut <- cut(pfit_dendogram, h=clust_cnt)
    
    #plot(the_cut$upper, 
    #     main="Upper tree of cut at h=25")
    #rect.hclust(pfit, k=30)
    
    # Assign to groups
    new_df$cluster <- 0   # Needed for initialization
    new_df$cluster <- groups
    new_df$time_iter <- time_iter
    
    # Create dataframe with stats with introductory data (delete later)
    # Definitions of columns to exclude
    exclude_var <- names(new_df) %in% c('review_id', 'business_id', 'user_id','review_date','year_wk')
    
    
    biz_mean <- t(apply(new_df[new_df$cluster == 1,][!exclude_var], 2, mean))
    #biz_sd <- t(apply(new_df[new_df$cluster == 1,][!exclude_var], 2, sd))
    item_count <- c()
    
    
    for (i in 1:clust_cnt) {
        row_count <- nrow(new_df[new_df$cluster == i,])
        #reviews <- mean(df_biz[df_biz$cluster == i, "review_count"])
        print(paste('Cluster ', i, ': ', row_count))#, ' ... avg reviews: ', reviews))
        item_count <- c(item_count, row_count)
        
        biz_row_mean <- t(apply(new_df[new_df$cluster == i,][!exclude_var], 2, mean))
        #biz_row_sd <- t(apply(new_df[new_df$cluster == i,][!exclude_var], 2, sd))
        
        # Add extra data to cluster mean
        #biz_row_mean[['item_count']] <- row_count
        #biz_row_mean[['time_iter']] <- 1
        
        biz_mean <- rbind(biz_mean, biz_row_mean)
        #biz_sd <- rbind(biz_sd, biz_row_sd)
    }
    
    # Delete duplicate first cluster
    biz_mean <- biz_mean[-1,]
    #biz_sd <- biz_sd[-1,]
    
    # Add the item count
    biz_mean <- cbind(biz_mean, item_count, this_year_wk, end_year_wk)
    
    
    # Delete duplicate first cluster
    t_biz_mean <- t(biz_mean[-c(1),])
    #t_biz_sd <- t(biz_sd[-c(1),])
    
    # Not the best way to do things, but checking for the first iteration and assigning the data frame will work
    if (time_iter == 1) {
        df_clusters <- biz_mean  

        write.table(
            new_df, 
            'mds576_data-with-cluster_20190125-13.csv', 
            col.names = TRUE,
            sep=','
        )
        
    }
    else {
        df_clusters <- rbind(df_clusters, biz_mean)
        
        write.table(
            new_df, 
            'mds576_data-with-cluster_20190125-13.csv', 
            sep=',',
            col.names = FALSE,
            append=TRUE
        )
        
    }
    
    
    # increment by 1 week... will have overlap... keep 4-week window (excluding 5th week)
    this_year_wk <- (((this_year_wk %/% 100) + ((this_year_wk %% 100) + 1) %/% 53) * 100) + (((this_year_wk %% 100) + 1) %% 53)
    end_year_wk <- (((this_year_wk %/% 100) + ((this_year_wk %% 100) + 4) %/% 53) * 100) + (((this_year_wk %% 100) + 4) %% 53)
    time_iter <- time_iter + 1
}
##################################################
write.csv(df_clusters, 'mds576-cluster-performance-clusters_20190125-00.csv')


# Add row for number of items in cluster
t_biz_mean <- rbind(t_biz_mean, item_count)
#t_biz_sd <- rbind(t_biz_sd, item_count)

# Write to file
write.csv(t_biz_mean, 'mds576-cluster-performance-mean_20190125-00.csv')
#write.csv(t_biz_sd, 'mds564-cluster-performance-stddev_20180804-14.csv')

# Create one hot encoding
for (i in 1:clust_cnt) {
    new_name <- paste('perf_c', i, sep='')
    df_biz[new_name] <- ifelse(df_biz$cluster == i, 1, 0)
}

# Write vector to table with ID and close connection
# start_cluster_index <- match('cluster', names(df_biz))
# dbWriteTable(
#     con, 
#     value <- df_biz[c(1, start_cluster_index + seq(1,clust_cnt))], 
#     name = "tor_cluster_performance_mds579_190122", 
#     overwrite = TRUE, 
#     append = FALSE
# )
dbDisconnect(con)

