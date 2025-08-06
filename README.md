# magma
Time-series dynamic clustering algorithm

Algorithm steps:

1. From the primary data frame, get block of observations with only the columns needed for clustering  covering the time window. 
2. Perform clustering on this block of observations. Mark the observations with the current time block ID. Specify max clusters and consistently apply throughout. Store the assigned cluster number for each observation. 
3. For each cluster in the time block, store the mean values for each column along with the time block ID and the cluster number. If necessary, create another data frame with the same columns but store the standard deviations instead. 
4. Shift the time window by the specified amount and repeat steps 1, 2, and 3 until done. 
5. Store the cluster data frames with the mean value and standard deviation columns.
6. For each time block in the cluster data frame, create one wide row that takes each cluster entry and appends its columns to the prior cluster until all clusters for a time block are in a single row. 
7. Repeat this for the next time block and append it’s row to this new data frame. Between the two rows, calculate the consume similarity between the cluster in the current row and the cluster in the prior row until n-squared similarities are calculated in a separate matrix. 
8. For each prior cluster, find the current cluster that has the highest similarity. Once selected, zero out the row and column from that matrix, and find the next highest similarity until the entire matrix is zeroed out. In a separate data frame, store the time block number, the prior cluster number, and the current cluster number. Advance to the next time block and repeat steps 7 and 8. 
REVISION: To avoid drift, another option is to cluster all of the clusters at one time and then align them accordingly instead of doing this after each time block. Even if done on a regular basis (daily, weekly, etc.) it might be a better choice because incremental drift is minimized.
