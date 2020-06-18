library(readr)
library(data.table)


setwd("~/../Google Drive/Writing/Anomaly_Detection/code")

# Generate some IP addresses 
# Set the maximum size of the network

max_size <- 198

# set the subnet address

network <- "10.0.69."

# combine the two

ip_addresses <- paste(network,seq(1:max_size),sep="")

# create a data frame that will allow us to chose and mark the ones chosen.

chosen.df <- data.frame(ip_address=ip_addresses,chosen=0)

#define our lists 

source_ip           <- list()
tgt_ip              <- list()
tgt_ip2             <- list()

data_set_good       <- list()
data_set_anomaly_1  <- list()
data_set_anomaly_2  <- list()
data_set_anomaly_3  <- list()

number_of_devices   <- 5


# This loop sets the source and target IP Addressess for the network. 


for (i in seq(1:number_of_devices)) {
  data_set_index <- i
  
  # pick a source IP address 
  
  source_ip[[data_set_index]] <- chosen.df[sample(1:max_size,1),]$ip_address
  
  # set the chosen value 
  
  chosen.df[chosen.df$ip_address == source_ip[[data_set_index]],]$chosen <- 1
  
  #source_number <- sample(1:max_size,1)
  
  # Each IP address will communicate with between 2 and 7 targets (prime numbers for the win)
  
  tgt_samples <- sample(1:max_size,sample(2:7,1))
  
  tgt_samples2 <- sample(1:max_size,sample(2:7,1))
  
  tgt_ip[[data_set_index]] <- chosen.df[chosen.df$ip_address %in% chosen.df[tgt_samples,]$ip_address 
                      & chosen.df$chosen == 0 ,]$ip_address
  
  chosen.df[chosen.df$ip_address %in% tgt_ip[[data_set_index]],]$chosen <- 1
  
  tgt_ip2[[data_set_index]] <- chosen.df[chosen.df$ip_address %in% chosen.df[tgt_samples2,]$ip_address 
                                        & chosen.df$chosen == 0 ,]$ip_address
  
  chosen.df[chosen.df$ip_address %in% tgt_ip2[[data_set_index]],]$chosen <- 1
  
  
  
}
minutes             <- 10 
sample_size         <- 1  

g_range_byte_min    <- 30 
g_range_byte_max    <- 50 
g_variance_byte_min <- 10
g_variance_byte_max <- 20 

b_range_byte_min    <- 300 
b_range_byte_max    <- 500 
b_variance_byte_min <- 100
b_variance_byte_max <- 200 


time_range          <- minutes*60 
time_sample_min     <- 1
time_sample_max     <- 10
wrong_target        <- 1

for(i in seq(1:number_of_devices)) { 
  data_set_index <- i

    data_set_good[[data_set_index]] <- rbind(
                                        data.frame(src=source_ip[[data_set_index]]
                                                   ,bytes=floor(rnorm(time_range
                                                                      ,sample(g_range_byte_min:g_range_byte_max,sample_size)
                                                                      ,sample(g_variance_byte_min:g_variance_byte_max,sample_size)
                                                   )
                                                   )
                                                   ,timestamp=seq.POSIXt(as.POSIXct(Sys.time()), units = "seconds", by = sample(time_sample_min:time_sample_max,sample_size),length.out= time_range)
                                                   ,target=tgt_ip[[data_set_index]]),
                                        
                                        data.frame(src=tgt_ip[[data_set_index]]
                                                   ,bytes=floor(rnorm(time_range
                                                                      ,sample(g_range_byte_min:g_range_byte_max,sample_size)
                                                                      ,sample(g_variance_byte_min:g_variance_byte_max,sample_size)
                                                   )
                                                   )
                                                   ,timestamp=seq.POSIXt(as.POSIXct(Sys.time()), units = "seconds", by = sample(time_sample_min:time_sample_max,sample_size),length.out= time_range)
                                                   ,target=tgt_ip2[[data_set_index]]),
                                        
                                        data.frame(src=tgt_ip2[[data_set_index]]
                                                   ,bytes=floor(rnorm(time_range
                                                                      ,sample(g_range_byte_min:g_range_byte_max,sample_size)
                                                                      ,sample(g_variance_byte_min:g_variance_byte_max,sample_size)
                                                   )
                                                   )
                                                   ,timestamp=seq.POSIXt(as.POSIXct(Sys.time()), units = "seconds", by = sample(time_sample_min:time_sample_max,sample_size),length.out= time_range)
                                                   ,target=source_ip[[data_set_index]])
                                      )	
  # I like prime numbers
  
  for(i in 1:nrow(data_set_good[[data_set_index]])) { data_set_good[[data_set_index]][i,]$target <- sample(unique(data_set_good[[data_set_index]]$target),1,replace=TRUE)}
  for(i in 1:nrow(data_set_good[[data_set_index]])) { data_set_good[[data_set_index]][i,]$target <- sample(unique(data_set_good[[data_set_index]]$target),1,replace=TRUE)}    
  for(i in 1:nrow(data_set_good[[data_set_index]])) { data_set_good[[data_set_index]][i,]$target <- sample(unique(data_set_good[[data_set_index]]$target),1,replace=TRUE)}  
  
  # Make sure no numbers below 0 
  
  data_set_good[[data_set_index]][data_set_good[[data_set_index]]$bytes <0,]$bytes <- data_set_good[[data_set_index]][data_set_good[[data_set_index]]$bytes <0,]$bytes * -1   
  
  # Write the data frame 
  
  write_csv(data_set_good[[data_set_index]],sprintf("main_data/device_%i__baseline_%s.csv", data_set_index,Sys.Date()))
}

# For some other types of analysis, let's look at the whole network generated. 
for( ix in (number_of_devices+1):(number_of_devices+number_of_devices)) {
  tgt_index <- ix-number_of_devices
  src_index <- tgt_index +1 
  if(src_index > number_of_devices) { src_index <- 1}
  data_set_good[[ix]] <-    data.frame(src=tgt_ip[[tgt_index]][1]
                                       ,bytes=sample(g_range_byte_min:g_range_byte_max,sample_size)
                                       ,timestamp=seq.POSIXt(as.POSIXct(Sys.time()), units = "seconds", by = sample(1:3,sample_size),length.out= sample_size)
                                       ,target=source_ip[[src_index]]) 
}

full_network.df <- rbindlist(data_set_good)


write_csv(full_network.df,sprintf("main_data/full_network_%s.csv", Sys.Date()))  



for(i in seq(1:number_of_devices)) { 
  data_set_index <- i
  

  # anomaly 1: more bytes
  
  data_set_anomaly_1[[data_set_index]] <- data.frame(src=data_set_good[[data_set_index]]$src,
                                                     bytes=floor(rnorm(time_range
                                                                      ,sample(b_range_byte_min:b_range_byte_max,sample_size)
                                                                      ,sample(b_variance_byte_min:b_variance_byte_max,sample_size)
                                                     )),
                                                     timestamp=data_set_good[[data_set_index]]$timestamp,
                                                     target=data_set_good[[data_set_index]]$target)
  
  # Make sure no numbers below 0 
  
  data_set_anomaly_1[[data_set_index]][data_set_anomaly_1[[data_set_index]]$bytes <0,]$bytes <- data_set_anomaly_1[[data_set_index]][data_set_anomaly_1[[data_set_index]]$bytes <0,]$bytes * -1   
  
  # Write the data frame 
  
  write_csv(data_set_anomaly_1[[data_set_index]],sprintf("main_data/device_%i_anomaly_01_%s.csv", data_set_index,Sys.Date())) 
  
  
  # anomaly 2: same bytes wrong IP 
  
  wrong_target <- wrong_target +1 
  
  if(wrong_target > 5) { wrong_target <- 1 }
    
  data_set_anomaly_2[[data_set_index]] <- data.frame(src=source_ip[[data_set_index]]
                                           ,bytes=floor(rnorm(time_range
                                                              ,sample(g_range_byte_min:g_range_byte_max,sample_size)
                                                              ,sample(g_variance_byte_min:g_variance_byte_max,sample_size)
                                                              )
                                                        )
                                           ,timestamp=seq.POSIXt(as.POSIXct(Sys.time()), units = "seconds", by = sample(time_sample_min:time_sample_max,sample_size),length.out= time_range)
                                           ,target=tgt_ip[[data_set_index]])
  # I like prime numbers
  
  for(i in 1:nrow(data_set_anomaly_2[[data_set_index]])) { data_set_anomaly_2[[data_set_index]][i,]$target <- sample(unique(data_set_good[[wrong_target]]$target),1,replace=TRUE)}
  for(i in 1:nrow(data_set_anomaly_2[[data_set_index]])) { data_set_anomaly_2[[data_set_index]][i,]$target <- sample(unique(data_set_good[[wrong_target]]$target),1,replace=TRUE)}    
  for(i in 1:nrow(data_set_anomaly_2[[data_set_index]])) { data_set_anomaly_2[[data_set_index]][i,]$target <- sample(unique(data_set_good[[wrong_target]]$target),1,replace=TRUE)}  
  
  # Make sure no numbers below 0 
  
    
  data_set_anomaly_2[[data_set_index]][data_set_anomaly_2[[data_set_index]]$bytes <0,]$bytes <- data_set_anomaly_2[[data_set_index]][data_set_anomaly_2[[data_set_index]]$bytes <0,]$bytes * -1     
  
  # Write the data frame 
  
  write_csv(data_set_anomaly_2[[data_set_index]],sprintf("main_data/device_%i_anomaly_02_%s.csv", data_set_index,Sys.Date())) 
  
  
  # anomaly 3: more bytes and wrong IP
  
  data_set_anomaly_3[[data_set_index]] <- data.frame(src=data_set_anomaly_2[[data_set_index]]$src,
                                                     bytes=floor(rnorm(time_range
                                                                      ,sample(b_range_byte_min:b_range_byte_max,sample_size)
                                                                      ,sample(b_variance_byte_min:b_variance_byte_max,sample_size)
                                                     )),
                                                     timestamp=data_set_anomaly_2[[data_set_index]]$timestamp,
                                                     target=data_set_anomaly_2[[data_set_index]]$target)

  # Make sure no numbers below 0 
  
  
  data_set_anomaly_3[[data_set_index]][data_set_anomaly_3[[data_set_index]]$bytes <0,]$bytes <- data_set_anomaly_3[[data_set_index]][data_set_anomaly_3[[data_set_index]]$bytes <0,]$bytes * -1       
  
  # Write the data frame 
  
  write_csv(data_set_anomaly_3[[data_set_index]],sprintf("main_data/device_%i_anomaly_03_%s.csv", data_set_index,Sys.Date())) 
  
  

  }