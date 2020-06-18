setwd("~/../Google Drive/Writing/Anomaly_Detection/code")
#Generate some IP addresses 
# Set the maximum size of the network
max_size <- 195
# set the subnet address
network <- "10.0.69."
# combine the two
ip_addresses <- paste(network,seq(1:max_size),sep="")
# create a data frame that will allow us to chose and mark the ones chosen.
chosen.df <- data.frame(ip_address=ip_addresses,chosen=0)
#define our lists 

source_ip         <- list()
tgt_ip            <- list()
#data_set          <- list()
data_set_good     <- list()
data_set_anomaly  <- list()


for (i in seq(1:5)) {
  data_set_index <- i
  
  # pick a source IP address 
  source_ip[[data_set_index]] <- chosen.df[sample(1:max_size,1),]$ip_address
  # set the chosen value 
  chosen.df[chosen.df$ip_address == source_ip[[data_set_index]],]$chosen <- 1
  #source_number <- sample(1:max_size,1)
  
  # Each IP address will communicate with between 2 and 7 targets (prime numbers for the win)
  tgt_samples <- sample(1:max_size,sample(2:7,1))
  
  
  tgt_ip[[data_set_index]] <- chosen.df[chosen.df$ip_address %in% chosen.df[tgt_samples,]$ip_address 
                      & chosen.df$chosen == 0 ,]$ip_address
  chosen.df[chosen.df$ip_address %in% tgt_ip[[data_set_index]],]$chosen <- 1
}
for(in in seq(1:5)) { 
  
  minutes           <- 10 
  range_byte_min    <- 30 
  range_byte_max    <- 50 
  sample_size       <- 1
  variance_byte_min <- 10
  variance_byte_max <- 20 
  time_range        <- minutes*60 
  time_sample_min   <- 1
  time_sample_max   <- 10
  wrong_target      <- 1
  
  
  data_set_good[[data_set_index]] <- data.frame(src=source_ip[[data_set_index]]
                                 ,bytes=floor(rnorm(150,sample(range_byte_min:range_byte_max,sample_size),sample(variance_byte_min:variance_byte_max,sample_size)))
                                 ,timestamp=seq.POSIXt(as.POSIXct(Sys.time()), units = "seconds", by = sample(time_sample_min:time_sample_max,sample_size),length.out= time_range)
                                 ,target=sample(tgt_ip[[data_set_index]],1,replace=TRUE))
  # I like prime numbers
  
  for(i in 1:nrow(data_set[[data_set_index]])) { data_set[[data_set_index]][i,]$target <- sample(tgt_ip[[data_set_index]],1,replace=TRUE)}
  for(i in 1:nrow(data_set[[data_set_index]])) { data_set[[data_set_index]][i,]$target <- sample(tgt_ip[[data_set_index]],1,replace=TRUE)}    
  for(i in 1:nrow(data_set[[data_set_index]])) { data_set[[data_set_index]][i,]$target <- sample(tgt_ip[[data_set_index]],1,replace=TRUE)}  
  write_csv(data_set[[data_set_index]],sprintf("main_data/device_%i_good_%s.csv", data_set_index,Sys.Date()))

  wrong_target <- wrong_target +1 
  if(wrong_target > 5) { wrong_target <- 1 }
  data_set_anomaly[[data_set_index]] <- data.frame(src=source_ip[[data_set_index]]
                                           ,bytes=floor(rnorm(150,sample(range_byte_min:range_byte_max,sample_size),sample(variance_byte_min:variance_byte_max,sample_size)))
                                           ,timestamp=seq.POSIXt(as.POSIXct(Sys.time()), units = "seconds", by = sample(time_sample_min:time_sample_max,sample_size),length.out= time_range)
                                           ,target=sample(tgt_ip[[wrong_target]],1,replace=TRUE))
  # I like prime numbers
  
  for(i in 1:nrow(data_set[[data_set_index]])) { data_set[[data_set_index]][i,]$target <- sample(tgt_ip[[wrong_target]],1,replace=TRUE)}
  for(i in 1:nrow(data_set[[data_set_index]])) { data_set[[data_set_index]][i,]$target <- sample(tgt_ip[[wrong_target]],1,replace=TRUE)}    
  for(i in 1:nrow(data_set[[data_set_index]])) { data_set[[data_set_index]][i,]$target <- sample(tgt_ip[[wrong_target]],1,replace=TRUE)}  
  write_csv(data_set[[data_set_index]],sprintf("main_data/device_%i_anomaly_%s.csv", data_set_index,Sys.Date())) 
  wrong_target <- wrong_target +1 
  
  }