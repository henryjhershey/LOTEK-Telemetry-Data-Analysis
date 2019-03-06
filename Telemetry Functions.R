#merge EMG data with position files based on timestamp.
#this function averages the EMG data over each minute, then assigns that average to all the 
#positions recorded during that minute

merge_EMG <- function(pos, EMG, removeNA=F){
  EMG$minutes <- as.POSIXct(round(EMG$timestamp, "min"))
  pos$minutes <- as.POSIXct(round(pos$timestamp, "min"))
  means <- data.frame(minutes= unique(EMG$minutes), means=tapply(EMG$Data, EMG$minutes, mean), row.names= T)
  means$minutes <- as.POSIXct(rownames(means))
  out<- merge(means, pos, by="minutes", all.y=T)
  if (removeNA== T){out <- out[!(is.na(out$means)),]
  }else
    out
}

# Create Master file from tag CSVs
createMasterT  <-  function(path1){ 
  
  library(plyr)
  # this function adds a tagID column to each csv and skips 33 text rows
  # (the metadata above the actual data)
  read_csv_filename <- function(filename){
    ret       <- read.csv(filename)
    ret$tagID <- unlist(strsplit(filename, split = "_", fixed = T))[1] #EDIT
    ret
  }
  
  # create list of csvs that are in the working directory
  filenames   <- list.files(path1)
  
  #apply read.csv to all filenames in working directory then row bind csvs together
  mast <- ldply(filenames, read_csv_filename)
  return(mast)
  
}

#Create master file from receiver text files
createMasterR  <-  function(path1){ 
  
  library(plyr)
  # this function adds a tagID column to each csv and skips 33 text rows
  # (the metadata above the actual data)
  read_csv_filename <- function(filename){
    ret       <- read.delim(filename, sep="", header=F, stringsAsFactors = F)
    ret$Receiver <- unlist(strsplit(filename, split = " ", fixed = T))[1] #EDIT
    ret
  }
  
  # create list of csvs that are in the working directory
  filenames   <- list.files(path1)
  
  #apply read.csv to all filenames in working directory then row bind csvs together
  mast <- ldply(filenames, read_csv_filename)
  return(mast)
  
}


#create a master file of all the EMG files in a working directory.
createMasterEMG  <-  function(path1){ 
  
  library(plyr)
  # this function adds a tagID column to each csv and skips 33 text rows
  # (the metadata above the actual data)
  read_fwf_filename <- function(filename){
    ret       <- read_fwf(filename, fwf_widths(c(10,10,10,10,11,8,7,10)))
    ret <- ret[,c(1:8)]
    ret <- subset(ret,X8=="CEMG2")
    ret
  }
  
  # create list of csvs that are in the working directory
  filenames   <- list.files(path1)
  
  #apply read_fwf to all filenames in working directory then row bind tables together
  mast <- ldply(filenames, read_fwf_filename)
  colnames(mast) <- c("Date","Time","Channel","TagID","Antenna","Power","Data","SensorType")
  return(mast)
}
