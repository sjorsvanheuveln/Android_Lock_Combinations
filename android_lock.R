#android lock
#SvH 26-11-2015

# LC: Use <<- for global variables

get_nb <- function(x,y,lock){
  #returns coordinates of neighbors
  nb_coor <- list()
  nb_index <- 1
  for (i in (x - 1) : (x + 1)){
    for (j in (y - 1) : (y + 1)){
      if ((i == x) & (j == y)){next} # skip current location
      if (!is.na(lock[i,j])){nb_coor[[nb_index]] <- c(i,j);nb_index <- nb_index+1}
    }
  }
  return(nb_coor)
}
recurse <- function(x,y,lock){
  lock[x,y] <- NA; #lock <- initiate(lock)
  cat(x,y,"level = ",cur_level,'\n')
  print(lock);cat('\n'); #Sys.sleep(0.03)
  if (cur_level < max_level){ #recurse if max_level is not reached
    cur_level <<- cur_level + 1
    for (coor in get_nb(x,y,lock)){
      recurse(coor[1],coor[2],lock)}
    cur_level <<- cur_level -1
  }
  else{total <<- total + 1;cat("total =", total,'\n')} 
  #add 1 to total when max_level is reached
}

lock <- matrix(, nrow = 5, ncol = 5); lock[2:4,2:4] <- 0;lock
total = 0
max_level = 4 #length of password

#start function
for (x in 2:4){  
  for (y in 2:4){
    cur_level = 1
    recurse(x,y,lock) }}
cat('\n','For a password length of',max_level,'there are',total,'possibilities!')


