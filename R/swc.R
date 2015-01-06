#software carpentry workshop

#using assignment operators
#preferred assignment using <- (not =), but both work
x<-5
5
x=8
x

#read in a data file
surveys<-read.csv(file="surveys.csv")
#don't need to have "file =", but recommended for reproducability

inflam2<-read.csv(file="inflammation-02.csv", header=FALSE)

#to view the first six lines of the data
head(inflam2)
#to figure out what kind of class the data is
class(inflam2)
#to determine the dimensions of the data
dim(inflam2)
#to figure out what specific entry is
inflam2[30,2]
inflam2[31,2]
#all values from day "2"
inflam2[,2]

#data format: [row, cols]; note: index starts at 1!!!
inflam2[1:4,1:5]

#want to know mean data for day 7:
mean(inflam2[,7])
#min value:
min(inflam2[,7])
#median:
median(inflam2[,7])
#standard deviation:
sd(inflam2[,7])


#MARGIN: "2" indicates columns; "1" indicates rows
#calculate means of all columns:
apply(inflam2, MARGIN=2, mean)

#create a vector of the mean values of each column:
avg_day_inflam<-apply(inflam2, MARGIN=2, mean)

#plot the vector above:
plot(avg_day_inflam)

max_day_inflam<-apply(inflam2, MARGIN=2, max)
plot(max_day_inflam)

min_day_inflam<-apply(inflam2, MARGIN=2, min)
plot(min_day_inflam)

sd_day_inflam<-apply(inflam2, MARGIN=2, sd)
plot(sd_day_inflam)


#write a function to convert farenheit to kelvin:
far_to_kelv<-function(temp){
  kelvin<-((temp-32)*(5/9))+273.15
  return(kelvin)
}

#write a function to convert kelvin to celcius:
kelv_to_cel<-function(temp){
  celcius<-temp-273.15
  return(celcius)
}

#write fn that converts farenheit to celcius using fns above:
far_to_cel<-function(temp){
  kelvin<-far_to_kelv(temp)
  celcius<-kelv_to_cel(kelvin)
  return(celcius)
}

#assign a vector (can be numbers, characters, ect...)
vec<-c(1,2,3)
vec<c("1","2","3")


best_practice<-c("write","programs","for","people","not","computers")
asterisk<-"***"
star<-"*"

#easy to concatenate a vector to another vector
new<-c(best_practice, asterisk)

#write fn that adds asterisk to beginning an end of vector
fence<-function(inside, outside){
  result<-c(outside, inside, outside)
  return(result)
}


#write a fn that creates mean, max, min plots for any data file 
analyze<-function(filename){
  data<-read.csv(file=filename, header=FALSE)
  
  avg_day_inflam<-apply(data, MARGIN=2, mean)
  plot(avg_day_inflam)
  
  max_day_inflam<-apply(data, MARGIN=2, max)
  plot(max_day_inflam)
  
  min_day_inflam<-apply(data, MARGIN=2, min)
  plot(min_day_inflam)
}


#writing for loops:
for(variable in collection){
  do things
}

length(best_practice)

#example of loop instead of using length fn:
len<-0
for(i in best_practice){
  len<-len+1
}
len

values<-c(1,2,3)
sum<-0
mysum<-function(vec){
  for(j in vec){
    sum=sum+j
    }
   return(sum)   
}

#how to create a loop to batch analyze data files:
#regular expressions can be used to call the specifics in header that you want
list.files(pattern="^inflammation.+\\.csv$")
#creates a vector of the files with the above conditions

filenames<-list.files(pattern="^inflammation.+\\.csv$")
for(k in filenames){
  print(k)
  analyze(k)
}

analyze_all<-function(datapattern){
  filenames<-list.files(pattern=datapattern)
  for(k in filenames){
    print(k)
    analyze(k)
  }
}


