#####################################################################################
############################# Prime Number Test #####################################

# Qn.1 Write a R program using control operators to test whether following values 
#      are prime numbers or not by providing a PRIME or NOT PRIME message as output: A.103 B.82 C.179

#Ans:

  check_prime <- function(num) 
  {
    if (num == 2) 
    {  print("PRIME")
    } 
    else if (any(num %% 2:(num-1) == 0)) 
    {  print("NOT PRIME")
    } 
    else 
    {  print("PRIME")
    }
  }
 
  check_prime(103)
  check_prime(82)
  check_prime(179)
  
#####################################################################################
# Qn.2 Write a R program using control operators to identify letter u and a both occur 
#      in the following words: 1. above 2. unit 3. Under 
  
  
  Find_letter <- function(word){
    
    if( grepl("a",word) & grepl("u",word) )     # if both the letters are occur
    { sprintf("letter a & u both occur in word %s", word ) }
    
    else if( grepl("a",word))                   # if only a occurs
     { sprintf("letter a is occur in word %s", word ) }
    
    else  if( grepl("u",word))                  # if only u occurs
    { sprintf("letter u is occur in word %s", word ) }
    
    else sprintf("both the letters are not occur in the word %s",word)
  }
  
  Find_letter("above")
  Find_letter("unit")
  Find_letter("under")
  Find_letter("sundar")  # if both the letters are occure
  Find_letter("bove")    # if both the letters are not occure
  
  ?grepl
####################################################################################  
####################### Body Mass Index Calculation ################################
  
# Qn.3 Write a function that to calculate BMI (Body Mass Index):
#      BMI for a person is defined as their body mass divided by the square of their height
#      The weight is in kilograms and the height in meters 

#Ans  

  BMI <- function(height, weight)
{
    { calc <- (weight/(height^2))
    }
    
    if(calc < 15)
    {  sprintf("Very severely underweight: BMI is %f", calc )  }
    
    else if (calc >= 15 & calc <16 ) 
    { sprintf("Severely underweight: BMI is %f", calc)   } 
    
    else if (calc >= 16 & calc < 18.5)
    { sprintf ("Underweight:BMI is %f", calc)   }
    
    else if (calc >= 18.5 & calc < 25)
    { sprintf("Normal (healthy weight): BMI is %f", calc)  }
    
    else if (calc >= 25 & calc < 30)
    { sprintf("Overweight: BMI is %f", calc)  } 
    
    else if (calc >= 30 & calc < 35)
    { sprintf("Obese Class I (Moderately obese) : BMI is %f", calc) }
    
    else if (calc >= 35 & calc < 40)
    { sprintf("Obese Class II (Severely obese) : BMI is %f", calc)  }
    
    else
    { sprintf("Obese Class III (Very severly obese) : BMI is %f", calc) }
}

# BMI of person of 1.78m hieght with 65 kg wieght
  
BMI(1.78,70)      

#####################################################################################
########################### SUM OF CUBES ############################################

# Qn.4 Write a function called sum_of_cubes, that calculates 
# the sum of cubes of the first n natural numbers :

#Ans:

Sum_of_cubes <- function(n)
{	
  x<-floor(n)   # Rounding the decimal value to floor, if given by user 
                # (As decimal values does not comes under NATURAL NUMBERS)
  
  { sum_cubes <- (x*(x+1)/2)^2  # Applying the sum of cubes formula
  }
  if (n<=0)
  { print("Enter POSITIVE number")
  }
  else 
  { sprintf ("Sum of cubes of first %d Natural numbers is %d ", x, sum_cubes)
  } 
}

Sum_of_cubes(3)
Sum_of_cubes(7.6)   # If decimal is given it will take to floor value

#####################################################################################
################################ Mode of a Vector ###################################

# Qn.5 Write a function to calculate the mode (highest frequency) of the following vector:
#     x = c(2,3,3,4,4,5,6,7,9,10)

# Ans..
# Steps to follow
# 1. create a vector v with given values & a vector with UNIQUE VALUES of given vector
# 2. match POSITIONS of V in unique vector
# 3. calculate FREQUENCY of each index positions
# 4. find out the highest FREQUENT INDEX position and corresponding value in unique value vector


v <- c(2,3,3,4,4,5,6,7,9,10)    # Given vector

getmode <- function(v) {
  uniq_values <- unique(v)
  index_frequency <- tabulate(match(v, uniq_values))
  high_index <- which.max(index_frequency)
  uniq_values[high_index]
}

result <- getmode(v)
print(result)

?tabulate
?unique

#####################################################################################
######################## Count of PRIME Numbers in vector ###########################

# Qn.6 Write a function to calculate the no. of prime numbers of the following vector :

x = c(2,2,3,3,4,5,7,11,15,19,24,29)   # Given vector

count_prime <- function(vector1)
{  count <- 0
   for(val in vector1)
   { if(val==2) count=count+1
     else if(any((val %% 2:(val-1)) == 0))
     next
     else{
          count=count+1
         }
   } 
sprintf("no. of prime numbers in vector is %d", count)
}

count_prime(x)

#####################################################################################
################## R package for calculating the count of prime numbers #############

# 7. Create a R package for calculating the count of prime numbers , name it as "CountPrime"

create("CountPrime")

count_prime <- function(vector1)
{  count <- 0
   for(val in vector1)
   { if(val==2) count=count+1
   else if(any((val %% 2:(val-1)) == 0))
   next
   else{
      count=count+1
       }
   } 
sprintf("no. of prime numbers in vector is %d", count)
}


#####################################################################################
# Qn.9 Create R functions for the following operations

# a.Find out unique combinations of data based on a particular column or group of columns.

# Select count(distinct stdid) from student group by classid

   aggregate(count ~ classid+sectionid,data = student,max) %>% 
     group_by(classid)

# Select count(distinct stdid) from student group by classid, sectionid
   aggregate(count ~ classid+sectionid,data = student,max) %>% 
     group_by(classid,sectionid)

#####################################################################################
# 10. Create R functions for the following operations

# a. Find out if there are any nulls in a dataset or in some specific number of columns
   
   find.nulls <- function(data){
     is.null(data)  
      }                        # function to ckeck for null in a dataset

find.nulls(dataset) 

#####################################################################################
# 11. Create R functions for the following operations

library(dplyr)

# a. Remove duplicates from a given vector and return it back.
rem_duplicates<- function(vector1){
  vector1 <- unique(vector1)
  }
  rem_duplicates()
  
# b. Compute count of distinct

count_distinct <- function(data){
  data %>% group_by(data$column) %>% tally() 
}
  count_distinct()
  
# c. Concatenate two strings.

# consider X and Y are two strings
c_strins <- function(x,y){
  data=data.frame(x,y)
paste0(x,y)
}
c_strings()

#####################################################################################
