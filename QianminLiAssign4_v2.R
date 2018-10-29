# STAT 6861 HW4

name = "Qianmin Li"

# (a)
#Define X
#X is the outcome of rolling one die
x = rep(1/6,6) # P(X = x) for x = 1,2,3,4,5,6

conv <- function(x,y) {
    # function starts 
    #define relevant lengths of vectors x & y
    n = length(x)
    m = length(y)
    #Make a new sequence a where new.x is a vector 
    #of x flipped from n:1 and the rest are 0's
    new.x = c(x[n:1],rep(0,(m-1)))
    #Make a new sequence new.y where new.y is a vector of 0's and then y
    new.y = c( rep(0,(n-1)),y )
    
    #Get relevant lengths k (only need to get of a 
    #since length(new.x) = length(new.y))
    k = (length(new.x))
    
    #Allocate vector for convolution
    conv.vec = numeric(k)
    
    #Start loop to calculate convolution
    for (i in 1:k)
    { #i
      conv.vec[i]=sum(new.x*new.y)
      
      #Re allocate new.x for next iteration in loop.
      #This is essentially shifting, 
      #getting the overlap, multiplying, and adding
      #across the overlap.
      new.x = c(0,new.x)
      new.x = new.x[1:k]
    }#i
    return(conv.vec)
  } # function ends

#Define two vectors for convolution function
rolling = rep(1/6,6)
dice = rep(1/6,6)

#Convolve 25 rolls of a die. So, if X1 = one roll of a die, 
#then Y = X1+X2+...+X25
for (i in 1:24)
{
  rolling = conv(rolling, dice)
}
rolling

# Plot of roll
plot(25:150,rolling,type="l", ylab="Probability", xlab="Total value of 25 rolls: 25 to 150")

# P( 79 <= Y <= 96)

sum(rolling[55:72])                 # 79 corresponds to the 55th indice & 96 to the 72nd


#P( 70 <= Y <= 105)
sum(rolling[46:71])                 # 70 corresponds to the 46th indice & 95 to the 71st


#(b)
#Define X~binomial(10,0.3) and Y~binomial(12,0.7)
n1 = 10 
p1 = 0.3
x = dbinom(0:n1,n1,p1) # pdf of values over support
n2 = 12
p2 = 0.7
y = dbinom(0:n2,n2,p2)

#Get convolution for Z = x + y
Z = conv(x,y)

#Plot of pdf,Z
plot(1:23,Z,type="l", ylab="Probability", xlab="Total value of Z = X + Y")

#Find probabilit Z<= 9
sum(Z[1:10])                 
