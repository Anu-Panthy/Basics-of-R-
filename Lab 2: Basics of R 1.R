x <-1
x  
x<-1
x < -1
X<--1
x <-1
x<-1
x
x <-1
(X<--1)
X==x
X!=x
x<-1.0
x
class(x)
x<-1L
class(x)
x<-"Anu"
x<-TRUE ;Y<-FALSE
class(x)
vector<-c(1,2,3,4)
vector<-1:4
matrix <- matrix (vector, nrow = 2, ncol = 2)
matrix
df <- data.frame(Microgreen_Number = c("A","B","C","D"), values = vector)
df
View(df)
num_vec <- c(1,2,3,4)
num_vec[3]
num_vec[5] <- 5
num_vec
num_vec[6]<-11
num_vec
num_vec[10]<-11        
num_vec
num_vec[3]
num_vec[3] <- num_vec[5]
num_vec[3]
num_vec
cha_vec <- c("this", "is", "a", "vector")
cha_vec[c(1,4)]
cha_vec<-c("Microgreen","is","a","superfood")
cha_vec[c(1,2,3,4)]
names(num_vec) <-c("RedCabbage", "Borage", "Mustard", "Firstsem", "secondsem")
num_vec
num_vec[c("second","fourth")]

#task included in the slides

#1.Create a numeric vector with a length of five
num_vec <- c(59,69,79,89,99,100)

# Create a character vector with a length of five
cha_vec<-c("Ram_Height", "Borage_Height", "Mustard_Height", "Firstsem", "secondsem")

#Assign the character vector as names to numeric vector.
(names(num_vec)<-cha_vec)

#Create a new vector that contains the first, third, and fifth indices of the numeric vector.
(new_vec <- num_vec[c(59,69,79)])

#Create a new vector that contains the names of the indices in the vector you created in 4.
(new_vec2<-names(new_vec))

#Activity: Vector Math
Leaf_area <- c(2.5, 1.6, 2.2, 2.6)
mean(Leaf_area)

Leaf_mass <- c(22, 15, 20, 24)
new<-c("Leaf_mass", "Leaf_area")
Leaf_area <- c(2.5, 1.6, 2.2, 2.6)
mean(Leaf_area)
Leaf_mass<- c(22, 15, 20, 24)
mean(Leaf_mass)
LMA<-Leaf_mass/Leaf_area
mean(LMA)
mean(Leaf_mass/Leaf_area)


