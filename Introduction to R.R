x <- c(1,2,4,6,3,7,1,6)
print(x)
print(x[2])
print(x[2:4])
print(x[c(2,5,8)])
print(x[-2])

x <- c(3,-1,5,-3,7)
ix <- c(T,T,F,F,T)
x[ix]

ix <- x > 0
x[ix]
x[x>0]


x <- runif(10)
x <- runif(10, min = 3,max = 10)

x <- rnorm(10)
x <- rnorm(10, mean = 3, sd = 1.2)
print(x)

x <- rnorm(100)
x[x > -0.5 & x < 0.5] <- 0
x

x <- 1:10
x

x <- 10:5
x


x <- seq (1,10, by = 2)
x

x <- seq (1,10, length.out = 5)
x

x <- rep(c(1,2,3), times = 3)
x

x <- rep(c(1,2,3), length.out = 8)
x      

x <- rep(c(1,2,3), each = 3)
x

x <- c(3,6,2,5)
x*3
x + 3
x**3

x <- 1:5
y <- 2:6

x + y

x <- 1:5
y <- 2:5

x + y

observed <- rnorm(100)
predicted <- rnorm(100)

mse <- mean((observed-predicted)**2)
mse


M <- matrix(1:6, ncol = 3)
M

M <- matrix(1:6, nrow = 3)
M

M <- matrix(1:7, ncol = 3)
M

M <- matrix(1:6, ncol = 2)
M

M <- matrix(1:6, ncol = 2, byrow = TRUE)
M

M[2,1]
M[2,]
M[,2]

M[1:2,]

M <- matrix(1:12, ncol = 4)
M

M[2:3,3:4]

x <- runif(100)

ifelse(x>0.5,1,0)

gender_vector <- c("Male", "Female", "Female", 
                   "Male", "Male"

gender_vector_factor <- factor(gender_vector)
gender_vector_factor

grades <- c('A','F','D','B','F','B','C','D','D','B','F')
grades_factor <- factor(grades, ordered = TRUE)
grades_factor

grades_factor <- factor(grades, ordered = TRUE, 
                        levels = c("F","D","C","B","A"))
grades_factor

grades_factor <- factor(grades, ordered = TRUE)
grades_factor
levels(grades_factor) <- rev(levels(grades_factor))
grades_factor


head(mtcars,5)
str(mtcars)
summary(mtcars)


name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", 
          "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

df <- data.frame(name,type,diameter,rotation,rings)
df
df[1:5,]
df[1:5,3:5]
df
df[,"rings"]
df[,c("name","rings")]

df$rotation

name1 <- name[1:7]
df <- data.frame(name,type,diameter,rotation,rings)
df

mean(df[df[,"rings"],"diameter"])

subset(df, rings == TRUE)
df_new <-subset(df, rings)
mean(df_new$diameter)

my_vec <- 1:10
my_mat <- matrix(my_vec,ncol = 2)
my_df <- mtcars[1:5,]

my_list <- list(vec = my_vec, mat = my_mat, df =  my_df)
my_list
my_list$df
my_list$df$hp
