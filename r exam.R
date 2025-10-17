#Q.1
exam_scores <- c(85, 90, 78, 92, 88, 76) # a.
mean_score <- mean(exam_scores) 
print(paste("Mean of the vector:" , mean_score)) 
# b.
exam_scores[ length(exam_scores)] <- 80 
print("Vector after replacing the last element with 80:" ) 
print(exam_scores)


#Q.2
fruits <- c("Apple", "Banana" , "Orange" , "Mango")
print(paste("Class o f 'fruits' before conversion:" , class(fruits)))
fruits_factor <- as.factor (fruits)
print("Vector after conversion to factor:" ) 
print(fruits_factor)
print(paste("Class of 'fruits_factor':" , class(fruits_factor)))

#Q.3


Name <- c("John", "Anna", "Mike") 
Age <- c(23, 25, 22)
Score <- c(88, 92, 81)
student_data <- data.frame (Name, Age, Score)
print("Created Data Frame:" ) 
print(student_data)
print("Age column:" )
print(student_data$Age)

#Q.4
a <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE) 
print(a)
sum_row2 <- sum(a[2, ])
print(paste("Sum of elements in the 2nd row:" , sum_row2)) 

#Q.5
# a.
my_list <- list(c(2, 4, 6), "Hello", TRUE) 
print(my_list)
# b.
extracted_vector <- my_list[[ 1]] 
print(extracted_vector)

#Q.6
array1 <- array(1:18, dim = c(3, 3, 2)) 
print(array1)
element <- array1[2, 3, 1]
print(paste("Element at (row -2, column -3, matrix -1):", element)) 


#Q.7
matrix_1 <- matrix(1:12, nrow = 3) 
print(matrix_1)
# a
print(matrix_1[, 2]) 
# b.
print(colSums(matrix_1)) 

#Q.8

list_1 <- list( c(10, 20, 30),
                matrix(1:4, nrow = 2, ncol = 2), "R Programming"
)
print(list_1) 
print(list_1[[ 3]])

#Q.9

df <- data.frame ( ID = c(1,2,3,4),
                   Name = c("A", "B", "C", "D"), Marks = c(85, 90, 78, 92)
)
print(df)
# a.
print(df[df $Marks > 80, ]) 
# b.
df$Grade <- ifelse(df$Marks >= 50, "Pass", "Fail") 
print(df)

#Q.10

grades <- c("B","A","C","B","A","C","B")
grades_factor <- factor(grades)
print(levels(grades_factor))
grades_factor_ordered <- factor(grades, levels = c("A", "B", "C"), ordered = TRUE)
print(levels(grades_factor_ordered)) 

#Q.11

my_array <- array(1:18, dim = c(3, 3, 2)) 
print(my_array)
matrix_2 <- my_array[,, 2] 
print(matrix_2)
sum_first_column_first_matrix <- sum(my_array[, 1,1])
print(sum_first_column_first_matrix)

#Q.12

Id_no <- 1:10
Age_of_husband_yrs <- c(28, 27, 26, 32, 30, 29, 35, 30, 32, 40)

Age_of_wife_yrs <- c(24, 24, 20, 28, 25, 26, 32, 26, 30, 35)
# a
d <- data.frame (n = Id_no, x = Age_of_husband_yrs, y = Age_of_wife_yrs) 
print(d)
# b
print(head(d, 6))
# c
print(tail(d,6)) 
# d
# Number of rows
print(paste("Number of rows in 'd':" , nrow(d))) 
# Number of columns
print(paste("Number of columns in 'd':" , ncol(d))) 
#e
# Display only first 3 rows 
print(head(d, 3))
# Display last 3 rows 
print(tail(d, 3))
# Display first 3 rows and two columns 
print(d[1:3, 1:2])
# Display 3rd row of 1st column entry
print(d[3,1])
#f
# Create new variable Z=(x+y)/2 and add it to "d" as label z 
d$z <- (d$x + d$y) / 2
print(d) 

#Q.13
# a.
Id_no <- 1:10
Age_of_son_yrs <- c(3, 2, 6, 2, 3, 7, 4, 5, 7, NA)
c <- data.frame (Id_no = Id_no, z1 = Age_of_son_yrs) 
print(c)
# b.
colnames (c)[ colnames (c) == "z1"] <- "z" 
print(c)



#Q.14
library(ggplot2) data(stackloss) 
# a.
hist(stackloss$Air.Flow, main = "Histogram of Air Flow" , xlab = "Air Flow" ) boxplot(stackloss$Air.Flow, main = "Boxplot of Air Flow" , ylab = "Air Flow" )

# b.
print("Enhanced Histogram of Air Flow (binwidth=5):" ) ggplot(stackloss, aes(x = Air.Flow)) +
  geom_histogram (binwidth = 5, fill = "skyblue" , color = "black") + labs(title = "Histogram of Air Flow (Binwidth = 5)" ,
                                                                           
                                                                           x = "Air Flow Rate" , y = "Frequency" ) +
  theme_minimal ()


# c.
print("Scatter Plot: Air Flow vs. Water Temperature:" ) ggplot(stackloss, aes(x = Air.Flow, y = Water.Temp)) + geom_point (color = "darkred" ) +
  labs(title = "Scatter Plot: Air Flow vs. Water Temperature" , x = "Air Flow Rate" ,
       y = "Water Temperature (°C)") + theme_minimal ()


#Q.15


library(readxl) library(ggplot2)

loan_data <- read_excel ("C:\\Users \\ARUSTAR GUPTA \\Downloads \\loan_data.xlsx" ) loan_data

#a
ggplot(loan_data, aes(x = Purpose)) + geom_bar (fill = "lightblue" ) +
  labs(title = "Simple Bar Plot of Loan Purpose" , x = "Purpose of Loan" , y = "Count") +
  theme_minimal ()

#b
ggplot(loan_data, aes(x = Purpose, fill = Creditability)) + geom_bar (position = "dodge") +
  labs(title = "Loan Purpose and Creditability by Customer Count" , x = "Purpose of Loan" ,
       y = "Count of Customers" , fill = "Creditability" ) +
  theme_minimal ()


#c
ggplot(loan_data, aes(x = `Credit.Amount `)) +
  geom_histogram (binwidth = 1000, fill = "lightblue" , color = "blue") + labs(title = "Histogram of Credit Amount" , x = "Credit Amount" , y = "Frequency" ) +
  theme_minimal ()


#d
ggplot(loan_data, aes(x = Creditability, y = `Credit.Amount `, fill = Creditability)) +
  
  geom_boxplot () +
  labs(title = "Boxplot of Credit Amount by Creditability" , x = "Creditability" ,
       y = "Credit Amount" ) + theme_minimal ()


#e
ggplot(loan_data, aes(x = `Credit.Amount `, color = Creditability)) + geom_freqpoly (binwidth = 1000) +
  labs(title = "Frequency Polygon of Credit Amount by Creditability" , x = "Credit Amount" ,
       y = "Frequency" ) + theme_minimal ()


# f.
print("Scatter Plot: Credit Amount vs. Age (Colored by Creditability):" ) ggplot(loan_data, aes(x = Age.years, y = Credit.Amount, color = Creditability)) +
  geom_point (alpha = 0.7) +
  labs(title = "Scatter Plot: Credit Amount vs. Age (Colored by Creditability)" ,
       x = "Age (Years)" ,
       y = "Credit Amount" ) + theme_minimal () +
  scale_color_manual (values = c("Good" = "darkgreen" , "Bad" = "darkred" ))


#Q.16


birth_rate_data <- data.frame (
  Country = c("India", "Germany" , "China", "Pakistan" , "Sweden" ), Birth_Rate_per_thousand = c(33, 16, 40, 35, 15)
)
print(birth_rate_data)
max_birth_rate <- max(birth_rate_data$Birth_Rate_per_thousand) 
print(paste("Maximum Birth Rate:" , max_birth_rate)) 
min_birth_rate <- min(birth_rate_data$Birth_Rate_per_thousand) 
print(paste("Minimum Birth Rate:" , min_birth_rate))
sorted_by_birth_rate <-birth_rate_data[ order(birth_rate_data$Birth_Rate_per_thousand, decreasing = TRUE), ]
print("Countries sorted by Birth Rate (descending):" )

print(sorted_by_birth_rate) 


#Q.17


library(reshape2) library(ggplot2)

# 1.
cities_temp <- data.frame (
  City = c("Delhi", "Kolkata" , "Mumbai" , "Chennai" ),
  Maximum = c(40.5, 42.8, 37.8, 39.4),
  Minimum = c(34.7, 33.5, 32.2, 33.1)
)

# 2.
cities_temp_long <- melt(cities_temp, id.vars = "City", variable.name = "Temperature_Type" , value.name = "Temperature" )

# 3.
ggplot(cities_temp_long, aes(x = City, y = Temperature, fill = Temperature_Type)) +
  geom_bar (stat = "identity" , position = "dodge", color = "black") + # 'position = "dodge"' creates grouped bars
  labs(title = "Maximum and Minimum Temperatures of Metropolitan Cities" , x = "City",
       y = "Temperature in C elsius (°C)" ,
       fill = "Temperature Type" ) + # Legend title for fill
  scale_fill_manual (values = c("Maximum" = "firebrick" , "Minimum" = "steelblue" )) +
  theme_minimal () +
  theme(plot.title = element_text (hjust = 0.5, face = "bold"))

#Q.18
marks_lower <- c(0, 10, 20, 30, 40)
marks_upper <- c(10, 20, 30, 40, 50)
midpoints <- (marks_lower + marks_upper) / 2
no_of_students <- c(6, 12, 25, 16, 11)
midpoints_ext <- c(midpoints[1] - 10, midpoints, midpoints[length(midpoints)]
                   + 10)
frequencies_ext <- c(0, no_of_students, 0)
plot(midpoints_ext, frequencies_ext, type = "l",
     xlab = "Marks", ylab = "No. of Students",
     main = "Frequency Polygon of Marks",
     col = "blue", lwd = 2,
     ylim = c(0, max(no_of_students) + 5))
points(midpoints, no_of_students, col = "red", pch = 19) 



#Q.19

years <- c("2000-2001", "2001-2002", "2002-2003")
arts <- c(20, 25, 30)
science <- c(10, 9, 20)
law <- c(5, 10, 20)
data_matrix <- rbind(arts, science, law)
colnames(data_matrix) <- years
barplot(data_matrix,
        main = "Number of Students in University 'X' by Faculty and Year",
        xlab = "Year", ylab = "Number of Students (in thousands)",
        col = c("lightblue", "lightgreen", "lightcoral"),
        legend = rownames(data_matrix),
        args.legend = list(x = "topleft"))

#Q.20


items <- c("Food", "Clothing", "Rent", "Fuel and Lighting", "Education",
           "Miscellaneous")
expenditure <- c(240, 66, 125, 57, 42, 190)
percentage <- round(expenditure / sum(expenditure) * 100, 1)
labels <- paste(items, " (", percentage, "%)", sep = "")
pie(expenditure,
    labels = labels,
    main = "Average Daily Expenditure of a Family",
    col = rainbow(length(expenditure)))

#Q.21
s1 <- c(37, 49, 7, 38)
s2 <- c(16, 37, 21, 42, 27, 40, 39, 51)
sp <- s1 + s2 
sp
sn <- (s1 + s2) / 2 
sn
sd <- s1 / s2 
sd
sm <- s1 * s2
sm


#Q.22
a <- seq(from = 1, to = 37, by = 3)
b <- seq(from = 1, to = 13, by = 1) 
no_of_observations_a <- length(a)

no_of_observations_b <- length(b) 
c <- a * b
c
d <- a / b 
d 
e <- a + b
e
f <- a - b
f



#Q.23
y <- c(40, 67, 75, 48, 44, 53, 56, 43, 66, 57, 65, 52, 83, 83, 80, 76, 85,
       
       88, 89, 87)
mean_y <- mean(y)
median_y <- median(y) 
get_mode <- function (v) 
  { uniqv <- unique(v)
uniqv[ which.max (tabulate (match(v, uniqv)))]
}
mode_y <- get_mode (y)
mode_y
log10_y <- log10(y) 
log10_y
log_y <- log(y)
log_y
num_observations_y <- length(y) 
num_observations_y
sorted_y <- sort(y) 
sorted_y
summary_y <- summary(y) 
summary_y
range_y <- range(y) 
range_y
sd_y <- sd(y) 
sd_y



#Q.24


A <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
B <- matrix(c(2, 8, 6, 12), nrow = 2, byrow = TRUE)
print(A)
print(B)
A_plus_B <- A + B 
print(A_plus_B) 
B_minus_A <- B - A 
print(B_minus_A)
AB <- A %*% B
print(AB) 
det_A <- det(A) 
print(det_A) 
det_B <- det(B)
print(det_B)
if (det_A != 0) { inverse_A <- solve(A) 
print("Inverse of A:" ) 
print(inverse_A)
} else {
  print("Matrix A is singular, inverse does not exist." )
}
if (det_B != 0) { inverse_B <- solve(B) 
print("Inverse of B:" ) 
print(inverse_B)
} else {
  print("Matrix B is singular, inverse does not exist." )
  
}
A_prime <- t(A) 
print(A_prime) 
AB_prime <- A %*% t(B)
print(AB_prime)
AB_transpose <- t(A %*% B) 
print(AB_transpose)
A_prime_B_prime <- t(A) %*% t(B) 
print(A_prime_B_prime)

#Q.25
A_q25 <- matrix(c(1, 3, 5, 8, 5, 5, 2, 3, 1), nrow = 3, byrow = TRUE)
B_q25 <- matrix(c(2, 3, 2, 6, 5, 1, 3, 2, 1), nrow = 3, byrow = TRUE) 
print(A_q25)
print(B_q25) 
# A + B
A_plus_B_q25 <- A_q25 + B_q25 
print(A_plus_B_q25)
# B - A
B_minus_A_q25 <- B_q25 - A_q25 
print(B_minus_A_q25)
# AB (Matrix multiplication) 
AB_q25 <- A_q25 %*% B_q25 
print(AB_q25)
# |A| (Determinant of A)
det_A_q25 <- det(A_q25) 
print(det_A_q25)
# |B| (Determinant of B)
det_B_q25 <- det(B_q25) 
print(det_B_q25)
# Inverse of A
if (det_A_q25 != 0) { inverse_A_q25 <- solve(A_q25) 
print("Inverse of A (Q.25):" )
print(inverse_A_q25)
} else {
  print("Matrix A (Q.25) is singular, inverse does not exist." )
}
# Inverse of B
if (det_B_q25 != 0) { inverse_B_q25 <- solve(B_q25) 
print("Inverse of B (Q.25):" ) 
print(inverse_B_q25)
} else {
  print("Matrix B (Q.25) is singular, inverse does not exist." )
}
# A' (Transpose of A)
A_prime_q25 <- t(A_q25) 
print(A_prime_q25)
# AB' (A multiplied by Transpose of B)
AB_prime_q25 <- A_q25 %*% t(B_q25) 
print(AB_prime_q25)

# inverse(AB)' (Inverse of AB then Transpose)
# First calculate AB
AB_q25_for_inv <- A_q25 %*% B_q25
# Check if AB is invertible
if (det(AB_q25_for_inv) != 0) {
  inverse_AB_q25 <- solve(AB_q25_for_inv)
  inverse_AB_prime_q25 <- t(inverse_AB_q25)
  print("Inverse of AB then Transpose (inverse(AB)') (Q.25):")
  print(inverse_AB_prime_q25)
} else {
  print("Matrix AB (Q.25) is singular, inverse(AB) does not exist.")
}
# A'B' (Transpose of A multiplied by Transpose of B)
A_prime_B_prime_q25 <- t(A_q25) %*% t(B_q25)
print("A'B' (Q.25):")
print(A_prime_B_prime_q25)


#Q.26


Id_no <- 1:10
Age_of_husband_yrs <- c(28, 27, 26, 32, 30, 29, 35, 30, 32, 40)
Age_of_wife_yrs <- c(24, 24, 20, 28, 25, 26, 32, 26, 30, 35)

df_ages <- data.frame (Id_no, Age_of_husband_yrs, Age_of_wife_yrs) 
print(df_ages)
n <- df_ages $Id_no
# Assuming 'Age of son' data is not provided and needs to be a placeholder or calculated later.
# For this example, l et's create a placeholder vector for z1.
z1 <- c(5, 6, 4, 8, 7, 6, 9, 7, 8, 10) # Example placeholder values for 'Age of son'

c <- data.frame (n = n, z1 = z1) 
print(c)
# Install and load dplyr if not already installed 
# install.packages("dplyr")
library(dplyr)
c_renamed <- c %>% 
  rename(z = z1)
print(c_renamed)
c_no_z <- c_renamed %>% 
  select(-z)
print(c_no_z)
# Assuming 'x' and 'y' are columns in df_ages for demonstration purposes. 
# For example, let's add dummy 'x' and 'y' columns to df_ages for this step. 
df_ages_with_xy <- df_ages %>%
mutate(x = rnorm(10), y = runif(10)) 
# Adding dummy x and y variables

df_ages_no_xy <- df_ages_with_xy %>% 
  select(-x, -y)
print(df_ages_no_xy) 

#Q.27


Id_no <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Age_of_son_yrs <- c(5, 3, 2, 6, 2, 3, 7, 4, 5, 7)
c <- data.frame (n = Id_no, z1 = Age_of_son_yrs)
print(c)
colnames (c)[ colnames (c) == "z1"] <- "z" 
print("Data frame 'c' after renaming 'z1' to 'z':" ) 
print(c)


#Q.28
Height <- c(140, 137, 150, 147, 139, 140, 150, 132, 138, 140)
Weight <- c(55, 57, 59, 62, 61, 60, 60, 58, 59, 57)
d <- data.frame (h = Height, w = Weight) 
print(d)
boys_height_gt_145 <- d$h[d$h > 145]
print("Vector of boys with height > 145:" ) 
print(boys_height_gt_145) 
boys_weight_gt_55 <- d$w[d$w > 55] 
print("Vector of boys with weight > 55:" )
print(boys_weight_gt_55)
boys_filtered_hw <- d[d$h > 140 & d$w > 60, ]
print("Data frame of boys with height > 140 and weight > 60:" )
print(boys_filtered_hw)
