##############################################################################
# File-Name: week1_introduction_to_r.r
# Date: January 22, 2024
# author: Tiago Ventura
# course: PPOL 6801 - text as data
# topics: Basic Introduction to R
# Machine: MacOS High Sierra
##############################################################################

## Acknowledgments: 

### As most materials from this course, this code combines my own code +
### materials I borrowed from other colleagues. 

###This code in particular takes  a lot from R Session materials from the TaD Class from Arthur Spirling, 
### Sessions prepare by TAS: 
### Elisa Wirsching, Lucia Motolinia, Pedro L. Rodriguez, Kevin Munger, 
### Patrick Chester and Leslie Huang.

# ============================================================================= #
# What is R? -----                             
# ============================================================================= #

## R is a versatile, open source programming/scripting language that's: 

## >>> Python for statistical analysis and visualization
## <<< Python for Machine Learning Tasks


## For those familiar with Python, R is a functional programming language. 
## This means most things in R work around uniquely defined functions + objects, 
## instead of classes, objects and methods as in Python. 


# ============================================================================= #
# How to Work with R -----
# ============================================================================= #

## RStudio is the premier R graphical user interface (GUI) and 
## integrated development environment (IDE) that makes R easier to use.

## In Rstudio, you can work with: 

### Scripts
### Command line
### Notebooks

## In Rstudio, you can visualize: 

## Script
## Command Line
## Environment
## Outputs


## Should I use scripts or notebooks? 

### Use notebooks for nice and small pieces of code, in which you want to add text
### Use scripts for more complex tasks,and longer projects. 

## Comments.

### use # signs to add comments within your code chunks.
### Help yourself in the future and make as much comments as you can in your code!

## Running code

### You run code using command/control + enter. 
### You can either run code line by line or in selected parts

plot(density(rnorm(100)))

## Errors

### When the text is a legitimate error, it will be prefaced with “Error:”,
### and R will try to explain what went wrong.

plot("hello")


# ============================================================================= #
# Working with R  -----
# ============================================================================= #

## Managing your environment ----

## Clearing environment
rm(list = ls())

## Working directory

## you can get and set the working directory manually (this is pretty bad coding actually)

### check where you are
getwd() 

### redefine 
setwd()

# Tip: use the Rproject feature. It will define your working directory for you. 

## R packages ----

## Packages are a combination of functions that someone developed for you. 
## You can install them either from the official R repository (CRAN)

install.packages("wesanderson")

## Or from a development version available on github

devtools::install_github("karthik/wesanderson")

## Install and Activate a package. 

### You install a package only once. 
### But you need to activate the package every time you want to use it. 

library(wesanderson)
library(devtools) # notice here I am calling devtools. 
                  # The :: was a way to access functions without calling a package

## Better Practice Pacman - https://github.com/trinker/pacman

#  install.packages("pacman")

# load all at once
pacman::p_load(tidyverse,
               haven,
               ggplot2,
               xtable,
               devtools, 
               janitor) 

# It is also pretty bad practice to load many packages (conflicts!!!); 
# don't do it, keep things clean

# Also, you don't have to load all packages that you want to use, you can always
# use the :: to load a package in a particular instance (see next line)


# ============================================================================= #
#   Working with Data in R   ------
# ============================================================================= #


# Loading data

polling_data  <- read_csv("data/national_clinton_trump_6_20_2016.csv")

# the readr + haven has many other read_* functions. 
??read_delim
??read_dta


# Take a peek, get to know the structure of the data

head(polling_data)  # display first lines of an object
head(polling_data, n = 10)  # same as above but specifying number of lines 
tail(polling_data)  # display last lines of an object
dim(polling_data)  # data dimensions
nrow(polling_data)  # number of rows
ncol(polling_data)  # number of columns
colnames(polling_data)  # column names
names(polling_data)  # also column names (more general command)
rownames(polling_data) # row names
class(polling_data)  # returns class of an R object
sapply(polling_data, class) # returns class for each variable (column)
str(polling_data)  # display structure of an R object (e.g. a dataframe)
glimpse(polling_data)
?sapply  # get R Documentation on this command (see Help panel below)

## Modyfing and saving dataframes

polling_data <- clean_names(polling_data)

## Manipulating dataframes --- 

# Get column with dollar sign operator
head(polling_data$pollster)

# Matrix identifier: df[rowname, colname]
polling_data[, "pollster"]

# That was pretty impossible to read in the console, let's try this:
view(polling_data[, c("pollster", "number_of_observations")])
View(polling_data[polling_data$pollster == "CBS", c("pollster", "number_of_observations")])


## Tidyverse

## `Tidyverse` is a family of R packages designed for working with dataframes.
## These packages share the same underlying design, philosophy,
## grammar and data structures.
## The purpose of `tidyverse` is to provide an integrated set of tools 
## for using R as a language in data science. 

## These are the main packages of `tidyverse`:
  
### - `dplyr`: for data manipulation.

### - `ggplot2`: for data visualization.

### - `tidyr`: to prepare your data for analysis.

### - `purrr`: to optimize your code and for functional programming.

### - `readr`: to open and organize the data.

### - `stringr`: for manipulating text objects.

### - `forcats`: for manipulation of the class factors.

### Tidyverse also is built under the idea of working with pipes in R


# Base R: From the inside out
x <- c(1:10)
round(exp(sqrt(mean (x))), 1)

### The Pipe
x%>%
  mean()%>%
  sqrt()%>%
  exp()%>%
  round(1)

# Using pipe notation with dataframe
polling_data %>% 
  select(pollster) %>% 
  head(.,10)

polling_data %>% 
  select(pollster, number_of_observations) %>% 
  head()

## How to locate row(s) in a data frame 

# Dollar sign operator
polling_data$number_of_observations[1] # Returns the first row of the data frame in the specified column (Pytho users: R indexing starts at 1)
polling_data$number_of_observations[1:5] # Returns the first 5 rows of the data frame in the specified column
polling_data$number_of_observations[polling_data$pollster == "Quinnipiac"] # Returns all rows for the variable "number_of_observations" where pollster = Quinnipiac

# Column name
polling_data[1, "number_of_observations"] 
polling_data[1:5, "number_of_observations"] 
polling_data[polling_data$pollster == "Quinnipiac","number_of_observations"] 

# Pipe syntax
polling_data %>% 
  slice(1) %>% 
  select(number_of_observations) 

polling_data %>% 
  slice(1:5) %>% 
  select(number_of_observations)

polling_data %>% 
  filter(pollster == "Quinnipiac") %>% 
  select(number_of_observations)

polling_data %>% 
  filter(pollster == "Quinnipiac") %>% 
  slice(1) %>% 
  select(number_of_observations)  # can keep "piping"

polling_data %>% 
  slice(1) %>% 
  filter(pollster == "Quinnipiac") %>% 
  select(number_of_observations)  # BUT note order can matter


# Creating new variables (columns) in a data frame 

# Dollar sign operator
polling_data$net_clinton_a <- polling_data$clinton - polling_data$trump


# Matrix identifier
polling_data[, "net_clinton_b"]  <- polling_data[, "clinton"] - polling_data[, "trump"]

# Pipe syntax + tidyverse
polling_data <- polling_data %>% 
  mutate(net_clinton_c = clinton - trump)

# Summarizing Data 

# General summary
summary(polling_data)  # summary statistics where appropriate (non-string/character variables)

# Single variable summary
mean(polling_data$net_clinton_a, na.rm=T)
sd(polling_data$net_clinton_a)

# pipe + tidyvers
polling_data %>% 
  summarise(mean_net_clinton = mean(net_clinton_a))  

polling_data %>% 
  filter(population == "Registered Voters") %>% 
  summarise(mean_net_clinton = mean(net_clinton_a))  # summary for a specific group

# Summary by group
polling_data %>% 
  group_by(pollster) %>% 
  summarise(mean_net_clinton = mean(net_clinton_a))  # use group_by

polling_data %>% 
  group_by(pollster) %>% 
  summarise(mean_net_clinton = mean(net_clinton_a), sd_net_clinton = sd(net_clinton_a))  # can perform multiple summary stats

table1 <- polling_data %>% 
  group_by(pollster, population) %>% 
  summarise(mean_net_clinton = mean(net_clinton_a)) %>% 
  ungroup %>% 
  arrange(pollster, Population) # can group by more than one variable


# Create a new variable by group
polling_data %>% 
  group_by(pollster, population) %>% 
  ## see the difference here
  mutate(mean_net_clinton = mean(net_clinton_a))


# Summarizing a variable with a histogram

# Basic R graphics
hist(polling_data$net_clinton_a)

# ggplot2 graphics
plot1 <- ggplot(aes(net_clinton_a), data = polling_data) + 
  geom_histogram(bins = 15) + 
  theme_light()

plot1



# ============================================================================= #
# Scaling up your code: Loops, functions and apply  -----
# ============================================================================= #

# For Loops: when you need to iterate through a sequence ----

for(x in names(polling_data)){ 
  # A loop that identifies and stores variables that contain characters
  if(is.character(polling_data[, x])) {
    print(x)
  }
}

## What is the loop doing?

### **`for(item in group_of_items)`: the sequence**
  
    ##### - item: index for each repetition.

     #### - group_of_items:  object we are drawing the item from

### **Parenthesis {}**
  
  ### - What it is to be repeated goes here. 

  
### **Containers**
  
  #### - Created outside of the loop. 

  #### - Object to save all your outputs. 


### Another example
mean_n = list()

for (i in unique(polling_data$pollster)){
  
mean_n[[i]] = polling_data %>%
  filter(pollster==i) %>%
  summarize(m=mean(`number-of-observations`, na.rm=TRUE))

}

# build a dataframe
data_frame(names=names(mean_trump), 
           values=unlist(mean_trump))


# User written functions ----

# Calculates the cosine similarity between two vectors
calculate_cosine_similarity <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  # %*% specifies dot product rather than entry by entry multiplication (we could also do: sum(x * y))
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  return(nominator/denominator)
}

set.seed(1984L)  # allows us to replicate result
x  <- rnorm(10) # Creates a vector of random normally distributed numbers
y  <- x*2 + 3

calculate_cosine_similarity(x,y)

# Python users: R cannot return multiple values from a function -- you will have to return a list of the values you want to return. 

calculate_distance <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  # %*% specifies dot product rather than entry by entry multiplication (we could also do: sum(x * y))
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  cos_dist <- nominator/denominator
  euc_dist <- sqrt(sum((vec1 - vec2)^2))
  return(list(cosine = cos_dist, euclidean = euc_dist))
}

calculate_distance(x,y)
dist_comp <- calculate_distance(x,y)  # we can store this result
dist_comp[["cosine"]]
dist_comp$cosine
dist_comp[[1]]


## Apply a function to multiple objects ----

### There are three  different ways we can scale up our functions. 

# - Loops

# - apply functions

# - `purr::map` 

### Let's see a bit more about how map works (more flexible and allows piping)

### Let's see an example. We will go through three steps. 

#### - Create ten dataframes and save as a .csv

#### - Open all of them with a loop. 

#### - Then open all of them with map. 


# create a dir
dir.create("data/fake_data")

# loop to create objects
for(i in 1:10){
d <- tibble(n=1000, 
            norm=rnorm(n, 0, 1), 
            unif=runif(n, 0,1))
write.csv(d, paste0("data/fake_data/data", i,".csv"))
}

## Open with a loop

# create container
data <- list()

# iterator
path=paste0("data/fake_data/", list.files("data/fake_data"))

# the loop
i=1
for(i in 1:length(path)){
data[[i]] <- read_csv(path[[i]])  
}

data[[1]]

## Using Map

# simple map = output is a list
l <- map(path, read.csv)

# and some extensions
l <- map_dfr(path, read.csv)

# Check
glimpse(l)

