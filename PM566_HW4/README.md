Assignment 4
================
Caroline He
11/16/2021

## R packages

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(parallel)
```

# HPC

## Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}
# rewrite 
fun1alt <- function(mat) {
  ans <- rowSums(mat) 
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}
# rewrite
fun2alt <- function(mat) {
  ans <- t(apply(mat,1,cumsum))
  ans
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min      lq     mean   median       uq       max neval
    ##     fun1(dat) 10.90933 10.5385 7.317953 10.41034 10.11449 0.3693895   100
    ##  fun1alt(dat)  1.00000  1.0000 1.000000  1.00000  1.00000 1.0000000   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq      max neval
    ##     fun2(dat) 3.324107 2.511757 2.182136 2.374054 2.329569 0.938984   100
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100

## Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   2.953   0.819   3.809

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
system.time({
  cl <- makePSOCKcluster(4L)
  clusterSetRNGStream(cl, 1231) 
  clusterExport(cl, "sim_pi")
  ans <- unlist(parLapply(cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  stopCluster(cl)
  ans
})
```

    ## [1] 3.141578

    ##    user  system elapsed 
    ##   0.011   0.006   1.284

# SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

When you write a new chunk, remember to replace the r with sql,
connection=con. Some of these questions will reqruire you to use an
inner join. Read more about them here
<https://www.w3schools.com/sql/sql_join_inner.asp>

## Question 1

How many many movies is there avaliable in each rating catagory.

``` sql
SELECT rating,
  COUNT(*) AS count
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | count |
|:-------|------:|
| G      |   180 |
| NC-17  |   210 |
| PG     |   194 |
| PG-13  |   223 |
| R      |   195 |

5 records

</div>

# Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
SELECT
  rating,
  AVG(replacement_cost) AS avg_replacement_cost, 
  AVG(rental_rate) AS avg_rental_rate
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | avg\_replacement\_cost | avg\_rental\_rate |
|:-------|-----------------------:|------------------:|
| G      |               20.12333 |          2.912222 |
| NC-17  |               20.13762 |          2.970952 |
| PG     |               18.95907 |          3.051856 |
| PG-13  |               20.40256 |          3.034843 |
| R      |               20.23103 |          2.938718 |

5 records

</div>

# Question 3

Use table film\_category together with film to find the how many films
there are with each category ID

``` sql
SELECT 
COUNT(*) AS "number of films", 
category_id
FROM film
INNER JOIN film_category
ON film.film_id = film_category.film_id
GROUP BY category_id
```

<div class="knitsql-table">

| number of films | category\_id |
|----------------:|-------------:|
|              64 |            1 |
|              66 |            2 |
|              60 |            3 |
|              57 |            4 |
|              58 |            5 |
|              68 |            6 |
|              62 |            7 |
|              69 |            8 |
|              73 |            9 |
|              61 |           10 |

Displaying records 1 - 10

</div>

# Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT 
COUNT(*) as "number_of_films", 
name
FROM film
INNER JOIN film_category
ON film.film_id = film_category.film_id
INNER JOIN category
ON category.category_id = film_category.category_id 
GROUP BY name
ORDER BY number_of_films DESC
```

<div class="knitsql-table">

| number\_of\_films | name        |
|------------------:|:------------|
|                74 | Sports      |
|                73 | Foreign     |
|                69 | Family      |
|                68 | Documentary |
|                66 | Animation   |
|                64 | Action      |
|                63 | New         |
|                62 | Drama       |
|                61 | Sci-Fi      |
|                61 | Games       |

Displaying records 1 - 10

</div>
