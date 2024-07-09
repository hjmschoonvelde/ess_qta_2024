## Introduction

In order to analyze text, we will use the `R` statistical language, a
versatile tool for doing all sorts of analyses, whether it is
statistical analysis, text analysis, or otherwise. Today we will
practice using `R`. In this script, you will learn how to distinguish
between different types of objects in `R`, how to create objects, and
how to use functions to manipulate these objects.

First, have a look at the image below. It shows the interface of a
typical RStudio session. When typing in commands in R, you will use the
`Console` panel. The `Script` panel can contain files with pre-written
lines of code (for example in an R Markdown file), which you can
copy-paste into the `Console`. The `Environment` panel displays objects
that you created during your R session. The `output` panel displays
function outputs such as figures.

<figure>
<img src="rstudio_panels.png" style="width:65.0%"
alt="Graphical interface in RStudio" />
<figcaption aria-hidden="true">Graphical interface in
RStudio</figcaption>
</figure>

R can serve many different purposes. For instance, we can use R as a
calculator. Give it a try by typing the following commands in the
`Console`.

**NB**: in these documents, you have to type in the commands appearing
in the grey blocks (the blocks are so called `code snippets`). Within
these code snippets, the `#` symbol is used to denote comments. Comments
are not executed by `R`, but are there to help you understand what the
code does. You can run the code in a line by pressing `Ctrl + Enter` or
by clicking on the `Run` button in the `Console` panel.

``` r
#addition
2+2
```

    ## [1] 4

``` r
#exponentiation
3**2 
```

    ## [1] 9

``` r
#subtraction
4-5
```

    ## [1] -1

``` r
3+2
```

    ## [1] 5

``` r
3-2
```

    ## [1] 1

See, for example,
[here](http://mercury.webster.edu/aleshunas/R_learning_infrastructure/Using%20R%20as%20a%20calculator.html)
for more on `R` as a calculator.

You can also do conduct more advanced operations, such as taking the
mean of two numbers, or finding the median of three numbers. For this we
use the `mean()` function, and the `median()` function respectively. R
contains a lot of functions like this. Functions in `R` are denoted with
round brackets What’s in between the round brackets is the argument (or
the arguments) that the function is being applied to. When using a
function, you have to make sure that the arguments are of the right
type. For example, the `mean()` function requires a numerical argument.

``` r
#take the mean of 2 and 4

mean(2,4)
```

    ## [1] 2

``` r
#take the median (the middle value) of 2, 4 and 100

median(2, 4, 100)
```

    ## [1] 2

We can also create a so-called `vector` of numbers using the `c()`
function (which stands for concatenate). A vector is just a row of
observations. For example, `c(2,4)` is just a vector of numbers 2 and 4.
`c("A", "B", "F")` is a vector of three capitalized letters.
`c("John", "Paul", "George", "Ringo")` is a vector with the first names
of the Beatles. And so on.

``` r
#take the mean of 2 and 4
mean(c(2,4))
```

    ## [1] 3

``` r
#take the median (the middle value) of 2, 4 and 100
median(c(2, 4, 100))
```

    ## [1] 4

We can create objects in `R` using the assign operator (`<-`). For
example, let’s create an object `x` which in fact is a vector of the
numbers 2, 3 and 10:

``` r
x <- c(2,3,10)
```

This is useful, because we can now take the mean of `x`:

``` r
mean(x)
```

    ## [1] 5

Check the `Environment` pane. It now contains the object `x` and you can
use it for all kinds of operations.

We can also create an object called Beatles with the first names of the
Beatles members.

``` r
beatles <- c("John", "Paul", "George", "Ringo")

newspaper_articles <- c("text article 1", "text article 2", "text article 3", "text article 4")
```

What happens if we take the mean of the beatles object?

``` r
mean(beatles)
```

    ## [1] NA

The output is `NA`, which is `R` speak for a missing value, which makes
sense, because taking the mean requires numerical input. The mean of a
set of names does not exist.

**Question**: Write some code to find the mean of the numbers 4, 5, 6
and 7 and call it y. The calculate x + y. What is the output you get?
Why is this?

``` r
y <- c(4,5,6,7)
x+y
```

    ## [1]  6  8 16  9

``` r
#your answer here
```

Now let’s try to take the square root of x using the `squareroot()`
function.

``` r
#squareroot(x)
```

This will throw an error saying that the function `squareroot` is not
found. This is because the function is called `sqrt` in `R`.
squareroot() does not exist, unless we define it ourselves. Rather the
function is called sqrt().

``` r
sqrt(x)
```

    ## [1] 1.414214 1.732051 3.162278

Steps to decode the error message:

1.  Read the error carefully: The error message clearly states that the
    function “squareroot” could not be found. This is a hint that there
    might be a typo or a misunderstanding about the function’s name.

2.  check for typos: The first step should be to check for typos in the
    function name. R functions are case-sensitive and must be spelled
    exactly as defined in R or any packages you are using.

3.  Reference the documentation: If the spelling seems correct or if you
    are unsure about the function’s existence, reference the R
    documentation or look it up online. For R, you can use the help()
    function or ? followed by the function name to check if it’s a valid
    function, like so

``` r
?sqrt

help(sqrt)
```

## Packages

So far you have used some functions that are built-in to `R` (base R).
However, a lot of the functionality of `R` comes from using so-called
`packages`. These packages contain all kinds of functions and objects to
do particular analyses in `R`. They are under continuous development by
active users of `R` who use the language to do particular types of
analyses. By using these packages you can build on their work.

**NB**: it’s good practice to cite a package you have used for your data
analysis. If you don’t know how to cite a package, just type
citation(package = “here”) in the console, replacing “here” with name of
the package you have used.

Today we will use 3 packages: `quanteda`, `ggplot2` and `stringr`. These
packages will allow us to do various cool things in `R`

For example `quanteda` is a library that contains functions related to
quantitative text analysis. In order to use the functions in these
libraries, you will need to 1) install them, using the
`install.packages()` function, and 2) load them using the `library()`
function.

Let’s install them first (you only need to do this once, after that you
can load the library using the `library()` function. In order to use the
`install.packages()` function, you need to remove the `#` symbol in
front of the function.

``` r
#install.packages(quanteda)
#install.packages(stringr)
#install.packages(ggplot2)
```

``` r
library(quanteda)
library(stringr)
library(ggplot2)
```

## Text as strings

So what this have to do with quantitative text analysis?

Well, text can be read into `R` as a character object or character
vector, which we can examine using various functions available to us in
the various libraries.

But before we get to that, let’s keep things simple first. Let’s say we
have a `vector` with European Commissioners. Let’s call this string
`commissioners`.

``` r
# Define the vector with the names of European Commissioners


commissioners <- c(
  "Ursula von der Leyen", "Wopke Hoekstra", "Margrethe Vestager", 
  "Valdis Dombrovskis", "Josep Borrell", "Stella Kyriakides", 
  "Didier Reynders","Jutta Urpilainen", "Elisa Ferreira", "Thierry Breton"
)

# Print the vector to confirm its contents
print(commissioners)
```

    ##  [1] "Ursula von der Leyen" "Wopke Hoekstra"       "Margrethe Vestager"  
    ##  [4] "Valdis Dombrovskis"   "Josep Borrell"        "Stella Kyriakides"   
    ##  [7] "Didier Reynders"      "Jutta Urpilainen"     "Elisa Ferreira"      
    ## [10] "Thierry Breton"

To access individual Commissioners in this `commissioners` vector, we
can use the square brackets. For example, if we type in
`commissioners[1]`, `R` will return the content of first element of
`commissioners`, if we type in `comissioners[5],`R\` will return the 5th
element of commissioners. Just try it out.

``` r
commissioners[1]
```

    ## [1] "Ursula von der Leyen"

``` r
commissioners[5]
```

    ## [1] "Josep Borrell"

Data in `R` can be of different `types` (`numeric`, `logical`,
`character`, and `factor`). For example, a vector of numbers is an
object of type `numeric`, and a vector of words like commissioners is of
type `character` (it is a so-called character string), and we can also
have logical vectors in which element is either `TRUE` or `FALSE`.

Depending on the type of object you are dealing with, you can do
different things in R. For example, if you want to multiply elements in
a character string `R` will throw you an error, simply because, say, Von
der Leyen times Hoekstra is a meaningless statement, whereas 2^3 (see
above) is 8. We can check the type of an object using the class()
function:

``` r
class(commissioners)
```

    ## [1] "character"

When data is of type character, we can use the `stringr()` library for
some useful functions. For example, in case we want to have the party
names in all caps, we can use the `str_to_upper()` function from the
`stringr` library

``` r
str_to_upper(commissioners)
```

    ##  [1] "URSULA VON DER LEYEN" "WOPKE HOEKSTRA"       "MARGRETHE VESTAGER"  
    ##  [4] "VALDIS DOMBROVSKIS"   "JOSEP BORRELL"        "STELLA KYRIAKIDES"   
    ##  [7] "DIDIER REYNDERS"      "JUTTA URPILAINEN"     "ELISA FERREIRA"      
    ## [10] "THIERRY BRETON"

``` r
str_to_lower(commissioners)
```

    ##  [1] "ursula von der leyen" "wopke hoekstra"       "margrethe vestager"  
    ##  [4] "valdis dombrovskis"   "josep borrell"        "stella kyriakides"   
    ##  [7] "didier reynders"      "jutta urpilainen"     "elisa ferreira"      
    ## [10] "thierry breton"

**Question**: how would you create a vector with all string characters
lower cased? If you don’t know, google `stringr` and `lowercasing`.
Figure out want function you would need to use.

``` r
#your answer here
```

We can also count the length of the vector elements:

``` r
str_count(commissioners)
```

    ##  [1] 20 14 18 18 13 17 15 16 14 14

**Question**: Ursula Von der Leyen has 17 letters but the `str_count`
function returns 20 Why is that?

We order the elements of a string vector using the `str_sort()`:

``` r
str_sort(commissioners, decreasing = TRUE)
```

    ##  [1] "Wopke Hoekstra"       "Valdis Dombrovskis"   "Ursula von der Leyen"
    ##  [4] "Thierry Breton"       "Stella Kyriakides"    "Margrethe Vestager"  
    ##  [7] "Jutta Urpilainen"     "Josep Borrell"        "Elisa Ferreira"      
    ## [10] "Didier Reynders"

``` r
str_sort(commissioners, decreasing = FALSE)
```

    ##  [1] "Didier Reynders"      "Elisa Ferreira"       "Josep Borrell"       
    ##  [4] "Jutta Urpilainen"     "Margrethe Vestager"   "Stella Kyriakides"   
    ##  [7] "Thierry Breton"       "Ursula von der Leyen" "Valdis Dombrovskis"  
    ## [10] "Wopke Hoekstra"

We can also detect the presence or absence of a pattern in a string.
This returns a logical vector for which elements are TRUE when a pattern
is detected, and false when a pattern is not detected. For example, we
can ask `R` which commissionsers have an `ll` in them:

``` r
str_detect(commissioners, "ll")
```

    ##  [1] FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE

**Question**: Write some code to detect whether the sequence `mm` in
which of these party names the sequence `rr` occurs.

``` r
#your answer here
```

**Bonus question**: Use the `str_replace()` function to replace the
sequence `l` with `xx`. The syntax of this function is slightly
different from these earlier functions. Use Google to try and figure out
how it works.

``` r
#your answer here
```

## Dataframes

A different object in `R` is a so called dataframe. Dataframes are data
displayed in tabular format. Dataframes can have different types of data
inside them While the first column can be character, the second and
third can be numeric or logical. This is different from other objects,
such as a `matrix` which can only contain one particular object type.

In order to create a dataframe we can use the \`data.frame()’ function.

Let’s the age of these commissioners. We’ll put this data in a numerical
variable called polls

``` r
age <- c(65, 48, 56, 52, 77, 68, 65, 48, 68, 69)
```

We can now use the data.frame() to create a dataframe, which we’ll call
data. This dataframe has two variables: the name of the commissioner and
their age.

``` r
data <- data.frame(name_commissioner = commissioners, 
                   age_commissioner = age)
print(data)
```

    ##       name_commissioner age_commissioner
    ## 1  Ursula von der Leyen               65
    ## 2        Wopke Hoekstra               48
    ## 3    Margrethe Vestager               56
    ## 4    Valdis Dombrovskis               52
    ## 5         Josep Borrell               77
    ## 6     Stella Kyriakides               68
    ## 7       Didier Reynders               65
    ## 8      Jutta Urpilainen               48
    ## 9        Elisa Ferreira               68
    ## 10       Thierry Breton               69

Now that we have the data in this format, we can plot it. We’ll use the
`ggplot2()` library to create a plot. Since we already loaded it in
memory we can use its functions. Let’s create a bar plot using the
`geom_bar()` function from ggplot.

NB: No worries if you don’t yet understand this syntax – this is a
practice exercise

``` r
plot <- ggplot(data = data, 
               aes(x = name_commissioner, y = age_commissioner)) +
  geom_bar(stat= "identity")

print(plot)
```

<img src="Lab_Session_QTA_1_Answers_files/figure-markdown_github/unnamed-chunk-26-1.png" width="\textwidth" />

Let’s make this plot a bit nicer by adding a theme

``` r
plot <- ggplot(data = data, aes(x = name_commissioner, 
                                y = age_commissioner)) +
  geom_bar(stat= "identity") +
  theme_minimal() 
print(plot)
```

<img src="Lab_Session_QTA_1_Answers_files/figure-markdown_github/unnamed-chunk-27-1.png" width="\textwidth" />

We’ll make this plot even nicer by rotating the x-axis labels.

``` r
plot <- ggplot(data = data, aes(x = name_commissioner, 
                                y = age_commissioner)) +
  geom_bar(stat= "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)
```

<img src="Lab_Session_QTA_1_Answers_files/figure-markdown_github/unnamed-chunk-28-1.png" width="\textwidth" />
Now we will order the commissioners by age. We can do this by using the
`reorder()` function.

``` r
plot <- ggplot(data = data, aes(x = reorder(name_commissioner, age_commissioner), 
                                y = age_commissioner)) +
  geom_bar(stat= "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)
```

<img src="Lab_Session_QTA_1_Answers_files/figure-markdown_github/unnamed-chunk-29-1.png" width="\textwidth" />

**Question** How would you make this plot nicer? What aspects would you
change?

## Practice excercises

1.  Create a character vector of countries (in the same order as
    `commissioners`) and call this vector `countries`. Append this
    variable to the `data` object as a new variable

``` r
commissioners <- c(
  "Ursula von der Leyen", "Wopke Hoekstra", "Margrethe Vestager", 
  "Valdis Dombrovskis", "Josep Borrell", "Stella Kyriakides", 
  "Didier Reynders","Jutta Urpilainen", "Elisa Ferreira", "Thierry Breton"
)

countries <- c("Germany", "Netherlands", "Denmark", "Latvia", "Spain", "Greece", "Belgium", "Finland", "Portugal", "France")
  
data <- cbind(data, countries)


gdp <- c(45000, 50000, 60000, 30000, 35000, 25000, 40000, 45000, 20000, 50000)

data <- cbind(data, gdp)

head(data)
```

    ##      name_commissioner age_commissioner   countries   gdp
    ## 1 Ursula von der Leyen               65     Germany 45000
    ## 2       Wopke Hoekstra               48 Netherlands 50000
    ## 3   Margrethe Vestager               56     Denmark 60000
    ## 4   Valdis Dombrovskis               52      Latvia 30000
    ## 5        Josep Borrell               77       Spain 35000
    ## 6    Stella Kyriakides               68      Greece 25000

1.  Look up some data on these countries that you find interesting.
    Perhaps their GDP per capita or something else. Plot this
    information using the ggplot code above.

``` r
plot <- ggplot(data = data, aes(x = reorder(countries, gdp), 
                                y = gdp)) +
  geom_bar(stat= "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)
```

<img src="Lab_Session_QTA_1_Answers_files/figure-markdown_github/unnamed-chunk-31-1.png" width="\textwidth" />

Now let’s save this in the working directory. We can do this using the
`ggsave()` function.

``` r
ggsave("gdp_plot.png", plot)
```
