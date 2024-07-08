The goal of today’s lab session is to inspect the functionality of the
**udpipe** library. **UDPipe** (Wijffels, 2022) offers
‘language-agnostic tokenization, tagging, lemmatization and dependency
parsing of raw text’. We will focus on tagging and lemmatization in
particular and how these pre-processing steps may make further analysis
more precise. Lemmatizing generally works better than stemming,
especially for inflected languages such as German or French.
Part-of-Speech (POS) tags identify the type of word (noun, verb, etc) so
it can be used to e.g. analyse only the verbs (actions) or adjectives
and adverbs (descriptions).

Another library that was developed by the quanteda team and that has
similar functionality is **spacyr** (Benoit & Matsuo, 2020), an R
wrapper around the spaCy package in Python. See this
[link](https://spacyr.quanteda.io/articles/using_spacyr.html) for more
information on using **spacyr**.

Let’s load required libraries first.

``` r
#load libraries
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.sentiment)
library(seededlda)
library(udpipe)
```

The primary challenges for our purposes is to communicate between
**udpipe** and **quanteda**. In the following code block we first turn
our corpus into a dataframe called `inaugural_speeches_df` and save the
speeches – which are now stored in `inaugural_speeches_df$text` – as a
character vector called txt. **udpipe** works with character vectors.

``` r
inaugural_speeches <- data_corpus_inaugural

inaugural_speeches_df <- convert(inaugural_speeches,
                                 to = "data.frame")

txt <- inaugural_speeches_df$text
str(txt)
```

    ##  chr [1:59] "Fellow-Citizens of the Senate and of the House of Representatives:\n\nAmong the vicissitudes incident to life n"| __truncated__ ...

Let’s apply the `udpipe` function to this `txt`. This function tags each
token in each speech, based on an English-language model which will be
downloaded into the working directory. We instruct `udpipe` to include
the doc_ids from our **quanteda** corpus object. This will help us later
on when we want to transform the output of our **udpipe** workflow back
into a corpus which we can inspect with **quanteda** functions:

``` r
parsed_tokens <-  udpipe(txt, "english", 
                         doc_id = inaugural_speeches_df$doc_id) %>% 
  as_tibble()
```

Let’s inspect this object

``` r
head(parsed_tokens)
```

    ## # A tibble: 6 × 17
    ##   doc_id          paragraph_id sentence_id sentence                     start   end term_id token_id token lemma upos  xpos  feats head_token_id dep_rel deps  misc 
    ##   <chr>                  <int>       <int> <chr>                        <int> <int>   <int> <chr>    <chr> <chr> <chr> <chr> <chr> <chr>         <chr>   <chr> <chr>
    ## 1 1789-Washington            1           1 Fellow-Citizens of the Sena…     1     6       1 1        Fell… fell… ADJ   JJ    Degr… 3             amod    <NA>  Spac…
    ## 2 1789-Washington            1           1 Fellow-Citizens of the Sena…     7     7       2 2        -     -     PUNCT HYPH  <NA>  3             punct   <NA>  Spac…
    ## 3 1789-Washington            1           1 Fellow-Citizens of the Sena…     8    15       3 3        Citi… citi… NOUN  NNS   Numb… 0             root    <NA>  <NA> 
    ## 4 1789-Washington            1           1 Fellow-Citizens of the Sena…    17    18       4 4        of    of    ADP   IN    <NA>  6             case    <NA>  <NA> 
    ## 5 1789-Washington            1           1 Fellow-Citizens of the Sena…    20    22       5 5        the   the   DET   DT    Defi… 6             det     <NA>  <NA> 
    ## 6 1789-Washington            1           1 Fellow-Citizens of the Sena…    24    29       6 6        Sena… Sena… PROPN NNP   Numb… 3             nmod    <NA>  <NA>

``` r
str(parsed_tokens)
```

    ## tibble [152,420 × 17] (S3: tbl_df/tbl/data.frame)
    ##  $ doc_id       : chr [1:152420] "1789-Washington" "1789-Washington" "1789-Washington" "1789-Washington" ...
    ##  $ paragraph_id : int [1:152420] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ sentence_id  : int [1:152420] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ sentence     : chr [1:152420] "Fellow-Citizens of the Senate and of the House of Representatives:" "Fellow-Citizens of the Senate and of the House of Representatives:" "Fellow-Citizens of the Senate and of the House of Representatives:" "Fellow-Citizens of the Senate and of the House of Representatives:" ...
    ##  $ start        : int [1:152420] 1 7 8 17 20 24 31 35 38 42 ...
    ##  $ end          : int [1:152420] 6 7 15 18 22 29 33 36 40 46 ...
    ##  $ term_id      : int [1:152420] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ token_id     : chr [1:152420] "1" "2" "3" "4" ...
    ##  $ token        : chr [1:152420] "Fellow" "-" "Citizens" "of" ...
    ##  $ lemma        : chr [1:152420] "fellow" "-" "citizen" "of" ...
    ##  $ upos         : chr [1:152420] "ADJ" "PUNCT" "NOUN" "ADP" ...
    ##  $ xpos         : chr [1:152420] "JJ" "HYPH" "NNS" "IN" ...
    ##  $ feats        : chr [1:152420] "Degree=Pos" NA "Number=Plur" NA ...
    ##  $ head_token_id: chr [1:152420] "3" "3" "0" "6" ...
    ##  $ dep_rel      : chr [1:152420] "amod" "punct" "root" "case" ...
    ##  $ deps         : chr [1:152420] NA NA NA NA ...
    ##  $ misc         : chr [1:152420] "SpaceAfter=No" "SpaceAfter=No" NA NA ...

As you can see, this object is a dataframe that consists of 152420 where
each row is a token, and each column is an annotation. For our purposes,
the most relevant variables are:

-   `doc_id` contains the document in which the token appeared;
-   `token` – contains the actual token;
-   `lemma` – contains the lemmatized token;
-   `upos` – contains the part of speech of the token, such as
    adjective, verb, noun, etc.;

Let’s select those variables

``` r
parsed_tokens <- parsed_tokens %>% 
  select(doc_id, token, upos, lemma)
```

Inspect how many nouns appear in the corpus

``` r
sum(parsed_tokens$upos == "NOUN")
```

    ## [1] 30639

Inspect how many verbs appear in the corpus

``` r
sum(parsed_tokens$upos == "VERB")
```

    ## [1] 14842

Inspect how many adjectives appear in the corpus

``` r
sum(parsed_tokens$upos == "ADJ")
```

    ## [1] 11640

We can also inspect all different POS tags in one go.

``` r
table(parsed_tokens$upos)
```

    ## 
    ##   ADJ   ADP   ADV   AUX CCONJ   DET  INTJ  NOUN   NUM  PART  PRON PROPN PUNCT SCONJ   SYM  VERB     X 
    ## 11640 18998  6243  9365  6852 16562    38 30639   509  3680 13794  2480 14073  2672    16 14842    17

An interesting tag is `PROPN`or proper noun that refers to the name (or
part of the name) of a unique entity, be it an individual, a place, or
an object. To get a feel for what entities we can filter out the proper
nouns and then count and sort their lemmas using `count()` from
**tidyverse**

``` r
propns <- parsed_tokens %>%
  filter(upos == "PROPN")

propns %>% count(lemma, sort = TRUE)
```

    ## # A tibble: 512 × 2
    ##    lemma         n
    ##    <chr>     <int>
    ##  1 States      310
    ##  2 America     237
    ##  3 United      170
    ##  4 Congress    126
    ##  5 God         107
    ##  6 President    88
    ##  7 Americans    75
    ##  8 Mr.          31
    ##  9 nation       29
    ## 10 Chief        27
    ## # ℹ 502 more rows

Say we are only interested in the nouns in those speeches

``` r
nouns <- parsed_tokens %>%
  filter(upos == "NOUN")
```

Let’s display their lemmas in a Wordcloud. We’ll first use the `split()`
function from base R to divide the nouns per speech in a list. We then
use `as.tokens()` in **quanteda** to turn that list into a tokens
object. We can create a `dfm` and take it from there.

``` r
nouns_dfm <- split(nouns$lemma, nouns$doc_id) %>% 
  as.tokens() %>% 
  dfm() 


textplot_wordcloud(nouns_dfm, max_words = 50)
```

![](Lab_Session_QTA_9_files/figure-markdown_github/unnamed-chunk-12-1.png)

Let’s do the same for verbs

``` r
verbs <- parsed_tokens %>%
  filter(upos == "VERB")

verbs_dfm <- split(verbs$lemma, verbs$doc_id) %>% 
  as.tokens() %>% dfm()

textplot_wordcloud(verbs_dfm, max_words = 50)
```

![](Lab_Session_QTA_9_files/figure-markdown_github/unnamed-chunk-13-1.png)

If we want to stitch back together the metadata to our newly created
`nouns_dfm` and `verbs_dfm` we can do this as follows:

``` r
docvars(nouns_dfm) <- inaugural_speeches_df %>% 
  select(Year, President, FirstName, Party)

docvars(verbs_dfm) <- inaugural_speeches_df %>%
  select(Year, President, FirstName, Party)
```

We are now in a position to inspect these dfms. For example, we may be
interested in what sort of verbs distinguish Republican presidents from
Democratic presidents.

``` r
verbs_dfm_grouped <- verbs_dfm %>% 
  dfm_group(groups = Party) %>%
  dfm_subset(Party == "Democratic" | Party == "Republican")

verb_keyness <- textstat_keyness(verbs_dfm_grouped, target = "Republican")

textplot_keyness(verb_keyness,
                 n = 10,
                 color = c("red", "blue"))
```

![](Lab_Session_QTA_9_files/figure-markdown_github/unnamed-chunk-15-1.png)
Let’s apply a topic model to the nouns

``` r
lda_10 <- textmodel_lda(nouns_dfm, 
                       k = 10,
                       alpha = 1,
                       max_iter = 2000)
```

Let’s inspect this topic model

``` r
terms(lda_10, 10)
```

    ##       topic1       topic2         topic3       topic4        topic5     topic6        topic7       topic8         topic9         topic10  
    ##  [1,] "world"      "union"        "today"      "war"         "side"     "law"         "government" "law"          "civilization" "thing"  
    ##  [2,] "nation"     "constitution" "time"       "nation"      "tax"      "business"    "people"     "citizenship"  "progress"     "spirit" 
    ##  [3,] "people"     "power"        "citizen"    "commerce"    "country"  "race"        "country"    "constitution" "ideal"        "hand"   
    ##  [4,] "freedom"    "opinion"      "work"       "force"       "price"    "legislation" "citizen"    "demand"       "task"         "form"   
    ##  [5,] "peace"      "territory"    "child"      "peace"       "burden"   "policy"      "duty"       "method"       "expression"   "part"   
    ##  [6,] "life"       "object"       "generation" "order"       "chance"   "condition"   "power"      "commerce"     "leadership"   "body"   
    ##  [7,] "man"        "liberty"      "land"       "year"        "name"     "change"      "interest"   "body"         "self"         "thought"
    ##  [8,] "government" "other"        "century"    "honor"       "globe"    "reform"      "time"       "community"    "today"        "action" 
    ##  [9,] "hope"       "member"       "promise"    "improvement" "pledge"   "necessity"   "party"      "endeavor"     "agency"       "measure"
    ## [10,] "year"       "hand"         "dream"      "object"      "struggle" "trade"       "law"        "territory"    "advance"      "fact"

``` r
head(lda_10$theta, 10)
```

    ##                     topic1     topic2      topic3    topic4      topic5      topic6    topic7     topic8      topic9     topic10
    ## 1789-Washington 0.07615894 0.10264901 0.013245033 0.2284768 0.013245033 0.006622517 0.3774834 0.02317881 0.046357616 0.112582781
    ## 1793-Washington 0.02857143 0.08571429 0.142857143 0.1428571 0.028571429 0.057142857 0.4285714 0.02857143 0.028571429 0.028571429
    ## 1797-Adams      0.11591696 0.03114187 0.005190311 0.3027682 0.001730104 0.010380623 0.4083045 0.03460208 0.003460208 0.086505190
    ## 1801-Jefferson  0.18734793 0.09975669 0.019464720 0.1484185 0.019464720 0.004866180 0.4257908 0.00486618 0.002433090 0.087591241
    ## 1805-Jefferson  0.07602339 0.10526316 0.001949318 0.1617934 0.025341131 0.025341131 0.4074074 0.09941520 0.007797271 0.089668616
    ## 1809-Madison    0.14338235 0.08823529 0.003676471 0.2941176 0.003676471 0.007352941 0.4080882 0.01838235 0.007352941 0.025735294
    ## 1813-Madison    0.08695652 0.01449275 0.014492754 0.2862319 0.086956522 0.007246377 0.3731884 0.05072464 0.054347826 0.025362319
    ## 1817-Monroe     0.01434720 0.09756098 0.001434720 0.2725968 0.012912482 0.002869440 0.5767575 0.00143472 0.010043042 0.010043042
    ## 1821-Monroe     0.01612903 0.06451613 0.002150538 0.4053763 0.008602151 0.012903226 0.4569892 0.01290323 0.018279570 0.002150538
    ## 1825-Adams      0.11885246 0.18715847 0.005464481 0.1912568 0.015027322 0.001366120 0.4398907 0.01502732 0.009562842 0.016393443

## Other languages

**updipe** allows you to work with pre-trained language models build for
more than 65 languages

<figure>
<img src="language_models.png" style="width:65.0%"
alt="Language models" />
<figcaption aria-hidden="true">Language models</figcaption>
</figure>

If you want to work with these models you first need to download them.
Let’s say I want to work with a Dutch corpus

``` r
udmodel_dutch <- udpipe_download_model(language = "dutch")

str(udmodel_dutch)
```

    ## 'data.frame':    1 obs. of  5 variables:
    ##  $ language        : chr "dutch-alpino"
    ##  $ file_model      : chr "/Users/hjms/Documents/Teaching/Essex/2023/Labs/Lab_9/dutch-alpino-ud-2.5-191206.udpipe"
    ##  $ url             : chr "https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.5/master/inst/udpipe-ud-2.5-191206/dutch-alpino-"| __truncated__
    ##  $ download_failed : logi FALSE
    ##  $ download_message: chr "OK"

I can now start tagging with vector of Dutch documents

``` r
dutch_documents <- c(d1 = "AZ wordt kampioen dit jaar",
                     d2 = "Mark Rutte, premier van Nederland, is op weg naar Brussel")

parsed_tokens_dutch <-  udpipe(dutch_documents, udmodel_dutch) %>% 
  as_tibble()

head(parsed_tokens_dutch)
```

    ## # A tibble: 6 × 17
    ##   doc_id paragraph_id sentence_id sentence                              start   end term_id token_id token lemma upos  xpos  feats head_token_id dep_rel deps  misc 
    ##   <chr>         <int>       <int> <chr>                                 <int> <int>   <int> <chr>    <chr> <chr> <chr> <chr> <chr> <chr>         <chr>   <chr> <chr>
    ## 1 d1                1           1 AZ wordt kampioen dit jaar                1     2       1 1        AZ    Az    NOUN  N|so… Gend… 2             nsubj   <NA>   <NA>
    ## 2 d1                1           1 AZ wordt kampioen dit jaar                4     8       2 2        wordt word… AUX   WW|p… Numb… 0             root    <NA>   <NA>
    ## 3 d1                1           1 AZ wordt kampioen dit jaar               10    17       3 3        kamp… kamp… NOUN  N|so… Gend… 2             xcomp   <NA>   <NA>
    ## 4 d1                1           1 AZ wordt kampioen dit jaar               19    21       4 4        dit   dit   DET   VNW|… <NA>  5             det     <NA>   <NA>
    ## 5 d1                1           1 AZ wordt kampioen dit jaar               23    26       5 5        jaar  jaar  NOUN  N|so… Gend… 3             obl     <NA>  "Spa…
    ## 6 d2                1           1 Mark Rutte, premier van Nederland, i…     1     4       1 1        Mark  mark  PROPN SPEC… <NA>  9             nsubj   <NA>   <NA>

If I have already downloaded the a language, I can load it as follows
(if the model is in the current working directory – otherwise I will
need to give it the full path to the file)

``` r
udmodel_dutch <- udpipe_load_model(file = "dutch-alpino-ud-2.5-191206.udpipe")
```

## Exercises

For these exercises we will work with the `parsed_tokens` dataframe that
we created in the above script.

1.  Create a dataframe `adjs` that contains all adjectives that appear
    in the corpus of inaugural speeches.

2.  Display the most occurring adjectives in the inaugural speeches
    using `count()`

3.  Group the the adjectives by speech and turn them into a dataframe
    called `adjs_dfm`.

4.  Append Year, President, FirstName and Party from
    `inaugural_speeches_df` as docvars to `adjs_dfm`

5.  Inspect `adjs_dfm` using the NRC Emotion Association Lexicon. If you
    don’t recall how to do this, have a look back at lab session 4. Call
    the output of `dfm_lookuop` as `dfm_inaugural_NRC`.

6.  Add the count of fear words as a variable `fear` to the docvars of
    `adjs_dfm`

**Advanced**

1.  Use tidyverse functions to display the mean number of fear words for
    Repulican and Democratic presidents (NB: normally we would divide
    this number by the total number of tokens in a speech). Have a look
    at [this link](https://dplyr.tidyverse.org/reference/group_by.html)
    for more info.

2.  Download a language model of your choice and inspect a vector of a
    few sentences using `udpipe`
