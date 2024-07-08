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

- `doc_id` contains the document in which the token appeared;
- `token` – contains the actual token;
- `lemma` – contains the lemmatized token;
- `upos` – contains the part of speech of the token, such as adjective,
  verb, noun, etc.;

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

![](Lab_Session_QTA_9_Answers_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Let’s do the same for verbs

``` r
verbs <- parsed_tokens %>%
  filter(upos == "VERB")

verbs_dfm <- split(verbs$lemma, verbs$doc_id) %>% 
  as.tokens() %>% dfm()

textplot_wordcloud(verbs_dfm, max_words = 50)
```

![](Lab_Session_QTA_9_Answers_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

![](Lab_Session_QTA_9_Answers_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
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

    ##       topic1       topic2       topic3     topic4         topic5        topic6      topic7        topic8        topic9       topic10       
    ##  [1,] "nation"     "government" "oath"     "union"        "citizenship" "life"      "war"         "law"         "government" "world"       
    ##  [2,] "people"     "people"     "economy"  "power"        "law"         "man"       "force"       "business"    "mind"       "progress"    
    ##  [3,] "world"      "country"    "job"      "constitution" "class"       "thing"     "peace"       "policy"      "happiness"  "civilization"
    ##  [4,] "freedom"    "nation"     "hour"     "state"        "demand"      "purpose"   "commerce"    "race"        "nature"     "liberty"     
    ##  [5,] "time"       "duty"       "crisis"   "object"       "fathers"     "spirit"    "year"        "tariff"      "love"       "opportunity" 
    ##  [6,] "peace"      "interest"   "birth"    "citizen"      "development" "change"    "arm"         "legislation" "good"       "action"      
    ##  [7,] "day"        "citizen"    "tax"      "part"         "evil"        "mankind"   "conflict"    "respect"     "honor"      "standard"    
    ##  [8,] "today"      "power"      "ceremony" "opinion"      "privileges"  "justice"   "improvement" "change"      "gratitude"  "justice"     
    ##  [9,] "government" "law"        "moment"   "character"    "method"      "knowledge" "part"        "trade"       "course"     "republic"    
    ## [10,] "history"    "time"       "path"     "liberty"      "ballot"      "earth"     "sovereignty" "currency"    "function"   "order"

``` r
head(lda_10$theta, 10)
```

    ##                      topic1    topic2      topic3     topic4      topic5     topic6     topic7      topic8     topic9     topic10
    ## 1789-Washington 0.036423841 0.3774834 0.003311258 0.12251656 0.016556291 0.02980132 0.02317881 0.056291391 0.30463576 0.029801325
    ## 1793-Washington 0.057142857 0.4285714 0.057142857 0.05714286 0.028571429 0.02857143 0.02857143 0.142857143 0.14285714 0.028571429
    ## 1797-Adams      0.025951557 0.4152249 0.001730104 0.09861592 0.005190311 0.07958478 0.04844291 0.001730104 0.30968858 0.013840830
    ## 1801-Jefferson  0.102189781 0.3260341 0.019464720 0.18734793 0.002433090 0.06082725 0.05352798 0.024330900 0.17274939 0.051094891
    ## 1805-Jefferson  0.052631579 0.3391813 0.013645224 0.14814815 0.011695906 0.05653021 0.07407407 0.021442495 0.26510721 0.017543860
    ## 1809-Madison    0.036764706 0.4191176 0.025735294 0.08455882 0.003676471 0.06617647 0.09191176 0.018382353 0.24264706 0.011029412
    ## 1813-Madison    0.028985507 0.2826087 0.032608696 0.05072464 0.079710145 0.11594203 0.22463768 0.057971014 0.03985507 0.086956522
    ## 1817-Monroe     0.001434720 0.5437590 0.001434720 0.14203730 0.007173601 0.01147776 0.21664275 0.004304161 0.06599713 0.005738881
    ## 1821-Monroe     0.001075269 0.4548387 0.001075269 0.12688172 0.005376344 0.01397849 0.35913978 0.009677419 0.02150538 0.006451613
    ## 1825-Adams      0.081967213 0.4398907 0.008196721 0.17759563 0.001366120 0.02868852 0.10928962 0.004098361 0.11748634 0.031420765

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

``` r
adjs <- parsed_tokens %>%
  filter(upos == "ADJ")
```

2.  Display the most occurring adjectives in the inaugural speeches
    using `count()`

``` r
adjs %>% count(lemma, sort = TRUE)
```

    ## # A tibble: 1,788 × 2
    ##    lemma        n
    ##    <chr>    <int>
    ##  1 great      334
    ##  2 other      268
    ##  3 new        245
    ##  4 own        244
    ##  5 public     217
    ##  6 free       182
    ##  7 american   163
    ##  8 fellow     151
    ##  9 such       147
    ## 10 national   141
    ## # ℹ 1,778 more rows

3.  Group the the adjectives by speech and turn them into a dataframe
    called `adjs_dfm`.

``` r
adjs_dfm <- split(adjs$lemma, adjs$doc_id) %>% 
  as.tokens() %>% dfm()
```

4.  Append Year, President, FirstName and Party from
    `inaugural_speeches_df` as docvars to `adjs_dfm`

``` r
docvars(adjs_dfm) <- inaugural_speeches_df %>%
  select(Year, President, FirstName, Party)
```

5.  Inspect `adjs_dfm` using the NRC Emotion Association Lexicon. If you
    don’t recall how to do this, have a look back at lab session 4. Call
    the output of `dfm_lookuop` as `dfm_inaugural_NRC`.

``` r
dfm_inaugural_NRC <- dfm_lookup(adjs_dfm, 
                                dictionary = data_dictionary_NRC)

head(dfm_inaugural_NRC)
```

    ## Document-feature matrix of: 6 documents, 10 features (6.67% sparse) and 4 docvars.
    ##                  features
    ## docs              anger anticipation disgust fear joy negative positive sadness surprise trust
    ##   1789-Washington     1           13       4    1  11        7       43       5        5    24
    ##   1793-Washington     0            1       0    0   1        1        4       0        1     4
    ##   1797-Adams          4           24       5    7  22       16       64       3        9    40
    ##   1801-Jefferson      7           15       8   12  18       14       55       9        6    29
    ##   1805-Jefferson      8           26       7    5  12       14       54       6        8    32
    ##   1809-Madison        6           19       4    8  19       18       43       4        6    20

6.  Add the count of fear words as a variable `fear` to the docvars of
    `adjs_dfm`

``` r
docvars(adjs_dfm, "fear") <- dfm_inaugural_NRC[,4]
```

**Advanced**

7.  Use tidyverse functions to display the mean number of fear words for
    Repulican and Democratic presidents (NB: normally we would divide
    this number by the total number of tokens in a speech). Have a look
    at [this link](https://dplyr.tidyverse.org/reference/group_by.html)
    for more info.

``` r
data <- docvars(adjs_dfm)

data_by_party <- data %>%
  group_by(Party) %>%
  filter(Party == "Republican" | Party == "Democratic") %>%
  summarise(mean_fear = mean(fear))

data_by_party
```

    ## # A tibble: 2 × 2
    ##   Party      mean_fear
    ##   <fct>          <dbl>
    ## 1 Democratic      7.09
    ## 2 Republican      8.46

8.  Download a language model of your choice and inspect a vector of a
    few sentences using `udpipe`

``` r
udmodel_german <- udpipe_download_model(language = "german")


german_documents <- c(d1 = "Ich bin ein Berliner",
                      d2 = "Wie geht es dir")

parsed_tokens_german <-  udpipe(german_documents, udmodel_german) %>% 
  as_tibble()

head(parsed_tokens_german)
```

    ## # A tibble: 6 × 17
    ##   doc_id paragraph_id sentence_id sentence             start   end term_id token_id token    lemma    upos  xpos  feats            head_token_id dep_rel deps  misc 
    ##   <chr>         <int>       <int> <chr>                <int> <int>   <int> <chr>    <chr>    <chr>    <chr> <chr> <chr>            <chr>         <chr>   <chr> <chr>
    ## 1 d1                1           1 Ich bin ein Berliner     1     3       1 1        Ich      ich      PRON  PPER  Case=Nom|Number… 4             nsubj   <NA>   <NA>
    ## 2 d1                1           1 Ich bin ein Berliner     5     7       2 2        bin      sein     AUX   VAFIN Mood=Ind|Number… 4             cop     <NA>   <NA>
    ## 3 d1                1           1 Ich bin ein Berliner     9    11       3 3        ein      ein      DET   ART   Case=Nom|Defini… 4             det     <NA>   <NA>
    ## 4 d1                1           1 Ich bin ein Berliner    13    20       4 4        Berliner Berliner NOUN  NN    Case=Nom|Gender… 0             root    <NA>  "Spa…
    ## 5 d2                1           1 Wie geht es dir          1     3       1 1        Wie      wie      ADV   NE    Case=Nom|Gender… 2             advmod  <NA>   <NA>
    ## 6 d2                1           1 Wie geht es dir          5     8       2 2        geht     gehen    VERB  VVFIN Mood=Ind|Number… 0             root    <NA>   <NA>
