QTA Day 9: Word embeddings
================
18 July, 2024

The goal of today’s lab session is to develop an understanding for word
embeddings. We’ll train a word embeddings model using the **text2vec**
library (Selivanov, Bickel & Wang, 2022) on a set of speeches of
European Commissioners and we’ll inspect these embeddings.

NB: Keep in mind that this lab session is meant for practice purposes
only. The word vectors that we’ll inspect require careful validation.

Let’s load the required libraries first.

``` r
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
library(text2vec)
```

## Preparing the data

Let’s read in the Commission speeches

``` r
load("european_commission.Rdata")

dim(commission_speeches)
```

    ## [1] 6140    2

``` r
names(commission_speeches)
```

    ## [1] "speaker" "text"

We’ll tokenise the speeches.

``` r
corpus_speeches <- corpus(commission_speeches)
tokens_speeches <- tokens(corpus_speeches,
                          what = "word",
                          remove_punct = TRUE, 
                          remove_symbols = TRUE, 
                          remove_numbers = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE,
                          split_hyphens = FALSE,
                          ) %>%
  tokens_remove(stopwords(source = "smart"), padding = FALSE)
```

NB: The next few steps draw on
[this](https://quanteda.io/articles/pkgdown/replication/text2vec.html)
**quanteda** tutorial.

We’ll select those features that occur at least 10 times

``` r
feats <- dfm(tokens_speeches) %>%
    dfm_trim(min_termfreq = 10) %>%
    featnames()
```

We’ll now select these features from the tokenised speeches.

``` r
tokens_speeches <- tokens_select(tokens_speeches, 
                                 feats)
```

Let’s inspect which features occur most often

``` r
tokens_speeches %>%
  dfm() %>%
  topfeatures(n = 100,
              decreasing = TRUE,
              scheme = c("count")
)
```

    ##      european            eu        europe    commission        member 
    ##         59741         36221         28885         26391         21919 
    ##        states      economic        policy        market         union 
    ##         21113         20112         20031         18333         17782 
    ##          work        energy     important        growth          make 
    ##         15250         14912         14768         14553         14211 
    ##     countries         today     financial        people       support 
    ##         13889         13600         12862         12828         12806 
    ##         world          time          year        future        social 
    ##         11848         11796         11584         11424         11386 
    ##      national         years        public   development       economy 
    ##         11285         11020         10909         10147          9952 
    ##        global         level        crisis      research    innovation 
    ##          9892          9771          9664          9422          9377 
    ##    investment      citizens        change     political    challenges 
    ##          8942          8888          8875          8675          8165 
    ##      services        ladies        sector          good     gentlemen 
    ##          8113          8005          7987          7966          7922 
    ##           key        single       markets        ensure         trade 
    ##          7805          7773          7666          7557          7524 
    ##       council international         rules         clear   cooperation 
    ##          7521          7413          7398          7379          7332 
    ##          area          role        action       process        common 
    ##          7176          7052          6986          6929          6872 
    ##          part   competition      business     framework          euro 
    ##          6862          6862          6758          6607          6544 
    ##        system       climate   sustainable        issues         state 
    ##          6526          6500          6491          6488          6485 
    ##       forward      security     companies      strategy          made 
    ##          6416          6386          6350          6323          6299 
    ##        rights         areas     president    parliament        reform 
    ##          6167          6126          6077          6063          6027 
    ##      progress       working      approach      continue           set 
    ##          5976          5942          5940          5909          5874 
    ##     agreement      measures         means          jobs      policies 
    ##          5862          5819          5814          5770          5589 
    ##         place        strong       efforts          high          open 
    ##          5411          5356          5347          5331          5318 
    ##       country      regional          data           law           put 
    ##          5272          5186          5140          5109          5065

We’ll create a feature-co-occurrence matrix using the `fcm()` function
which calculates co-occurrences of features within a user-defined
context. We’ll choose a window size of 5, but other choices are
available

``` r
speeches_fcm <- fcm(tokens_speeches, 
                    context = "window", 
                    window = 5,
                    tri = TRUE)

dim(speeches_fcm)
```

    ## [1] 21516 21516

Let’s see what `speeches_fcm()` looks like.

``` r
speeches_fcm[1:5,1:5]
```

    ## Feature co-occurrence matrix of: 5 by 5 features.
    ##            features
    ## features    Dear President Committee Regions Erasmus
    ##   Dear       474       221        45      16       5
    ##   President    0      1414       105      43       3
    ##   Committee    0         0       188     287       4
    ##   Regions      0         0         0      26       1
    ##   Erasmus      0         0         0       0     122

*Dear* and *President* co-occur 221 times in the corpus. *Dear* and
*Regions* only 16 times.

## Fitting a GloVe model

We’ll now fit a GloVe vector model. GloVe is an unsupervised learning
algorithm for obtaining vector representations for words. Training is
performed on the feature co-occurrence matrix, which represents
information about global word-word co-occurrence statistics in a corpus.

``` r
glove <- GlobalVectors$new(rank = 50, 
                           x_max = 10)

wv_main <- glove$fit_transform(speeches_fcm, 
                               n_iter = 10,
                               convergence_tol = 0.01, 
                               n_threads = 8)
```

    ## INFO  [17:05:30.253] epoch 1, loss 0.1513
    ## INFO  [17:05:33.531] epoch 2, loss 0.1102
    ## INFO  [17:05:36.932] epoch 3, loss 0.0989
    ## INFO  [17:05:40.178] epoch 4, loss 0.0931
    ## INFO  [17:05:43.401] epoch 5, loss 0.0894
    ## INFO  [17:05:46.636] epoch 6, loss 0.0869
    ## INFO  [17:05:49.940] epoch 7, loss 0.0851
    ## INFO  [17:05:54.661] epoch 8, loss 0.0837
    ## INFO  [17:05:58.006] epoch 9, loss 0.0826
    ## INFO  [17:06:01.312] epoch 10, loss 0.0817

``` r
dim(wv_main)
```

    ## [1] 21516    50

The model learns two sets of word vectors - main and context. They are
essentially the same.

``` r
wv_context <- glove$components
dim(wv_context)
```

    ## [1]    50 21516

Following recommendations in the **text2vec** package we sum these
vectors. We transpose the `wv_context` object so that it has the same
number of rows and columns as `wv_main`

``` r
word_vectors <- wv_main + t(wv_context)

dim(word_vectors)
```

    ## [1] 21516    50

We now have 50-dimension word_vectors for all 21516 tokens in our
corpus.

## Inspecting the GloVe model

Now it’s tme to inspect these word embeddings. For example, we find the
nearest neighbors of a word (or a set of words) of interest. Nearest
neighbors are those words that are most closely located in the vector
space. We can find those using by calculating cosine similarities
between the word vector of a target word and all other word vectors.

We’ll use a custom function
([source](https://s-ai-f.github.io/Natural-Language-Processing/Word-embeddings.html))
to finds these similar words It takes in three arguments: the target
word, the word_vectors object, and the number of neighbors we want to
inspect.

``` r
find_similar_words <- function(word, word_vectors, n = 10) {
  similarities <- word_vectors[word, , drop = FALSE] %>%
    sim2(word_vectors, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}
```

The Commissioner speeches span the time period 2007–2014, a time of
upheaval in the EU. Let’s take a look at the nearest neighbors of
‘crisis’. The `drop = FALSE` argument ensure that crisis is not
converted to a vector.

``` r
find_similar_words("crisis", word_vectors)
```

    ##       crisis    financial     economic        worst       facing     response 
    ##    1.0000000    0.7700851    0.7608627    0.7526268    0.7488370    0.7478936 
    ##    situation    recession         face consequences 
    ##    0.7343410    0.7197310    0.7168328    0.7167669

Crisis refers mostly to the Eurocrisis.

Let’s inspect the context of climate

``` r
find_similar_words("climate", word_vectors)
```

    ##    climate     change  challenge     global      major challenges       lead 
    ##  1.0000000  0.9174736  0.7645282  0.7518476  0.7170579  0.6832630  0.6785270 
    ## developing    Climate     action 
    ##  0.6704746  0.6693081  0.6564534

Global climate change needs to be addressed, that much is clear.

We can sum vectors to each find neigbors. Let’s add crisis + Ireland

``` r
crisis_Ireland <- word_vectors["crisis", , drop = FALSE] +
  word_vectors["Ireland", , drop = FALSE] 
```

And find the nearest neighbors of this sum. The sim2 function calculates
the cosine similarity between the word vectors of the target word and
all other word vectors.

``` r
cos_sim_crisis_Ireland <- sim2(x = word_vectors, y = crisis_Ireland, method = "cosine", norm = "l2")
head(sort(cos_sim_crisis_Ireland[,1], decreasing = TRUE), 20)
```

    ##    crisis   Ireland    Greece     shown    helped    facing situation financial 
    ## 0.9000325 0.8033603 0.7477974 0.7106924 0.7062906 0.6994423 0.6967463 0.6961909 
    ##      back   current      past difficult    return recession  recovery      time 
    ## 0.6841734 0.6784080 0.6758630 0.6712911 0.6639633 0.6619764 0.6578184 0.6570503 
    ##     years     worst    forget     Spain 
    ## 0.6567784 0.6561233 0.6532524 0.6530136

It mostly lists other countries that where also struggling at the time.

What if we substract the Ireland vector from the crisis vector?

``` r
crisis_Ireland <- word_vectors["crisis", , drop = FALSE] -
  word_vectors["Ireland", , drop = FALSE] 

cos_sim_crisis_Ireland <- sim2(x = word_vectors, y = crisis_Ireland, method = "cosine", norm = "l2")
head(sort(cos_sim_crisis_Ireland[,1], decreasing = TRUE), 10)
```

    ##        crisis  consequences fundamentally        crises      response 
    ##     0.7136996     0.6601463     0.6416758     0.6302471     0.6284338 
    ##      systemic      mitigate      economic          face         worst 
    ##     0.6078550     0.5999919     0.5975700     0.5865387     0.5713269

This time we get more general crisis terms.

Inspecting a word embeddings model like so can be useful for a few
different tasks:

1.  As a list of potential terms for dictionary construction;
2.  As an input to downstream QTA tasks;
3.  As a source for visualization.

Let’s take this first task an example. Perhaps we want to develop a
sentiment dictionary for Commissioner speeches, but we are less trusting
of off-the-shelf sentiment dictionaries because we suspect that these
may not capture how sentiment is expressed in Commissioner speeches. One
way to go is use a small seed dictionary of positive and negative words,
and use word embeddings to inspect what other words are close in the
embedding space to these seed words.

For example, we may take as positive words a small set of positive seed
words: *good*, *nice*, *excellent*, *positive*, *fortunate*, *correct*,
*superior*. And as negative words a small set of negative seed words:
*bad*, *nasty*, *poor*, *negative*, *wrong*, *unfortunate*

Let’s start by calculating the average vector for good.

``` r
positive <- (word_vectors["good", , drop = FALSE] +
  word_vectors["nice", , drop = FALSE] +
  word_vectors["excellent", , drop = FALSE] +
  word_vectors["positive", , drop = FALSE] + 
  word_vectors["fortunate", , drop = FALSE] + 
  word_vectors["correct", , drop = FALSE] + 
  word_vectors["superior", , drop = FALSE]) /7
```

And for bad

``` r
negative <- (word_vectors["bad", , drop = FALSE] +
  word_vectors["nasty", , drop = FALSE] +
  word_vectors["poor", , drop = FALSE] +
  word_vectors["negative", , drop = FALSE] + 
  word_vectors["wrong", , drop = FALSE] + 
  word_vectors["unfortunate", , drop = FALSE]) /6
```

We can now inspect the neighbors of our ‘positive’ seed dictionary.

``` r
cos_sim_positive <- sim2(x = word_vectors, y = positive, method = "cosine", norm = "l2")
head(sort(cos_sim_positive[,1], decreasing = TRUE), 20)
```

    ##       good        lot      start       hard  excellent successful  confident 
    ##  0.8295096  0.7423259  0.7283856  0.7279259  0.7201758  0.7196559  0.7183235 
    ##       back    results    success       hope      ahead   positive       time 
    ##  0.7168551  0.7163129  0.7117552  0.7090261  0.7048276  0.7043936  0.6999623 
    ##       made      takes      point      today   conclude   starting 
    ##  0.6922623  0.6860290  0.6800901  0.6661260  0.6630544  0.6614749

This includes some words that seem useful such as encouraging and
opportunity and forward. But also the word bad appears.

Let’s do the same for our ‘negative’ dictionary

``` r
cos_sim_negative <- sim2(x = word_vectors, y = negative, method = "cosine", norm = "l2")
head(sort(cos_sim_negative[,1], decreasing = TRUE), 20)
```

    ##          bad consequences        worse         poor     negative        wrong 
    ##    0.7648784    0.7043296    0.6640207    0.6581882    0.6421580    0.6359878 
    ##        avoid         felt          hit      effects    suffering      fragile 
    ##    0.6308899    0.6023716    0.5960297    0.5934129    0.5924499    0.5909730 
    ##        worst       damage       severe    happening      weather        thing 
    ##    0.5899138    0.5832185    0.5828702    0.5713584    0.5623844    0.5532088 
    ##         left  complacency 
    ##    0.5510380    0.5482371

Again we see a mix of useful and less useful words.

## Exercises

Estimate new word vectors but this time on a feature co-occurrence
matrix with a window size of 5 but with more weight given to words when
they appear closer to the target word (see the *count* and *weight*
arguments in `fcm()`. To estimate this model comment out the code chunk
below to run the model)

``` r
speeches_fcm_weighted <- fcm(tokens_speeches, 
                    context = "window", 
                    count = "weighted", 
                    weights = 1 / (1:5),
                    tri = TRUE)


glove <- GlobalVectors$new(rank = 50, 
                           x_max = 10)

wv_main_weighted <- glove$fit_transform(speeches_fcm_weighted, 
                                        n_iter = 10,
                                        convergence_tol = 0.01, 
                                        n_threads = 8)
```

    ## INFO  [17:06:05.521] epoch 1, loss 0.1560
    ## INFO  [17:06:08.900] epoch 2, loss 0.1155
    ## INFO  [17:06:12.109] epoch 3, loss 0.1026
    ## INFO  [17:06:15.559] epoch 4, loss 0.0965
    ## INFO  [17:06:18.916] epoch 5, loss 0.0926
    ## INFO  [17:06:22.133] epoch 6, loss 0.0900
    ## INFO  [17:06:25.376] epoch 7, loss 0.0880
    ## INFO  [17:06:28.792] epoch 8, loss 0.0864
    ## INFO  [17:06:31.963] epoch 9, loss 0.0852
    ## INFO  [17:06:35.123] epoch 10, loss 0.0842

``` r
wv_context_weighted <- glove$components

word_vectors_weighted <- wv_main_weighted + t(wv_context_weighted)
```

2.  Compare the nearest neighbors for crisis in both the original and
    the new model. Are they any different?

``` r
find_similar_words("crisis", word_vectors)
```

    ##       crisis    financial     economic        worst       facing     response 
    ##    1.0000000    0.7700851    0.7608627    0.7526268    0.7488370    0.7478936 
    ##    situation    recession         face consequences 
    ##    0.7343410    0.7197310    0.7168328    0.7167669

``` r
find_similar_words("crisis", word_vectors_weighted)
```

    ##    crisis financial    facing  economic  response      face  problems situation 
    ## 1.0000000 0.7624897 0.7461408 0.7307260 0.7273340 0.7248604 0.7054223 0.6986762 
    ##   current    crises 
    ## 0.6919101 0.6860715

3.  Inspect the nearest neighbors for Greece, Portugal, Spain and Italy
    and substract the vectors for Netherlands, Germany, Denmark and
    Austria

``` r
southern_northern  <- (word_vectors["Greece", , drop = FALSE] +
  word_vectors["Portugal", , drop = FALSE] +
  word_vectors["Spain", , drop = FALSE] +
  word_vectors["Italy", , drop = FALSE] -
  word_vectors["Netherlands", , drop = FALSE] -
  word_vectors["Germany", , drop = FALSE] -
  word_vectors["Denmark", , drop = FALSE] -
  word_vectors["Austria", , drop = FALSE])


cos_sim_southern_northern <- sim2(x = word_vectors, y = southern_northern, method = "cosine", norm = "l2")
head(sort(cos_sim_southern_northern[,1], decreasing = TRUE), 20)
```

    ##        Greece         Greek       restore     stability       reforms 
    ##     0.6807022     0.6071226     0.5424195     0.5340666     0.5309807 
    ##    assistance     situation    adjustment      recovery         steps 
    ##     0.5260148     0.5160561     0.5144179     0.5019823     0.4991451 
    ##    structural consolidation        reform      conclude        return 
    ##     0.4979891     0.4964581     0.4913954     0.4881791     0.4861812 
    ##     stabilise      measures          exit    confidence      Greece's 
    ##     0.4827456     0.4718979     0.4690960     0.4680950     0.4675269

4.  And turn this vector around

``` r
northern_southern  <- (word_vectors["Netherlands", , drop = FALSE] +
  word_vectors["Germany", , drop = FALSE] +
  word_vectors["Denmark", , drop = FALSE] +
  word_vectors["Austria", , drop = FALSE] -
  word_vectors["Greece", , drop = FALSE] -
  word_vectors["Portugal", , drop = FALSE] -
  word_vectors["Spain", , drop = FALSE] -
  word_vectors["Italy", , drop = FALSE])


cos_sim_northern_southern <- sim2(x = word_vectors, y = northern_southern, method = "cosine", norm = "l2")
head(sort(cos_sim_northern_southern[,1], decreasing = TRUE), 20)
```

    ##  intellectuals      Centuries        Attacks        Generic          units 
    ##      0.6194658      0.5064203      0.4997699      0.4916157      0.4908166 
    ##        GENERAL      ignorance      memoranda       prepares       scholars 
    ##      0.4897332      0.4887248      0.4860483      0.4845634      0.4788636 
    ##       Inviting     literature     performing           dies        Display 
    ##      0.4784631      0.4754823      0.4744598      0.4742727      0.4728557 
    ##           We’d        Springs classification       attained         Remote 
    ##      0.4718274      0.4698703      0.4659327      0.4603988      0.4583066

5.  Inspect these word vectors further. If you receive a
    `subscript out of bounds` error, it means that the word does not
    appear in the corpus.
