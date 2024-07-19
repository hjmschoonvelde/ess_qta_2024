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

    ## INFO  [17:03:20.536] epoch 1, loss 0.1514
    ## INFO  [17:03:23.765] epoch 2, loss 0.1103
    ## INFO  [17:03:26.975] epoch 3, loss 0.0991
    ## INFO  [17:03:30.160] epoch 4, loss 0.0933
    ## INFO  [17:03:33.355] epoch 5, loss 0.0897
    ## INFO  [17:03:36.549] epoch 6, loss 0.0871
    ## INFO  [17:03:39.753] epoch 7, loss 0.0853
    ## INFO  [17:03:42.963] epoch 8, loss 0.0838
    ## INFO  [17:03:46.164] epoch 9, loss 0.0827
    ## INFO  [17:03:49.374] epoch 10, loss 0.0818

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

    ##       crisis    financial     economic       facing     response         face 
    ##    1.0000000    0.7720751    0.7641775    0.7413728    0.7370255    0.7238863 
    ## consequences        worst    situation         past 
    ##    0.7099380    0.7084172    0.7056526    0.7005671

Crisis refers mostly to the Eurocrisis.

Let’s inspect the context of climate

``` r
find_similar_words("climate", word_vectors)
```

    ##    climate     change     global  challenge      major    Climate developing 
    ##  1.0000000  0.9231393  0.7476927  0.7306053  0.6884716  0.6797308  0.6791071 
    ##       lead challenges     action 
    ##  0.6618658  0.6566193  0.6486250

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

    ##       crisis      Ireland       facing       Greece    financial         past 
    ##    0.8988937    0.7861564    0.7172469    0.7076244    0.7016614    0.6996338 
    ##    situation         back     economic    difficult        shown        years 
    ##    0.6995472    0.6976457    0.6962478    0.6889091    0.6853684    0.6670418 
    ## difficulties         time         euro      current       forget         fact 
    ##    0.6669505    0.6614908    0.6605170    0.6576660    0.6554450    0.6528046 
    ##       helped      started 
    ##    0.6496106    0.6432041

It mostly lists other countries that where also struggling at the time.

What if we substract the Ireland vector from the crisis vector?

``` r
crisis_Ireland <- word_vectors["crisis", , drop = FALSE] -
  word_vectors["Ireland", , drop = FALSE] 

cos_sim_crisis_Ireland <- sim2(x = word_vectors, y = crisis_Ireland, method = "cosine", norm = "l2")
head(sort(cos_sim_crisis_Ireland[,1], decreasing = TRUE), 10)
```

    ##       crisis     systemic     response consequences       crises    financial 
    ##    0.7346659    0.6777083    0.6380374    0.6127668    0.5912191    0.5553756 
    ##        midst       urgent     economic         face 
    ##    0.5549008    0.5478941    0.5469639    0.5444004

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

    ##       good        lot      start    success   positive       hope  confident 
    ##  0.8426384  0.7709069  0.7388799  0.7314885  0.7254225  0.7233983  0.7214691 
    ##       time successful       made     taking  excellent     making       back 
    ##  0.7104527  0.6997398  0.6910427  0.6841490  0.6840285  0.6830315  0.6810424 
    ##     coming    results      point      takes      ahead      today 
    ##  0.6794501  0.6794499  0.6793053  0.6750520  0.6722713  0.6720345

This includes some words that seem useful such as encouraging and
opportunity and forward. But also the word bad appears.

Let’s do the same for our ‘negative’ dictionary

``` r
cos_sim_negative <- sim2(x = word_vectors, y = negative, method = "cosine", norm = "l2")
head(sort(cos_sim_negative[,1], decreasing = TRUE), 20)
```

    ##          bad         poor consequences        worse        wrong     negative 
    ##    0.7830673    0.7114921    0.6659107    0.6593086    0.6203496    0.6203404 
    ##         felt      effects     affected       damage      learned     problems 
    ##    0.6054218    0.6050681    0.5841829    0.5635160    0.5558923    0.5556926 
    ##      morally       ignore       losing       simply        thing       crisis 
    ##    0.5543220    0.5524187    0.5502797    0.5474387    0.5467835    0.5465727 
    ##          hit       severe 
    ##    0.5405302    0.5351870

Again we see a mix of useful and less useful words.

## Exercises

Estimate new word vectors but this time on a feature co-occurrence
matrix with a window size of 5 but with more weight given to words when
they appear closer to the target word (see the *count* and *weight*
arguments in `fcm()`. To estimate this model comment out the code chunk
below to run the model)

``` r
#speeches_fcm_weighted <- fcm(tokens_speeches, 
#                    context = "window", 
#                    count = "weighted", 
#                    weights = 1 / (1:5),
#                    tri = TRUE)


#glove <- GlobalVectors$new(rank = 50, 
#                           x_max = 10)

#wv_main_weighted <- glove$fit_transform(speeches_fcm_weighted, 
#                                        n_iter = 10,
#                                        convergence_tol = 0.01, 
#                                        n_threads = 8)

#wv_context_weighted <- glove$components

#word_vectors_weighted <- wv_main_weighted + t(wv_context_weighted)
```

2.  Compare the nearest neighbors for crisis in both the original and
    the new model. Are they any different?

``` r
#find_similar_words("crisis", word_vectors)
#find_similar_words("crisis", word_vectors_weighted)
```

3.  Inspect the nearest neighbors for Greece, Portugal, Spain and Italy
    and substract the vectors for Netherlands, Germany, Denmark and
    Austria

``` r
#southern_northern  <- (word_vectors["Greece", , drop = FALSE] +
#  word_vectors["Portugal", , drop = FALSE] +
#  word_vectors["Spain", , drop = FALSE] +
#  word_vectors["Italy", , drop = FALSE] -
#  word_vectors["Netherlands", , drop = FALSE] -
#  word_vectors["Germany", , drop = FALSE] -
#  word_vectors["Denmark", , drop = FALSE] -
#  word_vectors["Austria", , drop = FALSE])


#cos_sim_southern_northern <- sim2(x = word_vectors, y = southern_northern, method = "cosine", norm = "l2")
#head(sort(cos_sim_southern_northern[,1], decreasing = TRUE), 20)
```

4.  And turn this vector around

``` r
#northern_southern  <- (word_vectors["Netherlands", , drop = FALSE] +
#  word_vectors["Germany", , drop = FALSE] +
# word_vectors["Denmark", , drop = FALSE] +
# word_vectors["Austria", , drop = FALSE] -
# word_vectors["Greece", , drop = FALSE] -
#  word_vectors["Portugal", , drop = FALSE] -
#  word_vectors["Spain", , drop = FALSE] -
#  word_vectors["Italy", , drop = FALSE])


#cos_sim_northern_southern <- sim2(x = word_vectors, y = northern_southern, method = "cosine", norm = "l2")
#head(sort(cos_sim_northern_southern[,1], decreasing = TRUE), 20)
```

5.  Inspect these word vectors further. If you receive a
    `subscript out of bounds` error, it means that the word does not
    appear in the corpus.
