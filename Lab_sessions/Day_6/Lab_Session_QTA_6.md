QTA Day 6: Scaling methods
================
16 July, 2024

This document gives some examples of how to apply scaling methods
(Wordscores, Wordfish, LSS) in **quanteda**. For these examples, we use
the (English) speeches of EP group leaders that are part of the
[EUSpeech](https://dataverse.harvard.edu/dataverse/euspeech) dataset.

The **quanteda**, **quanteda.textmodels**, **quanteda.textstats**,
**quanteda.textplots**, **ggplot2** and **tidyverse** packages are
familiar at this point. The **quanteda.corpora** package can be
downloaded as follows:
`devtools::install_github("quanteda/quanteda.corpora")`. The **LSX**
package can be downloaded using `install.packages("LSX")`

``` r
#load libraries
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.corpora)
library(quanteda.textstats)
library(ggplot2)
library(tidyverse)
library(LSX)

#read in the EP speeches
speeches <- read.csv(file = "speeches_ep.csv", 
                     header = TRUE, 
                     stringsAsFactors = FALSE, 
                     sep = ",", 
                     encoding = "UTF-8")
```

Inspect how many unique speakers there are in the dataset.

``` r
unique(speeches$speaker)
```

    ##  [1] "Lothar Bisky"         "Martin Callanan"      "Daniel Cohn-Bendit"  
    ##  [4] "Brian Crowley"        "Joseph Daul"          "Nigel Farage"        
    ##  [7] "Monica Frassoni"      "Rebecca Harms"        "Syed Kamall"         
    ## [10] "Michal Kaminski"      "Cristiana Muscardini" "Martin Schulz"       
    ## [13] "Kathy Sinnott"        "Francesco Speroni"    "Hannes Swoboda"      
    ## [16] "Guy Verhofstadt"      "Graham Watson"        "Francis Wurtz"       
    ## [19] "Jan Zahradil"         "Gabriele Zimmer"

Let’s first merge the speeches for each speaker using some tidyverse
data-wrangling. The %\>%, group_by(), summarise() and ungroup()
functions are used to concatenate the speeches for each speaker. The
`%>%` command is the pipe function and helps us with a chain of
functions. Think of it as a way to pass the output of one function to
the next function. The paste() function concatenates the speeches for
each speaker.

``` r
speeches <- speeches %>%
  group_by(speaker) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup()

#confirm that you have a total of 20 (very long) concatenated speeches, 1 for each EP speaker
dim(speeches)
```

    ## [1] 20  2

Let’s create a corpus and tokenize it. We will remove punctuation,
symbols, numbers, urls, and separators. We will also remove stopwords
and split hyphens.

``` r
#create a corpus object

corpus_speeches <- corpus(speeches,
                          text_field = "text")

#tokenise the corpus
tokens_speeches <- tokens(corpus_speeches,
                          what = "word",
                          remove_punct = TRUE, 
                          remove_symbols = TRUE, 
                          remove_numbers = TRUE,
                          remove_url = TRUE,
                          remove_separators = TRUE,
                          split_hyphens = FALSE,
                          ) %>%
  tokens_remove(stopwords(source = "smart"), padding = TRUE)
```

MEP speeches are full of jargon and references to politicians. Let’s
append bigram collocations to our tokens object to account for this.

``` r
collocations <- tokens_speeches %>%
  tokens_sample(size = 10, replace = FALSE) %>%
  textstat_collocations(min_count = 20,
                        size = 2:3) %>%
  arrange(-lambda)

head(collocations, 20)
```

    ##               collocation count count_nested length    lambda         z
    ## 147          derek roland    24           24      2 16.419275  9.978630
    ## 148          roland clark    24           24      2 16.419275  9.978630
    ## 142            van rompuy   139          137      2 16.212456 11.181246
    ## 144          nigel farage    85           85      2 15.548680 10.750880
    ## 146         gerard batten    20           19      2 15.393733 10.075052
    ## 149           john bufton    22           22      2 14.297215  9.793065
    ## 121            tony blair    23           10      2 12.922315 17.459101
    ## 73        baroness ashton    58           31      2 12.635547 25.492937
    ## 88          death penalty    33            3      2 11.504525 23.977600
    ## 151           tax evasion    21           10      2 11.481472  8.015170
    ## 152            tax havens    21            8      2 11.481472  8.015170
    ## 134       transaction tax    45           45      2 11.272687 13.522628
    ## 22         prime minister   205          145      2 11.182949 38.110490
    ## 125        united kingdom    61            7      2 11.088553 17.055409
    ## 91           stress tests    26            5      2 10.880890 23.890386
    ## 79        rating agencies    21           11      2 10.734348 24.559792
    ## 139            herman van    20           20      2 10.683896 12.563452
    ## 31            middle east    49            7      2 10.444058 32.640114
    ## 87     youth unemployment    22            4      2  9.934285 24.040316
    ## 130 financial transaction    44           44      2  9.888247 15.165172

Let’s also append collocations of names to our tokens object.

``` r
collocations_names <- tokens_select(tokens_speeches, 
                                    pattern = "[A-Z]", 
                                    valuetype = "regex", 
                                    case_insensitive = FALSE, 
                                    padding = TRUE) %>%
  textstat_collocations(min_count = 10,
                        tolower = FALSE)

head(collocations_names, 20)
```

    ##            collocation count count_nested length    lambda         z
    ## 1         Mr President  2002            0      2  5.838233 151.82831
    ## 2       European Union  1624            0      2  6.409185 125.46152
    ## 3        Member States  1030            0      2  9.641637 100.29248
    ## 4  European Parliament   629            0      2  4.513421  87.99237
    ## 5           Mr Barroso   685            0      2  7.000140  68.13384
    ## 6          Applause Mr   328            0      2  4.479798  64.41126
    ## 7     European Council   368            0      2  3.752149  62.44155
    ## 8        United States   167            0      2  6.741955  58.45611
    ## 9         Member State   138            0      2  6.113949  55.08553
    ## 10       Lisbon Treaty   122            0      2  6.098099  54.74999
    ## 11          Mrs Merkel    82            0      2  8.049259  49.83637
    ## 12     Party Christian   116            0      2 10.413769  48.13534
    ## 13        Central Bank   115            0      2 10.493916  48.11377
    ## 14 Christian Democrats   122            0      2 10.179057  47.33390
    ## 15     Madam President   303            0      2  7.892003  41.96895
    ## 16      Prime Minister   384            0      2 12.421610  40.99329
    ## 17          Farage EFD    57            0      2  9.840650  40.57992
    ## 18   Chancellor Merkel    46            0      2  8.452911  39.84559
    ## 19      Stability Pact    45            0      2  9.204751  39.63545
    ## 20              Mr Van   186            0      2  6.550445  38.77489

If we want to add the most surprising collocations to our tokens object
we can do so using `tokens_compund`. We’ll include only collocations
with a lambda score higher than 5.

``` r
collocations <- collocations %>%
  filter(lambda > 5) %>%
  pull(collocation) %>%
  phrase()

tokens_speeches <- tokens_compound(tokens_speeches, collocations)

collocations_names <- collocations_names %>%
  filter(lambda > 5) %>%
  pull(collocation) %>%
  phrase()

tokens_speeches <- tokens_compound(tokens_speeches, collocations_names)
```

Create a dfm, and change the document names to the speaker names.

``` r
dfm_speeches <- dfm(tokens_speeches)
docnames(dfm_speeches) <- docvars(dfm_speeches, "speaker")
```

## Wordscores

Let’s see if we can use Wordscores to locate these speakers on a
pro-anti EU dimension. We’ll first need to determine reference texts to
anchor this dimension. On the anti-EU side we’ll locate Francesco
Speroni and Nigel Farage, leaders of the then European Freedom and
Democracy group, a Eurosceptic outlet. On the pro-EU dimension we’ll
locate Guy Verhofstadt, leader of the liberal ALDE group, and a pro-EU
voice, as well as Rebecca Harms, the leader of the Greens:

``` r
#append an empty reference_score variable to the speeches_dfm docvars
docvars(dfm_speeches, "reference_score") <- NA

#locate which rows correspond with Guy Verhofstadt and Rebecca Harms (pro_eu) and Francesco Speroni and Nigel Farage (anti_eu)
pro_eu <- which(docvars(dfm_speeches) == "Guy Verhofstadt" | docvars(dfm_speeches) == "Rebecca Harms")
anti_eu <- which(docvars(dfm_speeches) == "Francesco Speroni" |
                 docvars(dfm_speeches) == "Nigel Farage" )

#assign reference scores to Guy Verhofstadt and Rebecca Harms (1) and Francesco Speroni and Nigel Farage (-1)
docvars(dfm_speeches, "reference_score")[pro_eu] <- 1
docvars(dfm_speeches, "reference_score")[anti_eu] <- -1

#inspects the reference_score variable:
docvars(dfm_speeches, "reference_score")
```

    ##  [1] NA NA NA -1 NA NA NA  1 NA NA NA NA NA NA NA NA NA -1  1 NA

``` r
#implement wordscores as per Laver, Benoit, Garry (2003)
speeches_ws <- textmodel_wordscores(dfm_speeches, 
                                    y = docvars(dfm_speeches, "reference_score"), 
                                    scale = c("linear"), 
                                    smooth = 1)
summary(speeches_ws, 10)
```

    ## 
    ## Call:
    ## textmodel_wordscores.dfm(x = dfm_speeches, y = docvars(dfm_speeches, 
    ##     "reference_score"), scale = c("linear"), smooth = 1)
    ## 
    ## Reference Document Statistics:
    ##                      score  total min   max   mean median
    ## Brian Crowley           NA  31873   0 19693 1.6579      0
    ## Cristiana Muscardini    NA  17283   0 10020 0.8990      0
    ## Daniel Cohn-Bendit      NA  43997   0 28970 2.2885      0
    ## Francesco Speroni       -1  35113   0 22680 1.8264      0
    ## Francis Wurtz           NA  23888   0 14093 1.2425      0
    ## Gabriele Zimmer         NA  12010   0  7486 0.6247      0
    ## Graham Watson           NA  33787   0 19169 1.7575      0
    ## Guy Verhofstadt          1 100208   0 65963 5.2124      0
    ## Hannes Swoboda          NA  76949   0 50470 4.0025      0
    ## Jan Zahradil            NA  12504   0  8069 0.6504      0
    ## Joseph Daul             NA  85256   0 51801 4.4346      0
    ## Kathy Sinnott           NA  29791   0 17592 1.5496      0
    ## Lothar Bisky            NA  19654   0 11716 1.0223      0
    ## Martin Callanan         NA  33841   0 21180 1.7603      0
    ## Martin Schulz           NA 125559   0 80209 6.5310      0
    ## Michal Kaminski         NA  41067   0 23806 2.1361      0
    ## Monica Frassoni         NA  24207   0 15369 1.2591      0
    ## Nigel Farage            -1  54482   0 35348 2.8339      0
    ## Rebecca Harms            1  41665   0 26841 2.1672      0
    ## Syed Kamall             NA  14274   0  8981 0.7425      0
    ## 
    ## Wordscores:
    ## (showing first 10 elements)
    ##                            mr_president president-in-office             council 
    ##             0.05087            -0.02449             0.40385             0.86985 
    ##           president          commission     council_meeting               place 
    ##             0.02772             0.50801             0.41980            -0.34024 
    ##               month               focus 
    ##            -0.32974             0.74576

``` r
#sort most discriminant words:

#anti-EU words
head(sort(speeches_ws$wordscores), 10)
```

    ##       referendum     nation_state nigel_farage_efd             ukip 
    ##       -0.9381754       -0.9331389       -0.9310540       -0.9098930 
    ##           anthem              gbp      referendums          frankly 
    ##       -0.9096606       -0.9007205       -0.9005331       -0.8961408 
    ##          croatia          peoples 
    ##       -0.8950178       -0.8929586

``` r
#pro-EU words
tail(sort(speeches_ws$wordscores), 10)
```

    ##        reforms  nuclear_power     copenhagen     discussion        council 
    ##      0.8629029      0.8631816      0.8683533      0.8691359      0.8698531 
    ##          japan     colleagues        finally           task responsibility 
    ##      0.8725772      0.8778326      0.8946250      0.8958954      0.9370709

``` r
#histogram of wordscores
hist(speeches_ws$wordscore, col = "red", border = 0)
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Let’s use the Wordscores model to predict the document scores of the
speeches of the remaining group leaders

``` r
speeches_wordscores_predict <- predict(speeches_ws,
                                       newdata = dfm_speeches, 
                                       se = TRUE)
```

Which speakers are most like Farage and Speroni?

``` r
sort(speeches_wordscores_predict$fit, decreasing = FALSE)[1:5]
```

    ## Francesco Speroni      Nigel Farage     Kathy Sinnott   Martin Callanan 
    ##       -0.06099236       -0.04851617        0.02828642        0.03394538 
    ##       Syed Kamall 
    ##        0.04421186

This lists Kathy Sinnott (I&D), Martin Callanan (ECR) and Syed Kamall
(ECR) as the most anti-EU speakers.

Which speakers are most like Verhofstadt and Harms?

``` r
sort(speeches_wordscores_predict$fit, decreasing = TRUE)[1:5]
```

    ##   Rebecca Harms Guy Verhofstadt     Joseph Daul    Lothar Bisky Monica Frassoni 
    ##      0.12104643      0.10948332      0.08674725      0.08429510      0.07920753

This lists Joseph Daul (EPP), Lothar Bisky (Party of the European Left)
and Monica Frassoni (Green Party) as the most pro-EU speakers.

Visualize the document scores in a plot:

``` r
textplot_scale1d(speeches_wordscores_predict)
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Wordfish

Estimate a Wordfish model and inspect its output. Using the argument
`dir=c(4,8)` set the direction of the dimension so that the document
score for Francesco Speroni (speaker 4) is smaller than the document
score for Guy Verhofdstadt (speaker 8)

``` r
speeches_wf <- textmodel_wordfish(dfm_speeches,
                                  dir = c(4,8))
summary(speeches_wf)
```

    ## 
    ## Call:
    ## textmodel_wordfish.dfm(x = dfm_speeches, dir = c(4, 8))
    ## 
    ## Estimated Document Positions:
    ##                         theta       se
    ## Brian Crowley         1.05917 0.009479
    ## Cristiana Muscardini  0.97864 0.014338
    ## Daniel Cohn-Bendit   -0.49434 0.019145
    ## Francesco Speroni    -1.96876 0.008468
    ## Francis Wurtz         0.74965 0.015628
    ## Gabriele Zimmer       0.52892 0.026280
    ## Graham Watson         0.96471 0.010436
    ## Guy Verhofstadt      -1.06281 0.011414
    ## Hannes Swoboda       -0.15376 0.013937
    ## Jan Zahradil         -0.43788 0.035843
    ## Joseph Daul           0.53703 0.009811
    ## Kathy Sinnott         1.31632 0.006310
    ## Lothar Bisky          0.32721 0.023187
    ## Martin Callanan      -1.10518 0.019295
    ## Martin Schulz         0.15974 0.009911
    ## Michal Kaminski       1.16356 0.007118
    ## Monica Frassoni       0.46479 0.019311
    ## Nigel Farage         -1.88156 0.007884
    ## Rebecca Harms        -0.05245 0.018479
    ## Syed Kamall          -1.09299 0.029858
    ## 
    ## Estimated Feature Scores:
    ##              mr_president president-in-office council president commission
    ## beta -0.4759      -0.5099              0.5428 -0.3811    -0.461    -0.4908
    ## psi   9.9712       4.2270              2.0109  4.2202     3.784     4.3023
    ##      council_meeting   place   month   focus economic affairs european_union
    ## beta         -0.4069 -0.4008 -0.7097 0.03115  -0.4371 -0.3334        -0.4278
    ## psi           0.8236  2.9013  0.9791 1.65594   3.7304  1.1282         4.1719
    ##      proposals created  return economy previous  state implemented immediately
    ## beta   -0.3776 -0.3438 -0.3572 -0.2189  -0.2957 -0.499     -0.2572     -0.4777
    ## psi     2.3698  1.3380  1.6434  2.7351   1.1791  3.129      1.0743      1.2835
    ##      restart   today   speak  future europe  people   claim    care situation
    ## beta -0.2339 -0.5734 -0.0315 -0.1964 -0.428 -0.6226 -0.4094 -0.1093   -0.2699
    ## psi  -1.2713  3.8916  2.2208  3.3096  4.897  4.4407  0.6305  1.1770    3.2091

Let’s take out the word level parameters beta and psi. Beta is the
estimated word position on the underlying dimension and psi is the word
fixed effect.

``` r
wordfish_word_data <- data.frame(beta = speeches_wf$beta,
                            psi = speeches_wf$psi,
                            features = speeches_wf$features)

dim(wordfish_word_data)
```

    ## [1] 19225     3

``` r
head(wordfish_word_data)
```

    ##         beta      psi            features
    ## 1 -0.4759299 9.971192                    
    ## 2 -0.5098591 4.226982        mr_president
    ## 3  0.5427927 2.010872 president-in-office
    ## 4 -0.3811255 4.220164             council
    ## 5 -0.4610271 3.784273           president
    ## 6 -0.4908225 4.302252          commission

``` r
word_plot <- ggplot(data = wordfish_word_data, aes(x = beta, y = psi)) +
    geom_point(pch = 21, fill = "gray", color = "white", size = 0.75) +
  labs(x = "Beta", y = "Psi") + guides(size = "none", color = guide_legend("")) + 
  theme_minimal() +
  geom_text(data=subset(wordfish_word_data, beta > 7 | beta < -4.5 | psi > 4.5),  
            aes(x = beta, y = psi, label = features))

print(word_plot)
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Plot the document positions generated by Wordfish. Alpha is the document
position on the dimension and theta is the document fixed effect.

``` r
#generate a dataframe with document level alpha beta and omega
wordfish_document_data <- data.frame(alpha = speeches_wf$alpha,
                                     theta = speeches_wf$theta,
                                     se = speeches_wf$se.theta,
                                     speaker = speeches_wf$docs)

#order the speaker factor by theta
wordfish_document_data$speaker <- reorder(wordfish_document_data$speaker, 
                                           wordfish_document_data$theta)


#plot wordfish results using ggplot2
wordfish_plot <- ggplot(wordfish_document_data, 
                        aes(x= speaker, 
                            y = theta,
                            ymin = theta -1.96*se,
                            ymax = theta + 1.96*se)) +
  geom_pointrange(fill = "gray", color = "gray", size = .5) +
  theme_minimal() + coord_flip()
print(wordfish_plot)
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Both Wordscores and Wordfish are scaling models and if they pick up on
the same dimension they should give us similar results. Let’s see if
this indeed the case.

``` r
scaling_data <- rbind(data.frame(speeches_wordscores_predict, wordfish_document_data))

scaling_plot <- ggplot(scaling_data, aes(x = fit, 
                                         y = theta, 
                                         label = speaker)) +
  geom_point(pch = 21, fill = "gray25", color = "white", size = 2.5) +
  scale_x_continuous(name = "Wordscore prediction") +
  scale_y_continuous(name = "Wordfish prediction") +
  theme_minimal() + geom_text(aes(label=speaker), 
                                        hjust=0, 
                                        vjust=0, 
                                        size = 2)
  
print(scaling_plot)
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
correlation <- cor.test(x=scaling_data$fit, 
                        y=scaling_data$theta,
                        method = 'pearson')
print(correlation)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  scaling_data$fit and scaling_data$theta
    ## t = 2.4602, df = 18, p-value = 0.02423
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.07598625 0.77264399
    ## sample estimates:
    ##       cor 
    ## 0.5016392

## Latent semantic scaling (LSS)

In a third step, we’ll apply Latent Semantic Scaling (LSS) to the
speeches. LSS is a scaling method that uses a seed list of sentiment
words as a starting point. It then relies on word embeddings to find
nearest neighbors to these seed words. From this information it
calculates `polarity scores` for each word.

In order to apply LSS to the corpus we first need to transform the
corpus at the sentence level and tokenize it

``` r
corpus_speeches_sent <- corpus_reshape(corpus_speeches, to =  "sentences")

tokens_speeches_sent <- tokens(corpus_speeches_sent,
                               what = "word",
                               remove_punct = TRUE, 
                               remove_symbols = TRUE, 
                               remove_numbers = TRUE,
                               remove_url = TRUE,
                               remove_separators = TRUE,
                               split_hyphens = FALSE,
                          ) %>%
  tokens_remove(stopwords(source = "smart"), padding = FALSE)


tokens_speeches_sent <- tokens_compound(tokens_speeches_sent, collocations)
tokens_speeches_sent <- tokens_compound(tokens_speeches_sent, collocations_names)

dfmat_speeches_sent <- tokens_speeches_sent %>% 
  dfm()

topfeatures(dfmat_speeches_sent, 20)
```

    ##       european         europe         people     parliament     commission 
    ##           3644           3357           2203           1872           1866 
    ##   mr_president        council european_union         crisis      countries 
    ##           1735           1697           1625           1409           1394 
    ##           time             eu          today           make             mr 
    ##           1359           1317           1257           1175           1145 
    ##          group      president       economic  member_states       applause 
    ##           1139           1115           1046           1032            985

In this case we rely on the short list of sentiment words as a seed list

``` r
seed <- as.seedwords(data_dictionary_sentiment)
print(seed)
```

    ##        good        nice   excellent    positive   fortunate     correct 
    ##           1           1           1           1           1           1 
    ##    superior         bad       nasty        poor    negative unfortunate 
    ##           1          -1          -1          -1          -1          -1 
    ##       wrong    inferior 
    ##          -1          -1

Using the seed words, LSS computes polarity of words frequent in the
context of `europe*` (which in this example should denote how positive
or negative words are around references of membership). We’ll set the
p-value to 0.05, which means that only words that are significantly more
frequent in the context of `europe*` than in the rest of the corpus will
be included in the analysis.

``` r
# identify context words 
europe <- char_context(tokens_speeches_sent, pattern = "europe*", p = 0.05)
```

In a next step we run the LSS model. We set the number of embeddings
dimensions to 300 and cache the results, which speeds up the process.

``` r
tmod_lss <- textmodel_lss(dfmat_speeches_sent, 
                          seeds = seed,
                          terms = europe, 
                          k = 300, 
                          cache = TRUE)
```

Display the most positive words that appear in the context of Europe

``` r
head(coef(tmod_lss), 20)
```

    ##   convention  enlargement  coordinated  cooperation     interest       manage 
    ##   0.10533731   0.08842771   0.07870697   0.07623279   0.07597778   0.07360272 
    ##      succeed         rome        build    effective       spirit       closer 
    ##   0.06778046   0.06740908   0.06650366   0.06617072   0.06449787   0.06447704 
    ##         show      focused    incapable   unilateral   foundation        works 
    ##   0.06311580   0.06207654   0.05808067   0.05691814   0.05564631   0.05366143 
    ## member_state   determined 
    ##   0.05354966   0.05242009

Most negative words that appear in the context of Europe

``` r
tail(coef(tmod_lss), 20)
```

    ##         schuman       influence           calls         citizen            idea 
    ##     -0.03970908     -0.04027362     -0.04070250     -0.04157411     -0.04165787 
    ##            room    undemocratic         adopted           shape           trend 
    ##     -0.04303863     -0.04417738     -0.04508241     -0.04543940     -0.04569424 
    ##          values       unelected            rest     development         balkans 
    ##     -0.04587243     -0.04616606     -0.04673178     -0.04776851     -0.04838707 
    ##             big         federal       elections             row political_class 
    ##     -0.04875997     -0.04886974     -0.04952429     -0.05955934     -0.07161893

To obtain document-level scores, we use the `dfm_group()` to re-assemble
the sentence-level dfm back at the document-level. We then use
`predict()` to make document level LSS predictions.

``` r
dfmat_doc <- dfm_group(dfmat_speeches_sent)
dat <- docvars(dfmat_doc)
dat$fit <- unlist(predict(tmod_lss, 
                          newdata = dfmat_doc, 
                          se = TRUE)[1])
dat$se <- unlist(predict(tmod_lss, 
                         newdata = dfmat_doc, 
                         se = TRUE)[2])
```

We then plot these predictions, ordering speakers from most positive to
most negative on Europe.

``` r
dat$speaker <- with(dat, reorder(speaker, fit))


lss_plot <- ggplot(arrange(dat, fit), 
                        aes(x= speaker, 
                            y = fit,
                            ymin = fit -1.96*se,
                            ymax = fit + 1.96*se)) +
  geom_pointrange(pch = 21, fill = "gray", color = "gray", size = 0.75) +
  theme_minimal() + coord_flip()
print(lss_plot)
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

## Exercises

For this set of exercises we will use `data_corpus_irishbudget2010` a
corpus that consists of 2010 budget speeches in Ireland. The dataset is
included in the quanteda package.

1.  Tokenize `data_corpus_irishbudget2010`, remove stopwords,
    punctuation characters, and create a dfm called `dfm_budget_debates`

2.  Create a binary variable `ref_score` that equals 1 if the speaker’s
    name is “Lenihan” (i.e., the Minister of Finance at the time) and -1
    if the speaker’s name is “Kenny” (Enda Kenny was leader of the
    opposition at the time). For all other speakers, assign the value
    NA.

3.  Apply a Wordscores model for this document-feature matrix using
    `ref_score` as the value for `y`.

4.  Explore the scores for all words in the dfm using
    `textplot_scale1d()`. Note: set margin to “features”. Why are some
    terms clustered around -1 and +1?

5.  Use `predict()` for predicting the document-level word scores for
    all documents in the dfm. Set `se = TRUE` to add 95% confidence
    intervals.

6.  Apply `textplot_scale1d()` to the object created in question 5. Does
    this method distinguish between government (FF and Green) and
    oppoisiton parties?

Use the `dfm_budget_debates`. Create a new dfm object that only keeps
terms that appear in at least three documents and that appear at least
three times. Call the resulting dfm `dfm_budget_debates_trimmed`

Run a Wordfish model on this dfm.

Use `textplot_scale1d()` to plot (1) document-positions, and scores for
each word. You can achieve this by adjusting the `margin` argument.
