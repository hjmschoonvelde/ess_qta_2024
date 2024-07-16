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
    ## 150            van rompuy    90           90      2 15.689997 10.850267
    ## 153          21st century    22            4      2 13.342757  9.262196
    ## 113       baroness ashton    65           24      2 13.312609 19.490070
    ## 154           tax evasion    20            6      2 11.627708  8.111530
    ## 142       transaction tax    42           42      2 11.401741 13.654869
    ## 104         death penalty    26            0      2 11.239816 22.273708
    ## 73        rating agencies    23           12      2 10.830812 25.447617
    ## 96   progressive alliance    28            0      2 10.722625 23.292196
    ## 10         prime minister   226          147      2 10.603158 46.221296
    ## 131        united kingdom    37            7      2 10.546055 16.039693
    ## 119       redemption fund    37           10      2 10.457767 18.498362
    ## 30            middle east    56           13      2 10.340615 34.556555
    ## 51    christian democrats    50           43      2  9.991396 29.109468
    ## 46         interest rates    56           29      2  9.987648 30.335806
    ## 15            EUR billion    93           39      2  9.855727 42.692301
    ## 69          latin america    22            4      2  9.805999 26.265274
    ## 135 financial transaction    41           41      2  9.784384 14.982553
    ## 120         mrs malmström    23           11      2  9.525355 18.310931
    ## 20         speaker agreed    73            0      2  9.447520 40.145146
    ## 75     youth unemployment    26            5      2  9.282414 25.258173

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
    ## Brian Crowley           NA  31851   0 19693 1.6563      0
    ## Cristiana Muscardini    NA  17269   0 10020 0.8980      0
    ## Daniel Cohn-Bendit      NA  43988   0 28970 2.2875      0
    ## Francesco Speroni       -1  35183   0 22680 1.8296      0
    ## Francis Wurtz           NA  23883   0 14093 1.2420      0
    ## Gabriele Zimmer         NA  12003   0  7486 0.6242      0
    ## Graham Watson           NA  33806   0 19169 1.7580      0
    ## Guy Verhofstadt          1 100041   0 65963 5.2023      0
    ## Hannes Swoboda          NA  76886   0 50470 3.9982      0
    ## Jan Zahradil            NA  12497   0  8069 0.6499      0
    ## Joseph Daul             NA  85237   0 51801 4.4325      0
    ## Kathy Sinnott           NA  29796   0 17592 1.5495      0
    ## Lothar Bisky            NA  19666   0 11716 1.0227      0
    ## Martin Callanan         NA  33839   0 21180 1.7597      0
    ## Martin Schulz           NA 125564   0 80209 6.5296      0
    ## Michal Kaminski         NA  41073   0 23806 2.1359      0
    ## Monica Frassoni         NA  24188   0 15369 1.2578      0
    ## Nigel Farage            -1  54564   0 35348 2.8374      0
    ## Rebecca Harms            1  41699   0 26841 2.1684      0
    ## Syed Kamall             NA  14279   0  8981 0.7425      0
    ## 
    ## Wordscores:
    ## (showing first 10 elements)
    ##                            mr_president president-in-office             council 
    ##             0.05174            -0.02354             0.40430             0.85732 
    ##           president          commission             meeting               place 
    ##             0.02869             0.50881             0.65403            -0.33963 
    ##               month               focus 
    ##            -0.32878             0.74594

``` r
#sort most discriminant words:

#anti-EU words
head(sort(speeches_ws$wordscores), 10)
```

    ##       referendum nigel_farage_efd             ukip           anthem 
    ##       -0.9380516       -0.9309601       -0.9305978       -0.9095479 
    ##           nation              gbp      referendums          frankly 
    ##       -0.9028548       -0.9005957       -0.9004102       -0.8959805 
    ##          croatia          peoples 
    ##       -0.8948855       -0.8928074

``` r
#pro-EU words
tail(sort(speeches_ws$wordscores), 10)
```

    ##        council        reforms  nuclear_power     copenhagen     discussion 
    ##      0.8573181      0.8632123      0.8632699      0.8684927      0.8693547 
    ##          japan     colleagues        finally           task responsibility 
    ##      0.8727019      0.8780754      0.8948382      0.8960902      0.9371553

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
    ##       -0.06169123       -0.04902033        0.02839818        0.03387351 
    ##       Syed Kamall 
    ##        0.04443755

This lists Kathy Sinnott (I&D), Martin Callanan (ECR) and Syed Kamall
(ECR) as the most anti-EU speakers.

Which speakers are most like Verhofstadt and Harms?

``` r
sort(speeches_wordscores_predict$fit, decreasing = TRUE)[1:5]
```

    ##   Rebecca Harms Guy Verhofstadt     Joseph Daul    Lothar Bisky Monica Frassoni 
    ##      0.12165815      0.10884050      0.08644044      0.08428174      0.07916154

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
    ## Brian Crowley         1.05544 0.009407
    ## Cristiana Muscardini  0.97493 0.014242
    ## Daniel Cohn-Bendit   -0.48845 0.019158
    ## Francesco Speroni    -1.98454 0.008672
    ## Francis Wurtz         0.74547 0.015556
    ## Gabriele Zimmer       0.52748 0.026147
    ## Graham Watson         0.96193 0.010348
    ## Guy Verhofstadt      -1.06331 0.011520
    ## Hannes Swoboda       -0.14991 0.013906
    ## Jan Zahradil         -0.42436 0.035834
    ## Joseph Daul           0.53757 0.009746
    ## Kathy Sinnott         1.30891 0.006284
    ## Lothar Bisky          0.32354 0.023114
    ## Martin Callanan      -1.09941 0.019526
    ## Martin Schulz         0.16446 0.009856
    ## Michal Kaminski       1.15290 0.007134
    ## Monica Frassoni       0.47184 0.019120
    ## Nigel Farage         -1.89536 0.008091
    ## Rebecca Harms        -0.03372 0.018330
    ## Syed Kamall          -1.08542 0.030221
    ## 
    ## Estimated Feature Scores:
    ##              mr_president president-in-office council president commission
    ## beta -0.4808      -0.5151              0.5458  -0.385    -0.465    -0.4948
    ## psi   9.9713       4.2271              2.0076   4.253     3.785     4.3026
    ##      meeting   place   month   focus economic affairs european_union proposals
    ## beta -0.4012 -0.4075 -0.7167 0.02968  -0.4427 -0.3369        -0.4337   -0.3825
    ## psi   2.1567  2.9013  0.9784 1.65512   3.7305  1.1283         4.1719    2.3699
    ##      created  return economy previous   state implemented immediately restart
    ## beta -0.3479 -0.3626 -0.2235  -0.2987 -0.6403     -0.2603     -0.4805 -0.2376
    ## psi   1.3381  1.6435  2.7351   1.1792  3.1823      1.0743      1.2839 -1.2714
    ##        today    speak  future  europe  people   claim    care situation
    ## beta -0.5782 -0.03543 -0.2012 -0.4331 -0.6289 -0.4143 -0.1188   -0.2663
    ## psi   3.8918  2.22059  3.3096  4.8975  4.4404  0.6306  1.1777    3.1539

Let’s take out the word level parameters beta and psi. Beta is the
estimated word position on the underlying dimension and psi is the word
fixed effect.

``` r
wordfish_word_data <- data.frame(beta = speeches_wf$beta,
                            psi = speeches_wf$psi,
                            features = speeches_wf$features)

dim(wordfish_word_data)
```

    ## [1] 19230     3

``` r
head(wordfish_word_data)
```

    ##         beta      psi            features
    ## 1 -0.4807875 9.971348                    
    ## 2 -0.5151047 4.227079        mr_president
    ## 3  0.5458280 2.007618 president-in-office
    ## 4 -0.3849784 4.253342             council
    ## 5 -0.4650497 3.784531           president
    ## 6 -0.4947633 4.302551          commission

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
    ## t = 2.5007, df = 18, p-value = 0.02228
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.08417074 0.77594275
    ## sample estimates:
    ##       cor 
    ## 0.5077782

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
    ##        council   mr_president european_union         crisis      countries 
    ##           1756           1735           1625           1409           1394 
    ##           time             eu          today           make          group 
    ##           1359           1317           1257           1175           1139 
    ##      president             mr       economic  member_states       applause 
    ##           1115           1104           1046           1032            985

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

    ##   convention  enlargement  coordinated  cooperation        build     interest 
    ##   0.11470282   0.08717125   0.08463248   0.07687663   0.07665662   0.07572025 
    ##       manage    incapable      succeed         rome       closer    effective 
    ##   0.07511917   0.07136388   0.06776148   0.06746154   0.06277461   0.06274659 
    ##     socially member_state  involvement       spirit        ideal        daily 
    ##   0.06250453   0.06171761   0.06148405   0.06052453   0.05960473   0.05516287 
    ##        works      focused 
    ##   0.05379949   0.05291851

Most negative words that appear in the context of Europe

``` r
tail(coef(tmod_lss), 20)
```

    ##         idea undemocratic    influence      growing      citizen    unelected 
    ##  -0.03853310  -0.03891500  -0.03996760  -0.04021670  -0.04130685  -0.04134743 
    ##        shape         mark         room       values civilisation        class 
    ##  -0.04139160  -0.04185185  -0.04335514  -0.04416131  -0.04464318  -0.04544961 
    ##      adopted      federal    elections  development          big         rest 
    ##  -0.04581402  -0.04653197  -0.04658340  -0.04747090  -0.05087080  -0.05175101 
    ##      balkans          row 
    ##  -0.05224911  -0.05534609

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

``` r
dfm_budget_debates <- data_corpus_irishbudget2010 %>%
  tokens(remove_punct = TRUE) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  dfm()
```

2.  Create a binary variable `ref_score` that equals 1 if the speaker’s
    name is “Lenihan” (i.e., the Minister of Finance at the time) and -1
    if the speaker’s name is “Kenny” (Enda Kenny was leader of the
    opposition at the time). For all other speakers, assign the value
    NA.

``` r
ref_score <- rep(NA, nrow(dfm_budget_debates))

government <- which(docvars(dfm_budget_debates, "name") == "Lenihan")
opposition <- which(docvars(dfm_budget_debates, "name") == "Kenny")

ref_score[government] = 1
ref_score[opposition] = -1
```

3.  Apply a Wordscores model for this document-feature matrix using
    `ref_score` as the value for `y`.

``` r
budget_debates_ws <- textmodel_wordscores(dfm_budget_debates, 
                                          y = ref_score)
```

4.  Explore the scores for all words in the dfm using
    `textplot_scale1d()`. Note: set margin to “features”. Why are some
    terms clustered around -1 and +1?

``` r
textplot_scale1d(budget_debates_ws, margin = "features")
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

5.  Use `predict()` for predicting the document-level word scores for
    all documents in the dfm. Set `se = TRUE` to add 95% confidence
    intervals.

``` r
pred_ws <- predict(budget_debates_ws, 
                   newdata = dfm_budget_debates, 
                   se = TRUE)
```

    ## Warning: 2832 features in newdata not used in prediction.

6.  Apply `textplot_scale1d()` to the object created in question 5. Does
    this method distinguish between government (FF and Green) and
    oppoisiton parties?

``` r
textplot_scale1d(pred_ws)
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

Use the `dfm_budget_debates`. Create a new dfm object that only keeps
terms that appear in at least three documents and that appear at least
three times. Call the resulting dfm `dfm_budget_debates_trimmed`

``` r
dfm_budget_debates_trimmed <- dfm_budget_debates %>% 
  dfm_trim(min_termfreq = 3, min_docfreq = 3)
```

Run a Wordfish model on this dfm.

``` r
tmod_wf <- textmodel_wordfish(dfm_budget_debates_trimmed)
```

Use `textplot_scale1d()` to plot (1) document-positions, and scores for
each word. You can achieve this by adjusting the `margin` argument.

``` r
textplot_scale1d(tmod_wf, margin = "documents")
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
textplot_scale1d(tmod_wf, margin = "features")
```

![](Lab_Session_QTA_6_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->
