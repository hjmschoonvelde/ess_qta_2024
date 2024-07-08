# Essex Summer School 2024 — Quantitative Text Analysis

This page contains the materials for the Essex Summer School 2024 course *Introduction to Quantitative Text Analysis*. Materials will be added as we go along.

Instructor: [Martijn Schoonvelde](http://mschoonvelde.com)

You can find the syllabus [here](Syllabus_QTA.pdf).

## Communication

To facilitate communication and interaction throughout the course we will make use of a dedicated [Slack channel](https://essqta24.slack.com).

## Slides

| Date        | Slides           |  Date        | Slides           |
| ------------- |:-------------:| ------------- |:-------------:|
| July  9   | [.pdf](  Slides/Slides_QTA_1.pdf)| July  16   | [.pdf]( ) |
| July  10   | [.pdf]()| July  17   | [.pdf]() |
| July  11   | [.pdf]() | July  18   | [.pdf]() |
| July  12   | [.pdf]()| July  10   |[.pdf]() |
| July  15   | [.pdf]()| July  21   | [.pdf]() |


## Lab Sessions

| Date        | Link           | Solutions           |    
| ------------- |:-------------:|:-------------:|
| July  9   |  [.md]( Lab_sessions/Day_1/Lab_Session_QTA_1.md ) [.Rmd]( Lab_sessions/Day_1/Lab_Session_QTA_1.Rmd )  | [.md]() [.Rmd]() |
| July  10   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |
| July  11   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |
| July  12   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |
| July  15   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |
| July  16   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |
| July  17   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |
| July  18   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |
| July  19   | [.md]() [.Rmd]() | [.md]() [.Rmd]() |

<!-- ## Flash talks

| Name        | Link           | 
| ------------- |:-------------:| 
 -->

## Acknowledgements

I thank Stefan Müller for sharing his lab session materials for his QTA course at UCD.

## Course schedule


*Day 1 - July 9*

 - **Lecture**: What is quantitative text analysis? What will you learn in this course? Developing a corpus.
 
-  **Lab**: Working in RStudio. Working with libraries in R. Working with Quarto / RMarkdown. 

- **Readings**
  - Benoit, K.R. (2020). Text as Data: An Overview. Handbook of Research Methods in Political Science and International Relations. Ed. by L. Curini and R. Franzese. Thousand Oaks: Sage: pp. 461–497.
  - Grimmer, J., Roberts, M.E., and Stewart, B.M. (2022). Text as Data: A New Framework for Machine Learning and the Social Sciences. Princeton: Princeton University Press: chapter 4.

*Day 2 - July 10*

-	**Lecture**: Core assumptions in quantitative text analysis. Representations of text. Preprocessing and feature selection.

-	**Lab**: Working with strings variables. Regular expressions. Cleaning a string vector. Creating a document-feature matrix. 

- **Readings**:
  -  Benoit, K., Watanabe, K., Wang, H, Nulty, P., Obeng, A., Müller, & Matsuo, A. (2018). Quanteda: An R package for the quantitative analysis of textual data. Journal of Open Source Software, 3(30), 774.
  - Baden, C., Pipal, C., Schoonvelde, M. & van der Velden, M.A.C.G., (2022). Three Gaps in Computational Text Analysis Methods for Social Sciences: A Research Agenda. Communication Methods and Measures, 16(1): pp. 1–18.

*Day 3 - July 11*

-	**Lecture**: Advanced text representations. Word embeddings

-	**Lab**: Importing textual data into R. Introduction to **quanteda** (Benoit _et al._, 2018). Inspecting and visualizing a corpus. 

- **Readings**:
  -  Grimmer, J ., Roberts, M.E., and Stewart, B.M. (2022). Text as Data: A New Framework for Machine Learning and the Social Sciences. Princeton: Princeton University Press: chapter 8.
  - Rodriguez, P.L. and Spirling, A., (2022). Word embeddings: What works, what doesn't, and how to tell the difference for applied research. The Journal of Politics, 84(1): pp.101–115.
  
*Day 4 - July 12*

-	**Lecture**: What can we do with dictionaries and how can we validate them? Sensitivity and specificity.

-	**Lab**: Categorizing texts using off-the-shelf and home-made dictionaries. 

- **Readings**
  - Grimmer, J., Roberts, M.E., and Stewart, B.M. (2022). Text as Data: A New Framework for Machine Learning and the Social Sciences. Princeton: Princeton University Press: chapter 16.
  - Rauh, C., (2018). Validating a sentiment dictionary for German political language–a workbench note. Journal of Information Technology & Politics, 15(4): pp.319–343.

*Day 5 - July 15*

-	**Lecture**: Human coding (or machine coding) and document classification using supervised machine learning. Evaluating a classifier.

-	**Lab**: Binary classification of documents using a simple classifier.

- **Readings**:
  - Daniel Jurafsky and James H. Martin (2020). Speech and Language Processing: An Introduc- tion to Natural Language Processing, Computational Linguistics, and Speech Recognition. 3rd edition: Chapter 4
  - Gilardi, F., Alizadeh, M., & Kubli, M. (2023). “ChatGPT Outperforms Crowd-Workers for Text-Annotation Tasks”. Proceedings of the National Academy of Sciences of the United States of America 120 (3): e2305016120.

*Day 6 - July 16*

-	**Lecture**: Supervised, semi-supervised and unsupervised approaches to place text on an underlying dimension. 

-	**Lab**: Wordfish, Wordscores and Latent Semantic scaling.

- **Readings**:

  - Watanabe, K., (2021). Latent semantic scaling: A semisupervised text analysis technique for new domains and languages. Communication Methods and Measures, 15(2), pp.81-102.
  - Schwemmer, C. and Wieczorek, O., (2020). The methodological divide of sociology: Evidence from two decades of journal publications. Sociology, 54(1): pp.3-21.

*Day 7 - July 17*

-	**Lecture**: Understanding topic models. Discussing their pros and cons. 

-	**Lab**: Latent Dirichlet Allocation (LDA) and Structural topics models (STM).

- **Readings**:
 - Roberts, M et al. (2014). Structural topic models for open-ended survey responses. American Journal of Political Science, 58(4), 1064–1082.
 - Grimmer, J.., Roberts, M.E., and Stewart, B.M. (2022). Text as Data: A New Framework for Machine Learning and the Social Sciences. Princeton: Princeton University Press: chapter 13.

*Day 8 - July 18*

-	**Lecture**: New developments in data.  Multilingualism. Automated speech recognition. Images as data.

-	**Lab**: Linguistic preprocessing of text. POS tagging and lemmatizing using **udipe** (Wijffels, 2022)

- **Readings**:
  - Proksch, S.O., Wratil, C. and Wäckerle, J., (2019). Testing the validity of automatic speech recognition for political text analysis. Political Analysis, pp. 1–21
  - De Vries, E., Schoonvelde, M. & Schumacher, G., (2018). No longer lost in translation: Evidence that Google Translate works for comparative bag-of-words text applications. Political Analysis, 26(4), pp. 417–430.
  - Schwemmer, C., Unger, S. and Heiberger, R., (2023). 15. Automated image analysis for studying online behaviour. Research Handbook on Digital Sociology, p.278.

*Day 9 - July 19*

-	**Lecture**: Deep learning. Transfer Learning. LLMs. Concluding remarks 

-	**Lab**: Training a word embeddings model and inspecting document vectors using **text2vec** (Selivanov _et al_ 2022)

- **Readings**:
   - Chan, C.H., (2023). grafzahl: fine-tuning Transformers for text data from within R. Computational Communication Research, 5(1) p.76–84.
   - Laurer, M., Van Atteveldt, W., Casas, A. & Welbers, K., (2024). Less annotating, more classifying: Addressing the data scarcity issue of supervised machine learning with deep transfer learning and BERT-NLI. Political Analysis, 32(1) pp. 84–100.
   - Bail, C.A., (2024). Can Generative AI improve social science? Proceedings of the National Academy of Sciences, 121(21) p.e2314021121.


