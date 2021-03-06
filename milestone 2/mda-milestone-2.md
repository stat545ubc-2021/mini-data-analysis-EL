*To complete this milestone, you can either edit [this `.rmd`
file](https://raw.githubusercontent.com/UBC-STAT/stat545.stat.ubc.ca/master/content/mini-project/mini-project-2.Rmd)
directly. Fill in the sections that are commented out with
`<!--- start your work here--->`. When you are done, make sure to knit
to an `.md` file by changing the output in the YAML header to
`github_document`, before submitting a tagged release on canvas.*

# Welcome back to your mini data analysis project!

This time, we will explore more in depth the concept of *tidy data*, and
hopefully investigate further into your research questions that you
defined in milestone 1.

**NOTE**: The main purpose of the mini data analysis is to integrate
what you learn in class in an analysis. Although each milestone provides
a framework for you to conduct your analysis, it’s possible that you
might find the instructions too rigid for your data set. If this is the
case, you may deviate from the instructions – just make sure you’re
demonstrating a wide range of tools and techniques taught in this class.

Begin by loading your data and the tidyverse package below:

    library(datateachr) # <- might contain the data you picked!
    library(tidyverse)

    msk <- msk <- read_tsv("https://raw.githubusercontent.com/stat545ubc-2021/mini-data-analysis-EL/main/data/msk_impact_2017_clinical_data.tsv")

    #external datasets are first downloaded and loaded using readr

# Learning Objectives

By the end of this milestone, you should:

-   Become familiar with manipulating and summarizing your data in
    tibbles using `dplyr` and `tidyr`, with a research question in mind.
-   Understand what *tidy* data is, and how to create it. In milestone
    3, we will explore when this might be useful.
-   Generate a reproducible and clear report using R Markdown.
-   Gain a greater understanding of how to use R to answer research
    questions about your data.

**Things to keep in mind**

-   Remember to document your code, be explicit about what you are
    doing, and write notes in this markdown document when you feel that
    context is required. Create your analysis as if someone else will be
    reading it! **There will be 2.5 points reserved for reproducibility,
    readability, and repo organization.**

-   Before working on each task, you should always keep in mind the
    specific **research question** that you’re trying to answer.

# Task 1: Process and summarize your data (15 points)

From milestone 1, you should have an idea of the basic structure of your
dataset (e.g. number of rows and columns, class types, etc.). Here, we
will start investigating your data more in-depth using various data
manipulation functions.

### 1.1 (2.5 points)

First, write out the 4 research questions you defined in milestone 1
were. This will guide your work through milestone 2:

<!-------------------------- Start your work below ---------------------------->

1.  Which `Cancer Type` is the most deadly?
2.  Are patient variables or sample variables more predictive of overall
    survival?
3.  What are the differences in the genomic and mutational variables
    between primary and Metastasis samples?
4.  What are the genomic and mutational consequences of a patient’s
    smoking history?
    <!----------------------------------------------------------------------------->

### 1.2 (10 points)

Now, for each of your four research questions, choose one task from
options 1-4 (summarizing), and one other task from 4-8 (graphing). You
should have 2 tasks done for each research question (8 total). Make sure
it makes sense to do them! (e.g. don’t use a numerical variables for a
task that needs a categorical variable.). Comment on why each task helps
(or doesn’t!) answer the corresponding research question.

Ensure that the output of each operation is printed!

**Summarizing:**

1.  Compute the *range*, *mean*, and *two other summary statistics* of
    **one numerical variable** across the groups of **one categorical
    variable** from your data.
2.  Compute the number of observations for at least one of your
    categorical variables. Do not use the function `table()`!
3.  Create a categorical variable with 3 or more groups from an existing
    numerical variable. You can use this new variable in the other
    tasks! *An example: age in years into “child, teen, adult, senior”.*
4.  Based on two categorical variables, calculate two summary statistics
    of your choosing.

**Graphing:**

1.  Create a graph out of summarized variables that has at least two
    geom layers.
2.  Create a graph of your choosing, make one of the axes logarithmic,
    and format the axes labels so that they are “pretty” or easier to
    read.
3.  Make a graph where it makes sense to customize the alpha
    transparency.
4.  Create 3 histograms out of summarized variables, with each histogram
    having different sized bins. Pick the “best” one and explain why it
    is the best.

Make sure it’s clear what research question you are doing each operation
for!

<!------------------------- Start your work below ----------------------------->

**1. Which `Cancer Type` is the most deadly?**

While overall survival will also reflect other factors such as treatment
and stage of the disease, it will be the best surrogate for the disease
burden of each cancer type.

    # Calculate summary statistics of overall survival for each cancer type
    most_deadly <- msk %>%
      filter(!is.na(`Overall Survival (Months)`)) %>%
      group_by(`Cancer Type`) %>%
      summarize(median_os = median(`Overall Survival (Months)`),
                iqr_os = IQR(`Overall Survival (Months)`),
                mean_os = mean(`Overall Survival (Months)`),
                range_os = diff(range(`Overall Survival (Months)`)),
                n_samples = n()) %>%
      arrange(median_os)

    head(most_deadly, 20)

    ## # A tibble: 20 × 6
    ##    `Cancer Type`                       median_os iqr_os mean_os range_os n_samples
    ##    <chr>                                   <dbl>  <dbl>   <dbl>    <dbl>     <int>
    ##  1 Pineal Tumor                             3.96   3.11    3.96     6.22         2
    ##  2 Sellar Tumor                             4.92   4.12    5.76     7.53         4
    ##  3 Cancer of Unknown Primary                7.36   8.40    8.65    33.2        159
    ##  4 Ampullary Carcinoma                      7.4    5       8.27    13.9          5
    ##  5 Miscellaneous Neuroepithelial Tumor      8.78   3.98   10.0      5.1          5
    ##  6 Small Cell Lung Cancer                   8.99   8.34   10.5     32.8         72
    ##  7 Pancreatic Cancer                        9.12   8.39   10.2     29.7        376
    ##  8 Non-Small Cell Lung Cancer               9.14   9.32   10.5     38.4       1542
    ##  9 Vaginal Cancer                           9.34  10.7    13.3     21.3          3
    ## 10 Small Bowel Cancer                       9.7   12.7    12.1     31.7         28
    ## 11 Head and Neck Cancer                    10.0    9.39   11.1     32.9        147
    ## 12 Hodgkin Lymphoma                        10.1    3.13    9.94     7.1          4
    ## 13 Mature B-Cell Neoplasms                 10.3    8.46   10.1     22.4        124
    ## 14 Ampullary Cancer                        10.3   12.0    10.9     23.0         11
    ## 15 Melanoma                                10.4    9.58   11.4     33.8        322
    ## 16 Salivary Gland Cancer                   10.6   10.3    13.2     37.1         77
    ## 17 Nerve Sheath Tumor                      10.6   12.5    11.4     22.5         11
    ## 18 Embryonal Tumor                         10.8    5.22   11.5     14.2          4
    ## 19 Appendiceal Cancer                      10.9    9.78   12.8     28.5         60
    ## 20 Breast Sarcoma                          11.0    7.80   13.0     24.9         10

    # store the 10 most deadly types for graphing
    most_deadly_types <- most_deadly %>%
      head(10) %>%
      select(`Cancer Type`) %>%
      as_vector()

We can see that 9 cancer types have a median overall survival of under a
year. Generally, these were cancers with a small number of samples, of
unknown primary, or pancreatic and lung cancer. This can be represented
graphically with box plots and dot plots that show the overall survival
for each sample, segregated by `Cancer Type`.

    # We filter the msk dataset to include only the 10 most deadly cancer types defined previously
    # We created a new factor variable for cancer type that is reordered from lowest to highest median overall survival
    # We then generate boxplots and violin plots of the overall survival for the 10 cancer types
    msk %>%
      filter(`Cancer Type` %in% most_deadly_types & !is.na(`Overall Survival (Months)`)) %>%
      mutate(type = fct_reorder(`Cancer Type`, `Overall Survival (Months)`)) %>%
      ggplot(aes(x= type, y = `Overall Survival (Months)`, color = type)) +
      geom_violin(trim=FALSE) +
      geom_boxplot(width = 0.1, outlier.color = "black", outlier.size = 0.5) +
      theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1), legend.position = "none")

![](mda-milestone-2_files/figure-markdown_strict/question1b-1.png)

    # changes to the theme makes the geoms more visually appealing, the axis labels more readable, and the outliers more obvious. 

We visualized the boxplots for the 10 cancer types with the shortest
median overall survival. While Violin plots helped us to peek at the
underlying sample distributions, we must also consider the bias from the
variable sample size of each type.

**2. Are patient variables or sample variables more predictive of
overall survival?**

We will look at the following patient variables: `Smoking History` and
`Sex`. We will look at the following sample variables: `Specimen Type`
and `Specimen Preservation Type`.

    # remove NA values in overall survival and group by chosen patient variables to compute summary statistics
    msk_patient <- msk %>%
      filter(!is.na(`Overall Survival (Months)`)) %>%
      group_by(`Smoking History`, `Sex`) %>%
      summarize(median_os = median(`Overall Survival (Months)`),
                mean_os = mean(`Overall Survival (Months)`),
                count = n())

    print(msk_patient)

    ## # A tibble: 6 × 5
    ## # Groups:   Smoking History [3]
    ##   `Smoking History` Sex    median_os mean_os count
    ##   <chr>             <chr>      <dbl>   <dbl> <int>
    ## 1 Never             Female     12.8    13.8   1869
    ## 2 Never             Male       12.7    13.8   1576
    ## 3 Prev/Curr Smoker  Female     11.5    13.1   1500
    ## 4 Prev/Curr Smoker  Male       11.7    12.8   1822
    ## 5 Unknown           Female      6.18    8.30   747
    ## 6 Unknown           Male        6.25    7.61   628

    # repeated for chosen sample variables
    msk_sample <- msk %>%
      filter(!is.na(`Overall Survival (Months)`) & `Specimen Type` != "#N/A") %>%
      group_by(`Specimen Type`, `Specimen Preservation Type`) %>%
      summarize(median_os = median(`Overall Survival (Months)`),
                mean_os = mean(`Overall Survival (Months)`),
                count = n())

    print(msk_sample)

    ## # A tibble: 10 × 5
    ## # Groups:   Specimen Type [4]
    ##    `Specimen Type` `Specimen Preservation Type` median_os mean_os count
    ##    <chr>           <chr>                            <dbl>   <dbl> <int>
    ##  1 Biopsy          DNA                              13.8    12.4    731
    ##  2 Biopsy          FFPE                             10.2    11.9   3430
    ##  3 CUSA            FFPE                             13.3    13.3      1
    ##  4 Cytology        Cell Pellet                       7.36    7.55    19
    ##  5 Cytology        DNA                              14.3    12.6     86
    ##  6 Cytology        FFPE                              8.97   10.0    419
    ##  7 Cytology        FNA                              11.4    11.4      1
    ##  8 Cytology        Other                            15.2    15.2      1
    ##  9 Resection       DNA                              14.8    13.5    634
    ## 10 Resection       FFPE                             11.2    13.3   2819

From the calculated summary statistics, it appears that
`Smoking History` is associated with overall survival while `Sex` is
not. For sample variables, it appears that both the variables we
consider may be associated with overall survival. We will then generate
a scatter plot of `Fraction Genome Altered` and `Sample coverage`,
coloring the inidividual points patient survival status.

    # NA's in overall survival are removed, before a point geom is generated
    msk %>%
      filter(!is.na(`Overall Survival (Months)`) & !is.na(`Fraction Genome Altered`)) %>%
        ggplot(aes(x = `Fraction Genome Altered`, y = `Sample coverage`, color = `Overall Survival Status`)) +
      geom_point(alpha = 0.25, size = 1)

![](mda-milestone-2_files/figure-markdown_strict/question2b-1.png)

    #alpha is changed to uncover overlapping points

From the plot, it appears that sample with higher coverage were
typically from living patients.

**3. What are the genomic and mutational differences between primary and
Metastasis samples?**

We begin by calculating summary statistics for metastatic and primary
samples.

    # NA's in fraction genome altered is removed before summary statistics are calculated for each sample type
    msk_sample_type <- msk %>%
      filter(!is.na(`Fraction Genome Altered`)) %>%
      group_by(`Sample Type`) %>%
      summarize(median_genome_altered = median(`Fraction Genome Altered`),
                count = n(),
                iqr_genome_altered = IQR(`Fraction Genome Altered`),
                mean_genome_altered= mean(`Fraction Genome Altered`),
                mean_mutation_count = mean(`Mutation Count`),
                median_mutation_count = median(`Mutation Count`))


    print(msk_sample_type)

    ## # A tibble: 2 × 7
    ##   `Sample Type` median_genome_altered count iqr_genome_altered mean_genome_alte…
    ##   <chr>                         <dbl> <int>              <dbl>             <dbl>
    ## 1 Metastasis                    0.194  4730              0.302             0.233
    ## 2 Primary                       0.116  6209              0.224             0.166
    ## # … with 2 more variables: mean_mutation_count <dbl>,
    ## #   median_mutation_count <dbl>

We then plot the density curves of fraction of genome altered for both
types of samples. From our summarize data, we can also plot a vertical
line representing the median for both groups.

    # We remove rows that contains NA values for the fraction genome altered variable
    # A density curve is plotted for each sample type 
    msk %>%
      filter(!is.na(`Fraction Genome Altered`)) %>%
      ggplot(aes(x = `Fraction Genome Altered`, fill = `Sample Type`)) +
      geom_density(aes(color = `Sample Type`), alpha = 0.25) +
      geom_vline(data=msk_sample_type, aes(xintercept=median_genome_altered, color=`Sample Type`),
                 linetype="dashed")

![](mda-milestone-2_files/figure-markdown_strict/question3b-1.png)

    #we can add the median as a vertical line using our summarize data

This illustrates the substantial difference in `Fraction Genome Altered`
between the two samples types.

**4. What are the genomic and mutational consequences of a patient’s
smoking history?**

We do a similar analysis as for question 3, but this time grouping the
data by smoking history. We observe that the differences in the
calculated summary statistics between groups are small.

    # NA's in fraction genome altered and smoking history are removed
    # Summary statistics are calculated for each category of smoking history
    msk_smoking_history <- msk %>%
      filter(!is.na(`Fraction Genome Altered`) & !is.na(`Smoking History`)) %>%
      group_by(`Smoking History`) %>%
      summarize(median_genome_altered = median(`Fraction Genome Altered`),
                count = n(),
                iqr_genome_altered = IQR(`Fraction Genome Altered`),
                mean_genome_altered= mean(`Fraction Genome Altered`),
                median_mutation_count = median(`Mutation Count`),
                mean_mutation_count = mean(`Mutation Count`))

    print(msk_smoking_history)

    ## # A tibble: 3 × 7
    ##   `Smoking History` median_genome_altered count iqr_genome_alte… mean_genome_alt…
    ##   <chr>                             <dbl> <int>            <dbl>            <dbl>
    ## 1 Never                             0.150  4734            0.262            0.198
    ## 2 Prev/Curr Smoker                  0.142  4362            0.256            0.191
    ## 3 Unknown                           0.138  1841            0.270            0.194
    ## # … with 2 more variables: median_mutation_count <dbl>,
    ## #   mean_mutation_count <dbl>

This is then visualized and can be contrasted with the plot generated
for question 3. It is clear that any differences in
`Fraction Genome Altered` between smoking history groups is smaller than
between sample types.

    # NA's in the variables to be plotted are removed
    # A density curve is plotted for each smoking history
    msk %>%
      filter(!is.na(`Smoking History`) & !is.na(`Fraction Genome Altered`)) %>%
      ggplot(aes(x = `Fraction Genome Altered`, fill = `Smoking History`)) +
      geom_density(aes(color = `Smoking History`), alpha = 0.25) +
      geom_vline(data=msk_smoking_history, aes(xintercept = median_genome_altered, color = `Smoking History`),
                 linetype="dashed")

![](mda-milestone-2_files/figure-markdown_strict/question4b-1.png)
<!----------------------------------------------------------------------------->

### 1.3 (2.5 points)

Based on the operations that you’ve completed, how much closer are you
to answering your research questions? Think about what aspects of your
research questions remain unclear. Can your research questions be
refined, now that you’ve investigated your data a bit more? Which
research questions are yielding interesting results?

<!------------------------- Write your answer here ---------------------------->

**1. Which `Cancer Type` is the most deadly?**

From our analysis, we were able to arrange the cancer types in order of
lowest median overall survival, and this showed us a few interesting
observations. First, 5 of the 10 most deadly cancer types identified in
this manner had a sample size of under 10 out of over `nrow(msk)`
samples. As well, since Cancer of Unknown Primary was the third
deadliest cancer, we might be persuaded to make the conclusion that
disease in which we have less knowledge will lead to a lower patient
life expectancy. However, we must also consider that such observations
are more likely attributed to the small sample sizes for certain types
and that cancers of unknown primary would likely be late stage disease
that has metastasized, making it more deadly than disease that is
detected earlier or has yet to metastasize.

**2. Are patient variables or sample variables more predictive of
overall survival?**

In our analysis, we only looked at two patient variables and two sample
variables. While this may have revealed the association between certain
variables and overall survival, this was inherently biased as they were
chosen and not all possible variables were considered. Unfortunately,
the analysis done was of extremely limited scope, and fails to make full
use of the dataset. It remains an open question whether patient or
sample variables are more predictive of overall survival, and we may
have to account for the inherent bias present in the dataset as it
relates to this question (e.g. uneven number of patient and sample
variables).

**3. What are the differences in the genomic and mutational variables
between primary and Metastasis samples?**

From out dataset, we had nearly equal rows of metastatic and primary
samples. Looking at the mean and median of the fraction of the genome
altered in these two types of samples, it is evident that it was
generally higher in the metastatic samples. This was confirmed when we
plotted the density curves of the fraction of the genome altered for
these two groups, as the distribution of the metastatic samples was
shifted to the right. However, this was not reflected in the mean and
median of `Mutation Count` when stratified by sample type, which may
weaken the conclusions that are drawn from looking at only
`Fraction Genome Altered`.

**4. What are the genomic and mutational consequences of a patient’s
smoking history?**

We were able to calculate summary statistics for
`Fraction Genome Altered` and `Mutation Count`, but differences were
small. This was also reflected in the density curves of
`Fraction Genome Altered`grouped by `Smoking History`, which overlapped
significantly. In our analysis, we are only able to show the association
between a patient’s smoking history and the two variables that we used
as surrogate for the genomic and mutation characteristics of the
disease. While the difference in the mean `Mutation Count` between
smokers and non-smokers is intriguing, it may be due to differences in
the types of cancer that smoking is a risk factor for rather than due to
direct effect of smoking on the DNA of the tumour.

<!----------------------------------------------------------------------------->

# Task 2: Tidy your data (12.5 points)

In this task, we will do several exercises to reshape our data. The goal
here is to understand how to do this reshaping with the `tidyr` package.

A reminder of the definition of *tidy* data:

-   Each row is an **observation**
-   Each column is a **variable**
-   Each cell is a **value**

*Tidy’ing* data is sometimes necessary because it can simplify
computation. Other times it can be nice to organize data so that it can
be easier to understand when read manually.

### 2.1 (2.5 points)

Based on the definition above, can you identify if your data is tidy or
untidy? Go through all your columns, or if you have &gt;8 variables,
just pick 8, and explain whether the data is untidy or tidy.

<!--------------------------- Start your work below --------------------------->

In our dataset, each row corresponds to a unique tumor sample from
patients in the MSK-IMPACT Clinical Sequencing Cohort. As well, each
column is a variable that describes either a characteristic of the
patient (e.g. `Patient ID`, and `Smoking History`) or of the sample
itself (e.g. `Cancer Type`, `Oncotree Code`, `Fraction Genome Altered`,
`Metastatic Site`, `Primary Tumor Site`, and `Sample Type`). The data is
structured in a manner in which each cell only contains a single value,
but in some cases it is missing. Therefore, the structure of this
dataset is tidy, despite it being incomplete in the sense that there are
missing values.

    # we select 8 variables from the original dataset to illustrate the data is clean
    msk_tidy <- msk %>%
      select(`Patient ID`, `Cancer Type`, `Oncotree Code`, `Fraction Genome Altered`, `Metastatic Site`, `Primary Tumor Site`, `Smoking History`, `Sample Type`)

    head(msk_tidy, 10)

    ## # A tibble: 10 × 8
    ##    `Patient ID` `Cancer Type`  `Oncotree Code` `Fraction Genom… `Metastatic Sit…
    ##    <chr>        <chr>          <chr>                      <dbl> <chr>           
    ##  1 P-0000004    Breast Cancer  IDC                       0.278  <NA>            
    ##  2 P-0000015    Breast Cancer  IDC                       0.350  Liver           
    ##  3 P-0000023    Mesothelioma   PEMESO                    0.160  <NA>            
    ##  4 P-0000024    Endometrial C… UEC                       0.388  Lung            
    ##  5 P-0000025    Endometrial C… USC                       0      <NA>            
    ##  6 P-0000025    Endometrial C… USC                       0.102  Peritoneum      
    ##  7 P-0000026    Endometrial C… UEC                       0.420  Pelvis          
    ##  8 P-0000027    Mesothelioma   PLEMESO                   0.0295 <NA>            
    ##  9 P-0000030    Non-Small Cel… LUAD                      0.473  Lymph Node      
    ## 10 P-0000034    Bladder Cancer BLCA                      0.159  <NA>            
    ## # … with 3 more variables: Primary Tumor Site <chr>, Smoking History <chr>,
    ## #   Sample Type <chr>

<!----------------------------------------------------------------------------->

### 2.2 (5 points)

Now, if your data is tidy, untidy it! Then, tidy it back to it’s
original state.

If your data is untidy, then tidy it! Then, untidy it back to it’s
original state.

Be sure to explain your reasoning for this task. Show us the “before”
and “after”.

<!--------------------------- Start your work below --------------------------->

We will begin be selecting 8 variables of the original `ncol(msk)`, and
untidying the data so that each row corresponds to a patient by
combining the `Fraction Genome Altered` of primary and metastatic
samples. In the original dataset, each observation corresponded to a
different sample, which may have been from the same patient. Now, if
there are multiple samples from the same patient, they will no longer be
individual rows but rather as multiple observations contained in the
same row.

    # we use pivot wider to reduce the number of rows from the initial data set
    # Sample type is used for the names of the new columns
    # Fraction of genome altered becomes the values for the new variables

    msk_untidy <- msk_tidy %>%
      pivot_wider(id_cols = c(-`Sample Type`, -`Fraction Genome Altered`), 
                  names_from = `Sample Type`,
                  values_from = `Fraction Genome Altered`)

    head(msk_untidy, 10)

    ## # A tibble: 10 × 8
    ##    `Patient ID` `Cancer Type`  `Oncotree Code` `Metastatic Sit… `Primary Tumor …
    ##    <chr>        <chr>          <chr>           <chr>            <chr>           
    ##  1 P-0000004    Breast Cancer  IDC             <NA>             Breast          
    ##  2 P-0000015    Breast Cancer  IDC             Liver            Breast          
    ##  3 P-0000023    Mesothelioma   PEMESO          <NA>             Peritoneum      
    ##  4 P-0000024    Endometrial C… UEC             Lung             Uterus          
    ##  5 P-0000025    Endometrial C… USC             <NA>             Uterus          
    ##  6 P-0000025    Endometrial C… USC             Peritoneum       Uterus          
    ##  7 P-0000026    Endometrial C… UEC             Pelvis           Uterus          
    ##  8 P-0000027    Mesothelioma   PLEMESO         <NA>             Lung            
    ##  9 P-0000030    Non-Small Cel… LUAD            Lymph Node       Lung            
    ## 10 P-0000034    Bladder Cancer BLCA            <NA>             Bladder         
    ## # … with 3 more variables: Smoking History <chr>, Primary <list>,
    ## #   Metastasis <list>

Our data is now untidy, as we have a variable, `Sample Type`, that is in
the header of the table rather than as a column. As well, since some
patients had multiple samples of the same `Sample Type`, some cells
contain more than 1 value. Thus, the data has been untidied. We will now
tidy the data back to its original state, so that Sample Type is again
its own variable, and each row corresponds to a unique sample.

    # restore the Sample Type Column, and combine the values from the two variables in the untidy dataset into a single variable
    msk_retidy <- msk_untidy %>% 
      pivot_longer(cols = c(Primary, Metastasis),
                   names_to = "Sample Type",
                   values_to = "Fraction Genome Altered") %>%
      unnest()

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(`Fraction Genome Altered`)`

    # we must use unnest to regenerate each of the orignal columns as the pivot longer function initially creates a list column
    # rows that contained two values stored as a vector of length 2 in the list column are now individual samples represented in their own row

    head(msk_retidy, 10)

    ## # A tibble: 10 × 8
    ##    `Patient ID` `Cancer Type`  `Oncotree Code` `Metastatic Sit… `Primary Tumor …
    ##    <chr>        <chr>          <chr>           <chr>            <chr>           
    ##  1 P-0000004    Breast Cancer  IDC             <NA>             Breast          
    ##  2 P-0000015    Breast Cancer  IDC             Liver            Breast          
    ##  3 P-0000023    Mesothelioma   PEMESO          <NA>             Peritoneum      
    ##  4 P-0000024    Endometrial C… UEC             Lung             Uterus          
    ##  5 P-0000025    Endometrial C… USC             <NA>             Uterus          
    ##  6 P-0000025    Endometrial C… USC             Peritoneum       Uterus          
    ##  7 P-0000026    Endometrial C… UEC             Pelvis           Uterus          
    ##  8 P-0000027    Mesothelioma   PLEMESO         <NA>             Lung            
    ##  9 P-0000030    Non-Small Cel… LUAD            Lymph Node       Lung            
    ## 10 P-0000034    Bladder Cancer BLCA            <NA>             Bladder         
    ## # … with 3 more variables: Smoking History <chr>, Sample Type <chr>,
    ## #   Fraction Genome Altered <dbl>

<!----------------------------------------------------------------------------->

### 2.3 (5 points)

Now, you should be more familiar with your data, and also have made
progress in answering your research questions. Based on your interest,
and your analyses, pick 2 of the 4 research questions to continue your
analysis in milestone 3, and explain your decision.

Try to choose a version of your data that you think will be appropriate
to answer these 2 questions in milestone 3. Use between 4 and 8
functions that we’ve covered so far (i.e. by filtering, cleaning,
tidy’ing, dropping irrelvant columns, etc.).

<!--------------------------- Start your work below --------------------------->

From our initial 4 research questions, 2 have been selected for the
remainder of this project.

**Are patient variables or sample variables more predictive of overall
survival?**

While our data analysis and visualization has helped us in exploring
this question, it was unable to fully address the scope required to
provide a more comprehensive conclusion. As well, I believe this would
be a question that is better addressed by modelling rather than data
visualization, so I believe it well lend itself well to the next steps
of the project.

**What are the differences in the genomic and mutational variables
between primary and Metastasis samples?**

Our analysis and visualization has already revealed very intriguing
differences between primary and metastasis samples. In the subsequent
steps of this project, these observations can be fully dissected.
Metastasis is also a relevant clinical outcome that may be predictive of
disease burden, it would be interesting to see if this is independent of
other changes associated with metastatic disease.

We will now subset our data in preparation for the next steps of this
project.

    # we begin by selecting variables that are relevant to our research questions, and filtering for NA values as necessary.
    # we combine NA in smoking history with the unknown category, as they effectively have the same meaning
    # we arrange by overall survival to make the dataframe more intuitive for ourselves
    msk_clean <- msk %>%
      select(-`Study ID`, -`Sample ID`, -`Cancer Type Detailed`, -`Matched Status`, -`Sample Class`, -`Sample Collection Source`) %>%
      filter(!is.na(`Overall Survival (Months)`)) %>%
      mutate(`Smoking History` = ifelse(is.na(`Smoking History`), Unknown, `Smoking History`)) %>%
      arrange(desc(`Overall Survival (Months)`))

    head(msk_clean, 10)

    ## # A tibble: 10 × 20
    ##    `Patient ID` `Cancer Type`    `DNA Input` `Fraction Genome … `Metastatic Sit…
    ##    <chr>        <chr>                  <dbl>              <dbl> <chr>           
    ##  1 P-0004023    Breast Cancer           250              0.625  <NA>            
    ##  2 P-0000541    Prostate Cancer         250              0.313  <NA>            
    ##  3 P-0000541    Prostate Cancer         113.             0.0755 Pelvis          
    ##  4 P-0000283    Bladder Cancer          250              0.0197 <NA>            
    ##  5 P-0000382    Uterine Sarcoma         250              0.213  Lung            
    ##  6 P-0000042    Mesothelioma            250              0.210  <NA>            
    ##  7 P-0000196    Breast Cancer           250              0.294  <NA>            
    ##  8 P-0004019    Germ Cell Tumor         230              0.346  Lymph Node      
    ##  9 P-0000095    Germ Cell Tumor         250              0.0303 <NA>            
    ## 10 P-0000344    Non-Small Cell …        250              0.0606 <NA>            
    ## # … with 15 more variables: Mutation Count <dbl>, Oncotree Code <chr>,
    ## #   Overall Survival (Months) <dbl>, Overall Survival Status <chr>,
    ## #   Primary Tumor Site <chr>, Number of Samples Per Patient <dbl>,
    ## #   Sample coverage <dbl>, Sample Type <chr>, Sex <chr>, Smoking History <chr>,
    ## #   Somatic Status <chr>, Specimen Preservation Type <chr>,
    ## #   Specimen Type <chr>, Tumor Purity <dbl>, Patient's Vital Status <chr>

<!----------------------------------------------------------------------------->

*When you are done, knit an `md` file. This is what we will mark! Make
sure to open it and check that everything has knitted correctly before
submitting your tagged release.*

### Attribution

Thanks to Victor Yuan for mostly putting this together.
