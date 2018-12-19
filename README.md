## Exploratory-Analysis-Using-R

This repository has the code from the analysis on IBM-NoAttrition Dataset using various multivariate techniques such as PCA, CA, MCA, PLS, BADA, DICA and MFA.

Multivariate Statistics is the area of statisitcs that deals with the analysis of data sets with more than one variable. The main objective is to study how the variables are related to each other, and how they work in combination to discriminate between the groups on whcih the observations are made.

About the Dataset: IBM-NoAttrition Dataset

Explore important questions such as ‘show me a breakdown of distance from home by job role’ or ‘compare average monthly income by education’. This is a fictional data set created by IBM data scientists.
Note:

Education: 1 'Below College' 2 'College' 3 'Bachelor' 4 'Master' 5 'Doctor'

EnvironmentSatisfaction: 1 'Low' 2 'Medium' 3 'High' 4 'Very High'

JobInvolvement:  1 'Low' 2 'Medium' 3 'High' 4 'Very High'

JobSatisfaction: 1 'Low' 2 'Medium' 3 'High' 4 'Very High'

PerformanceRating:  1 'Low' 2 'Good' 3 'Excellent' 4 'Outstanding'

RelationshipSatisfaction:  1 'Low' 2 'Medium' 3 'High' 4 'Very High'

WorkLifeBalance: 1 'Bad' 2 'Good' 3 'Better' 4 'Best'

### Correlation Plot of the dataset
![unnamed-chunk-4-1](https://user-images.githubusercontent.com/25525725/50240641-e7bffd00-038a-11e9-853b-dda3360be6e5.png)

### Heat Map of the dataset
![unnamed-chunk-52-1](https://user-images.githubusercontent.com/25525725/50240808-61f08180-038b-11e9-9ee7-0e7c193b1e52.png)

#### PCA
Principal Component Analysis is the analysis of data to identify patterns and finding patterns to reduce the dimensions of the dataset with minimal loss of information.

### CA
Correspondence Analysis is a generalized principal component analysis tailored for the analysis of qualitative data. Also commonly known as reciprocal averaging is a multivariate statistical technique proposed by Herman Otto. It is conceptually similar to principal component analysis, but applies to categorical rather than continuous data. In a similar manner to principal component analysis, it provides a means of displaying or summarising a set of data in two-dimensional graphical form.This is a descriptive/exploratory technique designed to analyze simple two-way and multi-way tables containing some measure of correspondence between the rows and columns. The results provide information which is similar in nature to those produced by Factor Analysis techniques, and they allow you to explore the structure of categorical variables included in the table.Originally, ca was created to analyze contingency tables, but, ca is so versatile that it is used with a lot of other data table types.

### MCA
Multiple correspondence analysis (MCA) is an extension of correspondence analysis (CA) which allows one to analyze the pattern of relationships of several categorical dependent variables. MCA is used to analyze a set of observations described by a set of nominal variables. Each nominal variable comprises several levels, and each of these levels is coded as a binary variable. MCA can also accommodate quantitative variables by recoding them as bins.Because MCA has been (re)discovered many times, equivalent methods are known under several different names such as optimal scaling, optimal or appropriate scoring, dual scaling, homogeneity analysis, scalogram analysis, and quantification method. Technically MCA is obtained by using a standard correspondence analysis on an indicator matrix (a matrix whose entries are 0 or 1). The percentages of explained variance need to be corrected, and the correspondence analysis interpretation of inter-point distances needs to be adapted.

### PLS
Partial least square (PLS) methods (also sometimes called projection to latent structures) relate the information present in two data tables that collect measurements on the same set of observations. PLS methods proceed by deriving latent variables which are (optimal) linear combinations of the variables of a data table. When the goal is to find the shared information between two tables, the approach is equivalent to a correlation problem and the technique is then called partial least square correlation (PLSC) (also sometimes called PLS-SVD). In this case there are two sets of latent variables (one set per table), and these latent variables are required to have maximal covariance.

### BADA
Barycentric discriminant analysis (BADA) is a robust version of discriminant analysis that is used to assign, to pre-defined groups (also called categories), observations described by multiple variables.The goal of BADA is to combine the measurements to create new variables (called components or discriminant variables) that best separate the categories. These discriminant variables are also used to assign the original observations or new observations to the a-priori defined categories.Barycentric discriminant analysis is a robust version of discriminant analysis that is used when multiple measurements describe a set of observations in which each observation be- longs to one category (i.e., group) from a set of a priori defined categories. BADA combines the original variables to create new variables that best separate the groups and that can also be used to optimally assign old or new observations to these categories. The quality of the performance is evaluated by cross-validation techniques that estimate the performance of the classification model for new observations. BADA is a very versatile technique that can be declined in several different varieties that can handle, for example, qualitative data and data structured in blocks. This versatility make BADA particularly suited for the analysis of multi-modal and Big data.

### DiCA
The main idea behind DCA is to represent each group by the sum of its observations and to perform a simple CA on the groups by variables matrix. The original observations are then projected as supplementary elements and each observation is assigned to the closest group. The comparison between the a priori and the a posteriori classifications can be used to assess the quality of the discrimination. A similar procedure can be used to assign new observations to categories. The stability of the analysis can be evalu- ated using cross-validation techniques such as jackknifing or bootstrapping.

### DiSTATIS
DiSTATIS is a procedure that combines bootstrap estimation (to estimate the variability of the experimental conditions) and a new 3-way extension of MDS, that can be used to integrate the distance matrices generated by the bootstrap procedure and to represent the results as MDS-like maps. Reliability estimates are expressed as (1) tolerance intervals which reflect the accuracy of the assignment of scans to experimental categories and as (2) confidence intervals which generalize standard hypothesis testing.
The purpose of this study is to determine how musically trained and untrained listeners sort Western classical melodies into clusters based on perceived similarities. Listeners at three expertise levels sorted MIDI and natural excerpts from the piano music of Bach, Mozart, and Beethoven. We analyzed the data using DISTATIS1, which showed an effect of composer with both MIDI and natural stimuli, and an effect of pianist with natural stimuli. However, there was only a weak effect of music training.

























