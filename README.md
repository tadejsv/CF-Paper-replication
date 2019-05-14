# CF-Paper-replication
Replication of [Philippon and Guitierrez (2017), “Investment less Growth: An Empirical Investigation”](https://www.brookings.edu/bpea-articles/investment-less-growth-an-empirical-investigation/)

## Description of work

The replication of the paper involved two main parts:
- replication of data cleaning/preparation and aggregation
- the replication of regressions

We entirely rewrote the data part (translated it from Stata to Python/R), while for regressions/graph production we relied on the code provided by the authors (we would have ran the exact same regressions anyways). In obtaining the data, we make extensive use of various APIs (the authors here mainly downloaded files manually) to ensure replicability and enable extensibility of results to new data, if needed. Among other things, this involved interaction with raw BEA API output in Python and extensive SQL querying of the various WRDS libraries (which are often very poorly documented) - bellow we provide a list of external and internal datasets that we had to work with (external meaning that we obtained all data from that source on our own).

There were only two parts of the paper that we omitted:
- The simulation of a macroeconomics model (Matlab/Dynare)
- Results using the data on Multinational Corporations (MNE) from BEA (mainly the Globalization part in the paper)

We omitted the model simulation as this is not really related to the course, while the MNE results were omitted because of time constraints (the BEA API is not internally consistent, and MNE is the hardest one to query).

Additionally, we decided not to extend the model to recent data, due to the fact that the paper itself is already pretty recent (the maximum possible extension would have been by two years), and the fact that post-2016 data availability was pretty patchy - half the BEA data was only availible up to 2017, the Bushee dataset has not been updated since 2015, the ratings table that the authors used from Compustat was discontinued (!) in 2017, etc. 

## Code

The majority of code is written in Python (in the form of Jupyter Notebooks), and the Census part is written in R. All the data, except for some BEA (explained in comments) and Census data is downloaded automatically by the scripts (Census has to be downloaded by hand, as it has no usable API). 

We decided to follow the main structure of the replication files (for better comparability and easier "debugging"), so the files are named similarly to the ones in the replication files. They should be executed in alphabetical order (i.e., "A-B" should go first, "C-D" second, etc.). After all the "alphabetical" files are ran, the two main datasets are produced in `Data/Final`. The numerical stata files from the replication package can be safely executed against them (but make sure to match the names in the stata files to the name of the files).

The running time should not exceed 15 minutes (it should be executed on the server, some parts take advantage of parallel computation).

## Results

The results are availible in the `results.pdf` file - they consist of tables (mainly regression results) and graphs from the original paper (appendix results are saved as raw images/.tex files in Tables and Figures folders). We find that our results are generally consistent with the original paper - perhaps sometimes less statistically significant. 

The main difference that we noticed is the spike in many graphs around 1996/97 - we explain the reason for it in the comments bellow. Additionally, based on some manual (test) calculations that the authors provide in the code (and out replication of these calculations) we are able to deduce that some difference also stems from some data sources (mainly BEA) being retroactively changed.

## Comments

During the replication, we noticed a few things that we think are worth mentioning:
- The Industrial Value Added/Gross Ouptput data from BEA is availible only from 1997. Previous data is, as BEA warns, not compatible with the new data (which is calculated according to 2012 revision), but it is still availible to download as Excel files. Based on the code, the authors were using a joint (new and old data) excel file here, which has already been edited by them. We are not sure whether they applied any normalizations to join the data, or if at the time they donwloaded the data it was still availible in the old, unrevised format (we believe that this is unlikely, as the revision was done in 2012).
- The bank dependence variable `bankdep` appears to be miscoded in the replication files: it is defined as
```
replace bankdep = 1 if sprating ~= . & ltd > 0, 
```
which means that it is set to 1 (bank-dependent firm) is the credit rating of that firm is **not** missing - it should have been the opposite. However, in our results the `bankdep` coefficient kept the same sign (and remained insignificant), so perhaps they corrected this mistake later on.


## Data sources

### External
- [**FRED**](https://fred.stlouisfed.org/): Macro level data
- [**BEA**](https://www.bea.gov/data): Industry-level data on fixed assets, GDP components by industry ~~and multinational corporations~~
- [**WRDS**](https://wrds-web.wharton.upenn.edu/wrds/): Contains Compustat, CRSP, and Thomson Reuters data.
- [**Census**](https://www.census.gov/): US Census 

### Provided by the authors
- Various custom datasets (Bushee's owner classification, Regulation index, etc.) 
- Linking tables between some of the datasets (external and provided by authors).
