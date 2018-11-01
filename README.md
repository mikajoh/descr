### Replication material for "Do citizens make inferences from political candidate characteristics when aiming for substantive representation?"
Authored by [Dominik Duell](mailto:dominik.duell@essex.ac.uk), [Sveinung Arnesen](sveinung.arnesen@uib.no), and [Mikael Poul Johannesson](mailto:mikajoh@gmail.com).

> We elicit citizens' preferences over hypothetical candidates by applying conjoint survey experiments within a probability-based online panel of the Norwegian electorate. Our experimental treatments differ in whether citizens receive information about candidates' social characteristics only, candidates' issue positions only, or both. From this, we identify whether citizens are able to infer substantive policy positions from the descriptive characteristics of potential representatives and use that information to make candidate choices that achieve substantive representation. We find that candidate choice is driven more by knowledge about candidates' issue positions than by knowledge about their social characteristics and that citizens value substantive representation more robustly than descriptive representation. Importantly, while the direct experimental test of whether voters use the information they obtain from descriptive markers to choose a candidate that gives them substantive representation is inconclusive, we find that voters form beliefs about candidates' issue positions based solely on candidates’ social characteristics.

If you any questions about the code or data herein please feel free to contact me at [mikajoh@gmail.com](mailto:mikajoh@gmail.com).

#### Includes
- `data/`
  - `data-experiment-one.csv`: Prepared data for the candidate choice experiment.
  - `data-experiment-two.csv`: Prepared data for the candidate attitude prediction experiment.
  - `data-respondents.csv`: Prepared data on respondents from the Norwegian Citizen Panel (NCP).
  - `labels-var.xslx`: Spreadsheet with treatment labels used for the figures.
- `output/`: The figures produced with `02_analysis.R`.
- `raw/`: Raw data*
- `00_utils.R`: Utility functions used in the analysis, including the AMCE estimator function.
- `01_data.R` Prepares the raw data (from the Norwegian Citizen Panel).
- `02_analysis.R`: The code to reproduce the figures in the paper. It depends on the prepared data in `/data` and `00_utils.R`.
- `03_appendix.R`: Reproduces the figures and tables in the appendix. Also depends on the prepared data in `/data` and `00_utils.R`.

#### *The raw data

The raw NCP data cannot be shared openly at the moment due data protection laws, so only the prepared data is included here.
However, the raw data is available for researchers via the [NSD website](http://www.nsd.uib.no/nsddata/serier/norsk_medborgerpanel_eng.html). Or just email me.

The raw data used in the paper is from the combined wave 1-7 (english) NCP.
The Md5 checksum of the file is `fa8591edb6c48ecf3933ac3fcfcc791a` (see `tools::md5sum()` and `01_data.R`).
