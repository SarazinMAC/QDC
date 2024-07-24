# The *Querelle des Colleges* repository

Welcome to the repository for the *Querelle des Colleges* (QdC).

It contains dynamic network visualisations from two publications:

Tidman G. (2023). *The Emergence of Literature in Eighteenth-Century France: The Battle of the School Books*. Liverpool: Liverpool University Press. 

Authors (Submitted). *Movers and Haters. Dynamic Network Visualisation and Analysis of an Historical French Controversy*

ARTICLE 

## Organisation of the repo

The dynamic visualisations prepared for Tidman (2023) can be found within the repo's **docs** directory.

All other files and directories relate to the analyses carried out for Authors (Submitted):

- **article_figures_and_appendices** directory: contains outputs (figures and appendices) used in Authors (Submitted). 
- **original_qdc_datasets** directory: contains versions of the original dataset used to create all QdC statistics and visuals. Files are dated, with dates contained in file names; all current statistics and visuals for Authors (Submitted) were prepared from QDC_2024_06_07.xlsx.
- **dynamic_network_visuals** directory: contains three dynamic network visualisations each from the "text" and "person" networks of the QdC. Three visualisations were created for each network as, while all visualisations were created using the same procedures (in particular, the Kamada-Kawai force-directed algorithm for determining node position), those procedures can result in different node positions; The three visuals therefore serve to mitigate against biases that could occur from overly relying on single visualisations. 
- **community_network_visuals** directory: contains the static network visualisations mentioned in the "Community" subsection of the Results section of Authors (Submitted).
- **charts** directory: contains line graphs and histograms from the "centrality" and "community" subsections (Results section) of the article
- **stats** directory: contains statistics from the person and text networks of the QdC that are reported in the article (in each subsection of the Results section)

## Reproducing analyses from Authors (Submitted)

All network analyses and visualisations for the article were produced using R version 4.1.3. A table of packages and package versions used for the analyses can be found below.

The "main.R" file, contained within the repo's root directory, can be used to reproduce the article analyses, as well as the dynamic visualisations. Each analysis or visualisation can be produced by setting the correct configurable values in lines 1-32 of the file, setting the relevant object value to TRUE, and running the entire file. Guidance on how to set each configurable value is included within the "main.R" file.

For example, to produce the dynamic visualisations, the produce_dynamic_visual object should be set to TRUE (i.e. the beginning of line XX should be "produce_dynamic_visual <- TRUE"). the text_or_pers object in line XX should be set to "text" to produce the dynamic text network visual and to "pers" to produce the dynamic person network visual. The code for producing the dynamic text and person visuals can respectively be found in the "produce_dynamic_text_network.R" and "produce_dynamic_pers_network.R" files.

Other article analyses, listed below according to article section, can be run by setting the following objects to true:

### 3 Method

- Table 1: "run_ad_hoc_analyses" should be set to TRUE. Results are stored in the "table_1_results" object. Code can be found in the "ad_hoc_analyses.R" file

### 4 Results: Centrality

- Figures 7, 8: can be produced by setting "produce_centrality_histogram" to TRUE, and setting "histogram_measure" to "degree". Figures are produced by the "produce_histograms.R" file.
- Statistics on ties sent/received by various actors (lines 323-325, 370-371): produced by setting "produce_statistics_by_slice_or_year" to TRUE. This runs the "QDC_stats_by_slice.R" file, which produces .xlsx files containing various network statistics by slice or year of the QdC, either for the person or text network. The files are produced in the "stats" directory of the repository. The statistics themselves can then be retrieved from the .xlsx files, in the files' "degree" and "degree_neg" sheets, which show degree values per year or slice of the *Querelle* (to find the final degree values over the entire querelle, go to the rightmost columns in those sheets).
- Statistics on texts published in 1762 and 1763 and their references (lines 371-373): produced by setting "run_ad_hoc_analyses" to TRUE. Results are stored in the "XXX" objects.
- Statistics on texts referring to Rousseau (1762) and La Chalotais (1763) (lines 409-416): produced by setting "run_ad_hoc_analyses" to TRUE. Results are stored in the "XXX" objects.
- Statistics on texts receiving no references: obtained from the .xlsx files produced by setting "produce_statistics_by_slice_or_year" to TRUE. See the "indegree" sheet within the outputted files.

### 4 Results: Community

- Network visualisations resulting from the Louvain and Leiden community detection algorithms (Figures 9-12, Appendices F.1-F.3):  Produced by setting "produce_community_visuals" to TRUE. Uses the Louvain or Leiden algorithms depending on the configurable "cd_algorithm" value set within main.R.
- Modularity statistics (Appendices C-D) and charts (Figure 13, Appendix E) resulting from running the Louvain and Leiden algorithms: Produced by setting "produce_modularity_stats" to TRUE. Uses the Louvain or Leiden algorithms depending on the configurable "cd_algorithm" value set within main.R.
- Co-references between La Chalotais, Rousseau, D'Alembert, and De l'Education Publique: produced by setting "run_ad_hoc_analyses" to TRUE. Results are stored in the "XXX" objects.

### 4 Results: Preferential Attachment

- Statistics on Borrelly's undirected eigenvector centrality: obtained from the .xlsx files produced by "QDC_stats_by_slice.R".
- Borrelly's intervention leading to a drop in modularity: See 'Modularity statistics and charts' mentioned above.

### 4 Results: Balance

- Statistics on D'Alembert's degree and closeness centrality: obtained from the .xlsx files produced by "QDC_stats_by_slice.R".
- Analysis on balance in the ties sent to D'Alembert and Leroy: obtained from the dynamic visualisations. Can also be inspected by running lines X-X of the "ad_hoc_analyses.R" file.
