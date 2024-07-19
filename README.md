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

Files contained within the main repo directory can be used to reproduce the article analyses, as well as the dynamic visualisations. 

The dynamic visualisations, respectively for the text and person networks, can be produced by running the "produce_dynamic_text_network.R" and "produce_dynamic_pers_network.R" files.

The files that can be used to produce the article analyses are listed below according to article section:

### Method

- Table 1: can be produced by running the main processing file and setting "run_ad_hoc_analyses" to TRUE. Results are stored in the "table_1_results" object

### Results: Centrality

- Figures 1a, 1b, 2a, 2b: can be produced by running the "produce_histograms.R" file
- Statistics on ties sent/received by various actors (lines XXX, XXX): produced by running the "QDC_stats_by_slice.R" file. This file produces .xlsx files containing various network statistics by slice of the QdC, either for the person or text network (depending on the configurable "text_or_pers" value chosen at the top of the file). The statistics themselves can then be retrieved from the .xlsx files.
- Statistics on texts published in 1762 and 1763 and their references (lines 371-373): produced by running the ad-hoc analyses.
- Statistics on texts referring to Rousseau (1762) and La Chalotais (1763) (lines XXX): produced by running lines X-X of the "ad_hoc_analyses.R" file.
- Statistics on texts receiving no references: obtained from the .xlsx files produced by "QDC_stats_by_slice.R".
- Statistics on the closeness centrality of Rousseau (1762)/D'Alembert (1753): obtained from the .xlsx files produced by "QDC_stats_by_slice.R".

### Results: Community

- Modularity statistics (Figure X, Figure Y) and charts resulting from running the Louvain and Leiden algorithms: Produced by running the "produce_modularity_stats.R" file. The file produces Louvain or Leiden statistics based on the configurable "cd_algorithm" value set at the top of the file.
- Network visualisations resulting from the community detection algorithms:  Produced by running the "produce_community_visuals.R" file. The file produces Louvain or Leiden statistics based on the configurable "cd_algorithm" value set at the top of the file.
- Co-references between La Chalotais, Rousseau, D'Alembert, and De l'Education Publique: produced by running the "co_reference_analysis.R" file.

### Results: Preferential Attachment

- Statistics on Borrelly's undirected eigenvector centrality: obtained from the .xlsx files produced by "QDC_stats_by_slice.R".
- Borrelly's intervention leading to a drop in modularity: See 'Modularity statistics and charts' mentioned above.

### Results: Balance

- Statistics on D'Alembert's degree and closeness centrality: obtained from the .xlsx files produced by "QDC_stats_by_slice.R".
- Analysis on balance in the ties sent to D'Alembert and Leroy: obtained from the dynamic visualisations. Can also be inspected by running lines X-X of the "ad_hoc_analyses.R" file.
