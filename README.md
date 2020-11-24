This is a repository containing code and data to reproduce the analyses of Callaghan et al. 2020. How to build a biodiverse city: environmental determinants of bird diversity within and among 1581 cities. Biodiversity and Conservation. The paper is available (here)[https://doi.org/10.1007/s10531-020-02088-1]. The paper uses eBird data (available for download here: https://ebird.org/data/download). I created a MariaDB version on my local machine and extracted data accordingly from there. Although this won't be reproducible as one would not have access to the local copy of the MariaDB, the code would be reproducible if a user downloaded the data manually to their machine and created a MariaDB. For transparency, I included these scripts in the repository. Further, the phylogenetic tree is large and not in the repository, but can be downloaded here: https://birdtree.org/. And while most intermediate model results are available in the "Data" folder, not all of these are because they were exceedingly large. Specifically, one should run the "overall_gams_for_each_response.R" script which will save the files out and then these are used elsewhere in other scripts. If you have any questions, please email me at: callaghan.corey.t@gmail.com.

