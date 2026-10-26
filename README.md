# HTLNaquatics
This R package was developed to analyze aquatic data collected by the Heartland Inventory and Monitoring Network (HTLN).

The R package can be installed using `pak::pkg_install('DOI-NPS/HTLNaquatics')`

Previous archived versions of this R package can be found at [www.github.com/DOI-NPS/HTLNaquatics](www.github.com/DOI-NPS/HTLNaquatics)

The following functions are used to compile and analyze aquatic data collected using the monitoring protocol for aquatic invertebrates and fish communities. 
<ul>
<li>importData: Imports database tables from the HTLN fish or invertebrate Microsoft Access databases and compiles views of the data.</li>
<li>getInvtCounts: query invertebrate count data.</li>
<li>getInvtSummaryStats: calculates the invertebrate summary statistics for annual reporting. This function uses calculateEPT, calculateHBI, calcuateShannon, and calculateTRichness to get the full summary statistics.</li>
<li>calculateEPT: calculates the EPT ratio and richness for invertebrate data.</li>
<li>calculateHBI: calculates HBI for invertebrate data.</li>
<li>calculateShannon: calculates the Shannon evenness and diversity for invertebrate data.</li>
<li>calculateTRichness: calculates the taxa richness for invertebrate data.</li>
