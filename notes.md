### Tabs

***Variable and Category Selection***: this tabulates the number of cases in each study by survey sweep for 
population subgroups. Users can combine subgroups within a domain (e.g., "Homosexual" and "Bisexual" in the "Sexual Orientation" domain) to aggregate sample sizes, if they so wish. 

***Age Range Selection***: for particularly small subgroups, users may wish to combine data across cohorts and within a cohort across sweeps (e.g., defining disability as ever reporting being disabled in adult sweeps). This tab provides functionality to calculate sample sizes within each domain, combining data from multiple cohorts and from sweep within a specified age range.

### Procedure for Calculating Sample Sizes

In the ***Age Range Selection*** tab, sample sizes are calculated by summing the number of cases in each cohort across the sweeps that took place within the age range specified by the users. In this calculation, an individual is defined as a case if they recorded the specific category for the selected characteristic during that time period. This means that a given individual can be counted multiple times if they meet the criteria across different sweeps - for instance, a person who lived in England in the age 23y sweep of the NCDS but in Scotland at the age 33y sweep will be counted in both categories for the sample size calculation. Given this possibility, the sample sizes in this tab can exceed the total number of individuals in a given cohort.

### Variable Definitions

Variables were derived from survey data available via End User Licence on the UK Data Service. Alternate variable definitions may be possible using, for instance, linked administrative data (e.g., on free school meals). The characteristics included on this website are not always measured in precisely the same way across the four cohorts or within a cohort, in each survey sweep. Some variables were reported by parents and others by cohort members themselves. The person reporting a variable could also could differ for the same variable measured in different sweeps (e.g., parent report in early childhood, self-report thereafter). Please refer to the code used to clean variables that can be viewed on the <a href="https://github.com/CLS-Data/coverage" target="_blank" rel="noopener noreferrer">GitHub repository page</a>. 

Sex, ethnicity, and sexual orientation are treated as time invariant variables. Each is collected on multiple occasions in each cohort. For sex and ethnicity, use the first recorded observation per individual, regardless of the sweep when it was recorded. One exception to this is that we use pre-cleaned 'longitudinal' sex (BCS70) and sex and ethnicity (NCDS) where these were available. For sexual orientation, we defined a participant as heterosexual if they ever reported this, otherwise bisexual if ever reported, otherwise homosexual, otherwise 'other'. The sample size calculations in the ***Age Range Selection*** and ***Variable and Category Selection*** tabs for these variables therefore represent the number of individuals at a given sweep (or at sweeps in a given age range) who have the particular characteristic. This differs slightly from the treatment of the time variant variables, where it is the number of individuals with that characteristic collected at a given sweep (or in any of the sweeps in a given age range).

### Small Cell Counts

Some variable has few observations in specific categories. For data sharing reasons, we cannot share the exact cell counts in these cases. Instead, we partially mask the true value as < 10, and provided ranges for any aggregate values based upon these.