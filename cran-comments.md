## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Error 404 in inst/CITATION
I have modifeid the doi field in inst/CITATION from https://doi.org/10.7818/ECOS.3134 to 10.7818/ECOS.3134
I have expanded de Description field in DESCRIPTION and added the reference Miranda Cebrián, H. (2025) <doi:10.7818/ECOS.3134>

## Long examples
Examples in eidos_fuzzy_names and eidos_clean_checklist interact with an API to download some data. 
The download takes too long. 
They have been flagged with \dontrun to avoid the long runtime.
