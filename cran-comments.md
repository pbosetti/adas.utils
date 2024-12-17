## R CMD check results

0 errors | 0 warnings | 0 notes

This is a new release, fixing warnings in the previously submitted version, 
namely:

- Shortened title in DESCRIPTION
- Added reference to DESCRIPTION (ISBN)
- Improved documentation, each exported functuion has a \value section
- the `chauvenet` function returns a list of class `chauvenet`, and the proper `print.chauvenet()` method is provided
- Functions returning formulas do not change the `.GlobalEnv` anymore
- `fp_alias_matrix` function also accepts a `factorial.plam` object as input

