# Format Testimony

Format Testimony

## Usage

``` r
Format_Testimony(testimony_csv)
```

## Arguments

- testimony_csv:

  a .csv file of study testimony, formatted as shown in the template.
  Count, Page, Speaker, Bubble and Text are required columns.

## Value

a formatted csv file with appropriate div classes for speech bubbles

## Examples

``` r
if (FALSE) { # \dontrun{
library(utils)
template_testimony <- utils::read.csv("your/file/path/Combined_Testimony.csv")
formatted_testimony <- Format_Testimony(template_testimony)
} # }
```
