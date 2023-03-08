# datadrivencv-academic

This repository takes from [Pagedown's](https://github.com/rstudio/pagedown) resume template and the [Nick Strayer's datadrivencv concept](https://github.com/nstrayer/datadrivencv) to make a more comprehensive CV in a more academic context.

Some of the changes include:
- Formatting: adapted to A4 instead of US letter.
- Complete parametrisation: the Rmd template body shouldn't need mods.
- Subheadings: a _hacky_ way to add section subheaders by labelling h2 sections whithin `# Main` with `{#subtitle_something}`.
- Code to parse publications into the resume's format.
- Parsing of new Google Sheets with slightly different formats: `supervision`, `service` (e.g. for membership of institutional or professional associations), `training` (for professional development).


## Publications, conferences,...

Given I was already using Zotero to keep all my publications, presentations to conferences, etc. I found a bit of a waste of time having to manually add all the refs to a table given that there are R packages made to deal with standard citation file formats. However, most deal very poorly with non-ASCII characters or other text formatting, and it's pretty much the same for most of the exporting formats for Zotero. This was important because I have an accent in my surname, and because italicised words in the titles are common in biology (e.g. scientific names and gene names). My solution was to use the Better BibTeX plugin for Zotero, which also keeps the reference files synced, and export each of the reference subsections with the BetterBibTeX JSON format. This uses UTF-8 encoding (yay!!) while keeping the HTML tags from the Zotero titles.

##TBC