# datadrivencv-academic

This repository takes from [Pagedown's](https://github.com/rstudio/pagedown) resume template and the [Nick Strayer's datadrivencv concept](https://github.com/nstrayer/datadrivencv) to make a more comprehensive CV in a more academic context.

Some of the changes include:
- Formatting: adapted to A4 instead of the original US letter.
- Complete parametrisation: the Rmd template body shouldn't need mods.
- Broad usage of icons for sections and contact info: [Academicons](https://jpswalsh.github.io/academicons/) and [FontAwesome](https://fontawesome.com/).
- Subheadings: a _hacky_ way to add section subheaders by labelling h2 sections whithin `# Main` with `{#subtitle_something}`.
- Code to parse publications into the resume's format, including automatic DOI linking, automatic highlighting of the author's name, and automatic notes for co-first authorship (see below).
- Parsing of new Google Sheets with slightly different formats: `supervision`, `service` (e.g. for membership of institutional or professional associations), `training` (for professional development).
- "Short" and long CV formats.


## Publications, conferences,...

Given I was already using Zotero to keep all my publications, presentations to conferences, etc. I thought it'd be a waste of time having to manually add all the refs to a table given that there are R packages made to deal with standard citation file formats. However, most deal very poorly with non-ASCII characters or other text formatting, and it's pretty much the same for most of the exporting formats for Zotero. This was important because I have an accent in my surname, and because italicised words in the titles are common in biology (e.g. scientific names and gene names). My solution was to use the Better BibTeX plugin for Zotero, which also keeps the reference files synced, and export each of the reference subsections with the BetterBibTeX JSON format. This uses UTF-8 encoding (yay!!) while keeping the HTML tags from the Zotero titles.

It should be noted that aside using an specific JSON format, the dates for the references is expected to be in a format such as `2022 Dec 27`, or for conferences, something like `2018 Aug 13-16`.

I organise my own bibliography according to the following folder scheme, so the expected files for the references follow along:
- Peer-reviewed publications:
  - Published: `peer-reviewed.json`.
  - Pre-prints: `preprints.json`.
  - Manuscripts under review: `under_review.json`.
- Conferences (all stored as item type "conference paper" in Zotero):
  - Oral presentations: `conf_oral.json`.
  - Posters: `conf_poster.json`.
  - Any other coauthored abstract presented by coauthors: `conf_coauth.json`.
- Technical reports: `reports.json`.
- Invited talks: `invited_talks.json`.


### Special features for bibliography

Academic CVs can be very long. A way to shorten them is to reduce the included publications and conference contributions to a few key ones. To do so, the code will look for the _tag_ `selected` in the `peer-reviewed`, `conf_oral` and `conf_poster` JSON files. All other refs will not be shown explicitly. This also applies to the `conf_coauth` in the long CV.


#### Co-first authorship

Simply add the _tag_ `cofirst` to whichever pub has cofirst authors. This only works for two co-first authors, not more.


#### Automatic name highlighting

Your name can be automatically highlighted (__bold__) by adding your name to the `input_files/aliases.txt`. The detection of the author name happens after parsing the authors, where `First Middle Surname` become `F. M. Surname`. So you should only need to add additional aliases if e.g.:
- Missing first/middle name, e.g. `F. M. Surname` won't match `F. Surname`, you'll need to include both.
- Alternate spellings of the surname, e.g. in my case:
  - `X. Vázquez`
  - `X. Vázquez Campos`
  - `X. Vazquez-Campos`
  - `X. Vázquez-Campos`

> Note that it won't probably work if your name matches a coauthor.


## Acknowledgements

As mentioned above, this CV was mainly developed with [Pagedown's](https://github.com/rstudio/pagedown) resume template, and inspired by [Nick Strayer's datadrivencv concept](https://github.com/nstrayer/datadrivencv) (and quite a bit of his code). However, I've also sought inspiration from a few other resources:
- [Academicons](https://jpswalsh.github.io/academicons/)
- [Kevin Rue-Albrecht's CV](https://annekelincoln.com/resume-in-r/)
- [W. Jake Thompson's CV](https://github.com/wjakethompson/cv)
- [Anneke Lincoln Schoeman](https://annekelincoln.com/resume-in-r/) for the `print_skills_list()` function.

I also thank [Miriam Kronen](https://www.linkedin.com/in/miriam-kronen-51622b1a8) for her feedback on my CV.