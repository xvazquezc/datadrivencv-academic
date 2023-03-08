template_default <- 
"
### {title}

{loc}

{institution}

{timeline}

{ifelse(description_bullets != 'N/A', description_bullets, '')}

\n\n\n"


template_teaching <-
"
### {title}

{loc}.

{institution}

{timeline}

{ifelse(description_bullets != 'N/A', description_bullets, '')}

\n\n\n"


template_grants <- 
"
### {title}

{project}

{institution}

{timeline}

{ifelse(description_bullets != 'N/A', description_bullets, '')}

\n\n\n"


template_pubs <-
"
### {title}

*{journal}* {journal_numbers}. {doi}

N/A

{timeline}

{authors}.

\n\n\n"


template_under_review <-
  "
### {title}

*{journal}*, {journal_numbers}.

N/A

{timeline}

{authors}.

\n\n\n"


template_preprints <-
"
### {title}

*{journal}*, {journal_numbers}.{ifelse(is.na(doi), '', paste(' ', doi))}

N/A

{timeline}

{authors}.

\n\n\n"


template_reports <-
"
### {title}

{authors}. *{journal}*, p. {pages}.

N/A

{timeline}

\n\n\n"


template_conferences <-
"
### {title}

*{conferenceName}*.{ifelse(doi == 'N/A', '', paste(' ', doi))}

{place}

{timeline}

{authors}.

\n\n\n"


template_invited_talks <-
"
### {title}

{meetingName}.{ifelse(url == 'N/A', '', paste(' ', url))}

{place}

{timeline}

\n\n\n"


template_training <-
"
### {title}

{with}.

{place}

{timeline}

\n\n\n"


template_service_gen <-
"
### {dplyr::case_when(section == 'membership_inst' ~ 'Institutional Service', section == 'membership_prof' ~ 'Professional Affiliations', section == 'service_conf'    ~ 'Conferences', section == 'reviews'         ~ 'Peer Review')}

{bullets}

N/A

\n\n\n"


template_reviews <-
  "
### {dplyr::case_when(section == 'reviews' ~ 'Peer Reviews')}

{bullets}

N/A

N/A

\n\n\n"


template_supervision <-
  "
### {title}

{level}. {role}.

{where}

{timeline}

{description_1}

\n\n\n"
