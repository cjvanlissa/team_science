sourcetext <- readLines("policy_brief.qmd", encoding = "UTF8")
sourcetext <- sourcetext[grep("## Defining Team Science", sourcetext, fixed = T):grep("# Recommendations for a Vision for Team Science at Tilburg University", sourcetext, fixed = T)]
string <- paste0(sourcetext, collapse = "\n")

spans <- function(string){
  regex_header = "<span.+?id.{0,}?=.{0,}?\\b(.+?)\\b.+?>"
    regex_end = "(?<=</span>)"
    regex_issection = "</span>"
    open = "<span"
    close = "</span>"

  sectionends <- gregexpr(pattern = regex_end, text = string,
                          perl = TRUE)[[1]]

  sections <- lapply(sectionends, function(i) {
    substring(string, first = 1, last = i)
  })
  sections <- sections[grepl(pattern = regex_issection, x = sections)]
  sections_clean <- lapply(sections, function(x) {
    openbrackets <- list(position = gregexpr(pattern = open,
                                             text = x, fixed = TRUE)[[1]])
    openbrackets$type <- rep("open", times = length(openbrackets$position))
    closingbrackets <- list(position = gregexpr(pattern = close,
                                                text = x, fixed = TRUE)[[1]])
    closingbrackets$type <- rep("closed", times = length(closingbrackets$position))
    allbrackets <- openbrackets
    allbrackets$position <- c(allbrackets$position, closingbrackets$position)
    ordr <- order(allbrackets$position, decreasing = FALSE)
    allbrackets$type <- c(openbrackets$type, closingbrackets$type)[ordr]
    allbrackets$position <- allbrackets$position[ordr]
    out <- substring(text = x, first = (revise:::findposition(x = allbrackets)),
                     last = (nchar(x)))
    headr <- regmatches(out, gregexpr(regex_header, out,
                                      perl = TRUE))[[1]]
    headr <- headr[1]
    c(out, headr)
  })
  sectionheaders <- sapply(sections_clean, `[[`, 2)
  sections_clean <- sapply(sections_clean, `[[`, 1)
  sectionheaders <- gsub("^.+?id.{0,}?=.{0,}?[\"'](.+?)[\"'].*$",
                           "\\1", sectionheaders)
    sections_clean <- lapply(sections_clean, gsub, pattern = "^.+?>(.+)<.+?$",
                             replacement = "\\1")
  out <- sections_clean
  names(out) <- sectionheaders
  return(out)
}
tmp <- spans(string)

titles <- sourcetext
titles <- titles[grepl("(^## |id=)", titles)]

titles2 <- vector("character")
for(i in 1:(length(titles)-1)){
  is_header <- grepl("## ", titles[i], fixed = T)
  next_header <- grepl("## ", titles[i+1], fixed = T)
  if(is_header & next_header){
    next
  } else {
    titles2 <- c(titles2, titles[i])
  }
}
if(!grepl("## ", tail(titles, 1))){
  titles2 <- c(titles2, tail(titles, 1))
}
titles <- titles2

sections <- grep("id=", titles, fixed = T)

# Remove cross references
titles[-sections] <- gsub("\\s{0,}\\{#(.+?)\\}.*$", " \\(@\\1)", titles[-sections])


titles[-sections] <- gsub("## ", "**", titles[-sections], fixed = T)
titles[-sections] <- gsub(" (@", "** (@", titles[-sections], fixed = T)
titles[-sections][grep("[a-zA-Z]$", titles[-sections])] <- paste0(titles[-sections][grep("[a-zA-Z]$", titles[-sections])], "**")

# Clean up spans
titles[sections] <- gsub("^.+?id=\"(.+?)\".*$", "\\1", titles[sections])
# Add numbering
titles[sections] <- paste0(1:length(tmp), ". ", tmp[titles[sections]])

# Write to summary
txt <- readLines("summary.qmd")
txt <- c(txt[1:which(txt == "<!-- Insert here -->")],
         #paste0(1:length(tmp$sections), ". ", tmp$sections),
         paste0(titles, collapse = "\n\n"),
         txt[which(txt == "<!-- To here -->"):length(txt)])
writeLines(txt, "summary.qmd")
