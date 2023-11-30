tmp <- revise::read_manuscript("policy_brief.qmd")
tmp$sections
txt <- readLines("summary.qmd")
txt <- c(txt[1:which(txt == "<!-- Insert here -->")],
         paste0(1:length(tmp$sections), ". ", tmp$sections),
         txt[which(txt == "<!-- To here -->"):length(txt)])
writeLines(txt, "summary.qmd")
