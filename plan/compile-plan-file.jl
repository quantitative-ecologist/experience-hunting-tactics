# Import package
using Weave


# Weave te document 
weave("plan\\analysis-plan.Jmd",
        doctype = "pandoc2pdf",
        fig_path = "figures",
        pandoc_options = ["--toc"]) # toc includes a table of contents
