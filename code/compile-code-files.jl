# Import package
using Weave

# To weave all the Jmd documents
#jmd_files = filter(endswith("Jmd"), readdir())
#weave.(jmd_files)

directory = pwd()

# Weave each document seperately
weave("code/data-exploration.Jmd", 
        fig_path = "data-exploration-figs",
        doctype = "pandoc2pdf",
        fig_ext = ".png")

#weave("code/experience-metrics.Jmd",
#        fig_path = "experience-metrics-figs",
#        doctype = "pandoc2pdf",
#        fig_ext = ".png")

#weave("code/figure1.Jmd", 
#        fig_path = "/figures/figure1",
#        fig_ext = ".png")
