# Import package
using Weave

# Folder path
path = pwd()

# To weave all the Jmd documents
#jmd_files = filter(endswith("Jmd"), readdir())
#weave.(jmd_files)

# Weave each document seperately
weave("code/data-exploration.Jmd", 
        fig_path = "data-exploration-figs",
        doctype = "pandoc2pdf",
        fig_ext = ".png",
        pandoc_options = ["--toc"])

weave("code/01_PCA.Jmd", 
        fig_path = "PCA-figs",
        doctype = "pandoc2pdf",
        fig_ext = ".png",
        pandoc_options = ["--toc"])

#weave("code/figure1.Jmd", 
#        fig_path = "/figures/figure1",
#        fig_ext = ".png")