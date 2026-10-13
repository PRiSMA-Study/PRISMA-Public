# PRISMA MNH Maternal Outcomes Report

## Description

This folder contains the code used to generate the PRISMA MNH Maternal Outcomes Report. 

### Troubleshooting notes
- row_group_order() error:
  - There was an update to the gt package, which caused an error in row_group_order() and required updates to the code. Update: add id to tab_row_group (updated June 2026)
- LaTex / tinytex error:
  - install tinytex using this code:
    - tinytex::install_tinytex(repository = "ctan")
  - certain packages may not be installed, fix using:
    - tinytex::tlmgr_install(c("multirow", "wrapfig"))
