# app.R

# # app.R
# pkgload::load_all()
# golem::detach_all_attached()
# options("golem.app.prod" = FALSE)
# #run_app()  # this should match your Golem app's run function
#
#

data = as.data.table(read_parquet("inst/extdata/cbo_det"))
data$Year <- year(data$FORMATION_DATE)
data <- data[Year >= 2008 & Year <= 2021]



