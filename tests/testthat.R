library(testthat)
library(noaa)
library(ggplot2)
library(readr)


noaa<- read_delim('signif.txt',delim='\t')
noaa_clean <- eq_clean_data(noaa)
noaa_clean_short <- eq_clean_data(noaa,T)

test_that("clean output tibble", {
  expect_is(noaa_clean, "tbl_df")
})
#=================================================
test_that("eq_location_clean works !", {
  loc <- noaa_clean$LOCATION_NAME
  expect_equal(loc,eq_location_clean(noaa$LOCATION_NAME))
})
#=================================================
test_that("Label is a String", {
  expect_is(eq_create_label(noaa_clean),"character")
})
#================================================
test_that("Our map is leaflet", {
  foo <- eq_map(eq_clean_data(noaa,T),'DATE')
  expect_is(foo, "leaflet")
})
#================================================
test_that('The plot is ggplot',{
  gg <- ggplot(noaa_clean_short,
               aes('DATE'))+geom_timeline()
  expect_is(gg,'gg')
})
#===============================================
test_that('The plot is ggplot 2.0',{
  gg <- ggplot(noaa_clean_short,
               aes(x='DATE',magnitude = EQ_PRIMARY, label = LOCATION_NAME))+
    geom_timeline_label()
  expect_is(gg,'gg')
})
