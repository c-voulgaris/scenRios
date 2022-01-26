test_that("new headway set to right min", {
  new_feed <- gtfs_set_min_hdwy(nfta_gtfs,
                                route = "19",
                                service = "1",
                                new_hdwy = 14)
  new_hdwys <- gtfs_get_hdwys(new_feed,
                              route = "19",

                              service = "1")

  new_min <- min(new_hdwys$headway)

  expect_equal(new_min, 14, tolerance = 2)
})
