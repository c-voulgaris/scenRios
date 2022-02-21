test_that("new headway set to right min uta", {
  new_feed <- gtfs_set_min_hdwy(uta_gtfs,
                                route = "27610",
                                service = "4",
                                new_hdwy = 5)
  new_hdwys <- gtfs_get_hdwys(new_feed,
                              route = "27610",
                              service = "4")

  new_min <- min(new_hdwys$headway)

  expect_equal(new_min, 5, tolerance = 2)
})

test_that("new headway set to right min nfta", {
  new_feed <- gtfs_set_min_hdwy(nfta_gtfs,
                                route = "19",
                                service = "1",
                                new_hdwy = 5)
  new_hdwys <- gtfs_get_hdwys(new_feed,
                              route = "19",
                              service = "1")

  new_min <- min(new_hdwys$headway)

  expect_equal(new_min, 5, tolerance = 2)
})
