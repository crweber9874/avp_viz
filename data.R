  read.csv("~/Dropbox/github_repos/avp-map/avp_map_viz/data/voter_ld_public.csv") %>%
  shape_data(level = "ld") %>%
  st_as_sf(wkt = "shape_geom") %>%
  st_simplify(dTolerance = 0.02) %>%
  save(file = "~/Dropbox/github_repos/avp-map/avp_map_viz/simpleMap/ld_public.rda")

