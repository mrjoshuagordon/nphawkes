.onLoad <- function(libname = find.package("nphawkes"), pkgname = "nphawkes"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c("est",
        ".",
        'm_lb','m_ub', 't_lb','t_ub', 'r_lb','r_ub',
        'scale_x_log10',
        'ggplot',
        'geom_segment',
        'aes',
        'theme_bw',
        'labs',
        'scale_y_log10',
        '%>%',
        'mutate',
        'maxlon',
        'minlon',
        'maxlat',
        'minlat',
        'midlon',
        'midlat',
        'geom_tile',
        'scale_fill_gradientn',
        'theme',
        'element_line',
        'element_blank',
        'element_rect'


      )
    )
  invisible()
}
