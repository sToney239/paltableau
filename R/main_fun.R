# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

scale_fill_tableau_gradient = function(name = waiver(), ..., palette = pal_tableau$sequential$blue_teal, direction = 1,space = "Lab", na.value = "grey50",
                                       start = 0, end = 1,
                                       guide = "colourbar", aesthetics = "fill") {
  the_pal =  palette$value
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("fill",
                   name = name,
                   palette = manual_rescale,# scales::gradient_n_pal(colours = the_pal),
                   na.value = na.value,
                   guide = guide, ...)
}
scale_color_tableau_gradient = function(name = waiver(), ..., palette = pal_tableau$sequential$blue_teal, direction = 1,space = "Lab", na.value = "grey50",
                                        start = 0, end = 1,
                                        guide = "colourbar", aesthetics = "colour") {
  the_pal = palette$value
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("colour",
                   name = name,
                   palette = manual_rescale,# scales::gradient_n_pal(colours = the_pal),
                   na.value = na.value,
                   guide = guide, ...)
}
scale_fill_tableau_binned_gradient = function(name = waiver(), ..., palette = pal_tableau$sequential$blue_teal, na.value = "grey50",
                                              direction = 1,start = 0, end = 1,
                                              transform = "identity", guide = "colourbar", aesthetics = "fill") {
  the_pal = palette$value
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  binned_scale("fill",
               palette=  manual_rescale,# scales::gradient_n_pal(colours = the_pal),
               na.value = na.value,
               transform = transform,
               guide = guide_colorbar(theme = theme(legend.ticks = element_blank())),
               ...,
               oob = scales::squish)
}
scale_color_tableau_binned_gradient = function(name = waiver(), ..., palette = pal_tableau$sequential$blue_teal, na.value = "grey50",
                                               direction=1,start = 0, end = 1,
                                               transform = "identity", guide = "colourbar", aesthetics = "colour") {
  the_pal = palette$value
  if(direction != 1) the_pal = rev(the_pal)
  manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  binned_scale("color",
               palette=  manual_rescale,# scales::gradient_n_pal(colours = the_pal),
               na.value = na.value,
               transform = transform,
               guide = guide_colorbar(theme = theme(legend.ticks = element_blank())),
               ...,
               oob = scales::squish)
}

scale_fill_tableau_gradient2 = function(name = waiver(), ..., palette = pal_tableau$diverging$classic_red_blue, midpoint = 0, direction = 1,space = "Lab", na.value = "grey50",
                                        start = 0, end = 1,
                                        transform = "identity", guide = "colourbar", aesthetics = "fill") {
  the_pal = palette$value
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x = dplyr::case_when(
      x < 0.5 ~ scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
      x > 0.5 ~ scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
      .default = 0.5
    )
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  # manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("fill",
                   name = name,
                   palette =  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
                   na.value = na.value,
                   transform = transform,
                   guide = guide, ...,
                   rescaler = ggplot2:::mid_rescaler(mid = midpoint, transform = transform))
}

scale_color_tableau_gradient2 = function(name = waiver(), ..., palette = pal_tableau$diverging$classic_red_blue, midpoint = 0, direction = 1,space = "Lab", na.value = "grey50",
                                         start = 0, end = 1,
                                         transform = "identity", guide = "colourbar", aesthetics = "colour") {
  the_pal = palette$value
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x = dplyr::case_when(
      x < 0.5 ~ scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
      x > 0.5 ~ scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
      .default = 0.5
    )
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  # manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  continuous_scale("color",
                   name = name,
                   palette =  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
                   na.value = na.value,
                   transform = transform,
                   guide = guide, ...,
                   rescaler = ggplot2:::mid_rescaler(mid = midpoint, transform = transform))
}


scale_fill_tableau_binned_gradient2 = function(name = waiver(), ..., palette = pal_tableau$diverging$red_blue_white, na.value = "grey50",
                                               direction = 1,start = 0, end = 1,
                                               transform = "identity", guide = "colourbar", aesthetics = "fill") {
  the_pal = palette$value
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x = dplyr::case_when(
      x < 0.5 ~ scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
      x > 0.5 ~ scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
      .default = 0.5
    )
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  # manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  binned_scale("fill",
               palette=  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
               na.value = na.value,
               transform = transform,
               guide = guide_colorbar(theme = theme(legend.ticks = element_blank())),
               ...,
               oob = scales::squish)
}


scale_color_tableau_binned_gradient2 = function(name = waiver(), ..., palette = pal_tableau$diverging$red_blue_white, na.value = "grey50",
                                                direction=1,start = 0, end = 1,
                                                transform = "identity", guide = "colourbar", aesthetics = "colour") {
  the_pal = palette$value
  if(direction == 1) the_pal = rev(the_pal)
  start1 =  0.5 * (1-end)
  end1 = 0.5 * (1-start)
  start2 = 1-0.5 * (1-start)
  end2 = 1-0.5 *(1- end)
  manual_rescale2 = \(x) {
    x = dplyr::case_when(
      x < 0.5 ~ scales::rescale(x,to = c(start1,end1),from = c(0,0.5)),
      x > 0.5 ~ scales::rescale(x,to = c(start2,end2),from = c(0.5,1)),
      .default = 0.5
    )
    scales::gradient_n_pal(colours = the_pal)(x)
  }
  # manual_rescale = \(x) scales::gradient_n_pal(colours = the_pal)(scales::rescale(x,to = c(start,end),from = c(0,1)))
  binned_scale("color",
               palette=  manual_rescale2,# scales::gradient_n_pal(colours = the_pal),
               na.value = na.value,
               transform = transform,
               guide = guide_colorbar(theme = theme(legend.ticks = element_blank())),
               ...,
               oob = scales::squish)
}


