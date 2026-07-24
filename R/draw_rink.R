# Rink Argument Helper ---------------------------------------------------------

# Normalize rink argument.
.normalize_rink_argument <- function(value, argument, aliases) {
  choices <- paste(names(aliases), collapse = ', ')
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(
      sprintf('`%s` must be one of: %s.', argument, choices),
      call. = FALSE
    )
  }
  value <- tolower(trimws(value))
  if (!nzchar(value) || !value %in% names(aliases)) {
    stop(
      sprintf('`%s` must be one of: %s.', argument, choices),
      call. = FALSE
    )
  }
  unname(aliases[[value]])
}

# Rink Arc Helper -------------------------------------------------------------

# Build rink arc.
.rink_arc <- function(x, y, radius, start = 0, end = 2 * pi, points = 181L) {
  angle <- seq(start, end, length.out = points)
  data.frame(
    x = x + radius * cos(angle),
    y = y + radius * sin(angle)
  )
}

# Rink Arc Band Helper --------------------------------------------------------

# Build rink arc band.
.rink_arc_band <- function(
  x,
  y,
  radius,
  width,
  start = 0,
  end = 2 * pi,
  points = 181L
) {
  outer <- .rink_arc(
    x      = x,
    y      = y,
    radius = radius + width / 2,
    start  = start,
    end    = end,
    points = points
  )
  inner <- .rink_arc(
    x      = x,
    y      = y,
    radius = radius - width / 2,
    start  = end,
    end    = start,
    points = points
  )
  rbind(outer, inner)
}

# Rink Line Helper ------------------------------------------------------------

# Build rink line polygon.
.rink_line_polygon <- function(x, y, xend, yend, width) {
  delta_x     <- xend - x
  delta_y     <- yend - y
  line_length <- sqrt(delta_x^2 + delta_y^2)
  offset_x    <- -(delta_y / line_length) * width / 2
  offset_y    <- (delta_x / line_length) * width / 2
  data.frame(
    x = c(
      x + offset_x,
      xend + offset_x,
      xend - offset_x,
      x - offset_x
    ),
    y = c(
      y + offset_y,
      yend + offset_y,
      yend - offset_y,
      y - offset_y
    )
  )
}

# Rink Lines Helper -----------------------------------------------------------

# Build rink line polygons.
.rink_line_shapes <- function(lines, width) {
  .combine_rink_shapes(lapply(seq_len(nrow(lines)), function(i) {
    .rink_line_polygon(
      x     = lines$x[i],
      y     = lines$y[i],
      xend  = lines$xend[i],
      yend  = lines$yend[i],
      width = width
    )
  }))
}

# Rink Shape Helper -----------------------------------------------------------

# Combine rink shapes.
.combine_rink_shapes <- function(shapes) {
  shapes <- Filter(
    function(shape) !is.null(shape) && nrow(shape) > 0L,
    shapes
  )
  shapes <- lapply(seq_along(shapes), function(i) {
    shape       <- shapes[[i]]
    shape$group <- i
    shape
  })
  combined <- do.call(rbind, shapes)
  row.names(combined) <- NULL
  combined
}

# Rink Geometry Helper --------------------------------------------------------

# Build regulation rink geometry.
.rink_geometry <- function() {
  # Define regulation dimensions.
  rink_half_length  <- 100
  rink_half_width   <- 42.5
  corner_radius     <- 28
  corner_x          <- rink_half_length - corner_radius
  corner_y          <- rink_half_width - corner_radius
  goal_line_x       <- 89
  blue_line_x       <- 25
  faceoff_line      <- 2 / 12
  zone_line         <- 1
  board_width       <- 0.5
  goal_line_y       <- corner_y + sqrt(
    corner_radius^2 - (goal_line_x - corner_x)^2
  )

  # Build rink surface and boards.
  surface <- rbind(
    .rink_arc(
      x = corner_x, y = corner_y, radius = corner_radius,
      start = pi / 2, end = 0, points = 61L
    ),
    .rink_arc(
      x = corner_x, y = -corner_y, radius = corner_radius,
      start = 0, end = -pi / 2, points = 61L
    ),
    .rink_arc(
      x = -corner_x, y = -corner_y, radius = corner_radius,
      start = -pi / 2, end = -pi, points = 61L
    ),
    .rink_arc(
      x = -corner_x, y = corner_y, radius = corner_radius,
      start = pi, end = pi / 2, points = 61L
    )
  )
  board_radius <- corner_radius - board_width / 2
  boards <- .combine_rink_shapes(list(
    .rink_line_polygon(
      x = -corner_x, y = rink_half_width - board_width / 2,
      xend = corner_x, yend = rink_half_width - board_width / 2,
      width = board_width
    ),
    .rink_line_polygon(
      x = -corner_x, y = -rink_half_width + board_width / 2,
      xend = corner_x, yend = -rink_half_width + board_width / 2,
      width = board_width
    ),
    .rink_line_polygon(
      x = rink_half_length - board_width / 2, y = -corner_y,
      xend = rink_half_length - board_width / 2, yend = corner_y,
      width = board_width
    ),
    .rink_line_polygon(
      x = -rink_half_length + board_width / 2, y = -corner_y,
      xend = -rink_half_length + board_width / 2, yend = corner_y,
      width = board_width
    ),
    .rink_arc_band(
      x = corner_x, y = corner_y, radius = board_radius,
      width = board_width, start = pi / 2, end = 0, points = 61L
    ),
    .rink_arc_band(
      x = corner_x, y = -corner_y, radius = board_radius,
      width = board_width, start = 0, end = -pi / 2, points = 61L
    ),
    .rink_arc_band(
      x = -corner_x, y = -corner_y, radius = board_radius,
      width = board_width, start = -pi / 2, end = -pi, points = 61L
    ),
    .rink_arc_band(
      x = -corner_x, y = corner_y, radius = board_radius,
      width = board_width, start = pi, end = pi / 2, points = 61L
    )
  ))

  # Build center, blue, and goal lines.
  center_line <- .rink_line_polygon(
    x = 0, y = -rink_half_width, xend = 0, yend = rink_half_width,
    width = zone_line
  )
  center_gaps <- .combine_rink_shapes(lapply(
    seq(-40, 40, by = 4),
    function(gap_y) {
      .rink_line_polygon(
        x = 0, y = gap_y - 0.25, xend = 0, yend = gap_y + 0.25,
        width = zone_line + 0.02
      )
    }
  ))
  blue_lines <- .combine_rink_shapes(list(
    .rink_line_polygon(
      x = -blue_line_x, y = -rink_half_width,
      xend = -blue_line_x, yend = rink_half_width, width = zone_line
    ),
    .rink_line_polygon(
      x = blue_line_x, y = -rink_half_width,
      xend = blue_line_x, yend = rink_half_width, width = zone_line
    )
  ))
  goal_lines <- .combine_rink_shapes(list(
    .rink_line_polygon(
      x = -goal_line_x, y = -goal_line_y,
      xend = -goal_line_x, yend = goal_line_y, width = faceoff_line
    ),
    .rink_line_polygon(
      x = goal_line_x, y = -goal_line_y,
      xend = goal_line_x, yend = goal_line_y, width = faceoff_line
    )
  ))

  # Build faceoff circles and spots.
  end_spots <- data.frame(
    x = c(-69, -69, 69, 69),
    y = c(-22, 22, -22, 22)
  )
  neutral_spots <- data.frame(
    x = c(-20, -20, 20, 20),
    y = c(-22, 22, -22, 22)
  )
  faceoff_spots <- rbind(end_spots, neutral_spots)
  end_circles <- .combine_rink_shapes(lapply(
    seq_len(nrow(end_spots)),
    function(i) {
      .rink_arc_band(
        x = end_spots$x[i], y = end_spots$y[i], radius = 15,
        width = faceoff_line, points = 241L
      )
    }
  ))
  center_circle <- .rink_arc_band(
    x = 0, y = 0, radius = 15, width = faceoff_line, points = 241L
  )
  spot_outer <- .combine_rink_shapes(lapply(
    seq_len(nrow(faceoff_spots)),
    function(i) {
      .rink_arc(
        x = faceoff_spots$x[i], y = faceoff_spots$y[i],
        radius = 1, points = 121L
      )
    }
  ))
  spot_inner <- .combine_rink_shapes(lapply(
    seq_len(nrow(faceoff_spots)),
    function(i) {
      .rink_arc(
        x = faceoff_spots$x[i], y = faceoff_spots$y[i],
        radius = 1 - faceoff_line, points = 121L
      )
    }
  ))
  spot_band_x <- seq(-0.75, 0.75, length.out = 81L)
  spot_bands <- .combine_rink_shapes(lapply(
    seq_len(nrow(faceoff_spots)),
    function(i) {
      data.frame(
        x = c(spot_band_x, rev(spot_band_x)) + faceoff_spots$x[i],
        y = c(
          sqrt(1 - spot_band_x^2),
          -sqrt(1 - rev(spot_band_x)^2)
        ) + faceoff_spots$y[i]
      )
    }
  ))
  center_spot <- .rink_arc(x = 0, y = 0, radius = 0.5, points = 121L)

  # Build goal creases.
  crease_y      <- seq(-4, 4, length.out = 181L)
  crease_offset <- sqrt(6^2 - crease_y^2)
  crease_fill <- .combine_rink_shapes(list(
    data.frame(
      x = c(-goal_line_x, -goal_line_x + crease_offset, -goal_line_x),
      y = c(-4, crease_y, 4)
    ),
    data.frame(
      x = c(goal_line_x, goal_line_x - crease_offset, goal_line_x),
      y = c(-4, crease_y, 4)
    )
  ))
  crease_angle <- asin(4 / 6)
  crease_arcs <- .combine_rink_shapes(list(
    .rink_arc_band(
      x = -goal_line_x, y = 0, radius = 6, width = faceoff_line,
      start = -crease_angle, end = crease_angle, points = 121L
    ),
    .rink_arc_band(
      x = goal_line_x, y = 0, radius = 6, width = faceoff_line,
      start = pi + crease_angle, end = pi - crease_angle, points = 121L
    )
  ))
  crease_edge <- sqrt(6^2 - 4^2)

  # Build regulation red details.
  red_segments <- data.frame(
    x = c(
      -89, -89, 89, 89,
      -85, -85, 85, 85,
      -89, -89, 89, 89
    ),
    y = c(
      -4, 4, -4, 4,
      -4, 4, -4, 4,
      -11, 11, -11, 11
    ),
    xend = c(
      -89 + crease_edge, -89 + crease_edge,
      89 - crease_edge, 89 - crease_edge,
      -85, -85, 85, 85,
      -100, -100, 100, 100
    ),
    yend = c(
      -4, 4, -4, 4,
      -4 + 5 / 12, 4 - 5 / 12, -4 + 5 / 12, 4 - 5 / 12,
      -14, 14, -14, 14
    )
  )
  hash_offset <- (5 + 7 / 12) / 2
  config_depth <- 2 + 10 / 12
  for (i in seq_len(nrow(end_spots))) {
    x <- end_spots$x[i]
    y <- end_spots$y[i]
    red_segments <- rbind(
      red_segments,
      data.frame(
        x = x + c(
          -6, -6, 2, 2,
          -2, 2, -2, 2,
          -hash_offset, hash_offset, -hash_offset, hash_offset
        ),
        y = y + c(
          -0.75, 0.75, -0.75, 0.75,
          -0.75, -0.75, 0.75, 0.75,
          15, 15, -15, -15
        ),
        xend = x + c(
          -2, -2, 6, 6,
          -2, 2, -2, 2,
          -hash_offset, hash_offset, -hash_offset, hash_offset
        ),
        yend = y + c(
          -0.75, 0.75, -0.75, 0.75,
          -0.75 - config_depth, -0.75 - config_depth,
          0.75 + config_depth, 0.75 + config_depth,
          17, 17, -17, -17
        )
      )
    )
  }
  red_details <- .rink_line_shapes(red_segments, faceoff_line)
  referee_crease <- .rink_arc_band(
    x = 0, y = -rink_half_width, radius = 10, width = faceoff_line,
    start = 0, end = pi, points = 121L
  )

  # Build goal frames and nets.
  net_depth <- 40 / 12
  goal_nets <- .combine_rink_shapes(list(
    data.frame(
      x = c(-89, -89 - net_depth, -89 - net_depth, -89),
      y = c(-3, -2.5, 2.5, 3)
    ),
    data.frame(
      x = c(89, 89 + net_depth, 89 + net_depth, 89),
      y = c(-3, -2.5, 2.5, 3)
    )
  ))
  goal_net_outlines <- goal_nets
  goal_posts <- .combine_rink_shapes(lapply(
    list(c(-89, -3), c(-89, 3), c(89, -3), c(89, 3)),
    function(position) {
      .rink_arc(
        x = position[1], y = position[2],
        radius = 2.375 / 24, points = 61L
      )
    }
  ))

  # Return rink geometry.
  list(
    surface           = surface,
    boards            = boards,
    center_line       = center_line,
    center_gaps       = center_gaps,
    blue_lines        = blue_lines,
    goal_lines        = goal_lines,
    end_circles       = end_circles,
    center_circle     = center_circle,
    spot_outer        = spot_outer,
    spot_inner        = spot_inner,
    spot_bands        = spot_bands,
    center_spot       = center_spot,
    crease_fill       = crease_fill,
    crease_arcs       = crease_arcs,
    red_details       = red_details,
    referee_crease    = referee_crease,
    goal_nets         = goal_nets,
    goal_net_outlines = goal_net_outlines,
    goal_posts        = goal_posts
  )
}

# Rink Polygon Layer Helper ---------------------------------------------------

# Build rink polygon layer.
.rink_polygon_layer <- function(shape, fill) {
  group <- if ('group' %in% names(shape)) {
    shape$group
  } else {
    rep(1L, nrow(shape))
  }
  ggplot2::annotate(
    'polygon',
    x = shape$x,
    y = shape$y,
    group = group,
    fill = fill,
    colour = NA,
    rule = 'evenodd'
  )
}

# Rink Plotting Function ------------------------------------------------------

#' Draw an NHL rink
#'
#' `draw_rink()` creates a regulation NHL rink that can be extended with
#' `ggplot2` layers. The centered coordinate system matches the coordinate
#' fields returned by `nhlscraper`.
#'
#' @param view character rink view: `'full'` (`'f'`), `'offensive-half'`
#'   (`'oh'`), `'defensive-half'` (`'dh'`), `'offensive-zone'` (`'oz'`),
#'   `'defensive-zone'` (`'dz'`), or `'neutral-zone'` (`'nz'`)
#' @param orientation character orientation: `'horizontal'` (`'h'`) or
#'   `'vertical'` (`'v'`)
#'
#' @returns A `ggplot2` plot.
#'
#' @details
#' The offensive end is on the right for a horizontal rink and at the top for
#' a vertical rink. Use `xCoordNorm` and `yCoordNorm` from
#' [nhlscraper::gc_pbp()] when every event should attack toward that end. Raw
#' play-by-play and replay coordinates remain spatially aligned but retain
#' their physical direction unless normalized by the caller.
#'
#' Both orientations use the same layer mappings: map longitudinal coordinates
#' to `x` and lateral coordinates to `y`. On a vertical rink, positive lateral
#' coordinates appear on the right.
#'
#' @references
#' [National Hockey League Official Rules 2025-2026](https://media.nhl.com/site/asset/public/ext/2025-26/2025-26Rules.pdf)
#'
#' @examples
#' draw_rink()
#' draw_rink(view = 'oz', orientation = 'v')
#'
#' @export
draw_rink <- function(view = 'full', orientation = 'horizontal') {
  # Normalize function arguments.
  view <- .normalize_rink_argument(
    value = view,
    argument = 'view',
    aliases = c(
      full             = 'full',
      f                = 'full',
      `offensive-half` = 'offensive-half',
      oh               = 'offensive-half',
      `defensive-half` = 'defensive-half',
      dh               = 'defensive-half',
      `offensive-zone` = 'offensive-zone',
      oz               = 'offensive-zone',
      `defensive-zone` = 'defensive-zone',
      dz               = 'defensive-zone',
      `neutral-zone`   = 'neutral-zone',
      nz               = 'neutral-zone'
    )
  )
  orientation <- .normalize_rink_argument(
    value = orientation,
    argument = 'orientation',
    aliases = c(
      horizontal = 'horizontal',
      h          = 'horizontal',
      vertical   = 'vertical',
      v          = 'vertical'
    )
  )

  # Set requested view limits.
  x_limits <- switch(
    view,
    full             = c(-100, 100),
    `offensive-half` = c(0, 100),
    `defensive-half` = c(-100, 0),
    `offensive-zone` = c(25, 100),
    `defensive-zone` = c(-100, -25),
    `neutral-zone`   = c(-25, 25)
  )
  y_limits <- c(-42.5, 42.5)

  # Define rink colors.
  ice          <- '#FFFFFF'
  board        <- '#252525'
  rink_red     <- '#C8102E'
  rink_blue    <- '#0033A0'
  crease_blue  <- '#00B5E2'
  net_fill     <- '#F3F4F5'
  net_outline  <- '#8A8D8F'
  geometry     <- .rink_geometry()

  # Draw regulation rink.
  rink <- ggplot2::ggplot() +
    .rink_polygon_layer(geometry$surface, ice) +
    .rink_polygon_layer(geometry$center_line, rink_red) +
    .rink_polygon_layer(geometry$center_gaps, ice) +
    .rink_polygon_layer(geometry$blue_lines, rink_blue) +
    .rink_polygon_layer(geometry$crease_fill, crease_blue) +
    .rink_polygon_layer(geometry$end_circles, rink_red) +
    .rink_polygon_layer(geometry$center_circle, rink_blue) +
    .rink_polygon_layer(geometry$spot_outer, rink_red) +
    .rink_polygon_layer(geometry$spot_inner, ice) +
    .rink_polygon_layer(geometry$spot_bands, rink_red) +
    .rink_polygon_layer(geometry$center_spot, rink_blue) +
    .rink_polygon_layer(geometry$crease_arcs, rink_red) +
    .rink_polygon_layer(geometry$red_details, rink_red) +
    .rink_polygon_layer(geometry$referee_crease, rink_red) +
    .rink_polygon_layer(geometry$goal_nets, net_fill) +
    ggplot2::annotate(
      'path',
      x = geometry$goal_net_outlines$x, y = geometry$goal_net_outlines$y,
      group = geometry$goal_net_outlines$group, colour = net_outline,
      linewidth = 0.35, lineend = 'round', linejoin = 'round'
    ) +
    .rink_polygon_layer(geometry$goal_lines, rink_red) +
    .rink_polygon_layer(geometry$goal_posts, rink_red) +
    .rink_polygon_layer(geometry$boards, board) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
      plot.background  = ggplot2::element_rect(fill = 'transparent', colour = NA),
      legend.position  = 'none',
      plot.margin      = ggplot2::margin(2, 2, 2, 2, unit = 'pt')
    )

  # Apply requested orientation.
  if (orientation == 'horizontal') {
    rink <- rink + ggplot2::coord_fixed(
      ratio = 1,
      xlim = x_limits,
      ylim = y_limits,
      expand = FALSE,
      clip = 'on'
    )
  } else {
    rink <- rink +
      ggplot2::coord_flip(
        xlim = x_limits,
        ylim = y_limits,
        expand = FALSE,
        clip = 'on'
      ) +
      ggplot2::theme(
        aspect.ratio = diff(x_limits) / diff(y_limits)
      )
  }
  rink
}
