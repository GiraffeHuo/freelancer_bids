# The color scheme, adopted from
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# colors. See Upwork_Brand_Guidelines_04082021.pdf for reference
up_colors <- c(
  `dark green`     = "#2D6318",
  `darkish green`  = "#3C8224",
  `green`          = "#14A800",
  `lightish green` = "#66B549",
  `light green`    = "#88C872",
  `dark lime`      = "#47691B",
  `darkish lime`   = "#76A230",
  `lime`           = "#95DF00",
  `lightish lime`  = "#C0E666",
  `light lime`     = "#DBF0A3",
  `dark blue`      = "#254D97",
  `blue`           = "#1F57C3",
  `light blue`     = "#557AC9",
  `dark sky`       = "#4BA798",
  `sky`            = "#01CDBE",
  `light sky`      = "#6AD4CA",
  `black`          = "#001E00",
  `gray1`          = "#65735B",
  `gray2`          = "#9AAA97",
  `gray3`          = "#BECCBE",
  `gray4`          = "#D5E0D5",
  `gray5`          = "#E4EBE4",
  `gray6`          = "#F2F7F2",
  `white`          = "#FFFFFF",
  `forest`         = "#13544E",
  `mint`           = "#91E6B3",
  `brown`          = "#67541F",
  `kiwi`           = "#97922D",
  `yellow`         = "#DEBE1A",
  `pink`           = "#F66DBC",
  `brick`          = "#9B211B",
  `orange red`     = "#FF4B25",
  `clay`           = "#BC5118",
  `heather`        = "#A18085",
  `liliac`         = "#BDA1E7")

# Function to extract up colors as hex codes
# @param ... Character names of up_colors
#
up_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (up_colors)

  up_colors[cols]
}

# palettes
up_palettes <- list(
  `main`  = up_cols("green", "blue", "orange red",  "yellow", "brick"),

  `green`  = up_cols("dark green", "darkish green", "lightish green", "light green"),

  `lime`   = up_cols("dark lime", "darkish lime", "lightish lime", "light lime"),

  `blue`   = up_cols("dark blue", "blue", "light blue"),

  `sky`   = up_cols("dark sky", "sky", "light sky"),

  `yellow` = up_cols("brown", "kiwi", "yellow"),

  `pink`   = up_cols("pink", "orange red", "brick"),

  `purple` = up_cols ("liliac", "heather", "clay"),

  `temperature` = up_cols("blue", "sky", "gray3", "pink", "orange red"),

  `grey`  = up_cols("grey6", "grey5", "grey4", "grey3",
                    "grey2", "grey1"),

  `black and white` = up_cols("white", "grey6", "grey5", "grey4", "grey3",
                              "grey2", "grey1", "black")
)


# This function returns a "ColorRampPaletter" for scales to use
# Note: altered this compared to the website's instructions in
# order to ensure that the colors appear in the order written
# in the definition of palettes above. If the behavior is not desired,
# uncomment the function below and replace
#
# up_pal <- function(palette = "main", reverse = FALSE, ...) {
#   pal <- up_palettes[[palette]]
#
#   if (reverse) pal <- rev(pal)
#
#   colorRampPalette(pal, ...)
# }
#
up_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- up_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorSequence <- function(n){
    if(n>length(pal)){
      colorRampPalette(pal,...)(n)
    }
    else{
      pal[1:n] %>% unname
    }
  }

  return(colorSequence)
}

scale_color_up <- function(palette="main", discrete=TRUE, reverse=FALSE, ...) {
  pal <- up_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("up_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_up <- function(palette="main", discrete=TRUE, reverse=FALSE, ...) {
  pal <- up_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("up_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# The style function has two arguments that specify the location of the legend
# in graphs that need one. The defaults are specified, but feel free to change
# the location if the graph looks better that way
plot.up.theme <- function(leg.x = 0.75, leg.y = 0.1){
  extra <- 0
  theme_bw() +
    theme(panel.background = element_rect(),
          text = element_text(family="Times",size=11+2*extra),
          plot.title = element_text(size=12+2*extra, family="Times", face="bold",
                                    #hjust=0.5,
                                    lineheight=2),
          axis.title.x = element_text(family="Times",size=11+2*extra),
          axis.title.y = element_text(family="Times",size=11+2*extra),
          legend.title = element_text(size=8+2*extra,family="Times",
                                      face="bold"),
          legend.text  = element_text(family="Times",size=8+2*extra),
          legend.position = c(leg.x, leg.y),
          legend.direction = "horizontal",
          legend.background = element_rect(
            fill = "white",
            linewidth = 0.2,
            linetype="solid",
            colour = "black"))
}

# Update the default view of lines and points
update_geom_defaults("point", list(size = 1.5, color=up_cols('green')))
update_geom_defaults("line",  list(size = 0.7, color=up_cols('green')))
update_geom_defaults("bar",   list(fill=up_cols('green'), color=up_cols('black')))

table.up.theme <- function(ht){
  # style function to edit huxtables

  huxtable::top_padding(ht)    <- 2
  huxtable::bottom_padding(ht) <- 2
  huxtable::width(ht)          <- 0.8 # 80% of the document's width

  # Capitalize titles
  ht[1,1:ncol(ht)] <- tools::toTitleCase(names(ht))

  # Color of the font
  huxtable::text_color(ht) <- up_cols("black")

  # edit the title row
  huxtable::align(ht) <- "center"
  huxtable::align(ht)[2:nrow(ht), 1] <- "left"
  huxtable::text_color(ht)[1, 1:ncol(ht)] <- up_cols("forest")
  huxtable::bold(ht)[1, 1:ncol(ht)] <- TRUE
  huxtable::bottom_border(ht)[1, ] <- 1

  # add thick borders at the top and and the bottom
  outer.border.size <- 3

  huxtable::top_border(ht)[1, ] <- outer.border.size
  huxtable::top_border_color(ht) <- up_cols("forest")

  huxtable::bottom_border(ht)[nrow(ht), ] <- outer.border.size
  huxtable::bottom_border_color(ht) <- up_cols("forest")

  return(ht)
}

# Format the numbers in the inline code output
inline_hook <- function(x){
  if(is.numeric(x)){

    # integers get , to separate thousands
    if(abs(x - round(x)) < .Machine$double.eps){
      formatted <- format(x, big.mark = ",", scientific = F)
    } else {
    # consistent rounding of non-integers
      formatted <- format(x, digits=2, nsmall=2, scientific = F)
    }

  } else {

    # render text in bold (commented out)
    if(is.character(x)){
      # formatted <- paste0("**", x, "**")
      formatted <- x
    } else {
      formatted <- x
    }

  }

  formatted
}

knitr::knit_hooks$set(inline = inline_hook)
