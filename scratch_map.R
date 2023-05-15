library(mapview)
library(sf)
library(spData)
library(ggmap)
library(tidyverse)
library(ggsn)
library(cowplot)
library(ggthemes)
library(wesanderson)


pal <- wes_palette("Darjeeling1", 5)

# load montaverded protected areas
pa <- st_read("../data/gis/Protected_Areas.shp")%>% st_transform(4326) 



#load costa rica polygon
cr <- world %>% dplyr::filter(name_long == "Costa Rica")

# filter to only the parcels used in study
parcels <- c("Reserva La Calandria", 	"Refugio Ecológico Nacimiento y Vida")
pa_filt <- pa %>% filter(Nombre %in% parcels)

# make parcel bounding box to grab base map
map_bb <- pa_filt %>% st_buffer(100) %>% st_bbox
names(map_bb) <- c("left", "bottom", "right", "top")
mid_x <- ((map_bb[["left"]]-map_bb[["right"]])/2)+map_bb[["right"]]
mid_y <- ((map_bb[["top"]]-map_bb[["bottom"]])/2)+map_bb[["bottom"]]
center <- c(mid_x, mid_y)
center_df <- data.frame(x = center[1],
                        y = center[2])
# get base map
base_map <- get_googlemap(center = center, maptype = "satellite", source = "google", 
                          zoom = 13, messaging = T)

# create appx transect locations
trans_df <- data.frame(lat = c(-84.8405, -84.8412, -84.8418, -84.8411, -84.84106, -84.8407, -84.8384, -84.8365, -84.8368, -84.836),
                       lon =  c(10.3165, 10.3163, 10.3155, 10.3154, 10.3168, 10.316, 10.3057, 10.306, 10.305, 10.3052),
                       cond = c("Pasture", "T-10", "T-7", "T-10", "T-7", "Pasture", "T-15", NA, NA, "T-15"))
trans_sf <- st_as_sf(trans_df, coords = c("lat", "lon"), crs = 4326)





(inset <- ggplotGrob(ggplot() +
                       geom_sf(data = world, inherit.aes = F) +
                       geom_point(data = center_df, aes(x=x, y=y), shape = 19, size = 5)+
                       coord_sf(xlim = c(-86.5, -82), ylim = c(8, 12)) +
                       theme_map() +
                       theme(panel.background = element_rect(fill = NULL),
                             panel.border = element_rect(color = "black",
                                                         fill = NA,
                                                         size = 1))+
                     NULL)
)


(main <- ggmap(base_map, darken = c(0.6, "white")) +
    geom_sf(data = pa_filt, inherit.aes = F, color = "black", fill = NA, linewidth = 1) +
    geom_sf(data = trans_sf, inherit.aes = F, aes(color = cond), size = 3) +
    coord_sf(xlim = c(-84.845, -84.834), ylim = c(10.298, 10.32)) +
    # coord_equal()+
    # guides(alpha=FALSE, size=FALSE) + 
    scale_color_manual(values = pal, name = "Successional Stage", 
                       labels = c("Pasture", "T-7", "T-10", "T-15", "Old"))+
    scalebar(transform = T, dist_unit = "km", dist = .5, y.max = 10.32, y.min = 10.3,
             x.min = -84.844, x.max =-84.835, height = 0.006)+
    north(y.max = 10.32, y.min = 10.3, x.min = -84.844, x.max =-84.835,
          scale = .1, symbol = 12, location = "topleft") +
    xlab("")+
    ylab("")+
    theme(text = element_text(family = "Arial", size = 8) )+
    # north(pa_filt, location = "topright", symbol = 12)+
    NULL)



(mapfig <- main + annotation_custom(grob = inset, xmin = -84.838, xmax = -84.834,
                         ymin = 10.315, ymax = 10.320))

ggsave(filename = "../figures/fig1 - map.pdf", width = 4.5, height = 6, dpi = 300, device = cairo_pdf)

ggdraw(main) +
  draw_plot(
    {
      inset
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.55, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = .7,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.25, 
    height = 0.3)
