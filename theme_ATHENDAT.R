## ========================= Temas de colores personalizados para proyecto ATHENDAT ============================================

library(extrafont)


pal <- c("#ef233c", "#ee6c4d", "#815c4e", "#3d5a80","#6bab90")

# Create Base Theme
#------------------
theme_ATHENDAT_claro <- function(base_size = 10,
                                 base_family = "Raleway")
{
  color.background = "#FFFFFF"                # color de fondo
  color.grid.major = "#D9D9D9"                # color de líneas de cuadrícula
  color.axis.text = "#666666"                 # color del texto de los ejes
  color.axis.title = "#666666"                # color del título de los ejes
  color.title = "#666666"                     # color del título del gráfico
  color.subtitle = "#666666"                  # color del subtítulo del gráfico
  strip.background.color = '#9999CC'          # color de lasa tiras fondo
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Estableciendo toda la región del gráfico de color gris claro
    theme(panel.background = element_rect(fill = color.background, 
                                          color = color.background)) +
    
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    
    theme(panel.border = element_rect(color = color.background)) +
    
    # Formato de la cuadrícula
    theme(panel.grid.major = element_line(color = color.grid.major, 
                                          size = .55, 
                                          linetype = "dotted")) +
    
    theme(panel.grid.minor = element_line(color = color.grid.major, 
                                          size = .55, 
                                          linetype = "dotted")) +
    theme(axis.ticks = element_blank()) +
    
    # Formato de la leyenda, oculta por defecto
    theme(legend.position = "none") +
    
    theme(legend.background = element_rect(fill = color.background)) +
    
    theme(legend.text = element_text(size = base_size-3, 
                                     color = color.axis.title, 
                                     family = base_family)) +
    
    theme(strip.text.x = element_text(size = base_size, 
                                      color = color.background, 
                                      family = base_family)) +
    
    theme(strip.text.y = element_text(size = base_size, 
                                      color = color.background, 
                                      family = base_family)) +
    
    theme(strip.background = element_rect(fill = strip.background.color, 
                                          linetype="blank")) +
    
    theme(strip.background = element_rect(fill = "grey70", 
                                          colour = NA)) +
    
    theme(panel.border = element_rect(fill = NA, 
                                      colour = "grey70", 
                                      size = rel(1))) +
    
    # Formato de las etiquetas de título, eje y marcas de verificación
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5)) +
    
    theme(plot.subtitle = element_text(color = color.subtitle, size = base_size + 2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x = element_text(size = base_size, 
                                     color = color.axis.text, 
                                     family = base_family)) +
    
    theme(axis.text.y = element_text(size = base_size, 
                                     color = color.axis.text, 
                                     family = base_family)) +
    
    theme(text = element_text(size = base_size, 
                              color = color.axis.text, 
                              family = base_family)) +
    
    theme(axis.title.x = element_text(size = base_size + 2, 
                                      color = color.axis.title, 
                                      vjust = 0, 
                                      family = base_family)) +
    
    theme(axis.title.y = element_text(size = base_size + 2, 
                                      color = color.axis.title, 
                                      vjust = 1.25, 
                                      family = base_family)) +
    
    theme(plot.caption = element_text(size = base_size - 2, 
                                      color = color.axis.title, 
                                      vjust = 1.25, 
                                      family = base_family)) +
    
    # Leyenda  
    theme(legend.text = element_text(size = base_size, 
                                     color = color.axis.text, 
                                     family = base_family)) +
    
    theme(legend.title = element_text(size = base_size, 
                                      color = color.axis.text, 
                                      family = base_family)) +
    
    theme(legend.key = element_rect(colour = color.background, 
                                    fill = color.background)) +
    
    theme(legend.position = "top", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t = 0, 
                                 r = 0, 
                                 b = 0, 
                                 l = 0, 
                                 unit="cm")) +
    
    # Márgenes del gráfico
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}
