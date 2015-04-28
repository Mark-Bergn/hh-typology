theme_attitude_upright <- function(base_size = 18, base_family = "Helvetica") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #axis.line   	= element_line(size=0.5, colour="grey70"),
      axis.line 		= element_blank(),
      #axis.line.y		= element_line(size=0.5, colour="grey70"),
      #axis.line.x		= element_blank(),
      plot.title		= element_text(size = rel(0.8), colour="black", face="bold", hjust=0),
      #strip.text.x 		= element_text(size=18, colour="black", face= "bold"),
      #strip.text.y 		= element_text(size=18, face="bold", angle=90),
      #strip.background 	= element_rect(colour="white", fill="#CCCCCC"),
      axis.text.x		= element_text(size = rel(0.8), colour="black"),
      #axis.text.x		= element_blank(),
      #axis.text.y		= element_text(size = rel(1.0), colour="black", vjust=0.5, hjust=1),
      axis.text.y		= element_blank(),
      #axis.title.x		= element_text(size= rel(0.8), colour="grey50", face='italic'),
      axis.title.x		= element_blank(),
      axis.title.y		= element_blank(),
      #axis.title.y		= element_text(size= rel(0.8), colour="grey50", angle=90, face='italic'),
      axis.ticks       = element_blank(),
      #axis.ticks		= element_line(colour='black'),
      legend.key        = element_blank(),
      legend.background = element_blank(),
      legend.key.size	= unit(1.2, "lines"),
      legend.text		= element_text(size= rel(0.8)),
      legend.title		= element_text(size= rel(0.8), face="bold"),
      #legend.title		= element_blank(),
      legend.position	= 'top',
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      plot.background	= element_blank(),	
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.margin		= unit(c(1.0,1.0,1.0,1.0), "lines")
    )
}

