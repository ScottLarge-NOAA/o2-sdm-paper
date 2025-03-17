plot_predict <- function(test_predict_O2, test_year, us_coast_proj) {
  # Plot ####
  data_plot <- ggplot(us_coast_proj) + geom_sf() +
    geom_point(
      data = test_predict_O2,
      aes(
        x = X * 1000,
        y = Y * 1000,
        col = do
      ),
      size = 1.0,
      alpha = 1.0
    ) +
    scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
    ylim(ylimits[1], ylimits[2]) +
    scale_colour_viridis_c(
      limits = c(0, 200),
      oob = scales::squish,
      name = bquote(O[2]),
      breaks = c(0, 100, 200)
    ) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank()
      ,
      panel.grid.minor = element_blank()
      ,
      panel.border = element_blank()
      ,
      strip.background = element_blank()
      ,
      strip.text = element_blank()
    ) +
    theme(axis.line = element_line(color = "black")) +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.position = "bottom") +
    guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                      0.5))

  predict_plot <- ggplot(us_coast_proj) + geom_sf() +
    geom_point(
      data = test_predict_O2,
      aes(
        x = X * 1000,
        y = Y * 1000,
        col = (est)
      ),
      size = 1.0,
      alpha = 1.0
    ) +
    scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
    ylim(ylimits[1], ylimits[2]) +
    scale_colour_viridis_c(
      limits = c(0, 200),
      oob = scales::squish,
      name = bquote(O[2]),
      breaks = c(0, 100, 200)
    ) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank()
      ,
      panel.grid.minor = element_blank()
      ,
      panel.border = element_blank()
      ,
      strip.background = element_blank()
      ,
      strip.text = element_blank()
    ) +
    theme(axis.line = element_line(color = "black")) +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.position = "bottom") +
    guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                      0.5))

  residual_plot <- ggplot(us_coast_proj) + geom_sf() +
    geom_point(
      data = test_predict_O2,
      aes(
        x = X * 1000,
        y = Y * 1000,
        col = residual
      ),
      size = 1.0,
      alpha = 1.0
    ) +
    scale_x_continuous(breaks = c(-125, -120), limits = xlimits) +
    ylim(ylimits[1], ylimits[2]) +
    scale_colour_distiller(palette = "RdBu", limits = c(-50, 50)) +
    #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank()
      ,
      panel.grid.minor = element_blank()
      ,
      panel.border = element_blank()
      ,
      strip.background = element_blank()
      ,
      strip.text = element_blank()
    ) +
    theme(axis.line = element_line(color = "black")) +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.position = "bottom") +
    guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                      0.5))

  ## put all plots in one ####
  grid.arrange(data_plot, predict_plot, residual_plot, ncol = 3)
  # plot residuals vs. prediction
  resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = Y)) +
    geom_point() +
    scale_colour_viridis_c(
      limits = c(31, 50),
      oob = scales::squish,
      name = "latitude",
      breaks = c(35, 40, 45)
    ) +
    ggtitle(test_year) +
    labs(x = "Predicted", y = "Residual") +
    theme(legend.position = "none")
  pred_vs_actual <- ggplot(data = test_predict_O2, aes(x = o2, y = est, col = Y)) +
    geom_point() +
    scale_colour_viridis_c(
      limits = c(31, 50),
      oob = scales::squish,
      name = "latitude",
      breaks = c(35, 40, 45)
    ) +
    ggtitle(test_year) +
    labs(x = "Observed", y = "Predicted") +
    geom_abline(intercept = 0, slope = 1) +
    theme(legend.position = "none")
  return(grid.arrange(pred_vs_actual, resid_vs_pred, ncol = 2))
}

calc_rmse <- function(rmse_list, n){
  rmse2 <- as.data.frame(rmse_list)
  rmse2$n <- n
  rmse2 <- filter(rmse2, n>50)
  rmse2$rmse2 <- rmse2$rmse_list ^ 2
  rmse2$xminusxbarsq <- rmse2$n * rmse2$rmse2
  rmse2 <- drop_na(rmse2, xminusxbarsq)
  rmse_total<- sqrt(sum(rmse2$xminusxbarsq, na.rm=T) / sum(rmse2$n, na.rm=T))
  return(rmse_total)
}

plot_simple <- function(output, dat.2.use){
  #Separate test and training data, predictions, and models from output list
  train_data <- output[[1]]
  test_data <- output[[2]]
  preds <- output[[3]]
  models <- output[[4]]
  #Set latitude and longitude
  xlims <- c(min(dat.2.use$X)*1000, max(dat.2.use$X)*1000)
  ylims <- c(min(dat.2.use$Y)*1000, max(dat.2.use$Y)*1000)
  lats <- c(round(min(dat.2.use$latitude)),  round(max(dat.2.use$latitude)))
  lons <- c(round(min(dat.2.use$longitude)+2), round(max(dat.2.use$longitude)))
  data_map <-
    ggplot(us_coast_proj) + geom_sf() +
    geom_point(train_data, mapping=aes(x=X*1000, y=Y*1000, col=o2),
                       # data = train_data,
                        #aes(
                        # x = X * 1000,
                        # y = Y * 1000,
                        #  col = o2
                        #  ),
                        size = 1.0,
                        alpha = 1.0
             ) +
            ylim(ylims)+
            scale_x_continuous(breaks=lons, limits=xlims)+
             scale_colour_viridis_c(
               limits = c(0, 200),
               oob = scales::squish,
               name = bquote(O[2]),
               breaks = c(0, 100, 200)
             ) +
             labs(x = "Longitude", y = "Latitude") +
             theme_bw() +
             theme(
               panel.grid.major = element_blank()
               ,
               panel.grid.minor = element_blank()
               ,
               panel.border = element_blank()
               ,
               strip.background = element_blank()
               ,
               strip.text = element_blank()
             ) +
             theme(axis.line = element_line(color = "black")) +
             theme(axis.text = element_text(size = 11)) +
             theme(axis.title = element_text(size = 12)) +
             theme(legend.text = element_text(size = 11)) +
             theme(legend.position = "none")
             #guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                            #   0.5))

  dat_available <- ggplot(train_data, aes(x=year))+
    stat_count(aes(fill=survey))+
    theme_bw() +
    theme(
      panel.grid.major = element_blank()
      ,
      panel.grid.minor = element_blank()
      ,
      panel.border = element_blank()
      ,
      strip.background = element_blank()
      ,
      strip.text = element_blank()
    ) +
    theme(axis.line = element_line(color = "black")) +
    theme(axis.text = element_text(size = 11)) +
    theme(axis.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 11)) +
    theme(legend.position="bottom")+
    xlab("Year")+
    ylab("Number of \n observations")

  model_names <- c("Persistent Spatial", "Persistent Spatial + Year", "Year+Temp+Salinity", "Temp+Salinity+Spatio-temporal")
  test_year <- unique(test_data$year)
  test_region <- unique(test_data$region)
  pred_plots <- list()
  resid_plots <- list()
  pred_obs <- list()
  for (i in 1:length(preds)){
    try(pred_plots[[i]] <-  ggplot(us_coast_proj) + geom_sf() +
          geom_point(preds[[i]], mapping=aes(x=X*1000, y=Y*1000, col=o2),
                 size = 1.0,
                 alpha = 1.0
      ) +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
      scale_colour_viridis_c(
        limits = c(0, 200),
        oob = scales::squish,
        name = bquote(O[2]~Predictions),
        breaks = c(0, 100, 200)
      ) +
      labs(x = "Longitude", y = "Latitude") +
      theme_bw() +
      theme(
        panel.grid.major = element_blank()
        ,
        panel.grid.minor = element_blank()
        ,
        panel.border = element_blank()
        ,
        strip.background = element_blank()
        ,
        strip.text = element_blank()
      ) +
      theme(axis.line = element_line(color = "black")) +
      theme(axis.text = element_text(size = 11)) +
      theme(axis.title = element_text(size = 12)) +
      theme(legend.text = element_text(size = 11)) +
      theme(legend.position = "bottom") +
      guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                        0.5)))

    try(resid_plots[[i]] <-  ggplot(us_coast_proj) + geom_sf() +
          geom_point(preds[[i]], mapping=aes(x=X*1000, y=Y*1000, col=residual),
                 size = 1.0,
                 alpha = 1.0
      ) +
        scale_colour_distiller(palette = "RdBu", limits = c(-50, 50)) +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
      #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
      labs(x = "Longitude", y = "Latitude") +
      theme_bw() +
      theme(
        panel.grid.major = element_blank()
        ,
        panel.grid.minor = element_blank()
        ,
        panel.border = element_blank()
        ,
        strip.background = element_blank()
        ,
        strip.text = element_blank()
      ) +
      theme(axis.line = element_line(color = "black")) +
      theme(axis.text = element_text(size = 11)) +
      theme(axis.title = element_text(size = 12)) +
      theme(legend.text = element_text(size = 11)) +
      theme(legend.position = "bottom") +
      guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                        0.5)))

    try(pred_obs[[i]] <- ggplot(data = preds[[i]], aes(x = o2, y = est, col = latitude)) +
      geom_point() +
      scale_colour_distiller(
       # limits = c(31, 50),
       #oob = scales::squish,
       name = "latitude",
       palette="Greys"
       # breaks = c(35, 40, 45)
      ) +
      theme(legend.position = "bottom") +
      theme_bw() +
      theme(
        panel.grid.major = element_blank()
        ,
        panel.grid.minor = element_blank()
        ,
        panel.border = element_blank()
        ,
        strip.background = element_blank()
        ,
        strip.text = element_blank()
      ) +
      theme(axis.line = element_line(color = "black")) +
      theme(axis.text = element_text(size = 11)) +
      theme(axis.title = element_text(size = 12)) +
      theme(legend.text = element_text(size = 11)) +
      labs(x = "Observed", y = "Predicted") +
      geom_abline(intercept = 0, slope = 1)+
      theme(legend.position="bottom"))
  }
  # plot residuals vs. prediction
#  resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = Y)) +
   # geom_point() +
    #scale_colour_viridis_c(
    #  limits = c(31, 50),
    #  oob = scales::squish,
    #  name = "latitude",
    #  breaks = c(35, 40, 45)
   # ) +
   # ggtitle(test_year) +
   # labs(x = "Predicted", y = "Residual") +
   # theme(legend.position = "none")

  figure1 <- ggarrange(data_map, dat_available, labels=c("A", "B"),
                      ncol = 2, nrow = 1)
  figure1 <- annotate_figure(figure1, left="Training Data", fig.lab.size=14, fig.lab.face="bold")
  figure2 <- ggarrange(pred_plots[[1]], resid_plots[[1]], pred_obs[[1]], ncol=3, nrow=1, legend="none", labels=c("C", "D", "E"))
  figure2 <- annotate_figure(figure2, left=paste(model_names[1]), fig.lab.size=14, fig.lab.face="bold")
  figure3 <- ggarrange(pred_plots[[2]], resid_plots[[2]], pred_obs[[2]], ncol=3, nrow=1, legend="none")
  figure3 <- annotate_figure(figure3, left=paste(model_names[2]), fig.lab.size=14, fig.lab.face="bold")
  figure4 <- ggarrange(pred_plots[[3]], resid_plots[[3]], pred_obs[[3]], ncol=3, nrow=1, legend="none")
  figure4 <- annotate_figure(figure3, left=paste(model_names[3]), fig.lab.size=14, fig.lab.face="bold")
  figure5 <- try(ggarrange(pred_plots[[4]], resid_plots[[4]], pred_obs[[4]], ncol=3, nrow=1))
  figure5 <- try(annotate_figure(figure5, left=paste(model_names[4]), fig.lab.size=14, fig.lab.face="bold"))
  if(!is.list(figure5)){
    figure4 <- ggarrange(ggplot(), ggplot(), ggplot())
  }

  figure <- ggarrange(figure1, figure2, figure3, figure4, figure5, ncol=1, nrow=5, heights=c(1,1,1,1, 1.25), align="h")
  annotate_figure(figure, top=paste(test_year), fig.lab.size=18, fig.lab.face="bold")

  ggsave(
    paste("outputs/plots/", test_region, "/preds/preds_", test_year, ".pdf", sep=""),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = 8.5,
    height = 11,
    units = c("in"),
    dpi = 600,
    limitsize = TRUE
  )
  return(figure)
}

plot_glorys <- function(preds, dat.2.use){
  #Separate test and training data, predictions, and models from output list
  #Set latitude and longitude
  xlims <- c(min(dat.2.use$X)*1000, max(dat.2.use$X)*1000)
  ylims <- c(min(dat.2.use$Y)*1000, max(dat.2.use$Y)*1000)
  lats <- c(round(min(dat.2.use$latitude)),  round(max(dat.2.use$latitude)))
  lons <- c(round(min(dat.2.use$longitude)+2), round(max(dat.2.use$longitude)))
  test_region <- unique(preds$region)
  test_year <- unique(preds$year)

    try(pred_plot <-  ggplot(us_coast_proj) + geom_sf() +
          geom_point(preds, mapping=aes(x=X*1000, y=Y*1000, col=o2),
                     size = 1.0,
                     alpha = 1.0
          ) +
          ylim(ylims)+
          scale_x_continuous(breaks=lons, limits=xlims)+
          scale_colour_viridis_c(
            limits = c(0, 200),
            oob = scales::squish,
            name = bquote(O[2]~Predictions),
            breaks = c(0, 100, 200)
          ) +
          labs(x = "Longitude", y = "Latitude") +
          theme_bw() +
          theme(
            panel.grid.major = element_blank()
            ,
            panel.grid.minor = element_blank()
            ,
            panel.border = element_blank()
            ,
            strip.background = element_blank()
            ,
            strip.text = element_blank()
          ) +
          theme(axis.line = element_line(color = "black")) +
          theme(axis.text = element_text(size = 11)) +
          theme(axis.title = element_text(size = 12)) +
          theme(legend.text = element_text(size = 11)) +
          theme(legend.position = "bottom") +
          guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                            0.5)))+
    ggtitle("Predictions")

    try(resid_plot <-  ggplot(us_coast_proj) + geom_sf() +
          geom_point(preds, mapping=aes(x=X*1000, y=Y*1000, col=residual),
                     size = 1.0,
                     alpha = 1.0
          ) +
          scale_colour_distiller(palette = "RdBu", limits = c(-50, 50)) +
          ylim(ylims)+
          scale_x_continuous(breaks=lons, limits=xlims)+
          #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
          labs(x = "Longitude", y = "Latitude") +
          theme_bw() +
          theme(
            panel.grid.major = element_blank()
            ,
            panel.grid.minor = element_blank()
            ,
            panel.border = element_blank()
            ,
            strip.background = element_blank()
            ,
            strip.text = element_blank()
          ) +
          theme(axis.line = element_line(color = "black")) +
          theme(axis.text = element_text(size = 11)) +
          theme(axis.title = element_text(size = 12)) +
          theme(legend.text = element_text(size = 11)) +
          theme(legend.position = "bottom") +
          guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                            0.5)))+
        ggtitle("Prediction Residuals")

    try(pred_obs <- ggplot(data = preds, aes(x = o2, y = est, col = latitude)) +
          geom_point() +
          scale_colour_distiller(
            # limits = c(31, 50),
            #oob = scales::squish,
            name = "latitude",
            palette="Greys"
            # breaks = c(35, 40, 45)
          ) +
          theme(legend.position = "bottom") +
          theme_bw() +
          theme(
            panel.grid.major = element_blank()
            ,
            panel.grid.minor = element_blank()
            ,
            panel.border = element_blank()
            ,
            strip.background = element_blank()
            ,
            strip.text = element_blank()
          ) +
          theme(axis.line = element_line(color = "black")) +
          theme(axis.text = element_text(size = 11)) +
          theme(axis.title = element_text(size = 12)) +
          theme(legend.text = element_text(size = 11)) +
          labs(x = "Observed", y = "Predicted") +
          geom_abline(intercept = 0, slope = 1)+
          theme(legend.position="bottom"))

  # plot residuals vs. prediction
  #  resid_vs_pred <- ggplot(data = test_predict_O2, aes(x = (est), y = residual, col = Y)) +
  # geom_point() +
  #scale_colour_viridis_c(
  #  limits = c(31, 50),
  #  oob = scales::squish,
  #  name = "latitude",
  #  breaks = c(35, 40, 45)
  # ) +
  # ggtitle(test_year) +
  # labs(x = "Predicted", y = "Residual") +
  # theme(legend.position = "none")

  figure <- ggarrange(pred_plot, resid_plot, pred_obs, ncol=3, nrow=1, labels=c("A", "B", "C"), widths=c(1,1,2), heights=c(1,1,0.3))
  annotate_figure(figure, top=paste(test_year), fig.lab.size=18, fig.lab.face="bold")

  ggsave(
    paste("outputs/plots/", test_region, "/glorys/_glorys", test_year, ".pdf", sep=""),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = 8.5,
    height = 5.5,
    units = c("in"),
    dpi = 600,
    limitsize = TRUE
  )
  return(figure)
}

##Function to plot marginal effects of spatio-temporal model
plot_marginal_effects <- function(models,preds, dat.2.use, i){
  m <- try(models[[i]])
  if(is.list(m)){ #don't do all this if the model failed anyway
    xlims <- c(min(dat.2.use$X)*1000, max(dat.2.use$X)*1000)
    ylims <- c(min(dat.2.use$Y)*1000, max(dat.2.use$Y)*1000)
    lats <- c(round(min(dat.2.use$latitude)),  round(max(dat.2.use$latitude)))
    lons <- c(round(min(dat.2.use$longitude)+2), round(max(dat.2.use$longitude)))
    #Prediction dataframe for epsilon and omega
    preds <- try(preds[[i]])
    test_region <- unique(preds$region)
    test_year <- unique(preds$year)
  #marginal effects
    pdf(paste("outputs/plots/", test_region, "/margeffects/margeffects_", test_year,".pdf", sep=""))
    par(mfrow=c(2,2))
  visreg(m, "sigma0")
  visreg(m, "temp")
  visreg(m, "doy")
  visreg(m, "depth_ln")
  dev.off()

  omega <- ggplot(us_coast_proj) + geom_sf() +
        geom_point(preds, mapping=aes(x=X*1000, y=Y*1000, col=omega_s),
                   size = 1.0,
                   alpha = 1.0
        ) +
        scale_colour_distiller(palette = "RdBu") +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
        #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
        labs(x = "Longitude", y = "Latitude") +
        theme_bw() +
        theme(
          panel.grid.major = element_blank()
          ,
          panel.grid.minor = element_blank()
          ,
          panel.border = element_blank()
          ,
          strip.background = element_blank()
          ,
          strip.text = element_blank()
        ) +
        theme(axis.line = element_line(color = "black")) +
        theme(axis.text = element_text(size = 11)) +
        theme(axis.title = element_text(size = 12)) +
        theme(legend.text = element_text(size = 11)) +
        theme(legend.position = "bottom") +
        guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                          0.5))
  try(epsilon <- ggplot(us_coast_proj) + geom_sf() +
        geom_point(preds, mapping=aes(x=X*1000, y=Y*1000, col=epsilon_st),
                   size = 1.0,
                   alpha = 1.0
        ) +
        scale_colour_distiller(palette = "RdBu") +
        ylim(ylims)+
        scale_x_continuous(breaks=lons, limits=xlims)+
        #, limits = c(-40, 40), oob = scales::squish, name = bquote(O[2]), breaks = c(-40, 0, 40)) +
        labs(x = "Longitude", y = "Latitude") +
        theme_bw() +
        theme(
          panel.grid.major = element_blank()
          ,
          panel.grid.minor = element_blank()
          ,
          panel.border = element_blank()
          ,
          strip.background = element_blank()
          ,
          strip.text = element_blank()
        ) +
        theme(axis.line = element_line(color = "black")) +
        theme(axis.text = element_text(size = 11)) +
        theme(axis.title = element_text(size = 12)) +
        theme(legend.text = element_text(size = 11)) +
        theme(legend.position = "bottom") +
        guides(colour = guide_colourbar(title.position = "top", title.hjust =
                                          0.5)))

  ggsave(
    paste("outputs/plots/", test_region, "/spatiotemp_var/spatiotemp_var_", test_year, ".pdf", sep=""),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = 8.5,
    height = 11,
    units = c("in"),
    dpi = 600,
    limitsize = TRUE
  )

  }
  if(!is.list(m)){
    return(paste("model not fit"))
  }
}

#Convert GLORYS from .nc format
convert_glorys <- function(file_name, do_threshold, filter_depth, filter_time) {
  nc <- tidync(file_name)
  if(filter_depth){
  nc_df <- nc %>%
    hyper_tibble %>%
    group_by(longitude, latitude) %>%
    filter(depth == max(depth)) %>%
    ungroup()
  }
  if(!filter_depth){
    nc_df <- nc %>%
      hyper_tibble %>%
      group_by(longitude, latitude) %>%
      ungroup()
  }

  # replace DO below threshold with the threshold level
  nc_df <- nc_df %>%
    mutate(o2 = case_when(
      o2 < do_threshold ~ do_threshold,
      TRUE ~ o2  # Keep other values unchanged
    ))


  #Make columns numeric
  nc_df$longitude <- as.numeric(nc_df$longitude)
  nc_df$latitude <- as.numeric(nc_df$latitude)
  nc_df$depth <- as.numeric(nc_df$depth)

  # remove large list from memory
  rm(nc)
  nc_df$time <- as.Date(substr(nc_df$time, 1, 10))
  if(filter_time){
 # nc_df$time <- round(nc_df$time)
  days <- unique(nc_df$time)
  n_days <- length(days)
  first_day <- days[1]
  last_day <- days[n_days]
  days.2.use <- seq(first_day, last_day, by = 3)
  nc_df <- nc_df %>%
    filter(time %in% days.2.use)
  }

  #nc_df$time <- round(nc_df$time)
  #nc_df$time <- (as_datetime("1950-01-01")+hours(nc_df$time))
  nc_df <- nc_df %>%
    st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2]) %>%
    st_set_geometry(NULL)
  nc_df$doy <- as.POSIXlt(nc_df$time, format = "%Y-%b-%d")$yday
  nc_df$year <- year(nc_df$time)
  nc_df$month <- month(nc_df$time)
  return(nc_df)
}

isolate_preds <- function(x, models){
  preds <- try(x[["predictions"]][[paste(models)]])
  return(preds)
}

isolate_preds2 <- function(x){
  preds <- try(x[["predictions"]])
  return(preds)
}


combine_preds <- function(x, glorys, models){
  if(!glorys){
  preds <- lapply(x, isolate_preds, models)
  }
  if(glorys){
    preds <- lapply(x, isolate_preds2)
  }
  row_lt2 <- which(sapply(preds, is.data.frame))
  preds <- preds[row_lt2]
  preds <- bind_rows(preds)
  return(preds)
}

##For model comparison of SDMs
extract_pars <- function(model, model_name){
  model_name <- paste(model_name)
  if(is.list(model)){
  p1 <- try(tidy(model, effects="fixed", conf.int=T))
  p2 <- try(tidy(model, effects="ran_pars", conf.int=T))
  p <- try(as.data.frame(bind_rows(p1,p2)))
  p$model <- try(paste(model_name))
  }
  return(p)
  if(!is.list(model)){
    p <- data.frame(ncol=7, nrow=1)
    p$model <- paste(model_name)
  }
}

gobh_interpolate <- function(dat, test_region, filter_depth, filter_time, gloryswd, basewd, do_threshold){
  predictions <- list()
  yearlist <- sort(unique(dat$year))
  for (i in 1:length(yearlist)) {
    test_year <- yearlist[i]
    print(test_year)
    ##Trawl testing data
    data <- dat %>%
      filter(year==test_year)
    data <- as.data.frame(data)
    ##Pull GLORYS data
    setwd(gloryswd)
    if(test_region=="cc"){
      files <- list.files("wc_o2", pattern=paste(test_year))
      files <- paste("wc_o2/", files, sep="")
    }

    if(test_region=="bc"){
      files <- list.files("bc_o2", pattern=paste(test_year))
      files <- paste("bc_o2/", files, sep="")
    }

    if(test_region=="ebs"|test_region=="goa"|test_region=="ai"){
      files <- list.files("alaska_o2_combined", pattern=paste(test_year))
      files <- paste("alaska_o2_combined/", files, sep="")
    }

    #Convert GLOYRS file from .nc format to dataframe in right format
    print("converting GLORYS")
    if(length(files)==1){
      glorys <- convert_glorys(files, do_threshold, filter_depth, filter_time)
    }

    #Use function to get data for all files if multiple files in year (this is mostly a thing for Alaska)
    if(length(files)>1){
      # get first year of glorys
      glorys <- convert_glorys(files[1], do_threshold, filter_depth, filter_time)
      # get remaining years of glorys and combine into single data frame
      for (j in 2:length(files)) {
        tmp_glorys <- convert_glorys(files[j], do_threshold, filter_depth, filter_time)
        glorys <- rbind(glorys, tmp_glorys)
      }
    }

    ##Pull in survey extent polygon from GLORYS data
    # Regional polygon
    poly <- filter(regions.hull, region==test_region)
    #Convert GLORYS to an sf
    glorys_sf <-  st_as_sf(glorys, coords = c("longitude", "latitude"), crs = st_crs(4326))
    # pull out observations within each region
    region_dat  <- st_filter(glorys_sf, poly)
    region_dat <- as.data.frame(region_dat)

    #Log depth
    region_dat$depth_ln <- log(region_dat$depth)

    #Set working directory back to base for saving
    setwd(basewd)

    #Mesh
    spde <- make_mesh(data = region_dat,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)

   region_dat$o2 <- region_dat$o2/100

    #Fit model
    print("fitting GLOBH model intercept")
    m <- try(sdmTMB(formula = o2 ~ 1 +s(depth_ln) + s(doy),
                    mesh = spde,
                    data = region_dat,
                    family = gaussian(),
                    spatial = "on",
                    spatiotemporal  = "off"))
    preds <- try(predict(m, data))
    if(is.list(m)){
      predictions[[i]] <- preds
    }
  }
  return(predictions)
}

#Run just breakpoint
model_comparison <- function(species, test_region, gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred){
  #Pull species data
  #Set seed
  # set.seed(5)

  #Fish data
  file <- list.files("data/processed_data", pattern=paste(species))
  file <- paste("data/processed_data/", file, sep="")[1]
  dat <- readRDS(file)
  dat <- as.data.frame(dat)
  dat$geometry <- NULL

  #Filter region
  if(test_region=="cc"){
    test_survey <- "nwfsc"
    #Filter to region
    dat <- filter(dat, survey==test_survey)
  }
  if(test_region=="bc"){
    test_survey <- "dfo"
    #Filter to region
    dat <- filter(dat, survey==test_survey)
  }
  dat <- as.data.frame(dat)

  #Merge temp data
  dat$temperature_C <- ifelse((is.na(dat$temperature_C)& !is.na(dat$bottom_temperature_c)), dat$bottom_temperature_c, dat$temperature_C)
  dat$temp <- dat$temperature_C
  #Remove NAs
  dat <- dat  %>%
    drop_na(depth,year, temp, X, Y)
  if(remove_outlier==T){
    #Remove outliers = catch > 10 sd above the mean
    dat$cpue_s <- scale(dat$cpue_kg_km2)
    dat <- dplyr::filter(dat, cpue_s <=20)
  }

  if(filter_size==T){
    dat$cpue_kg_km2 <- dat$cpue_kg_km2_sub
  }

  #Format columns
  dat$depth_ln <- log(dat$depth)
  dat$log_depth_scaled <- scale(log(dat$depth))
  dat$log_depth_scaled2 <- dat$log_depth_scaled^2
  dat$depth_s <- scale(dat$depth)
  dat$temp_s <- scale(dat$temp)

  ###Syoptic in situ only###
  dat_syn <- dat %>%
    drop_na(O2_umolkg)
  dat_syn$o2 <- dat_syn$O2_umolkg
  dat_syn$log_depth_scaled <- scale(log(dat_syn$depth))
  dat_syn$log_depth_scaled2 <- dat_syn$log_depth_scaled^2
  dat_syn$o2_s <- scale(dat_syn$o2)

  ###Fit model and predict to data###
  #Load oxygen data
  dat_o2 <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

  #Filter test region
  dat_o2 <- filter(dat_o2, region==test_region)

  #Remove any rows with missing dat_o2a
  dat_o2 <- dat_o2 %>%
    drop_na(depth, o2, temp,doy, X, Y, year)

  #Remove oxygen outliers
  dat_o2 <- filter(dat_o2, o2<1500)

  #Set minimum sigma
  minsigma0 <- 24
  dat_o2$sigma0[dat_o2$sigma0 <= minsigma0] <- minsigma0

  # remove older (earlier than 2000) dat_o2a
  dat_o2 <- dplyr::filter(dat_o2, year >=2000)

  #Log depth
  dat_o2$depth_ln <- log(dat_o2$depth)

  ##Fit in situ model ##
  #Extra years
  missing_years <- setdiff(unique(dat$year), unique(dat_o2$year))
  train_years <- seq(from=min(dat_o2$year), to=max(dat_o2$year), by=1)
  train_years2 <- setdiff(train_years, unique(dat_o2$year))
  extra_years <- append(missing_years, train_years2)
  extra_years <- extra_years[!is.na(extra_years)]
  if(length(extra_years)==0) {extra_years = NULL}

  spde <- make_mesh(data = dat_o2,
                    xy_cols = c("X", "Y"),
                    cutoff = 45)

  #Scale
  dat_o2$o2 <- dat_o2$o2/100
  dat_o2 <- as.data.frame(dat_o2)

  print("fitting integrated model")
  m <- sdmTMB(
    formula = o2  ~ 1+s(depth_ln) + s(doy)+s(temp),
    mesh = spde,
    dat = dat_o2,
    family = gaussian(),
    time = "year",
    spatial = "on",
    spatiotemporal  = "ar1",
    extra_time=c(extra_years))

  #Add predictions of O2
  dat_pred <- predict(m, newdata = dat)
  dat_pred$o2 <- dat_pred$est*100
  #Replace with zero
  dat_pred$o2 <- ifelse(dat_pred$o2<0, 0, dat_pred$o2)

  #Merge predicted and in situ data--use in situ when available, and predicted only when not
  if(combine_pred==T){
    dat_pred$o2 <- ifelse(is.na(dat_pred$O2_umolkg), dat_pred$o2, dat_pred$O2_umolkg)
  }
  dat_pred$o2_s <- scale(dat_pred$o2)

  if(GOBH!=T){
    dat_gobh <- dat
    dat_gobh$o2 <- dat_gobh$o2_gobh
    dat_gobh$o2 <- ifelse(dat_gobh$o2<0, 0, dat_gobh$o2)
    dat_gobh$o2_s <- scale(dat_gobh$o2)
    dat_gobh <- dat_gobh %>%
      drop_na(o2_s, temp, X, Y, depth, year)
  }

  if(GOBH==T){
    ##GOBH predictions
    #Set do threshold level for GLORYS data
    do_threshold <- 0

    #Filter days of GLORYS data to every 10th day?
    filter_time <- T

    #Filter GLORYS data to bottom depth? ()
    filter_depth <- F

    print("fitting and predicting GOBH model")
    #Apply
    dat_gobh <- gobh_interpolate(dat, "cc", filter_depth, filter_time, gloryswd, basewd, do_threshold)
    #Combine
    dat_gobh <- bind_rows(dat_gobh)

    #Get columns set up
    dat_gobh$o2 <- dat_gobh$est*100
    dat_gobh$o2 <- ifelse(dat_gobh$o2<0, 0, dat_pred$o2)
    dat_gobh$o2_s <- scale(dat_gobh$o2)

  }

  #Filter years to the same years as synoptic?
  if(filter_years==T){
    dat_pred <- dat_pred %>% drop_na(O2_umolkg)
    dat_gobh <- dat_gobh %>% drop_na(O2_umolkg)
  }

  #List the dataframes to test
  dats <- list(dat_syn, dat_pred, dat_gobh)
  #List data names
  dat_names <- c("Synoptic", "Predicted", "GOBH")
  model_names <- c("breakpt(o2)")

  #Create summary matrix
  par_summary <- list()
  preds <- list()
  models <- list()

  #if(new_breakpt==T){
 # remove.packages("sdmTMB")
#  remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE,  ref="newbreakpt")
#  library(sdmTMB)

  print("fitting SDMs")
  for (i in 1:length(dats)){
    ###Test alternative fish SDMs with the three different oxygen data
    dat <- as.data.frame(dats[i])
    paste(dat_names[i])
    extra_time <- setdiff(min(dat$year):max(dat$year),unique(dat$year))

    spde <- make_mesh(data = dat,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)

    print("fitting SDM m6")
      start <- matrix(0, nrow = 2, ncol = 1)
      start[1,1] <- 1
      start[2,1] <- -1
      lower <- matrix(0, nrow = 2, ncol = 1)
      lower[1,1] <- 0
      lower[2,1] <- -Inf
      upper <- matrix(0, nrow = 2, ncol = 1)
      upper[1,1] <- Inf
      upper[2,1] <- -Inf
    if(test_region=="cc"|(test_region=="bc"& species=="sablefish")){
      m6 <- try(sdmTMB(
        formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+breakpt(o2_s),
        mesh = spde,
        data = dat,
        family = tweedie(link="log"),
        time=NULL,
        spatial = "on",
        spatiotemporal  = "off",
        anisotropy=TRUE))
    }
    if(test_region=="bc" & species=="dsole"){
      m6 <- try(sdmTMB(
        formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+breakpt(o2_s),
        mesh = spde,
        data = dat,
        family = tweedie(link="log"),
        time=NULL,
        spatial = "on",
        spatiotemporal  = "off",
        anisotropy=TRUE,
        control = sdmTMBcontrol(start = list(b_threshold = start))))
                                #lower = list(b_threshold = lower),
                             #   upper=list(b_threshold = upper))))
    }
    model_list <- list(m6)
    names(model_list) <- model_names
    ##Extract each parameter table
    pars <- mapply(extract_pars, model_list, model_names, SIMPLIFY=F)
    pars <- try(bind_rows(pars))
    pars$data <- paste(dat_names[i])
    par_summary[[i]] <- pars

    ##Predict fish density
    temp <- list()
    for(j in 1:length(model_list)){
      dat <- dats[[i]]
      if(test_survey=="dfo"){pred_year <- 2019L}
      if(test_survey=="nwfsc"){pred_year <- 2012L}
      nd_po2 <- data.frame(o2_s = seq(min(dat$o2_s), max(dat$o2_s), length.out = 300),
                        #   temp_s = 0,
                           log_depth_scaled=0,
                           log_depth_scaled2=0,
                           year = pred_year)
      nd_po2 <- convert_class(nd_po2)
      p1 <- predict(model_list[[j]], newdata = nd_po2, se_fit = TRUE, re_form = NA)
      p1$model <- paste(model_names[j])
      p1$data <- paste(dat_names[i])
      temp[[j]] <- p1
    }
    names(temp) <- model_names
    preds[[i]] <- temp
    models[[i]] <- model_list
  }
  par_summary <- as.data.frame(bind_rows(par_summary))
  names(models) <- dat_names
  names(preds) <- dat_names
  names(dats) <- dat_names
  output <- list(dats,models, par_summary, preds)
  names(output) <- c("data", "models", "parameter_estimates", "predictions")

  return(output)
}

##Run full model comparison
model_comparison3 <- function(species, test_region, gloryswd, basewd, GOBH, remove_outlier, filter_size, new_breakpt, filter_years, combine_pred){
  #Pull species data
  #Set seed
  # set.seed(5)

  #Fish data
  file <- list.files("data/processed_data", pattern=paste(species))
  file <- paste("data/processed_data/", file, sep="")[1]
  dat <- readRDS(file)
  dat <- as.data.frame(dat)
  dat$geometry <- NULL

  #Filter region
  if(test_region=="cc"){
    test_survey <- "nwfsc"
    #Filter to region
    dat <- filter(dat, survey==test_survey)
  }
  if(test_region=="bc"){
    test_survey <- "dfo"
    #Filter to region
    dat <- filter(dat, survey==test_survey)
  }
  dat <- as.data.frame(dat)

  #Merge temp data
  dat$temperature_C <- ifelse((is.na(dat$temperature_C)& !is.na(dat$bottom_temperature_c)), dat$bottom_temperature_c, dat$temperature_C)
  dat$temp <- dat$temperature_C
  #Remove NAs
  dat <- dat  %>%
    drop_na(depth,year, temp, X, Y)
  if(remove_outlier==T){
    #Remove outliers = catch > 10 sd above the mean
    dat$cpue_s <- scale(dat$cpue_kg_km2)
    dat <- dplyr::filter(dat, cpue_s <=20)
  }

  if(filter_size==T){
    dat$cpue_kg_km2 <- dat$cpue_kg_km2_sub
  }

  #Format columns
  dat$depth_ln <- log(dat$depth)
  dat$log_depth_scaled <- scale(log(dat$depth))
  dat$log_depth_scaled2 <- dat$log_depth_scaled^2
  dat$temp_s <- scale(dat$temp)

  ###Syoptic in situ only###
  dat_syn <- dat %>%
    drop_na(O2_umolkg)
  dat_syn$o2 <- dat_syn$O2_umolkg
  dat_syn$o2_s <- scale(dat_syn$o2)
  dat_syn$log_depth_scaled <- scale(log(dat_syn$depth))
  dat_syn$log_depth_scaled2 <- dat_syn$log_depth_scaled^2
  dat_syn$o2_s <- scale(dat_syn$o2)

  ###Fit model and predict to data###
  #Load oxygen data
  dat_o2 <- as.data.frame(readRDS("data/processed_data/all_o2_dat_filtered.rds"))

  #Filter test region
  dat_o2 <- filter(dat_o2, region==test_region)

  #Remove any rows with missing dat_o2a
  dat_o2 <- dat_o2 %>%
    drop_na(depth, o2, temp,doy, X, Y, year)

  #Remove oxygen outliers
  dat_o2 <- filter(dat_o2, o2<1500)

  #Set minimum sigma
  minsigma0 <- 24
  dat_o2$sigma0[dat_o2$sigma0 <= minsigma0] <- minsigma0

  # remove older (earlier than 2000) dat_o2a
  dat_o2 <- dplyr::filter(dat_o2, year >=2000)

  #Log depth
  dat_o2$depth_ln <- log(dat_o2$depth)

  ##Fit in situ model ##
  #Extra years
  missing_years <- setdiff(unique(dat$year), unique(dat_o2$year))
  train_years <- seq(from=min(dat_o2$year), to=max(dat_o2$year), by=1)
  train_years2 <- setdiff(train_years, unique(dat_o2$year))
  extra_years <- append(missing_years, train_years2)
  extra_years <- extra_years[!is.na(extra_years)]
  if(length(extra_years)==0) {extra_years = NULL}

  spde <- make_mesh(data = dat_o2,
                    xy_cols = c("X", "Y"),
                    cutoff = 45)

  #Scale
  dat_o2$o2 <- dat_o2$o2/100
  dat_o2 <- as.data.frame(dat_o2)

  print("fitting integrated model")
  m <- sdmTMB(
    formula = o2  ~ 1+s(depth_ln) + s(doy)+s(temp),
    mesh = spde,
    dat = dat_o2,
    family = gaussian(),
    time = "year",
    spatial = "on",
    spatiotemporal  = "ar1",
    extra_time=c(extra_years))

  #Add predictions of O2
  dat_pred <- predict(m, newdata = dat)
  dat_pred$o2 <- dat_pred$est*100
  #Replace with zero
  dat_pred$o2 <- ifelse(dat_pred$o2<0, 0, dat_pred$o2)
  dat_pred$o2_s <- scale(dat_pred$o2)

  if(GOBH!=T){
    dat_gobh <- dat
    dat_gobh$o2 <- dat_gobh$o2_gobh
    dat_gobh$o2 <- ifelse(dat_gobh$o2<0, 0, dat_gobh$o2)
    dat_gobh$o2_s <- scale(dat_gobh$o2)
    dat_gobh <- dat_gobh %>%
      drop_na(o2_s, temp, X, Y, depth, year)
  }

  if(GOBH==T){
    ##GOBH predictions
    #Set do threshold level for GLORYS data
    do_threshold <- 0

    #Filter days of GLORYS data to every 10th day?
    filter_time <- T

    #Filter GLORYS data to bottom depth? ()
    filter_depth <- F

    print("fitting and predicting GOBH model")
    #Apply
    dat_gobh <- gobh_interpolate(dat, "cc", filter_depth, filter_time, gloryswd, basewd, do_threshold)
    #Combine
    dat_gobh <- bind_rows(dat_gobh)

    #Get columns set up
    dat_gobh$o2 <- dat_gobh$est*100
    dat_gobh$o2 <- ifelse(dat_gobh$o2<0, 0, dat_pred$o2)
    dat_gobh$o2_s <- scale(dat_gobh$o2)

  }

  #Filter years to the same years as synoptic?
  if(filter_years==T){
    dat_pred <- dat_pred %>% drop_na(O2_umolkg)
    dat_gobh <- dat_gobh %>% drop_na(O2_umolkg)
  }
  #Merge predicted and in situ data--use in situ when available, and predicted only when not
  if(combine_pred==T){
    dat_pred$o2 <- ifelse(is.na(dat_pred$O2_umolkg), dat_pred$o2, dat_pred$O2_umolkg)
  }

  #List the dataframes to test
  dats <- list(dat_syn, dat_pred, dat_gobh)
  #List data names
  dat_names <- c("Synoptic", "Predicted", "GOBH")
  model_names <- c("null", "temp","o2", "temp+o2", "temp+o2+temp*02", "breakpt(o2)", "breakpt(o2)+temp")

  #Create summary matrix
  aic <- as.data.frame(matrix(nrow=length(model_names), ncol=length(dat_names)))
  par_summary <- list()
  preds <- list()
  models <- list()

  if(new_breakpt==T){
    remove.packages("sdmTMB")
    remotes::install_github("pbs-assess/sdmTMB", dependencies = TRUE, ref="newlogistic")
    library(sdmTMB)
  }

  print("fitting SDMs")
  for (i in 1:length(dats)){
    ###Test alternative fish SDMs with the three different oxygen data
    dat <- as.data.frame(dats[i])
    paste(dat_names[i])
    extra_time <- setdiff(min(dat$year):max(dat$year),unique(dat$year))

    spde <- make_mesh(data = dat,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)
    print("fitting SDM m1")
    m1 <- try(sdmTMB(
      formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2,
      mesh = spde,
      data = dat,
      family = tweedie(link="log"),
      time=NULL,
      spatial = "on",
      spatiotemporal  = "off",
      anisotropy=TRUE
    ))
    print("fitting SDM m2")
    m2 <- try(sdmTMB(
      formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+temp_s,
      mesh = spde,
      data = dat,
      family = tweedie(link="log"),
      time=NULL,
      spatial = "on",
      spatiotemporal  = "off",
      anisotropy=TRUE
    ))
    print("fitting SDM m3")
    m3 <- try(sdmTMB(
      formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+o2_s,
      mesh = spde,
      data = dat,
      family = tweedie(link="log"),
      time=NULL,
      spatial = "on",
      spatiotemporal  = "off",
      anisotropy=TRUE
    ))
    print("fitting SDM m4")
    m4 <- try(sdmTMB(
      formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+temp_s+o2_s,
      mesh = spde,
      data = dat,
      family = tweedie(link="log"),
      time=NULL,
      spatial = "on",
      spatiotemporal  = "off",
      anisotropy=TRUE
    ))
    print("fitting SDM m5")
    m5 <- try(sdmTMB(
      formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+temp_s+o2_s+temp_s*o2_s,
      mesh = spde,
      data = dat,
      family = tweedie(link="log"),
      time=NULL,
      spatial = "on",
      spatiotemporal  = "off",
      anisotropy=TRUE
    ))
    print("fitting SDM m6")
    if(test_region=="bc"){
      start <- matrix(0, nrow = 2, ncol = 1)
      start[1,1] <- 1
      start[2,1] <- -1
      lower <- matrix(0, nrow = 2, ncol = 1)
      lower[1,1] <- 0
      lower[2,1] <- -Inf
      upper <- matrix(0, nrow = 2, ncol = 1)
      upper[1,1] <- Inf
      upper[2,1] <- -Inf
      m6 <- try(sdmTMB(
        formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+breakpt(o2_s),
        mesh = spde,
        data = dat,
        family = tweedie(link="log"),
        time=NULL,
        spatial = "on",
        spatiotemporal  = "off",
        anisotropy=TRUE,
        control = sdmTMBcontrol(start = list(b_threshold = start),
                                #lower = list(b_threshold = lower),
                                #  upper=list(b_threshold = upper)
        )))
    }
    if(test_region=="cc"){
      m6 <- try(sdmTMB(
        formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+breakpt(o2_s),
        mesh = spde,
        data = dat,
        family = tweedie(link="log"),
        time=NULL,
        spatial = "on",
        spatiotemporal  = "off",
        anisotropy=TRUE))
    }
    print("fitting SDM m7")
    if(test_region=="bc"){
      m7 <- try(sdmTMB(
        formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+breakpt(o2_s)+temp_s,
        mesh = spde,
        data = dat,
        family = tweedie(link="log"),
        time=NULL,
        spatial = "on",
        spatiotemporal  = "off",
        anisotropy=TRUE,
        control = sdmTMBcontrol(start = list(b_threshold = start),
                                #lower = list(b_threshold = lower),
                                #  upper=list(b_threshold = upper)
        )))
    }
    if(test_region=="cc"){
      m7 <- try(sdmTMB(
        formula = cpue_kg_km2  ~ 1+as.factor(year)+log_depth_scaled+log_depth_scaled2+breakpt(o2_s)+temp_s,
        mesh = spde,
        data = dat,
        family = tweedie(link="log"),
        time=NULL,
        spatial = "on",
        spatiotemporal  = "off",
        anisotropy=TRUE))
    }
    model_list <- list(m1, m2,m3, m4,m5,m6,m7)
    names(model_list) <- model_names

    #Other model ideas: smoother on depth, bivariate spline on depth and o2

    ##Add AIC to summary table
    temp <- matrix(nrow=7, ncol=2)
    temp[1,1] <- try(AIC(m1))
    temp[2,1] <- try(AIC(m2))
    temp[3,1] <- try(AIC(m3))
    temp[4,1] <- try(AIC(m4))
    temp[5,1] <- try(AIC(m5))
    temp[6,1] <- try(AIC(m6))
    temp[7,1] <- try(AIC(m7))

    temp[,2] <- try(abs(min(temp[,1], na.rm=T)-(temp[,1])))

    aic[,i] <- temp[,2]

    ##Extract each parameter table
    pars <- mapply(extract_pars, model_list, model_names, SIMPLIFY=F)
    pars <- try(bind_rows(pars))
    pars$data <- paste(dat_names[i])
    par_summary[[i]] <- pars

    ##Predict fish density
    temp <- list()
    for(j in 1:length(model_list)){
      dat <- dats[[i]]
      if(test_survey=="dfo"){pred_year <- 2019L}
      if(test_survey=="nwfsc"){pred_year <- 2012L}
      nd_po2 <- data.frame(o2_s = seq(min(dat$o2_s), max(dat$o2_s), length.out = 300),
                           temp_s = 0,
                           log_depth_scaled = 0,
                           log_depth_scaled2 = 0,
                           year = pred_year)
      nd_po2 <- convert_class(nd_po2)
      p1 <- predict(model_list[[j]], newdata = nd_po2, se_fit = TRUE, re_form = NA)
      p1$model <- paste(model_names[j])
      p1$data <- paste(dat_names[i])
      temp[[j]] <- p1
    }
    names(temp) <- model_names
    preds[[i]] <- temp
    models[[i]] <- model_list
  }
  par_summary <- as.data.frame(bind_rows(par_summary))
  colnames(aic) <- dat_names
  aic <- as.data.frame(aic)
  aic$data <- model_names
  names(models) <- dat_names
  names(preds) <- dat_names
  names(dats) <- dat_names
  output <- list(dats,models, aic, par_summary, preds)
  names(output) <- c("data", "models", "aic_table", "parameter_estimates", "predictions")

  return(output)
}

back.convert <- function(x, mean_orig, sd_orig) {
  x* sd_orig+mean_orig
}

convert_class <- function(x) {
  for (i in 1:ncol(x)) x[,i] <- as(x[,i], Class = "matrix")
  return(x)
}

#Pull and extract ITIS info to get species names
itis_extract <- function(catch){ #catch is dataframe with the itis numbers
  itis <- unique(catch$itis)

  scientific_names <-  taxize::classification(itis, db="itis")
  itis <- as.data.frame(itis)
  names <-lapply(scientific_names, extract_name)
  names <- unlist(names)
  itis$scientific_name <- tolower(names)
}

print_species <- function(type){
  biomass <- combine_all(type)
  species <- biomass[1:2]
  return(species)
}

load_all_hauls <- function() {
  install.packages("remotes")
  remotes::install_github("nwfsc-assess/nwfscSurvey")
  haul = nwfscSurvey::PullHaul.fn(SurveyName = "NWFSC.Combo")
  haul <- plyr::rename(haul, replace=c("salinity_at_gear_psu_der" = "sal",
                                       "temperature_at_gear_c_der" = "temp",
                                       "o2_at_gear_ml_per_l_der" = "o2",
                                       "depth_hi_prec_m" = "depth"))

  # read in the grid cell data from the survey design
  grid_cells = readxl::read_excel("data/Selection Set 2018 with Cell Corners.xlsx")
  grid_cells = dplyr::mutate(grid_cells,
                             depth_min = as.numeric(unlist(strsplit(grid_cells$Depth.Range,"-"))[1]),
                             depth_max = as.numeric(unlist(strsplit(grid_cells$Depth.Range,"-"))[2]))

  # convert grid_cells to sp object
  grid = SpatialPoints(cbind(grid_cells$Cent.Long,grid_cells$Cent.Lat),
                       proj4string = CRS("+proj=longlat +datum=WGS84"))
  r = raster::rasterize(x=grid, y = raster(nrow=length(unique(grid_cells$Cent.Lat)),
                                           ncol=length(unique(grid_cells$Cent.Long))))
  rasterToPoints(r)

  raster = aggregate(r, fact = 2)
  raster = projectRaster(raster, crs = "+proj=tmerc +lat_0=31.96 +lon_0=-121.6 +k=1 +x_0=390000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # create matrix of point data with coordinates and depth from raster
  grid = as.data.frame(rasterToPoints(raster))

  # Figure out the grid cell corresponding to each tow location
  haul$Cent.Lat = NA
  haul$Cent.Lon = NA
  haul$Cent.ID = NA
  for(i in 1:nrow(haul)) {
    indx = which(grid_cells$NW.LAT > haul$latitude_dd[i] &
                   grid_cells$SW.LAT < haul$latitude_dd[i] &
                   grid_cells$NW.LON < haul$longitude_dd[i] &
                   grid_cells$NE.LON > haul$longitude_dd[i])
    if(length(indx) > 0) {
      haul$Cent.ID[i] = grid_cells$Cent.ID[indx]
      haul$Cent.Lat[i] = grid_cells$Cent.Lat[indx]
      haul$Cent.Lon[i] = grid_cells$Cent.Long[indx]
    }
  }

  # project lat/lon to UTM, after removing missing values and unsatisfactory hauls
  haul = haul %>% filter(!is.na(Cent.Lon), performance == "Satisfactory")

  haul_trans = haul
  coordinates(haul_trans) <- c("Cent.Lon", "Cent.Lat")
  proj4string(haul_trans) <- CRS("+proj=longlat +datum=WGS84")
  newproj = paste("+proj=utm +zone=10 ellps=WGS84")
  haul_trans <- spTransform(haul_trans, CRS(newproj))
  haul_trans = as.data.frame(haul_trans)
  haul_trans$Cent.Lon = haul_trans$Cent.Lon/10000
  haul_trans$Cent.Lat = haul_trans$Cent.Lat/10000
  haul_trans$year = as.numeric(substr(haul_trans$date_yyyymmdd,1,4))

  haul$X = haul_trans$Cent.Lon
  haul$Y = haul_trans$Cent.Lat
  haul$year = haul_trans$year
  #haul$year_centered = haul$year - mean(unique(haul$year))

  return(haul)
}

load_data_nwfsc <- function(spc,sci_name, dat.by.size, length=T) {
  dat <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
  names(dat) = tolower(names(dat))
  dat$common_name <- tolower(dat$common_name)
  dat = dplyr::filter(dat, common_name == spc)
  if(is.data.frame(dat.by.size)){
    dat.by.size$trawl_id <- as.character(dat.by.size$trawl_id)
    dat <- left_join(dat, dat.by.size, by = "trawl_id")
    # remove tows where there was positive catch but no length measurements
  }
  if(!is.data.frame(dat.by.size)){
    dat$nlength <- 0
    dat$median_weight <- NA
    dat$haul_weight <- NA
    dat$p1 <- NA
    dat$p2 <- NA
    dat$p3 <- NA
    dat$p4 <- NA
  }
  # remove tows where there was positive catch but no length measurements
  if(length){
    dat <- dplyr::filter(dat, !is.na(p1))
  }
  # analyze or years and hauls with adequate oxygen and temperature data, within range of occurrence

  #O2 from trawl data is in ml/l
  # just in case, remove any missing or nonsense values from sensors
  # dat <- dplyr::filter(dat, !is.na(o2), !is.na(sal), !is.na(temp), is.finite(sal))
  # dat <- calc_po2_mi(dat)
  # dat <- dplyr::filter(dat, !is.na(temp), !is.na(mi))

  # prepare data and models -------------------------------------------------
  dat$longitude <- dat$longitude_dd
  dat$latitude <- dat$latitude_dd
  dat$event_id <- dat$trawl_id
  dat$date <- as.POSIXct(as.Date(as.POSIXct("1970-01-01")+as.difftime(dat$date,units="days")))
  # get julian day
  dat$julian_day <- rep(NA, nrow(dat))
  for (i in 1:nrow(dat)){
    dat$julian_day[i] <- as.POSIXlt(dat$date[i], format = "%Y-%b-%d")$yday
  }
  dat <- dplyr::select(dat, event_id, common_name, project, vessel, tow, year, date, longitude_dd, latitude_dd, longitude, latitude, cpue_kg_km2,
                       depth_m, julian_day, nlength, median_weight, haul_weight, pass, p1, p2, p3, p4)

  # UTM transformation
  dat_ll = dat
  sp::coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
  sp::proj4string(dat_ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  # convert to utm with spTransform
  dat_utm = sp::spTransform(dat_ll,
                            sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
  # convert back from sp object to data frame
  dat = as.data.frame(dat_utm)
  dat = dplyr::rename(dat, X = coords.x1,
                      Y = coords.x2)
  dat$scientific_name <- sci_name
  dat$depth <- dat$depth_m
  dat$depth_m <- NULL
  dat$survey <- "nwfsc"
  return(dat)
}


# Species of interest and max. juvenile lengths (define ontogenetic classes)
length_expand_nwfsc <- function(spc, sci_name) {
  # load, clean, and join data
  bio <- readRDS("data/fish_raw/NOAA/nwfsc_bio.rds")
  catch <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
  names(catch) = tolower(names(catch))
  names(bio) = tolower(names(bio))
  bio$scientific_name <- tolower(bio$scientific_name)
  bio$common_name <- tolower(bio$common_name)
  catch$common_name <- tolower(catch$common_name)

  bio$trawl_id = as.character(bio$trawl_id)
  catch$trawl_id=as.character(catch$trawl_id)

  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)

  #Combine data
  dat = dplyr::left_join(filter(bio[,c("trawl_id", "year", "scientific_name", "common_name", "weight", "ageing_lab", "length_cm", "width_cm", "sex", "age")], !is.na(length_cm)),
                         catch[,c("trawl_id","common_name", "subsample_count","area_swept_ha","longitude_dd", "latitude_dd","subsample_wt_kg","total_catch_numbers","total_catch_wt_kg","cpue_kg_km2")],
                         relationship = "many-to-many")


  # filter out species of interest from joined (catch/haul/bio) dataset
  dat_sub = dplyr::filter(dat, common_name == spc)

  #Warning if species not present
  if(nrow(dat_sub)==0){
    warning("species not present in data")
  }

  # fit length-weight regression by year to predict fish weights that have lengths only.
  # note a rank-deficiency warning may indicate there is insufficient data for some year/sex combinations (likely for unsexed group)
  #Create one set of data with only year/survey combinations with at least some weight data:

  if(nrow(dat_sub)>0) {
    #Add column counting number of length observations per catch
    dat_sub$is_length <- ifelse(!is.na(dat_sub$length_cm), 1,0)
    dat_sub <- group_by(dat_sub, trawl_id) %>% mutate(nlength=sum(is_length)) %>% ungroup()
    dat_sub$trawl_id <- as.numeric(dat_sub$trawl_id)

    fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
      group_by(year) %>%
      mutate(sum = sum(weight, na.rm=T)) %>%
      filter(sum>0) %>%
      ungroup()
    fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
    #And one for those without any weight data:
    not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
      group_by(year) %>%
      mutate(sum = sum(weight, na.rm=T)) %>%
      filter(sum==0) %>%
      ungroup()

    if(nrow(fitted)>0){
      fitted <-
        group_nest(fitted, year) %>%
        mutate(
          model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
          tidied = purrr::map(model, broom::tidy),
          augmented = purrr::map(model, broom::augment),
          predictions = purrr::map2(data, model, modelr::add_predictions)
        )

      # replace missing weights with predicted weights
      dat_pos2 = fitted %>%
        tidyr::unnest(predictions) %>%
        dplyr::select(-data, -model, -tidied, -augmented) %>%
        dplyr::mutate(weight = ifelse(is.na(weight), exp(pred), weight))

      dat_pos <- bind_rows(dat_pos2, not_fitted)
    }

    if(nrow(fitted)==0 & nrow(not_fitted)>0){
      dat_pos <- not_fitted
    }

    if(nrow(fitted)>0 | nrow(not_fitted)>0){
      #If there is no weight data available to get weight-length empirical interpolation, use fishbase to fill in
      # find length-weight relationship parameters for one species
      pars <- rfishbase::length_weight(
        #convert scientific name to first letter capitalized for fishbase
        species_list = stringr::str_to_sentence(sci_name, locale = "en"))
      #Get mean
      a <- mean(pars$a)
      b <- mean(pars$b)
      #Make NAs where text
      dat_pos$weight <- ifelse(dat_pos$weight=="NaN", NA, dat_pos$weight)
      #Remove any zero weights
      dat_pos$weight <- ifelse(dat_pos$weight==0, NA, dat_pos$weight)
      #Calculate weight (and convert from g to kg)
      dat_pos <- dplyr::mutate(dat_pos, weight = ifelse(is.na(weight), ((a*length_cm^b)*0.001), weight))
      #Add column getting the mean individual weight
      dat_test <- group_by(dat_pos, trawl_id) %>% mutate(haul_weight=mean(weight)) %>% ungroup()

      trawlids <- unique(dat_pos$trawl_id)
      if(length(trawlids!=0)){
        p <- data.frame(trawl_id = trawlids,
                        p1 = 0,
                        p2 = 0,
                        p3 = 0,
                        p4 = 0)

        sizethresholds <- quantile(dat_pos$weight, c(0.15, 0.5, 0.85, 1), na.rm = T)
        for (i in 1:length(trawlids)) {
          haul_sample<- dplyr::filter(dat_pos, trawl_id == trawlids[i])
          if(nrow(haul_sample) > 0 | var(haul_sample$weight >0)) {
            # fit kernel density to weight frequency
            smoothed_w <- KernSmooth::bkde(haul_sample$weight, range.x = c(min(dat_pos$weight), max(dat_pos$weight)), bandwidth = 2)
            # make sure smoother predicts positive or zero density
            smoothed_w$y[smoothed_w$y<0] <- 0
            # calculate proportion by biomass and by number
            p_w_byweight <- smoothed_w$y * smoothed_w$x / sum(smoothed_w$x*smoothed_w$y)

            p_w_byweight[p_w_byweight<0] <- 0
            #p_w_bynum[p_w_bynum<0] <- 0

            p1 <- sum(p_w_byweight[smoothed_w$x<=sizethresholds[1]])
            p2 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[1] & smoothed_w$x <=sizethresholds[2]])
            p3 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[2] & smoothed_w$x <=sizethresholds[3]])
            p4 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[3]])

            p[i,2:5] <- c(p1, p2, p3, p4)

          }
          else {
            indx <- which(sizethresholds>haul_sample$weight)
            p[i, min(indx)+1] <- 1
          }
        }
        # add hauls with zero catch back in
        absent = filter(dat_sub, cpue_kg_km2 == 0)
        trawlids <- unique(absent$trawl_id)
        #  absent.df <- data.frame(trawl_id = trawlids,
        #      p1 = 0,
        #     p2 = 0,
        #     p3 = 0,
        #     p4 = 0)
        all_hauls <- p
        # all_hauls <- rbind(p, absent.df)
        all_hauls$trawl_id <- as.numeric(all_hauls$trawl_id)
        dat_sub$median_weight <- median(dat_sub$weight, na.rm=T)
        nlengths <- unique(dat_sub[,c("trawl_id","nlength", "median_weight")])
        meanweight <- unique(dat_test[,c("trawl_id","haul_weight")])
        all_hauls2 <- left_join(all_hauls, nlengths)
        all_hauls2 <- left_join(all_hauls2, meanweight)
        return(all_hauls2)
      }
    }
  }
  if(nrow(dat_sub)>0){
    if(nrow(fitted)==0 & nrow(not_fitted)==0){
      trawlids <- unique(dat_sub$trawl_id)
      absent.df <- data.frame(trawl_id = trawlids,
                              p1 = NA,
                              p2 = NA,
                              p3 = NA,
                              p4 = NA,
                              nlength=0,
                              haul_weight=NA,
                              median_weight=NA)
      return(absent.df)
    }
  }
  if(nrow(dat_sub)>0){
    if(length(trawlids)==0){
      trawlids <- unique(dat_sub$trawl_id)
      absent.df <- data.frame(trawl_id = trawlids,
                              p1 = NA,
                              p2 = NA,
                              p3 = NA,
                              p4 = NA,
                              nlength=0,
                              haul_weight=NA)
      return(absent.df)
    }
  }
  if(nrow(dat_sub)==0){

    return(warning("species not present in data"))
  }
}

# Species of interest and max. juvenile lengths (define ontogenetic classes)
length_expand_afsc <- function(sci_name) {
  # load, clean, and join data
  bio2 <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_all_levels.RDS")
  catch2 <- readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_cpue_zerofilled.RDS")

  #Isolate necessary parts of full data to get specimen weights/lengths per haul
  haul <- bio2$haul
  specimen <- bio2$specimen
  species <- bio2$species
  size <- bio2$size

  #make lowercase
  names(haul) <- tolower(names(haul))
  names(specimen) <- tolower(names(specimen))
  names(species) <- tolower(names(species))
  names(size) <- tolower(names(bio2$size))

  species$species_name <- tolower(species$species_name)

  #Combine size and species
  lengths <- dplyr::left_join(size, species)
  lengths <- dplyr::left_join(lengths, haul)

  #Expand to make separate row for each measurement
  lengths <- dplyr::filter(lengths, !is.na(frequency))
  lengths2 <- tidyr::uncount(lengths, weights=frequency)

  #Combine
  bio <- dplyr::left_join(specimen, species)
  bio <- dplyr::left_join(bio, haul)

  #Combine specimen and length data
  bio3 <- dplyr::bind_rows(bio, lengths)

  #Convert length from mm to cm
  bio3$length_cm <- bio3$length*0.1

  #Convert weight from g to kg
  bio3$weight <- bio3$weight*0.001

  #Combine catch data with species data
  names(catch2) <- tolower(names(catch2))
  catch <- dplyr::left_join(catch2, species)

  # filter out species of interest from joined (catch/haul/bio) dataset
  catch_sub = dplyr::filter(catch, species_name == sci_name)
  bio_sub = dplyr::filter(bio3, species_name == sci_name)

  #Select only necessary columns for joining
  catch4 <- catch_sub[,c("hauljoin", "survey", "year", "depth_m", "latitude_dd_start", "longitude_dd_start", "cpue_kgkm2", "species_name", "common_name")]
  bio4 <- dplyr::filter(bio_sub[,c("hauljoin", "performance", "species_name", "common_name", "length_cm", "weight", "sex", "age")], !is.na(length_cm))
  #Combine data
  dat <-dplyr::left_join(bio4, catch4, relationship = "many-to-many")
  dat <- dplyr::mutate(dat, trawl_id=hauljoin)
  #According to the codebook https://repository.library.noaa.gov/view/noaa/50147, 0 means Good performance, and the other numbers are for "Satisfactory, and then a "but"..."; negative numbers are Unsatisfactory
  #Dataset already includes only Good and Satisfactory hauls, Unsatisfactory are removed

  #If years=T in the function, this will subset data to 1999 onward to remove possibly funky data prior to 1999
  years <- F
  if(years){
    dat_sub = dplyr::filter(dat, year>1999)
  }

  if(!years){
    dat_sub = dat
  }

  #Add column counting number of length observations per catch
  dat_sub$is_length <- ifelse(!is.na(dat_sub$length_cm), 1,0)
  dat_sub <- group_by(dat_sub, trawl_id) %>% mutate(nlength=sum(is_length)) %>% ungroup()
  dat_sub$trawl_id <- as.numeric(dat_sub$trawl_id)

  #Warning if no species present
  if(nrow(dat_sub)==0){
    warning("species not present in data")
  }

  # fit length-weight regression by year to predict fish weights that have lengths only.
  # note a rank-deficiency warning may indicate there is insufficient data for some year/sex combinations (likely for unsexed group)

  if(nrow(dat_sub)>0){
    fitted = dat_sub
    # #If region=T in function, this will do the length-weight regression for each broad geographic region (EBS, NBS, and GOA)
    # if(region){
    #   # dplyr::select(trawl_id,year,
    #   #               subsample_wt_kg, total_catch_wt_kg, area_swept_ha_der, cpue_kg_km2,
    #   #               individual_tracking_id, sex, length_cm, weight) %>%
    #
    # #Create one set of data with only year/survey combinations with at least some weight data:
    #   fitted <-  filter(fitted, !is.na(length_cm)) %>%
    #     group_by(year,survey) %>%
    #     mutate(sum = sum(weight, na.rm=T)) %>%
    #     filter(sum>0) %>%
    #     ungroup()
    #
    #   #Remove erroneous zero weight observations because cause error in lm()
    #   fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
    #
    #  #And one for those without any weight data:
    #   not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
    #     group_by(year,survey) %>%
    #     mutate(sum = sum(weight, na.rm=T)) %>%
    #     filter(sum==0) %>%
    #     ungroup()
    #
    # #Fit regression model for years with data
    #
    #   fitted <-
    #   group_nest(fitted, year,survey) %>%
    #   mutate(
    #     model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
    #     tidied = purrr::map(model, broom::tidy),
    #     augmented = purrr::map(model, broom::augment),
    #     predictions = purrr::map2(data, model, modelr::add_predictions)
    #   )
    # }

    #If region=F in function, this will do length-weight regression for all of Alaska combined
    # dplyr::select(trawl_id,year,
    #               subsample_wt_kg, total_catch_wt_kg, area_swept_ha_der, cpue_kg_km2,
    #               individual_tracking_id, sex, length_cm, weight) %>%

    #Create one set of data with only year/survey combinations with at least some weight data:
    fitted <-  filter(fitted, !is.na(length_cm)) %>%
      group_by(year) %>%
      mutate(sum = sum(weight, na.rm=T)) %>%
      filter(sum>0) %>%
      ungroup()
    #And one for those without any weight data:
    not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
      group_by(year,survey) %>%
      mutate(sum = sum(weight, na.rm=T)) %>%
      filter(sum==0) %>%
      ungroup()

    if(nrow(fitted)>0){
      #Fit regression model for years with data
      #Remove weird zero weights
      fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
      fitted <-
        group_nest(fitted, year) %>%
        mutate(
          model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
          tidied = purrr::map(model, broom::tidy),
          augmented = purrr::map(model, broom::augment),
          predictions = purrr::map2(data, model, modelr::add_predictions)
        )
      # replace missing weights with predicted weights for years with data
      dat_pos2 = fitted %>%
        tidyr::unnest(predictions) %>%
        dplyr::select(-data, -model, -tidied, -augmented) %>%
        try(dplyr::mutate(weight = ifelse(is.na(weight), exp(pred), weight)))

      #combine back with data for years without data
      dat_pos <- bind_rows(dat_pos2, not_fitted)
    }
    if(nrow(fitted)==0){
      dat_pos <- not_fitted
    }
    ##Checked that this works by checking number of rows--looks like it passes!

    #If there is no weight data available to get weight-length empirical interpolation, use fishbase to fill in
    # find length-weight relationship parameters for one species
    pars <- rfishbase::length_weight(
      #convert scientific name to first letter capitalized for fishbase
      species_list = stringr::str_to_sentence(sci_name, locale = "en"))
    #Get mean
    a <- mean(pars$a)
    b <- mean(pars$b)
    #For longspine thornyhead, which is not in database for some reason, got these values from fishbase direct page
    if(sci_name=="sebastolobus altivelis"){
      a <- 0.00912
      b=3.09
    }
    #Calculate weight (and convert from g to kg)
    dat_pos <- dplyr::mutate(dat_pos, weight = ifelse(is.na(weight), ((a*length_cm^b)*0.001), weight))
    #convert cm-g units
    #haul level weight
    dat_test <- group_by(dat_pos, trawl_id) %>% mutate(haul_weight=mean(weight)) %>% ungroup()

    #make column of trawl_id, which is called hauljoin originally in the AFSC data
    trawlids <- unique(dat_pos$trawl_id)
    if(length(trawlids!=0)){
      p <- data.frame(trawl_id = trawlids,
                      p1 = 0,
                      p2 = 0,
                      p3 = 0,
                      p4 = 0)

      sizethresholds <- quantile(dat_pos$weight, c(0.15, 0.5, 0.85, 1), na.rm = T)
      for (i in 1:length(trawlids)) {
        haul_sample<- dplyr::filter(dat_pos, trawl_id == trawlids[i])
        if(nrow(haul_sample) > 0 | var(haul_sample$weight >0)) {
          # fit kernel density to weight frequency
          smoothed_w <- KernSmooth::bkde(haul_sample$weight, range.x = c(min(dat_pos$weight), max(dat_pos$weight)), bandwidth = 2)
          # make sure smoother predicts positive or zero density
          smoothed_w$y[smoothed_w$y<0] <- 0
          # calculate proportion by biomass and by number
          p_w_byweight <- smoothed_w$y * smoothed_w$x / sum(smoothed_w$x*smoothed_w$y)


          p_w_byweight[p_w_byweight<0] <- 0
          #p_w_bynum[p_w_bynum<0] <- 0

          p1 <- sum(p_w_byweight[smoothed_w$x<=sizethresholds[1]])
          p2 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[1] & smoothed_w$x <=sizethresholds[2]])
          p3 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[2] & smoothed_w$x <=sizethresholds[3]])
          p4 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[3]])

          p[i,2:5] <- c(p1, p2, p3, p4)

        }
        else {
          indx <- which(sizethresholds>haul_sample$weight)
          p[i, min(indx)+1] <- 1
        }
      }

      # add hauls with zero catch back in
      absent = filter(dat_sub, cpue_kgkm2 == 0)
      if(nrow(absent)>0){
        trawlids <- unique(absent$trawl_id)
        absent.df <- data.frame(trawl_id = trawlids,
                                p1 = 0,
                                p2 = 0,
                                p3 = 0,
                                p4 = 0)

        all_hauls <- rbind(p, absent.df)
      }
      if(nrow(absent)==0){
        all_hauls <- p
      }
      all_hauls$trawl_id <- as.numeric(all_hauls$trawl_id)
      dat_sub$median_weight <- median(dat_sub$weight, na.rm=T)
      nlengths <- unique(dat_sub[,c("trawl_id","nlength", "median_weight")])
      meanweight <- unique(dat_test[,c("trawl_id","haul_weight")])
      all_hauls2 <- left_join(all_hauls, nlengths)
      all_hauls2 <- left_join(all_hauls2, meanweight)
      return(all_hauls2)
    }
  }
  if(nrow(dat_sub)>0){
    if(nrow(fitted)==0 & nrow(not_fitted)==0){
      trawlids <- unique(dat_sub$trawl_id)
      absent.df <- data.frame(trawl_id = trawlids,
                              p1 = NA,
                              p2 = NA,
                              p3 = NA,
                              p4 = NA,
                              nlength=0,
                              haul_weight=NA,
                              median_weight=NA)
      return(absent.df)
    }
  }
  #If there are no hauls at all with any length measurements, do this instead (because caused an error in the kernel density function otherwise)
  if(nrow(dat_sub)>0){
    trawlids <- unique(dat_sub$trawl_id)
    if(length(trawlids)==0){
      absent.df <- data.frame(trawl_id = trawlids,
                              p1 = NA,
                              p2 = NA,
                              p3 = NA,
                              p4 = NA,
                              nlength=0,
                              haul_weight=NA)
      return(absent.df)
    }
  }
  if(nrow(dat_sub)==0){
    return(warning("species not present in data"))
  }
}

load_data_afsc <- function(sci_name, spc, dat.by.size, length=T) {
  bio2 <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_all_levels.RDS")
  dat <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_indivero_cpue_zerofilled.RDS")
  species <- bio2$species
  names(dat) = tolower(names(dat))
  names(species) =tolower(names(species))
  species$species_name <- tolower(species$species_name)
  species$common_name <- tolower(species$common_name)
  dat <- dplyr::left_join(dat, species)
  dat.by.size$trawl_id <- as.character(dat.by.size$trawl_id)
  dat$trawl_id <-as.character(dat$hauljoin)
  dat = dplyr::filter(dat, species_name ==sci_name)
  dat <- dplyr::left_join(dat, dat.by.size, by = "trawl_id")
  # remove tows where there was positive catch but no length measurements
  if(length){
    dat <- dplyr::filter(dat, !is.na(p1))
  }
  # analyze or years and hauls with adequate oxygen and temperature data, within range of occurrence

  # get julian day
  dat$julian_day <- rep(NA, nrow(dat))
  haul <- bio2$haul
  names(haul) <- tolower(names(haul))
  haul <- haul[,c("hauljoin", "start_time")]
  dat <- left_join(dat, haul)
  for (i in 1:nrow(dat)) dat$julian_day[i] <- as.POSIXlt(dat$start_time[i], format = "%Y-%b-%d")$yday

  #O2 from trawl data is in ml/l
  # just in case, remove any missing or nonsense values from sensors
  # dat <- dplyr::filter(dat, !is.na(o2), !is.na(sal), !is.na(temp), is.finite(sal))
  # dat <- calc_po2_mi(dat)
  # dat <- dplyr::filter(dat, !is.na(temp), !is.na(mi))

  # prepare data and models -------------------------------------------------
  dat$longitude_dd <- dat$longitude_dd_start
  dat$latitude_dd <- dat$latitude_dd_start
  dat$longitude <- dat$longitude_dd
  dat$latitude <- dat$latitude_dd
  dat$scientific_name <- dat$species_name
  dat$cpue_kg_km2 <- dat$cpue_kgkm2
  dat$project <- dat$survey
  dat$event_id <- dat$trawl_id
  dat$date <- as.POSIXct(as.Date(dat$start_time, format = "%Y-%b-%d"))
  dat <- dplyr::select(dat, event_id, common_name, scientific_name, project, survey, year, date, bottom_temperature_c, longitude_dd, latitude_dd, longitude, latitude, cpue_kg_km2,
                       depth_m, julian_day, nlength, median_weight, haul_weight, p1, p2, p3, p4)


  # UTM transformation
  dat_ll = dat
  sp::coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
  sp::proj4string(dat_ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  # convert to utm with spTransform
  dat_utm = sp::spTransform(dat_ll,
                            sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
  # convert back from sp object to data frame
  dat = as.data.frame(dat_utm)
  dat = dplyr::rename(dat, X = coords.x1,
                      Y = coords.x2)
  dat$depth <- dat$depth_m
  dat$depth_m <- NULL
  return(dat)
}

length_expand_bc <- function(sci_name, spc) {
  # load, clean, and join data
  itis <- readRDS("data/fish_raw/BC/species-table.rds")
  haul <- readRDS("data/fish_raw/BC/pbs-haul.rds")
  catch <- readRDS("data/fish_raw/BC/pbs-catch.rds")
  bio2 <- readRDS("data/fish_raw/BC/pbs-bio-samples.rds")

  #Merge the official BC bio data and the official BC haul data to get metadata (from Sean, or here: https://open.canada.ca/data/en/dataset/86af7918-c2ab-4f1a-ba83-94c9cebb0e6c)
  bio <- dplyr::full_join(bio2, haul, by="event_id", relationship="many-to-many")

  #Combine with species data
  bio <- dplyr::full_join(bio, itis, by="itis", relationship="many-to-many")

  #Put bio data in the same format as the NOAA bio data
  #Convert g to kg
  bio$weight <- bio$weight*0.001

  #rename columnns
  bio$scientific_name <- bio$species_science_name
  bio$common_name <- bio$species_common_name
  bio$length_cm <- bio$length

  #Clean catch data
  names(catch) = tolower(names(catch))

  #Merge with ITIS information to get scientific name
  catch <- left_join(catch,itis)

  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)

  #Combine catch data with haul data
  dat <- dplyr::left_join(catch, haul, relationship = "many-to-many")

  #Rename missing species
  if(sci_name=="sebastes aleutianus"){
    bio$scientific_name <- ifelse(str_detect(bio$scientific_name, "sebastes aleutianus"), "sebastes aleutianus", bio$scientific_name)
    bio$common_name <- ifelse(str_detect(bio$common_name, "rougheye"), "rougheye rockfish", bio$common_name)
  }
  dat$scientific_name <- dat$species_science_name
  dat$common_name <- dat$species_common_name
  dat$species_science_name <- NULL
  dat$species_common_name <- NULL
  #Combine bio/haul data with catch data
  dat <- dplyr::left_join(dat, filter(bio[,c("event_id", "age","length_cm", "weight", "scientific_name", "common_name")], !is.na(length_cm)), relationship = "many-to-many")

  # filter out species of interest from joined (catch/haul/bio) dataset
  dat_sub = dplyr::filter(dat, scientific_name==sci_name)

  dat_sub$event_id <- as.numeric(dat_sub$event_id)
  trawlids <- unique(dat_sub$event_id)

  if(nrow(dat_sub)>0) {
    #Add column counting number of length observations per catch
    dat_sub$is_length <- ifelse(!is.na(dat_sub$length_cm), 1,0)
    dat_sub <- group_by(dat_sub, event_id) %>% mutate(nlength=sum(is_length)) %>% ungroup()

    # fit length-weight regression by year to predict fish weights that have lengths only.
    # note a rank-deficiency warning may indicate there is insufficient data for some year/sex combinations (likely for unsexed group)

    #Create one set of data with only year/survey combinations with at least some weight data:
    fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
      group_by(year) %>%
      mutate(sum = sum(weight, na.rm=T)) %>%
      filter(sum>0) %>%
      ungroup()
    fitted$weight <- ifelse(fitted$weight==0, NA, fitted$weight)
    #And one for those without any weight data:
    not_fitted <-  filter(dat_sub, !is.na(length_cm)) %>%
      group_by(year) %>%
      mutate(sum = sum(weight, na.rm=T)) %>%
      filter(sum==0) %>%
      ungroup()

    #Fit regression model for years with data
    fitted <-
      group_nest(fitted, year) %>%
      mutate(
        model = purrr::map(data, ~ lm(log(weight) ~ log(length_cm), data = .x)),
        tidied = purrr::map(model, broom::tidy),
        augmented = purrr::map(model, broom::augment),
        predictions = purrr::map2(data, model, modelr::add_predictions)
      )

    # replace missing weights with predicted weights
    dat_pos2 = fitted %>%
      tidyr::unnest(predictions) %>%
      dplyr::select(-data, -model, -tidied, -augmented) %>%
      try(dplyr::mutate(weight = ifelse(is.na(weight), exp(pred), weight)))

    #combine back with data for years without data
    dat_pos <- bind_rows(dat_pos2, not_fitted)

    ##Checked that this works by checking number of rows--looks like it passes!

    #If there is no weight data available to get weight-length empirical interpolation, use fishbase to fill in
    # find length-weight relationship parameters for one species
    pars <- rfishbase::length_weight(
      #convert scientific name to first letter capitalized for fishbase
      species_list = stringr::str_to_sentence(sci_name, locale = "en"))
    #Get mean
    a <- mean(pars$a)
    b <- mean(pars$b)
    #For longspine thornyhead, which is not in database for some reason, got these values from fishbase direct page
    if(sci_name=="sebastolobus altivelis"){
      a <- 0.00912
      b=3.09
    }
    #Calculate weight (and convert from g to kg)
    dat_pos <- dplyr::mutate(dat_pos, weight = ifelse(is.na(weight), ((a*length_cm^b)*0.001), weight))
    dat_test <- group_by(dat_pos, event_id) %>% mutate(haul_weight=mean(weight)) %>% ungroup()

    trawlids <- unique(dat_pos$event_id)
    if(length(trawlids!=0)){
      p <- data.frame(event_id = trawlids,
                      p1 = 0,
                      p2 = 0,
                      p3 = 0,
                      p4 = 0)

      sizethresholds <- quantile(dat_pos$weight, c(0.15, 0.5, 0.85, 1), na.rm = T)
      for (i in 1:length(trawlids)) {
        haul_sample<- dplyr::filter(dat_pos, event_id == trawlids[i])
        if(nrow(haul_sample) > 0 | var(haul_sample$weight >0)) {
          # fit kernel density to weight frequency
          smoothed_w <- KernSmooth::bkde(haul_sample$weight, range.x = c(min(dat_pos$weight), max(dat_pos$weight)), bandwidth = 2)
          # make sure smoother predicts positive or zero density
          smoothed_w$y[smoothed_w$y<0] <- 0
          # calculate proportion by biomass and by number
          p_w_byweight <- smoothed_w$y * smoothed_w$x / sum(smoothed_w$x*smoothed_w$y)

          p_w_byweight[p_w_byweight<0] <- 0
          #p_w_bynum[p_w_bynum<0] <- 0

          p1 <- sum(p_w_byweight[smoothed_w$x<=sizethresholds[1]])
          p2 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[1] & smoothed_w$x <=sizethresholds[2]])
          p3 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[2] & smoothed_w$x <=sizethresholds[3]])
          p4 <- sum(p_w_byweight[smoothed_w$x>sizethresholds[3]])

          p[i,2:5] <- c(p1, p2, p3, p4)

        }
        else {
          indx <- which(sizethresholds>haul_sample$weight)
          p[i, min(indx)+1] <- 1
        }
      }

      # add hauls with zero catch back in
      absent = filter(dat_sub, catch_weight == 0)
      trawlids <- unique(absent$event_id)
      absent.df <- data.frame(event_id = trawlids,
                              p1 = 0,
                              p2 = 0,
                              p3 = 0,
                              p4 = 0)

      all_hauls <- rbind(p, absent.df)
      all_hauls$event_id <- as.numeric(all_hauls$event_id)
      dat_sub$median_weight <- median(dat_sub$weight, na.rm=T)
      nlengths <- unique(dat_sub[,c("event_id","nlength", "median_weight")])
      meanweight <- unique(dat_test[,c("event_id","haul_weight")])
      all_hauls2 <- left_join(all_hauls, nlengths)
      all_hauls2 <- left_join(all_hauls2, meanweight)
      return(all_hauls2)
    }
  }
  if(nrow(dat_sub)>0){
    if(nrow(fitted)==0 & nrow(not_fitted)==0){
      trawlids <- unique(dat_sub$trawl_id)
      absent.df <- data.frame(trawl_id = trawlids,
                              p1 = NA,
                              p2 = NA,
                              p3 = NA,
                              p4 = NA,
                              nlength=0,
                              haul_weight=NA,
                              median_weight=NA)
      return(absent.df)
    }
  }
  if(nrow(dat_sub)>0){
    if(length(trawlids)==0){
      absent.df <- data.frame(trawl_id = trawlids,
                              p1 = NA,
                              p2 =NA,
                              p3 = NA,
                              p4 = NA,
                              nlength=0,
                              haul_weight=NA)
      return(absent.df)
    }
  }
  if(nrow(dat_sub)==0){
    return(warning("species not present in data"))
  }
}

load_data_bc <- function(sci_name,dat.by.size, length=T, spc) {
  catch <- readRDS("data/fish_raw/BC/pbs-catch.rds")
  haul <- readRDS("data/fish_raw/BC/pbs-haul.rds")
  itis  <- readRDS("data/fish_raw/BC/itis_bc.rds")

  catch <- left_join(catch,itis)

  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)

  #Combine catch data with haul data
  dat <- dplyr::left_join(catch, haul, relationship = "many-to-many")

  # dat.by.size$event_id <- as.character(dat.by.size$event_id)
  dat = dplyr::filter(dat, scientific_name == sci_name)
  dat <- left_join(dat, dat.by.size, by = "event_id")
  # remove tows where there was positive catch but no length measurements
  if(length){
    dat <- dplyr::filter(dat, !is.na(p1))
  }
  # analyze or years and hauls with adequate oxygen and temperature data, within range of occurrence

  # get julian day
  dat$julian_day <- rep(NA, nrow(dat))
  for (i in 1:nrow(dat)) dat$julian_day[i] <- as.POSIXlt(dat$date[i], format = "%Y-%b-%d")$yday


  #O2 from trawl data is in ml/l
  # just in case, remove any missing or nonsense values from sensors
  # dat <- dplyr::filter(dat, !is.na(o2), !is.na(sal), !is.na(temp), is.finite(sal))
  # dat <- calc_po2_mi(dat)
  # dat <- dplyr::filter(dat, !is.na(temp), !is.na(mi))

  # prepare data and models -------------------------------------------------
  dat$cpue_kg_km2 <- dat$catch_weight
  dat$longitude_dd <- dat$lon_start
  dat$latitude_dd <- dat$lat_start
  dat$longitude <- dat$lon_start
  dat$latitude <- dat$lat_start
  dat$event_id <- as.character(dat$event_id)
  dat$year <- substr(dat$date, start=1, stop=4)
  dat$year <- as.integer(dat$year)
  dat$date <- as.POSIXct(as.Date(dat$date, format = "%Y-%b-%d"))
  dat$project <- dat$survey_name
  dat$salinity_psu <- dat$salinity_PSU
  dat$salinity_PSU <- NULL
  dat <- dplyr::select(dat, event_id, scientific_name, project, year, date, longitude_dd, latitude_dd, longitude, latitude, cpue_kg_km2,
                       depth_m, julian_day, nlength,median_weight, haul_weight, pass, p1, p2, p3, p4, temperature_C, do_mlpL, salinity_psu)
  dat <- filter(dat, !is.na(latitude_dd))


  # UTM transformation
  dat_ll = dat
  sp::coordinates(dat_ll) <- c("longitude_dd", "latitude_dd")
  sp::proj4string(dat_ll) <- sp::CRS("+proj=longlat +datum=WGS84")
  # convert to utm with spTransform
  dat_utm = sp::spTransform(dat_ll,
                            sp::CRS("+proj=utm +zone=10 +datum=WGS84 +units=km"))
  # convert back from sp object to data frame
  dat = as.data.frame(dat_utm)
  dat = dplyr::rename(dat, X = coords.x1,
                      Y = coords.x2)
  dat$common_name <- spc
  dat$depth <- dat$depth_m
  dat$depth_m <- NULL
  dat$survey <- "dfo"

  #convert oxygen mg/L to umol_kg
  SA = gsw_SA_from_SP(dat$salinity_psu,dat$depth,dat$longitude,dat$latitude) #absolute salinity for pot T calc
  pt = gsw_pt_from_t(SA,dat$temperature_C,dat$depth) #potential temp at a particular depth
  CT = gsw_CT_from_t(SA,dat$temperature_C,dat$depth) #conservative temp
  dat$sigma0_kgm3 = gsw_sigma0(SA,CT)
  dat$O2_umolkg = dat$do_mlpL*44660/(dat$sigma0_kgm3+1000)

  return(dat)
}

combine_all <- function(type){
  #BC
  #BC raw data
  bio_qc <- read.csv("data/fish_raw/BC/QCS_biology.csv")
  bio_vi <- read.csv("data/fish_raw/BC/WCVI_biology.csv")
  bio_hs <- read.csv("data/fish_raw/BC/HS_biology.csv")
  bio_hg <- read.csv("data/fish_raw/BC/WCHG_biology.csv")
  itis  <- readRDS("data/fish_raw/BC/itis_bc.rds")

  haul <- readRDS("data/fish_raw/BC/pbs-haul.rds")
  catch <- readRDS("data/fish_raw/BC/pbs-catch.rds")

  haul_qc <- read.csv("data/fish_raw/BC/QCS_effort.csv")
  haul_vi <- read.csv("data/fish_raw/BC/WCVI_effort.csv")
  haul_hs <- read.csv("data/fish_raw/BC/HS_effort.csv")
  haul_hg <- read.csv("data/fish_raw/BC/WCHG_effort.csv")

  #Combine BC bio and haul data to combine together
  bio2 <- rbind(bio_hg, bio_hs, bio_qc, bio_vi)
  haul2 <- rbind(haul_hg, haul_hs, haul_qc, haul_vi)

  #Merge the official BC bio data and the official BC haul data to get metadata (from here: https://open.canada.ca/data/en/dataset/86af7918-c2ab-4f1a-ba83-94c9cebb0e6c)
  bio2$Set.number <- bio2$Tow.number
  bio <- dplyr::left_join(bio2, haul2, relationship="many-to-many")

  names(bio) = tolower(names(bio))
  names(haul) = tolower(names(haul))

  #Put bio data in the same format as the NOAA bio data
  bio$scientific_name <- tolower(bio$scientific.name)
  bio$common_name <- tolower(bio$english.common.name)
  bio$weight <- bio$weight..g.*0.001
  bio$length_cm <- bio$fork.length..mm.*0.1
  bio$length_cm <- ifelse(is.na(bio$length_cm), (bio$total.length..mm.*0.1), bio$length_cm)

  #Make column names consistent so can join to surveyjoin haul data to get event_id
  haul$year <- as.character(substr(haul$date, start=1, stop=4))
  bio$year <- as.character(bio$survey.year)
  haul$set.date <- substr(haul$date, start=1, stop=10)
  bio$lat_start <- bio$start.latitude
  bio$lat_end <- bio$end.latitude
  bio$lon_start <- bio$start.longitude
  bio$lon_end <- bio$end.longitude

  #Combine BC bio data and haul metadata with surveyjoin metadata
  bio3 <- dplyr::left_join(bio[,c("age", "sex", "length_cm", "weight", "scientific_name", "common_name", "lat_start", "lon_start", "lat_end", "lon_end", "set.date")], haul, relationship="many-to-many")
  bio <- bio3

  #Clean catch data
  names(catch) = tolower(names(catch))

  catch <- dplyr::left_join(catch,itis)

  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)

  #Combine catch data with haul data
  dat_bc <- dplyr::left_join(catch, haul, relationship = "many-to-many")
  #Combine bio/haul data with catch data
  #Only positive catches
  dat_bc <- subset(dat_bc, catch_weight>0)
  dat_bc$cpue_kg_km2 <- dat_bc$catch_weight
  #Biomass
  if(type=="hauls"){
    counts_bc <- aggregate(cpue_kg_km2~scientific_name, dat_bc, FUN=length)
  }
  if(type=="biomass"){
    counts_bc <- aggregate(cpue_kg_km2~scientific_name, dat_bc, FUN=sum)
  }

  #Add common name
  counts_bc <- unique(dplyr::left_join(counts_bc, bio[,c("scientific_name", "common_name")]))
  counts_bc <- dplyr::mutate(counts_bc, bc=cpue_kg_km2)
  counts_bc$cpue_kg_km2 <- NULL
  rm(list=setdiff(ls(), "counts_bc"))

  #NWFSC
  # load, clean, and join data
  bio <- readRDS("data/fish_raw/NOAA/nwfsc_bio.rds")
  load("data/fish_raw/NOAA/nwfsc_haul.rda")
  haul <- nwfsc_haul
  catch <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
  names(catch) = tolower(names(catch))
  names(bio) = tolower(names(bio))
  names(haul) = tolower(names(haul))
  bio$scientific_name <- tolower(bio$scientific_name)
  bio$common_name <- tolower(bio$common_name)
  catch$common_name <- tolower(catch$common_name)

  bio$trawl_id = as.character(bio$trawl_id)
  haul$trawl_id = as.character(haul$event_id)
  catch$trawl_id=as.character(catch$trawl_id)
  haul$year <- as.character(substr(haul$date, start=1, stop=4))
  bio$year <- as.character(bio$year)
  catch$date <- NULL
  bio$date <- NULL
  bio$year <- NULL

  #haul$sampling_end_hhmmss = as.numeric(haul$sampling_end_hhmmss)
  #haul$sampling_start_hhmmss = as.numeric(haul$sampling_start_hhmmss)

  #Combine data
  dat = dplyr::left_join(catch[,c("trawl_id","common_name", "subsample_count","area_swept_ha","longitude_dd", "latitude_dd",
                                  "subsample_wt_kg","total_catch_numbers","total_catch_wt_kg","cpue_kg_km2")], haul, relationship = "many-to-many") %>%
    dplyr::left_join(filter(bio[,c("trawl_id", "scientific_name", "common_name", "weight", "ageing_lab", "oto_id", "length_cm", "width_cm", "sex", "age")], !is.na(length_cm)), relationship = "many-to-many") %>%
    filter(performance == "Satisfactory")

  #Only positive catches
  dat_nw <- subset(dat, cpue_kg_km2>0)
  #Biomass
  if(type=="hauls"){
    counts_nw <- aggregate(cpue_kg_km2~scientific_name, dat_nw, FUN=length)
  }
  if(type=="biomass"){
    counts_nw <- aggregate(cpue_kg_km2~scientific_name, dat_nw, FUN=sum)
  }
  counts_nw <- unique(left_join(counts_nw, bio[,c("scientific_name", "common_name")]))
  counts_nw <- dplyr::mutate(counts_nw, nw=cpue_kg_km2)
  counts_nw$cpue_kg_km2 <- NULL

  rm(list=setdiff(ls(), c("counts_bc", "counts_nw")))

  ##Alaska
  bio2 <-readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_all_levels.RDS")
  catch2 <- readRDS("data/fish_raw/NOAA/ak_bts_goa_ebs_nbs_cpue_zerofilled.RDS")

  #Isolate necessary parts of full data to get specimen weights/lengths per haul
  haul <- bio2$haul
  specimen <- bio2$specimen
  species <- bio2$species
  size <- bio2$size

  #make lowercase
  names(haul) <- tolower(names(haul))
  names(specimen) <- tolower(names(specimen))
  names(species) <- tolower(names(species))
  names(size) <- tolower(names(bio2$size))

  species$species_name <- tolower(species$species_name)

  #Combine catch data with species data
  names(catch2) <- tolower(names(catch2))
  catch <- dplyr::left_join(catch2, species)

  #Select only necessary columns for joining
  catch4 <- catch[,c("hauljoin", "survey", "year", "depth_m", "latitude_dd_start", "longitude_dd_start", "cpue_kgkm2", "species_name", "common_name")]
  dat <- dplyr::filter(catch4, year>1998)
  #bio4 <- dplyr::filter(bio4, year>1998)

  #Combine data
  dat <- dplyr::mutate(dat, trawl_id=hauljoin)
  #According to the codebook https://repository.library.noaa.gov/view/noaa/50147, 0 means Good performance, and the other numbers are for "Satisfactory, and then a "but"..."; negative numbers are Unsatisfactory
  #Dataset already includes only Good and Satisfactory hauls, Unsatisfactory are removed

  #Only positive catches
  dat$cpue_kg_km2 <- dat$cpue_kgkm2
  dat_ak <- subset(dat, cpue_kg_km2>0)
  dat_ak$scientific_name <- dat_ak$species_name
  #Biomass
  if(type=="hauls"){
    counts_ak <- aggregate(cpue_kg_km2~scientific_name, dat_ak, FUN=length)
  }
  if(type=="biomass"){
    counts_ak <- aggregate(cpue_kg_km2~scientific_name, dat_ak, FUN=sum)
  }
  counts_ak <- dplyr::mutate(counts_ak, ak=cpue_kg_km2)
  counts_ak$cpue_kg_km2 <- NULL
  counts_ak <- dplyr::left_join(counts_ak, species[,c("common_name", "species_name")], by=c("scientific_name"="species_name"))
  counts_ak$common_name <- tolower(counts_ak$common_name)

  #counts_ak <- unique(left_join(counts_nw, bio4[,c("scientific_name", "common_name")]))

  #Combine
  counts <- full_join(counts_nw, counts_bc)
  counts <- full_join(counts, counts_ak)
  return(counts)
}

IPHC <- function (catch, adjustment) {

  #Calculate CPUE as U32 and O32 count and weight divided by effective skates hauled
  catch$cpue_O32_count <- catch$`O32 Pacific halibut count`/catch$`Effective skates hauled`
  catch$cpue_U32_count <- catch$`U32 Pacific halibut count`/catch$`Effective skates hauled`
  catch$cpue_O32_weight <- catch$`O32 Pacific halibut weight`/catch$`Effective skates hauled`
  catch$cpue_U32_weight <- catch$`U32 Pacific halibut weight`/catch$`Effective skates hauled`

  #make lowercase
  colnames(catch) <- tolower(colnames(catch))
  colnames(adjustment) <- tolower(colnames(adjustment))

  #join
  adjustment$stlkey <- as.character(adjustment$stlkey)
  data <- left_join(catch, adjustment, by="stlkey")
  data$h.adj <- as.numeric(data$h.adj)

  #Calculate ajustment factor
  data$cpue_o32_count <- data$cpue_o32_count * data$h.adj
  data$cpue_u32_count <- data$cpue_u32_count * data$h.adj
  data$cpue_o32_weight <- data$cpue_o32_weight * data$h.adj
  data$cpue_u32_weight <- data$cpue_u32_weight * data$h.adj

  #Extract columns of interest
  dat <- data[,c("year.x", "date.x", "midlat", "midlon", "avgdepth (fm)", "temp c", "salinity psu", "oxygen_ml",  "cpue_o32_weight", "cpue_u32_weight")]

  #Re-name columns to match the NOAA and BC data
  colnames(dat) <- c("year", "date", "latitude", "longitude", "depth", "temperature_C", "salinity_psu", "do_mlpL", "cpue_o32_weight", "cpue_u32_weight")

  #Add columns to match the NOAA and BC data
  dat$project <- "iphc"
  dat$survey <- "iphc"
  dat$common_name <- "pacific halibut"
  dat$scientific_name <- "hippoglossus stenolepis"

  #Date and month in right format
  dat$month <- case_when(grepl("May",dat$date) ~5,
                         grepl("Jun",dat$date)  ~6,
                         grepl("Jul",dat$date)  ~7,
                         grepl("Aug",dat$date)  ~8,
                         grepl("Sep",dat$date)  ~9,
                         grepl("Oct",dat$date)  ~10)
  dat$day <- as.numeric(substr(dat$date, 1,2))
  dat$date <-  as.POSIXct(as.Date(with(dat,paste(year,month,day,sep="-")),"%Y-%m-%d"))
  dat$doy <- as.POSIXlt(dat$date, format = "%Y-%b-%d")$yday
  dat$year <- as.numeric(dat$year)
  dat$day <- NULL
  dat$depth <- dat$depth*1.8288


  #convert oxygen mg/L to umol_kg
  SA = gsw_SA_from_SP(dat$salinity_psu,dat$depth,dat$longitude,dat$latitude) #absolute salinity for pot T calc
  pt = gsw_pt_from_t(SA,dat$temperature_C,dat$depth) #potential temp at a particular depth
  CT = gsw_CT_from_t(SA,dat$temperature_C,dat$depth) #conservative temp
  dat$sigma0_kgm3 = gsw_sigma0(SA,CT)
  dat$O2_umolkg = dat$do_mlpL*44660/(dat$sigma0_kgm3+1000)

  #Convert coordinates
  dat <- subset(dat, !is.na(latitude))
  dat <- dat %>%
    st_as_sf(coords=c('longitude','latitude'),crs=4326,remove = F) %>%
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
    mutate(X=st_coordinates(.)[,1],Y=st_coordinates(.)[,2])

  return(dat)
}

calc_po2_sat <- function(salinity, temp, depth, oxygen, lat, long, umol_m3, ml_L) {
  # Input:       S = Salinity (pss-78)
  #              T = Temp (deg C) ! use potential temp
  #depth is in meters

  #Pena et al. ROMS and GLORYS are in mmol per m^3 (needs to be converted to umol per kg) (o2 from trawl data was in mL/L, so had to do extra conversions)
  gas_const = 8.31
  partial_molar_vol = 0.000032
  kelvin = 273.15
  boltz = 0.000086173324

  #lol this was dumb because it is actually equivalent to umol/kg
  #convert mmol to umol
  umol_m3 <- oxygen*1000
  #convert m3 to l
  umol_l <- umol_m3/1000
  #convert from molality (moles per volume) to molarity (moles per mass)
  #1 L of water = 1 kg of water, so no equation needed?? Right??
  o2_umolkg <- umol_l * 1/1

  SA = gsw_SA_from_SP(salinity,depth,long,lat) #absolute salinity for pot T calc
  pt = gsw_pt_from_t(SA,temp,depth) #potential temp at a particular depth
  #this is for if using data that has oxygen in ml/L
  #CT = gsw_CT_from_t(SA,temp,depth) #conservative temp
  #sigma0 = gsw_sigma0(SA,CT)
  # o2_umolkg = oxygen*44660/(sigma0+1000)

  O2_Sat0 = gsw_O2sol_SP_pt(salinity,pt)

  #= o2satv2a(sal,pt) #uses practical salinity and potential temp - solubity at p =1 atm
  press = exp(depth*10000*partial_molar_vol/gas_const/(temp+kelvin))
  O2_satdepth = O2_Sat0*press

  #solubility at p=0
  sol0 = O2_Sat0/0.209
  sol_Dep = sol0*press
  po2 = o2_umolkg/sol_Dep
  po2 <- po2 * 101.325 # convert to kPa
  return(po2)

}

calc_mi <- function(Eo, Ao, W, n,po2, inv.temp) {
  mi = W^n*Ao*po2 *exp(Eo * inv.temp)
  return(mi)
}

# calc o2 solubility, relies on o2 in umol/kg
gsw_O2sol_SP_pt <- function(sal,pt) {
  x = sal
  pt68 = pt*1.00024
  y = log((298.15 - pt68)/(273.15 + pt68))

  a0 =  5.80871
  a1 =  3.20291
  a2 =  4.17887
  a3 =  5.10006
  a4 = -9.86643e-2
  a5 =  3.80369
  b0 = -7.01577e-3
  b1 = -7.70028e-3
  b2 = -1.13864e-2
  b3 = -9.51519e-3
  c0 = -2.75915e-7

  O2sol = exp(a0 + y*(a1 + y*(a2 + y*(a3 + y*(a4 + a5*y)))) + x*(b0 + y*(b1 + y*(b2 + b3*y)) + c0*x))
  return(O2sol)
}

prepare_data <- function(spc,sci_name,mi){
  dat.by.size <- try(length_expand_bc(sci_name))
  gc()
  if(is.data.frame(dat.by.size)){
    dat3 <- try(load_data_bc(sci_name = sci_name, spc=spc, dat.by.size = dat.by.size, length=F))
    dat3 <- try(unique(dat3))
  }
  gc()
  rm(dat.by.size)

  dat.by.size <- try(length_expand_nwfsc(spc=spc, sci_name=sci_name))
  gc()
  if(is.data.frame(dat.by.size)){
    dat2 <- try(load_data_nwfsc(spc= spc, sci_name=sci_name, dat.by.size = dat.by.size, length=F))
    dat2 <- try(unique(dat2))
  }

  if(!is.data.frame(dat.by.size)){
    catch <- readRDS("data/fish_raw/NOAA/nwfsc_catch.rds")
    names(catch) <- tolower(names(catch))
    catch$common_name <- tolower(catch$common_name)
    catch <- dplyr::filter(catch, common_name == spc)
    if(length(catch)>0){
      dat.by.size <- NA
      dat2 <- try(load_data_nwfsc(spc= spc, sci_name=sci_name, dat.by.size = dat.by.size, length=F))
      dat2 <- try(unique(dat2))
    }
  }

  gc()
  rm(dat.by.size)

  dat.by.size <- try(length_expand_afsc(sci_name))
  gc()
  if(is.data.frame(dat.by.size)){
    dat5 <- try(load_data_afsc(sci_name = sci_name, spc=spc, dat.by.size = dat.by.size, length=F))
    dat5 <- try(unique(dat5))
  }

  gc()

  #All regions present
  if(exists("dat3") & exists("dat2") & exists("dat5")){
    dat4 <- bind_rows(dat3, dat2, dat5)
  }
  #Only BC
  if(exists("dat3") & !exists("dat2") & !exists("dat5")){
    dat4 <- dat3
  }
  #Only AK
  if(!exists("dat3") & !exists("dat2") & exists("dat5")){
    dat4 <- dat5
  }

  #Only NWFSC
  if(!exists("dat3") & exists("dat2") & !exists("dat5")){
    dat4 <- dat2

  }
  #BC & NW
  if(exists("dat3") & exists("dat2") & !exists("dat5")){
    dat4 <- bind_rows(dat3, dat2)
  }
  #BC & AK
  if(exists("dat3") & !exists("dat2")& exists("dat5")){
    dat4 <- bind_rows(dat3, dat5)
  }
  #AK & NW
  if(!exists("dat3") & exists("dat2")& exists("dat5")){
    dat4 <- bind_rows(dat2, dat5)
  }

  if(spc=="pacific halibut"){
    catch <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/Set and Pacific halibut data.xlsx")
    adjustment <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/iphc-2023-fiss-hadj-20231031.xlsx")
    dat_IPHC <- IPHC(catch, adjustment)
    dat4 <- bind_rows(dat4, dat_IPHC)
  }

    dat <- dat4

  ###Add in situ data####
    #Isolate just NWFSC and EBS/NBS/GOA data for joining, because iphc and dfo are already in the dataset
    insitu <- readRDS("data/processed_data/insitu_combined.rds")
    insitu <- filter(insitu, survey!="iphc")
    insitu <- filter(insitu, survey!="dfo")
    #Northwest
    dat6 <- filter(dat4, survey=="nwfsc")
    #Just columns of interest
    dat6 <- left_join(dat6[,c("event_id", "date", "year", "project","survey", "latitude", "longitude", "depth", "X", "Y", "cpue_kg_km2", "julian_day", "nlength", "median_weight", "haul_weight", "pass", "p1", "p2", "p3", "p4", "common_name", "scientific_name", "depth", "vessel", "tow", "bottom_temperature_c")], insitu, by=c("date", "latitude", "longitude"))
    #Edit columns
    dat6$event_id <- dat6$event_id.x
    dat6$event_id.x <- NULL
    dat6$event_id.y <- NULL
    dat6$year <- dat6$year.x
    dat6$year.y <- NULL
    dat6$year.x <- NULL
    dat6$depth <- dat6$depth.x
    dat6$depth.x <- NULL
    dat6$depth.y <- NULL
    dat6$X <- dat6$X.x
    dat6$Y <- dat6$Y.x
    dat6$X.x <- NULL
    dat6$X.y <- NULL
    dat6$Y.x <- NULL
    dat6$Y.y <- NULL
    dat6$depth.1 <- NULL
    dat6$month <- NULL
    dat6$doy <- NULL
    dat6$survey <- dat6$survey.x
    dat6$survey.x <- NULL
    dat6$survey.y <- NULL

    dat7 <- dat6

    #GOA, EBS & NBS
    dat6 <- filter(dat4, survey=="EBS"|survey=="NBS"|survey=="GOA")
    insitu$event_id <- as.character(insitu$hauljoin)
    dat6 <- left_join(dat6[,c("event_id", "date", "year", "project","survey", "latitude", "longitude", "depth", "X", "Y", "cpue_kg_km2", "julian_day", "nlength", "median_weight", "haul_weight", "pass", "p1", "p2", "p3", "p4", "common_name", "scientific_name", "depth", "vessel", "tow", "bottom_temperature_c")], insitu[,c("event_id", "temperature_C", "do_mlpL", "salinity_psu", "sigma0_kgm3", "O2_umolkg")], by=c("event_id"))

    #Edit columns
    dat6$depth.1 <- NULL

    #Recombine back with DFO data
    if(exists("dat3")){
      dat <- bind_rows(dat6, dat7,dat3)
    }

    if(!exists("dat3")){
      dat <- bind_rows(dat6, dat7)
    }

    dat$doy <- dat$julian_day
    dat$julian_day <- NULL
    dat$month <- month(dat$date)

    #Add IPHC data
    if(spc=="pacific halibut"){
      catch <-  read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/Set and Pacific halibut data.xlsx")
      adjustment <- read_excel("~/Dropbox/choke species/code/choke-species-data/data/fish_raw/IPHC/iphc-2023-fiss-hadj-20231031.xlsx")
      dat_IPHC <- IPHC(catch, adjustment)
      dat <- bind_rows(dat, dat_IPHC)
    }

    #Some other columns
    dat$cpue_kg_km2_sub <- dat$cpue_kg_km2 * (dat$p2+dat$p3)
    dat$cpue_kg_km2_sub <- ifelse(dat$cpue_kg_km2==0, 0, dat$cpue_kg_km2_sub)
    dat$p1 <- ifelse(dat$cpue_kg_km2==0, 0, dat$p1)
    dat$p2 <- ifelse(dat$cpue_kg_km2==0, 0, dat$p2)
    dat$p3 <- ifelse(dat$cpue_kg_km2==0, 0, dat$p3)
    dat$p4 <- ifelse(dat$cpue_kg_km2==0, 0, dat$p4)
    dat$log_depth_scaled <- scale(log(dat$depth))
    dat$log_depth_scaled2 <- with(dat, log_depth_scaled ^ 2)

    if(mi==T){
    ##Calculate metabolic index
    #Calculate pO2 from umol kg
    dat$po2 <- calc_po2_sat(salinity=dat$salinity_psu, temp=dat$temperature_C, depth=dat$depth, oxygen=dat$O2_umolkg, lat=dat$latitude, long=dat$longitude, umol_m3=T, ml_L=F)

    ##Calculate inverse temp
    kelvin = 273.15
    boltz = 0.000086173324
    tref <- 12
    dat$invtemp <- (1 / boltz)  * ( 1 / (dat$temperature_C + 273.15) - 1 / (tref + 273.15))

    ##Calculate Metabolic index
    #Pull parameters ((pulled in code file 2a))
    MI_pars <- readRDS("metabolic_index-main/MI_pars.rds")
    Eo1 <-  MI_pars[1,3] #Median
    Eo2 <-  Eo1-(MI_pars[2,3]*1.28)    #low 90th percentile, close to zero
    Eo3 <-  Eo1+(MI_pars[2,3]*1.28)    #High 90th percentile, close to zero
    Ao <- 1/exp(MI_pars[1,1])
    n <- MI_pars[1,2]
    #Average weight across all hauls--in case want to make universal
    dat$mean_weight <- mean(dat$median_weight, na.rm=T)

    #Calculate metabolic index with each Eo
    dat$mi1 <- calc_mi(Eo1, Ao, dat$mean_weight,  n, dat$po2, dat$invtemp)
    dat$mi2 <- calc_mi(Eo2, Ao, dat$mean_weight, n, dat$po2, dat$invtemp)
    dat$mi3 <- calc_mi(Eo3, Ao, dat$mean_weight, n, dat$po2, dat$invtemp)

    #For average body size from each haul
    dat$mi5 <- calc_mi(Eo1, Ao, dat$haul_weight,  n, dat$po2, dat$invtemp)
    dat$po2_s <- (scale(dat$po2))
    dat$mi1_s <-(scale(dat$mi1))
    dat$mi2_s <-(scale(dat$mi2))
    dat$mi3_s <-(scale(dat$mi3))
    dat$mi5_s <- scale(dat$mi5)
    }

    #Reorder columns
    if(spc!="pacific halibut"){
      dat <- relocate(dat, scientific_name, common_name, project, survey, year, date, doy, month, depth, longitude, latitude, cpue_kg_km2, cpue_kg_km2_sub, salinity_psu, temperature_C, sigma0_kgm3,do_mlpL, O2_umolkg, log_depth_scaled, log_depth_scaled2, X, Y, p1,p2,p3,p4,median_weight, haul_weight, nlength, pass, vessel, tow, bottom_temperature_c)
    }
    if(spc=="pacific halibut"){
      dat <- relocate(dat, scientific_name, common_name, project, survey, year, date, doy, month, depth, longitude, latitude, cpue_kg_km2, cpue_kg_km2_sub,cpue_o32_weight, cpue_u32_weight, salinity_psu, temperature_C, sigma0_kgm3, do_mlpL, O2_umolkg, log_depth_scaled, log_depth_scaled2, X, Y, p1,p2,p3,p4,median_weight, mean_weight, haul_weight, nlength, pass, vessel, tow, bottom_temperature_c)

    }

  try(return(dat))
}

plot_marginal2 <- function(output, model){
  pred <- output[["predictions"]][["Synoptic"]][[print(model)]]
  pred2 <- output[["predictions"]][["Predicted"]][[print(model)]]
  pred3 <- output[["predictions"]][["GOBH"]][[print(model)]]
  dat <- output[["data"]][["Synoptic"]]
  pred$center <- attr(dat$o2_s, "scaled:center")
  pred$scale <- attr(dat$o2_s, "scaled:scale")
  dat <- output[["data"]][["Predicted"]]
  pred2$center <- attr(dat$o2_s, "scaled:center")
  pred2$scale <- attr(dat$o2_s, "scaled:scale")
  dat <- output[["data"]][["GOBH"]]
  pred3$center <- attr(dat$o2_s, "scaled:center")
  pred3$scale <- attr(dat$o2_s, "scaled:scale")
  preds <- bind_rows(pred, pred2, pred3)
  preds$unscaled <-  preds$o2_s* preds$scale+preds$center
  plot <- ggplot(preds, aes(unscaled, y=exp(est)))+
    geom_line(aes(colour=data))+
    geom_ribbon(aes(ymin = (exp(est) - exp(est_se)), ymax = (exp(est) + exp(est_se)), fill=data), alpha=0.4)+
    #scale_y_continuous(limits = c(100, 400), expand = expansion(mult = c(0, 0.0))) +
    xlim(0,200)+
    labs(x = bquote('Oxygen'~mu~"mol"~(kg^-1)), y = bquote('Population Density'~(kg~km^-2)))+
    theme_minimal()+
    theme(text=element_text(size=15))
  return(plot)
}

plot_marginal3 <- function(outputs, model){
  toplot <- list()
  for (i in 1:length(outputs)){
  output <- outputs[[i]]
  pred <- output[["predictions"]][["Synoptic"]][[print(model)]]
  pred2 <- output[["predictions"]][["Predicted"]][[print(model)]]
  pred3 <- output[["predictions"]][["GOBH"]][[print(model)]]
  dat <- output[["data"]][["Synoptic"]]
  pred$center <- attr(dat$o2_s, "scaled:center")
  pred$scale <- attr(dat$o2_s, "scaled:scale")
  dat <- output[["data"]][["Predicted"]]
  pred2$center <- attr(dat$o2_s, "scaled:center")
  pred2$scale <- attr(dat$o2_s, "scaled:scale")
  dat <- output[["data"]][["GOBH"]]
  pred3$center <- attr(dat$o2_s, "scaled:center")
  pred3$scale <- attr(dat$o2_s, "scaled:scale")
  preds <- bind_rows(pred, pred2, pred3)
  preds$unscaled <-  preds$o2_s* preds$scale+preds$center
  preds$region <- paste(region[i])
  preds$species <- paste(species[i])
  toplot[[i]] <- preds
  }
  preds <- bind_rows(toplot)
  preds$species <- factor(preds$species, levels=c("Sablefish", "Dover sole"))
  preds$region <- factor(preds$region, levels=c("California Current", "British Columbia"))

  preds[preds == "NaN"] <- NA


  preds <-  preds %>% group_by(species,region)%>%
    mutate(est_sc= ifelse((species=="Dover sole" & region=="California Current"), (exp(est)/max(exp(est))), (exp(est)/max((exp(est)+exp(est_se)), na.rm=T))))%>%
    ungroup()
  preds <- preds$est_se_sc <- preds %>% group_by(species,region)%>%
    mutate(est_se_sc1= ifelse((species=="Dover sole" & region=="California Current"), (exp(est)/max(exp(est))), ((exp(est)-exp(est_se))/max((exp(est)+exp(est_se)), na.rm=T))))%>%
    ungroup()
  preds <- preds$est_se_sc <- preds %>% group_by(species,region)%>%
    mutate(est_se_sc2= ifelse((species=="Dover sole" & region=="California Current"), (exp(est)/max(exp(est))), ((exp(est)+exp(est_se))/max((exp(est)+exp(est_se)), na.rm=T))))%>%
    ungroup()
  preds$est_sc <- ifelse((preds$region=="California Current"& preds$species=="Dover sole"& preds$data=="GOBH"), (preds$est_sc-0.01), preds$est_sc)

  preds$data<- sub('Predicted','Empirical \n Statistical',preds$data)
  preds$data<- sub('Synoptic','Concurrent',preds$data)
  preds$data<- sub('GOBH','Oceanographic',preds$data)

  #plot <- ggplot(preds, aes(unscaled, y=exp(est)))+
  plot <- ggplot(preds, aes(unscaled, y=est_sc))+
    facet_wrap(region~species)+
    geom_ribbon(aes(ymin = est_se_sc1, ymax = est_se_sc2, fill=data), alpha=0.4)+
    geom_line(aes(colour=data))+
    #geom_ribbon(aes(ymin = (exp(est)-exp(est_se)), ymax = (exp(est) + exp(est_se)), fill=data), alpha=0.4)+
   scale_y_continuous(limits=c(0,1), expand = expansion(mult = c(0, 0.0))) +
    xlim(0,200)+
    labs(x = bquote('Oxygen'~mu~"mol"~kg^-1), y = bquote('Population Density (Scaled)'))+
    theme_minimal()+
    theme(legend.position="top")+
  theme(text=element_text(size=15))

  return(plot)
}


plot_marginal <- function(output, data_type, model){
  pred <- output[["predictions"]][[print(data_type)]][[print(model)]]
  dat <- output[["data"]][[print(data_type)]]
  center <- attr(dat$o2_s, "scaled:center")
  scale <- attr(dat$o2_s, "scaled:scale")
  m <- output[["models"]][[print(data_type)]][[print(model)]]
  plot <- ggplot(pred, aes(back.convert(pred$o2_s, center, scale), y=exp(est)))+
    geom_line()+
    geom_ribbon(pred, mapping=aes(ymin = (exp(est) - exp(est_se)), ymax = (exp(est) + exp(est_se))), alpha=0.4)+
    #scale_y_continuous(limits = c(100, 400), expand = expansion(mult = c(0, 0.0))) +
    xlim(0,200)+
    labs(x = bquote('Oxygen'~mu~"mol"~(kg^-1)), y = bquote('Population Density'~(kg~km^-2)))+
    theme_minimal()+
    theme(text=element_text(size=15))
  return(plot)
}

par_estimates <- function(output, region, species){
  pars <- output[["parameter_estimates"]]
  pars <- filter(pars, (model=="breakpt(o2)")& (term=="o2_s-slope"|term=="o2_s-breakpt"))
  pars$region <- region
  pars$species <- species
  return(pars)
}

par_table <- function(output, species, region){
  pars <- output[["parameter_estimates"]]
  pars <- filter(pars, (model=="breakpt(o2)")& (term=="o2_s-slope"|term=="o2_s-breakpt"))
  dat1 <- output[["data"]][["Synoptic"]]
  dat2 <- output[["data"]][["Predicted"]]
  dat3 <- output[["data"]][["GOBH"]]
  pars$center <- case_when(pars$data=="Synoptic"~attr(dat1$o2_s, "scaled:center"),
                           pars$data=="Predicted"~attr(dat2$o2_s, "scaled:center"),
                           pars$data=="GOBH"~attr(dat3$o2_s, "scaled:center"))
  pars$scale <- case_when(pars$data=="Synoptic"~attr(dat1$o2_s, "scaled:scale"),
                          pars$data=="Predicted"~attr(dat2$o2_s, "scaled:scale"),
                          pars$data=="GOBH"~attr(dat3$o2_s, "scaled:scale"))
  #Un-scale
  pars$estimate2 <-  case_when(pars$term=="o2_s-breakpt"~round(pars$estimate* pars$scale+pars$center, 4),
                              pars$term!="o2_s-breakpt"~ exp(pars$estimate))
  pars$std.error2 <-  case_when(pars$term=="o2_s-breakpt"~round(pars$std.error* pars$scale+pars$center, 4),
                              pars$term!="o2_s-breakpt"~exp(pars$std.error))
  #Columns needed
 # pars <- select(pars, data, term,estimate,std.error)
  #Combine into one
#  pars$est <- paste(pars$estimate,"+-",pars$std.error)
  #Columns needed
  #Wide
  #pars <- pivot_wider(pars,names_from=data, values_from=est)
  #write.csv(pars, file=paste("outputs/table_pars", species, region, sep="_"), row.names=F)
  pars <- as.data.frame(pars)
 # pars$estimate <- format(pars$estimate, scientific=FALSE)
  #pars$std.error <- format(pars$std.error, scientific=FALSE)
  pars$estimate2 <- round(pars$estimate2, 3)
  pars$std.error2 <- round(pars$std.error2, 3)
  return(pars)
}

plot_dat <- function(output, legend){
  #Add data type column and scale by geometric mean
  dat1 <- output[["data"]][[1]]
  dat1$type <- "Concurrent"

  dat2 <- output[["data"]][[2]]
  dat2$type <- "Empirical \n Statistical"

  dat3 <- output[["data"]][[3]]
  dat3$type <- "Oceanographic"


  all_dat <- bind_rows(dat1, dat2,dat3)

  mean <- exp(mean(log(all_dat$cpue_kg_km2+1)))
  se <- exp(sd(log(all_dat$cpue_kg_km2+1)))
  all_dat$cpue_s <- (log(all_dat$cpue_kg_km2+1)-mean)/se

  plot <-ggplot(all_dat, aes(x=o2, y=-depth))+geom_point(aes(colour=cpue_s))+
    facet_wrap("type")+
    theme_minimal()+
    theme(text=element_text(size=15))+
    xlab("")+
    ylab("")+
    scale_colour_viridis_c(name=bquote("Scaled log(CPUE)"),
    breaks = c(-2.4, -2.2, -2))
     # name=bquote('log(CPUE'~"kg"~km^-2~")"))

return(plot)

}
plot_dat2 <- function(outputs){
  dats <- list()
  regions <- c("California Current", "British Columbia", "California Current", "British Columbia")
  for(i in 1:length(outputs)){
    output <- outputs[[i]]
    dat1 <- output[["data"]][[1]]
    dat1$type <- "Synoptic"
    dat2 <- output[["data"]][[2]]
    dat2$type <- "Integrated \n Prediction"
    dat3 <- output[["data"]][[3]]
    dat3$type <- "GOBH"
    all_dat <- bind_rows(dat1, dat2,dat3)
    all_dat$region <- regions[i]
    dats[[i]] <- all_dat
  }
  dats <- bind_rows(dats)
  labs <- c("Sablefish", "Dover Sole")
  names(labs) <- c("sablefish", "dover sole")
  dats$common_name <- factor(dats$common_name, levels=c("sablefish", "dover sole"))
  dats$region <- factor(dats$region, levels=c("California Current", "British Columbia"))


  plot <- ggplot(dats, aes(x=o2, y=-depth))+geom_point(aes(colour=log(cpue_kg_km2+1)))+
    facet_wrap(common_name~region+type, labeller = labeller(common_name=labs), nrow=4)+
    theme_minimal()+
    theme(text=element_text(size=15))+
    labs(x=bquote('Dissolved Bottom Oxygen ('~mu~"mol"~kg^-1~")"))+
    ylab("Depth (m)")+
    scale_colour_viridis_c(
      limits = c(0, 10),
      oob = scales::squish,
      breaks = c(0, 5,10),
      name=bquote('log(CPUE'~"kg"~km^-2~")"))
  return(plot)
}

par_table_raw <- function(output, species, region){
  pars <- output[["parameter_estimates"]]
  pars <- filter(pars, (model=="breakpt(o2)")& (term=="o2_s-slope"|term=="o2_s-breakpt"))
  dat1 <- output[["data"]][["Synoptic"]]
  dat2 <- output[["data"]][["Predicted"]]
  dat3 <- output[["data"]][["GOBH"]]
  pars$center <- case_when(pars$data=="Synoptic"~attr(dat1$o2_s, "scaled:center"),
                           pars$data=="Predicted"~attr(dat2$o2_s, "scaled:center"),
                           pars$data=="GOBH"~attr(dat3$o2_s, "scaled:center"))
  pars$scale <- case_when(pars$data=="Synoptic"~attr(dat1$o2_s, "scaled:scale"),
                          pars$data=="Predicted"~attr(dat2$o2_s, "scaled:scale"),
                          pars$data=="GOBH"~attr(dat3$o2_s, "scaled:scale"))
  #Un-scale
  pars$estimate <-  round(pars$estimate* pars$scale+pars$center, 2)
  pars$std.error <-  round(pars$std.error* pars$scale+pars$center, 2)
  #Columns needed
 # pars <- select(pars, data, term,estimate, std.error)
  #Wide
#  pars <- pivot_wider(pars,names_from=data, values_from=est)
 # write.csv(pars, file=paste("outputs/table_pars", species, region, sep="_"), row.names=F)
  pars <- as.data.frame(pars)
  return(pars)
}

load_fish <- function(species, test_region){
  file <- list.files("data/processed_data", pattern=paste(species))
  file <- paste("data/processed_data/", file, sep="")
  dat <- readRDS(file)

  #Filter region
  if(test_region=="cc"){
    test_survey <- "nwfsc"
  }
  if(test_region=="bc"){
    test_survey <- "dfo"
  }
  dat <- as.data.frame(dat)
  #Filter to region
  dat <- filter(dat, survey==test_survey)
  return(dat)
}

###Run the oxygen models with the IPHC as test data, and no annual effects
fit_models2 <- function(dat, test_region, plot_title){
  ##Set up data
  #Filter to region
  dat.2.use <- as.data.frame(filter(dat, region==test_region))
  if(test_region=="goa") {dat.2.use <- filter(dat.2.use, !(survey=="ai"))}
  #Just trawl survey data
  trawl_dat <- dat.2.use %>%
    filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai"))
  other_dat <- dat.2.use %>%
    filter(!(survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai")))
  #Years in trawl data available
  yearlist <- sort(unique(trawl_dat$year))

  if(n_50){
    counts <- count(trawl_dat, year)
    counts <- filter(counts, n>50)
    yearlist <- sort(unique(counts$year))
  }

  #Remove NA if there
  yearlist <- yearlist[!is.na(yearlist)]


  #Create lists and matrices for storing RMSE and list for storing prediction datasets
  models <- c("persistent", "persistent_spatiotemporal", "null_covariates", "null_temponly", "persistent_temponly", "persistent_covariates",  "spatiotemporal_temp","spatiotemporal_covariates", "null_null")
  models.2.use <- length(models)
  other.cols <- 2
  rmse_summary <- matrix(data=NA, nrow=length(yearlist), ncol=models.2.use + other.cols)
  colnames(rmse_summary) <- c("persistent", "persistent_spatiotemporal", "null_temponly","null_covariates", "persistent_temponly", "persistent_covariates", "spatiotemporal_temp","spatiotemporal_covariates","null_null", "n_test","n_train")
  output <- list()
  # rsqlist <- rmselist <- rep(NA, length(yearlist))
  ##Fit model for each year of training data
  for (i in 1:length(yearlist)) {
    #Separate test and training data
    test_year <- yearlist[i]
    print(test_year)
    test_data <- dat.2.use %>%
      filter(survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai", "iphc") & year==test_year)
    train_data <- dat.2.use %>%
      filter(!((survey %in% c("nwfsc", "dfo", "goa", "EBS", "ai", "iphc") & year==test_year)))
    train_data <- as.data.frame(train_data)
    test_data <- as.data.frame(test_data)
    #Determine if extra time is needed
    extra_years <- setdiff(yearlist, unique(train_data$year))
    #Are years continuous?
    train_years <- unique(train_data$year)
    train_years2 <- seq(from=min(train_years), to=max(train_years), by=1)
    extra_years2 <- setdiff(train_years2, train_years)
    extra_years <- append(extra_years, extra_years2)

    if(length(extra_years)==0) {extra_years = NULL}
    ## Make Mesh and fit model ####
    spde <- make_mesh(data = train_data,
                      xy_cols = c("X", "Y"),
                      cutoff = 45)
    ##No covariates
    #Persistent
    print("fitting m1")
    m1 <- try(sdmTMB(
      formula = o2  ~ 1+s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    #Persistent_spatiotemporal
    print("fitting m3")
    m3 <- try(sdmTMB(
      formula = o2  ~ 1+ s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      time = "year",
      spatial = "on",
      spatiotemporal  = "ar1",
      extra_time=c(extra_years)
    ))
    ##Covariates
    #Null--no random effects
    print("fitting m4")
    m4 <- try(sdmTMB(
      formula = o2  ~ 1+s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "off",
      spatiotemporal  = "off"
    ))
    print("fitting m5")
    m5 <- try(sdmTMB(
      formula = o2  ~ 1 + s(sigma0) + s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "off",
      spatiotemporal  = "off"
    ))
    #Covariates + persistent
    print("fitting m6")
    m6 <- try(sdmTMB(
      formula = o2  ~ 1+s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    print("fitting m7")
    m7 <- try(sdmTMB(
      formula = o2  ~ 1 + s(sigma0) + s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      spatiotemporal  = "off"
    ))
    #Covariates+spatiotemporal
    print("fitting m10")
    m10 <- try(sdmTMB(
      formula = o2  ~ 1+s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatial = "on",
      time="year",
      spatiotemporal  = "ar1",
      extra_time=c(extra_years)
    ))
    print("fitting m11")
    m11 <- try(sdmTMB(
      formula = o2  ~ 1 +s(sigma0) + s(temp) + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      time="year",
      spatiotemporal  = "ar1",
      extra_time=c(extra_years)
    ))
    print("fitting m12")
    m12 <-try(sdmTMB(
      formula = o2  ~ 1 + s(depth_ln) + s(doy),
      mesh = spde,
      data = train_data,
      family = gaussian(),
      spatiotemporal  = "off",
      spatial="off"
    ))
    models <- list(m1,m3, m4,m5, m6,m7,m10,m11, m12)
    names(models) <- models
    tmp.preds <- list()
    #Predict data from each model and calculate RMSE
    for (j in 1:length(models)){
      # Predict onto data
      test_predict_O2 <- try(predict(models[[j]], newdata = test_data))
      if(scale){
        test_predict_O2$est <- test_predict_O2$est*100
        test_predict_O2$o2 <- test_predict_O2$o2*100
        test_predict_O2$est2 <- ifelse(test_predict_O2$est<0, 0, test_predict_O2$est)
      }
      test_predict_O2$residual = try(test_predict_O2$o2 - test_predict_O2$est)
      test_predict_O2$residual2 = try(test_predict_O2$o2 - test_predict_O2$est2)
      rmse_summary[i,j] <- try(rmse(test_predict_O2$o2, test_predict_O2$est), silent=T)
      # rmse_summary2[i,j] <- try(rmse(test_predict_O2$o2, test_predict_O2$est2), silent=T)
      tmp.preds[[j]] <- test_predict_O2
      #Number of datapoints in each year for calculating overall RMSE late
      if(j==1){
        ncols <- ncol(rmse_summary)
        rmse_summary[i,ncols -1] <- nrow(test_data)
        rmse_summary[i,ncols] <- nrow(train_data)
      }
    }
    tmp.output <- list(train_data, test_data, tmp.preds, models)
    names(tmp.output) <-c("train_data", "test_data", "predictions", "models")
    names(tmp.preds) <- models
    output[[i]] <- tmp.output
  }
  #Plot
  if(plotmodel){
    print("plots")
    try(plot_simple(tmp.output, dat.2.use))
    if(is.list(models[5])){
      print("marginal effects")
      try(plot_marginal_effects(models, tmp.preds, dat.2.use, 4))
    }
  }


  #Clean RMSE table
  rmse_summary <- as.data.frame(rmse_summary)
  rmse_summary$year <- yearlist
  rmse_summary$region <- test_region
  # rmse_summary$persistent_spatial <- as.numeric(rmse_summary$persistent_spatial)
  # rmse_summary$persistent_spatial_year <- as.numeric(rmse_summary$persistent_spatial_year)
  # rmse_summary$year_temp_salinity <- as.numeric(rmse_summary$year_temp_salinity)
  # rmse_summary$temp_salinity_spatiotemporal <- as.numeric(rmse_summary$temp_salinity_spatiotemporal)
  #
  #  rmse_summary2 <- as.data.frame(rmse_summary2)
  #  rmse_summary2$year <- yearlist
  #  rmse_summary2$region <- test_region
  # rmse_summary2$persistent_spatial <- as.numeric(rmse_summary2$persistent_spatial)
  # rmse_summary2$persistent_spatial_year <- as.numeric(rmse_summary2$persistent_spatial_year)
  # rmse_summary2$year_temp_salinity <- as.numeric(rmse_summary2$year_temp_salinity)
  # rmse_summary2$temp_salinity_spatiotemporal <- as.numeric(rmse_summary2$temp_salinity_spatiotemporal)
  #
  ##Save models
  if (savemodel) {
    names(output) <- yearlist
    save(x = output, file = paste("outputs/o2_models_", test_region, ".Rdata", sep=""))
  }

  if(plotmodel){
    #Plot RMSE and save
    rmse_long <- pivot_longer(rmse_summary, 1:models.2.use, names_to="model")
    #Remove rows with less than n=50 in test data
    rmse_long <- filter(rmse_summary, n_test>50)
    rmse_long <- pivot_longer(rmse_long, 1:models.2.use, names_to="model")
    ggplot(rmse_long, aes(x=year, y=value))+
      geom_col(aes(fill=model), position="dodge")+
      ylab("RMSE")+
      ggtitle(paste(plot_title))+
      xlab("Year")+
      theme(legend.position="top")+
      theme_set(theme_bw(base_size = 15))+
      theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    ggsave(paste("outputs/", plot_title, "_rmse_plot.pdf", sep=""))
  }
  #Calculate overall RMSE
  rmse_total <- as.data.frame(sapply(rmse_summary[,1:models.2.use], calc_rmse, rmse_summary$n_test))
  colnames(rmse_total) <- "rmse_total"
  rmse_total$region <- test_region
  print(rmse_total)

  #Save
  saveRDS(rmse_summary, file=paste("outputs/rmse_years_", test_region, ".rds", sep=""))
  # saveRDS(rmse_summary2, file=paste("outputs/rmse2_years_", test_region, ".rds", sep=""))
  saveRDS(rmse_total, file=paste("outputs/rmse_total_", test_region, ".rds", sep=""))

  #Return RMSE table
  return(rmse_summary)
}

brkptfun <- function(output, test_survey, species){
  #Isolate data, par estimates from output
  datas <- output[["data"]]
  pars <- output[["parameter_estimates"]]
  #List of data types
  types <- c("Synoptic", "Predicted", "GOBH")
  #Create list for saving predictions
  preds <- list()
  #Run predictions for each data type
  for(i in 1:length(types)){
  #Isolate data and name of data type
  dat <- datas[[i]]
  dat_type <- types[i]
  #Pull out scaling parameters
  center <- attr(dat$o2_s, "scaled:center")
  scale <- attr(dat$o2_s, "scaled:scale")
  #Create range of oxygen to predict on
  nd_po2 <- data.frame(o2= seq(0, 100, length.out = 1000))
  nd_po2$o2_s <- (nd_po2$o2-center)/scale
  #Add regional and data type to dataframe
  nd_po2$data <- dat_type
  nd_po2$region <- test_survey
  nd_po2$species <- species
  #Get just the o2 parameter terms
  slope <- filter(pars, (data==dat_type & model=="breakpt(o2)")& (term=="o2_s-slope"))
  thresh <- filter(pars, (data==dat_type & model=="breakpt(o2)")& (term=="o2_s-breakpt"))
  #Function to calculate breakpoint effect
   breakpoint_calc <- function(x, b_slope, b_thresh){
    if (x < b_thresh) {
      pred = x  * exp(b_slope)
      #pred = (x-b_thresh) * b_slope
    } else {
      pred=b_thresh * exp(b_slope)
      #pred = 0
    }
     return(pred)
   }

  nd_po2$est <- sapply(nd_po2$o2_s,breakpoint_calc, slope$estimate,thresh$estimate)
  nd_po2$est_low <- sapply(nd_po2$o2_s,breakpoint_calc, (slope$estimate-slope$std.error), (thresh$estimate-thresh$std.error))
  nd_po2$est_high <- sapply(nd_po2$o2_s,breakpoint_calc, (slope$estimate+slope$std.error), (thresh$estimate+thresh$std.error))
  preds[[i]] <- nd_po2
  }
  preds <- bind_rows(preds)
  return(preds)
}

