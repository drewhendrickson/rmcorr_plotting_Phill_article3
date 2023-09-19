library(rmcorr)
library(ggplot2)
library(extrafont)
font_import()
loadfonts()


data <- read.csv("RMCOR ALL.csv")

data$ID <- factor(data$ID)

head(data)
str(data)
summary(data)

##################################

for (phys in c("SCR",  "SCL", "HR_BPM")) {
  for (s_report in c("NRS", "FMS")) {

    #phys = "SCR"
    #s_report = "NRS"

    # group two measures to correlate
    tmp_data_full <- data.frame(subjectID = data$ID,
                                physio_measure = data[, phys],
                                self_report_measure = data[, s_report])

    # build variables for labels
    current_x = s_report
    current_y = phys
    current_title = paste0("Repeated Meaure correlation between ",
                           s_report, " and ", phys)

    # build file names
    filename = paste0(s_report, "_", phys, ".tiff")
    faceted_filename = paste0("faceted_", s_report, "_", phys, ".tiff")

    # make things special for Heart Rate because Drew is grumpy
    if (phys == "HR_BPM") {
      current_title = paste0("Repeated Meaure correlation between ",
                             s_report, " and Heart Rate (BPM)")
      current_y = "Heart Rate (BPM)"
    }

    # remove all the incomplete rows
    tmp_data_clean <- tmp_data_full[complete.cases(tmp_data_full),]

    # check the data
    str(tmp_data_clean)
    head(tmp_data_clean)

    # run rmcorr analysis
    my.rmc <- rmcorr(participant = subjectID,
                     measure1 = self_report_measure,
                     measure2 = physio_measure,
                     dataset = tmp_data_clean)

    # report the r, df, p-value, CI% values
    print(my.rmc)

    # plotting
    ggplot(tmp_data_clean,
           aes(x = self_report_measure,
               y = physio_measure,
               group = subjectID, color = subjectID)) +
      geom_point(aes(colour = subjectID),
                 size = 2) +
      geom_line(aes(y = my.rmc$model$fitted.values),
                linetype = 1,
                size = 0.75) +
      theme_bw() +
      labs(x = current_x,
           y = current_y,
           title = current_title) +
      guides(color = FALSE, group = FALSE) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            plot.title = element_text(hjust = 0.5,
                                      face="bold",
                                      size = 14))

    # save the plot, change these values to change the size
    ggsave(filename,
           width = 2500,
           height = 2500,
           units = "px",
           dpi = 350)


    # plotting with facets
    ggplot(tmp_data_clean,
           aes(x = self_report_measure,
               y = physio_measure,
               group = subjectID, color = subjectID)) +
      geom_point(aes(colour = subjectID),
                 size = 2) +
      geom_line(aes(y = my.rmc$model$fitted.values),
                linetype = 1,
                size = 0.75) +
      theme_bw() +
      labs(x = current_x,
           y = current_y,
           title = current_title) +
      guides(color = FALSE, group = FALSE) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            plot.title = element_text(hjust = 0.5,
                                      face="bold",
                                      size = 14)) +
      facet_wrap( ~ subjectID)

    # save the faceted plot
    # change these values to change the size
    ggsave(faceted_filename,
           width = 2500,
           height = 2500,
           units = "px",
           dpi = 350)
  }
}
