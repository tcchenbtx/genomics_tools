
if(!require(ggplot2)){
    install.packages("ggplot2")
}

library(ggplot2)

make_annotation_graphs <- function (data_input){
    input_data <- read.table(data_input, fill = TRUE, sep="\t", header=T, quote="")
    input_data$Annotation <- sub(" .*", "", input_data$Annotation)
    #input_data <- subset(input_data, !(input_data$Strand == ""))
    total_num <- nrow(input_data)
    count_table <- table(input_data$Annotation)
    count_summary <- cbind(count_table)
    count_summary <- data.frame(count_summary)
    count_summary$percentage <- count_summary$count_table/total_num*100
    # clean up the count_summary
    #if ("" %in% rownames(count_summary)){
    #    count_summary <- subset(count_summary, !(rownames(count_summary) == ""))
    #}
    
    x_names <- character(length = 0)
    for (i in row.names(count_summary)){
        
        if (i == "3'"){
            x_names <- c(x_names, "3'-UTR")
        }else if (i == "5'"){
            x_names <- c(x_names, "5'-UTR")
        }else{
            x_names <- c(x_names, i)
        }
    }
    
    row.names(count_summary) <- x_names
    
    x_lables <- character()
    figure_legend <- character()
    pie_label <- character()
    for (i in x_names){
        x_lables <- c(x_lables, LETTERS[match(i, x_names)])
        figure_legend <- c(figure_legend, paste(LETTERS[match(i, x_names)], " : ", i, " (", format(round(count_summary[i,"percentage"], digits = 2), nsmall = 2), "%)", sep =""))
        pie_label <- c(pie_label, paste(i, " (", format(round(count_summary[i,"percentage"], digits = 2), nsmall = 2 ), "%)", sep = ""))
    }
    
    # assemble required data to make plot
    for_plot <- count_summary
    for_plot$x <- x_lables
    for_plot$annotation <- figure_legend
    
    # make and save bar plot
    output_title <- substr(data_input, 1, nchar(data_input)-4)
    bargraph <- ggplot(data = for_plot, aes(x=x, y=percentage, fill=annotation))
    bargraph + geom_bar(stat = "identity", color="black") + ggtitle(c(output_title, "_bar")) + labs(y="Percentage(%)", x="")
    ggsave(filename=paste(output_title, "_bar.png", sep=""), device="png")
    
    # make and save pie plot
    piegraph <- ggplot(data = for_plot, aes(x="", y=percentage, fill=annotation))
    piegraph + geom_bar(stat = "identity", color="black", width = 1) + ggtitle(c(output_title, "_pie")) + coord_polar(theta = "y") + theme_void()
    ggsave(filename=paste(output_title, "_pie.png", sep=""), device = "png")
    
    
    ######### make plots without ggplot2 #########
    #y_max <- max(count_summary$freq) + 20
    #jpeg(paste(output_title, "_bar", ".jpg", sep=""))
    #par(mar = c(5, 4, 4, 10)+0.1) 
    #barplot(count_summary$freq, main= output_title, names.arg = x_lables, ylab = "percentage (%)", ylim=c(0,y_max))
    #legend(10,50, legend=figure_legend, cex=0.7)
    #dev.off()
    
    # make and save pie plot
    #jpeg(paste(output_title, "_pie", ".jpg", sep=""))
    #pie(count_summary$freq, pie_label, main= output_title, cex=0.6)
    #dev.off()
}

#################################################################
# To use:
make_annotation_graphs("Your_Homer_annotation_txt_file.txt")
