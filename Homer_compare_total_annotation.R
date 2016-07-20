
# take two annotation output (.txt) from Homer to compare the total annotation changes:

compare_annotation <- function (input1, input2){
    input_data1 <- read.table(input1, header = TRUE, fill = TRUE, quote = "", sep = "\t")
    input_data2 <- read.table(input2, header = TRUE, fill = TRUE, quote = "", sep = "\t")
    input_data1$Annotation <- sub(" .*", "", input_data1$Annotation)
    input_data2$Annotation <- sub(" .*", "", input_data2$Annotation)
    count_table1 <- table(input_data1$Annotation)
    count_table2 <- table(input_data2$Annotation)
    count_summary1 <- cbind(count_table1)
    count_summary2 <- cbind(count_table2)
    count_summary1 <- data.frame(count_summary1)
    count_summary2 <- data.frame(count_summary2)
    colnames(count_summary1) <- "annotation_count"
    colnames(count_summary2) <- "annotation_count"
    input1_name <- substr(input1, 1, nchar(input1)-4)
    input2_name <- substr(input2, 1, nchar(input2)-4)
    count_summary1$input <- replicate(nrow(count_summary1), input1_name)
    count_summary2$input <- replicate(nrow(count_summary2), input2_name)
    
    check_num = nrow(count_summary1)
    if (sum(row.names(count_summary1) == row.names(count_summary2)) == check_num){
        x_names <- character(length = 0)
        letter_labels <- character()
        
        for (i in row.names(count_summary1)){
            if (i == "3'"){
                x_names <- c(x_names, "3'-UTR")
            }else if (i == "5'"){
                x_names <- c(x_names, "5'-UTR")
            }else{
                x_names <- c(x_names, i)
            }
        }
        
        for (i in row.names(count_summary1)){
            letter_labels <- c(letter_labels, LETTERS[match(i, row.names(count_summary1))])
        }
        
        count_summary1$annotation <- x_names
        count_summary2$annotation <- x_names
    
        
    }else{
        x_names1 <- character(length = 0)
        for (i in row.names(count_summary1)){
            if (i == "3'"){
                x_names1 <- c(x_names1, "3'-UTR")
            }else if (i == "5'"){
                x_names1 <- c(x_names1, "5'-UTR")
            }else{
                x_names1 <- c(x_names1, i)
            }
        }
        x_names2 <- character(length = 0)
        for (i in row.names(count_summary2)){
            if (i == "3'"){
                x_names2 <- c(x_names2, "3'-UTR")
            }else if (i == "5'"){
                x_names2 <- c(x_names2, "5'-UTR")
            }else{
                x_names2 <- c(x_names2, i)
            }
        }
        count_summary1$annotation <- x_names1
        count_summary2$annotation <- x_names2
    }
    
    merge_table <- rbind(count_summary1, count_summary2)
    output_title <- paste(input1_name, "_", input2_name, sep="")
    
    gg <- ggplot(data = merge_table, aes(x=annotation, y=annotation_count, fill=input))
    gg + geom_bar(stat="identity", position = "dodge") + theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.5)) + ggtitle(output_title) + geom_text(aes(label = annotation_count), vjust=-1, position = position_dodge(0.9), size = 2)
    ggsave(paste(output_title, ".png", sep=""), device = "png")
    
}

#################################################################
# To use:
compare_annotation("Homer_annotation_file1.txt", "Homer_annotation_file2.txt")

