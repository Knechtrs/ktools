#----- doesnt work yet!! ----#


# # load Plotting_Expression function first
# source('/data/cephfs-1/work/groups/duda/users/knechtrs_c/Projects/Functions/Plotting/Plotting_Expression.r')

# # create function to get markers and plot plots: 
# GetMarker_Plotting <- function(Seurat_obj, GeneNames) {  
   
#     # Create plots by applying Plotting_Expression function
#     Plots_Expression <- lapply(GeneNames, ~ Plotting_Expression(Seurat_obj, .x))
    
#     # Remove NULL plots from the list
#     Plots_Expression <- keep(Plots_Expression, ~ !is.null(.x))
    
#     # Combine plots and set shared axis labels if there are any plots to display
#     if (length(Plots_Expression) > 0) {
#        Plots_Expression <- wrap_plots(Plots_Expression, axis_title="collect") & theme(plot.margin = margin(10, 10, 10, 10, "pt"))
#         Plots_Expression
#     } else {
#         print("No markers meet the threshold criteria.")
#     }
# }