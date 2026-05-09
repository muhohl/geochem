utils::globalVariables(c(
    # clipping_element / geom_dare / geom_magnetite_deposit / geom_magnetite_origin:
    # bare column names used in aes()
    "x", "y", "label",
    # geom_pca_arrows: column names from recipes::tidy() output
    "component", "terms",
    # geom_tas_diagram: column names in annotation data frames
    "xmin", "xmax", "ymin", "ymax",
    # ggcoloredqq: column added by mutate(), used in aes()
    ".theoretical",
    # ggfspmolar: columns added by fsp_molar(), used in aes()
    "na_al_molar", "k_al_molar",
    # ggspider / spider_plot: internal column names from pivot/summarise
    "ppm", "ID", "Median", "XXX", "median",
    # eigen_value_plot: columns created by mutate(), used in aes()
    "KNN", "Element",
    # laser_map / laser_map2 / pca_plot: column names from recipes::tidy() output
    "value",
    # niggli_numbers: Niggli number output columns used in mutate()
    "al", "alk", "fm", "si",
    # oxides_to_elements: magrittr dot placeholder and rlang := operator
    ".", ":=",
    # drillhole_section: rlang .data pronoun
    ".data",
    # raster_map: intermediate columns added by mutate()
    "new_x", "new_y",
    # raster_section: tidyselect::where helper
    "where",
    # xmod_df_wrangler: column names in XMOD CSV files
    "Sample", "Wt", "Mineral", "MineralName", "Xray_X", "Xray_Y",
    # xmod_double_mapping: column names in XMOD data frames
    "Mineral_Name", "Wt%", "Center_X", "Center_Y"
))
