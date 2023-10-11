# Define server
server <- shiny::shinyServer(function(input, output){
  # mfa plot fifth page
  output$mfa_plot <- renderPlot({
    if (input$impute_MFA == 1) {
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = input$slider_mfa, method = "Regularized")
      
      parallel::stopCluster(cl)
      
      data <- as.data.frame(data$completeObs)
      df_numeric <- data[ , sapply(data, is.numeric)]
      df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
      df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
      df <- cbind(df_positives_factor, df_positives_numeric)
      colnames(df) <- c(colnames(data)[1:3], rep(composition, 7))
      res.mfa <- MFA(df, group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                     name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
    }
    else {
      df <- data
      colnames(df) <- c(colnames(data)[1:8], rep(composition, 7))
      
      cl <- parallel::makeCluster(2)
      doParallel::registerDoParallel(cl)
      
      res.mfa <- FactoMineR::MFA(df[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                                 name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
      
      parallel::stopCluster(cl)
      
    }
    habillage_ind <- conversion(input$habillage_ind)
    
    if (input$radio_MFA == 1){
      plot(res.mfa, choix = "ind", habillage = habillage_ind)
    }
    else if (input$radio_MFA == 2) {
      plot(res.mfa, choix = "var")
    }
    else if (input$radio_MFA == 3) {
      plot(res.mfa, choix = "group")
    }
    else if (input$radio_MFA == 4) {
      plot(res.mfa, choix = "axes")
    }
    else {
      plot(res.mfa, choix = "ind", partial = "all")
    }
    
  })
  
  
  # mfa results fifth page
  output$results_MFA <- renderPrint({
    if (input$radio_results_MFA == 1) {
      if (input$impute_MFA == 1) {
        
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        data <- missMDA::imputeMFA(data[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = input$slider_mfa, method = "Regularized")
        
        parallel::stopCluster(cl)
        
        data <- as.data.frame(data$completeObs)
        df_numeric <- data[ , sapply(data, is.numeric)]
        df_positives_numeric <- df_numeric[rowSums(df_numeric > 0) == ncol(df_numeric), , drop = FALSE]
        df_positives_factor <- data[rownames(df_positives_numeric), !sapply(data, is.numeric), drop = FALSE]
        df <- cbind(df_positives_factor, df_positives_numeric)
        colnames(df) <- c(colnames(data)[1:3], rep(composition, 7))
        
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        res.mfa <- MFA(df, group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                       name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
        
        parallel::stopCluster(cl)
        
      }
      else {
        df <- data
        colnames(df) <- c(colnames(data)[1:8], rep(composition, 7))
        
        cl <- parallel::makeCluster(2)
        doParallel::registerDoParallel(cl)
        
        res.mfa <- FactoMineR::MFA(df[, -c(1, 2, 3, 7, 8)], group = c(1, 2, rep(7, 7)), type = c("n", "n", rep("s", 7)), ncp = 5,
                                   name.group = c("Species", "Soil", "Stem_bark", "Stem_sapwood", "Stem_heartwood", "Stem_wood", "Coarse_branch_bark", "Coarse_branch_wood_without_bark", "Fine_branches"))
      }
      habillage_ind <- conversion(input$habillage_ind)
      
      if (input$radio_MFA == 1) {
        if (input$results_ind == "Eign values") {
          print(res.mfa$eig)
        }
        else if (input$results_ind == "Inertia ratio") {
          print(res.mfa$inertia.ratio)
        }
        else if (input$results_ind == "Contribution") {
          print(res.mfa$ind$contrib)
        }
        else if (input$results_ind == "Cos2") {
          print(res.mfa$ind$cos2)
        }
        else {
          print(res.mfa$ind$within.inertia)
        }
      }
      
      else if (input$radio_MFA == 2) {
        if (input$results_var == 1) {
          if (input$results_var_quali == "Contribution") {
            print(res.mfa$quali.var$contrib)
          }
          else if (input$results_var_quali == "Cos2") {
            print(res.mfa$quali.var$cos2)
          }
          else if (input$results_var_quali == "Within inertia") {
            print(res.mfa$quali.var$within.inertia)
          }
          else {
            print(res.mfa$quali.var$v.test)
          }
        }
        else {
          if (input$results_var_qaunti == "Correlation") {
            print(res.mfa$quanti.var$cor)
          }
          else if (input$results_var_qaunti == "Contribution") {
            print(res.mfa$quanti.var$contrib)
          }
          else {
            print(res.mfa$quanti.var$cos2)
          }
        }
      }
      
      else if (input$radio_MFA == 3) {
        if (input$results_groups == "Lg") {
          print(res.mfa$group$Lg)
        }
        else if (input$results_groups == "RV") {
          print(res.mfa$group$RV)
        }
        else if (input$results_groups == "Correlation") {
          print(res.mfa$group$correlation)
        }
        else if (input$results_groups == "Contribution") {
          print(res.mfa$group$contrib)
        }
        else {
          print(res.mfa$group$cos2)
        }
      }
      
      else if (input$radio_MFA == 4) {
        if (input$results_partial_axes == "Correlation") {
          print(res.mfa$partial.axes$cor)
        }
        else {
          print(res.mfa$partial.axes$contrib)
        }
      }
      
    }
  })
  
})
