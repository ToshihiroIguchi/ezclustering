


#距離を計算
dist.calc <- function(df, method, xdata, ydata, scale = TRUE){
  
  #データを準備
  data <- df %>% dplyr::select(xdata) %>% as.matrix()
  
  #行名をつける
  rownames(data) <- as.vector(as.matrix(df[ydata]))
  
  
  #スケーリングの有無、距離の計算方法を指定して距離を計算
  dist.method <- function(x, method, scale = TRUE){
    #スケーリングする
    if(scale){x <- scale(x)}
    
    #method中の大文字を小文字にする
    method <- tolower(method)
    
    #距離を計算
    ret <- dist(x, method = method)

    #戻り値
    return(ret)
  }
  
  
  #距離を計算
  dist.data <- data %>% dist.method(method = method, scale = scale)
  
  return(dist.data)

  
}

#クラスタリング
clust <- function(dist, method = "Ward"){
  
  #クラスタリングの方法
  #hclust関数で認識する値と、表示する名前
  hclust.method <- c("ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
  names(hclust.method) <- c("Ward", "Single", "Complete", "Average", "McQuitty", "Median", "Centroid")
 
  #クラスタリング
  ret <- hclust(dist, method = hclust.method[method])
  
  #戻り値
  return(ret)
}

#デンドログラムの表示





