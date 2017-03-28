### import

import_simd = function(){
  simd = read.csv("../data/import/simd.csv", stringsAsFactors = FALSE)
  
  return(list(simd=simd))
}