sudoku = matrix(
  c(3,0,0,4,5,1,0,0,0,
    6,0,9,0,0,2,4,0,0,
    1,0,0,0,0,9,5,8,7,
    5,0,2,0,0,4,0,3,8,
    0,0,6,0,0,0,9,0,0,
    7,1,0,5,0,0,6,0,4,
    9,4,7,1,0,0,0,0,5,
    0,0,1,9,0,0,3,0,2,
    0,0,0,7,8,6,0,0,9), 
  nrow = 9, 
  ncol = 9, 
  byrow = TRUE)

validate = function(sudoku,x,y,n){

  if(n %in% sudoku[x,]){return(FALSE)} 

  if(n %in% sudoku[,y]){return(FALSE)}

  for(i in 1:3){
    for(j in 1:3){
      if(sudoku[(((x-1) %/% 3 ) * 3) + i, (((y-1) %/% 3 ) * 3) + j] == n){
        return(FALSE)
      }
    }
  }

  return(TRUE)
}

solver = function(sudoku){
  for(x in 1:9){
    for(y in 1:9)
      if(sudoku[x,y] == 0){
        for(n in 1:9){
          if(validate(sudoku = sudoku, x = x, y = y, n = n)){
            sudoku[x,y] = n
            solver(sudoku = sudoku)} else {
              sudoku[x,y] = 0
            }
          }
        return()
      }
    } 
  print(sudoku)
}

solver(sudoku = sudoku)
