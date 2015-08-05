module SudokuSolver --Sudoku solver based on Knuth's X

-- type of constraints, e.g. 'each entry occurs in each block'
data Constraint n = 
  BlockEntry (Fin n) (Fin n) (Fin (n*n))| 
  ColumnEntry (Fin (n*n)) (Fin (n*n))| 
  ColumnRow (Fin (n*n)) (Fin (n*n))| 
  RowEntry (Fin (n*n)) (Fin (n*n))
  
-- type of Sudokuboard
data SudokuBoard n = Vec (n*n) (Vec (n*n) Maybe (Fin (n * n)))


