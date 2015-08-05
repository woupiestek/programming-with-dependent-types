module SudokuSolver

data Constraint n = 
  BlockEntry (Fin n) (Fin n) (Fin (n*n)) | 
  ColumnEntry (Fin (n*n)) (Fin (n*n)) | 
  ColumnRow (Fin (n*n)) (Fin (n*n))| 
  RowEntry (Fin (n*n)) (Fin (n*n))
  
data SudokuBoard n = Vec (n*n) (Vec (n*n) Maybe (Fin (n * n)))
