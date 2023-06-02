# sudoku
to solve a sudoku with human's way or cumputer's way

-----------------------------
## Compilation
```bash
gfortran -O3 sudoku.f90 -o sudoku.x
```
or

```bash
ifort -O3 sudoku.f90 -o sudoku.x
```

## Usage
run this command in a terminal

```bash
./sudoku.x text_file method check_multi
```

* text_file: a text file with 81 integer elements of a puzzle, 0 for empty

* method (optional):
  * 1 (default), human's way, https://www.sudokuwiki.org/Strategy_Families
  * 2, cumputer's way, only back tracking, just try and check the consistency

* check_multi (optional): 
  * 0 (default), don't check multiple solutions
  * 1, check multiple solutions


### puzzles for test
* puzzle1.txt: all 0
* puzzle2.txt: an easy case
* puzzle3.txt: Arto Inkala 2006
* puzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* puzzle5.txt: the 299th sudoku in my first phone
* puzzle6.txt, puzzle7.txt: http://norvig.com/sudoku.html

the solutions of puzzles 2-5 are unique; puzzle 6 has multiple solutions; puzzle 7 have no solution, the duration of solving could be a few minutes

For exmaple, use human's way to solve puzzle 3, don't check multiple solutions; And return the duration of solving

```bash
time ./sudoku.x puzzle3.txt 1
```
