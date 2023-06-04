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
./sudoku.x text_file method n_solved_max
```

* text_file: a text file with 81 integer elements of a puzzle, a 0 precent an empty cell

* method (optional):
  * 1 (default), human's way, https://www.sudokuwiki.org/Strategy_Families
  * 2, cumputer's way, only back tracking, just try and check the consistency

* n_solved_max (optional): 
a sudoku could has multiple solutions, 
this parameter is the maximum number of solutions that the program will give.
Default and minimum is 2, to check the uniqueness of the solution


### puzzles for test
* puzzle1.txt: all 0
* puzzle2.txt: an easy case
* puzzle3.txt: Arto Inkala 2006
* puzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* puzzle5.txt: the 299th sudoku in my first phone
* puzzle6.txt, puzzle7.txt: http://norvig.com/sudoku.html

the solutions of puzzles 2-5 are unique; puzzle 6 has multiple solutions; puzzle 7 has no solution

For exmaple, use computer's way to solve puzzle 3, don't check multiple solutions; and return the duration of solving

```bash
time ./sudoku.x puzzle3.txt 2
```
