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

* text_file: a text file with 81 integer elements of a puzzle, a 0 present an empty cell

* method (optional):
  * 1 (default), human's way, https://www.sudokuwiki.org/Strategy_Families
  * 2, cumputer's way, only back tracking, just try and check the consistency
* n_solved_max (optional): 
a sudoku could has multiple solutions, 
this parameter is the maximum number of solutions that the program will give.
If this number is more that the number of solutions, it will report the solution count of the sudoku. 
The default value is 2, to check the uniqueness of the solution. If it is set to 1, the program will not check the uniqueness but will be faster. 


## Puzzles for test
the solutions of puzzles 2-5 are unique

* puzzle1.txt: all empty, has all solutions of all sudokus
* puzzle2.txt: an easy case
* puzzle3.txt: Arto Inkala 2006
* puzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* puzzle5.txt: the 299th sudoku in my first phone
* puzzle6.txt: http://norvig.com/sudoku.html, has 100000+ solutions
* puzzle7.txt: http://norvig.com/sudoku.html, has no solution
* puzzle8.txt, a sudoku has 13 solutions

## Demos

use computer's way to solve puzzle 3, and set n_solved_max=2

```bash
./sudoku.x puzzle3.txt 2
```

use human's way to solve puzzle 8, and set n_solved_max=100
```bash
./sudoku.x puzzle8.txt 1 100
```
