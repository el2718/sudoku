# sudoku
to solve a sudoku with logical strategies or brute force.

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
  * 1 (default), logical strategies, https://www.sudokuwiki.org/Strategy_Families
  * 2, brute force, just try and check the consistency with back tracking
* n_solved_max (optional): 
the maximum number of solutions that the program could give, 
in case a sudoku may have multiple solutions. 
  * If set to a number more than the count of all solutions, the count will be reported. 
  * The default is 2, to check the uniqueness of the solution. 
  * If set to 1, will not check the uniqueness, but will be faster. 


## Puzzles for test
the solutions of puzzles 2-5 are unique.

* puzzle1.txt: all empty, has all solutions of all sudokus
* puzzle2.txt: an easy case
* puzzle3.txt: Arto Inkala 2006
* puzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* puzzle5.txt: the 299th sudoku in my first phone
* puzzle6.txt: from http://norvig.com/sudoku.html (hereafter **norvig**), has 100000+ solutions
* puzzle7.txt: from **norvig**, has no solution
* puzzle8.txt: a sudoku has 13 solutions

## Demos

use brute force to solve puzzle 3, and set n_solved_max=2

```bash
./sudoku.x puzzle3.txt 2
```

use logical strategies to solve puzzle 8, and set n_solved_max=100

```bash
./sudoku.x puzzle8.txt 1 100
```

## Algorithm comparision 
* https://www.sudokuwiki.org/Brute_Force_vs_Logical_Strategies gives a comparision between methods.
https://www.sudokuwiki.org/sudoku.htm (hereafter **sudokuwiki**) provides both
brute force (click <mark style="background-color: #FFFF00">solution count</mark>) 
and logical strategies (click <mark style="background-color: #FF0000">take step</mark>) 
to solve a sudoku.
* For the puzzles above, all results from sudodu.f90 are consisted with the outputs from **sudokuwiki**.
* **norvig** takes a few minutes to give solutions for  puzzle 6, 
and to confirms no solution for puzzle 7 even longer.
* For puzzle 6, both sudodu.f90 and the brute force of **sudokuwiki** give solutions immediately.
* For puzzle 7, with brute force, both sudodu.f90 and **sudokuwiki** take a few minutes to confirm no solution; 
while with logical strategies, both sudodu.f90 and **sudokuwiki** confirm no solution immediately.
