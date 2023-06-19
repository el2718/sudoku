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
./sudoku.x puzzle method solved_max export eliminated_max
```

* puzzle: a text file with 81 integer elements of a puzzle, a 0 present an empty cell

* method (optional): 
  * 1 (default), logical strategies, https://www.sudokuwiki.org/Strategy_Families
  * 2, brute force, just try and check the consistency with back tracking
* solved_max (optional): the maximum number of solutions that the program could give, in case a sudoku may have multiple solutions.
  * If set to be larger than 1, non-repeative solutions will be presented
  * If set to a number more than the count of all solutions, the count will be reported
  * The default is 2, to check the uniqueness of the solution
  * If set to 1, will not check the uniqueness, but will be faster
* export (optional):
  * 0 (default), do not export results to text files
  * 1, export results to text files. A puzzle with 81 non-empty elements will not be exported
* eliminated_max (optional): If a puzzle has a unique solution, eliminating some non-empty elements from the puzzle could still give the same solution. This value is the maximum number of non-empty elements can be eliminated for the same unique solution. If then no any more non-empty element can be eliminated, the actual eliminated number could be smaller than this value
  * 0 (default), no elimination
  * larger than 0, will present the eliminated puzzle has the same unique solution. The sequence of elimination is random. This can create a puzzle from a full filled sudodu, e.g. from a solution of another puzzle

## Puzzles for test
the solutions of puzzles 1-5 are unique, any puzzle with less than 17 non-empty elements can not have an unique solution. Any non-empty element of puzzles 3-5 can not be eliminated.

* puzzle0.txt: all empty, has all solutions of all sudokus
* puzzle1.txt: an easy case
* puzzle2.txt: the 299th puzzle in my first phone
* puzzle3.txt: a pullze with 17 non-empty elements
* puzzle4.txt: Arto Inkala 2006
* puzzle5.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* puzzle6.txt: a puzzle has 13 solutions
* puzzle7.txt: from http://norvig.com/sudoku.html (hereafter **norvig**), has 148357268 solutions
* puzzle8.txt: from **norvig**, do not have any solution

## Demos

Use logical strategies to solve puzzle 5, set solved_max=2, do not export, no elimination

```bash
./sudoku.x puzzle5.txt
```

Use brute force to solve puzzle 6, and set solved_max=100, save the solutions to text files. Then elminate 30 elements from the solution 10 of puzzle 6, do not save the results.

```bash
./sudoku.x puzzle6.txt 2 100 1
./sudoku.x puzzle6_solution10.txt 2 2 0 30
```

Eliminate elements from puzzle 2 as much as possible, save the solution and the elminated puzzle.

```bash
./sudoku.x puzzle2.txt 1 2 1 81
```

## Algorithm comparision 
* https://www.sudokuwiki.org/Brute_Force_vs_Logical_Strategies gives a comparision between methods.
https://www.sudokuwiki.org/sudoku.htm (hereafter **sudokuwiki**) provides both
brute force (click <mark style="background-color: #FFFF00">solution count</mark>) 
and logical strategies (click <mark style="background-color: #FF0000">take step</mark>) 
to solve a sudoku.
* For the puzzles above, all results from sudodu.f90 are consisted with the outputs from **sudokuwiki**.
* **norvig** takes a few minutes to give solutions for  puzzle 7, 
and to confirms no solution for puzzle 8 even longer.
* For puzzle 7, both sudodu.f90 and the brute force of **sudokuwiki** give solutions immediately.
* For puzzle 8, with brute force, both sudodu.f90 and **sudokuwiki** take a few minutes to confirm no solution; 
while with logical strategies, both sudodu.f90 and **sudokuwiki** confirm no solution immediately.
