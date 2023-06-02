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
run in a terminal

```bash
./sudoku.x text_file method check_multi
```

* text_file: 
Type 81 integer elements of a pulzzle to a text file, and set

* method (optional):
  * 1 (default), human's way, https://www.sudokuwiki.org/Strategy_Families
  * 2, cumputer's way, only back tracking, just try and check the consistency

* check_multi (optional): 
  * 0 (default), don't check multiple solutions
  * 1, check multiple solutions


### pulzzles for test
* pulzzle1.txt: all 0
* pulzzle2.txt: an easy case
* pulzzle3.txt: Arto Inkala 2006
* pulzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* pulzzle5.txt: the 299th sudoku in my first phone
* pulzzle6.txt, pulzzle7.txt: http://norvig.com/sudoku.html

the solutions of pulzzles 2-5 are unique; pulzzle 6 has multiple solutions; pulzzle 7 have no solution, the duration of solving could be a few minutes

For exmaple, use human's way to solve pulzzle 3, and return the duration of solving

```bash
time ./sudoku.x 1 pulzzle3.txt
```
