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

## Idea
* method 1, human's way, https://www.sudokuwiki.org/Strategy_Families
* method 2, cumputer's way, only back tracking, just try and check the consistency

## UsagI
Type 81 integer elements of a pulzzle to a text file, then run

```bash
./sudoku.x method text_file
```

### pulzzles for test
* pulzzle1.txt: all 0
* pulzzle2.txt: an easy case
* pulzzle3.txt: Arto Inkala 2006
* pulzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* pulzzle5.txt: the 299th sudoku in my first phone
* pulzzle6.txt, pulzzle7.txt: http://norvig.com/sudoku.html

pulzzles 2-5 have unique solutions; pulzzle 6 has multiple solutions; pulzzle 7 have no solution, the duration of solving could be a few minutes

For exmaple, use human's way to solve pulzzle3.txt, and also return the duration of solving
```bash
time ./sudoku.x 1 pulzzle3.txt
```
