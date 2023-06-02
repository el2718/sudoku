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

## Usage
Type a pulzzle to a text file that has 81 integer elements, then run this command in a terminal

```bash
./sudoku.x method text_file
```

### pulzzles for test
* pulzzle1.txt: all 0
* pulzzle2.txt: an easy case
* pulzzle3.txt: Arto Inkala 2006
* pulzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* pulzzle5.txt: the 299th sudoku in my first phone
* pulzzle6.txt: the  hardest of http://norvig.com/sudoku.html; it has multiple solutions
* pulzzle7.txt: the unsolvable one of http://norvig.com/sudoku.html; the duration of solving could be a few minutes

For exmaple, use human's way to solve pulzzle3.txt, and also return the duration of solving
```bash
time ./sudoku.x 1 pulzzle3.txt
```
