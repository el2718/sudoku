# sudoku
to solve a sudoku with human's way or cumputer's way

-----------------------------
## files of pulzzles
* pulzzle1.txt: all 0
* pulzzle2.txt: an easy case
* pulzzle3.txt: Arto Inkala 2006
* pulzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* pulzzle5.txt: the 299th sudoku in my first phone
* pulzzle6.txt: http://norvig.com/sudoku.html, hardest, has multiple solutions
* pulzzle7.txt: http://norvig.com/sudoku.html, unsolvable

## Usage
method 1 is human's way, method 2 is cumputer's way

```bash
gfortran -O3 sudoku.f90 -o sudoku.x
sudoku.x txt_file method
```

For exmaple, use human's way to solve pulzzle5.txt
```bash
sudoku.x pulzzle5.txt 1
```
