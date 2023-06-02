# sudoku
to solve a sudoku with human's way or cumputer's way

-----------------------------
## Compilation
```bash
gfortran -O3 sudoku.f90 -o sudoku.x
```

## Usage
method 1 is human's way, method 2 is cumputer's way

type your pulzzle to a txt file that has 81 integer elements like pulzzle*.txt

```bash
./sudoku.x method txt_file
```

### pulzzles for test
* pulzzle1.txt: all 0
* pulzzle2.txt: an easy case
* pulzzle3.txt: Arto Inkala 2006
* pulzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
* pulzzle5.txt: the 299th sudoku in my first phone
* pulzzle6.txt: http://norvig.com/sudoku.html, hardest, has multiple solutions
* pulzzle7.txt: http://norvig.com/sudoku.html, unsolvable

For exmaple, use human's way to solve pulzzle3.txt, and also return the duration of solving
```bash
time ./sudoku.x 1 pulzzle3.txt
```
