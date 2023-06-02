# sudoku
to solve sudoku with computer's or human's way

-----------------------------
## files of pulzzles
pulzzle1.txt: all 0
pulzzle2.txt: an easy case
pulzzle3.txt: Arto Inkala 2006
pulzzle4.txt: https://www.sudokuwiki.org/Arto_Inkala_Sudoku
pulzzle5.txt: the 299th sudoku in my first phone
pulzzle6.txt: http://norvig.com/sudoku.html,hardest,has multiple solutions
pulzzle7.txt: http://norvig.com/sudoku.html unsolvable

## Usage
>gfortran -O3 sudoku.f90 -o sudoku.x
>
>sudoku.x txt_file method_str

For exmaple, use method 1 (human's way) to solve pulzzle5.txt
>sudoku.x pulzzle5.txt 1
