!contact: Chen, Jun; el2718@mail.ustc.edu.cn
!gfortran -O3 sudoku.f90 -o sudoku.x; 
!sudoku.x text_file method n_solved_max

module share
implicit none
integer::n_solved, n_solved_max
integer, allocatable::solved_sudoku(:,:,:)
end module share


program main
use share
implicit none
integer::sudoku(9,9), method
character(len=128):: text_file
character(len=1):: method_str
character(len=9):: n_solved_max_str
logical:: exist_flag
!--------------------------------------------
call get_command_argument(1, text_file)
call get_command_argument(2, method_str)
call get_command_argument(3, n_solved_max_str)

if (method_str .eq. "2") then
	method=2
else
	method=1
endif

if(n_solved_max_str .eq. "") then
	n_solved_max=2
else
	read(n_solved_max_str, '(i9)') n_solved_max
endif

inquire(file = trim(text_file), exist=exist_flag)
if(exist_flag) then	
	open(unit=8, file=trim(text_file), status='old')
	read(8, *) sudoku
	close(8)
	call resolve(sudoku, method)
else
	print*, trim(text_file)//' not exist'
endif
end program main


subroutine resolve(sudoku, method)
use share
implicit none
integer::sudoku(9,9), k, method
logical::candidate(9,9,9), bug_flag
!--------------------------------------------
call check_sudoku(sudoku, bug_flag)
if (bug_flag) then
	write(*,"('problematic input')")
	return
endif
!--------------------------------------------
write(*,"('  == input ==')")
call print_sudoku(sudoku)
n_solved=0
allocate(solved_sudoku(9,9,n_solved_max))
!--------------------------------------------
select case(method)
case(1) !human's way
	call initialize_candidate(sudoku, candidate)
	call process(candidate, bug_flag)
	if (.not. bug_flag)	call try_candidate(candidate, bug_flag)
case(2) !computer's way	
	call try_sudoku(1, sudoku)
end select
!--------------------------------------------
do k=1, n_solved
	write(*,"('  == solution', i5, ' ==')") k
	call print_sudoku(solved_sudoku(:,:,k))
enddo
	
if (n_solved .lt. n_solved_max) then
	select case (n_solved)
	case(0)
		write(*,"('  == it do not have any solution ==')")
	case(1)	
		write(*,"('  == it has a unique solution ==')")
	case default
		write(*,"('  == it has', i5,' solutions ==')") n_solved
	end select
endif
!--------------------------------------------
deallocate(solved_sudoku)
end subroutine resolve


subroutine process(candidate, bug_flag)
implicit none
integer::no_update_times, n_candidate, n_candidate0
logical::candidate(9,9,9), bug_flag
!--------------------------------------------
no_update_times=0
n_candidate0=count(candidate)
do while(count(candidate)>81)
	call strategy(no_update_times+1, candidate, bug_flag)	
	if (bug_flag) return
	
	n_candidate=count(candidate)	
	if(n_candidate0==n_candidate) then
		no_update_times=no_update_times+1
		if (no_update_times==3) return
	else
		n_candidate0=n_candidate
		no_update_times=0
	endif
enddo
end subroutine process


recursive subroutine strategy(n, candidate, bug_flag)
!https://www.sudokuwiki.org/strategy_families
!n=1, basic
!n=2, naked pairs, hidden pairs
!n=3, naked triples, hidden triples
implicit none
integer::k, n, m, way, group(9), i_group, c9n, i_combination
integer::i_series(9), j_series(9), indgen(9)=(/1,2,3,4,5,6,7,8,9/)
logical::candidate(9,9,9), candidates(9), positions(9), bug_flag
!--------------------------------------------
call combination_number(9, n, c9n)
do i_combination=1,c9n	
	call combination_group(n, group, i_combination)
do way=1,3
do k=1,9
!--------------------------------------------
!if n positions have only n candidates
	call ij_series(k, way, group, i_series, j_series)
	candidates=candidate(:, i_series(1), j_series(1))	
	do i_group=2,n		
		candidates=candidates .or. candidate(:,i_series(i_group),j_series(i_group))	
	enddo
	
	bug_flag=count(candidates) .lt. n
	if(bug_flag) return	
	
	if (count(candidates) .eq. n) then
		do i_group=n+1,9
			where(candidates) candidate(:,i_series(i_group),j_series(i_group))=.false.
		enddo
	endif	
!--------------------------------------------
!if n candidates found in only n positions
	call ij_series(k, way, indgen, i_series, j_series)
	forall(m=1:9) positions(m)=any(candidate(group(1:n), i_series(m), j_series(m)))
	
	bug_flag=count(positions) .lt. n
	if(bug_flag) return
	
	if (count(positions) .eq. n) &
	forall(m=1:9, positions(m)) candidate(group(n+1:9), i_series(m), j_series(m))=.false.
!--------------------------------------------
enddo
enddo
enddo
call check_candidate(candidate, bug_flag)
end subroutine strategy


recursive subroutine try_candidate(candidate, bug_flag)
use share
implicit none
integer::candidate_first, k, i, j, n_solved0, n_guess
logical::candidate(9,9,9), candidate_try(9,9,9), bug_flag
!--------------------------------------------
100 continue

if (count(candidate) .le. 81) then
	call check_candidate(candidate, bug_flag)
	if (bug_flag) return
	n_solved=n_solved+1
	forall(i=1:9,j=1:9)	solved_sudoku(i,j,n_solved)=findloc(candidate(:,i,j),.true.,1)
	return
endif
!--------------------------------------------
do n_guess=2,9
do j=1,9
do i=1,9
if (count(candidate(:,i,j)) .eq. n_guess) then	
	candidate_try=candidate
	candidate_first=findloc(candidate(:,i,j),.true.,1)
	candidate_try(:,i,j)=.false.
	candidate_try(candidate_first,i,j)=.true.
	call process(candidate_try, bug_flag)
	
	n_solved0=n_solved
	if (.not. bug_flag)	call try_candidate(candidate_try, bug_flag)
	
	if (n_solved .eq. n_solved_max) then
		candidate=candidate_try
		return
	endif
		
	if (bug_flag .or. n_solved>n_solved0) then
		candidate(candidate_first,i,j)=.false.
		call process(candidate, bug_flag)
		
		if (bug_flag) then
			return
		else
			goto 100
		endif
	endif
endif
enddo
enddo
enddo
end subroutine try_candidate


recursive subroutine try_sudoku(ij, sudoku)
use share
implicit none
integer::sudoku(9,9), sudoku_try(9,9), ij, i, j, m, i0, j0
logical::bug_flag
!--------------------------------------------
if (ij .eq. 82) then
	n_solved=n_solved+1
	solved_sudoku(:,:,n_solved)=sudoku
	return
endif
!--------------------------------------------
j=(ij-1)/9+1
i=mod(ij-1,9)+1
if(sudoku(i,j)==0)then
	sudoku_try=sudoku
	do m=1,9
		i0=i-mod(i-1,3)
		j0=j-mod(j-1,3)
		if(.not.(any(sudoku(i,:) .eq. m) .or. & 
				 any(sudoku(:,j) .eq. m) .or. & 
				 any(sudoku(i0:i0+2,j0:j0+2) .eq. m))) then
			sudoku_try(i,j)=m
		
			call try_sudoku(ij+1, sudoku_try)
		
			if (n_solved .eq. n_solved_max) then
				sudoku=sudoku_try
				return
			endif
			
		endif
	enddo
else
	call try_sudoku(ij+1, sudoku)
endif
end subroutine try_sudoku


subroutine initialize_candidate(sudoku, candidate)
implicit none
integer::sudoku(9,9), i, j
logical::candidate(9,9,9)
!--------------------------------------------
candidate=.true.

do j=1,9
do i=1,9
	if (sudoku(i,j)>0) then
		candidate(:,i,j)=.false.
		candidate(sudoku(i,j),i,j)=.true.
	endif
enddo
enddo
end subroutine initialize_candidate


subroutine check_candidate(candidate, bug_flag)
implicit none
integer::k, m, i0, j0
logical::candidate(9,9,9), bug_flag
!--------------------------------------------
do k=1,9
	j0=(k-1)/3*3+1
	i0=mod(k-1,3)*3+1
do m=1,9
	!check no any candidate in a cell
	bug_flag= .not. (any(candidate(:,m,k)) .and. & 
	!check the candidate m lacked in a unit
	any(candidate(m,k,:)) .and. any(candidate(m,:,k)) .and. any(candidate(m,i0:i0+2,j0:j0+2)))
	if(bug_flag) return
enddo
enddo
end subroutine check_candidate


subroutine check_sudoku(sudoku, bug_flag)
implicit none
integer::sudoku(9,9), k, m, i0, j0
logical::bug_flag
!--------------------------------------------
do k=1,9
	j0=(k-1)/3*3+1
	i0=mod(k-1,3)*3+1
do m=1,9
	bug_flag=count(sudoku(k,:) .eq. m)>1 .or. & 
	         count(sudoku(:,k) .eq. m)>1 .or. & 
	         count(sudoku(i0:i0+2,j0:j0+2) .eq. m)>1
	if(bug_flag) return
enddo
enddo
end subroutine check_sudoku


subroutine print_sudoku(sudoku)
implicit none
integer::sudoku(9,9), i, j
character(len=30)::row_str, divide_str
!--------------------------------------------
divide_str='       ------+-------+------'
do j=1,9
	write(row_str,"(6x, 3i2,' |',3i2,' |',3i2)") (sudoku(i,j),i=1,9)
	do i=8,31
		if (row_str(i:i) .eq. '0') row_str(i:i)='.'
	enddo
	write(*,*) row_str	
	if (j==3 .or. j==6) write(*,*) divide_str
enddo
end subroutine print_sudoku


subroutine ij_series(k, way, group, i_series, j_series)
integer::k, way, group(9), i_series(9), j_series(9)
!--------------------------------------------
select case(way)
case(1)
	i_series=k; j_series=group
case(2)	
	i_series=group; j_series=k
case(3)
	j_series=((k-1)/3)*3+1+(group-1)/3
	i_series=(mod(k-1,3))*3+1+mod(group-1,3)
end select
end subroutine ij_series


subroutine combination_number(m, n, cmn)
implicit none
integer::i, m, n, cmn
!--------------------------------------------
!if (n .eq. 1 .or. n .eq. 8) cmn=9
!if (n .eq. 2 .or. n .eq. 7) cmn=36
!if (n .eq. 3 .or. n .eq. 6) cmn=84
!if (n .eq. 4 .or. n .eq. 5) cmn=126
 cmn=m-n+1
do i=m-n+2,m
	cmn=cmn*i
enddo
do i=2,n
	cmn=cmn/i
enddo
end subroutine combination_number


subroutine combination_group(n, group, i)
implicit none
integer::i, j, k, n, group(9), step
!--------------------------------------------
do k=1,9
	group(k)=k
enddo

do step=2,i
	call group_plus1(group, n, n)
enddo

do k=n+1,9
do j=1,k-1
	if (all(j .ne. group(1:k-1))) then 
		group(k)=j
		exit
	endif
enddo
enddo
end subroutine combination_group


recursive subroutine group_plus1(group, k, n)
implicit none
integer::group(9), k, n
!--------------------------------------------	
if (group(k) .lt. 9-n+k) then
	group(k)=group(k)+1
else
	if (k .eq. 1) return
	call group_plus1(group, k-1, n)
	group(k)=group(k-1)+1
endif
end subroutine group_plus1
