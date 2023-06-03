!Contact: Chen, Jun; el2718@mail.ustc.edu.cn
!gfortran -O3 sudoku.f90 -o sudoku.x; 
!sudoku.x text_file method multi

module share
implicit none
integer::n_reach_end, n_reach_end_max, n_sovled, n_sovled_max, solved_sudoku(9,9,2)
logical::solved_candidate(9,9,9,2)
end module share


program main
use share
implicit none
integer::method, sudoku(9,9)
character(len=128):: text_file
character(len=1):: method_str, multi_str
logical:: exist_flag
!--------------------------------------------
CALL GET_COMMAND_ARGUMENT(1, text_file)
CALL GET_COMMAND_ARGUMENT(2, method_str)
CALL GET_COMMAND_ARGUMENT(3, multi_str)

if (method_str .eq. "2") then
	method=2
else
	method=1
endif

if (multi_str .eq. "2") then
	n_sovled_max=2
	n_reach_end_max=10
else
	n_sovled_max=1
	n_reach_end_max=1
endif

INQUIRE(FILE = trim(text_file), exist=exist_flag)
if(exist_flag) then
	open(unit=8, file=text_file, status='old')
	read(8, *) sudoku
	close(8)
	call resolve(sudoku, method)
endif
end program main


subroutine resolve(sudoku, method)
use share
implicit none
integer::sudoku(9,9), method, i, j, k
logical::candidate(9,9,9), bug_flag
!--------------------------------------------
call check_sudoku(sudoku, bug_flag, .false.)
if (bug_flag) then
	write(*,"('problematic input')")
	return
endif
!--------------------------------------------
write(*,"(7X,9I2)") ((sudoku(i,j),i=1,9),j=1,9)
n_reach_end=0
!--------------------------------------------
select case(method)
case(1) !human's way
	call initialize_candidate(sudoku, candidate)
	call try_candidate(candidate)
case(2) !computer's way	
	call try_sudoku(1, sudoku)
end select
!--------------------------------------------
select case(n_sovled)
case(0)
 	print*,"no solution"	
case(1:2) 
	do k=1, n_sovled
		if (n_sovled .eq. 1) write(*,"('---')")
		if (n_sovled .eq. 2) write(*,"('--- solution', I2, ' --------')") k
		if (method ==1) &
		forall(i=1:9,j=1:9) solved_sudoku(i,j,k)=findloc(solved_candidate(:,i,j,k),.true.,1) 		
		write(*,"(7X,9I2)") ((solved_sudoku(i,j,k),i=1,9),j=1,9)
	enddo
	if (n_sovled .eq. 1) print*,"solved"
 	if (n_sovled .eq. 2) print*,"multiple solutions" 	
end select

end subroutine resolve


subroutine initialize_candidate(sudoku, candidate)
implicit none
integer::sudoku(9,9), i, j
logical::candidate(9,9,9)
!--------------------------------------------
candidate=.true.

do j=1,9
do i=1,9
	if(sudoku(i,j)>0)then
		candidate(:,i,j)=.false.
		candidate(sudoku(i,j),i,j)=.true.
	endif
enddo
enddo
call process(candidate)
end subroutine initialize_candidate


subroutine process(candidate)
!https://www.sudokuwiki.org/Strategy_Families
implicit none
integer::no_update_times, n_candidate, n_candidate0
logical::candidate(9,9,9)
!--------------------------------------------
no_update_times=0
n_candidate0=count(candidate)
do while(count(candidate)>81)
	select case(no_update_times)
	case(0)
		call strategy(1, candidate) !Basic
	case(1)
		call strategy(2, candidate) !Naked Pairs, Hidden Pairs; 
		call strategy(3, candidate) !Naked Triples, Hidden Triples
	case(2)
		exit
	end select
!--------------------------------------------
	n_candidate=count(candidate)
	
	if(n_candidate0==n_candidate) then
		no_update_times=no_update_times+1
	else 
		n_candidate0=n_candidate
		no_update_times=0
	endif
enddo
end subroutine process


subroutine check_candidate(candidate, bug_flag, final_check)
use share
implicit none
integer::i, j, k, m, way, i_series(9), j_series(9)
logical::candidate(9,9,9), bug_flag, final_check, eqv_flag, candidates(9)
integer::indgen(9)=(/1,2,3,4,5,6,7,8,9/)
!--------------------------------------------
bug_flag=.false.
do j=1,9
do i=1,9
	if(count(candidate(:,i,j))==0) then
		bug_flag=.true.
		exit
	endif
enddo
if(bug_flag) exit
enddo
!--------------------------------------------
!check same candidate in a unit
do way=1,3
if(bug_flag) exit
do k=1,9
	call ij_series(k, way, indgen, i_series, j_series)
	candidates=candidate(:, i_series(1), j_series(1))	
	do m=2,9		
		candidates=candidates .or. candidate(:,i_series(m),j_series(m))	
	enddo
	if (count(candidates) .lt. 9) then
		bug_flag=.true.
		exit
	endif
enddo
enddo
!--------------------------------------------
if (.not. bug_flag .and. final_check) then
	n_reach_end=n_reach_end+1
	eqv_flag=.false.
	if (n_reach_end .gt. 1)then		
		do i=1, n_sovled			
		   if (all(candidate .eqv. solved_candidate(:,:,:,i))) then
			   eqv_flag=.true.
			   exit
		   endif
		enddo
	endif
	
	if (n_reach_end .eq. 1 .or. .not. eqv_flag) then  
		n_sovled=n_sovled+1
		solved_candidate(:,:,:,n_sovled)=candidate
	endif
endif

end subroutine check_candidate


recursive subroutine strategy(n, candidate)
implicit none
integer::group(9), i_group, c9n, i_combination
integer::i, j, k, n, m, way, i_series(9), j_series(9)
logical::candidate(9,9,9), candidates(9), positions(9)
integer::indgen(9)=(/1,2,3,4,5,6,7,8,9/)
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
	if (count(candidates) .eq. n) then
		do i_group=n+1,9
			where(candidates) candidate(:,i_series(i_group),j_series(i_group))=.false.
		enddo
	endif
!--------------------------------------------
!if n candidates found in only n positions
	call ij_series(k, way, indgen, i_series, j_series)
	forall(m=1:9) positions(m)=any(candidate(group(1:n), i_series(m), j_series(m)))
	if (count(positions) .eq. n) &
	forall(m=1:9, positions(m)) candidate(group(n+1:9), i_series(m), j_series(m))=.false.
!--------------------------------------------
enddo
enddo
enddo
end subroutine strategy


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
 cmn=1
do i=m-n+1,m
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


subroutine findloc_all(candidate_ij, candidate_number)
implicit none
integer::k, step, candidate_number(9)
logical::candidate_ij(9)
!--------------------------------------------
candidate_number=0
step=0
do k=1,9
	if(candidate_ij(k))then   
		step=step+1
 		candidate_number(step)=k
	endif
enddo
end subroutine findloc_all


recursive subroutine try_candidate(candidate)
use share
implicit none
integer::candidate_number(9), i, j, k
logical::candidate(9,9,9), candidate_try(9,9,9), bug_flag
!--------------------------------------------
if (count(candidate)==81) then
	call check_candidate(candidate, bug_flag, .true.)
	return
endif
do j=1,9
do i=1,9
if (count(candidate(:,i,j)) .ge. 2) then
	call findloc_all(candidate(:,i,j), candidate_number)
	do k=1, count(candidate(:,i,j))
		candidate_try=candidate
		candidate_try(:,i,j)=.false.
		candidate_try(candidate_number(k),i,j)=.true.
		call process(candidate_try)
		call check_candidate(candidate_try, bug_flag, .false.)
		if (bug_flag) then
			candidate(candidate_number(k),i,j)=.false.
			call process(candidate)
			exit
		else
			call try_candidate(candidate_try)
			if (n_sovled .eq. n_sovled_max .or. n_reach_end .eq. n_reach_end_max) then
				candidate=candidate_try
				return
			endif
		endif
	enddo
endif
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
	call check_sudoku(sudoku, bug_flag, .true.)
	return
endif

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
		
			if (n_sovled .eq. n_sovled_max .or. n_reach_end .eq. n_reach_end_max) then
				sudoku=sudoku_try
				return
			endif
		endif
	enddo
else
	call try_sudoku(ij+1, sudoku)
endif
end subroutine try_sudoku


subroutine check_sudoku(sudoku, bug_flag, final_check)
use share
implicit none
integer::sudoku(9,9), i, j, k, m, i0, j0
logical::bug_flag, eqv_flag, final_check
!--------------------------------------------
bug_flag=.false.
do k=1,9
	j0=(k-1)/3*3+1
	i0=mod(k-1,3)*3+1
do m=1,9
	if(count(sudoku(k,:) .eq. m)>1 .or. & 
	   count(sudoku(:,k) .eq. m)>1 .or. & 
	   count(sudoku(i0:i0+2,j0:j0+2) .eq. m)>1) then
		bug_flag=.true.	
		exit   
	endif
enddo
	if(bug_flag) exit
enddo
!--------------------------------------------
if (.not. bug_flag .and. final_check) then
	n_reach_end=n_reach_end+1
	eqv_flag=.false.
	if (n_reach_end .gt. 1)then		
		do i=1, n_sovled
		   if (all(sudoku .eq. solved_sudoku(:,:,i))) then
		   eqv_flag=.true.
		   exit
		   endif
		enddo
	endif
	
	if (n_reach_end .eq. 1 .or. .not. eqv_flag) then  
		n_sovled=n_sovled+1
		solved_sudoku(:,:,n_sovled)=sudoku
	endif
endif
end subroutine check_sudoku
