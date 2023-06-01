!Contact: Chen, Jun; el2718@mail.ustc.edu.cn
!gfortran sudoku.f90; time ./a.out

program main
implicit none
integer::sudoku(9,9), i, j, k, sudokus(9,9,5)
logical::candidate(9,9,9), bug_flag
!--------------------------------------------
data (((sudokus(i,j,k),i=1,9),j=1,9),k=1,5) /&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,&
0,0,0,0,0,0,0,0,0,& !0
!--------------------
0,5,0,0,4,7,0,0,0,&
0,0,4,0,0,0,0,3,0,&
2,0,0,0,9,0,0,1,0,&
8,0,1,0,0,6,0,0,0,&
0,0,6,9,0,8,4,0,0,&
0,0,0,5,0,0,1,0,8,&
0,4,0,0,2,0,0,0,1,&
0,3,0,0,0,0,9,0,0,&
0,0,0,7,5,0,0,2,0,&  !easy
!--------------------
0,0,5,3,0,0,0,0,0,&
8,0,0,0,0,0,0,2,0,&
0,7,0,0,1,0,5,0,0,&
4,0,0,0,0,5,3,0,0,&
0,1,0,0,7,0,0,0,6,&
0,0,3,2,0,0,0,8,0,&
0,6,0,5,0,0,0,0,9,&
0,0,4,0,0,0,0,3,0,&
0,0,0,0,0,9,7,0,0,& !Arto Inkala 2006
!--------------------
8,0,0,0,0,0,0,0,0,&
0,0,3,6,0,0,0,0,0,&
0,7,0,0,9,0,2,0,0,&
0,5,0,0,0,7,0,0,0,&
0,0,0,0,4,5,7,0,0,&
0,0,0,1,0,0,0,3,0,&
0,0,1,0,0,0,0,6,8,&
0,0,8,5,0,0,0,1,0,&
0,9,0,0,0,0,4,0,0,& !https://www.sudokuwiki.org/Arto_Inkala_Sudoku
!--------------------
4,0,6,0,0,2,0,0,0,&
0,0,0,1,0,0,0,0,2,&
0,0,1,0,9,0,5,4,0,&
0,9,4,0,0,5,0,1,0,&
1,8,0,0,2,6,0,0,0,&
5,0,2,0,0,0,0,7,0,&
0,0,0,0,6,0,0,3,0,&
7,1,0,0,8,0,9,0,0,&
0,0,0,9,0,0,0,0,0/  !the 299th sudoku in my first phone

do k=3,4
	sudoku=sudokus(:,:,k)
	call initialize(sudoku, candidate)
	call solver(sudoku, candidate, .true.)
	call check(sudoku, candidate, bug_flag, .true.)
enddo
end program main


subroutine initialize(sudoku, candidate)
implicit none
integer::sudoku(9,9), i, j
logical::candidate(9,9,9)
!--------------------------------------------
write(*,"(7X,9I2)") ((sudoku(i,j),i=1,9),j=1,9)

candidate=.true.
do j=1,9
do i=1,9
	if(sudoku(i,j)>0)then
		candidate(:,i,j)=.false.
		candidate(sudoku(i,j),i,j)=.true.
	endif
enddo
enddo
end subroutine initialize


subroutine solver(sudoku, candidate, recursive_flag)
!https://www.sudokuwiki.org/Strategy_Families
implicit none
integer::sudoku(9,9), i, j, ij, guess, no_update_times
integer::n_sudoku, n_sudoku0, n_candidate, n_candidate0
logical::candidate(9,9,9), recursive_flag, recursive_flag0
!--------------------------------------------
no_update_times=0
n_sudoku=count(sudoku>0)
n_sudoku0=n_sudoku
n_candidate0=count(candidate)
do while(n_sudoku<81)
	select case(no_update_times)
	case(0)
		call process(1, sudoku, candidate) !Basic
	case(1)
!		call process(2, sudoku, candidate) !Naked Pairs, Hidden Pairs
		call process(3, sudoku, candidate) !Naked Triples, Hidden Triples
	case(2:3) 
		if (no_update_times==2) recursive_flag0=.false. !*-Wing ...
		if (no_update_times==3) recursive_flag0=.true.
		do ij=1,81
			j=(ij-1)/9+1
			i=mod(ij-1,9)+1
			guess=count(candidate(:,i,j))
			if((no_update_times==2 .and. guess .eq. 2) .or. &
			   (no_update_times==3 .and. guess .ge. 2)) then
				call try_candidate(i, j, sudoku, candidate, recursive_flag0)
				if(count(sudoku>0) .ne. n_sudoku0) exit
			endif	
		enddo
	case(4)
		return
	end select
!--------------------------------------------	
	n_sudoku=count(sudoku>0)
	n_candidate=count(candidate)
	
	if(n_sudoku0==n_sudoku .and. n_candidate0==n_candidate) then
		no_update_times=no_update_times+1
		if((.not. recursive_flag) .and. (no_update_times .eq. 2)) return
	else 
		n_sudoku0=n_sudoku
		n_candidate0=n_candidate
		no_update_times=0
	endif
enddo
end subroutine solver


subroutine check(sudoku, candidate, bug_flag, final_check)
implicit none
integer::sudoku(9,9), i, j, k, m, i0, j0
logical::candidate(9,9,9), bug_flag, final_check, exit0
!--------------------------------------------
bug_flag=.false.
exit0=.false.
do k=1,9
	j0=(k-1)/3*3+1
	i0=mod(k-1,3)*3+1
do m=1,9
	if(count(sudoku(k,:) .eq. m)>1 .or. & 
	   count(sudoku(:,k) .eq. m)>1 .or. & 
	   count(sudoku(i0:i0+2,j0:j0+2) .eq. m)>1) then
		bug_flag=.true.
		exit0=.true.
		exit   
	endif
enddo
	if(exit0) exit
enddo
!--------------------------------------------
if (final_check) then
	write(*,*) "---"
	write(*,"(7X,9I2)") ((sudoku(i,j),i=1,9),j=1,9)
	if (any(sudoku==0)) bug_flag=.true.
	if (bug_flag) then
		print*,"failed"	
		if (exit0) then
			if(count(sudoku(k,:) .eq. m)>1) sudoku(k,:)=0
			if(count(sudoku(:,k) .eq. m)>1) sudoku(:,k)=0
			if(count(sudoku(i0:i0+2,j0:j0+2) .eq. m)>1) sudoku(i0:i0+2,j0:j0+2)=0
			write(*,"(7X,9I2)") ((sudoku(i,j),i=1,9),j=1,9)
		endif
	else
		print*,"solved"	
	endif
else
	do j=1,9
	do i=1,9
		if(count(candidate(:,i,j))==0) then
			bug_flag=.true.
			return
		endif
	enddo
	enddo
endif
end subroutine check


recursive subroutine process(n, sudoku, candidate)
implicit none
integer::sudoku(9,9), group(9), i_group, c9n, i_combination
integer::i, j, k, n, m, way, i_series(9), j_series(9)
logical::candidate(9,9,9), candidates(9), positions(9)
!--------------------------------------------
call combination_number(9, n, c9n)
do i_combination=1,c9n	
	call combination_group(n, group, i_combination)
do k=1,9
do way=1,3
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
	call ij_series2(k, way, i_series, j_series)
	forall(m=1:9) positions(m)=any(candidate(group(1:n), i_series(m), j_series(m)))
	if (count(positions) .eq. n) &
	forall(m=1:9, positions(m)) candidate(group(n+1:9), i_series(m), j_series(m))=.false.
!--------------------------------------------
enddo
enddo
enddo
!--------------------------------------------
do j=1,9
do i=1,9
	if(sudoku(i,j)==0 .and. count(candidate(:,i,j))==1) &
	sudoku(i,j)=findloc(candidate(:,i,j),.true.,1)
enddo
enddo
end subroutine process


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


subroutine ij_series2(k, way, i_series, j_series)
integer::k, way, i_series(9), j_series(9)
integer::indgen(9)=(/1,2,3,4,5,6,7,8,9/)
!--------------------------------------------
select case(way)
case(1)
	i_series=k; j_series=indgen
case(2)
	i_series=indgen; j_series=k
case(3)
	j_series=((k-1)/3)*3+1+(indgen-1)/3
	i_series=(mod(k-1,3))*3+1+mod(indgen-1,3)
end select
end subroutine ij_series2


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
	if (all(j .ne. group(1:k-1)))  then 
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


recursive subroutine try_candidate(i, j, sudoku, candidate, recursive_flag)
implicit none
integer::sudoku(9,9), sudoku_try(9,9), candidate_number(9), i, j, k
logical::candidate(9,9,9), candidate_try(9,9,9), bug_flag, recursive_flag
!--------------------------------------------
call findloc_all(candidate(:,i,j), candidate_number) 
do k=1, count(candidate(:,i,j))
	sudoku_try=sudoku
	candidate_try=candidate
	sudoku_try(i,j)=candidate_number(k)
	candidate_try(:,i,j)=.false.
	candidate_try(candidate_number(k),i,j)=.true.
	call solver(sudoku_try, candidate_try, recursive_flag)
	call check(sudoku_try, candidate_try, bug_flag, .false.)
	if (bug_flag) then 
		candidate(candidate_number(k),i,j)=.false.
		return
	else
		if(count(sudoku_try>0)==81)then
			sudoku=sudoku_try
			candidate=candidate_try
			return	
		endif		
	endif
enddo
end subroutine try_candidate
