PROGRAM sub_diagonal
implicit none
integer :: i,j,a,b,sum
integer dimension (100,100)
sum = 0

do i=1, 100
 do j=1, 100
  read (*,*) dimension (i,j)
 end do
end do

do a=1, 100
 do b=1, 100
  if ((a-b) .eq. 1) then
   sum = sum + dimension (a,b)
  end if
 end do
end do

print *, sum

end program sub_diagonal
