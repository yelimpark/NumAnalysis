PROGRAM Prime_number
implicit none
integer :: num, count1, count2, tmp , root, i, j, k, m
count1 =0
count2 =0
num = 10
write(*,*) "start"

print *, "when divide into n-1"
print *, "2, "
do i=3, num
 do j=2, (i-1)
  tmp = mod(i,j)
  count1 = count1+1
  if (tmp .eq. 0) then
   exit
  end if
 end do
 write(*,*) i,", "
end do
write (*,*) "the number of division ", count1

print *, "when divide into n-1"
print *, "2, "
do k=3, num
 write(int(SQRT(real(k))),*) root
 do m=2, root
  tmp = mod(k,m)
  count2 = count2+1
  if (tmp .eq. 0) then
   exit
  end if
 end do
 write (*,*) k,", "
end do
write (*,*) "the number of division ", count2

end program Prime_number
