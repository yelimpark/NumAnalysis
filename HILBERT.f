PROGRAM HILBERT
IMPLICIT NONE
integer :: comb, i, j, b1, b2, b3, n
REAL :: a(10,10)

n=10

DO  i = 1,n
 DO  j = 1,n
  a(i,j) = 1.0/(i+j-1.0)
 END DO
END DO

do i = 1,n
   do j = i,n
      b1 = comb(4+i-1, 4-j);
      b2 = comb(4+j-1, 4-i);
      b3 = comb(i+j-2, i-1);
      a(i,j) = (-1)**(i+j) * (i+j-1) * b1 * b2 * b3**2;
      a(j,i) = a(i,j);
   end do
end do

DO  i = 1,n
 DO  j = 1,n
  write(*,*) a(i,j)
 END DO
END DO

END PROGRAM HILBERT

integer function comb(n,r)
	implicit none
	integer :: n,r, fac,y
    y = n-r
    comb = fac(n)/(fac(r)*fac(n-r))
end function comb

integer function fac(num)
	implicit none
	real :: x
    integer:: i, num
    x = 1.0d0
    
    do i = 1,num
     x = x*i
    end do
    
	fac = x
end function fac
