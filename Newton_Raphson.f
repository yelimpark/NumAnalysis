program newton
   implicit none
   double precision, EXTERNAL :: f, f_prime
   double precision :: error, start, root, f_val
   
   start=1
   error = 1.0e-08
   call Newton_Raphson(f, f_prime, start, error, root)
   
   f_val = f(root)
   PRINT '(2(A,E15.6))',"x =", root
end program Newton

SUBROUTINE Newton_Raphson(f, f_prime, start, error, root)
   IMPLICIT NONE
   double precision, EXTERNAL :: f, f_prime
   double precision, INTENT(IN) :: start, error
   double precision, INTENT(INOUT) :: root
 
   double precision :: f_val, f_der

   root = start

   15 f_val = f(root)
   IF(ABS(f_val) <= error) THEN
     RETURN
   END IF
   f_der = f_prime(root)
      
   root = root - f_val/f_der
   goto 15
END SUBROUTINE Newton_Raphson

double precision function f(x)
	implicit none
	double precision::x
	f=x**3 -cos(x)
end function f

double precision function f_prime(x)
	implicit none
	double precision :: x
	f_prime =3*(x**2) + sin(x)
end function f_prime
