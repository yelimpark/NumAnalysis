program bisection
	implicit none
	double precision:: x1,x2,root,error,f
    	error=1e-08
	x1=0.0d0
	x2=1.0d0

	10 if (f(x1)*f(x2) < 0) then
		root=(x1+x2)/2.0
	end if

	if (f(x1)*f(root) < 0) then
		x2=root
	else
		x1=root
	end if

	if (abs(x2-x1) > error) goto 10

	write(*,*)"The root is",root
end program
