program palindrome
implicit none
logical :: is_palindrome
integer :: i
read (*,*) i
print *, is_palindrome(i)
end program palindrome

logical function is_palindrome(i)
 integer :: len
 character(50) :: str

 Write( str, * ) i
 str = adjustl(str)
 len = LEN_TRIM(str)

 do i=1, (len/2)
  if (str(i:i) /= str((len-i+1):(len-i+1))) then
   is_palindrome = .false.
   return
  end if
 end do
 is_palindrome = .true.
end function is_palindrome
