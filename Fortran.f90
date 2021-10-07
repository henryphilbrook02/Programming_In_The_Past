! This is the main program.
program MAIN
    implicit none
    character (len = 31) :: shifted, normal
    shifted = encrypt("This is a test string from Alan", 8) 
    print*, shifted
    normal = decrypt(shifted, 8)
    print*, normal
    
    call solve("HAL", 26)

contains 
    
    function encrypt(text, shift) result(cipher)
        implicit none
        integer, intent(in):: shift
        integer :: i, c, f
        
        character (len = *), intent(in) :: text
        character (len = len(text)) :: cipher, msg

        msg = to_upper(text)
        cipher = ""
        f = 0
        do i = 1, len(msg)
            c = iachar(msg(i:i))
            if(c /= iachar(" ")) then
                c = c + (mod(shift, 26))
                if(c > iachar("Z")) then
                    c = c - 26
                end if
            end if
            if(c == iachar(" ")) then
                f = 1
            else if (f == 1) then  
                cipher = trim(cipher)//" "//char(c)
                f = 0
            else
                cipher = trim(cipher) // char(c)
            end if
        end do
    end function
    

    function decrypt(text, shift) result(cipher)
        implicit none
        integer, intent(in):: shift
        integer :: i, c, f
        
        character (len = *), intent(in) :: text
        character (len = len(text)) :: cipher, msg

        msg = to_upper(text)
        cipher = "" 
        f = 0 
        do i = 1, len(msg)
            c = iachar(msg(i:i))
            if(c /= iachar(" ")) then
                c = c - (mod(shift, 26))
                if(c < iachar("A")) then
                    c = c + 26
                end if
            end if
            if(c == iachar(" ")) then 
                f = 1
            else if (f == 1) then  
                cipher = trim(cipher)//" "//char(c)
                f = 0
            else
                cipher = trim(cipher) // char(c)
            end if
        end do
    end function

    
    subroutine solve(text, maxVal)
        implicit none
        integer, intent(in):: maxVal
        integer :: i, c, f, n
        
        character (len = *), intent(in) :: text
        character (len = len(text)) :: cipher, msg

        msg = to_upper(text)
        cipher = "" 
        f = 0  
        do n = 0, maxVal
            cipher = "" 
            do i = 1, len(msg)
                c = iachar(msg(i:i))
                if(c /= iachar(" ")) then
                    c = c + (mod(n, 26))
                    if(c > iachar("Z")) then
                        c = c - 26
                    end if
                end if
                if(c == iachar(" ")) then
                    f = 1
                else if (f == 1) then  
                    cipher = trim(cipher)//" "//char(c)
                    f = 0
                else
                    cipher = trim(cipher) // char(c)
                end if
            end do
            print*, "Caesar " , n , ": " // cipher
        end do
    
    end subroutine

    function to_upper(strIn) result(strOut)
    ! code from:https://stackoverflow.com/questions/10759375/how-can-i-write-a-to-upper-or-to-lower-function-in-f90
    ! Original author: jvriesem
    ! Did not want to implement the to upper myself

         implicit none

         character(len=*) :: strIn
         character(len=len(strIn)) :: strOut
         integer :: i,j

         do i = 1, len(strIn)
              j = iachar(strIn(i:i))
              if (j>= iachar("a") .and. j<=iachar("z") ) then
                   strOut(i:i) = achar(iachar(strIn(i:i))-32)
              else
                   strOut(i:i) = strIn(i:i)
              end if
         end do

    end function to_upper

end program
