! This is the main program.
program MAIN
    ! let's try to call a function.
    implicit none
    character (len = 31) :: shifted, normal
    shifted = encrypt("This is a test string from Alan", 8)  ! We have to declare the return type of the function.
    print*, shifted
    normal = decrypt(shifted, 8)
    print*, normal
    
    call solve("HAL", 26)

contains ! I'll explain why I did this in my write up but I hate myself for doing this 

    
    function encrypt(text, shift) result(cipher)
        implicit none
        integer, intent(in):: shift
        integer :: i, c, f
        
        character (len = *), intent(in) :: text
        character (len = len(text)) :: cipher, msg

        msg = to_upper(text)
        cipher = "" ! I need this or else the output is trashed for some reason
        f = 0 !This is the space flag and will be turned to 1 if a space has occurred (this will not work for sentences with more than once space in a row)
        do i = 1, len(msg)
            c = iachar(msg(i:i))
            if(c /= iachar(" ")) then
                c = c + (mod(shift, 26))
                if(c > iachar("Z")) then
                    c = c - 26
                end if
            end if
            if(c == iachar(" ")) then ! the reason i need this is becuase i need to trim the variable and i cant have a space at the end without it being trimmed off
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
        cipher = "" ! I need this or else the output is trashed for some reason
        f = 0 !This is the space flag and will be turned to 1 if a space has occurred (this will not work for sentences with more than once space in a row)
        do i = 1, len(msg)
            c = iachar(msg(i:i))
            if(c /= iachar(" ")) then
                c = c - (mod(shift, 26))
                if(c < iachar("A")) then
                    c = c + 26
                end if
            end if
            if(c == iachar(" ")) then ! the reason i need this is becuase i need to trim the variable and i cant have a space at the end without it being trimmed off
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
        cipher = "" ! I need this or else the output is trashed for some reason
        f = 0 !This is the space flag and will be turned to 1 if a space has occured (this will not work for sentances with more than once space 
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
                if(c == iachar(" ")) then ! the reason i need this is becuase i need to trim the variable and i cant have a space at the end without it being trimmed off
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
