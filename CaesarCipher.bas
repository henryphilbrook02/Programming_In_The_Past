result$ = encrypt("This is a test string from Alan", 8)
Print result$

otherResult$ = decrypt(result$, 8)
Print otherResult$

Sleep 2

Call solve("HAL", 26)


Function encrypt$ (text$, shift)
    msg$ = UCase$(text$)
    cipher$ = ""

    For letter = 1 To Len(msg$) Step 1
        c = Asc(Mid$(msg$, letter, 1))
        If c <> Asc(" ") Then
            c = c + (shift Mod 26)
            If c > Asc("Z") Then
                c = c - 26
            End If
        End If
        cipher$ = cipher$ + Chr$(c)
    Next letter

    encrypt$ = cipher$

End Function

Function decrypt$ (text$, shift)
    msg$ = UCase$(text$)
    cipher$ = ""

    For letter = 1 To Len(msg$) Step 1
        c = Asc(Mid$(msg$, letter, 1))
        If c <> Asc(" ") Then
            c = c - (shift Mod 26)
            If c < Asc("A") Then
                c = c + 26
            End If
        End If
        cipher$ = cipher$ + Chr$(c)
    Next letter

    decrypt$ = cipher$

End Function


Sub solve (text$, maxVal)
    msg$ = UCase$(text$)
    cipher$ = ""

    For i = 0 To maxVal Step 1
        cipher$ = ""
        For letter = 1 To Len(msg$) Step 1
            c = Asc(Mid$(msg$, letter, 1))
            If c <> Asc(" ") Then
                c = c + (i Mod 26)
                If c > Asc("Z") Then
                    c = c - 26
                End If
            End If
            cipher$ = cipher$ + Chr$(c)
        Next letter
        Print "Caesar " + Str$(i) + ": " + cipher$
    Next i

End Sub
