module lib
    implicit none

    contains
        pure double precision recursive function eval(func, x, lptr, rptr)
            implicit none
            character(*), intent(in):: func
            double precision, intent(in):: x
            integer, intent(in):: lptr, rptr

            integer:: depth, sptr, bptr
            logical:: reading_const
            double precision:: rvalue, fvalue
            character:: c, op

            depth = 0
            bptr = 0
            rvalue = 0
            op = '+'
            reading_const = .false.

            do sptr = lptr, rptr
                c = func(sptr:sptr)
                if (c == '(') then
                    if (depth == 0) then
                        bptr = sptr
                    depth = depth+1
                    end if

                else if (c == ')') then
                    depth = depth-1
                    if (depth == 0) then
                        fvalue = eval(func, x, bptr+1, sptr)
                        rvalue = apply(op, rvalue, fvalue)
                    end if

                else if (depth == 0) then
                    if ( (reading_const == .true.) .and. (is_alpha(c) == .false.) ) then
                        read(func(bptr:sptr-1), "(F1000.0)") fvalue
                        rvalue = apply(op, rvalue, fvalue)
                        reading_const = .false.
                        op = c

                    else if (is_operation(c) == .true.) then
                        op = c
                    else if (c == 'x') then
                        rvalue = apply(op, rvalue, x)
                    else
                        if (reading_const .ne. .true.) then
                            bptr = sptr
                            reading_const = .true.
                        end if
                    end if
                end if
            end do

            if (reading_const == .true.) then
                read(func(bptr:rptr-1), "(F1000.0)") fvalue
                rvalue = apply(op, rvalue, fvalue)
            end if
            eval = rvalue
        end function eval

        pure logical function is_alpha(c)
            implicit none
            character, intent(in):: c

            if (c == '0') then
                is_alpha = .true.
            else if (c == '1') then
                is_alpha = .true.
            else if (c == '2') then
                is_alpha = .true.
            else if (c == '3') then
                is_alpha = .true.
            else if (c == '4') then
                is_alpha = .true.
            else if (c == '5') then
                is_alpha = .true.
            else if (c == '6') then
                is_alpha = .true.
            else if (c == '7') then
                is_alpha = .true.
            else if (c == '8') then
                is_alpha = .true.
            else if (c == '9') then
                is_alpha = .true.
            else if (c == '.') then
                is_alpha = .true.
            else
                is_alpha = .false.
            end if
        end function is_alpha

        pure logical function is_operation(c)
            implicit none
            character, intent(in):: c

            if (c == '+') then
                is_operation = .true.
            else if (c == '-') then
                is_operation = .true.
            else if (c == '/') then
                is_operation = .true.
            else if (c == '*') then
                is_operation = .true.
            else if (c == '^') then
                is_operation = .true.
            else
                is_operation = .false.
            end if
        end function is_operation

        pure double precision function apply(op, rvalue, fvalue)
            character, intent(in):: op
            double precision, intent(in):: rvalue, fvalue

            if (op == '+') then
                apply = rvalue+fvalue
            else if (op == '-') then
                apply = rvalue-fvalue
            else if (op == '*') then
                apply = rvalue*fvalue
            else if (op == '/') then
                apply = rvalue/fvalue
            else if (op == '^') then
                apply = rvalue**fvalue
            end if

        end function apply

        double precision function integrate(func, istart, iend, dx)
            character(*), intent(in):: func
            double precision, intent(in):: istart, iend, dx

            double precision:: x, s, fend
            integer:: flen
            flen = len_trim(func)+1
            x = istart
            fend = iend-dx
            
            s = eval(func, x, 1, flen)/2

            do
                x = x + dx
                if (x > fend) exit

                s = s+eval(func, x, 1, flen)
            end do

            s = s+eval(func, iend, 1, flen)/2

            integrate = s*dx
        end function integrate
end module lib