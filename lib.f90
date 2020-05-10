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

        subroutine solve_consts(evals, cs, tau, i)
            double precision, intent(in):: tau
            double precision, dimension(4):: evals, cs
            integer, intent(in):: i

            integer:: i1, i2, i3

            i1 = i+1
            if (i1 > 4) i1 = i1 - 4
            i2 = i+2
            if (i2 > 4) i2 = i2 - 4
            i3 = i+3
            if (i3 > 4) i3 = i3 - 4

            cs(1) = evals(i1)
            cs(2) = tau*(evals(i2)-evals(i))
            cs(3) = 3*(evals(i2)-evals(i1))-tau*(evals(i3)-evals(i1)+2*(evals(i3)-evals(i)))
            cs(4) = -2*(evals(i2)-evals(i1))+tau*(evals(i3)-evals(i1)+evals(i2)-evals(i))
        end subroutine solve_consts

        double precision function integrate(func, istart, iend, dx, tau)
            character(*), intent(in):: func
            double precision, intent(in):: istart, iend, dx, tau

            double precision:: x, s, fend
            double precision, dimension(4):: evals, cs
            integer:: flen, i
            flen = len_trim(func)
            x = istart
            fend = iend+dx
            
            do i = -1, 2
                evals(i+1) = eval(func, istart+i*dx, 1, flen)
            end do

            i = 0
            call solve_consts(evals, cs, tau, 0)
            s = cs(4)/4+cs(3)/3+cs(2)/2+cs(1)

            do
                i = i + 1
                if (i > 4) i = 1

                evals(i) = eval(func, x, 1, flen)
                call solve_consts(evals, cs, tau, 0)
                s = s + cs(4)/4+cs(3)/3+cs(2)/2+cs(1)

                x = x + dx
                if (x > fend) exit
            end do

            integrate = s*dx
        end function integrate
end module lib