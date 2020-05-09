logical function is_alpha(c)
    implicit none
    character:: c

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
    end if

    is_alpha = .false.
end function is_alpha

double precision recursive function eval(func, x, lptr, rptr)
    implicit none
    character(len=*):: func
    double precision:: x, rvalue=0, fvalue
    integer:: lptr, rptr, depth=0, sptr, bptr=0
    logical:: reading_const = .false.
    character:: c, op='+'

    do sptr = lptr, rptr
        c = func(sptr)
        if (c == '(') then
            if (depth == 0) then
                bptr = sptr
            end if
            depth = depth+1

        else if (c == ')') then
            depth = depth-1
            if (depth == 0) then
                fvalue = eval(func, x, bptr, sptr)

                if (op == '+') then
                    rvalue = rvalue+fvalue
                else if (op == '-') then
                    rvalue = rvalue-fvalue
                else if (op == '*') then
                    rvalue = rvalue*fvalue
                else if (op == '/') then
                    rvalue = rvalue/fvalue
                end if
            end if

        else if (depth == 0) then
            if (reading_const .and. .not. is_alpha(c)) then
                read(func(:)(bptr:sptr-1), '(F20.0)')fvalue

                if (op == '+') then
                    rvalue = rvalue+fvalue
                else if (op == '-') then
                    rvalue = rvalue-fvalue
                else if (op == '*') then
                    rvalue = rvalue*fvalue
                else if (op == '/') then
                    rvalue = rvalue/fvalue
                end if

                reading_const = .false.
                op = c
            else if (c == '+') then
                op = '+'
            else if (c == '-') then
                op = '-'
            else if (c == '*') then
                op = '*'
            else if (c == '/') then
                op = '/'
            else if (c == 'x') then
                if (op == '+') then
                    rvalue = rvalue+x
                else if (op == '-') then
                    rvalue = rvalue-x
                else if (op == '*') then
                    rvalue = rvalue*x
                else if (op == '/') then
                    rvalue = rvalue/x
                end if
            else
                if (.not. reading_const) then
                    bptr = sptr
                    reading_const = .true.
                end if
            end if
        end if
    end do

    eval = rvalue
end function eval

program Numeric_Integrator
    double precision:: i_start=0, i_end=0
    character(len=1000):: func

    read '(F20.0)', i_start
    read '(F20.0)', i_end
    read '(A)', func

    print '(F20.0)', eval(func, 2, 0, 999)

    print *, "hello!"
end program Numeric_Integrator