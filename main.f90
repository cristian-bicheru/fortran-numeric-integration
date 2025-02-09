include 'lib.f90'

program Numeric_Integrator
    use lib
    implicit none

    double precision:: istart, iend, result
    character(len=1000):: func

    print *, "Bound 1:"
    read '(F20.0)', istart
    print *, "Bound 2:"
    read '(F20.0)', iend
    print *, "f(x):"
    read '(A)', func

    print *, "Result:"
    print *, integrate(func, istart, iend, (iend-istart)/1000.0)
end program Numeric_Integrator