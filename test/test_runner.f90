module test_array_operations
    use ftools__constants, only: pr
contains

    subroutine run_tests
        call test_mask
        call test_sort
    end subroutine

    subroutine test_mask
        use ftools__array_operations, only: mask
        real(pr) :: x(5)
        real(pr), allocatable :: y(:)

        x = [2, 5, 1, 6, 3]
        y = x(mask(x < 4))
        if (maxval(abs(y - [2, 1, 3])) > 1e-10) error stop
    end subroutine

    subroutine test_sort
        use ftools__array_operations, only: sort
        real(pr) :: x(5)
        integer :: idx(5)
        
        x = [2, 5, 1, 6, 3]
        call sort(x, idx)
        print *, x
        print *, idx

    end subroutine
end module

module test_optionals
    use ftools__constants, only: pr
contains
    subroutine run_tests
        call test_optval
    end subroutine

    subroutine test_optval
        use ftools__optional, only: optval
        if (abs(f(2._pr) - 4) > 1e-10) error stop
        if (abs(f(2._pr, 3._pr) - 6) > 1e-10) error stop
    contains
        real(pr) function f(x, y)
            real(pr), intent(in) :: x
            real(pr), optional, intent(in) :: y
            real(pr) :: c
            
            c = optval(y, 2._pr)
            f = c*x
        end function
    end subroutine
end module

program main
    use test_array_operations, only: aop => run_tests
    use test_optionals, only: opt => run_tests
    implicit none

    call opt
    call aop
end program
