module ftools__optional
    use ftools__constants, only: pr

contains

    function optval(opt, def) result(val)
        real(pr), optional, intent(in) :: opt !! Possible optional
        real(pr), intent(in) :: def !! Default value if opt is not present
        real(pr) :: val

        if (present(opt)) then
            val = opt
        else
            val = def
        end if
    end function
end module