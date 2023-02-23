module autodiff
    !! Adition of generic function gradient and hessian calculation subroutine
    !! added by Federico Benelli
    !! IPQA
    !! Universidad Nacional de Córdoba (UNC) - CONICET
    use constants, only: pr
    use hyperdual_mod

   !===========================================================================
   ! Generic Function interface 
   !---------------------------------------------------------------------------
   abstract interface
       function abs_f(X)
         import hyperdual
         type(hyperdual) :: X(:)
         type(hyperdual) :: abs_f
       end function
   end interface
   !============================================================================

contains

   subroutine generic_deriv(X, fun, f, df, df2)
      real(pr) :: X(:)
      procedure(abs_f) :: fun
      real(pr) :: f
      real(pr) :: df(size(X))
      real(pr) :: df2(size(X), size(X))

      type(hyperdual) :: X_dual(size(X))
      type(hyperdual) :: f_dual
      integer :: i, j, n

      n = size(X)

      do i = 1, n
         do j = i, n
            print *, i, j
            X_dual = X

            X_dual(i)%f1 = 1
            X_dual(j)%f2 = 1

            f_dual = fun(X_dual)

            df(i) = f_dual%f1
            df2(i, j) = f_dual%f12
         end do
      end do

      f = f_dual%f0
   end subroutine

end module autodiff