module ftools__array_operations
   !! General array operations
   use ftools__constants, only: pr
   implicit none

contains

   subroutine append_2d(array, values)
      real(pr), allocatable, intent(in out) :: array(:, :)
      real(pr), allocatable, intent(in) :: values(:)
      real(pr), allocatable :: tmp_array(:, :)

      integer :: ni, nj, sh(2)

      sh = shape(array)
      ni = sh(1)
      nj = sh(2)

      allocate (tmp_array(ni + 1, nj))

      tmp_array(:ni, :) = array
      tmp_array(ni + 1, :) = values

      deallocate (array)
      array = tmp_array
   end subroutine append_2d

   pure function get_point_index(x, y, x_point, y_point)
      !! Given two sets of points of the same dimension (x and y values)
      !! get the index of the first closest number based on the maximum
      !! difference between two consecutives points
      !!
      !! @note This could be improved to ask which point (first, second, etc)
      !! to get.
      real(pr), intent(in) :: x(:) !! x values
      real(pr), intent(in) :: y(size(x)) !! y values
      real(pr), intent(in) :: x_point !! Desired point x value
      real(pr), intent(in) :: y_point !! Desired point y value
      integer :: get_point_index !! Index of the first closest point

      real(pr) :: delta_x(size(x)-1), delta_y(size(y)-1)

      integer :: i

      delta_x = maxval(abs(diff(x)))
      delta_y = maxval(abs(diff(y)))

      do i=lbound(x, dim=1), ubound(x, dim=1) - 1
         if (abs(x(i) - x_point) < delta_x(i) .and. abs(y(i) - y_point) < delta_y(i)) then
            get_point_index=i
            exit
         end if
      end do
   end function get_point_index

   pure function diff(array)
      !! Obtain the delta values of an array by doing:
      !!
      !! \[ diff_i = array_i - array_{i-1} \]
      !!
      real(pr), intent(in) :: array(:) !! Array to diff
      real(pr) :: diff(size(array)) !! Diff

      integer :: n
      n = size(array)

      diff(2:n) = array(2:n) - array(1:n-1)
   end function diff

   subroutine sort(array, sorted_index)
      real(pr), intent(in out) :: array(:)
      integer, optional, intent(out) :: sorted_index(:)

      integer :: i
      logical :: sorted
      sorted_index = [(i, i=lbound(array,dim=1),ubound(array,dim=1))]


      do
         sorted = .true.
         do i=lbound(array, dim=1), ubound(array, dim=1)-1
            if (array(i+1) < array(i)) then
               sorted = .false.
               array(i:i+1) = array(i+1:i:-1)
               if(present(sorted_index)) sorted_index(i:i+1) = sorted_index(i+1:i:-1)
            end if
         end do
         if (sorted) exit
      end do
   end subroutine

   pure function mask(bool_array)
      !! Receives a boolean array and returns an array with the index numbers
      !! where they're true. This can be used to mask an array based on an
      !! filter
      !!
      !! @note The boolean array could be an inline expression like
      !! ```fortran
      !! x = [1, 4, 6, 11, 2, 15]
      !! mask(x < 10)
      !! ```
      !!
      !! ```
      !! >> [4, 6]
      !! ```
      logical, intent(in) :: bool_array(:) !! Array of booleans
      integer, allocatable :: mask(:) !! Output array of indexes

      integer :: i

      allocate(mask(0))
      do i = 1,size(bool_array)
         if (bool_array(i)) then
            mask = [mask, i]
         end if
      end do
   end function mask
end module ftools__array_operations
