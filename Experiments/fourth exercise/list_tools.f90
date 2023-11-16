!!---------------------FourthExercise-------------------!!
!!---------------------ModuleSort-----------------------!!
module list_tools
  implicit none

contains
!!---------------------FunctionSort---------------------!!
  logical function is_sorted(arr, size)
    real, dimension(:), intent(in) :: arr
    integer, intent(in) :: size
    integer :: i

    do i = 1, size-1
      if (arr(i) > arr(i+1)) then
        is_sorted = .false.
        return
      end if
    end do
    is_sorted = .true.
  end function is_sorted

end module list_tools
