!!---------------------ModuleSort-----------------------!!
module list_tools
  implicit none

contains

!!---------------------FunctionSort---------------------!!
  logical function is_sorted(arr, size, dirorder)
    real, dimension(:), intent(in) :: arr
    integer,            intent(in) :: size
    character(*),       intent(in), optional :: dirorder
    integer :: i
    character(len=10) :: order
    order = "ascending"
    if (present(dirorder))then
      order=dirorder
    end if

    is_sorted = .true.
    if (trim(adjustl(order)) == 'ascending') then
      do i = 1, size-1
        if (arr(i) > arr(i+1)) then
          is_sorted = .false.
          exit
        end if
      end do
      !AscendSort = .true.
      !print *, "Is Ascending Sorted:", AscendSort
    elseif (trim(adjustl(order)) == 'descending') then
      do i = 1, size-1
        if (arr(i) < arr(i+1)) then
          is_sorted = .false.
          exit
        end if
      end do
      !DescendSort = .true.
      !print *, "Is Descending Sorted:", DescendSort
    else
      print *, "Invalid sorting order. Please specify 'ascending' or 'descending'."
      is_sorted = .false.
      return
    end if
  end function is_sorted
!!---------------------EndFunction-----------------------!!
end module list_tools

