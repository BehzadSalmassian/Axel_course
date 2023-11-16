!!---------------------ModuleSort-----------------------!!
module list_tools
  implicit none

  private
  public :: is_sorted

  !!--------------------InterfacePart--------------------!!
interface is_sorted
  procedure::is_sorted_real,is_sorted_Int
end interface is_sorted


contains

!!---------------------FunctionSort---------------------!!
  logical function is_sorted_real(arr, size, dirorder)
    real, dimension(:), intent(in) :: arr
    integer,            intent(in) :: size
    character(*),       intent(in), optional :: dirorder
    integer :: i
    character(len=10) :: order
    order = "ascending"
!!--------------------PresentVheck----------------------!!
    if (present(dirorder))then
      order=dirorder
    end if

    is_sorted_real = .true.
    if (trim(adjustl(order)) == 'ascending') then
      do i = 1, size-1
        if (arr(i) > arr(i+1)) then
          is_sorted_real = .false.
          exit
        end if
      end do
    elseif (trim(adjustl(order)) == 'descending') then
      do i = 1, size-1
        if (arr(i) < arr(i+1)) then
          is_sorted_real = .false.
          exit
        end if
      end do

    else
      print *, "Invalid sorting order. Please specify 'ascending' or 'descending'."
      is_sorted_real = .false.
      return
    end if
  end function is_sorted_real
!!---------------------EndFunction-----------------------!!
!!---------------------IntFunction-----------------------!!

!!---------------------FunctionSort---------------------!!
  logical function is_sorted_Int(arr, size, dirorder)
    integer, dimension(:), intent(in) :: arr
    integer,            intent(in) :: size
    character(*),       intent(in), optional :: dirorder
    integer :: i
    character(len=10) :: order
    order = "ascending"
    
!!---------------------PresentCheck----------------------!!
    if (present(dirorder))then
      order=dirorder
    end if

    is_sorted_Int = .true.
    if (trim(adjustl(order)) == 'ascending') then
      do i = 1, size-1
        if (arr(i) > arr(i+1)) then
          is_sorted_Int = .false.
          exit
        end if
      end do
      !AscendSort = .true.
      !print *, "Is Ascending Sorted:", AscendSort
    elseif (trim(adjustl(order)) == 'descending') then
      do i = 1, size-1
        if (arr(i) < arr(i+1)) then
          is_sorted_Int = .false.
          exit
        end if
      end do

    else
      print *, "Invalid sorting order. Please specify 'ascending' or 'descending'."
      is_sorted_Int = .false.
      return
    end if
  end function is_sorted_Int
!!----------------------------------
end module list_tools

