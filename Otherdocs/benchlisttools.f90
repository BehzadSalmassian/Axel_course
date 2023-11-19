!!---------------------ModuleSort-----------------------!!
module list_tools
  use list_types

  implicit none
  private
  public :: is_sorted, swap

  !!--------------------InterfacePart--------------------!!
interface is_sorted
  procedure::is_sorted_pair, is_sorted_real,is_sorted_Int
end interface is_sorted


contains
!!---------------------FunctionSort---------------------!!
  logical function is_sorted_pair(arr, size, dirorder)
    type(pair), dimension(:), intent(in) :: arr
    integer,            intent(in) :: size
    character(*),       intent(in), optional :: dirorder
    integer :: i
    character(len=10) :: order
    order = "ascending"
    if (present(dirorder))then
      order=dirorder
    end if

    is_sorted_pair = .true.
    if (trim(adjustl(order)) == 'ascending') then
      do i = 1, size-1
        if (arr(i)%value > arr(i+1)%value) then
          is_sorted_pair = .false.
          exit
        end if
      end do
    elseif (trim(adjustl(order)) == 'descending') then
      do i = 1, size-1
        if (arr(i)%value < arr(i+1)%value) then
          is_sorted_pair = .false.
          exit
        end if
      end do

    else
      print *, "Invalid sorting order. Please specify 'ascending' or 'descending'."
      is_sorted_pair = .false.
      return
    end if
  end function is_sorted_pair
!!---------------------EndFunction-----------------------!!
!!---------------------FunctionSort---------------------!!
  logical function is_sorted_real(arr, size, dirorder)
    real, dimension(:), intent(in) :: arr
    integer,            intent(in) :: size
    character(*),       intent(in), optional :: dirorder
    integer :: i
    character(len=10) :: order
    order = "ascending"
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
!!added this line
SUBROUTINE swap(dat,count)
  IMPLICIT NONE
  REAL, DIMENSION(:),INTENT(inout) :: dat
  INTEGER, INTENT(in) :: count
  REAL,DIMENSION(2) :: rval
  INTEGER :: i,num,i1,i2
  REAL :: tmp

  num = SIZE(dat,1)
  DO i=1,count
      ! pick two elements at random
      CALL RANDOM_NUMBER(rval)
      rval = rval*REAL(num)+0.5
      i1 = INT(rval(1))
      i2 = INT(rval(2))
      ! paranoia check to avoid out-of-bounds access
      IF ((i1 < 1) .OR. (i1 > num) .OR. (i2 < 1) .OR. (i2 > num)) CYCLE
      ! swap the elements
      tmp = dat(i1)
      dat(i1) = dat(i2)
      dat(i2) = tmp
  END DO
END SUBROUTINE swap
end module list_tools
