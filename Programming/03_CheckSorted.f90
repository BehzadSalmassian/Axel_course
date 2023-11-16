!!---------------------ThirExercise---------------------!!
!!---------------------ModuleSort-----------------------!!
module list_tools
  implicit none

contains
!!---------------------FunctionSort---------------------!!
  logical function is_sorted(arr, size)
    integer, dimension(:), intent(in) :: arr
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

!!---------------------MainProgram---------------------!!
program ThirdExercise
  use list_tools
  implicit none

!!---------------------ParametersInitialization---------!!
  integer, dimension(:), allocatable :: Data
  integer :: SumAll, ExpectedSum
  integer :: ArraySize, i
  logical :: SumMatch, AscendSort

!!--------------------ReadArraySize---------------------!!
  read(5, *) ArraySize

!!--------------------ReadDataAllocateMemory------------!!
  allocate(Data(ArraySize))
  read(5, *) Data

!!--------------------ReadStoreExpectedSum--------------!!
  read(5, *) ExpectedSum

!!--------------------CalculateSumAll-------------------!!
  SumAll = sum(Data)

!!--------------------CheckAscendSort---------------------!!
  AscendSort = is_sorted(Data, ArraySize)

!!--------------------CheckSumMatch---------------------!!
  SumMatch = (SumAll == ExpectedSum)

!!--------------------OutputResults---------------------!!
  print *, "Size of the array:", ArraySize
  print *, "Check Sum Match:", SumMatch
  print *, "Computed and Expected Sum:", SumAll
  print *, "Is Ascending Sorted:", AscendSort

!!--------------------IfPrint---------------------------!!
  if (.not. SumMatch) then
    print *, "Expected Sum:", ExpectedSum
    print *, "Computed Sum:", SumAll
  end if

!!--------------------EndCode---------------------------!!
end program ThirdExercise

