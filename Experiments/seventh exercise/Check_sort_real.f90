
!!---------------------MainProgram---------------------!!
program FourthExercise
  use list_tools
  implicit none

!!---------------------DeriveType-----------------------!!
type :: pair
  integer :: key
  real:: value
end type
  type( pair ), dimension(:), allocatable :: list


!!---------------------ParametersInitialization---------!!
  real :: SumAll, ExpectedSum
  integer :: ArraySize, i
  logical :: SumMatch, AscendSort

!!--------------------ReadArraySize---------------------!!
  read(5, *) ArraySize

!!--------------------ReadDataAllocateMemory------------!!
!!  allocate(list(ArraySize))

  allocate(list(ArraySize))
  read(5, *) list

!!--------------------ReadStoreExpectedSum--------------!!
  read(5, *) ExpectedSum

!!--------------------CalculateSumAll-------------------!!
  SumAll = sum(list%value)

!!--------------------CheckAscendSort---------------------!!
  AscendSort = is_sorted(list%value, ArraySize)

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
end program FourthExercise
