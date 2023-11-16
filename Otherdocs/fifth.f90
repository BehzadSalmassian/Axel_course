

!!---------------------MainProgram---------------------!!
program FourthExercise
  use is_sorted_module
  implicit none

!!---------------------ParametersInitialization---------!!
      character(len=20) :: SortingOrder
  real, dimension(:), allocatable :: Data
  real :: SumAll, ExpectedSum
  integer :: ArraySize, i
  logical :: SumMatch, sorted

!!--------------------ReadArraySize---------------------!!
  read(5, *) ArraySize

!!--------------------ReadDataAllocateMemory------------!!
  allocate(Data(ArraySize))
  read(5, *) Data

!!--------------------ReadStoreExpectedSum--------------!!
  read(5, *) ExpectedSum

!!--------------------CheckAscendDesendSort---------------------!!
  !print *, "Do you want to sort in 'ascending' or 'descending' order?"
  !read(5, *) SortingOrder
  !:x
  !SortingOrder = "ascending"
  SortingOrder = "ascending"
  Sorted = is_sorted(Data, ArraySize, trim(SortingOrder))
  !Sorted = is_sorted(Data, ArraySize)
  print*,"Is ",SortingOrder," Sorted?", Sorted


!!--------------------CalculateSumAll-------------------!!
  SumAll = sum(Data)

!!--------------------CheckSumMatch---------------------!!
  SumMatch = (SumAll == ExpectedSum)

!!--------------------OutputResults---------------------!!
  print *, "Size of the array:", ArraySize
  print *, "Check Sum Match:", SumMatch
  print *, "Computed and Expected Sum:", SumAll

!!--------------------IfPrint---------------------------!!
  if (.not. SumMatch) then
    print *, "Expected Sum:", ExpectedSum
    print *, "Computed Sum:", SumAll
  end if

!!--------------------EndCode---------------------------!!
end program FourthExercise
