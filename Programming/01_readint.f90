!!---------------------FirstExercise-------------------!!
!!---------------------FirstDefinition-----------------!!
program FirstExercise
  implicit none

!!---------------------ParametersInitialization---------!!
  integer, dimension(:), allocatable :: Data
  integer :: ArraySize, SumAll, ExpectedSum, i
  logical :: SumMatch

!!--------------------ReadArraySize---------------------!!
  read(5, *) ArraySize

!!--------------------ReadDataAllocateMemory------------!!
  allocate(Data(ArraySize))
  read(5, *) Data

!!--------------------ReadStoreExpectedSum--------------!!
  read(5, *) ExpectedSum

!!--------------------CalculateSumAll-------------------!!
  SumAll = 0
  do i = 1, ArraySize
    SumAll = SumAll + Data(i)
  end do

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
end program FirstExercise

