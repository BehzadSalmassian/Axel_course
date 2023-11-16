
!!---------------------FirstExercise-------------------!!
!!---------------------FirstDefinition-----------------!!
program FirstExercise
  implicit none

!!---------------------ParametersInitialization---------!!
  real, dimension(:), allocatable :: Data
  real :: SumAll, ExpectedSum
  integer :: ArraySize, i
  character(len=50) :: fileName
  logical :: SumMatch

!!--------------------FileName--------------------------!!
  fileName = 'd2_1.dat'

!!--------------------OpenFileReadSize------------------!!
  open(unit=10, file=fileName, status='old', action='read')
  read(10, *) ArraySize

!!--------------------ReadDataAllocateMemory------------!!
  allocate(Data(ArraySize))
  read(10, *) Data

!!--------------------ReadStoreExpectedSum--------------!!
  read(10, *) ExpectedSum

!!--------------------CloseFile-------------------------!!
  close(10)

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
