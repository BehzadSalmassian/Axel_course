!!---------------------FirstExercise-------------------!!
!!---------------------FirstDefinition-----------------!!
program FirstExercise
  implicit none

!!---------------------ParametersInitialization---------!!
  integer, dimension(:), allocatable :: Data
  integer :: ArraySize, SumAll, ExpectedSum, i
  character(len=50) :: fileName
  logical :: SumMatch, AssendSort

!!--------------------FileName--------------------------!!
  fileName = 'd1_1.dat'

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

!!--------------------CheckAssendSort----------------!!
  AssendSort = .true.
  do i = 1, ArraySize-1
    if (Data(i) > Data(i+1)) then
      AssendSort = .false.
      exit
    end if
  end do

!!--------------------CalculateSumAll-------------------!!
  SumAll = sum(Data)

!!--------------------CheckSumMatch---------------------!!
  SumMatch = (SumAll == ExpectedSum)

!!--------------------OutputResults---------------------!!
  print *, "Size of the array:", ArraySize
  print *, "Check Sum Match:", SumMatch
  print *, "Computed and Expected Sum:", SumAll
  print *, "Is Ascending Sorted:", AssendSort

!!--------------------IfPrint---------------------------!!
  if (.not. SumMatch) then
    print *, "Expected Sum:", ExpectedSum
    print *, "Computed Sum:", SumAll
  end if

!!--------------------EndCode---------------------------!!
end program FirstExercise
