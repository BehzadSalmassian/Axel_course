program LinkedListExample
  implicit none

  ! Define a linked list node type
  type Node
    integer :: data
    type(Node), pointer :: next => null()
  end type Node

  ! Declare variables
  type(Node), pointer :: head => null()
  type(Node), pointer :: current => null()
  integer :: i

  ! Create a linked list with nodes containing integers 1 to 5
  do i = 1, 5
    call appendNode(head, i)
  end do

  ! Traverse and print the linked list
  current => head
  do while (associated(current))
    print *, current%data
    current => current%next
  end do

contains

  ! Subroutine to append a new node to the linked list
  subroutine appendNode(listHead, value)
    type(Node), pointer :: listHead
    integer, intent(in) :: value

    type(Node), pointer :: newNode

    ! Allocate memory for the new node
    allocate(newNode)

    ! Initialize the new node
    newNode%data = value
    newNode%next => null()

    ! If the list is empty, set the head to the new node
    if (.not. associated(listHead)) then
      listHead => newNode
    else
      ! Otherwise, find the end of the list and append the new node
      current => listHead
      do while (associated(current%next))
        current => current%next
      end do
      current%next => newNode
    end if
  end subroutine appendNode

end program LinkedListExample
