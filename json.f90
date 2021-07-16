module json

implicit none

integer, parameter :: iunit = 42
logical  :: has_previous

contains

subroutine open_dbg_out(filename)
  character(len=*), intent(in) :: filename

  open(unit=iunit, file=trim(filename), status="unknown")
  write(iunit, '(A)', advance="no") "{"
  has_previous = .false.
end subroutine open_dbg_out

subroutine close_dbg_out
  write(iunit, '(A)', advance="no") "}"
  close(iunit)
end subroutine close_dbg_out

subroutine add_element(name, content)
  character(len=*), intent(in) :: name
  character(len=*), intent(in) :: content

  if (has_previous) then
    write(iunit, '(A)', advance="no") ','
  end if
  write(iunit, '(4A)', advance="no") '"',trim(adjustl(name)),'":',trim(adjustl(content))

  has_previous = .true.
end subroutine add_element

subroutine add_array_1d(name, n, content)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n
  character(len=*), dimension(n), intent(in) :: content

  integer :: i

  if (has_previous) then
    write(iunit, '(A)', advance="no") ','
  end if

  write(iunit, '(3A)', advance="no") '"',trim(adjustl(name)),'":['
  do i = 1, n-1
    write(iunit, '(2A)', advance="no") trim(adjustl(content(i))),','
  end do
  write(iunit, '(2A)', advance="no") trim(adjustl(content(n))),']'

  has_previous = .true.
end subroutine add_array_1d

subroutine add_array_2d(name, n2, n1, content)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n2, n1
  character(len=*), dimension(n2,n1), intent(in) :: content

  integer :: i, j

  if (has_previous) then
    write(iunit, '(A)', advance="no") ','
  end if

  write(iunit, '(3A)', advance="no") '"',trim(adjustl(name)),'":['
  do i = 1, n1
    write(iunit, '(A)', advance="no") '['
    do j = 1, n2
      write(iunit, '(2A)', advance="no") trim(adjustl(content(j, i)))
      if (j .lt. n2) then
        write(iunit, '(2A)', advance="no") ','
      end if
    end do
    write(iunit, '(2A)', advance="no") ']'
    if (i .lt. n1) then
      write(iunit, '(2A)', advance="no") ','
    end if
  end do
  write(iunit, '(2A)', advance="no") ']'

  has_previous = .true.
end subroutine add_array_2d

subroutine add_int(name, val)
  character(len=*), intent(in) :: name
  integer         , intent(in) :: val

  character(len=64) :: temp

  write(temp, *) val
  call add_element(name, temp)
end subroutine add_int

subroutine add_int_1d(name, n, arr)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n
  integer, dimension(n), intent(in) :: arr

  character(len=64), dimension(:), allocatable :: temp
  integer :: i

  allocate(temp(n))
  do i = 1, n
    write(temp(i), *) arr(i)
  end do
  call add_array_1d(name, n, temp)

  deallocate(temp)
end subroutine add_int_1d

subroutine add_int_2d(name, n2, n1, arr)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n2, n1
  integer, dimension(n2, n1), intent(in) :: arr

  character(len=64), dimension(:,:), allocatable :: temp
  integer :: i, j

  allocate(temp(n2, n1))
  do i = 1, n1
    do j = 1, n2
      write(temp(j,i), *) arr(j,i)
    end do
  end do
  call add_array_2d(name, n2, n1, temp)

  deallocate(temp)
end subroutine add_int_2d


end module json
