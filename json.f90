module json

implicit none

integer, parameter :: dbg_unit = 42
integer, parameter, private :: dp = selected_real_kind(15, 300)
logical  :: has_previous

contains

!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

subroutine open_dbg_out(filename)
  character(len=*), intent(in) :: filename

  open(unit=dbg_unit, file=trim(filename), status="unknown")
  write(dbg_unit, '(A)', advance="no") "{"
  has_previous = .false.
end subroutine open_dbg_out

subroutine close_dbg_out
  write(dbg_unit, '(A)', advance="no") "}"
  close(dbg_unit)
end subroutine close_dbg_out

!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

subroutine add_element(name, content)
  character(len=*), intent(in) :: name
  character(len=*), intent(in) :: content

  if (has_previous) then
    write(dbg_unit, '(A)', advance="no") ','
  end if
  write(dbg_unit, '(4A)', advance="no") '"',trim(adjustl(name)),'":',trim(adjustl(content))

  has_previous = .true.
end subroutine add_element

subroutine add_array_1d(name, n, content)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n
  character(len=*), dimension(n), intent(in) :: content

  integer :: i

  if (has_previous) then
    write(dbg_unit, '(A)', advance="no") ','
  end if

  write(dbg_unit, '(3A)', advance="no") '"',trim(adjustl(name)),'":['
  do i = 1, n-1
    write(dbg_unit, '(2A)', advance="no") trim(adjustl(content(i))),','
  end do
  write(dbg_unit, '(2A)', advance="no") trim(adjustl(content(n))),']'

  has_previous = .true.
end subroutine add_array_1d

subroutine add_array_2d(name, n1, n2, content)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2
  character(len=*), dimension(n1,n2), intent(in) :: content

  integer :: i, j

  if (has_previous) then
    write(dbg_unit, '(A)', advance="no") ','
  end if

  write(dbg_unit, '(3A)', advance="no") '"',trim(adjustl(name)),'":['
  do i = 1, n1
    write(dbg_unit, '(A)', advance="no") '['
    do j = 1, n2
      write(dbg_unit, '(2A)', advance="no") trim(adjustl(content(i,j)))
      if (j .lt. n2) then
        write(dbg_unit, '(2A)', advance="no") ','
      end if
    end do
    write(dbg_unit, '(2A)', advance="no") ']'
    if (i .lt. n1) then
      write(dbg_unit, '(2A)', advance="no") ','
    end if
  end do
  write(dbg_unit, '(2A)', advance="no") ']'

  has_previous = .true.
end subroutine add_array_2d

subroutine add_array_3d(name, n1, n2, n3, content)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3
  character(len=*), dimension(n1,n2,n3), intent(in) :: content

  integer :: i, j, k

  if (has_previous) then
    write(dbg_unit, '(A)', advance="no") ','
  end if

  write(dbg_unit, '(3A)', advance="no") '"',trim(adjustl(name)),'":['
  do i = 1, n1
    write(dbg_unit, '(A)', advance="no") '['
    do j = 1, n2
      write(dbg_unit, '(A)', advance="no") '['
      do k = 1, n3
        write(dbg_unit, '(2A)', advance="no") trim(adjustl(content(i,j,k)))
        if (k .lt. n3) then
          write(dbg_unit, '(2A)', advance="no") ','
        end if
      end do
      write(dbg_unit, '(2A)', advance="no") ']'
      if (j .lt. n2) then
        write(dbg_unit, '(2A)', advance="no") ','
      end if
    end do
    write(dbg_unit, '(2A)', advance="no") ']'
    if (i .lt. n1) then
      write(dbg_unit, '(2A)', advance="no") ','
    end if
  end do
  write(dbg_unit, '(2A)', advance="no") ']'

  has_previous = .true.
end subroutine add_array_3d

subroutine add_array_4d(name, n1, n2, n3, n4, content)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3, n4
  character(len=*), dimension(n1,n2,n3,n4), intent(in) :: content

  integer :: i, j, k, l

  if (has_previous) then
    write(dbg_unit, '(A)', advance="no") ','
  end if

  write(dbg_unit, '(3A)', advance="no") '"',trim(adjustl(name)),'":['
  do i = 1, n1
    write(dbg_unit, '(A)', advance="no") '['
    do j = 1, n2
      write(dbg_unit, '(A)', advance="no") '['
      do k = 1, n3
        write(dbg_unit, '(A)', advance="no") '['
        do l = 1, n4
          write(dbg_unit, '(2A)', advance="no") trim(adjustl(content(i,j,k,l)))
          if (l .lt. n4) then
            write(dbg_unit, '(2A)', advance="no") ','
          end if
        end do
        write(dbg_unit, '(2A)', advance="no") ']'
        if (k .lt. n3) then
          write(dbg_unit, '(2A)', advance="no") ','
        end if
      end do
      write(dbg_unit, '(2A)', advance="no") ']'
      if (j .lt. n2) then
        write(dbg_unit, '(2A)', advance="no") ','
      end if
    end do
    write(dbg_unit, '(2A)', advance="no") ']'
    if (i .lt. n1) then
      write(dbg_unit, '(2A)', advance="no") ','
    end if
  end do
  write(dbg_unit, '(2A)', advance="no") ']'

  has_previous = .true.
end subroutine add_array_4d

subroutine add_array_5d(name, n1, n2, n3, n4, n5, content)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3, n4, n5
  character(len=*), dimension(n1,n2,n3,n4,n5), intent(in) :: content

  integer :: i, j, k, l, m

  if (has_previous) then
    write(dbg_unit, '(A)', advance="no") ','
  end if

  write(dbg_unit, '(3A)', advance="no") '"',trim(adjustl(name)),'":['
  do i = 1, n1
    write(dbg_unit, '(A)', advance="no") '['
    do j = 1, n2
      write(dbg_unit, '(A)', advance="no") '['
      do k = 1, n3
        write(dbg_unit, '(A)', advance="no") '['
        do l = 1, n4
          write(dbg_unit, '(A)', advance="no") '['
          do m = 1, n5
            write(dbg_unit, '(2A)', advance="no") trim(adjustl(content(i,j,k,l,m)))
            if (m .lt. n5) then
              write(dbg_unit, '(2A)', advance="no") ','
            end if
          end do
          write(dbg_unit, '(2A)', advance="no") ']'
          if (l .lt. n4) then
            write(dbg_unit, '(2A)', advance="no") ','
          end if
        end do
        write(dbg_unit, '(2A)', advance="no") ']'
        if (k .lt. n3) then
          write(dbg_unit, '(2A)', advance="no") ','
        end if
      end do
      write(dbg_unit, '(2A)', advance="no") ']'
      if (j .lt. n2) then
        write(dbg_unit, '(2A)', advance="no") ','
      end if
    end do
    write(dbg_unit, '(2A)', advance="no") ']'
    if (i .lt. n1) then
      write(dbg_unit, '(2A)', advance="no") ','
    end if
  end do
  write(dbg_unit, '(2A)', advance="no") ']'

  has_previous = .true.
end subroutine add_array_5d

!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

subroutine add_null(name)
  character(len=*), intent(in) :: name

  call add_element(name, "null")
end subroutine add_null

subroutine add_null_1d(name)
  character(len=*), intent(in) :: name

  call add_array_1d(name, 1, "null")
end subroutine add_null_1d

subroutine add_null_2d(name)
  character(len=*), intent(in) :: name

  call add_array_2d(name, 1, 1, "null")
end subroutine add_null_2d

subroutine add_null_3d(name)
  character(len=*), intent(in) :: name

  call add_array_3d(name, 1, 1, 1, "null")
end subroutine add_null_3d

subroutine add_null_4d(name)
  character(len=*), intent(in) :: name

  call add_array_4d(name, 1, 1, 1, 1, "null")
end subroutine add_null_4d

subroutine add_null_5d(name)
  character(len=*), intent(in) :: name

  call add_array_5d(name, 1, 1, 1, 1, 1, "null")
end subroutine add_null_5d

!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

subroutine add_none(name)
  character(len=*), intent(in) :: name

  call add_element(name, "")
end subroutine add_none

subroutine add_none_1d(name)
  character(len=*), intent(in) :: name

  call add_array_1d(name, 1, "")
end subroutine add_none_1d

subroutine add_none_2d(name)
  character(len=*), intent(in) :: name

  call add_array_2d(name, 1, 1, "")
end subroutine add_none_2d

subroutine add_none_3d(name)
  character(len=*), intent(in) :: name

  call add_array_3d(name, 1, 1, 1, "")
end subroutine add_none_3d

subroutine add_none_4d(name)
  character(len=*), intent(in) :: name

  call add_array_4d(name, 1, 1, 1, 1, "")
end subroutine add_none_4d

subroutine add_none_5d(name)
  character(len=*), intent(in) :: name

  call add_array_5d(name, 1, 1, 1, 1, 1, "")
end subroutine add_none_5d

!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

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

subroutine add_int_2d(name, n1, n2, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2
  integer, dimension(n1, n2), intent(in) :: arr
  integer, dimension(2), intent(in), optional :: order

  character(len=64), dimension(:,:), allocatable :: temp
  integer :: i, j

  allocate(temp(n1, n2))
  do i = 1, n1
    do j = 1, n2
      write(temp(i,j), *) arr(i,j)
    end do
  end do
  if (present(order)) then
    call add_array_2d(name, n1, n2, &
           reshape(temp, (/ n1, n2 /), order=order))
  else
    call add_array_2d(name, n1, n2, temp)
  end if

  deallocate(temp)
end subroutine add_int_2d

subroutine add_int_3d(name, n1, n2, n3, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3
  integer, dimension(n1, n2, n3), intent(in) :: arr
  integer, dimension(3), intent(in), optional :: order

  character(len=64), dimension(:,:,:), allocatable :: temp
  integer :: i, j, k

  allocate(temp(n1,n2,n3))
  do i = 1, n1
    do j = 1, n2
      do k = 1, n3
        write(temp(i,j,k), *) arr(i,j,k)
      end do
    end do
  end do
  if (present(order)) then
    call add_array_3d(name, n1, n2, n3, &
           reshape(temp, (/ n1, n2, n3 /), order=order))
  else
    call add_array_3d(name, n1, n2, n3, temp)
  end if

  deallocate(temp)
end subroutine add_int_3d

subroutine add_int_4d(name, n1, n2, n3, n4, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3, n4
  integer, dimension(n1,n2,n3,n4), intent(in) :: arr
  integer, dimension(4), intent(in), optional :: order

  character(len=64), dimension(:,:,:,:), allocatable :: temp
  integer :: i, j, k, l

  allocate(temp(n1,n2,n3,n4))
  do i = 1, n1
    do j = 1, n2
      do k = 1, n3
        do l = 1, n4
          write(temp(i,j,k,l), *) arr(i,j,k,l)
        end do
      end do
    end do
  end do
  if (present(order)) then
    call add_array_4d(name, n1, n2, n3, n4, &
           reshape(temp, (/ n1, n2, n3, n4 /), order=order))
  else
    call add_array_4d(name, n1, n2, n3, n4, temp)
  end if

  deallocate(temp)
end subroutine add_int_4d

subroutine add_int_5d(name, n1, n2, n3, n4, n5, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3, n4, n5
  integer, dimension(n1,n2,n3,n4,n5), intent(in) :: arr
  integer, dimension(5), intent(in), optional :: order

  character(len=64), dimension(:,:,:,:,:), allocatable :: temp
  integer :: i, j, k, l, m

  allocate(temp(n1,n2,n3,n4,n5))
  do i = 1, n1
    do j = 1, n2
      do k = 1, n3
        do l = 1, n4
          do m = 1, n5
            write(temp(i,j,k,l,m), *) arr(i,j,k,l,m)
          end do
        end do
      end do
    end do
  end do
  if (present(order)) then
    call add_array_5d(name, n1, n2, n3, n4, n5, &
           reshape(temp, (/ n1, n2, n3, n4, n5 /), order=order))
  else
    call add_array_5d(name, n1, n2, n3, n4, n5, temp)
  end if

  deallocate(temp)
end subroutine add_int_5d

!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-

subroutine add_real(name, val)
  character(len=*), intent(in) :: name
  real(dp)        , intent(in) :: val

  character(len=64) :: temp

  write(temp, *) val
  call add_element(name, temp)
end subroutine add_real

subroutine add_real_1d(name, n, arr)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n
  real(dp), dimension(n), intent(in) :: arr

  character(len=64), dimension(:), allocatable :: temp
  integer :: i

  allocate(temp(n))
  do i = 1, n
    write(temp(i), *) arr(i)
  end do
  call add_array_1d(name, n, temp)

  deallocate(temp)
end subroutine add_real_1d

subroutine add_real_2d(name, n1, n2, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2
  real(dp), dimension(n1, n2), intent(in) :: arr
  integer, dimension(2), intent(in), optional :: order

  character(len=64), dimension(:,:), allocatable :: temp
  integer :: i, j

  allocate(temp(n1, n2))
  do i = 1, n1
    do j = 1, n2
      write(temp(i,j), *) arr(i,j)
    end do
  end do
  if (present(order)) then
    call add_array_2d(name, n1, n2, &
           reshape(temp, (/ n1, n2 /), order=order))
  else
    call add_array_2d(name, n1, n2, temp)
  end if

  deallocate(temp)
end subroutine add_real_2d

subroutine add_real_3d(name, n1, n2, n3, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3
  real(dp), dimension(n1, n2, n3), intent(in) :: arr
  integer, dimension(3), intent(in), optional :: order

  character(len=64), dimension(:,:,:), allocatable :: temp
  integer :: i, j, k

  allocate(temp(n1,n2,n3))
  do i = 1, n1
    do j = 1, n2
      do k = 1, n3
        write(temp(i,j,k), *) arr(i,j,k)
      end do
    end do
  end do
  if (present(order)) then
    call add_array_3d(name, n1, n2, n3, &
           reshape(temp, (/ n1, n2, n3 /), order=order))
  else
    call add_array_3d(name, n1, n2, n3, temp)
  end if

  deallocate(temp)
end subroutine add_real_3d

subroutine add_real_4d(name, n1, n2, n3, n4, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3, n4
  real(dp), dimension(n1,n2,n3,n4), intent(in) :: arr
  integer, dimension(4), intent(in), optional :: order

  character(len=64), dimension(:,:,:,:), allocatable :: temp
  integer :: i, j, k, l

  allocate(temp(n1,n2,n3,n4))
  do i = 1, n1
    do j = 1, n2
      do k = 1, n3
        do l = 1, n4
          write(temp(i,j,k,l), *) arr(i,j,k,l)
        end do
      end do
    end do
  end do
  if (present(order)) then
    call add_array_4d(name, n1, n2, n3, n4, &
           reshape(temp, (/ n1, n2, n3, n4 /), order=order))
  else
    call add_array_4d(name, n1, n2, n3, n4, temp)
  end if

  deallocate(temp)
end subroutine add_real_4d

subroutine add_real_5d(name, n1, n2, n3, n4, n5, arr, order)
  character(len=*), intent(in) :: name
  integer, intent(in) :: n1, n2, n3, n4, n5
  real(dp), dimension(n1,n2,n3,n4,n5), intent(in) :: arr
  integer, dimension(5), intent(in), optional :: order

  character(len=64), dimension(:,:,:,:,:), allocatable :: temp
  integer :: i, j, k, l, m

  allocate(temp(n1,n2,n3,n4,n5))
  do i = 1, n1
    do j = 1, n2
      do k = 1, n3
        do l = 1, n4
          do m = 1, n5
            write(temp(i,j,k,l,m), *) arr(i,j,k,l,m)
          end do
        end do
      end do
    end do
  end do
  if (present(order)) then
    call add_array_5d(name, n1, n2, n3, n4, n5, &
           reshape(temp, (/ n1, n2, n3, n4, n5 /), order=order))
  else
    call add_array_5d(name, n1, n2, n3, n4, n5, temp)
  end if

  deallocate(temp)
end subroutine add_real_5d


end module json
