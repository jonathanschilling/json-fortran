program test_json
  use json

  call open_dbg_out("test.json")

  call add_int("a", 42)
  call add_int_1d("b", 3,       (/ 1, 2, 3 /) )
  call add_int_2d("c", 3, 2, (/ (/ 1, 2, 3 /), &
                                (/ 4, 5, 6 /) /) )

  call add_int_3d("d", 3, 2, 2, (/ &
                                (/ (/  1,  2,  3 /), &
                                   (/  4,  5,  6 /) /), &
                                (/ (/  7,  8,  9 /), &
                                   (/ 10, 11, 12 /) /) &
                                /) )

  call close_dbg_out()

end program
