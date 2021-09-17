# json-fortran
KISS Fortran routines to write (multi-dimensional) data as JSON

## Example

```Fortran
program test_json
  use json

  call open_dbg_out("test.json")

  call add_int("a", 42)
  call add_int_1d("b", 3,       (/ 1, 2, 3 /) )
  call add_int_2d("c", 3, 2, (/ (/ 1, 2, 3 /), &
                                (/ 4, 5, 6 /) /) )

  call close_dbg_out()

end program
```

produces the following JSON output in `test.json`:

```json
{"a":42,"b":[1,2,3],"c":[[1,2,3],[4,5,6]]}
```

## Include into existing projects
Here is how you can add this project as a [Git submodule](https://git-scm.com/book/en/v2/Git-Tools-Submodules) into your code:

```bash
$ git submodule add git@github.com:jonathanschilling/json-fortran.git
```

In order to use this project in an existing CMake setup, simply include the following in your `CMakeLists.txt`:

```
add_subdirectory(json-fortran)
```

It you do not work on VMEC, you will have to adjust the `vmec_sources` array name in `json-fortran`'s `CMakeLists.txt`.
