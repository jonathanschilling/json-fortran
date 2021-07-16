
FC=gfortran

test_json: json.f90 test_json.f90
	${FC} json.f90 test_json.f90 -o test_json

all: test_json

test:	test_json
	./test_json

clean:
	rm -f *.mod test_json
