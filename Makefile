all: compile

compile:
	mkdir -p ./test/ebin
	erl -make

clean:
	rm ./ebin/*.beam
	rm ./test/ebin/*.beam

# test: compile
# 	erl -noshell \
# 		-pa ebin \
# 		-pa test/ebin \
# 		-s test_suite test \
# 		-s init stop
