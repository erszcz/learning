all: reckless-correct reckless-buggy

reckless-correct: reckless-cxx.cpp
	c++ -DCORRECT=1 -std=c++11 $< -o $@

reckless-buggy: reckless-cxx.cpp
	c++ -std=c++11 $< -o $@
