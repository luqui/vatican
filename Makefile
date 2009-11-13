CXXFLAGS = -g -Wall

all: bubs

bubs: vatican.cpp bubs.cpp

%.png: %.dot
	dot -T png -o $@ $^
