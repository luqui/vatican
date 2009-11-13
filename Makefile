CXXFLAGS = -g -Wall

all: interp

interp: vatican.cpp interp.cpp

%.png: %.dot
	dot -T png -o $@ $^
