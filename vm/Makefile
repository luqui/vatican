# -fprofile-instr-generate
HFILES = Vatican.h GC.h Makefile
CXXFLAGS = -std=c++11 -g -Wall
OFILES = main.o Vatican.o GC.o

%.o: %.cpp $(HFILES)
	$(CXX) $(CXXFLAGS) -c $<

main: $(OFILES)
	$(CXX) $(CXXFLAGS) -o $@ $^
