HFILES = Vatican.h Makefile
CXXFLAGS = -g -Wall
OFILES = main.o Vatican.o

%.o: %.cpp $(HFILES)
	$(CXX) $(CXXFLAGS) -c $<

main: $(OFILES)
	$(CXX) $(CXXFLAGS) -o $@ $^
