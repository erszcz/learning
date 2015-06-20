CXXFLAGS = -std=c++11
LDFLAGS  = -lboost_program_options

SRCS 	:= $(wildcard *.cc)
OBJS	:= $(patsubst %.cc,%.o,$(SRCS))

burp: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm *.o
	rm burp
