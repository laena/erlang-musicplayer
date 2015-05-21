OUR_C_FLAGS =  -g -Wall -fpic
OUR_LDFLAGS =  -Wl,-rpath=.
CFLAGS += $(OUR_C_FLAGS)
CXXFLAGS += $(OUR_C_FLAGS)

TARGETS = bassdriver.so

all: $(TARGETS)

clean:
	rm -f $(TARGETS) *.o
	rm -f core erl_crash.dump
	rm -f *~

%.so: %.cc
	$(CXX) $(CXXFLAGS) $< -shared -o $@

%.so: %.o
	$(CC) $(CFLAGS) $(OUR_LDFLAGS) -shared $< -L . -lbass -o $@ 

%: %.cc
	$(CXX) $(CXXFLAGS) $< -o $@

bassdriver.beam: bassdriver.erl
	erlc bassdriver.erl