.PHONY : all clean

# TODO: check for definition of constant, do error in compilation
# 		if the constants are not defined properly.

EBIN := ../ebin/

CPPFLAGS += -I. -I/usr/local/erlang-R14/lib/erlang/usr/include
CPPFLAGS += -D MODULE=$(MODULE)
CFLAGS := -Wall -std=c99 -fPIC
CXXFLAGS := -Wall -std=c++0x -fPIC
LDFLAGS := -shared

FILES := $(wildcard $(M)/*.c) $(wildcard $(M)/*.cc)
OBJS = $(addsuffix .o, $(basename $(FILES)))

all: $(OBJS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $(EBIN)/$(MODULE).so $(OBJS)

clean:
	@rm -vf $(OBJS) $(M)/*.dep *.dep *.so

%.dep: %.cc
	$(CXX) $(CPPFLAGS) -MM -MF $@ $<

%.dep: %.c
	$(CC) $(CPPFLAGS) -MM -MF $@ $<

-include $(OBJS:.o=.dep)
