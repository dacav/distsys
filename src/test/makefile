.PHONY : all clean

export MODULE := pingpong_peer_native
export FILES := $(wildcard *cc)

all:
	$(MAKE) -C ../../priv M=$(PWD) all

clean:
	$(MAKE) -C ../../priv M=$(PWD) clean
