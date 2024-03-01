# TODO don't hardcode this
export JAVA_HOME="/usr/lib/jvm/java-1.11.0-openjdk-amd64"

INCLUDES = -I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux
LIBS = -ljvm -L ${JAVA_HOME}/lib -L ${JAVA_HOME}/lib/server

# entry.so maker might have more flags than needed but I did not want to risk breaking
# anything so it stays the way it is
all:
	gcc -g -Wall -Wl,--no-as-needed -fPIC $(INCLUDES) -c src/entry.c -o entry.o -rdynamic $(LIBS)
	gcc -g -fPIC -Wl,--no-as-needed $(INCLUDES) $(LIBS) -shared -o entry.so entry.o -rdynamic
	rm entry.o
