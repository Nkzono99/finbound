.PHONY: all clean

PROGRAM = build/lib/libboundaries.a
OBJS = \
	build/rectangle.o \
	build/plane.o \
	build/vector.o \
	build/boundary_base.o \
	build/circle.o \
	build/cylinder.o

TEST_OBJS = \
	build/test/boundary_assertion.o

MODS = \
	m_rectangle_boundary.mod \
	m_plane_boundary.mod \
	m_vector.mod \
	m_boundary_base.mod \
	m_circle_boundary.mod \
	m_cylinder_boundary.mod \
	m_boundary_assertion.mod

LIBRARY_TO_BUILD = build/lib
INCLUDE = build/include

FC = ftn
FLAGS = -module $(INCLUDE) -I$(INCLUDE) 
AR = ar rc

RM = rm -f
MKDIR = mkdir -p

ifeq ($(OS),Windows_NT)
	RM = powershell del
	MKDIR = mkdir
endif

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(AR) $(PROGRAM) $^

build/rectangle.o: src/rectangle.F90 build/boundary_base.o
	$(FC) $(FLAGS) -c $< -o build/rectangle.o

build/plane.o: src/plane.F90 build/vector.o build/boundary_base.o
	$(FC) $(FLAGS) -c $< -o build/plane.o

build/vector.o: src/vector.F90 
	$(FC) $(FLAGS) -c $< -o build/vector.o

build/boundary_base.o: src/boundary_base.F90 
	$(FC) $(FLAGS) -c $< -o build/boundary_base.o

build/circle.o: src/circle.F90 build/boundary_base.o
	$(FC) $(FLAGS) -c $< -o build/circle.o

build/cylinder.o: src/cylinder.F90 build/vector.o build/boundary_base.o
	$(FC) $(FLAGS) -c $< -o build/cylinder.o

build/test/test_plane.exe: test/test_plane.F90 build/test/boundary_assertion.o $(PROGRAM)
	$(FC) $(FLAGS) $^ -o build/test/test_plane.exe

test_plane: build/test/test_plane.exe
	./build/test/test_plane.exe

build/test/boundary_assertion.o: test/boundary_assertion.F90  $(PROGRAM)
	$(FC) $(FLAGS) -c $< -o build/test/boundary_assertion.o

test: test_plane build/test

builddir:
	-$(MKDIR) build
	-$(MKDIR) build/test
	-$(MKDIR) build/test/lib
	-$(MKDIR) $(LIBRARY_TO_BUILD)
	-$(MKDIR) $(INCLUDE)

clean:
	-$(RM) $(PROGRAM)
	-$(RM) $(INCLUDE)/*.mod
	-$(RM) $(LIBRARY)/*.a
	-$(RM) build/*.o
	-$(RM) build/test/*.exe
	-$(RM) build/test/*.o
	-$(RM) build/test/lib/*.a
