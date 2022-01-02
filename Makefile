.PHONY: all clean

PROGRAM = build/lib/libfinbound.a
OBJS = \
	build/boundary_base.o \
	build/plane.o \
	build/circle.o \
	build/cylinder.o \
	build/plane_with_hole.o \
	build/rectangle.o

TEST_OBJS = \
	build/test/boundary_assertion.o

MODS = \
	m_boundary_base.mod \
	m_plane_boundary.mod \
	m_circle_boundary.mod \
	m_cylinder_boundary.mod \
	m_plane_with_hole_boundary.mod \
	m_rectangle_boundary.mod \
	m_boundary_assertion.mod

LIBRARY_TO_BUILD = build/lib
INCLUDE = build/include

FC = ftn  # ifort
IFLAGS = -module $(INCLUDE) -I$(INCLUDE) 

# FC = gfortran
# IFLAGS = -J$(INCLUDE) -I$(INCLUDE) 

LFLAGS = -Llib/futils/build/lib -Ilib/futils/build/include/ -lfutils
FLAGS = $(IFLAGS) $(LFLAGS) 
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

build/boundary_base.o: src/boundary_base.F90 
	$(FC) -c $< -o build/boundary_base.o $(FLAGS)

build/plane.o: src/plane.F90 build/boundary_base.o
	$(FC) -c $< -o build/plane.o $(FLAGS)

build/circle.o: src/circle.F90 build/boundary_base.o
	$(FC) -c $< -o build/circle.o $(FLAGS)

build/cylinder.o: src/cylinder.F90 build/boundary_base.o
	$(FC) -c $< -o build/cylinder.o $(FLAGS)

build/plane_with_hole.o: src/plane_with_hole.F90 build/boundary_base.o build/plane.o
	$(FC) -c $< -o build/plane_with_hole.o $(FLAGS)

build/rectangle.o: src/rectangle.F90 build/boundary_base.o
	$(FC) -c $< -o build/rectangle.o $(FLAGS)

build/test/boundary_assertion.o: test/boundary_assertion.F90  $(PROGRAM)
	$(FC) -c $< -o build/test/boundary_assertion.o $(FLAGS)

build/test/test_cylinder.exe: test/test_cylinder.F90 build/test/boundary_assertion.o $(PROGRAM)
	$(FC) $^ -o build/test/test_cylinder.exe $(FLAGS)

test_cylinder: build/test/test_cylinder.exe
	./build/test/test_cylinder.exe

build/test/test_plane.exe: test/test_plane.F90 build/test/boundary_assertion.o $(PROGRAM)
	$(FC) $^ -o build/test/test_plane.exe $(FLAGS)

test_plane: build/test/test_plane.exe
	./build/test/test_plane.exe

build/test/test_plane_with_hole.exe: test/test_plane_with_hole.F90 build/test/boundary_assertion.o $(PROGRAM)
	$(FC) $^ -o build/test/test_plane_with_hole.exe $(FLAGS)

test_plane_with_hole: build/test/test_plane_with_hole.exe
	./build/test/test_plane_with_hole.exe

build/test/test_rectangle.exe: test/test_rectangle.F90 build/test/boundary_assertion.o $(PROGRAM)
	$(FC) $^ -o build/test/test_rectangle.exe $(FLAGS)

test_rectangle: build/test/test_rectangle.exe
	./build/test/test_rectangle.exe

test: test_cylinder test_plane test_plane_with_hole test_rectangle build/test

build/test/boundary_assertion.o: lib/futils/build/lib/libfutils.a

lib/futils/build/lib/libfutils.a:
	make -C lib/futils builddir 
	make -C lib/futils

builddir:
	-$(MKDIR) build
	-$(MKDIR) build\test
	-$(MKDIR) build\test\lib
	-$(MKDIR) $(LIBRARY_TO_BUILD)
	-$(MKDIR) $(INCLUDE)
	mkdir -C lib/futils builddir

clean:
	-$(RM) $(PROGRAM)
	-$(RM) $(INCLUDE)/*.mod
	-$(RM) $(LIBRARY)/*.a
	-$(RM) build/*.o
	-$(RM) build/test/*.exe
	-$(RM) build/test/*.o
	-$(RM) build/test/lib/*.a
	make -C lib/futils clean
