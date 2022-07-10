module m_camera
    use m_ray, only: t_Ray, new_Ray
    use m_vector
    implicit none

    type t_ParallelCamera
        double precision :: phiz
        double precision :: phixy

        integer :: nx
        integer :: ny
        integer :: nz

        double precision :: S  ! Area of ray-generation plane
        double precision :: Sl ! Area of simulation-box by parallel perspective

        double precision :: p(3, 4)  ! Points of ray-generation plane
        double precision :: p12(3), p14(3)  ! for random ray-generation
        double precision :: n(3)  ! Normal vector of ray-generation plane

    contains
        procedure :: generate_randray => parallelCamera_generate_randray
        procedure :: rotate => parallelCamera_rotate
        procedure :: rotate_rev => parallelCamera_rotate_rev

    end type

    private
    public t_ParallelCamera
    public new_ParallelCamera
    public new_ParallelCamera_optimized

contains

    function new_ParallelCamera_optimized(phiz, phixy, ranges) result(obj)
        type(t_ParallelCamera) :: obj
        double precision, intent(in) :: phiz, phixy
        integer, intent(in) :: ranges(2, 3)

        double precision :: ps(3, 8)
        double precision :: nc1(3), nc2(3)

        obj%phiz = phiz
        obj%phixy = phixy

        obj%n = [0d0, 0d0, -1d0]
        obj%n = obj%rotate(obj%n)

        nc1 = [1d0, 0d0, 0d0]
        nc2 = [0d0, 1d0, 0d0]

        nc1 = obj%rotate(nc1)
        nc2 = obj%rotate(nc2)

        block
            double precision :: xl, yl, zl
            xl = ranges(2, 1) - ranges(1, 1)
            yl = ranges(2, 2) - ranges(1, 2)
            zl = ranges(2, 3) - ranges(1, 3)
            obj%Sl = abs(xl*yl*cos(phiz)) &
                     + abs(yl*zl*sin(phiz)*cos(phixy)) &
                     + abs(zl*xl*sin(phiz)*sin(phixy))
        end block

        block
            integer :: i, j, k
            double precision :: p(3)
            do concurrent(i=1:2, j=1:2, k=1:2)
                p = [ranges(i, 1), ranges(j, 2), ranges(k, 3)]
                p = [dot(nc1, p), dot(nc2, p), dot(obj%n, p)]
                ps(:, (i - 1) + (j - 1)*2 + (k - 1)*4 + 1) = p(:)
            end do
        end block

        obj%p(1:2, 1) = [minval(ps(1, :)), minval(ps(2, :))]
        obj%p(1:2, 2) = [maxval(ps(1, :)), minval(ps(2, :))]
        obj%p(1:2, 3) = [maxval(ps(1, :)), maxval(ps(2, :))]
        obj%p(1:2, 4) = [minval(ps(1, :)), maxval(ps(2, :))]
        obj%p(3, :) = minval(ps(3, :)) - 1.0d0

        obj%S = (obj%p(1, 3) - obj%p(1, 1))*(obj%p(2, 3) - obj%p(2, 1))

        obj%p(1:3, 1) = obj%p(1, 1)*nc1 + obj%p(2, 1)*nc2 + obj%p(3, 1)*obj%n
        obj%p(1:3, 2) = obj%p(1, 2)*nc1 + obj%p(2, 2)*nc2 + obj%p(3, 2)*obj%n
        obj%p(1:3, 3) = obj%p(1, 3)*nc1 + obj%p(2, 3)*nc2 + obj%p(3, 3)*obj%n
        obj%p(1:3, 4) = obj%p(1, 4)*nc1 + obj%p(2, 4)*nc2 + obj%p(3, 4)*obj%n

        obj%p12(:) = obj%p(:, 2) - obj%p(:, 1)
        obj%p14(:) = obj%p(:, 4) - obj%p(:, 1)
    end function

    function new_ParallelCamera(phiz, phixy, nx, ny, nz) result(obj)
        type(t_ParallelCamera) :: obj
        double precision, intent(in) :: phiz, phixy
        integer, intent(in) :: nx, ny, nz
        double precision :: norm

        double precision :: v12(3), v23(3)

        obj%phiz = phiz
        obj%phixy = phixy

        obj%nx = nx
        obj%ny = ny
        obj%nz = nz

        norm = nx*nx + ny*ny + nz*nz
        norm = sqrt(norm)

        obj%S = norm*norm
        obj%Sl = abs(nx*ny*cos(phiz)) &
                 + abs(ny*nz*sin(phiz)*cos(phixy)) &
                 + abs(nz*nx*sin(phiz)*sin(phixy))

        obj%p(:, 1) = [-norm/2, norm/2, norm/2]
        obj%p(:, 2) = [norm/2, norm/2, norm/2]
        obj%p(:, 3) = [norm/2, -norm/2, norm/2]
        obj%p(:, 4) = [-norm/2, -norm/2, norm/2]

        obj%p(:, 1) = obj%rotate(obj%p(:, 1)) + [nx/2, ny/2, nz/2]
        obj%p(:, 2) = obj%rotate(obj%p(:, 2)) + [nx/2, ny/2, nz/2]
        obj%p(:, 3) = obj%rotate(obj%p(:, 3)) + [nx/2, ny/2, nz/2]
        obj%p(:, 4) = obj%rotate(obj%p(:, 4)) + [nx/2, ny/2, nz/2]

        v12 = obj%p(:, 2) - obj%p(:, 1)
        v23 = obj%p(:, 3) - obj%p(:, 2)

        obj%n = cross(v12, v23)
        call normalize(obj%n)

        obj%p(:, 1) = obj%p(:, 1) - obj%n*norm
        obj%p(:, 2) = obj%p(:, 2) - obj%n*norm
        obj%p(:, 3) = obj%p(:, 3) - obj%n*norm
        obj%p(:, 4) = obj%p(:, 4) - obj%n*norm

        obj%p12(:) = obj%p(:, 2) - obj%p(:, 1)
        obj%p14(:) = obj%p(:, 4) - obj%p(:, 1)
    end function

    function parallelCamera_generate_randray(self, rands) result(ray)
        class(t_ParallelCamera), intent(inout) :: self
        double precision, intent(in) :: rands(2) !> 0.0 ~ 1.0
        type(t_Ray) :: ray

        double precision :: o(3), d(3)
        double precision :: s, t

        s = rands(1)
        t = rands(2)

        o(:) = s*self%p12 + t*self%p14 + self%p(:, 1)
        d(:) = self%n

        ray = new_Ray(o, d)
    end function

    function parallelCamera_rotate(self, vec) result(ret)
        class(t_ParallelCamera), intent(inout) :: self
        double precision, intent(in) :: vec(3)

        double precision :: ret(3)

        ret(:) = rot3d_y(vec(:), self%phiz)
        ret(:) = rot3d_z(ret(:), self%phixy)
    end function

    function parallelCamera_rotate_rev(self, vec) result(ret)
        class(t_ParallelCamera), intent(inout) :: self
        double precision, intent(in) :: vec(3)

        double precision :: ret(3)

        ret(:) = rot3d_z(vec, -self%phixy)
        ret(:) = rot3d_y(ret, -self%phiz)
    end function

end module
