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

contains

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
