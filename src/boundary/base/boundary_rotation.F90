module m_boundary_rotation
    use m_boundary
    use m_ray
    use m_vector
    implicit none

    type, extends(t_Boundary) :: t_BoundaryRotationXYZ
        class(t_Boundary), pointer :: pboundary
        integer :: axis
        double precision :: rotation_rad
        double precision :: origin(3)
    contains
        procedure :: check_collision => boundaryRotation_check_collision
        procedure :: hit => boundaryRotation_hit
        procedure :: is_overlap => boundaryRotation_is_overlap
        procedure :: pnormal => boundaryRotation_pnormal

        procedure :: destroy => boundaryRotation_destroy

        procedure, private :: forward => boundaryRotation_forward
        procedure, private :: backward => boundaryRotation_backward
    end type

    private
    public t_BoundaryRotationXYZ
    public new_BoundaryRotationXYZ
    public new_BoundaryRotationX
    public new_BoundaryRotationY
    public new_BoundaryRotationZ

contains

    function new_BoundaryRotationXYZ(pboundary, axis, rotation_rad, origin) result(obj)
        class(t_Boundary), pointer, intent(in) :: pboundary
        integer, intent(in) :: axis
        double precision, intent(in) :: rotation_rad
        double precision, intent(in), optional :: origin(3)
        type(t_BoundaryRotationXYZ) :: obj

        obj%pboundary => pboundary
        obj%axis = axis
        obj%rotation_rad = rotation_rad

        if (present(origin)) then
            obj%origin = origin
        else
            obj%origin = [0d0, 0d0, 0d0]
        end if
    end function

    function new_BoundaryRotationX(pboundary, rotation_rad, origin) result(obj)
        class(t_Boundary), pointer, intent(in) :: pboundary
        double precision, intent(in) :: rotation_rad
        double precision, intent(in), optional :: origin(3)
        type(t_BoundaryRotationXYZ) :: obj

        obj = new_BoundaryRotationXYZ(pboundary, 1, rotation_rad, origin)
    end function

    function new_BoundaryRotationY(pboundary, rotation_rad, origin) result(obj)
        class(t_Boundary), pointer, intent(in) :: pboundary
        double precision, intent(in) :: rotation_rad
        double precision, intent(in), optional :: origin(3)
        type(t_BoundaryRotationXYZ) :: obj

        obj = new_BoundaryRotationXYZ(pboundary, 2, rotation_rad, origin)
    end function

    function new_BoundaryRotationZ(pboundary, rotation_rad, origin) result(obj)
        class(t_Boundary), pointer, intent(in) :: pboundary
        double precision, intent(in) :: rotation_rad
        double precision, intent(in), optional :: origin(3)
        type(t_BoundaryRotationXYZ) :: obj

        obj = new_BoundaryRotationXYZ(pboundary, 3, rotation_rad, origin)
    end function

    pure recursive function boundaryRotation_check_collision(self, p1, p2) result(record)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: q1(3), q2(3), cosine, sine

        cosine = cos(self%rotation_rad)
        sine = sin(self%rotation_rad)
        q1 = rotate_coefficients(p1 - self%origin, self%axis, cosine, -sine)
        q2 = rotate_coefficients(p2 - self%origin, self%axis, cosine, -sine)

        record = self%pboundary%check_collision(q1, q2)

        if (record%is_collided) then
            record%position = rotate_coefficients(record%position, self%axis, cosine, sine) + self%origin
            record%priority = self%priority
        end if
    end function

    pure recursive function boundaryRotation_hit(self, ray) result(hit_record)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        type(t_Ray) :: ray_rotated
        double precision :: cosine, sine

        cosine = cos(self%rotation_rad)
        sine = sin(self%rotation_rad)
        ! Converts to rotational coordinate system.
        ray_rotated%origin = rotate_coefficients(ray%origin - self%origin, self%axis, cosine, -sine)
        ray_rotated%direction = rotate_coefficients(ray%direction, self%axis, cosine, -sine)

        ! Ray at.
        hit_record = self%pboundary%hit(ray_rotated)
        if (.not. hit_record%is_hit) return

        ! Inverse converts from rotational coordinate system.
        hit_record%position = rotate_coefficients(hit_record%position, self%axis, cosine, sine) + self%origin
        hit_record%n = rotate_coefficients(hit_record%n, self%axis, cosine, sine)
        hit_record%priority = self%priority
    end function

    pure recursive function boundaryRotation_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)
        double precision :: local_bounds(2, 3), corner(3)
        double precision :: cosine, sine
        integer :: ix, iy, iz

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)
        cosine = cos(self%rotation_rad)
        sine = sin(self%rotation_rad)

        local_bounds(1, :) = huge(1d0)
        local_bounds(2, :) = -huge(1d0)
        do ix = 1, 2
            do iy = 1, 2
                do iz = 1, 2
                    corner = [sdoms_(ix, 1), sdoms_(iy, 2), sdoms_(iz, 3)] - self%origin
                    corner = rotate_coefficients(corner, self%axis, cosine, -sine)
                    local_bounds(1, :) = min(local_bounds(1, :), corner)
                    local_bounds(2, :) = max(local_bounds(2, :), corner)
                end do
            end do
        end do

        ! The world-space extent has already been applied before rotation.
        is_overlap = self%pboundary%is_overlap(local_bounds, extent=0d0*extent_)
    end function

    pure recursive function boundaryRotation_pnormal(self, position) result(pnormal)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        double precision :: p(3), pn(3), cosine, sine

        cosine = cos(self%rotation_rad)
        sine = sin(self%rotation_rad)
        p = rotate_coefficients(position - self%origin, self%axis, cosine, -sine)

        pn(:) = self%pboundary%pnormal(p(:))
        pnormal = rotate_coefficients(pn, self%axis, cosine, sine)
    end function

    subroutine boundaryRotation_destroy(self)
        class(t_BoundaryRotationXYZ), intent(inout) :: self

        call self%pboundary%destroy()
    end subroutine

    pure recursive function boundaryRotation_forward(self, p) result(q)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: p(3)
        double precision :: q(3)

        q = rotate_vector(p - self%origin, self%axis, -self%rotation_rad)
    end function

    pure recursive function boundaryRotation_backward(self, q) result(p)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: q(3)
        double precision :: p(3)

        p = rotate_vector(q, self%axis, self%rotation_rad) + self%origin
    end function

    pure function rotate_vector(vector, axis, angle) result(rotated)
        double precision, intent(in) :: vector(3), angle
        integer, intent(in) :: axis
        double precision :: rotated(3)

        rotated = rotate_coefficients(vector, axis, cos(angle), sin(angle))
    end function

    pure function rotate_coefficients(vector, axis, cosine, sine) result(rotated)
        double precision, intent(in) :: vector(3), cosine, sine
        integer, intent(in) :: axis
        double precision :: rotated(3)

        rotated = vector
        select case (axis)
        case (1)
            rotated(2) = cosine*vector(2) + sine*vector(3)
            rotated(3) = -sine*vector(2) + cosine*vector(3)
        case (2)
            rotated(1) = -sine*vector(3) + cosine*vector(1)
            rotated(3) = cosine*vector(3) + sine*vector(1)
        case (3)
            rotated(1) = cosine*vector(1) + sine*vector(2)
            rotated(2) = -sine*vector(1) + cosine*vector(2)
        end select
    end function

end module
