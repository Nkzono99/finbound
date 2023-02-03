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

    pure function boundaryRotation_check_collision(self, p1, p2) result(record)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: q1(3), q2(3)

        q1 = self%forward(p1)
        q2 = self%forward(p2)

        record = self%pboundary%check_collision(q1, q2)

        if (record%is_collided) then
            record%position = self%backward(record%position)
        end if
    end function

    pure function boundaryRotation_hit(self, ray) result(hit_record)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        type(t_Ray) :: ray_rotated
        double precision :: to(3)

        ! Converts to rotational coordinate system.
        to(:) = ray%origin(:) + ray%direction(:)
        ray_rotated%origin(:) = self%forward(ray%origin(:))
        to(:) = self%forward(to(:))
        ray_rotated%direction(:) = to(:) - ray_rotated%origin(:)

        ! Ray at.
        hit_record = self%pboundary%hit(ray_rotated)

        ! Inverse converts from rotational coordinate system.
        to(:) = hit_record%position(:) + hit_record%n(:)
        hit_record%position(:) = self%backward(hit_record%position(:))
        to(:) = self%backward(to(:))
        hit_record%n(:) = to(:) - hit_record%position(:)
    end function

    pure function boundaryRotation_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)

        sdoms_(1, :) = self%forward(sdoms_(1, :))
        sdoms_(2, :) = self%forward(sdoms_(2, :))

        is_overlap = self%pboundary%is_overlap(sdoms)
    end function

    pure function boundaryRotation_pnormal(self, position) result(pnormal)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        double precision :: p(3), pn(3), pnn(3), pnb(3)

        p(:) = self%forward(position)

        pn(:) = self%pboundary%pnormal(p(:))
        pnn(:) = p(:) + pn(:)

        pnb(:) = self%backward(pnn(:))
        pnormal(:) = pnn(:) - position(:)
    end function

    subroutine boundaryRotation_destroy(self)
        class(t_BoundaryRotationXYZ), intent(inout) :: self

        call self%pboundary%destroy()
    end subroutine

    pure function boundaryRotation_forward(self, p) result(q)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: p(3)
        double precision :: q(3)

        q = p - self%origin

        select case (self%axis)
        case (1)
            q = rot3d_x(q, -self%rotation_rad)
        case (2)
            q = rot3d_y(q, -self%rotation_rad)
        case (3)
            q = rot3d_z(q, -self%rotation_rad)
        end select
    end function

    pure function boundaryRotation_backward(self, q) result(p)
        class(t_BoundaryRotationXYZ), intent(in) :: self
        double precision, intent(in) :: q(3)
        double precision :: p(3)

        select case (self%axis)
        case (1)
            p = rot3d_x(q, self%rotation_rad)
        case (2)
            p = rot3d_y(q, self%rotation_rad)
        case (3)
            p = rot3d_z(q, self%rotation_rad)
        end select

        p = p + self%origin
    end function

end module
