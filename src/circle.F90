module m_circle_boundary
    use m_boundary_base
    implicit none

    type, extends(t_Boundary) :: t_CircleXYZ
        !> Axis (1: x, 2: y, 3: z)
        integer :: axis
        !> Original position.
        double precision :: origin(3)
        double precision :: radius

    contains
        procedure :: check_collision => circleXYZ_check_collision
        procedure :: is_overlap => circleXYZ_is_overlap
    end type

    private

    public t_CircleXYZ
    public new_CircleXYZ
    public new_CircleX
    public new_CircleY
    public new_CircleZ

contains

    pure function new_CircleXYZ(axis, origin, radius) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: origin
        double precision, intent(in) :: radius
        type(t_CircleXYZ) :: obj

        obj%axis = axis
        obj%origin = origin
        obj%radius = radius
    end function

    pure function new_CircleX(origin, radius) result(obj)
        double precision, intent(in) :: origin
        double precision, intent(in) :: radius
        type(t_CircleXYZ) :: obj

        obj = new_CircleXYZ(1, origin, radius)
    end function

    pure function new_CircleY(origin, radius) result(obj)
        double precision, intent(in) :: origin
        double precision, intent(in) :: radius
        type(t_CircleXYZ) :: obj

        obj = new_CircleXYZ(2, origin, radius)
    end function

    pure function new_CircleZ(origin, radius) result(obj)
        double precision, intent(in) :: origin
        double precision, intent(in) :: radius
        type(t_CircleXYZ) :: obj

        obj = new_CircleXYZ(3, origin, radius)
    end function

    pure function circleXYZ_check_collision(self, p1, p2) result(record)
        class(t_circleXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: d1, d2
        double precision :: r
        double precision :: pos_collided(3)
        double precision :: r1, r2
        integer :: axis1, axis2

        d1 = p1(self%axis) - self%origin(self%axis)
        d2 = p2(self%axis) - self%origin(self%axis)
        if (d1*d2 >= 0) then
            record%is_collided = .false.
            return
        end if

        r = abs(d1)/(abs(d1) + abs(d2))
        pos_collided = (p2 - p1)*r + p1

        axis1 = mod(self%axis + 1, 3) + 1
        axis2 = mod(self%axis + 2, 3) + 1

        r1 = pos_collided(axis1) - self%origin(axis1)
        r2 = pos_collided(axis2) - self%origin(axis2)
        if (r1*r1 + r2*r2 > self%radius*self%radius) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
    end function

    pure function circleXYZ_is_overlap(self, sdoms) result(is_overlap)
        class(t_circleXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        logical :: is_overlap

        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0 + 1, 3) + 1
        axis2 = mod(axis0 + 2, 3) + 1

        if (self%origin(axis0) < sdoms(1, axis0) - 1 &
            .or. sdoms(2, axis0) + 1 < self%origin(axis0)) then
            is_overlap = .false.
        end if

        if (self%origin(axis1) + self%radius < sdoms(1, axis1) - 1 &
            .or. sdoms(2, axis1) + 1 < self%origin(axis1)) then
            is_overlap = .false.
        end if

        if (self%origin(axis2) + self%radius < sdoms(1, axis2) - 1 &
            .or. sdoms(2, axis2) + 1 < self%origin(axis2)) then
            is_overlap = .false.
        end if

        is_overlap = .true.
    end function

end module
