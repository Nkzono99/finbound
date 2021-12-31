module m_rectangle_boundary
    use m_boundary_base
    implicit none

    type, extends(t_Boundary) :: t_RectangleXYZ
        !> Axis (1: x, 2: y, 3: z)
        integer :: axis
        !> Original position.
        double precision :: origin(3)
        !> Rectangle width in mod(axis+1, 3)+1 axis.
        double precision :: w1
        !> Rectangle width in mod(axis+2, 3)+1 axis.
        double precision :: w2

    contains
        procedure check_collision => rectangleXYZ_check_collision
        procedure is_overlap => rectangleXYZ_is_overlap
    end type

    private

    public t_RectangleXYZ
    public new_rectangleXYZ
    public new_rectangleX
    public new_rectangleY
    public new_rectangleZ

contains

    pure function new_rectangleXYZ(axis, origin, w1, w2) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: origin
        double precision, intent(in) :: w1
        double precision, intent(in) :: w2
        type(t_RectangleXYZ) :: obj

        obj%axis = axis
        obj%origin = origin
        obj%w1 = w1
        obj%w2 = w2
    end function

    pure function new_rectangleX(origin, wy, wz) result(obj)
        double precision, intent(in) :: origin
        !> The width in y-axis (= self%w1)
        double precision, intent(in) :: wy
        !> The width in z-axis (= self%w2)
        double precision, intent(in) :: wz
        type(t_RectangleXYZ) :: obj

        obj = new_rectangleXYZ(1, origin, wy, wz)
    end function

    pure function new_rectangleY(origin, wz, wx) result(obj)
        double precision, intent(in) :: origin
        !> The width in z-axis (= self%w1)
        double precision, intent(in) :: wz
        !> The width in x-axis (= self%w2)
        double precision, intent(in) :: wx
        type(t_RectangleXYZ) :: obj

        obj = new_rectangleXYZ(2, origin, wz, wx)
    end function

    pure function new_rectangleZ(origin, wx, wy) result(obj)
        double precision, intent(in) :: origin
        !> The width in x-axis (= self%w1)
        double precision, intent(in) :: wx
        !> The width in y-axis (= self%w2)
        double precision, intent(in) :: wy
        type(t_RectangleXYZ) :: obj

        obj = new_rectangleXYZ(3, origin, wx, wy)
    end function

    pure function rectangleXYZ_check_collision(self, p1, p2) result(record)
        class(t_RectangleXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: d1, d2
        double precision :: r
        double precision :: pos_collided(3)
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
        if (pos_collided(axis1) < self%origin(axis1) &
            .or. self%origin(axis1) + self%w1 < pos_collided(axis1) &
            .or. pos_collided(axis2) < self%origin(axis2) &
            .or. self%origin(axis2) + self%w2 < pos_collided(axis2)) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
    end function

    pure function rectangleXYZ_is_overlap(self, sdoms) result(is_overlap)
        class(t_RectangleXYZ), intent(in) :: self
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

        if (self%origin(axis1) + self%w1 < sdoms(1, axis1) - 1 &
            .or. sdoms(2, axis1) + 1 < self%origin(axis1)) then
            is_overlap = .false.
        end if

        if (self%origin(axis2) + self%w2 < sdoms(1, axis2) - 1 &
            .or. sdoms(2, axis2) + 1 < self%origin(axis2)) then
            is_overlap = .false.
        end if

        is_overlap = .true.
    end function

end module
