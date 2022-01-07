module m_cylinder_boundary
    use m_vector
    use m_boundary_base
    implicit none

    !> The cylinder boundary.
    !
    !  ^ axis
    !  |  ______
    !　 ｨ       ｀ヽ
    !  |'.______=.'|
    !  |           |
    !  |           |
    !  |           |  height
    !  |  r        |
    !  |-----.origin
    !  '.＿＿＿＿_.'  -> axis+1 or axis+2
    type, extends(t_Boundary) :: t_CylinderXYZ
        integer :: axis
        double precision :: origin(3)
        double precision :: radius
        double precision :: height
    contains
        procedure :: check_collision => cylinderXYZ_check_collision
        procedure :: is_overlap => cylinderXYZ_is_overlap
    end type

    private
    public t_CylinderXYZ
    public new_cylinderXYZ
    public new_cylinderX
    public new_cylinderY
    public new_cylinderZ

contains

    pure function new_cylinderXYZ(axis, origin, radius, height) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: height
        type(t_CylinderXYZ) :: obj

        obj%axis = axis
        obj%origin = origin
        obj%radius = radius
        obj%height = height
    end function

    pure function new_cylinderX(origin, radius, height) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: height
        type(t_CylinderXYZ) :: obj

        obj = new_cylinderXYZ(1, origin, radius, height)
    end function

    pure function new_cylinderY(origin, radius, height) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: height
        type(t_CylinderXYZ) :: obj

        obj = new_cylinderXYZ(2, origin, radius, height)
    end function

    pure function new_cylinderZ(origin, radius, height) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: radius
        double precision, intent(in) :: height
        type(t_CylinderXYZ) :: obj

        obj = new_cylinderXYZ(3, origin, radius, height)
    end function

    pure function cylinderXYZ_check_collision(self, p1, p2) result(record)
        class(t_CylinderXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        integer :: axis0, axis1, axis2
        double precision :: x1, y1, x2, y2
        double precision :: a, b, c
        double precision :: d, d2
        double precision :: r
        double precision :: pos_collided(3)

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        x1 = p1(axis1) - self%origin(axis1)
        y1 = p1(axis2) - self%origin(axis2)
        x2 = p2(axis1) - self%origin(axis1)
        y2 = p2(axis2) - self%origin(axis2)

        a = x1*x1 + y1*y1 + x2*x2 + y2*y2 - 2*x1*x2 - 2*y1*y2
        b = x1*x2 + y1*y2 - x1*x1 - y1*y1
        c = x1*x1 + y1*y1 - self%radius*self%radius

        d2 = b*b - a*c
        if (d2 < 0.0d0) then
            record%is_collided = .false.
            return
        end if

        d = sqrt(d2)
        r = (-b - d)/a
        if (r < 0.0d0 .or. 1.0d0 < r) then
            r = (-b + d)/a
        end if
        if (r < 0.0d0 .or. 1.0d0 < r) then
            record%is_collided = .false.
            return
        end if

        pos_collided = (p2 - p1)*r + p1

        if (pos_collided(axis0) < self%origin(axis0) &
            .or. self%origin(axis0) + self%height < pos_collided(axis0)) then
            record%is_collided = .false.
            return
        end if

        record%is_collided = .true.
        record%t = r
        record%position = pos_collided
    end function

    pure function cylinderXYZ_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_CylinderXYZ), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        double precision :: extent_(2, 3)
        double precision :: sdoms_(2, 3)

        integer :: axis0, axis1, axis2

        extent_ = get_default_extent(extent)
        sdoms_(1, :) = sdoms(1, :) - extent_(1, :)
        sdoms_(2, :) = sdoms(2, :) + extent_(2, :)

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        if (self%origin(axis0) + self%height < sdoms_(1, axis0) &
            .or. sdoms_(2, axis0) < self%origin(axis0)) then
            is_overlap = .false.
            return
        end if

        if (self%origin(axis1) + self%radius < sdoms_(1, axis1) &
            .or. sdoms_(2, axis1) < self%origin(axis1) - self%radius) then
            is_overlap = .false.
            return
        end if

        if (self%origin(axis2) + self%radius < sdoms_(1, axis2) &
            .or. sdoms_(2, axis2) < self%origin(axis2) - self%radius) then
            is_overlap = .false.
            return
        end if

        is_overlap = .true.
    end function
end module
