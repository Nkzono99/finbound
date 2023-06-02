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
        procedure :: hit => cylinderXYZ_hit
        procedure :: is_overlap => cylinderXYZ_is_overlap
        procedure :: pnormal => cylinder_pnormal
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
        double precision :: a, b, c
        double precision :: d2
        double precision :: r
        double precision :: pos_collided(3)

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        ! block
        !     double precision :: x1, y1, x2, y2
        !     x1 = p1(axis1) - self%origin(axis1)
        !     y1 = p1(axis2) - self%origin(axis2)
        !     x2 = p2(axis1) - self%origin(axis1)
        !     y2 = p2(axis2) - self%origin(axis2)

        !     a = x1*x1 + y1*y1 + x2*x2 + y2*y2 - 2*x1*x2 - 2*y1*y2
        !     b = x1*x2 + y1*y2 - x1*x1 - y1*y1
        !     c = x1*x1 + y1*y1 - self%radius*self%radius
        ! end block
        block
            double precision :: xr, yr
            double precision :: dx, dy
            xr = p1(axis1) - self%origin(axis1)
            yr = p1(axis2) - self%origin(axis2)
            dx = p2(axis1) - p1(axis1)
            dy = p2(axis2) - p1(axis2)

            a = dx*dx + dy*dy
            b = xr*dx + yr*dy
            c = xr*xr + yr*yr - self%radius*self%radius
        end block

        d2 = b*b - a*c
        if (d2 < 0.0d0) then
            record%is_collided = .false.
            return
        end if

        block
            double precision :: d
            d = sqrt(d2)

            r = (-b - d)/a
            if (r < 0.0d0 .or. 1.0d0 < r) then
                r = (-b + d)/a
            end if
        end block

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
        record%material = self%material
    end function

    pure function cylinderXYZ_hit(self, ray) result(hit_record)
        class(t_CylinderXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: p1(3), p2(3)

        double precision :: t
        double precision :: pos_hit(3)
        
        double precision :: a, b, c
        double precision :: d2, d1
        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        p1(:) = ray%origin(:)
        p2(:) = ray%origin(:) + ray%direction(:)

        block
            ! Solve equation.
            !   P = Ro + t*d
            !   |OP| = r
            !
            !   P: cross point
            !   Ro: origin of the ray
            !   t: coefficient
            !   d: direction of the ray
            !   O: origin of the cylinder
            !   r: radius of the cylinder
            double precision :: xr, yr
            double precision :: dx, dy
            xr = p1(axis1) - self%origin(axis1)
            yr = p1(axis2) - self%origin(axis2)
            dx = p2(axis1) - p1(axis1)
            dy = p2(axis2) - p1(axis2)

            a = dx*dx + dy*dy
            b = xr*dx + yr*dy
            c = xr*xr + yr*yr - self%radius*self%radius
        end block

        d2 = b*b - a*c
        if (d2 < 0.0d0) then
            hit_record%is_hit = .false.
            return
        end if

        d1 = sqrt(d2)

        if (a >= 0) then
            t = (-b - d1)/a
        else
            t = (-b + d1)/a
        end if
        if (t >= 0.0d0) then
            pos_hit = (p2 - p1)*t + p1

            if (self%origin(axis0) <= pos_hit(axis0) &
                .and. pos_hit(axis0) <= self%origin(axis0) + self%height) then
                hit_record%is_hit = .true.
                hit_record%t = t
                hit_record%position(:) = pos_hit(:)
                hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
                hit_record%material = self%material
                return
            end if
        end if

        if (a >= 0) then
            t = (-b + d1)/a
        else
            t = (-b - d1)/a
        end if
        if (t >= 0.0d0) then
            pos_hit = (p2 - p1)*t + p1

            if (self%origin(axis0) <= pos_hit(axis0) &
                .and. pos_hit(axis0) <= self%origin(axis0) + self%height) then
                hit_record%t = t
                hit_record%position(:) = pos_hit(:)
                hit_record%is_hit = .true.
                hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
                hit_record%material = self%material
                return
            end if
        end if

        hit_record%is_hit = .false.
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

    pure function cylinder_pnormal(self, position) result(pnormal)
        class(t_CylinderXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        pnormal(axis0) = 0d0
        pnormal(axis1) = position(axis1) - self%origin(axis1)
        pnormal(axis2) = position(axis2) - self%origin(axis2)

        call normalize(pnormal)
    end function

end module
