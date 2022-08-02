module m_hyperboloid_boundary
    use m_vector
    use m_boundary_base
    implicit none

    !> The hyperboloid boundary.
    !>   x^2/a^2 + y^2/b^2 - z^2/c^2 = 1
    type, extends(t_Boundary) :: t_HyperboloidXYZ
        integer :: axis
        double precision :: origin(3)
        double precision :: max_radius
        double precision :: min_radius
        double precision :: height
        double precision :: a, b, c
    contains
        procedure :: check_collision => hyperboloidXYZ_check_collision
        procedure :: is_overlap => hyperboloidXYZ_is_overlap
        procedure :: pnormal => hyperboloid_pnormal
        procedure :: hit => hyperboloidXYZ_hit
    end type

    private
    public t_HyperboloidXYZ
    public new_hyperboloidXYZ
    public new_hyperboloidX
    public new_hyperboloidY
    public new_HyperboloidZ

contains

    pure function new_hyperboloidXYZ(axis, origin, max_radius, min_radius, height) result(obj)
        integer, intent(in) :: axis
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: max_radius
        double precision, intent(in) :: min_radius
        double precision, intent(in) :: height
        type(t_HyperboloidXYZ) :: obj

        obj%axis = axis
        obj%origin = origin
        obj%max_radius = max_radius
        obj%min_radius = min_radius
        obj%height = height

        obj%a = min_radius
        obj%b = min_radius
        obj%c = sqrt((0.5d0*height)**2/((max_radius/min_radius)**2 - 1.0d0))
    end function

    pure function new_hyperboloidX(origin, max_radius, min_radius, height) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: max_radius
        double precision, intent(in) :: min_radius
        double precision, intent(in) :: height
        type(t_HyperboloidXYZ) :: obj

        obj = new_hyperboloidXYZ(1, origin, max_radius, min_radius, height)
    end function

    pure function new_hyperboloidY(origin, max_radius, min_radius, height) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: max_radius
        double precision, intent(in) :: min_radius
        double precision, intent(in) :: height
        type(t_HyperboloidXYZ) :: obj

        obj = new_hyperboloidXYZ(2, origin, max_radius, min_radius, height)
    end function

    pure function new_HyperboloidZ(origin, max_radius, min_radius, height) result(obj)
        double precision, intent(in) :: origin(3)
        double precision, intent(in) :: max_radius
        double precision, intent(in) :: min_radius
        double precision, intent(in) :: height
        type(t_HyperboloidXYZ) :: obj

        obj = new_hyperboloidXYZ(3, origin, max_radius, min_radius, height)
    end function

    pure function hyperboloidXYZ_check_collision(self, p1, p2) result(record)
        class(t_HyperboloidXYZ), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        integer :: axis0, axis1, axis2

        double precision :: o(3)
        double precision :: d(3)
        double precision :: a, b, c
        double precision :: d1
        double precision :: d2
        double precision :: t
        double precision :: pos_collided(3)

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        o(:) = p1(:) - self%origin(:)
        d(:) = p2(:) - p1(:)

        a = (d(axis1)/self%a)**2 + (d(axis2)/self%b)**2 - (d(axis0)/self%c)**2
        b = (o(axis1)*d(axis1)/self%a**2) + (o(axis2)*d(axis2)/self%b**2) - (o(axis0)*d(axis0)/self%c**2)
        c = (o(axis1)/self%a)**2 + (o(axis2)/self%b)**2 - (o(axis0)/self%c)**2 - 1.0d0

        d2 = b*b - a*c
        if (d2 < 0.0d0) then
            record%is_collided = .false.
            return
        end if

        d1 = sqrt(d2)

        if (a >= 0) then
            t = (-b - d1)/a
        else
            t = (-b + d1)/a
        end if
        if (0.0d0 <= t .and. t <= 1.0d0) then
            pos_collided = (p2 - p1)*t + p1

            if (self%origin(axis0) - 0.5d0*self%height <= pos_collided(axis0) &
                .and. pos_collided(axis0) <= self%origin(axis0) + 0.5d0*self%height) then
                record%is_collided = .true.
                record%t = t
                record%position(:) = pos_collided(:)
                record%material = self%material
                return
            end if
        end if

        if (a >= 0) then
            t = (-b + d1)/a
        else
            t = (-b - d1)/a
        end if
        if (0.0d0 <= t .and. t <= 1.0d0) then
            pos_collided = (p2 - p1)*t + p1

            if (self%origin(axis0) - 0.5d0*self%height <= pos_collided(axis0) &
                .and. pos_collided(axis0) <= self%origin(axis0) + 0.5d0*self%height) then
                record%is_collided = .true.
                record%t = t
                record%position(:) = pos_collided(:)
                record%material = self%material
                return
            end if
        end if

        record%is_collided = .false.
    end function

    pure function hyperboloidXYZ_hit(self, ray) result(hit_record)
        class(t_hyperboloidXYZ), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        double precision :: t
        double precision :: pos_hit(3)

        double precision :: p1(3), p2(3)

        double precision :: o(3)
        double precision :: d(3)
        double precision :: a, b, c
        double precision :: d2, d1

        integer :: axis0, axis1, axis2

        p1(:) = ray%origin(:)
        p2(:) = ray%origin(:) + ray%direction(:)

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        o(:) = p1(:) - self%origin(:)
        d(:) = p2(:) - p1(:)

        a = (d(axis1)/self%a)**2 + (d(axis2)/self%b)**2 - (d(axis0)/self%c)**2
        b = (o(axis1)*d(axis1)/self%a**2) + (o(axis2)*d(axis2)/self%b**2) - (o(axis0)*d(axis0)/self%c**2)
        c = (o(axis1)/self%a)**2 + (o(axis2)/self%b)**2 - (o(axis0)/self%c)**2 - 1.0d0

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

            if (self%origin(axis0) - 0.5d0*self%height <= pos_hit(axis0) &
                .and. pos_hit(axis0) <= self%origin(axis0) + 0.5d0*self%height) then
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

            if (self%origin(axis0) - 0.5d0*self%height <= pos_hit(axis0) &
                .and. pos_hit(axis0) <= self%origin(axis0) + 0.5d0*self%height) then
                hit_record%is_hit = .true.
                hit_record%t = t
                hit_record%position(:) = pos_hit(:)
                hit_record%n(:) = self%normal(pos_hit(:), ray%origin(:))
                hit_record%material = self%material
                return
            end if
        end if

        hit_record%is_hit = .false.
    end function

    pure function hyperboloidXYZ_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_HyperboloidXYZ), intent(in) :: self
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

        if (self%origin(axis0) + self%height*0.5d0 < sdoms_(1, axis0) &
            .or. sdoms_(2, axis0) < self%origin(axis0) - self%height*0.5d0) then
            is_overlap = .false.
            return
        end if

        if (self%origin(axis1) + self%max_radius < sdoms_(1, axis1) &
            .or. sdoms_(2, axis1) < self%origin(axis1) - self%max_radius) then
            is_overlap = .false.
            return
        end if

        if (self%origin(axis2) + self%max_radius < sdoms_(1, axis2) &
            .or. sdoms_(2, axis2) < self%origin(axis2) - self%max_radius) then
            is_overlap = .false.
            return
        end if

        is_overlap = .true.
    end function

    pure function hyperboloid_pnormal(self, position) result(pnormal)
        class(t_HyperboloidXYZ), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        integer :: axis0, axis1, axis2

        axis0 = self%axis
        axis1 = mod(axis0, 3) + 1
        axis2 = mod(axis0 + 1, 3) + 1

        pnormal(axis0) = -(position(axis0) - self%origin(axis0))/self%c**2
        pnormal(axis1) = (position(axis1) - self%origin(axis1))/self%a**2
        pnormal(axis2) = (position(axis2) - self%origin(axis2))/self%b**2

        call normalize(pnormal)
    end function

end module
