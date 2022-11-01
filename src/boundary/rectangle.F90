module m_rectangle_boundary
    use m_boundary_base
    use m_triangle_boundary, only: t_Triangle, new_Triangle

    type, extends(t_Boundary) :: t_Rectangle
        double precision :: vertex(3, 4)
        type(t_Triangle) :: triangles(2)
    contains
        procedure :: check_collision => rectangle_check_collision
        procedure :: hit => rectangle_hit
        procedure :: is_overlap => rectangle_is_overlap
        procedure :: pnormal => rectangle_pnormal
    end type

    private
    public t_Rectangle
    public new_Rectangle

contains

    pure function new_Rectangle(vertex) result(obj)
        double precision, intent(in) :: vertex(3, 4)

        type(t_Rectangle) :: obj

        double precision :: vertexes(3, 3, 2)

        obj%vertex(:, :) = vertex(:, :)

        vertexes(:, 1, 1) = vertex(:, 1)
        vertexes(:, 2, 1) = vertex(:, 2)
        vertexes(:, 3, 1) = vertex(:, 3)
        obj%triangles(1) = new_Triangle(vertexes(:, :, 1))

        vertexes(:, 1, 2) = vertex(:, 3)
        vertexes(:, 2, 2) = vertex(:, 4)
        vertexes(:, 3, 2) = vertex(:, 1)
        obj%triangles(2) = new_Triangle(vertexes(:, :, 2))
    end function

    pure function rectangle_check_collision(self, p1, p2) result(record)
        class(t_Rectangle), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        record = self%triangles(1)%check_collision(p1, p2)
        if (record%is_collided) then
            record%material = self%material
            return
        end if

        record = self%triangles(2)%check_collision(p1, p2)
        if (record%is_collided) then
            record%material = self%material
            return
        end if
    end function

    pure function rectangle_hit(self, ray) result(hit_record)
        class(t_Rectangle), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        hit_record = self%triangles(1)%hit(ray)
        if (hit_record%is_hit) then
            hit_record%material = self%material
            return
        end if

        hit_record = self%triangles(2)%hit(ray)
        if (hit_record%is_hit) then
            hit_record%material = self%material
            return
        end if
    end function

    pure function rectangle_is_overlap(self, sdoms, extent) result(is_overlap)
        class(t_Rectangle), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        if (self%triangles(1)%is_overlap(sdoms, extent) &
            .or. self%triangles(2)%is_overlap(sdoms, extent)) then
            is_overlap = .true.
            return
        end if

        is_overlap = .false.
    end function

    pure function rectangle_pnormal(self, position) result(pnormal)
        class(t_Rectangle), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = self%triangles(1)%n(:)
    end function

end module
