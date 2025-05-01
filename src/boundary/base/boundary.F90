module m_boundary
    use futils, only: str, dot
    use m_material
    use m_ray
    implicit none

    double precision, parameter :: DEFAULT_DOMAIN_EXTENT(2, 3) = &
        reshape([[1d0, 1d0], [1d0, 1d0], [1d0, 1d0]], [2, 3])

    type t_CollisionRecord
        logical :: is_collided = .false.
        double precision :: position(3)
        double precision :: t = -1.0d0
        integer :: priority = 0
        type(t_Material) :: material
    contains
        procedure :: to_string => record_to_string
    end type

    type, abstract :: t_Boundary
        integer :: priority = 0
        type(t_Material) :: material
    contains
        procedure(boundary_check_collision), deferred :: check_collision
        procedure(boundary_hit), deferred :: hit
        procedure(boundary_is_overlap), deferred :: is_overlap
        procedure(boundary_pnormal), deferred :: pnormal
        procedure :: normal => boundary_normal
        procedure :: destroy => boundary_destroy
    end type

    interface
        pure function boundary_check_collision(self, p1, p2) result(record)
            import t_Boundary
            import t_CollisionRecord
            class(t_Boundary), intent(in) :: self
            double precision, intent(in) :: p1(3)
            double precision, intent(in) :: p2(3)
            type(t_CollisionRecord) :: record
        end function

        pure function boundary_hit(self, ray) result(hit_record)
            import t_Boundary
            import t_Ray
            import t_HitRecord
            class(t_Boundary), intent(in) :: self
            type(t_Ray), intent(in) :: ray
            type(t_HitRecord) :: hit_record
        end function

        pure function boundary_is_overlap(self, sdoms, extent) result(is_overlap)
            import t_Boundary
            class(t_Boundary), intent(in) :: self
            double precision, intent(in) :: sdoms(2, 3)
            double precision, intent(in), optional :: extent(2, 3)
            logical :: is_overlap
        end function

        pure function boundary_pnormal(self, position) result(pnormal)
            import t_Boundary
            class(t_Boundary), intent(in) :: self
            double precision, intent(in) :: position(3)
            double precision :: pnormal(3)
        end function
    end interface

    type, extends(t_Boundary) :: tp_Boundary
        class(t_Boundary), pointer :: ref
    contains

        procedure :: check_collision => pboundary_check_collision
        procedure :: is_overlap => pboundary_is_overlap
        procedure :: pnormal => pboundary_pnormal
        procedure :: hit => pboundary_hit
        procedure :: destroy => pboundary_destroy
    end type

    private
    public DEFAULT_DOMAIN_EXTENT
    public t_Boundary
    public tp_Boundary
    public t_CollisionRecord
    public get_default_extent

contains

    function record_to_string(self) result(ret)
        class(t_CollisionRecord), intent(in) :: self
        character(len=100) :: ret

        ret = 'Record(' &
              //str(self%is_collided)//',' &
              //str(self%position(1))//',' &
              //str(self%position(2))//',' &
              //str(self%position(3))//',' &
              //str(self%t) &
              //')'
    end function

    !> Get normal vectory on the position.
    !>
    !> Example
    !>   Case: pnormal and headed are on the same side.
    !>
    !>             |           . headed
    !>             . position
    !>             | -> pnormal
    !>             | -> normal
    !>
    !>   Case: pnormal and headed are on the opposite sides.
    !>             |           . headed
    !>             . position
    !>          <- |
    !>             | -> normal
    !>
    pure function boundary_normal(self, position_on_boundary, headed_by_vector) result(normal)
        class(t_Boundary), intent(in) :: self
        double precision, intent(in) :: position_on_boundary(3)
        double precision, intent(in), optional :: headed_by_vector(3)
        double precision :: normal(3)

        double precision :: ip  ! inner product

        normal(:) = self%pnormal(position_on_boundary)

        if (.not. present(headed_by_vector)) then
            return
        end if

        ip = dot(normal(:), (headed_by_vector(:) - position_on_boundary(:)))
        normal(:) = normal(:)*(ip/abs(ip))
    end function

    pure function pboundary_check_collision(self, p1, p2) result(record)
        class(tp_Boundary), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        record = self%ref%check_collision(p1, p2)
        record%priority = self%ref%priority
    end function

    pure function pboundary_hit(self, ray) result(hit_record)
        class(tp_Boundary), intent(in) :: self
        type(t_Ray), intent(in) :: ray
        type(t_HitRecord) :: hit_record

        hit_record = self%ref%hit(ray)
        hit_record%priority = self%ref%priority
    end function

    pure function pboundary_is_overlap(self, sdoms, extent) result(is_overlap)
        class(tp_Boundary), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        is_overlap = self%ref%is_overlap(sdoms, extent)
    end function

    pure function pboundary_pnormal(self, position) result(pnormal)
        class(tp_Boundary), intent(in) :: self
        double precision, intent(in) :: position(3)
        double precision :: pnormal(3)

        pnormal(:) = self%ref%pnormal(position(:))
    end function

    pure function get_default_extent(extent) result(ret)
        double precision, intent(in), optional :: extent(2, 3)
        double precision :: ret(2, 3)

        if (present(extent)) then
            ret = extent
        else
            ret = DEFAULT_DOMAIN_EXTENT
        end if
    end function

    !> Destroy instance.
    !>
    !> To be called if no further references are made.
    !> Overriding in child classes, as the abstract class does nothing.
    subroutine boundary_destroy(self)
        class(t_Boundary), intent(inout) :: self
    end subroutine

    subroutine pboundary_destroy(self)
        class(tp_Boundary), intent(inout) :: self

        deallocate (self%ref)
    end subroutine

end module
