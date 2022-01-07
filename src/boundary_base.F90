module m_boundary_base
    use futils, only: str
    implicit none

    double precision, parameter :: DEFAULT_DOMAIN_EXTENT(2, 3) = &
        reshape([[1d0, 1d0], [1d0, 1d0], [1d0, 1d0]], [2, 3])

    type t_CollisionRecord
        logical :: is_collided = .false.
        double precision :: position(3)
        double precision :: t = -1.0d0
    contains
        procedure :: to_string => record_to_string
    end type

    type, abstract :: t_Boundary
    contains
        procedure(boundary_check_collision), deferred :: check_collision
        procedure(boundary_is_overlap), deferred :: is_overlap
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

        pure function boundary_is_overlap(self, sdoms, extent) result(is_overlap)
            import t_Boundary
            class(t_Boundary), intent(in) :: self
            double precision, intent(in) :: sdoms(2, 3)
            double precision, intent(in), optional :: extent(2, 3)
            logical :: is_overlap
        end function
    end interface

    type, extends(t_Boundary) :: tp_Boundary
        class(t_Boundary), pointer :: ref
    contains

        procedure :: check_collision => pboundary_check_collision
        procedure :: is_overlap => pboundary_is_overlap
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
        character(len=50) :: ret

        ret = 'Record(' &
              //str(self%is_collided)//',' &
              //str(self%position(1))//',' &
              //str(self%position(2))//',' &
              //str(self%position(3))//',' &
              //str(self%t) &
              //')'
    end function

    pure function pboundary_check_collision(self, p1, p2) result(record)
        class(tp_Boundary), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        record = self%ref%check_collision(p1, p2)
    end function

    pure function pboundary_is_overlap(self, sdoms, extent) result(is_overlap)
        class(tp_Boundary), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        double precision, intent(in), optional :: extent(2, 3)
        logical :: is_overlap

        is_overlap = self%ref%is_overlap(sdoms, extent)
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

end module
