module m_boundary_base
    implicit none

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

        pure function boundary_is_overlap(self, sdoms) result(is_overlap)
            import t_Boundary
            class(t_Boundary), intent(in) :: self
            double precision, intent(in) :: sdoms(2, 3)
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
    public t_Boundary
    public tp_Boundary
    public t_CollisionRecord

contains

    function record_to_string(self) result(ret)
        class(t_CollisionRecord), intent(in) :: self
        character(len=50) :: ret
        character(len=1) :: bool
        character(len=8) :: x, y, z
        character(len=8) :: t

        if (self%is_collided) then
            bool = 'T'
        else
            bool = 'F'
        end if

        write (x, '(f8.3)') self%position(1)
        write (y, '(f8.3)') self%position(2)
        write (z, '(f8.3)') self%position(3)
        write(t, '(f8.3)') self%t
        ret = 'Record('//bool//','//x//','//y//','//z//','//t//')'
    end function

    pure function pboundary_check_collision(self, p1, p2) result(record)
        class(tp_Boundary), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        record = self%ref%check_collision(p1, p2)
    end function

    pure function pboundary_is_overlap(self, sdoms) result(is_overlap)
        class(tp_Boundary), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        logical :: is_overlap

        is_overlap = self%ref%is_overlap(sdoms)
    end function

end module
