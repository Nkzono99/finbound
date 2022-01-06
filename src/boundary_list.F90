module m_boundary_list
    use m_boundary_base
    implicit none

    integer, parameter :: DEFAULT_MAX_NBOUNDARIES = 30
    double precision, parameter :: DEFAULT_GROWTH_FACTOR = 1.125d0

    type, extends(t_Boundary) :: t_BoundaryList
        integer :: nboundaries = 0
        type(tp_Boundary), pointer :: boundaries(:)

        integer :: max_nboundaries = DEFAULT_MAX_NBOUNDARIES
        double precision :: growth_factor = DEFAULT_GROWTH_FACTOR
    contains
        procedure :: check_collision => boundaryList_check_collision
        procedure :: is_overlap => boundaryList_is_overlap

        procedure :: add_boundary => boundaryList_add_boundary

        procedure, private :: extent_size => boundaryList_extent_size
    end type

    private
    public t_BoundaryList
    public new_boundaryList

contains

    function new_BoundaryList(max_nboundaries, growth_factor) result(obj)
        integer, intent(in), optional :: max_nboundaries
        double precision, intent(in), optional :: growth_factor
        type(t_BoundaryList) :: obj

        if (present(max_nboundaries)) then
            obj%max_nboundaries = max_nboundaries
        else
            obj%max_nboundaries = DEFAULT_MAX_NBOUNDARIES
        end if

        if (present(growth_factor)) then
            obj%growth_factor = growth_factor
        else
            obj%growth_factor = DEFAULT_GROWTH_FACTOR
        end if

        obj%nboundaries = 0

        allocate (obj%boundaries(obj%max_nboundaries))
    end function

    pure function boundaryList_check_collision(self, p1, p2) result(record)
        class(t_BoundaryList), intent(in) :: self
        double precision, intent(in) :: p1(3)
        double precision, intent(in) :: p2(3)
        type(t_CollisionRecord) :: record

        double precision :: min_t
        integer :: i
        type(t_CollisionRecord) :: tmp_record

        record%is_collided = .false.

        min_t = 100
        do i = 1, self%nboundaries
            tmp_record = self%boundaries(i)%check_collision(p1, p2)
            if (.not. tmp_record%is_collided) then
                continue
            end if

            if (tmp_record%t < min_t) then
                min_t = tmp_record%t
                record = tmp_record
            end if
        end do
    end function

    pure function boundaryList_is_overlap(self, sdoms) result(is_overlap)
        class(t_BoundaryList), intent(in) :: self
        double precision, intent(in) :: sdoms(2, 3)
        logical :: is_overlap

        integer :: i

        do i = 1, self%nboundaries
            if (self%boundaries(i)%is_overlap(sdoms)) then
                is_overlap = .true.
                return
            end if
        end do

        is_overlap = .false.
    end function

    subroutine boundaryList_add_boundary(self, pboundary)
        class(t_BoundaryList), intent(inout) :: self
        class(t_Boundary), pointer, intent(in) :: pboundary

        if (self%nboundaries == self%max_nboundaries) then
            call self%extent_size
        end if

        self%nboundaries = self%nboundaries + 1
        self%boundaries(self%nboundaries)%ref => pboundary
    end subroutine

    subroutine boundaryList_extent_size(self)
        class(t_BoundaryList), intent(inout) :: self

        integer :: new_max_nboundaries
        type(tp_Boundary), pointer :: new_boundaries(:)

        integer :: i

        new_max_nboundaries = max( &
                              int(self%max_nboundaries*self%growth_factor), &
                              self%max_nboundaries + 1)

        allocate (new_boundaries(new_max_nboundaries))

        do i = 1, self%nboundaries
            new_boundaries(i)%ref => self%boundaries(i)%ref
        end do

        deallocate (self%boundaries)
        self%boundaries => new_boundaries
        self%max_nboundaries = new_max_nboundaries
    end subroutine
end module
