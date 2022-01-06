module finbound
    use m_boundary_base, only: t_Boundary, &
                               tp_Boundary, &
                               t_CollisionRecord

    use m_boundary_list, only: t_BoundaryList, new_BoundaryList

    use m_plane_boundary, only: t_Plane, &
                                new_plane, &
                                t_PlaneXYZ, &
                                new_planeXYZ, &
                                new_planeX, &
                                new_planeY, &
                                new_planeZ

    use m_rectangle_boundary, only: t_RectangleXYZ, &
                                    new_rectangleXYZ, &
                                    new_rectangleX, &
                                    new_rectangleY, &
                                    new_rectangleZ

    use m_circle_boundary, only: t_CircleXYZ, &
                                 new_CircleXYZ, &
                                 new_CircleX, &
                                 new_CircleY, &
                                 new_CircleZ

    use m_cylinder_boundary, only: t_CylinderXYZ, &
                                   new_cylinderXYZ, &
                                   new_cylinderX, &
                                   new_cylinderY, &
                                   new_cylinderZ

    use m_plane_with_hole_boundary, only: t_PlaneXYZWithCircleHole, &
                                          new_planeXYZWithCircleHole, &
                                          new_planeXYZWithCircleHoleX, &
                                          new_planeXYZWithCircleHoleY, &
                                          new_planeXYZWithCircleHoleZ
    implicit none
end module finbound