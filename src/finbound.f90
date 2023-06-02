module finbound
    use m_material, only: t_Material
    use m_boundary, only: t_Boundary, &
                          tp_Boundary, &
                          t_CollisionRecord, &
                          DEFAULT_DOMAIN_EXTENT

    use m_boundary_list, only: t_BoundaryList, new_BoundaryList

    use m_plane_boundary, only: t_Plane, &
                                new_plane, &
                                t_PlaneXYZ, &
                                new_planeXYZ, &
                                new_planeX, &
                                new_planeY, &
                                new_planeZ

    use m_triangle_boundary, only: t_Triangle, &
                                   new_Triangle

    use m_rectangle_boundary, only: t_Rectangle, &
                                    new_Rectangle

    use m_rectangleXYZ_boundary, only: t_RectangleXYZ, &
                                       new_rectangleXYZ, &
                                       new_rectangleX, &
                                       new_rectangleY, &
                                       new_rectangleZ

    use m_circle_boundary, only: t_CircleXYZ, &
                                 new_CircleXYZ, &
                                 new_CircleX, &
                                 new_CircleY, &
                                 new_CircleZ

    use m_donut_boundary, only: t_DonutXYZ, &
                                new_DonutXYZ, &
                                new_DonutX, &
                                new_DonutY, &
                                new_DonutZ

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

    use m_boundary_rotation, only: t_BoundaryRotationXYZ, &
                                   new_BoundaryRotationXYZ, &
                                   new_BoundaryRotationX, &
                                   new_BoundaryRotationY, &
                                   new_BoundaryRotationZ

    use m_sphere_boundary, only: t_Sphere, &
                                 new_Sphere, &
                                 t_CutSphereXYZ, &
                                 new_CutSphereX, &
                                 new_CutSphereY, &
                                 new_CutSphereZ

    use m_hyperboloid_boundary, only: t_HyperboloidXYZ, &
                                      new_hyperboloidXYZ, &
                                      new_hyperboloidX, &
                                      new_hyperboloidY, &
                                      new_HyperboloidZ

    use m_ellipsoid_boundary, only: t_EllipsoidXYZ, &
                                    new_ellipsoidXYZ, &
                                    new_ellipsoidX, &
                                    new_ellipsoidY, &
                                    new_ellipsoidZ

    use m_ray, only: t_Ray, &
                     new_Ray, &
                     t_HitRecord

    use m_camera, only: t_ParallelCamera, &
                        new_ParallelCamera, &
                        new_ParallelCamera_optimized
end module finbound
