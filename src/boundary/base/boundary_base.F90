module m_boundary_base
    use m_material, only: t_Material
    use m_boundary, only: DEFAULT_DOMAIN_EXTENT, &
                          t_Boundary, &
                          tp_Boundary, &
                          t_CollisionRecord, &
                          get_default_extent

    use m_ray, only: t_Ray, &
                     new_Ray, &
                     t_HitRecord

    use m_boundary_rotation, only: t_BoundaryRotationXYZ, &
                                   new_BoundaryRotationXYZ, &
                                   new_BoundaryRotationX, &
                                   new_BoundaryRotationY, &
                                   new_BoundaryRotationZ

    use m_boundary_list, only: t_BoundaryList, &
                               new_BoundaryList

end module
