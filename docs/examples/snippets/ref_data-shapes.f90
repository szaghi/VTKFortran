real(R8P) :: s(8)                        ! rank 1: one value per point
real(R8P) :: t(2,2,2)                    ! rank 3, (i,j,k): a scalar field, needs one_component=.true.
real(R8P) :: vec(3,8)                    ! rank 2, (components, points)
real(R8P) :: vec4(3,2,2,2)               ! rank 4, (components, i, j, k)
real(R8P) :: vx(8), vy(8), vz(8)         ! the 3 components as separate rank-1 arrays
real(R8P) :: wx(2,2,2), wy(2,2,2), wz(2,2,2) ! the 3 components as separate rank-3 arrays
real(R8P) :: txx(8), tyy(8), tzz(8), txy(8), tyz(8), txz(8) ! a symmetric tensor as 6 separate arrays
real(R8P) :: full(9,8)                   ! rank 2: a full tensor, 9 components
