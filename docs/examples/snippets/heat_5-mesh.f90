! the points, numbered from 0 with i running fastest
do k=1, n ; do j=1, n ; do i=1, n
  px(id(i,j,k) + 1) = x(i) ; py(id(i,j,k) + 1) = x(j) ; pz(id(i,j,k) + 1) = x(k)
enddo ; enddo ; enddo
! the cells: hexahedra (VTK type 12), their 8 points in the VTK order, bottom face then top face
c = 0
do k=1, n - 1 ; do j=1, n - 1 ; do i=1, n - 1
  c = c + 1
  connect(8*c-7:8*c) = [id(i,j,k), id(i+1,j,k), id(i+1,j+1,k), id(i,j+1,k), &
                        id(i,j,k+1), id(i+1,j,k+1), id(i+1,j+1,k+1), id(i,j+1,k+1)]
  offset(c) = 8*c
  tc(c) = sum(t(i:i+1,j:j+1,k:k+1))/8  ! the mean of its points
enddo ; enddo ; enddo
cell_type = 12_I1P
