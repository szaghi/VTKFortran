x = [(real(i - 1, R8P)/(n - 1), i=1, n)]
do k=1, n ; do j=1, n ; do i=1, n
  ! two hot blobs in a cold cube: the walls are kept at 0
  t(i,j,k) = blob(x(i), x(j), x(k), [0.35_R8P, 0.4_R8P, 0.5_R8P]) + 0.6_R8P*blob(x(i), x(j), x(k), [0.7_R8P, 0.65_R8P, 0.45_R8P])
enddo ; enddo ; enddo
t(1,:,:) = 0 ; t(n,:,:) = 0 ; t(:,1,:) = 0 ; t(:,n,:) = 0 ; t(:,:,1) = 0 ; t(:,:,n) = 0
