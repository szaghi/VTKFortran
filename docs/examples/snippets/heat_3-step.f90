subroutine step(t)
!< Advance the temperature by one explicit time step of the heat equation; the walls stay at 0.
real(R8P), intent(inout) :: t(:,:,:)

t(2:n-1,2:n-1,2:n-1) = t(2:n-1,2:n-1,2:n-1) + dt/h**2*(t(1:n-2,2:n-1,2:n-1) + t(3:n,2:n-1,2:n-1) + &
                                                        t(2:n-1,1:n-2,2:n-1) + t(2:n-1,3:n,2:n-1) + &
                                                        t(2:n-1,2:n-1,1:n-2) + t(2:n-1,2:n-1,3:n) - 6*t(2:n-1,2:n-1,2:n-1))
endsubroutine step

subroutine heat_flux(t, qx, qy, qz)
!< The heat flux, minus the gradient of the temperature (centred differences, 0 on the walls).
real(R8P), intent(in)  :: t(:,:,:)
real(R8P), intent(out) :: qx(:,:,:), qy(:,:,:), qz(:,:,:)

qx = 0 ; qy = 0 ; qz = 0
qx(2:n-1,:,:) = -(t(3:n,:,:) - t(1:n-2,:,:))/(2*h)
qy(:,2:n-1,:) = -(t(:,3:n,:) - t(:,1:n-2,:))/(2*h)
qz(:,:,2:n-1) = -(t(:,:,3:n) - t(:,:,1:n-2))/(2*h)
endsubroutine heat_flux
