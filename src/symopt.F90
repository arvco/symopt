program symopt

! Calculates the required number of beam positions for the computation of HAADF vortex beams for a cubic crystal and a given grid


! Vortex beam -> no mirror symmetry
! Cubic system -> exploit rotational and translational symmetry

implicit none

integer :: i,j,k,count
integer, parameter :: gridx = 50
integer, parameter :: gridy = 50

integer, dimension(gridx,gridy) :: grid

do i=1,gridx
  do j=1,gridy
    if ( dimension(i,j) == 0 ) then
      dimension(i,j) = 1
      count = + 1

      ! Perform rotational symmetry operation
      do k=1,3
        if ( dimension(i,
        
        dimension(i,j) = 2
      end do
    else ( dimension(i,j) == 1 ) then
      continue
    end if
  end do
end do

end program symopt

