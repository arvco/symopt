program symopt

  ! Calculates the required number of beam positions for the computation of HAADF vortex beams for a cubic crystal and a given grid
  
  
  ! Vortex beam -> no mirror symmetry
  ! Cubic system -> exploit rotational and translational symmetry
  
  implicit none
  
  integer :: i,j,n=0
  
  ! Calculation grid within one unit cell
  integer, parameter :: gridx = 20
  integer, parameter :: gridy = 20
  
  ! Grid to perform symmetry operations on
  integer, dimension(gridx,gridy) :: grid=0
  
  ! Output array for beam positions
  integer, parameter :: pairleng = (gridy  + (( (gridx + gridy) - 2) * ((gridx + gridy) - 1)) / 2)
  integer, dimension( 2, pairleng) :: output=0
  
  print *, pairleng

  ! Variable to store the number of required beam positions
  n = 0
  
  ! Cycle over one quarter of the grid and perform on grid point symmetry operations in order to minimize the number of needed beam positions
  do i=1,gridx
    do j=1,gridy
      select case ( grid(i,j) )
        case (0)
          call mark_beam_pos(i,j,n,grid,gridx,gridy,output,pairleng)
        case (1,2)
          cycle
      end select        
    end do
  end do
  
  ! Output results
  print *, n
  open(unit=10, file='grid.out', action='write')
  open(unit=11, file='beam_pos.out', action='write')
  
  ! Output visualization of the necessary beam positions
  do i=1,gridx
    write(10,*) grid(i,:)
  end do
  
  ! Output beam positions
  do i=1,pairleng
    write(11,*) output(:,i)
!    write(11,1000) output(1,i)
!    write(11,1000) output(2,i)
    1000 format(i3)
  end do
  
  close(10)
  close(11)
  
end program symopt


subroutine mark_beam_pos(i,j,n,grid,gridx,gridy,output,pairleng)
  
  integer :: i,j,k,l,n
  integer :: rux,ruy,llx,lly,rlx,rly
  integer :: gridx,gridy,pairleng
  integer, dimension(gridx,gridy) :: grid
  integer, dimension( 2, pairleng) :: output
  
  grid(i,j) = 1
  n = n + 1
  ! Use modified Cantor pairing function to assign each grid position 
  output(1, (j + ( (i+j-2) * (i+j-1) ) / 2)) = i
  output(2, (j + ( (i+j-2) * (i+j-1) ) / 2)) = j
  
  
  ! Perform 4-fold rotational symmetry operation
  ! Rotation by pi/2
  llx = gridx - j + 1 
  lly = i
  if ( grid(llx,lly) == 0) grid(llx,lly) = 2
  ! Rotation by pi
  rlx = gridx - i + 1
  rly = gridy - j + 1
  if ( grid(rlx,rly) == 0) grid(rlx,rly) = 2
  ! Rotation by 3*pi/2
  rux = j
  ruy = gridy - i + 1     
  if ( grid(rux,ruy) == 0) grid(rux,ruy) = 2
  
  
  ! Perform translational symmetry operation on all grid points
  ! Translate point grid(i,j)
  do k = i, gridx, (gridx - 1)
     do l = j, gridy, (gridy - 1)
       if ( grid(k,l) == 0) grid(k,l) = 2
    end do
  end do
  ! Translate point grid(llx,lly)
  do k = llx, 0, -(gridx - 1)
    do l = lly, gridy, (gridy - 1)
      if ( grid(k,l) == 0) grid(k,l) = 2
    end do
  end do
  ! Translate point grid(rlx,rly)
  do k = rlx, 0, -(gridx - 1)
    do l = rly, 0, -(gridy - 1)
      if ( grid(k,l) == 0) grid(k,l) = 2
    end do
  end do
  ! Translate point grid(rux,ruy)
  do k = rux, gridx, (gridx - 1)
    do l = ruy, 0, -(gridy - 1)
      if ( grid(k,l) == 0) grid(k,l) = 2
    end do
  end do
  
end subroutine mark_beam_pos


