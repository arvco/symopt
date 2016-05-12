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
  integer, dimension(gridx,gridy) :: grid
  grid = 0;
  
  ! Variable to store the number of required beam positions
  n = 0
  
  
  !outer: do i=1,9
  !  inner: do j=1,9
  !    print *, "before: ",i,j
  !    if(j>3) cycle outer
  !    print *, "before: ",i,j
  !    if(j>6) exit outer
  !    print *, "after: ",i,j
  !  end do inner
  !end do outer
  
  !print *, grid
  
  do i=1,( gridx / 2 )
    do j=1,( gridy / 2 )
      select case ( grid(i,j) )
        case (0)
          call mark_beam_pos(i,j,n,grid,gridx,gridy)
        case (1,2)
          cycle
        case default
          grid(i,j) = 0
          call mark_beam_pos(i,j,n,grid,gridx,gridy)
      end select        
    end do
  end do
  
  ! Output results
  print *, n
  open(unit=10, file='grid.out', action='write')
  
  do i=1,gridx
    write(10,*) grid(i,:)
  end do
  
  close(10)
  
end program symopt


subroutine mark_beam_pos(i,j,n,grid,gridx,gridy)
  
  integer :: i,j,k,l,n
  integer :: rux,ruy,llx,lly,rlx,rly
  integer :: gridx,gridy
  integer, dimension(gridx,gridy) :: grid
  
  grid(i,j) = 1
  n = n + 1
  
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
!  print *, llx, lly, rlx, rly, rux, ruy
  

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

  
!  do k=1,gridx
!    print *, grid(k,:)
!  end do
!  print *, ' '

end subroutine mark_beam_pos


