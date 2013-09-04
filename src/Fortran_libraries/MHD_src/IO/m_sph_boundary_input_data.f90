!m_sph_boundary_input_data.f90
!     module m_sph_boundary_input_data
!
!     written by H. Matsui on Dec., 2013
!
!
      module m_sph_boundary_input_data
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: id_boundary_file = 41
!
      integer(kind = kint), parameter :: IFLAG_SURFACE_GROUP = 1
      integer(kind = kint), parameter :: IFLAG_NODE_GROUP =    0
!
      integer(kind = kint) :: num_bc_field_ctl
      integer(kind = kint) :: ntot_bc_data_ctl
      character(len=kchara), allocatable :: bc_type_ctl(:)
      character(len=kchara), allocatable :: bc_group_ctl(:)
      character(len=kchara), allocatable :: bc_field_ctl(:)
      integer(kind = kint), allocatable :: iflag_bc_type_ctl(:)
!
      integer(kind = kint), allocatable :: num_bc_data_ctl(:)
      integer(kind = kint), allocatable :: istack_bc_data_ctl(:)
!
      integer(kind = kint), allocatable :: imode_bc_gl_ctl(:,:)
      real(kind = kreal), allocatable ::   bc_data_ctl(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocalte_num_sph_bc_ctl
!
!
      allocate(bc_type_ctl(num_bc_field_ctl))
      allocate(bc_group_ctl(num_bc_field_ctl))
      allocate(bc_field_ctl(num_bc_field_ctl))
      allocate(iflag_bc_type_ctl(num_bc_field_ctl))
!
      allocate(num_bc_data_ctl(num_bc_field_ctl))
      allocate(istack_bc_data_ctl(0:num_bc_field_ctl))
!
      num_bc_data_ctl = 0
      if(num_bc_field_ctl .gt. 0) then
        iflag_bc_type_ctl = 0
        num_bc_data_ctl =   0
      end if
!
      end subroutine allocalte_num_sph_bc_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocalte_sph_bc_item_ctl
!
!
      ntot_bc_data_ctl = istack_bc_data_ctl(num_bc_field_ctl)
      allocate(imode_bc_gl_ctl(2,ntot_bc_data_ctl))
      allocate(bc_data_ctl(ntot_bc_data_ctl))
!
      if(ntot_bc_data_ctl .gt. 0) then
        imode_bc_gl_ctl = 0
        bc_data_ctl =   0.0d0
      end if
!
      end subroutine allocalte_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_boundary_spectr_file
!
      use skip_comment_f
!
      integer(kind = kint) :: igrp, ist, ied, inum
      character(len=255) :: tmpchara
!
!
      open(id_boundary_file, file='boundary_spectr.dat')
!
      call skip_comment(tmpchara,id_boundary_file)
      read(tmpchara,*) num_bc_field_ctl
!
      call allocalte_num_sph_bc_ctl
!
      read(id_boundary_file,*) num_bc_data_ctl(1:num_bc_field_ctl)
!
      istack_bc_data_ctl(0) = 0
      do igrp = 1, num_bc_field_ctl
        istack_bc_data_ctl(igrp) = istack_bc_data_ctl(igrp-1)           &
     &                            + num_bc_data_ctl(igrp)
      end do
      ntot_bc_data_ctl = istack_bc_data_ctl(num_bc_field_ctl)
!
      call allocalte_sph_bc_item_ctl
!
!
      do igrp = 1, num_bc_field_ctl
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*)  bc_field_ctl(igrp), bc_type_ctl(igrp)
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*) bc_group_ctl(igrp)
!
        ist = istack_bc_data_ctl(igrp-1) + 1
        ied = istack_bc_data_ctl(igrp  )
        do inum = ist, ied
          call skip_comment(tmpchara,id_boundary_file)
          read(tmpchara,*) imode_bc_gl_ctl(1:2,inum), bc_data_ctl(inum)
        end do
      end do
      close(id_boundary_file)
!
      do igrp = 1, num_bc_field_ctl
        if( bc_type_ctl(igrp) .eq. 'gradient') then
          iflag_bc_type_ctl(igrp) = IFLAG_SURFACE_GROUP
        else
          iflag_bc_type_ctl(igrp) = IFLAG_NODE_GROUP
        end if
      end do
!
      end subroutine read_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_boundary_spectr_file
!
      integer(kind = kint) :: igrp, ist, ied, inum
!
!
      open(id_boundary_file, file='boundary_spectr.dat')
!
      write(id_boundary_file,'(a)') '#'
      write(id_boundary_file,'(a)') '#  number of boundary conditions'
      write(id_boundary_file,'(a)') '#  number of conditions for each'
      write(id_boundary_file,'(a)') '#'
!
      write(id_boundary_file,'(i8)') num_bc_field_ctl
      write(id_boundary_file,'(10i8)')                                  &
     &                      num_bc_data_ctl(1:num_bc_field_ctl)
!
      write(id_boundary_file,'(a)') '#'
      write(id_boundary_file,'(a)') '#   boundary condition data list'
      write(id_boundary_file,'(a)') '#'
      do igrp = 1, num_bc_field_ctl
        write(id_boundary_file,'(a,a3,a)') trim(bc_field_ctl(igrp) ),   &
     &                              ',  ', trim(bc_type_ctl(igrp)  )
        write(id_boundary_file,'(a)') trim(bc_group_ctl(igrp) )
!
        ist = istack_bc_data_ctl(igrp-1) + 1
        ied = istack_bc_data_ctl(igrp  )
        do inum = ist, ied
          write(id_boundary_file,'(2i10,1pE25.15e3)')                   &
     &              imode_bc_gl_ctl(1:2,inum), bc_data_ctl(inum)
        end do
      end do
!
      close(id_boundary_file)
!
      end subroutine write_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_scalar_bc_by_file(field_name, bc_name,    &
     &          jmax, bc_data, iflag_bc_scalar)
!
      use m_control_params_sph_MHD
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: bc_name
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp, ist_bc, num_bc
!
!
      do igrp = 1, num_bc_field_ctl
        ist_bc = istack_bc_data_ctl(igrp-1) + 1
        num_bc = istack_bc_data_ctl(igrp  )                             &
     &               - istack_bc_data_ctl(igrp-1)
!
        if( bc_field_ctl(igrp) .eq. field_name                          &
     &      .and. iflag_bc_type_ctl(igrp) .eq. IFLAG_NODE_GROUP) then
          if     (bc_group_ctl(igrp) .eq. bc_name) then
            iflag_bc_scalar =  iflag_fixed_field
            call set_bc_for_sph_scalar_by_file                          &
     &          (istack_bc_data_ctl(igrp-1), jmax, bc_data)
          end if
        end if
      end do
!
      end subroutine set_fixed_scalar_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_gradient_bc_by_file(field_name, ref_nod_grp, &
     &          ref_sf_grp, jmax, bc_data, iflag_bc_scalar)
!
      use m_control_params_sph_MHD
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_nod_grp, ref_sf_grp
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp, ist_bc, num_bc
!
!
      do igrp = 1, num_bc_field_ctl
        ist_bc = istack_bc_data_ctl(igrp-1) + 1
        num_bc = istack_bc_data_ctl(igrp  )                             &
     &               - istack_bc_data_ctl(igrp-1)
!
        if( bc_field_ctl(igrp) .eq. field_name                          &
          .and. iflag_bc_type_ctl(igrp) .eq. IFLAG_SURFACE_GROUP) then
          if     (bc_group_ctl(igrp) .eq. ref_nod_grp                   &
     &         .or. bc_group_ctl(igrp) .eq. ref_sf_grp) then
            iflag_bc_scalar =  iflag_fixed_flux
            call set_bc_for_sph_scalar_by_file                          &
     &          (istack_bc_data_ctl(igrp-1), jmax, bc_data)
          end if
        end if
      end do
!
      end subroutine set_fixed_gradient_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_for_sph_scalar_by_file(istack, jmax, bc_data)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: istack(0:1)
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
!
      integer(kind = kint) :: inum, j
!
!
      do inum = istack(0)+1, istack(1)
         j = find_local_sph_mode_address(imode_bc_gl_ctl(1,inum),       &
     &                                   imode_bc_gl_ctl(2,inum) )
         if(j .gt. 0) bc_data(j) = bc_data_ctl(inum)
      end do
!
      end subroutine set_bc_for_sph_scalar_by_file
!
! -----------------------------------------------------------------------
!
      end module m_sph_boundary_input_data
      