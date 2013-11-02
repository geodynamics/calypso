!> @file  m_sph_boundary_input_data.f90
!!      module m_sph_boundary_input_data
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine deallocalte_sph_bc_item_ctl
!!
!!      subroutine read_boundary_spectr_file
!!      subroutine write_boundary_spectr_file
!!
!!      subroutine set_fixed_scalar_bc_by_file(field_name, ref_nod_grp, &
!!     &          jmax, bc_data, iflag_bc_scalar)
!!      subroutine set_fixed_gradient_bc_by_file(field_name,            &
!!     &          ref_nod_grp, ref_sf_grp, jmax, bc_data,               &
!!     &          iflag_bc_scalar)
!!
!!  ---------------------------------------------------------------------
!!      Data format
!!       line 1:  Number of total boundary conditiones to be defedined
!!
!!       line 2:     Field name to define a boundary condition
!!       line 3:     Number of modes for each boundary conditions
!!       line 4:     Place to define a boundary condision
!!       line 5...:  Spectrum data for the boundary conditinos
!!                  degrre i, order, m, harmonics coefficients
!!        Return to 2...
!!  ---------------------------------------------------------------------
!!@endverbatim
!!
!!@param    field_name       Field name to be define
!!                           the boundary condition by file
!!@param    ref_nod_grp      Boundary group name to be defined
!!                           the boundary condition by file
!!@param    ref_sf_grp       Surface group name to be defined
!!                           the boundary condition by file
!!@param    jamx             Number of local spherical harmonics modes
!!@param    bc_data(jmax)    Local boundary condition spectrum
!!@param    iflag_bc_scalar  Boundary condition type flag
!
      module m_sph_boundary_input_data
!
      use m_precision
!
      implicit  none
!
      character(len=kchara) :: bc_sph_file_name = 'boundary_spectr.dat'
      integer(kind = kint), parameter :: id_boundary_file = 41
!
      integer(kind = kint) :: ntot_bc_data_ctl
!
      integer(kind = kint), allocatable :: istack_bc_data_ctl(:)
!
!>        Structure for each boundary condition
      type each_boundary_spectr
!>        Name of group to apply boundary conditions
        character(len=kchara) :: bc_group
!>        Name of field to apply boundary conditions
        character(len=kchara) :: bc_field
!>        Number of components of boundary condition data
        integer(kind = kint) :: ncomp_bc
!>        Number of spherical harmonics modes of boundary condition data
        integer(kind = kint) :: num_bc_mode
!>        spherical harmonics modes of boundary condition data
!!          bc_input(1:num_bc_mode): degree l
!!          bc_input(2:num_bc_mode): order  m
        integer(kind = kint), pointer :: imode_gl(:,:)
!>        boundary condition spectrum  bc_input(mode,component)
        real(kind = kreal), pointer ::   bc_input(:,:)
      end type each_boundary_spectr
!
!>        Name of boundary conditions to set
      integer(kind = kint), save :: num_bc_field_ctl
!>        Structures for boundary conditions
      type(each_boundary_spectr), allocatable, save :: bc_ctls(:)
!
      private :: num_bc_field_ctl, bc_ctls
!
      private :: alloc_each_bc_item_ctl, dealloc_each_bc_item_ctl
      private :: allocalte_sph_bc_item_ctl, set_num_comp_bc_data
      private :: set_bc_for_sph_scalar_by_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_bc_item_ctl(bc)
!
      type(each_boundary_spectr), intent(inout) :: bc
!
!
      allocate(bc%imode_gl(2,bc%num_bc_mode))
      allocate(bc%bc_input(bc%num_bc_mode,bc%ncomp_bc))
!
      if(bc%num_bc_mode .gt. 0) then
        bc%imode_gl = 0
        bc%bc_input =  0.0d0
      end if
!
      end subroutine alloc_each_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_bc_item_ctl(bc)
!
      type(each_boundary_spectr), intent(inout) :: bc
!
!
      deallocate(bc%imode_gl, bc%bc_input)
!
      end subroutine dealloc_each_bc_item_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocalte_sph_bc_item_ctl
!
!
      allocate(bc_ctls(num_bc_field_ctl))
      bc_ctls(1:num_bc_field_ctl)%num_bc_mode = 0
      bc_ctls(1:num_bc_field_ctl)%ncomp_bc =    1
!
      end subroutine allocalte_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocalte_sph_bc_item_ctl
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_bc_field_ctl
        call dealloc_each_bc_item_ctl(bc_ctls(i))
      end do
      deallocate(bc_ctls)
!
      end subroutine deallocalte_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_boundary_spectr_file
!
      use m_machine_parameter
      use skip_comment_f
!
      integer(kind = kint) :: igrp, inum
      character(len=255) :: tmpchara
!
!
      if(iflag_debug .gt. 0) write(*,*) 'read boundary condition: ',    &
     &                      trim(bc_sph_file_name)
      open(id_boundary_file, file=bc_sph_file_name)
!
      call skip_comment(tmpchara,id_boundary_file)
      read(tmpchara,*) num_bc_field_ctl
!
      call allocalte_sph_bc_item_ctl
!
      do igrp = 1, num_bc_field_ctl
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*)  bc_ctls(igrp)%bc_field
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*) bc_ctls(igrp)%bc_group
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*) bc_ctls(igrp)%num_bc_mode
!
        call set_num_comp_bc_data(bc_ctls(igrp))
        call alloc_each_bc_item_ctl(bc_ctls(igrp))
!
        do inum = 1, bc_ctls(igrp)%num_bc_mode
          call skip_comment(tmpchara,id_boundary_file)
          read(tmpchara,*) bc_ctls(igrp)%imode_gl(1:2,inum),            &
     &          bc_ctls(igrp)%bc_input(inum,1:bc_ctls(igrp)%ncomp_bc)
        end do
      end do
      close(id_boundary_file)
!
      end subroutine read_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_boundary_spectr_file
!
      integer(kind = kint) :: igrp, inum
!
!
      open(id_boundary_file, file=bc_sph_file_name)
!
      write(id_boundary_file,'(a)') '#'
      write(id_boundary_file,'(a)') '#  number of boundary conditions'
      write(id_boundary_file,'(a)') '#'
!
      write(id_boundary_file,'(i8)') num_bc_field_ctl
!
      do igrp = 1, num_bc_field_ctl
        write(id_boundary_file,'(a)') '#'
        write(id_boundary_file,'(a)') '#   boundary condition data list'
        write(id_boundary_file,'(a)') '#'
!
        write(id_boundary_file,'(a)')   trim(bc_ctls(igrp)%bc_field)
        write(id_boundary_file,'(a)')   trim(bc_ctls(igrp)%bc_group)
        write(id_boundary_file,'(i8)')  bc_ctls(igrp)%num_bc_mode
!
        do inum = 1, bc_ctls(igrp)%num_bc_mode
          write(id_boundary_file,'(2i10,1p10E25.15e3)')                 &
     &          bc_ctls(igrp)%imode_gl(1:2,inum),                       &
     &          bc_ctls(igrp)%bc_input(inum,1:bc_ctls(igrp)%ncomp_bc)
        end do
      end do
!
      close(id_boundary_file)
!
      end subroutine write_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_scalar_bc_by_file(field_name, ref_nod_grp,   &
     &          jmax, bc_data, iflag_bc_scalar)
!
      use m_control_params_sph_MHD
      use skip_comment_f
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_nod_grp
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp, iflag
!
!
      do igrp = 1, num_bc_field_ctl
        iflag = cmp_no_case(bc_ctls(igrp)%bc_field, field_name)         &
     &        * cmp_no_case(bc_ctls(igrp)%bc_group, ref_nod_grp)
        if(iflag .gt. 0) then
          iflag_bc_scalar =  iflag_fixed_field
          call set_bc_for_sph_scalar_by_file                            &
     &              (bc_ctls(igrp), jmax, bc_data)
        end if
      end do
!
      end subroutine set_fixed_scalar_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_gradient_bc_by_file(field_name,              &
     &          ref_nod_grp, ref_sf_grp, jmax, bc_data,                 &
     &          iflag_bc_scalar)
!
      use m_control_params_sph_MHD
      use skip_comment_f
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_nod_grp, ref_sf_grp
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp, iflag
!
!
      do igrp = 1, num_bc_field_ctl
        iflag = cmp_no_case(bc_ctls(igrp)%bc_field, field_name)         &
     &        * (cmp_no_case(bc_ctls(igrp)%bc_group, ref_nod_grp)       &
     &         + cmp_no_case(bc_ctls(igrp)%bc_group, ref_sf_grp))
        if(iflag .gt. 0) then
          iflag_bc_scalar =  iflag_fixed_flux
          call set_bc_for_sph_scalar_by_file                            &
     &                (bc_ctls(igrp), jmax, bc_data)
        end if
      end do
!
      end subroutine set_fixed_gradient_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_for_sph_scalar_by_file(bc, jmax, bc_data)
!
      use m_spheric_parameter
!
      type(each_boundary_spectr), intent(in) :: bc
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
!
      integer(kind = kint) :: inum, j
!
!
      do inum = 1, bc%num_bc_mode
         j = find_local_sph_mode_address(bc%imode_gl(1,inum),           &
     &                                   bc%imode_gl(2,inum) )
         if(j .gt. 0) bc_data(j) = bc%bc_input(inum,1)
      end do
!
      end subroutine set_bc_for_sph_scalar_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_num_comp_bc_data(bc)
!
      use m_phys_labels
      use skip_comment_f
!
      type(each_boundary_spectr), intent(inout) :: bc
      integer(kind = kint) :: iflag
!
!
      iflag =  cmp_no_case(bc%bc_field, fhd_velo)                       &
     &       + cmp_no_case(bc%bc_field, fhd_vort)                       &
     &       + cmp_no_case(bc%bc_field, fhd_magne)
     if(iflag .gt. 0)  bc%ncomp_bc = 2
!
      iflag =  cmp_no_case(bc%bc_field, fhd_temp)                       &
     &       + cmp_no_case(bc%bc_field, fhd_light)                      &
     &       + cmp_no_case(bc%bc_field, fhd_entropy)                    &
     &       + cmp_no_case(bc%bc_field, fhd_h_flux)                     &
     &       + cmp_no_case(bc%bc_field, fhd_c_flux)
     if(iflag .gt. 0)  bc%ncomp_bc = 1
!
      end subroutine set_num_comp_bc_data
!
! -----------------------------------------------------------------------
!
      end module m_sph_boundary_input_data
