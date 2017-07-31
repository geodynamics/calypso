!> @file  t_sph_boundary_input_data.f90
!!      module t_sph_boundary_input_data
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine dealloc_sph_bc_item_ctl
!!
!!      subroutine read_boundary_spectr_file
!!      subroutine write_boundary_spectr_file
!!
!!      subroutine set_fixed_scalar_bc_by_file(field_name, sph_rj,      &
!!     &          bc_IO, ref_grp, bc_data, iflag_bc_scalar)
!!      subroutine set_fixed_gradient_bc_by_file(field_name, sph_rj,    &
!!     &          bc_IO, ref_grp, bc_data, iflag_bc_scalar)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!  ---------------------------------------------------------------------
!!      Data format
!!       line 1:  Number of total boundary conditions to be defined
!!
!!       line 2:     Field name to define the first boundary condition
!!       line 3:     Place to define the first boundary condition
!!       line 4:     Number of spherical harmonics modes 
!!                    for each boundary condition
!!       line 5...:  Spectrum data for the boundary conditions 
!!                  (degree $l$, order $m$, and harmonics coefficients)
!!        Return to 2...
!!  ---------------------------------------------------------------------
!!@endverbatim
!!
!!@param    field_name       Field name to be define
!!                           the boundary condition by file
!!@param    ref_grp          Boundary group name to be defined
!!                           the boundary condition by file
!!@param    bc_data(sph_rj%nidx_rj(2)) Local boundary condition spectrum
!!@param    iflag_bc_scalar  Boundary condition type flag
!
      module t_sph_boundary_input_data
!
      use m_precision
      use t_spheric_rj_data
!
      implicit  none
!
!>      File ID for boundary condition file
      integer(kind = kint), parameter :: id_boundary_file = 41
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
        integer(kind = kint), allocatable :: imode_gl(:,:)
!>        boundary condition spectrum  bc_input(mode,component)
        real(kind = kreal), allocatable ::   bc_input(:,:)
      end type each_boundary_spectr
!
!
      type boundary_spectra
!>        File name for boundary condition file
        character(len=kchara) :: file_name = 'boundary_spectr.dat'
!>        Name of boundary conditions to set
        integer(kind = kint) :: num_bc_fld
!>        Structures for boundary conditions
        type(each_boundary_spectr), allocatable :: ctls(:)
      end type boundary_spectra
!
!
      private :: id_boundary_file
!
      private :: alloc_each_bc_item_ctl, dealloc_each_bc_item_ctl
      private :: alloc_sph_bc_item_ctl, set_num_comp_bc_data
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
      subroutine alloc_sph_bc_item_ctl(bc_IO)
!
      type(boundary_spectra), intent(inout) :: bc_IO
!
!
      allocate(bc_IO%ctls(bc_IO%num_bc_fld))
      bc_IO%ctls(1:bc_IO%num_bc_fld)%num_bc_mode = 0
      bc_IO%ctls(1:bc_IO%num_bc_fld)%ncomp_bc =    1
!
      end subroutine alloc_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_bc_item_ctl(bc_IO)
!
      type(boundary_spectra), intent(inout) :: bc_IO
!
      integer(kind = kint) :: i
!
!
      do i = 1, bc_IO%num_bc_fld
        call dealloc_each_bc_item_ctl(bc_IO%ctls(i))
      end do
      deallocate(bc_IO%ctls)
!
      end subroutine dealloc_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_boundary_spectr_file(bc_IO)
!
      use m_machine_parameter
      use skip_comment_f
!
      type(boundary_spectra), intent(inout) :: bc_IO
!
      integer(kind = kint) :: igrp, inum
      character(len=255) :: tmpchara
!
!
      if(iflag_debug .gt. 0) write(*,*) 'read boundary condition: ',    &
     &                      trim(bc_IO%file_name)
      open(id_boundary_file, file=bc_IO%file_name)
!
      call skip_comment(tmpchara,id_boundary_file)
      read(tmpchara,*) bc_IO%num_bc_fld
!
      call alloc_sph_bc_item_ctl(bc_IO)
!
      do igrp = 1, bc_IO%num_bc_fld
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*)  bc_IO%ctls(igrp)%bc_field
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*) bc_IO%ctls(igrp)%bc_group
        call skip_comment(tmpchara,id_boundary_file)
        read(tmpchara,*) bc_IO%ctls(igrp)%num_bc_mode
!
        call set_num_comp_bc_data(bc_IO%ctls(igrp))
        call alloc_each_bc_item_ctl(bc_IO%ctls(igrp))
!
        do inum = 1, bc_IO%ctls(igrp)%num_bc_mode
          call skip_comment(tmpchara,id_boundary_file)
          read(tmpchara,*) bc_IO%ctls(igrp)%imode_gl(1:2,inum),         &
     &      bc_IO%ctls(igrp)%bc_input(inum,1:bc_IO%ctls(igrp)%ncomp_bc)
        end do
      end do
      close(id_boundary_file)
!
      end subroutine read_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine write_boundary_spectr_file(bc_IO)
!
      type(boundary_spectra), intent(in) :: bc_IO
!
      integer(kind = kint) :: igrp, inum
!
!
      open(id_boundary_file, file=bc_IO%file_name)
!
      write(id_boundary_file,'(a)') '#'
      write(id_boundary_file,'(a)') '#  number of boundary conditions'
      write(id_boundary_file,'(a)') '#'
!
      write(id_boundary_file,'(i16)') bc_IO%num_bc_fld
!
      do igrp = 1, bc_IO%num_bc_fld
        write(id_boundary_file,'(a)') '#'
        write(id_boundary_file,'(a)') '#   boundary condition data list'
        write(id_boundary_file,'(a)') '#'
!
        write(id_boundary_file,'(a)')   trim(bc_IO%ctls(igrp)%bc_field)
        write(id_boundary_file,'(a)')   trim(bc_IO%ctls(igrp)%bc_group)
        write(id_boundary_file,'(i16)')  bc_IO%ctls(igrp)%num_bc_mode
!
        do inum = 1, bc_IO%ctls(igrp)%num_bc_mode
          write(id_boundary_file,'(2i16,1p10E25.15e3)')                 &
     &      bc_IO%ctls(igrp)%imode_gl(1:2,inum),                        &
     &      bc_IO%ctls(igrp)%bc_input(inum,1:bc_IO%ctls(igrp)%ncomp_bc)
        end do
      end do
!
      close(id_boundary_file)
!
      end subroutine write_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_scalar_bc_by_file(field_name, sph_rj,        &
     &          bc_IO, ref_grp, bc_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use skip_comment_f
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_grp
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(      cmp_no_case(bc_IO%ctls(igrp)%bc_field, field_name)     &
     &     .and. cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          iflag_bc_scalar =  iflag_fixed_field
          call set_bc_for_sph_scalar_by_file                            &
     &              (bc_IO%ctls(igrp), sph_rj, bc_data)
        end if
      end do
!
      end subroutine set_fixed_scalar_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_gradient_bc_by_file(field_name, sph_rj,      &
     &          bc_IO, ref_grp, bc_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use skip_comment_f
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: ref_grp
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(     cmp_no_case(bc_IO%ctls(igrp)%bc_field, field_name)      &
     &    .and. cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          iflag_bc_scalar =  iflag_fixed_flux
          call set_bc_for_sph_scalar_by_file                            &
     &                (bc_IO%ctls(igrp), sph_rj, bc_data)
          return
        end if
      end do
!
      end subroutine set_fixed_gradient_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_for_sph_scalar_by_file(bc, sph_rj, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer(kind = 4) :: l, m
!
!
      do inum = 1, bc%num_bc_mode
        l = int(bc%imode_gl(1,inum))
        m = int(bc%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
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
!
!
      if(      cmp_no_case(bc%bc_field, fhd_velo)                       &
     &    .or. cmp_no_case(bc%bc_field, fhd_vort)                       &
     &    .or. cmp_no_case(bc%bc_field, fhd_magne))  bc%ncomp_bc = 2
!
      if(      cmp_no_case(bc%bc_field, fhd_temp)                       &
     &    .or. cmp_no_case(bc%bc_field, fhd_light)                      &
     &    .or. cmp_no_case(bc%bc_field, fhd_entropy)                    &
     &    .or. cmp_no_case(bc%bc_field, fhd_h_flux)                     &
     &    .or. cmp_no_case(bc%bc_field, fhd_c_flux))  bc%ncomp_bc = 1
!
      end subroutine set_num_comp_bc_data
!
! -----------------------------------------------------------------------
!
      end module t_sph_boundary_input_data
