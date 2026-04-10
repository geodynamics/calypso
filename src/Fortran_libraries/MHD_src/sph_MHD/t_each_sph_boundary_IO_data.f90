!> @file  t_each_sph_boundary_IO_data.f90
!!      module t_each_sph_boundary_IO_data
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine alloc_each_bc_item_ctl(bc_ctls)
!!      subroutine dealloc_each_bc_item_ctl(bc_ctls)
!!        type(each_boundary_spectr), intent(inout) :: bc_ctls
!!
!!      subroutine read_each_boundary_spectr(id_file, bc_ctls, iend)
!!        type(each_boundary_spectr), intent(inout) :: bc_ctls
!!        integer(kind = kint), intent(inout) :: iend
!!      subroutine write_each_boundary_spectr(id_file, bc_ctls)
!!        type(each_boundary_spectr), intent(in) :: bc_ctls
!!
!!  ---------------------------------------------------------------------
!!       line 2:     Field name to define the first boundary condition
!!       line 3:     Place to define the first boundary condition
!!       line 4:     Number of spherical harmonics modes 
!!                    for each boundary condition
!!       line 5...:  Spectrum data for the boundary conditions 
!!                  (degree $l$, order $m$, and harmonics coefficients)
!!        Return to 2...
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module t_each_sph_boundary_IO_data
!
      use m_precision
      use t_spheric_rj_data
      use t_boundary_sph_spectr
      use t_boundary_params_sph_MHD
!
      implicit  none
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_bc_item_ctl(bc_ctls)
!
      type(each_boundary_spectr), intent(inout) :: bc_ctls
!
!
      allocate(bc_ctls%imode_gl(2,bc_ctls%num_bc_mode))
      allocate(bc_ctls%bc_input(bc_ctls%num_bc_mode,bc_ctls%ncomp_bc))
!
      if(bc_ctls%num_bc_mode .gt. 0) then
        bc_ctls%imode_gl = 0
        bc_ctls%bc_input =  0.0d0
      end if
!
      end subroutine alloc_each_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_bc_item_ctl(bc_ctls)
!
      type(each_boundary_spectr), intent(inout) :: bc_ctls
!
!
      deallocate(bc_ctls%imode_gl, bc_ctls%bc_input)
!
      end subroutine dealloc_each_bc_item_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_each_boundary_spectr(id_file, bc_ctls, iend)
!
      use m_machine_parameter
      use set_sph_boundary_from_file
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(each_boundary_spectr), intent(inout) :: bc_ctls
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: inum
      character(len=255) :: tmpchara
!
!
      call skip_comment(id_file, tmpchara, iend)
      if(iend .gt. 0) return
      read(tmpchara,*)  bc_ctls%bc_field
!
      call skip_comment(id_file, tmpchara, iend)
      if(iend .gt. 0) return
      read(tmpchara,*) bc_ctls%bc_group
!
      call skip_comment(id_file, tmpchara, iend)
      if(iend .gt. 0) return
      read(tmpchara,*) bc_ctls%num_bc_mode
!
      bc_ctls%ncomp_bc = num_comp_bc_data(bc_ctls%bc_field)
      call alloc_each_bc_item_ctl(bc_ctls)
!
      do inum = 1, bc_ctls%num_bc_mode
        call skip_comment(id_file, tmpchara, iend)
        if(iend .gt. 0) return
        read(tmpchara,*) bc_ctls%imode_gl(1:2,inum),                    &
     &      bc_ctls%bc_input(inum,1:bc_ctls%ncomp_bc)
      end do
!
      end subroutine read_each_boundary_spectr
!
! -----------------------------------------------------------------------
!
      subroutine write_each_boundary_spectr(id_file, bc_ctls)
!
      integer(kind = kint), intent(in) :: id_file
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      integer(kind = kint) :: inum
!
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '#   boundary condition data list'
      write(id_file,'(a)') '#'
!
      write(id_file,'(a)')   trim(bc_ctls%bc_field)
      write(id_file,'(a)')   trim(bc_ctls%bc_group)
      write(id_file,'(i16)')  bc_ctls%num_bc_mode
!
      do inum = 1, bc_ctls%num_bc_mode
        write(id_file,'(2i16,1p10E25.15e3)')                            &
     &    bc_ctls%imode_gl(1:2,inum),                                   &
     &    bc_ctls%bc_input(inum,1:bc_ctls%ncomp_bc)
      end do
!
      end subroutine write_each_boundary_spectr
!
! -----------------------------------------------------------------------
!
      end module t_each_sph_boundary_IO_data
