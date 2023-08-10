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
!!      subroutine bcast_each_bc_item_ctl(bc_ctls)
!!      subroutine dealloc_each_bc_item_ctl(bc_ctls)
!!        type(each_boundary_spectr), intent(inout) :: bc_ctls
!!
!!      subroutine read_each_boundary_spectr(id_file, bc_ctls, iend)
!!        type(each_boundary_spectr), intent(inout) :: bc_ctls
!!        integer(kind = kint), intent(inout) :: iend
!!      subroutine write_each_boundary_spectr(id_file, bc_ctls)
!!        type(each_boundary_spectr), intent(in) :: bc_ctls
!!
!!      subroutine set_bc_4_sph_scalar_by_file(sph_rj, bc_ctls, bc_data)
!!      subroutine set_bc_4_sph_vector_by_file                          &
!!     &         (sph_rj, bc_ctls, vp_data, dp_data, vt_data)
!!      subroutine bc_4_evo_scalar_sph_by_file(sph_rj, bc_ctls, bc_data)
!!      subroutine bc_4_evo_vect2_sph_by_file                           &
!!     &         (sph_rj, bc_ctls, vp_data, vt_data)
!!      subroutine bc_4_evo_vector_sph_by_file(sph_rj, bc_ctls,         &
!!     &          vp_data, dp_data, vt_data)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(each_boundary_spectr), intent(in) :: bc_ctls
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
!
      subroutine bcast_each_bc_item_num(bc_ctls)
!
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(each_boundary_spectr), intent(inout) :: bc_ctls
!
!
      call calypso_mpi_bcast_character                                  &
     &   (bc_ctls%bc_group, cast_long(kchara), 0)
      call calypso_mpi_bcast_character                                  &
     &   (bc_ctls%bc_field, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(bc_ctls%ncomp_bc, 0)
      call calypso_mpi_bcast_one_int(bc_ctls%num_bc_mode, 0)
!
      end subroutine bcast_each_bc_item_num
!
! -----------------------------------------------------------------------
!
      subroutine bcast_each_bc_item_ctl(bc_ctls)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use transfer_to_long_integers
!
      type(each_boundary_spectr), intent(inout) :: bc_ctls
!
!
      call calypso_mpi_bcast_int                                        &
     &   (bc_ctls%imode_gl, cast_long(2*bc_ctls%num_bc_mode), 0)
      call calypso_mpi_bcast_real(bc_ctls%bc_input,                     &
     &    cast_long(bc_ctls%num_bc_mode*bc_ctls%ncomp_bc), 0)
!
      end subroutine bcast_each_bc_item_ctl
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
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_sph_scalar_by_file(sph_rj, bc_ctls, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) bc_data(j) =  bc_ctls%bc_input(inum,1)
      end do
!
      end subroutine set_bc_4_sph_scalar_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_sph_vector_by_file                            &
     &         (sph_rj, bc_ctls, vp_data, dp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: dp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          dp_data(j) = bc_ctls%bc_input(inum,2)
          vt_data(j) = bc_ctls%bc_input(inum,3)
        end if
      end do
!
      end subroutine set_bc_4_sph_vector_by_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bc_4_evo_scalar_sph_by_file(sph_rj, bc_ctls, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) bc_data(j) = bc_ctls%bc_input(inum,1)
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) bc_data(j) = bc_ctls%bc_input(inum,1)
      end do
!
      end subroutine bc_4_evo_scalar_sph_by_file
!
! -----------------------------------------------------------------------
!
      subroutine bc_4_evo_vect2_sph_by_file                             &
     &         (sph_rj, bc_ctls, vp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          vt_data(j) = bc_ctls%bc_input(inum,2)
        end if
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          vt_data(j) = bc_ctls%bc_input(inum,2)
        end if
      end do
!
      end subroutine bc_4_evo_vect2_sph_by_file
!
! -----------------------------------------------------------------------
!
      subroutine bc_4_evo_vector_sph_by_file(sph_rj, bc_ctls,           &
     &          vp_data, dp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: dp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          dp_data(j) = bc_ctls%bc_input(inum,2)
          vt_data(j) = bc_ctls%bc_input(inum,3)
        end if
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          dp_data(j) = bc_ctls%bc_input(inum,2)
          vt_data(j) = bc_ctls%bc_input(inum,3)
        end if
      end do
!
      end subroutine bc_4_evo_vector_sph_by_file
!
! -----------------------------------------------------------------------
!
      end module t_each_sph_boundary_IO_data
