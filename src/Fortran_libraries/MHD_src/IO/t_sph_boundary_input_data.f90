!> @file  t_sph_boundary_input_data.f90
!!      module t_sph_boundary_input_data
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine dealloc_sph_bc_item_ctl(bc_IO)
!!      subroutine bcast_boundary_spectr_file(bc_IO)
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!
!!      subroutine read_boundary_spectr_file(bc_IO, iend)
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!        integer(kind = kint), intent(inout) :: iend
!!      subroutine write_boundary_spectr_file(bc_IO)
!!        type(boundary_spectra), intent(in) :: bc_IO
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
      use t_each_sph_boundary_IO_data
!
      implicit  none
!
!>      File ID for boundary condition file
      integer(kind = kint), parameter :: id_boundary_file = 41
!
      type boundary_spectra
!>        File name for boundary condition file
        character(len=kchara) :: file_name = 'boundary_spectr.dat'
!>        Name of boundary conditions to set
        integer(kind = kint) :: num_bc_fld = 0
!>        Structures for boundary conditions
        type(each_boundary_spectr), allocatable :: ctls(:)
      end type boundary_spectra
!
!
      private :: id_boundary_file
      private :: alloc_sph_bc_item_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
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
!
      subroutine bcast_boundary_spectr_file(bc_IO)
!
      use calypso_mpi_int
      use calypso_mpi
!
      type(boundary_spectra), intent(inout) :: bc_IO
!
      integer(kind = kint) :: igrp
!
!
      call calypso_mpi_bcast_one_int(bc_IO%num_bc_fld,  0)
      if(my_rank .ne. 0) call alloc_sph_bc_item_ctl(bc_IO)
      call calypso_mpi_barrier
!
      do igrp = 1, bc_IO%num_bc_fld
        call bcast_each_bc_item_num(bc_IO%ctls(igrp))
        if(my_rank .ne. 0) then
          call alloc_each_bc_item_ctl(bc_IO%ctls(igrp))
        end if
        call bcast_each_bc_item_ctl(bc_IO%ctls(igrp))
      end do
!
      end subroutine bcast_boundary_spectr_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_boundary_spectr_file(bc_IO, iend)
!
      use m_machine_parameter
      use skip_comment_f
!
      type(boundary_spectra), intent(inout) :: bc_IO
      integer(kind = kint), intent(inout) :: iend
!
      integer(kind = kint) :: igrp
      character(len=255) :: tmpchara
!
!
      if(iflag_debug .gt. 0) write(*,*) 'read boundary condition: ',    &
     &                      trim(bc_IO%file_name)
      open(id_boundary_file, file=bc_IO%file_name)
!
      call skip_comment(id_boundary_file, tmpchara, iend)
      if(iend .gt. 0) return
      read(tmpchara,*) bc_IO%num_bc_fld
!
      call alloc_sph_bc_item_ctl(bc_IO)
!
      do igrp = 1, bc_IO%num_bc_fld
        call read_each_boundary_spectr                                  &
     &     (id_boundary_file, bc_IO%ctls(igrp), iend)
        if(iend .gt. 0) return
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
      integer(kind = kint) :: igrp
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
        call write_each_boundary_spectr                                 &
     &     (id_boundary_file, bc_IO%ctls(igrp))
      end do
!
      close(id_boundary_file)
!
      end subroutine write_boundary_spectr_file
!
! -----------------------------------------------------------------------
!
      end module t_sph_boundary_input_data
