!>@file   set_control_4_SPH_to_FEM.f90
!!@brief  module set_control_4_SPH_to_FEM
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!@endverbatim
!
!
      module set_control_4_SPH_to_FEM
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_sph_boundary_input_data
      use t_bc_data_list
!
      implicit none
!
       private :: bcast_boundary_spectr_file
       private :: bcast_each_bc_item_num, bcast_each_bc_item_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!
      use m_machine_parameter
      use calypso_mpi_int
      use check_read_bc_file
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(boundary_spectra), intent(inout) :: bc_IO
!
      integer(kind = kint) :: iflag, iend
!
!
      iflag = check_read_boundary_files(MHD_prop, MHD_BC)
      if (iflag .eq. id_no_boundary_file) return
!
      if(iflag_debug .gt. 0) write(*,*) 'read_boundary_spectr_file'
      if(my_rank .eq. 0) call read_boundary_spectr_file(bc_IO, iend)
      call calypso_mpi_bcast_one_int(iend, 0)
      if(iend .gt. 0) call calypso_MPI_abort(iend, e_message)
!
      call bcast_boundary_spectr_file(bc_IO)
!
      end subroutine sph_boundary_IO_control
!
! ----------------------------------------------------------------------
!
      subroutine bcast_boundary_spectr_file(bc_IO)
!
      use calypso_mpi_int
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
      subroutine bcast_each_bc_item_num(bc_ctls)
!
      use t_each_sph_boundary_IO_data
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
      use t_each_sph_boundary_IO_data
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
!
      end module set_control_4_SPH_to_FEM
