!check_read_bc_file.f90
!      module check_read_bc_file
!
!      Written by H. Matsui on July, 2005
!
!      subroutine check_read_boundary_files
!
      module check_read_bc_file
!
      use m_precision
!
      implicit none
!
      private :: set_serch_boundary_file_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_read_boundary_files
!
      use calypso_mpi
      use m_control_parameter
      use m_bc_data_list
      use m_surf_data_list
!
!
      iflag_boundary_file = 0
!
! ----  read boundary data for temperature
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &    num_bc_e, ibc_e_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &    num_bc_h_flux, ibc_h_flux_type)
      end if
!
! ----  read boundary data for velocity
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_v, ibc_v_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_tq, ibc_tq_type)
!
!  set boundary conditions for pressure
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_p, ibc_p_type)
      end if
!
! ----  read boundary data for dummy scalar
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_composit, ibc_composit_type)
      end if
!
! ----  read boundary data for magnetic field
!
      if ( iflag_t_evo_4_magne .gt. id_no_evolution                     &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_b, ibc_b_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_bs, ibc_bs_type)
!
! ----  read boundary data for magnetic potential
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_mag_p, ibc_mag_p_type)
      end if
!
! ----  read boundary data for vector potential
!
      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_vp, ibc_vp_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &     num_bc_vps, ibc_vps_type)
!
! ----  read boundary data for magnetic potential
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_mag_p, ibc_mag_p_type)
      end if
!
! ----  read boundary data for current density
!
      if ( iflag_t_evo_4_magne .gt. id_no_evolution                     &
     &      .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_j, ibc_j_type)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      num_bc_js, ibc_js_type)
      end if
!
      end subroutine check_read_boundary_files
!
! -----------------------------------------------------------------------
!
       subroutine set_serch_boundary_file_flag(iflag, num_bc, ibc_type)
!
       integer (kind = kint), intent(inout) :: iflag
       integer (kind = kint), intent(in) :: num_bc
       integer (kind = kint), intent(in) :: ibc_type(num_bc)
!
       integer(kind = kint) :: i
!
       if (iflag .eq. 0 ) then
         if (num_bc .gt. 0) then
           do i = 1, num_bc
            if (ibc_type(i) .lt. 0) iflag = 1
           end do
         end if
       end if
!
       end subroutine set_serch_boundary_file_flag
!
! -----------------------------------------------------------------------
!
      end module check_read_bc_file
