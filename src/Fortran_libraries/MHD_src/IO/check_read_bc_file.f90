!> @file  check_read_bc_file.f90
!!      module check_read_bc_file
!!
!! @author  H. Matsui
!! @date Programmed in July, 2005
!
!> @brief Decide if boundary condition data field is read
!!
!!@verbatim
!!      integer(kind=kint) function check_read_boundary_files           &
!!     &                          (MHD_prop, MHD_BC)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!@endverbatim
!
      module check_read_bc_file
!
      use m_precision
      use t_bc_data_list
!
      implicit none
!
!>      ID not to read external boundary condition file
      integer (kind=kint), parameter :: id_no_boundary_file =   0
!>      ID to read external boundary condition file
      integer (kind=kint), parameter :: id_read_boundary_file = 1
!
      private :: set_serch_boundary_file_flag
      private :: chk_read_boundary_files
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind=kint) function check_read_boundary_files             &
     &                          (MHD_prop, MHD_BC)
!
      use t_control_parameter
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
!
      check_read_boundary_files                                         &
     &   = chk_read_boundary_files(MHD_prop%fl_prop, MHD_prop%cd_prop,  &
     &                             MHD_prop%ht_prop, MHD_prop%cp_prop,  &
     &                             MHD_BC)
!
      end function check_read_boundary_files
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function chk_read_boundary_files             &
     &                   (fl_prop, cd_prop, ht_prop, cp_prop, MHD_BC)
!
      use calypso_mpi
      use t_physical_property
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      integer(kind = kint) :: iflag_boundary_file
!
      iflag_boundary_file = 0
!
! ----  read boundary data for temperature
!
      if ( ht_prop%iflag_scheme .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%temp_BC%nod_BC)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%temp_BC%surf_BC)
      end if
!
! ----  read boundary data for velocity
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%velo_BC%nod_BC)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%velo_BC%surf_BC)
!
!  set boundary conditions for pressure
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%press_BC%nod_BC)
      end if
!
! ----  read boundary data for dummy scalar
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%light_BC%nod_BC)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%light_BC%surf_BC)
      end if
!
! ----  read boundary data for magnetic field
!
      if (    cd_prop%iflag_Bevo_scheme .gt. id_no_evolution            &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%magne_BC%nod_BC)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%magne_BC%surf_BC)
!
! ----  read boundary data for magnetic potential
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%e_potential_BC%nod_BC)
      end if
!
! ----  read boundary data for vector potential
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%a_potential_BC%nod_BC)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%a_potential_BC%surf_BC)
!
! ----  read boundary data for magnetic potential
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%e_potential_BC%nod_BC)
      end if
!
! ----  read boundary data for current density
!
      if (     cd_prop%iflag_Bevo_scheme .gt. id_no_evolution           &
     &    .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
!
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%current_BC%nod_BC)
        call set_serch_boundary_file_flag(iflag_boundary_file,          &
     &      MHD_BC%current_BC%surf_BC)
      end if
!
      chk_read_boundary_files = iflag_boundary_file
!
      end function chk_read_boundary_files
!
! -----------------------------------------------------------------------
!
      subroutine set_serch_boundary_file_flag(iflag, bc_list)
!
      type(boundary_condition_list), intent(in) :: bc_list
      integer (kind = kint), intent(inout) :: iflag
!
      integer(kind = kint) :: i
!
      if (iflag .eq. 0 ) then
        if(bc_list%num_bc .gt. 0) then
          do i = 1, bc_list%num_bc
           if(bc_list%ibc_type(i) .lt. 0) iflag = 1
          end do
        end if
      end if
!
       end subroutine set_serch_boundary_file_flag
!
! -----------------------------------------------------------------------
!
      end module check_read_bc_file
