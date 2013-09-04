!m_ctl_data_surf_boundary.f90
!      module m_ctl_data_surf_boundary
!
!        programmed by H.Matsui on March, 2006
!        Modified by H.Matsui on Oct., 2007
!
!      subroutine deallocate_bc_h_flux_ctl
!      subroutine deallocate_bc_torque_ctl
!      subroutine deallocate_bc_press_sf_ctl
!      subroutine deallocate_bc_magne_sf_ctl
!      subroutine deallocate_bc_vecp_sf_ctl
!      subroutine deallocate_bc_current_sf_ctl
!      subroutine deallocate_bc_mag_p_sf_ctl
!      subroutine deallocate_sf_dscalar_ctl
!      subroutine deallocate_sf_infty_ctl
!
!      subroutine read_bc_4_surf
!
! ------------------------------------------------------------------
!   example
!
!    begin bc_4_surface
!!!!!  boundary condition for heat flux  !!!!!!!!!!!!!!!!!!!!!!!!!!
!  available type:  fixed, file, SGS_commute
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      array heat_flux_surf  2
!        heat_flux_surf  fixed       outer  0.000  end
!        heat_flux_surf  SGS_commute inner  0.000  end
!      end array heat_flux_surf
!!!!!  boundary condition for torque  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  available type
!     fix_x,  fix_y,  fix_z
!     file_x, file_y, file_z
!     normal_velocity
!     free_shell_in, free_shell_out
!     free_4_plane
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      array velocity_surf     2
!        velocity_surf  free_shell_in inner_surf   0.000  end
!        velocity_surf  free_shell_out  outer_surf   0.000  end
!      end array velocity_surf
!!!!!  boundary condition for pressure gradiend !!!!!!!!!!!!!!!!!!!
!  available type:  inner_shell, outer_shell
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array pressure_surf  2
!         pressure_surf   inner_shell inner_surf 0.000  end
!         pressure_surf   outer_shell outer_surf 0.000  end
!      end array pressure_surf
!!!!!  boundary condition for gradientof magnetic field  !!!!!!!!!!
!     fix_x,  fix_y,  fix_z
!     insulate_in, insulate_out (not recommended)
!     far_away                  (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array magnetic_field_surf  3
!          magnetic_field_surf  insulate_in  ICB_surf  0.000 end
!          magnetic_field_surf  insulate_out CMB_surf  0.000 end
!          magnetic_field_surf  far_away infinity_surf  0.000 end
!      end array magnetic_field_surf
!!!!!  boundary condition for gradientof magnetic field  !!!!!!!!!!
!     fix_x,  fix_y,  fix_z
!     insulate_in, insulate_out (not recommended)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array vector_potential_surf  1
!          vector_potential_surf  insulate_out CMB_surf  0.000 end
!      end array vector_potential_surf
!!!!!  boundary condition for current density on surface  !!!!!!!!!!
!     fix_x,  fix_y,  fix_z
!     insulate_in,insulate_out (not recommended)
!     far_away                  (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array current_surf  3
!          current_surf  insulate_in  ICB_surf  0.000 end
!          current_surf  insulate_out CMB_surf  0.000 end
!          current_surf  far_away infinity_surf  0.000 end
!      end array current_surf
!!!!!  boundary condition for magnetic potential !!!!!!!!!!!!!!!!!
!  available type:  fixed (not used), file (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array electric_potential_surf  1
!          electric_potential_surf  insulate_in  ICB_surf  0.000 end
!          electric_potential_surf  insulate_out CMB_surf  0.000 end
!          electric_potential_surf  far_away infinity_surf  0.000 end
!      end array electric_potential_surf
!!!!!  boundary condition for dummy scalar !!!!!!!!!!!!!!!!!
!  available type:  fixed_grad (not used), file_grad (not used)
!                   fixed_field
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       array composition_surf  3
!          composition_surf  insulate_in  ICB_surf  0.000 end
!          composition_surf  insulate_out CMB_surf  0.000 end
!          composition_surf  far_away infinity_surf  0.000 end
!      end array composition_surf
!!!!!  boundary condition for infinity (obsolute) !!!!!!!!!!!!!!!!!
!  available type:  fixed (not used), file (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      array infinity_surf 1
!        infinity_surf  fixed infinity_surf  0.000  end
!      end array infinity_surf
!    end  bc_4_surface
!
! ------------------------------------------------------------------
!
      module m_ctl_data_surf_boundary
!
      use m_precision
!
      use m_machine_parameter
!
      implicit  none
!
!
      integer(kind=kint) :: num_bc_h_flux_ctl = 0
      character (len=kchara), allocatable :: bc_h_flux_name_ctl(:)
      character (len=kchara), allocatable :: bc_h_flux_type_ctl(:)
      real (kind=kreal), allocatable :: bc_h_flux_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_torque_ctl = 0
      character (len=kchara), allocatable :: bc_torque_name_ctl(:)
      character (len=kchara), allocatable :: bc_torque_type_ctl(:)
      real (kind=kreal), allocatable :: bc_torque_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_grad_p_ctl = 0
      character (len=kchara), allocatable :: bc_grad_p_name_ctl(:)
      character (len=kchara), allocatable :: bc_grad_p_type_ctl(:)
      real (kind=kreal), allocatable :: bc_grad_p_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_grad_b_ctl = 0
      character (len=kchara), allocatable :: bc_grad_b_name_ctl(:)
      character (len=kchara), allocatable :: bc_grad_b_type_ctl(:)
      real (kind=kreal), allocatable :: bc_grad_b_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_grad_j_ctl = 0
      character (len=kchara), allocatable :: bc_grad_j_name_ctl(:)
      character (len=kchara), allocatable :: bc_grad_j_type_ctl(:)
      real (kind=kreal), allocatable :: bc_grad_j_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_vps_ctl = 0
      character (len=kchara), allocatable :: bc_vps_name_ctl(:)
      character (len=kchara), allocatable :: bc_vps_type_ctl(:)
      real (kind=kreal), allocatable :: bc_vps_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_grad_magp_ctl = 0
      character (len=kchara), allocatable :: bc_grad_magp_name_ctl(:)
      character (len=kchara), allocatable :: bc_grad_magp_type_ctl(:)
      real (kind=kreal), allocatable :: bc_grad_magp_magnitude_ctl(:)
!
      integer(kind=kint) :: num_bc_grad_ds_ctl = 0
      character (len=kchara), allocatable :: bc_grad_ds_name_ctl(:)
      character (len=kchara), allocatable :: bc_grad_ds_type_ctl(:)
      real (kind=kreal), allocatable :: bc_grad_ds_magnitude_ctl(:)
!
!
      integer(kind=kint) :: num_bc_infinity_ctl = 0
      character (len=kchara), allocatable :: bc_infinity_name_ctl(:)
      character (len=kchara), allocatable :: bc_infinity_type_ctl(:)
      real (kind=kreal), allocatable :: bc_infinity_magnitude_ctl(:)
      integer(kind=kint), allocatable                                   &
     &                   :: ibc_infinity_surface_num_ctl(:)
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_surf =    'bc_4_surface'
      integer (kind=kint) :: i_bc_4_surf =     0
!
!   4th level for surface boundary
!
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_hf =     'heat_flux_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_mf =     'velocity_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_gradp =  'pressure_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_gradb =  'magnetic_field_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_grada =  'vector_potential_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_gradj =  'current_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_gradmp = 'electric_potential_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_gradds = 'dummy_scalar_surf'
      character(len=kchara), parameter                                  &
     &       :: hd_n_bc_infty =  'infinity_surf'
!
      integer (kind=kint) :: i_n_bc_hf =     0
      integer (kind=kint) :: i_n_bc_mf =     0
      integer (kind=kint) :: i_n_bc_gradp =  0
      integer (kind=kint) :: i_n_bc_gradb =  0
      integer (kind=kint) :: i_n_bc_grada =  0
      integer (kind=kint) :: i_n_bc_gradj =  0
      integer (kind=kint) :: i_n_bc_gradmp = 0
      integer (kind=kint) :: i_n_bc_gradds = 0
      integer (kind=kint) :: i_n_bc_infty =  0
!
!
      private :: hd_bc_4_surf, i_bc_4_surf
      private :: hd_n_bc_hf, hd_n_bc_gradds, hd_n_bc_infty
      private :: hd_n_bc_mf, hd_n_bc_gradp, hd_n_bc_gradmp
      private :: hd_n_bc_gradb, hd_n_bc_grada, hd_n_bc_gradj
!
      private :: allocate_bc_h_flux_ctl
      private :: allocate_bc_torque_ctl
      private :: allocate_bc_press_sf_ctl
      private :: allocate_bc_magne_sf_ctl
      private :: allocate_bc_vecp_sf_ctl
      private :: allocate_bc_current_sf_ctl
      private :: allocate_bc_m_potential_sf_ctl
      private :: allocate_bc_d_scalar_sf_ctl
      private :: allocate_bc_infinity_sf_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
       subroutine allocate_bc_h_flux_ctl
!
        allocate(bc_h_flux_magnitude_ctl( num_bc_h_flux_ctl))
        allocate(bc_h_flux_name_ctl(num_bc_h_flux_ctl))
        allocate(bc_h_flux_type_ctl(num_bc_h_flux_ctl))
        bc_h_flux_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_h_flux_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_torque_ctl
!
        allocate(bc_torque_magnitude_ctl( num_bc_torque_ctl))
        allocate(bc_torque_name_ctl(num_bc_torque_ctl))
        allocate(bc_torque_type_ctl(num_bc_torque_ctl))
        bc_torque_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_torque_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_press_sf_ctl
!
        allocate(bc_grad_p_magnitude_ctl( num_bc_grad_p_ctl))
        allocate(bc_grad_p_name_ctl(num_bc_grad_p_ctl))
        allocate(bc_grad_p_type_ctl(num_bc_grad_p_ctl))
        bc_grad_p_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_press_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magne_sf_ctl
!
        allocate(bc_grad_b_magnitude_ctl( num_bc_grad_b_ctl))
        allocate(bc_grad_b_name_ctl(num_bc_grad_b_ctl))
        allocate(bc_grad_b_type_ctl(num_bc_grad_b_ctl))
        bc_grad_b_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_magne_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vecp_sf_ctl
!
        allocate(bc_vps_magnitude_ctl( num_bc_vps_ctl))
        allocate(bc_vps_name_ctl(num_bc_vps_ctl))
        allocate(bc_vps_type_ctl(num_bc_vps_ctl))
        bc_vps_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_vecp_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_current_sf_ctl
!
        allocate(bc_grad_j_magnitude_ctl( num_bc_grad_j_ctl))
        allocate(bc_grad_j_name_ctl(num_bc_grad_j_ctl))
        allocate(bc_grad_j_type_ctl(num_bc_grad_j_ctl))
        bc_grad_j_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_current_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_m_potential_sf_ctl
!
        allocate(bc_grad_magp_magnitude_ctl( num_bc_grad_magp_ctl))
        allocate(bc_grad_magp_name_ctl(num_bc_grad_magp_ctl))
        allocate(bc_grad_magp_type_ctl(num_bc_grad_magp_ctl))
        bc_grad_magp_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_m_potential_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_d_scalar_sf_ctl
!
        allocate(bc_grad_ds_magnitude_ctl( num_bc_grad_ds_ctl))
        allocate(bc_grad_ds_name_ctl(num_bc_grad_ds_ctl))
        allocate(bc_grad_ds_type_ctl(num_bc_grad_ds_ctl))
        bc_grad_ds_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_d_scalar_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_infinity_sf_ctl
!
        allocate(bc_infinity_magnitude_ctl( num_bc_infinity_ctl))
        allocate(bc_infinity_name_ctl(num_bc_infinity_ctl))
        allocate(bc_infinity_type_ctl(num_bc_infinity_ctl))
        bc_infinity_magnitude_ctl = 0.0d0
!
       end subroutine allocate_bc_infinity_sf_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_h_flux_ctl
!
        deallocate(bc_h_flux_magnitude_ctl)
        deallocate(bc_h_flux_name_ctl, bc_h_flux_type_ctl)
!
       end subroutine deallocate_bc_h_flux_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_torque_ctl
!
        deallocate(bc_torque_magnitude_ctl)
        deallocate(bc_torque_name_ctl, bc_torque_type_ctl)
!
       end subroutine deallocate_bc_torque_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_press_sf_ctl
!
        deallocate(bc_grad_p_magnitude_ctl)
        deallocate(bc_grad_p_name_ctl, bc_grad_p_type_ctl)
!
       end subroutine deallocate_bc_press_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_magne_sf_ctl
!
        deallocate(bc_grad_b_magnitude_ctl)
        deallocate(bc_grad_b_name_ctl, bc_grad_b_type_ctl)
!
       end subroutine deallocate_bc_magne_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_vecp_sf_ctl
!
        deallocate(bc_vps_magnitude_ctl)
        deallocate(bc_vps_name_ctl, bc_vps_type_ctl)
!
       end subroutine deallocate_bc_vecp_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_current_sf_ctl
!
        deallocate(bc_grad_j_magnitude_ctl)
        deallocate(bc_grad_j_name_ctl, bc_grad_j_type_ctl)
!
       end subroutine deallocate_bc_current_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_bc_mag_p_sf_ctl
!
        deallocate(bc_grad_magp_magnitude_ctl)
        deallocate(bc_grad_magp_name_ctl, bc_grad_magp_type_ctl)
!
       end subroutine deallocate_bc_mag_p_sf_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_sf_dscalar_ctl
!
        deallocate(bc_grad_ds_magnitude_ctl)
        deallocate(bc_grad_ds_name_ctl, bc_grad_ds_type_ctl)
!
       end subroutine deallocate_sf_dscalar_ctl
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_sf_infty_ctl
!
        deallocate(bc_infinity_magnitude_ctl)
        deallocate(bc_infinity_name_ctl, bc_infinity_type_ctl)
!
       end subroutine deallocate_sf_infty_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_bc_4_surf
!
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_bc_4_surf) .eq. 0) return
      if (i_bc_4_surf.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_bc_4_surf, i_bc_4_surf)
        if(i_bc_4_surf .gt. 0) exit
!
!-------read b.c. for heat flux ---------------------------
!
        call find_control_array_flag(hd_n_bc_hf, num_bc_h_flux_ctl)
        if(num_bc_h_flux_ctl.gt.0 .and. i_n_bc_hf.eq.0) then
          call allocate_bc_h_flux_ctl
          call read_control_array_c2_r_list(hd_n_bc_hf,                 &
     &        num_bc_h_flux_ctl, i_n_bc_hf, bc_h_flux_type_ctl,         &
     &        bc_h_flux_name_ctl, bc_h_flux_magnitude_ctl)
        end if
!
!-------read b.c. for torque ---------------------------
!
        call find_control_array_flag(hd_n_bc_mf, num_bc_torque_ctl)
        if(num_bc_torque_ctl.gt.0 .and. i_n_bc_mf.eq.0) then
          call allocate_bc_torque_ctl
          call read_control_array_c2_r_list(hd_n_bc_mf,                 &
     &        num_bc_torque_ctl, i_n_bc_mf, bc_torque_type_ctl,         &
     &        bc_torque_name_ctl, bc_torque_magnitude_ctl)
        end if
!
!-------read b.c. for pressure gradient ---------------------------
!
        call find_control_array_flag(hd_n_bc_gradp, num_bc_grad_p_ctl)
        if(num_bc_grad_p_ctl.gt.0 .and. i_n_bc_gradp.eq.0) then
          call allocate_bc_press_sf_ctl
          call read_control_array_c2_r_list(hd_n_bc_gradp,              &
     &        num_bc_grad_p_ctl, i_n_bc_gradp, bc_grad_p_type_ctl,      &
     &        bc_grad_p_name_ctl, bc_grad_p_magnitude_ctl)
        end if
!
!-------read b.c. for gradient of magnetic field----------------
!
        call find_control_array_flag(hd_n_bc_gradb, num_bc_grad_b_ctl)
        if(num_bc_grad_b_ctl.gt.0 .and. i_n_bc_gradb.eq.0) then
          call allocate_bc_magne_sf_ctl
          call read_control_array_c2_r_list(hd_n_bc_gradb,              &
     &        num_bc_grad_b_ctl, i_n_bc_gradb, bc_grad_b_type_ctl,      &
     &        bc_grad_b_name_ctl, bc_grad_b_magnitude_ctl)
        end if
!
!-------read b.c. for gradient of vector potential ----------------
!
        call find_control_array_flag(hd_n_bc_grada, num_bc_vps_ctl)
        if(num_bc_vps_ctl.gt.0 .and. i_n_bc_grada.eq.0) then
          call allocate_bc_vecp_sf_ctl
          call read_control_array_c2_r_list(hd_n_bc_grada,              &
     &        num_bc_vps_ctl, i_n_bc_grada, bc_vps_type_ctl,            &
     &        bc_vps_name_ctl, bc_vps_magnitude_ctl)
        end if
!
!-------read b.c. for current density on surface ----------------
!
        call find_control_array_flag(hd_n_bc_gradj, num_bc_grad_j_ctl)
        if(num_bc_grad_j_ctl.gt.0 .and. i_n_bc_gradj.eq.0) then
          call allocate_bc_current_sf_ctl
          call read_control_array_c2_r_list(hd_n_bc_gradj,              &
     &        num_bc_grad_j_ctl, i_n_bc_gradj, bc_grad_j_type_ctl,      &
     &        bc_grad_j_name_ctl, bc_grad_j_magnitude_ctl)
        end if
!
!-------read b.c. for magnetic potential on surface ----------------
!
        call find_control_array_flag(hd_n_bc_gradmp,                    &
     &      num_bc_grad_magp_ctl)
        if(num_bc_grad_magp_ctl.gt.0 .and. i_n_bc_gradmp.eq.0) then
          call allocate_bc_m_potential_sf_ctl
          call read_control_array_c2_r_list(hd_n_bc_gradmp,             &
     &        num_bc_grad_magp_ctl, i_n_bc_gradmp,                      &
     &        bc_grad_magp_type_ctl, bc_grad_magp_name_ctl,             &
     &        bc_grad_magp_magnitude_ctl)
        end if
!
!-------read b.c. for dummy scalar on surface ----------------
!
        call find_control_array_flag(hd_n_bc_gradds,                    &
     &      num_bc_grad_ds_ctl)
        if(num_bc_grad_ds_ctl.gt.0 .and. i_n_bc_gradds.eq.0) then
          call allocate_bc_d_scalar_sf_ctl
          call read_control_array_c2_r_list(hd_n_bc_gradds,             &
     &        num_bc_grad_ds_ctl, i_n_bc_gradds, bc_grad_ds_type_ctl,   &
     &        bc_grad_ds_name_ctl, bc_grad_ds_magnitude_ctl)
        end if
!
!-------read b.c. for infinity----------------
!
        call find_control_array_flag(hd_n_bc_infty,                     &
     &      num_bc_infinity_ctl)
        if(num_bc_infinity_ctl.gt.0 .and. i_n_bc_infty.eq.0) then
          call allocate_bc_infinity_sf_ctl
          call read_control_array_c2_r_list(hd_n_bc_infty,              &
     &        num_bc_infinity_ctl, i_n_bc_infty, bc_infinity_type_ctl,  &
     &        bc_infinity_name_ctl, bc_infinity_magnitude_ctl)
        end if
!
      end do
!
      end subroutine read_bc_4_surf
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_surf_boundary
