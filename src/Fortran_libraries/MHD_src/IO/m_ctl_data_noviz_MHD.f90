!m_ctl_data_noviz_MHD.f90
!     module m_ctl_data_noviz_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!        Modified by H. Matsui on Oct., 2007
!        Modified by H. Matsui on Oct., 2012
!
!      subroutine read_control_4_MHD_noviz
!      subroutine read_control_4_snap_noviz
!
      module m_ctl_data_noviz_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit none
!
      integer(kind=kint), parameter :: control_file_code = 11
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
      character(len=kchara), parameter                                  &
     &                      :: snap_ctl_name = 'control_snapshot'
!
!   Top level of label
!
      character(len=kchara) :: hd_mhd_ctl = 'MHD_control'
      integer (kind=kint) :: i_mhd_ctl = 0
!
!   2nd level for MHD
!
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
!
      integer (kind=kint) :: i_model =        0
      integer (kind=kint) :: i_control =      0
!
      private :: MHD_ctl_name, snap_ctl_name
      private :: hd_mhd_ctl, i_mhd_ctl
!
      private :: hd_model, hd_control, i_model, i_control
!
      private :: read_sph_mhd_control_data
      private :: read_sph_mhd_model, read_sph_mhd_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_MHD_noviz
!
!
      ctl_file_code = control_file_code
      open ( ctl_file_code, file = MHD_ctl_name, status='old' )
!
      call load_ctl_label_and_line
      call read_sph_mhd_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_MHD_noviz
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_snap_noviz
!
!
      ctl_file_code = control_file_code
      open ( ctl_file_code, file = snap_ctl_name, status='old' )
!
      call load_ctl_label_and_line
      call read_sph_mhd_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_snap_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_control_data
!
      use calypso_mpi
      use m_ctl_data_4_platforms
      use m_ctl_data_node_monitor
      use m_ctl_data_4_pickup_sph
      use m_ctl_data_4_org_data
!
!
      if(right_begin_flag(hd_mhd_ctl) .eq. 0) return
      if (i_mhd_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_mhd_ctl, i_mhd_ctl)
        if(i_mhd_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
!
        call read_sph_mhd_model
        call read_sph_mhd_control
!
        call read_monitor_data_ctl
        call read_pickup_sph_ctl
      end do
!
      end subroutine read_sph_mhd_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_sph_mhd_model
!
      use m_ctl_data_4_fields
      use m_ctl_data_mhd_evolution
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_ctl_data_mhd_forces
      use m_ctl_data_mhd_normalize
      use m_ctl_data_temp_model
!
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_model, i_model)
        if(i_model .gt. 0) exit
!
        call read_phys_values
!
        call read_mhd_time_evo_ctl
        call read_mhd_layer_ctl
!
        call read_bc_4_node
        call read_bc_4_surf
!
        call read_forces_ctl
        call read_dimless_ctl
        call read_coef_term_ctl
!
        call read_gravity_ctl
        call read_coriolis_ctl
        call read_magneto_ctl
        call read_temp_def
      end do
!
      end subroutine read_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_mhd_control
!
      use m_ctl_data_4_time_steps
      use m_ctl_data_mhd_evo_scheme
!
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
!
        call read_time_step_ctl
        call read_restart_ctl
!
        call read_time_loop_ctl
      end do
!
      end subroutine read_sph_mhd_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_noviz_MHD
