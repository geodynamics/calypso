!m_control_param_newsph.f90
!      module m_control_param_newsph
!
!      Written by H. Matsui
!
!      subroutine set_control_4_newsph
!
      module m_control_param_newsph
!
      use m_precision
!
      implicit    none
!
!
      character(len=kchara) :: org_sph_fst_head = "restart/rst"
      character(len=kchara) :: new_sph_fst_head = "rst_new/rst"
!
      integer(kind=kint ) :: istep_start, istep_end, increment_step
!
      character(len=kchara) :: new_sph_head = 'mesh_new/in_rj'
!
!
      integer(kind=kint ) :: ifmt_org_sph_file =      0
      integer(kind=kint ) :: ifmt_new_sph_file =      0
!
      integer(kind=kint ) :: iflag_delete_org_sph =   0
      real(kind = kreal) :: b_sph_ratio
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_control_4_newsph
!
      use m_control_data_4_merge
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_ctl_data_4_time_steps
      use m_merge_spheric_mesh
      use m_node_id_spherical_IO
      use m_field_data_IO
      use m_file_format_switch
      use m_ucd_data
      use set_control_platform_data
      use ucd_IO_select
!
!
      if (i_num_subdomain .gt. 0) then
        np_sph_org = num_subdomain_ctl
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      call set_control_sph_mesh
      call set_control_restart_file_def
      call set_control_ucd_file_def
!
      ifmt_org_sph_file =     iflag_sph_file_fmt
!
      if (i_num_new_domain .gt. 0) then
        np_sph_new = num_new_domain_ctl
      else
        write(*,*) 'Set number of subdomains for new grid'
        stop
      end if
!
      if (i_new_sph_mode_head .gt. 0) then
        new_sph_head = new_sph_mode_prefix
      end if
!
      call choose_file_format(new_sph_file_fmt_ctl,                     &
     &     i_new_sph_files_fmt, ifmt_new_sph_file)
!
!
      if (i_rst_header .gt. 0) then
        org_sph_fst_head = restart_file_prefix
      end if
!
      if (i_new_rst_head .gt. 0) then
        new_sph_fst_head = new_restart_prefix
      end if
!
      if(i_del_org_data .gt. 0) then
        if(    del_org_data_ctl .eq. 'on'                               &
     &    .or. del_org_data_ctl .eq. 'On'                               &
     &    .or. del_org_data_ctl .eq. 'ON'                               &
     &    .or. del_org_data_ctl .eq. 'yes'                              &
     &    .or. del_org_data_ctl .eq. 'Yes'                              &
     &    .or. del_org_data_ctl .eq. 'YES') then
          iflag_delete_org_sph = 1
        end if
      end if
!
      if (i_newrst_magne .gt. 0) then
        b_sph_ratio = magnetic_field_ratio_ctl
      else
        b_sph_ratio = 1.0d0
      end if
!
      istep_start =    i_step_init_ctl
      istep_end =      i_step_number_ctl
      increment_step = i_step_rst_ctl
!
      write(*,*) 'istep_start, istep_end, increment_step',              &
     &           istep_start, istep_end, increment_step
!
      end subroutine set_control_4_newsph
!
! -----------------------------------------------------------------------
!
      end module m_control_param_newsph
