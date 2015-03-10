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
      integer(kind = kint) :: np_sph_org
      integer(kind = kint) :: np_sph_new
!
      character(len=kchara) :: org_sph_fst_head = "restart/rst"
      character(len=kchara) :: new_sph_fst_head = "rst_new/rst"
!
      integer(kind=kint ) :: istep_start, istep_end, increment_step
!
      character(len=kchara) :: org_sph_head = 'mesh_org/in_rj'
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
      use m_node_id_spherical_IO
      use m_field_data_IO
      use m_file_format_switch
      use m_ucd_data
      use set_control_platform_data
      use ucd_IO_select
      use skip_comment_f
!
!
      if (ndomain_ctl%iflag .gt. 0) then
        np_sph_org = ndomain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains'
        stop
      end if
!
      if (num_new_domain_ctl%iflag .gt. 0) then
        np_sph_new = num_new_domain_ctl%intvalue
      else
        write(*,*) 'Set number of subdomains for new grid'
        stop
      end if
!
      call set_control_restart_file_def
      call set_control_ucd_file_def
!
      if(sph_file_prefix%iflag .gt. 0) then
        org_sph_head = sph_file_prefix%charavalue
      end if
      if (new_sph_mode_prefix%iflag .gt. 0) then
        new_sph_head = new_sph_mode_prefix%charavalue
      end if
!
      call choose_file_format(sph_file_fmt_ctl%charavalue,              &
     &    sph_file_fmt_ctl%iflag, ifmt_org_sph_file)
      call choose_file_format(new_sph_file_fmt_ctl%charavalue,          &
     &    new_sph_file_fmt_ctl%iflag, ifmt_new_sph_file)
!
!
      if (restart_file_prefix%iflag .gt. 0) then
        org_sph_fst_head = restart_file_prefix%charavalue
      end if
!
      if (new_restart_prefix%iflag .gt. 0) then
        new_sph_fst_head = new_restart_prefix%charavalue
      end if
!
      if(del_org_data_ctl%iflag .gt. 0) then
        if(yes_flag(del_org_data_ctl%charavalue)) then
          iflag_delete_org_sph = 1
        end if
      end if
!
      b_sph_ratio = 1.0d0
      if (i_newrst_magne .gt. 0) then
        b_sph_ratio = magnetic_field_ratio_ctl
      end if
!
      istep_start = 1
      if(i_step_init_ctl%iflag .gt. 0) then
        istep_start = i_step_init_ctl%intvalue
      end if
!
      istep_end =  1
      if(i_step_number_ctl%iflag .gt. 0) then
        istep_end = i_step_number_ctl%intvalue
      end if
!
      increment_step = 1
      if (i_step_rst_ctl%iflag .gt. 0) then
        increment_step = i_step_rst_ctl%intvalue
      end if
!
      write(*,*) 'istep_start, istep_end, increment_step',              &
     &           istep_start, istep_end, increment_step
!
      end subroutine set_control_4_newsph
!
! -----------------------------------------------------------------------
!
      end module m_control_param_newsph
