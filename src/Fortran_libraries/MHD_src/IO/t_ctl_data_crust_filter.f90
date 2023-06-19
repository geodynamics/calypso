!>@file   t_ctl_data_crust_filter.f90
!!@brief  module t_ctl_data_crust_filter
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2017
!
!> @brief Control data structure for zonal mean visualization controls
!!
!!@verbatim
!!      subroutine read_crustal_filtering_ctl                           &
!!     &         (id_control, hd_block, crust_filter_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(clust_filtering_ctl), intent(inout) :: crust_filter_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_crustal_filtering_ctl                          &
!!     &         (id_control, hd_block, crust_filter_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(clust_filtering_ctl), intent(in) :: crust_filter_c
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine reset_crustal_filtering_ctl(crust_filter_c)
!!        type(clust_filtering_ctl), intent(inout) :: crust_filter_c
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin dynamo_vizs_control
!!    begin crustal_filtering_ctl
!!      truncation_degree_ctl        13
!!    end crustal_filtering_ctl
!!  end dynamo_vizs_control
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_ctl_data_crust_filter
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_array_integer
      use t_control_data_sections
!
      implicit  none
!
!
!>      Structure of crustal filtering of mangeitc field
      type clust_filtering_ctl
!>        Truncation dgree by crustal field
        type(read_integer_item) :: crust_truncation_ctl
!
        integer (kind=kint) :: i_crustal_filtering = 0
      end type clust_filtering_ctl
!
!
      character(len=kchara), parameter                                  &
     &             :: hd_crustal_truncation = 'truncation_degree_ctl'
!
      private :: hd_crustal_truncation
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_crustal_filtering_ctl                             &
     &         (id_control, hd_block, crust_filter_c, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(clust_filtering_ctl), intent(inout) :: crust_filter_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(crust_filter_c%i_crustal_filtering .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_integer_ctl_type(c_buf, hd_crustal_truncation,        &
     &      crust_filter_c%crust_truncation_ctl)
       end do
       crust_filter_c%i_crustal_filtering = 1
!
      end subroutine read_crustal_filtering_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_crustal_filtering_ctl                            &
     &         (id_control, hd_block, crust_filter_c, level)
!
      use t_read_control_elements
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(clust_filtering_ctl), intent(in) :: crust_filter_c
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(crust_filter_c%i_crustal_filtering .le. 0) return
!
      maxlen = len_trim(hd_crustal_truncation)
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &      hd_crustal_truncation, crust_filter_c%crust_truncation_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_crustal_filtering_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_crustal_filtering_ctl(crust_filter_c)
!
      type(clust_filtering_ctl), intent(inout) :: crust_filter_c
!
!
      crust_filter_c%crust_truncation_ctl%iflag = 0
      crust_filter_c%i_crustal_filtering = 0
!
      end subroutine reset_crustal_filtering_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_crust_filter
