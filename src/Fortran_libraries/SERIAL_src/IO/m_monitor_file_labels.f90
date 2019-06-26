!>@file  m_monitor_file_labels.f90
!!       module m_monitor_file_labels
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Comments for mesh data
!!
!!@verbatim
!!      character(len=ilen_time_label) function hd_time_label()
!!      character(len=ilen_time_sph_label) function hd_time_sph_label()
!!      character(len=ilen_pick_sph_head) function hd_pick_sph_head()
!!      character(len=ilen_pick_sph_num) function hd_pick_sph_num()
!!      character(len=ilen_pk_gauss_head) function hd_pick_gauss_head()
!!@endverbatim
!
      module m_monitor_file_labels
!
      use m_precision
!
!>      length of hd_fem_para
      integer(kind = kint), parameter :: ilen_time_label = 18
!>      length of hd_fem_para
      integer(kind = kint), parameter :: ilen_time_sph_label = 18+23+19
!>      length of hd_fem_para
      integer(kind = kint), parameter :: ilen_pick_sph_head = 1+24+2
!>      length of hd_fem_para
      integer(kind = kint), parameter :: ilen_pick_sph_num = 21+1
!>      length of hd_fem_para
      integer(kind = kint), parameter :: ilen_pk_gauss_head = 28+1
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      character(len=ilen_time_label) function hd_time_label()
!
      hd_time_label = 't_step    time    '
!
      end function hd_time_label
!
!------------------------------------------------------------------
!
      character(len=ilen_time_sph_label) function hd_time_sph_label()
!
      hd_time_sph_label = 't_step    time    '                          &
     &                 // 'radius_ID    radius    '                     &
     &                //  'degree    order    '
!
      end function hd_time_sph_label
!
!------------------------------------------------------------------
!
      character(len=ilen_pick_sph_head) function hd_pick_sph_head()
!
      hd_pick_sph_head = '#' // char(10)                                &
     &                // '# num_layers, num_spectr' // char(10)
!
      end function hd_pick_sph_head
!
!------------------------------------------------------------------
!
      character(len=ilen_pick_sph_num) function hd_pick_sph_num()
!
      hd_pick_sph_num = '# number of component' // char(10)
!
      end function hd_pick_sph_num
!
!------------------------------------------------------------------
!
      character(len=ilen_pk_gauss_head) function hd_pick_gauss_head()
!
!
      hd_pick_gauss_head = 'num_spectr, reference_radius' // char(10)
!
      end function hd_pick_gauss_head
!
!------------------------------------------------------------------
!
      end module m_monitor_file_labels
