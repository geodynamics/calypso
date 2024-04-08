!>@file  m_pvr_control_labels.f90
!!       module m_pvr_control_labels
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine pvr_isosurf_dir_list_array(array_c)
!!      subroutine pvr_surf_enhance_mode_array(array_c)
!!
!!      subroutine pvr_movie_mode_list_array(array_c)
!!      subroutine lic_movie_mode_list_array(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!@endverbatim
!
      module m_pvr_control_labels
!
      use m_precision
      use m_constants
!
      implicit  none
!
      character(len = kchara), parameter                                &
     &                        :: LABEL_INCREASE = 'increase'
      character(len = kchara), parameter                                &
     &                        :: LABEL_DECREASE = 'decrease'
!
!
      character(len = kchara), parameter :: LABEL_EDGE = 'boarder'
      character(len = kchara), parameter                                &
     &                        :: LABEL_FORWARD = 'forward_surface'
      character(len = kchara), parameter                                &
     &                        :: LABEL_REVERSE = 'reverse_surface'
      character(len = kchara), parameter                                &
     &                        :: LABEL_BOTH =    'both_surface'
!
!
      character(len=kchara), parameter :: FLAG_ZOOM = 'zoom'
      character(len=kchara), parameter                                  &
     &                      :: FLAG_ROTATE_MOVIE =    'rotation'
      character(len=kchara), parameter                                  &
     &                      :: FLAG_START_END_VIEW =  'view_matrices'
      character(len=kchara), parameter                                  &
     &                      :: FLAG_LIC_KERNEL = 'LIC_kernel'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_isosurf_dir_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(LABEL_INCREASE, array_c)
      call append_c_to_ctl_array(LABEL_DECREASE, array_c)
!
      end subroutine pvr_isosurf_dir_list_array
!
! ----------------------------------------------------------------------
!
      subroutine pvr_surf_enhance_mode_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(LABEL_BOTH,    array_c)
      call append_c_to_ctl_array(LABEL_FORWARD, array_c)
      call append_c_to_ctl_array(LABEL_REVERSE, array_c)
      call append_c_to_ctl_array(LABEL_EDGE,    array_c)
!
      end subroutine pvr_surf_enhance_mode_array
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine pvr_movie_mode_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(FLAG_ROTATE_MOVIE,   array_c)
      call append_c_to_ctl_array(FLAG_ZOOM,           array_c)
      call append_c_to_ctl_array(FLAG_START_END_VIEW, array_c)
!
      end subroutine pvr_movie_mode_list_array
!
! ----------------------------------------------------------------------
!
      subroutine lic_movie_mode_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(FLAG_ROTATE_MOVIE,   array_c)
      call append_c_to_ctl_array(FLAG_ZOOM,           array_c)
      call append_c_to_ctl_array(FLAG_START_END_VIEW, array_c)
      call append_c_to_ctl_array(FLAG_LIC_KERNEL,     array_c)
!
      end subroutine lic_movie_mode_list_array
!
! ----------------------------------------------------------------------
!
      end module m_pvr_control_labels
