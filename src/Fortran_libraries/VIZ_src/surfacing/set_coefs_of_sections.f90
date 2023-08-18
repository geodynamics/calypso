!>@file   set_coefs_of_sections.f90
!!@brief  module set_coefs_of_sections
!!
!!@author H. Matsui
!!@date Programmed in May. 2006
!
!>@brief  Construct equation for cross section
!!
!!@verbatim
!!      subroutine s_set_coefs_of_sections                              &
!!     &         (psf_c, id_section_method, const_psf, ierr)
!!        type(psf_define_ctl), intent(in) :: psf_def_c
!!        real(kind = kreal) function side_of_plane(const_psf, xx)
!!      subroutine cal_normal_of_plane(const_psf, xx, normal)
!!      subroutine cal_normal4_of_plane(const_psf, xx4, normal4)
!!@endverbatim
!
      module set_coefs_of_sections
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_coefs_of_sections                                &
     &         (psf_def_c, id_section_method, const_psf, ierr)
!
      use m_error_IDs
      use m_section_coef_flags
      use t_control_data_4_psf_def
      use t_psf_patch_data
      use set_cross_section_coefs
!
      type(psf_define_ctl), intent(in) :: psf_def_c
!
      integer(kind = kint), intent(inout)  :: id_section_method
      real(kind = kreal), intent(inout) :: const_psf(10)
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
!
!
      ierr = 0
      tmpchara = psf_def_c%section_method_ctl%charavalue
!
      if(cmp_no_case(tmpchara, cflag_eq)) then
        id_section_method = 1
        call set_coefs_4_psf(psf_def_c%psf_coefs_ctl%num,               &
     &      psf_def_c%psf_coefs_ctl%c_tbl,                              &
     &      psf_def_c%psf_coefs_ctl%vect, const_psf(1))
!
      else if(cmp_no_case(tmpchara, cflag_pln)) then
        id_section_method = 2
        call set_coefs_4_plane(psf_def_c, const_psf(1))
!
      else if(cmp_no_case(tmpchara, cflag_sph)) then
        id_section_method = 2
        call set_coefs_4_sphere(psf_def_c, const_psf(1))
!
      else if(cmp_no_case(tmpchara, cflag_elp)) then
        id_section_method = 3
        call set_coefs_4_ellipsode(psf_def_c, const_psf(1) )
!
      else if(cmp_no_case(tmpchara, cflag_hyp)) then
        id_section_method = 4
        call set_coefs_4_hyperboloide(psf_def_c, const_psf(1) )
!
      else if(cmp_no_case(tmpchara, cflag_prb)) then
        id_section_method = 5
        call set_coefs_4_parabolic(psf_def_c, const_psf(1) )
      else
        ierr = ierr_VIZ
        write(e_message,'(a)') 'Set cross section mode'
        return
      end if
!
      end subroutine s_set_coefs_of_sections
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function side_of_plane(const_psf, xx)
!
      real(kind = kreal), intent(in) :: xx(3)
      real(kind = kreal), intent(in) :: const_psf(10)
!
!
      side_of_plane =  const_psf( 1) * (xx(1)*xx(1))                    &
     &               + const_psf( 2) * (xx(2)*xx(2))                    &
     &               + const_psf( 3) * (xx(3)*xx(3))                    &
     &               + const_psf( 4) * (xx(1)*xx(2))                    &
     &               + const_psf( 5) * (xx(2)*xx(3))                    &
     &               + const_psf( 6) * (xx(3)*xx(1))                    &
     &               + const_psf( 7) *  xx(1)                           &
     &               + const_psf( 8) *  xx(2)                           &
     &               + const_psf( 9) *  xx(3)                           &
     &               + const_psf(10)
!
      end function side_of_plane
!
!  ---------------------------------------------------------------------
!
      subroutine cal_normal_of_plane(const_psf, xx, normal)
!
      real(kind = kreal), intent(in) :: xx(3)
      real(kind = kreal), intent(in) :: const_psf(10)
!
      real(kind = kreal), intent(inout) :: normal(3)
!
!
      normal(1) =      const_psf( 1) *  xx(1) * two                     &
     &               + const_psf( 4) *  xx(2)                           &
     &               + const_psf( 6) *  xx(3)                           &
     &               + const_psf( 7) *  xx(1)
!
      normal(2) =      const_psf( 2) *  xx(2) * two                     &
     &               + const_psf( 4) *  xx(1)                           &
     &               + const_psf( 5) *  xx(3)                           &
     &               + const_psf( 8) *  xx(2)
!
      normal(3) =      const_psf( 3) *  xx(3) * two                     &
     &               + const_psf( 5) *  xx(2)                           &
     &               + const_psf( 6) *  xx(1)                           &
     &               + const_psf( 9) *  xx(3)
!
      end subroutine cal_normal_of_plane
!
!  ---------------------------------------------------------------------
!
      subroutine cal_normal4_of_plane(const_psf, xx4, normal4)
!
      real(kind = kreal), intent(in) :: xx4(4)
      real(kind = kreal), intent(in) :: const_psf(10)
!
      real(kind = kreal), intent(inout) :: normal4(4)
!
!
      normal4(1) =     const_psf( 1) *  xx4(1) * two                    &
     &               + const_psf( 4) *  xx4(2)                          &
     &               + const_psf( 6) *  xx4(3)                          &
     &               + const_psf( 7) *  xx4(1)
!
      normal4(2) =     const_psf( 2) *  xx4(2) * two                    &
     &               + const_psf( 4) *  xx4(1)                          &
     &               + const_psf( 5) *  xx4(3)                          &
     &               + const_psf( 8) *  xx4(2)
!
      normal4(3) =     const_psf( 3) *  xx4(3) * two                    &
     &               + const_psf( 5) *  xx4(2)                          &
     &               + const_psf( 6) *  xx4(1)                          &
     &               + const_psf( 9) *  xx4(3)
      normal4(4) =   0.0d0
!
      end subroutine cal_normal4_of_plane
!
!  ---------------------------------------------------------------------
!
      end module set_coefs_of_sections
